#I @"C:\dev\FSharp.Formatting\src\FSharp.Formatting.Markdown\bin\Debug\netstandard2.1\"
#r "FSharp.Formatting.Common.dll"
#r "FSharp.Formatting.Markdown.dll"
#r "nuget: Feliz.ViewEngine"
open System.IO
open FSharp.Formatting.Markdown

type Colors =
    | Blue
    | Red
    | Yellow
    | Purple
    | Green

type TextColor =
    | Accent
    | Color of Colors
    | DefaultColor

type FontStyle =
    | Regular
    | Italic
    | Bold

type Style =
    { FontStyle: FontStyle
      Color: TextColor}



type Situation =
    { Id: int 
      Title: string
      Color: Colors
      Text: (Style * string) list 
      Reactions: Reaction list 
      Escalades: Map<char, Reaction> }
and Reaction =
    { Title: string
      Text: (Style * string) list
      Consequences: Consequence list }    
and Consequence =
    { Range: Range
      Title: string
      Text: (Style * string) list
      Score: Score }
and Range = { Min: int; Max: int}
    with
        member this.Percents = (this.Max + 1 - this.Min) * 10
        member this.Probability = decimal this.Percents / 100m
and Score =
    | Score of (int * string) option * char list



type ConsequenceMd = MarkdownSpans * MarkdownParagraphs
type ReactionMd = MarkdownSpans * ConsequenceMd list

let rec toText' style (spans: MarkdownSpans) =
    match spans with
    | [] -> []
    | span :: tail ->
        [   match span with
            | MarkdownSpan.Literal(txt,_) -> style, txt
            | MarkdownSpan.Emphasis(body, _) ->
                yield! toText' { style with FontStyle = Italic } body
            | MarkdownSpan.Strong(body, _) ->
                yield! toText' { style with FontStyle = Bold } body
            | _ -> ()
            yield! toText' style tail
        ]

let textToString (text: (Style * string) list) =
    let builder = System.Text.StringBuilder()
    for _,t in text do
        builder.Append(t) |> ignore
    string builder

let toText (spans: MarkdownSpans) =
    toText' { FontStyle = Regular; Color = DefaultColor } spans
    
let tryParseLink (description: MarkdownSpans) =
    match description with
    | IndirectLink(body,_,_,_) :: _ ->
        let text = toText body
        Some (textToString text)
    | _ -> None

let rangeRx = System.Text.RegularExpressions.Regex(@"^\s*(\d+)(\s+à\s+(\d+))?\s*:\s*")
let scoreRx = System.Text.RegularExpressions.Regex(@"\s*\(([+\-]?\d)([^)+]*)(\s*\+ Escalade\s*)?\)\s*")
let escaladeRx = System.Text.RegularExpressions.Regex(@"\(\s*voir\s+Escalade(\s+\$)?\s*\)")

let rec parseEscalade (description, items) : Reaction option =
    match tryParseLink description with
    | Some _ -> None
    | None -> 
        let text = toText description
        let title = textToString text
        let conseqs = List.map (parseConsequence Map.empty) items
        Some
            { Title = title
              Text = text
              Consequences = conseqs }

and extractEscalade ps =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) :: _ ->
        description, List.map extractConsequence items
    | Span(description,_) :: _ ->
        description, []
    | _ -> failwith "Invalid escalade format"
    

and extractEscalades ps =
    match ps with
    | ListBlock(_, cases,_) :: _ ->
        List.map extractEscalade cases
    | h :: _ ->
        printfn $"Invalid escalades format %A{h}"
        []
    | [] ->
        []

and extractConsequence ps : ConsequenceMd =
    match ps with
    | Span(description, _) :: tail 
    | Paragraph( description , _) :: tail ->
        description, tail
    | s :: _ -> failwith $"{ s.GetType().Name }" 
    | [] -> failwith "Empty consequence"

and parseConsequence (escalades: Map<char, Reaction>) ((description, escaladesMd): ConsequenceMd)  : Consequence =
    let range= 
        description |> List.tryPick ( function
            | Literal(txt,_) ->
                let m = rangeRx.Match(txt)
                if m.Success then
                    let min = int m.Groups[1].Value
                    let max =
                        if m.Groups[3].Success then
                            int m.Groups[3].Value
                        else
                            min
                    { Min = min
                      Max = max} |> Some
                else
                    None
            | _ -> None
        )
    let escaladeKeys =
        escaladesMd
        |> extractEscalades
        |> List.choose (fun (description,_) ->
            match tryParseLink description with
            | None -> 
                let title = description |> toText |> textToString
                escalades |> Map.tryFindKey (fun _ e -> e.Title = title) 
            | Some link ->
                escalades |> Map.tryFindKey (fun _ e -> e.Title.Contains (link.Trim())) 
        )
        |> List.sort

    
    let score = 
        let s =
            description |> List.tryPick ( function
                    | Literal(txt,_) ->
                        let m = scoreRx.Match(txt)
                        if m.Success then
                            Some (int m.Groups[1].Value, m.Groups[2].Value)
                        else
                            None
                    | _ -> None)
        Score(s, escaladeKeys)
        

    let text =
        description
        |> List.map (function
            | Literal(txt,r) when rangeRx.IsMatch(txt) ||scoreRx.IsMatch(txt) || escaladeRx.IsMatch(txt) -> 
                let txt2 = rangeRx.Replace(txt,"")
                let txt3 = scoreRx.Replace(txt2,"")
                Literal(escaladeRx.Replace(txt3,""),r)
            | t -> t
        ) 
        |> toText


    { Range = range |> Option.defaultValue { Min = 0; Max = 0}
      Title = textToString text
      Text = text
      Score = score }


let extractReaction ps : ReactionMd =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) ::_ ->
        description, List.map extractConsequence items
    | [ Span(description,_) ] ->
        description, []
    | _ -> failwith $"Format de réaction invalide:\n%A{ps}"

let extractReactions ps : ReactionMd list * MarkdownParagraphs =
    match ps with
    | ListBlock(_, items,_) :: tail ->
        List.map extractReaction items, tail
    | tail ->  [], tail

let escaladeId (i: int) =
    char(int 'A' + i)

let parseReaction (escalades: Map<char, Reaction>) (description, items) =
    let conseqs = List.map (parseConsequence escalades) items 
    let text = toText description
    { Title = textToString text
      Text = text
      Consequences = conseqs
    }            

let parseTitle (spans: MarkdownSpans) =
    (System.Text.StringBuilder(), spans)
    ||> List.fold (fun acc span ->
            match span with
            | Literal(text,_)  -> acc.Append text
            | _ -> acc
    )
    |> string
    |> fun s -> s.Trim()

let parseSituations (md : MarkdownDocument) = 
    let rec loop id ps result =
        match ps with
        | Heading(2, title,_) :: Paragraph(txtSituation,_) :: tail ->
            
            let reactionsMd, tail = extractReactions tail

            let escaladesMd = 
                [ for _,conseqsMd in reactionsMd do
                    for _,escaladesMd in conseqsMd do
                        yield! extractEscalades escaladesMd ]
            let escalades =
                escaladesMd
                |> List.choose parseEscalade



                |> Seq.mapi (fun i e -> escaladeId i,e)
                |> Map.ofSeq
            
            let reactions = List.map (parseReaction escalades) reactionsMd

            let situation =
                { Id = id
                  Title = parseTitle title
                  Color = 
                    let id = id%25
                    if id <= 5 then
                        Blue
                    elif id <= 10 then
                        Red
                    elif id < 15 then
                        Yellow 
                    elif id < 20 then
                        Purple
                    else
                        Green

                  Text = toText txtSituation
                  Reactions = reactions
                  Escalades = escalades
                 } 

            loop (id+1) tail (situation :: result)
        | [] -> List.rev result
        | head :: tail ->
            loop id tail result

    loop 1 md.Paragraphs []


let rmdRx = System.Text.RegularExpressions.Regex(@"(\s*)(\*\*|_)( *)")
let punctRx = System.Text.RegularExpressions.Regex(@"(\s*)(\.\.\.|\.|,|\?[!?]?|\![!?]?|;|:|')( *)")
let quoteRx = System.Text.RegularExpressions.Regex(@"[""“”«]\s*([^""“”»]*)\s*[""“”»]( +)?")
let normSpaceRx = System.Text.RegularExpressions.Regex(@"(\*|_) +")

let cleanMd (md: string) = 
    let md2 =
        rmdRx.Replace(md, (fun m ->
            match m.Groups[2].Value with
            | "**" -> m.Groups[1].Value + m.Groups[3].Value
            | "_" -> m.Groups[1].Value + m.Groups[3].Value
            | _ -> m.Value )
        )
    let md3 =
        punctRx.Replace(md2, (fun m ->
            match m.Groups[2].Value with
            | "." -> ". "
            | "," -> ", "
            | ";" -> "; "
            | ":" -> " : "
            | "'" -> "’"
            | "..." -> "… "
            | s when s.StartsWith("?") || s.StartsWith("!") -> $"\xA0{s} "
            | _ -> m.Value )
        )
    let md4 =
       quoteRx.Replace(md3, (fun m -> 

        if m.Groups[2].Success then
            "_«\xA0" + m.Groups[1].Value + "\xA0»_ "
        else
            "_«\xA0" + m.Groups[1].Value + "\xA0»_"
            )
        )
    let md5 =
        normSpaceRx.Replace(md4, (fun m -> m.Groups[1].Value + " "))
    md5

let parse path =
    let mdText = 
        File.ReadAllText(path)
        |> cleanMd

    let md = Markdown.Parse(mdText)

    let situations = parseSituations md
    situations

let situationCards situation =
    1 + situation.Reactions.Length + situation.Escalades.Count

let cut len (s: string) =
    if s.Length >= len-1 then
        s.Substring(0,len-1) + "…"
    else
        s 

let situationScore situation =
    let scores =
        [ let reactionProba = 1m / decimal situation.Reactions.Length
          for reaction in situation.Reactions do
            for cons in reaction.Consequences do
                    match cons.Score with
                    | Score(Some(n,_),_) -> 
                        reactionProba * cons.Range.Probability * decimal n
                    | Score(None,[]) -> 0m
                    | Score(None, es) ->
                        let escaladeReactionProba = 1m / decimal es.Length
                        for k in es do
                            let reac = situation.Escalades[k]
                            for c in reac.Consequences do
                                for j in c.Range.Min .. c.Range.Max do
                                    match c.Score with
                                    | Score(Some(n,_),_) ->
                                       reactionProba * cons.Range.Probability *
                                        escaladeReactionProba * c.Range.Probability * decimal n
                                    | _ -> 0m
        ]
    scores |> List.sum


let check (situations: Situation list) =
    let checks =
        [
            for situation in situations do
                let errors = ResizeArray()
                let warn (s:string) =
                    errors.Add("\x1b[33m" + s + "\x1b[0m")

                let checkRanges name reaction =
                    if reaction.Consequences = [] then
                        warn $"  [{name}] consequences manquantes \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                    let ranges =
                        [ for cons in reaction.Consequences do
                            yield! [cons.Range.Min .. cons.Range.Max] ]
                    if List.contains 0 ranges then
                        warn $"  [{name}] pourcentage non spécifée \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"

                    let missing = (set [1..10] - set ranges)
                    if not missing.IsEmpty then
                        if missing = set [1..10] then
                            warn $"  [{name}] pourcentages manquants \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                        else
                            let m = missing |> Seq.map string |> String.concat ", "
                            warn $"  [{name}] pourcentages {m} manquants \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"
                    let multi = ranges |> List.countBy id |> List.filter (fun (_,c) -> c > 1) |> List.map fst
                    if not multi.IsEmpty then
                        for n in multi do
                            warn $"  [{name}] valeur {n} multiple \x1b[38;2;128;128;128m/ { textToString reaction.Text |> cut 40}"

                if situation.Text = [] then 
                    warn "  texte situation manquant"
                if situation.Reactions = [] then
                    warn "  reactions manquantes"
                else
                    for i,reaction in situation.Reactions |> Seq.indexed do
                        if reaction.Text = [] then
                            warn $"  [reaction {i+1}] texte manquant"
                        checkRanges $"reaction {i+1}" reaction

                        for cons in reaction.Consequences do
                            match cons.Score with
                            | Score(None, []) ->
                                    warn $"  [reaction {i+1}] score manquant \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                            | Score(None, _) -> ()
                            | Score(Some(n,_),_) -> 
                                if n > 3 then
                                    warn $"  [reaction {i+1}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [reaction {i+1}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                
                    for k,escalade in situation.Escalades |> Map.toSeq do
                        if escalade.Text = [] then
                            warn $"  [escalade {k}] texte manquant"
                        checkRanges $"escalade {k}" escalade
                        for c in escalade.Consequences do
                            match c.Score with
                            | Score(Some(n,_),_) ->
                                if n > 3 then
                                    warn $"  [escalade {k}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [escalade {k}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"
                            | Score(None,[]) ->
                                    warn $"  [escalade {k}] score manquant \x1b[38;2;128;128;128m/ {textToString  c.Text |> cut 40 }"

                            | _ -> ()

                    let usedEscaladeKeys =
                        [ for r in situation.Reactions do
                            for c in r.Consequences do
                                match c.Score with
                                | Score(_,es) -> yield! es
                        ]
                        |> set

                    let unusedEscalades = set situation.Escalades.Keys - usedEscaladeKeys
                    if not unusedEscalades.IsEmpty then
                        let ls = unusedEscalades |> Seq.map string |> String.concat ""
                        warn $"  escalades {ls} non utilisées"
                let result = 
                    if errors.Count = 0 then
                        let score = situationScore situation

                        Ok score
                        
                    else
                        Error errors
                situation,   result
        ]
        |> List.sortBy(function
            | _,Ok _ -> 1
            | _ -> 0)

    for situation, result in checks do
        let cards = situationCards situation
        let title = cut 40 situation.Title
        match result with
        | Ok score ->
            printfn "✅ S%d %s \x1b[38;2;128;128;128m(%d réactions / %d escalades / %d cards) \x1b[32m(score %.2f)\x1b[0m" situation.Id title situation.Reactions.Length situation.Escalades.Count cards score
        | Error errors ->
            printfn "❌ S%d %s \x1b[38;2;128;128;128m(%d réactions / %d escalades / %d cards)\x1b[0m" situation.Id title situation.Reactions.Length situation.Escalades.Count cards
            for error in errors do
                printfn "%s" error

    [ for situation,result in checks do
        match result with
        | Ok _ -> situation
        | Error _ -> ()
    ]

open Feliz.ViewEngine

let colorProp = function
    | Blue -> "blue"
    | Red -> "red"
    | Green -> "green"
    | Yellow -> "yellow"
    | Purple -> "purple"

type Card =
| Situation of int * Situation
| Reaction of int * Situation * Reaction
| Escalade of char * int * Situation * Reaction

let pos n =
    let c = 1+n%3;
    let r = 1+n/3; 
    $"c{c} r{r}"

let renderSituationRecto n (situation: Situation) =

    Html.div [
        prop.className $"card recto situation { colorProp situation.Color } {pos n}" 
        prop.children [
            Html.h1 $"Situation {situation.Id}"
            Html.div [ 
                prop.className "description" 
                prop.children [
                Html.p [
                    for style, text in situation.Text do
                        match style.FontStyle with
                        | Regular -> Html.text text
                        | Italic -> Html.em text
                        | Bold -> Html.strong text
                    ]
                ]
            ]
        ]
    ]

let renderSituationVerso n dice =
    Html.div [
        prop.className $"card verso situation {pos n}" 
        prop.children [
            Html.div [ 
                prop.custom("data-dice", string dice)
            ]
        ]
    ]

let renderReactionRecto (n: int) (r: int) key (situation: Situation) (reaction: Reaction) =
    Html.div [
        let cls = match key with None -> "reaction" | Some _ -> "escalade"
        prop.className $"card recto {cls}  {colorProp situation.Color } {pos n}"
        prop.children [
            Html.h1 (
                match key with 
                | None -> $"Réaction S{situation.Id}"
                | Some c -> $"Escalade {situation.Id} {c}")
            Html.div [
                prop.className "description"
                prop.children [
                    Html.p [
                        for style, text in reaction.Text do
                            match style.FontStyle with
                            | Regular -> Html.text text
                            | Italic -> Html.em text
                            | Bold -> Html.strong text
                        ]
                    ]
                ]
            Html.div [
                let count = 
                    match key with
                    | None -> situation.Reactions.Length
                    | Some _ ->  situation.Escalades.Count
                prop.className "count"
                prop.children [
                    Html.div (r+1)
                    Html.div count
                ]
            ]
                
            ]
        ]
let plusEscalade ids =
    match ids with
    | [] -> ""
    | _ ->
        let list = ids |> List.map string |> String.concat ""
        $" + Escalade %s{list}"

let renderReactionVerso n key (situation: Situation) (reaction: Reaction) =
    Html.div [
        let cls = match key with None -> "reaction" | Some _ -> "escalade"
        prop.className $"card verso {cls} {colorProp situation.Color } {pos n}"
        prop.children [
            Html.h1 (
                match key with 
                | None -> $"Réaction S{situation.Id}"
                | Some c -> $"Escalade {situation.Id} {c}")
            Html.div [
                prop.className "consequences"
                prop.children [
                    for consequence in reaction.Consequences do
                        Html.p [
                            Html.span [ 
                                prop.className "dice"
                                if consequence.Range.Min = consequence.Range.Max then
                                    prop.text $"{consequence.Range.Min} :"
                                else
                                    prop.text $"{consequence.Range.Min} à {consequence.Range.Max} :"
                            ]
                            Html.text " "
                            for style, text in consequence.Text do
                                match style.FontStyle with
                                | Regular -> Html.span text
                                | Italic -> Html.em text
                                | Bold -> Html.strong text
                            Html.text " "

                            Html.span [
                                match consequence.Score with
                                | Score (Some(score,txt),ids) when score > 0 ->
                                    prop.className "positif"
                                    prop.text $" (+%d{score}%s{txt}%s{plusEscalade ids})\n"
                                | Score(Some(score,txt), ids) when score < 0 ->
                                    prop.className "negatif"
                                    prop.text $" (-%d{-score}%s{txt}%s{plusEscalade ids})\n"
                                | Score (Some(_,txt), ids) ->
                                    prop.className "zero"
                                    prop.text $" (0%s{txt}%s{plusEscalade ids})\n"
                                | Score(None, ids) ->
                                    prop.className "escalade"
                                    let list = ids |> List.map string |> String.concat ""

                                    prop.text $" (Escalade %s{list})\n"
                                ]
                            ]
                            
                        ]
                ]
                
            ]
        ]


let render (cards: Card list) =
    Html.html [
        Html.head [
            Html.meta [ prop.charset "utf-8" ]
            Html.title [ prop.text "Transmission(s)" ]
            Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
            Html.link [prop.href "./interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
            Html.link [ prop.href "./cards.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
        ]
        Html.body [
            
                for page in cards |> List.chunkBySize 9 do
                    Html.section [
                        prop.className "recto"
                        prop.children [
                            Html.div [ prop.className "mark m-left m-top"]
                            Html.div [ prop.className "mark m-right m-top"]
                            Html.div [ prop.className "mark m-left m-bottom"]
                            Html.div [ prop.className "mark m-right m-bottom"]
                            for n,card in List.indexed page do 
                                match card with
                                | Situation(_, sit) ->
                                    renderSituationRecto n sit
                                | Reaction(r,situation, reaction) ->
                                    renderReactionRecto n r None situation reaction
                                | Escalade(c, r, situation, reaction) -> 
                                    renderReactionRecto n r (Some c) situation reaction
                        ]
                    ]
                    Html.section [
                        prop.className "verso"
                        prop.children [
                            for n,card in List.indexed page do
                                match card with
                                | Situation(dice,_) ->
                                    renderSituationVerso n dice
                                | Reaction(_,situation, reaction) ->
                                    renderReactionVerso n None situation reaction
                                | Escalade(c,_, situation, reaction) -> 
                                    renderReactionVerso n (Some c) situation reaction
                        ]
                    ]
        ]
    ]

let situations = 
    parse @"C:\dev\transmissions\src\Server\situations.md"
    |> check

File.ReadAllText(@"C:\dev\transmissions\src\Server\situations.md")
|> cleanMd
|> fun s ->
    let pos = s.IndexOf "Moi, je veux pas"
    s.Substring(pos-4, 80)



let dice =
    seq {
        let rand = System.Random(42)
        while true do
            yield! [1 .. 10] |> Seq.sortBy (fun _ -> rand.Next())

    }

let cards =
    [ 
        for situation, dice in Seq.zip situations dice do
            Situation(dice, situation)
            for n,reaction in situation.Reactions |> Seq.indexed do
                Reaction (n, situation, reaction)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
    ]

let html =
    render cards 
    |> Render.htmlView

System.IO.File.WriteAllText("./cards/index.html", html)