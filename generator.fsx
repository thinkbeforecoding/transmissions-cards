#r "nuget: Feliz.ViewEngine"
#r "nuget: FSharp.Formatting"
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
      Text: (Style * string) list list
      Strategies: Strategy list
      Escalades: Map<char, Strategy> }
and Strategy =
    { Title: string
      Text: (Style * string) list list
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
type StrategyMd = MarkdownSpans * ConsequenceMd list

type Lines =
    { Current: MarkdownSpans
      Lines: MarkdownSpans list }

module Lines =
    let empty = { Current = []; Lines = []}
    let addSpan span lines =
        { lines with Current = span :: lines.Current }
    let nextLine lines =
        if lines.Current = [] then
            lines
        else
            { Current = []
              Lines = (List.rev lines.Current) :: lines.Lines}
    let rec addLines spans (lines: Lines) =
        match spans with
        | [] -> lines
        | [ span ] -> addSpan span lines
        | span :: tail -> addLines tail (lines |> addSpan span |> nextLine)

    let close lines =
        (nextLine lines).Lines |> List.rev

let rec splitSpan (span: MarkdownSpan) =
    match span with
    | Literal(txt, _) ->
        txt.Split('\\') |> Array.toList |> List.map (fun txt -> Literal(txt, None))
    | Emphasis(body, _) ->
        splitList body Lines.empty
        |> List.map (fun l -> Emphasis(l, None))
    | Strong(body, _) ->
        splitList body Lines.empty
        |> List.map (fun l -> Strong(l, None))
    | x -> [x]


and splitList spans lines =
    match spans with
    | [] -> Lines.close lines
    | span :: tail ->
        let parts = splitSpan span
        splitList tail (Lines.addLines parts lines)





let splitLines spans = splitList spans Lines.empty



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

let rec parseEscalade (description, items) : Strategy option =
    match tryParseLink description with
    | Some _ -> None
    | None ->
        let text =
            description
            |> splitLines
            |> List.map toText
        let title = textToString (List.concat text)
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
    | [] -> failwith "Empty consequences"

and parseConsequence (escalades: Map<char, Strategy>) ((description, escaladesMd): ConsequenceMd)  : Consequence =
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
                let title = description
                                |> splitLines
                                |> List.map toText
                                |> List.concat
                                |> textToString
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


let extractStrategy ps : StrategyMd =
    match ps with
    | Span(description,_) :: ListBlock(_,items,_) ::_ ->
        description, List.map extractConsequence items
    | [ Span(description,_) ] ->
        description, []
    | _ -> failwith $"Format de stratégie invalide:\n%A{ps}"

let extractStrategies ps : StrategyMd list * MarkdownParagraphs =
    match ps with
    | ListBlock(_, items,_) :: tail ->
        List.map extractStrategy items, tail
    | tail ->  [], tail

let escaladeId (i: int) =
    char(int 'A' + i)

let parseStrategy (escalades: Map<char, Strategy>) (description, items) =
    let conseqs = List.map (parseConsequence escalades) items
    let text =
        description
        |> splitLines
        |> List.map toText
    { Title = textToString (List.concat text)
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


let idRx = System.Text.RegularExpressions.Regex(@"^((?<num>\d+)\.\s*)?(?<title>.*)$")

let parseSituations (md : MarkdownDocument) =
    let rec loop ps result =
        match ps with
        | Heading(2, title,_) :: Paragraph(txtSituation,_) :: tail ->


            let situation =
                try
                    let strategiesMd, tail = extractStrategies tail

                    let escaladesMd =
                        let rec loop strategiesMd =
                            [ for _,conseqsMd in strategiesMd do
                                for _,escaladesMd in conseqsMd do
                                    let escalades = extractEscalades escaladesMd
                                    yield! escalades
                                    yield! loop escalades

                            ]
                        loop strategiesMd
                    let escalades =
                        escaladesMd
                        |> List.choose parseEscalade



                        |> Seq.mapi (fun i e -> escaladeId i,e)
                        |> Map.ofSeq

                    let strategies = List.map (parseStrategy escalades) strategiesMd
                    let tit = parseTitle title
                    let m = idRx.Match(tit)
                    let id =
                        if m.Groups["num"].Success then
                            int m.Groups["num"].Value
                        else
                            0
                    let titleTxt = m.Groups["title"].Value


                    { Id = id
                      Title = titleTxt
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

                      Text =
                        txtSituation
                        |> splitLines
                        |> List.map toText
                      Strategies = strategies
                      Escalades = escalades
                    } |> Some
                with
                | ex ->
                    printfn $"%O{ex}"
                    None


            match situation with
            | Some situation ->
                loop tail (situation :: result)
            | None ->
                loop tail result

        | [] -> List.rev result
        | head :: tail ->
            loop tail result

    loop md.Paragraphs []


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
    md5.Replace(" \xA0","\xA0")

let parse path =
    let mdText =
        File.ReadAllText(path)
        |> cleanMd

    let md = Markdown.Parse(mdText)

    let situations = parseSituations md
    situations

let situationCards situation =
    1 + situation.Strategies.Length + situation.Escalades.Count

let cut len (s: string) =
    if s.Length >= len-1 then
        s.Substring(0,len-1) + "…"
    else
        s

let situationScore situation =
    let scores =
        [ let strategyProba = 1m / decimal situation.Strategies.Length
          for strategy in situation.Strategies do
            for cons in strategy.Consequences do
                    match cons.Score with
                    | Score(Some(n,_),_) ->
                        strategyProba * cons.Range.Probability * decimal n
                    | Score(None,[]) -> 0m
                    | Score(None, es) ->
                        let escaladeStrategyProba = 1m / decimal es.Length
                        for k in es do
                            let strategy = situation.Escalades[k]
                            for c in strategy.Consequences do
                                for j in c.Range.Min .. c.Range.Max do
                                    match c.Score with
                                    | Score(Some(n,_),_) ->
                                       strategyProba * cons.Range.Probability *
                                        escaladeStrategyProba * c.Range.Probability * decimal n
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

                let checkRanges name strategy =
                    if strategy.Consequences = [] then
                        warn $"  [{name}] consequences manquantes \x1b[38;2;128;128;128m/ { strategy.Title |> cut 40}"
                    let ranges =
                        [ for cons in strategy.Consequences do
                            yield! [cons.Range.Min .. cons.Range.Max] ]
                    if List.contains 0 ranges then
                        warn $"  [{name}] pourcentage non spécifée \x1b[38;2;128;128;128m/ { strategy.Title |> cut 40}"

                    let missing = (set [1..10] - set ranges)
                    if not missing.IsEmpty then
                        if missing = set [1..10] then
                            warn $"  [{name}] pourcentages manquants \x1b[38;2;128;128;128m/ { strategy.Title |> cut 40}"
                        else
                            let m = missing |> Seq.map string |> String.concat ", "
                            warn $"  [{name}] pourcentages {m} manquants \x1b[38;2;128;128;128m/ { strategy.Title |> cut 40}"
                    let multi = ranges |> List.countBy id |> List.filter (fun (_,c) -> c > 1) |> List.map fst
                    if not multi.IsEmpty then
                        for n in multi do
                            warn $"  [{name}] valeur {n} multiple \x1b[38;2;128;128;128m/ { strategy.Title |> cut 40}"

                if situation.Text = [] then
                    warn "  texte situation manquant"
                if situation.Strategies = [] then
                    warn "  stratégies manquantes"
                else
                    for i,strategy in situation.Strategies |> Seq.indexed do
                        if strategy.Text = [] then
                            warn $"  [stratégies {i+1}] texte manquant"
                        checkRanges $"stratégie {i+1}" strategy

                        for cons in strategy.Consequences do
                            match cons.Score with
                            | Score(None, []) ->
                                    warn $"  [stratégie {i+1}] score manquant \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                            | Score(None, _) -> ()
                            | Score(Some(n,_),_) ->
                                if n > 3 then
                                    warn $"  [stratégie {i+1}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [stratégie {i+1}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"

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
                        [ for r in situation.Strategies do
                            for c in r.Consequences do
                                match c.Score with
                                | Score(_,es) -> yield! es
                        ]
                        |> set

                    let unusedEscalades = set situation.Escalades.Keys - usedEscaladeKeys
                    if not unusedEscalades.IsEmpty then
                        for e in unusedEscalades do
                            let escalade = situation.Escalades[e]

                            warn $"  escalades {e} non utilisées \x1b[38;2;128;128;128m/ {textToString  (List.concat escalade.Text)  |> cut 40 }"
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
            printfn "✅ S%d %s \x1b[38;2;128;128;128m(%d stratégies / %d escalades / %d cards) \x1b[32m(score %.2f)\x1b[0m" situation.Id title situation.Strategies.Length situation.Escalades.Count cards score
        | Error errors ->
            printfn "❌ S%d %s \x1b[38;2;128;128;128m(%d stratégies / %d escalades / %d cards)\x1b[0m" situation.Id title situation.Strategies.Length situation.Escalades.Count cards
            for error in errors do
                printfn $"%s{error}"

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
| Alea of int
| Situation of  Situation
| Strategy of situationNumber:int * Situation * Strategy
| Escalade of char * situationNumber:int * Situation * Strategy

let pos n =
    let c = 1+n%3;
    let r = 1+n/3;
    $"c{c} r{r}"

let renderSituationRecto n (situation: Situation) =

    Html.div [
        prop.className $"card recto situation { colorProp situation.Color } {pos n}"
        if System.IO.File.Exists($"./cards/img/illustrations/situation-{situation.Id}.webp") then
            prop.style [style.custom("--illustration", $"url(img/illustrations/situation-{situation.Id}.webp)") ]
        prop.children [
            Html.h1 $"Situation {situation.Id}"
            Html.div [
                prop.className "description"
                prop.children [
                    for line in situation.Text do
                        Html.p [
                            for style, text in line do
                                match style.FontStyle with
                                | Regular -> Html.text text
                                | Italic -> Html.em text
                                | Bold -> Html.strong text
                            ]
                        ]
            ]
        ]
    ]

let renderSituationVerso n  =
    Html.div [
        prop.className $"card verso situation {pos n}"
    ]

let renderStrategyRecto (n: int) (r: int) key (situation: Situation) (strategy: Strategy) =
    Html.div [
        let cls = match key with None -> "strategy" | Some _ -> "escalade"
        prop.className $"card recto {cls} {colorProp situation.Color } {pos n}"
        // match key with
        // | None ->
        if key.IsSome && System.IO.File.Exists($"./cards/img/illustrations/strategie-e-{situation.Id}.webp") then
            prop.style [style.custom("--illustration", $"url(img/illustrations/strategie-e-{situation.Id}.webp)") ]
        // | Some _ -> ()
        elif System.IO.File.Exists($"./cards/img/illustrations/strategie-{situation.Id}.webp") then
                prop.style [style.custom("--illustration", $"url(img/illustrations/strategie-{situation.Id}.webp)") ]
        // | Some _ -> ()

        prop.children [
            Html.h1 (
                match key with
                | None -> $"Stratégie {situation.Id}"
                | Some c -> $"Escalade {situation.Id} {c}")
            Html.div [
                prop.className "description"
                prop.children [
                    for line in strategy.Text do
                        Html.p [
                            for style, text in line do
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
                    | None -> situation.Strategies.Length
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

let renderStrategyVerso n key (situation: Situation) (strategy: Strategy) =
    Html.div [
        let cls = match key with None -> "strategy" | Some _ -> "escalade"
        prop.className $"card verso {cls} {colorProp situation.Color } {pos n}"
        // match key with
        // | None ->
        if key.IsSome && System.IO.File.Exists($"./cards/img/illustrations/consequences-e-{situation.Id}.webp") then
            prop.style [style.custom("--illustration", $"url(img/illustrations/consequences-e-{situation.Id}.webp)") ]
        elif System.IO.File.Exists($"./cards/img/illustrations/consequences-{situation.Id}.webp") then
            prop.style [style.custom("--illustration", $"url(img/illustrations/consequences-{situation.Id}.webp)") ]
        // | Some _ -> ()

        prop.children [
            Html.h1 (
                match key with
                | None -> $"Conséquences {situation.Id}"
                | Some c -> $"Conséquences {situation.Id} {c}")
            Html.div [
                prop.className "consequences"
                prop.children [
                    for consequence in strategy.Consequences do
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
                                    prop.className "score positif"
                                    prop.text $" (+%d{score}%s{txt}%s{plusEscalade ids})\n"
                                | Score(Some(score,txt), ids) when score < 0 ->
                                    prop.className "score negatif"
                                    prop.text $" (-%d{-score}%s{txt}%s{plusEscalade ids})\n"
                                | Score (Some(_,txt), ids) ->
                                    prop.className "score zero"
                                    prop.text $" (0%s{txt}%s{plusEscalade ids})\n"
                                | Score(None, ids) ->
                                    prop.className "score escalade"
                                    let list = ids |> List.map string |> String.concat ""

                                    prop.text $" (Escalade %s{list})\n"
                                ]
                            ]

                        ]
                ]

            ]
        ]

let renderAleaRecto i (n: int) =
    Html.div [
        prop.className $"card recto alea a{n} {pos i}"
        prop.children [
            Html.div n
        ]
    ]

let renderAleaVerso i  =
    Html.div [
        prop.className $"card verso alea {pos i}"
    ]

let render (cards: Card list) =
    Html.html [
        Html.head [
            Html.meta [ prop.charset "utf-8" ]
            Html.title [ prop.text "Transmission(s)" ]
            Html.link [prop.href "./stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
            Html.link [ prop.href "./stylesheets/cards.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
            Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
            Html.script [ prop.src "./js/anchor.js" ]
        ]
        Html.body [

                for page in cards |> List.chunkBySize 9 do
                    Html.section [
                        prop.className "recto"
                        prop.children [
                            // Divs for the cricut cut marks
                            Html.div [ prop.className "mark m-left m-top"]
                            Html.div [ prop.className "mark m-right m-top"]
                            Html.div [ prop.className "mark m-left m-bottom"]
                            Html.div [ prop.className "mark m-right m-bottom"]

                            for n,card in List.indexed page do
                                match card with
                                | Alea x ->
                                    renderAleaRecto n x
                                | Situation sit ->
                                    renderSituationRecto n sit
                                | Strategy(r,situation, strategy) ->
                                    renderStrategyRecto n r None situation strategy
                                | Escalade(c, r, situation, strategy) ->
                                    renderStrategyRecto n r (Some c) situation strategy
                        ]
                    ]
                    Html.section [
                        prop.className "verso"
                        prop.children [
                            for n,card in List.indexed page do
                                match card with
                                | Alea _ ->
                                    renderAleaVerso n
                                | Situation _ ->
                                    renderSituationVerso n
                                | Strategy(_,situation, strategy) ->
                                    renderStrategyVerso n None situation strategy
                                | Escalade(c,_, situation, strategy) ->
                                    renderStrategyVerso n (Some c) situation strategy
                        ]
                    ]
        ]
    ]

fsi.PrintDepth <- 1
let champigny =
    parse @"champigny.md"
    |> check

let situations =
    parse @"situations.md"
    |> check


System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let cards =
    // [   for i in 1 .. 10 do
            // Alea i
    [
        // for i in 1 .. 10 do
        //     Alea i

        for situation in champigny do

            Situation( situation)
            for n,strategy in situation.Strategies |> Seq.indexed do
                Strategy (n, situation, strategy)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
        // Alea 10
        // for i in 1 .. 10 do
        //     Alea i
    ]

let html =
    render cards
    |> Render.htmlView

// System.IO.File.WriteAllText("./cards/situations.html", html)
System.IO.File.WriteAllText("./cards/champigny.html", html)
// System.IO.File.WriteAllText("./cards/champigny-alea.html", html)

let alea = [ for i in 1 .. 10 do Alea i ]

let aleahtml =
    render (alea @ alea @ alea )
    |> Render.htmlView
System.IO.File.WriteAllText("./cards/alea.html", aleahtml)



[
    for situation in champigny do
        Situation( situation)
]
|> render
|> Render.htmlView
|> fun html -> System.IO.File.WriteAllText("./cards/situations-champigny.html", html)
[ for situation in champigny do
        if situation.Id = 17 then
            Situation( situation)
            for n,strategy in situation.Strategies |> Seq.indexed do
                Strategy (n, situation, strategy)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
]
|> render
|> Render.htmlView
|> fun html -> System.IO.File.WriteAllText("./cards/situation-17.html", html)


[ for situation in champigny do
        if situation.Id = 16 then
            Situation( situation)
            for n,strategy in situation.Strategies |> Seq.indexed do
                Strategy (n, situation, strategy)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
]
|> render
|> Render.htmlView
|> fun html -> System.IO.File.WriteAllText("./cards/situation-16.html", html)
