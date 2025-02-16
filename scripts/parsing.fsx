#r "nuget: FSharp.Formatting"
#load "model.fsx"
open System
open System.IO
open Model
open FSharp.Formatting.Markdown

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


let toText (spans: MarkdownSpans) =
    let rec toText' style (spans: MarkdownSpans) =
        match spans with
        | [] -> []
        | span :: tail ->
            [   match span with
                | MarkdownSpan.Literal(txt,_) ->
                    style, txt
                | MarkdownSpan.Emphasis(body, _) ->
                    yield! toText'  Italic  body
                | MarkdownSpan.Strong(body, _) ->
                    yield! toText'  Bold  body
                | _ -> ()
                yield! toText' style tail
            ]

    toText'  Regular spans

let textToString (text: (FontStyle * string) list) =
    let builder = System.Text.StringBuilder()
    for _,t in text do
        builder.Append(t) |> ignore
    string builder

let tryParseLink (description: MarkdownSpans) =
    match description with
    | IndirectLink(body,_,_,_) :: _ ->
        let text = toText body
        Some (textToString text)
    | _ -> None

let rangeRx = System.Text.RegularExpressions.Regex(@"^\s*(\d+)(\s+à\s+(\d+))?\s*:\s*")
let scoreRx = System.Text.RegularExpressions.Regex(@"\s*\(([+\-]\d|J\d|S\d|0)\s*([^)+]*)(\+ Escalade)?\s*\)\s*")
let escaladeRx = System.Text.RegularExpressions.Regex(@"\(\s*(voir\s+)?Escalade(\s+\$)?\s*\)")

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
        |> List.mapi (fun i (description,_) ->
            match tryParseLink description with
            | None ->
                let title = description
                                |> splitLines
                                |> List.map toText
                                |> List.concat
                                |> textToString
                match escalades |> Map.tryFindKey (fun _ e -> e.Title = title) with
                | Some k -> k
                | None -> char (int '1' + i)
            | Some link ->
                match escalades |> Map.tryFindKey (fun _ e -> e.Title.Contains (link.Trim())) with
                | Some k -> k
                | None -> char (int '1' + i)
                
        )
        |> List.sort


    let score =
        let s =
            description |> List.tryPick ( function
                    | Literal(txt,_) ->
                        let m = scoreRx.Match(txt)
                        if m.Success then
                            let value = m.Groups[1].Value
                            if value.StartsWith('J') then
                                let s = Int32.Parse(value.AsSpan(1))
                                Some(s, m.Groups[2].Value.Trim())
                            elif value.StartsWith('S') then
                                let s = Int32.Parse(value.AsSpan(1))
                                Some(-s, m.Groups[2].Value.Trim())
                            else
                                Some (int m.Groups[1].Value, m.Groups[2].Value.Trim())
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
                      Number = id
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
                      Illustration = id
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


open System.Text.RegularExpressions
let rmdRx = Regex(@"(\s*)(\*\*|_)( *)")
let punctRx = Regex(@"(\s*)(\.\.\.|…|\.|,|\?[!?]?|\![!?]?|;|:|')( *)")
let quoteRx = Regex(@"[""“”«]\s*([^""“”»]*)\s*[""“”»]( +)?")
let normSpaceRx = Regex(@"(\*|_) +")

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
            | ":" -> "\xA0: "
            | "'" -> "’"
            | "..." -> "… "
            | "…" -> "… "
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
    md5.Replace(" \xA0","\xA0").Replace("  "," ")

let parse path =
    let mdText =
        File.ReadAllText(path)
        |> cleanMd

    let md = Markdown.Parse(mdText)

    let situations = parseSituations md
    situations