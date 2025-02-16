
#r "nuget: FSharp.Formatting"
open System
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

let (|EndsWith|_|) (str: string) (input: string) = 
    if input.EndsWith(str) then
        Some(input.Substring(0,input.Length-str.Length))
    else
        None

    

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
let scoreRx = System.Text.RegularExpressions.Regex(@"\s*\(([+\-]\d|0)\s*([^)+]*)(\+ Escalade)?\s*\)\s*")
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

let rec strategyScore  (escalades: Map<char,Strategy>) (strategies: Strategy list) (strategy: Strategy) = 
    [ for cons in strategy.Consequences do
            match cons.Score with
            | Score(Some(n,""),[]) ->
                cons.Range.Probability * decimal n
            | Score(Some(n,l),es) -> 
                let newSituations =  (List.filter (fun s -> s <> strategy) strategies) @ [ for e in es -> escalades[e] ]
                cons.Range.Probability * ( decimal n + situationScore escalades newSituations )
            | Score(None, es) ->
                cons.Range.Probability * situationScore  escalades [ for e in es -> escalades[e] ]
            // | _ -> failwith "No score"
    ]
    |> List.sum

and situationScore (escalades: Map<char,Strategy>) (strategies: Strategy list) : decimal =
    if List.isEmpty strategies then
        failwith "❌ Plus de stratégies"
    
    [ for strategy in strategies do
            strategyScore escalades strategies strategy
    ]
    |> List.average 

let score (situation: Situation) =
    try
        situationScore situation.Escalades situation.Strategies
    with
    | ex ->
        printfn $"❌ Erreur calcul score situation S{situation.Id} {situation.Title}"
        0m


let check (situations: Situation list) =
    let checks =
        [
            for situation in situations |> Seq.sortBy (fun s -> s.Id) do
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
                            | Score(None, es) -> 
                                for k in es do
                                    if not (Map.containsKey k situation.Escalades) then
                                        warn $"  [stratégie {i+1}] escalade non trouvée {k} \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                            | Score(Some(n,txt),es) ->
                                if n > 3 then
                                    warn $"  [stratégie {i+1}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
                                elif n < -3 then
                                    warn $"  [stratégie {i+1}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"

                                match txt with
                                | ""
                                | "ET choisis une autre Stratégie"
                                | "OU choisis une autre Stratégie" -> ()
                                | _ -> warn $"  [stratégie {i+1}] text score inconnu ({txt}) \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"

                                for k in es do
                                    if not (Map.containsKey k situation.Escalades) then
                                        warn $"  [stratégie {i+1}] escalade non trouvée {k} \x1b[38;2;128;128;128m/ {textToString  cons.Text |> cut 40 }"
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
                        Ok (score situation)

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

fsi.PrintDepth <- 1
let champigny =
    parse @"champigny.md"
    |> check

type PlayerId = PlayerId of int
type InterventionCard = Green | Red
type WitnessIntervention = 
    | Support
    | BeCareful of Strategy
type Interventions =
    { Red: int
      Green: int }
type Player =
    { Id: int
      Confidence: int
      Interventions: Interventions
    }
type Table =
    { Players: Player[]
      Current: int}
type Game = 
    {  Table: Table
       Turn: int
       System: int
       InterventionDrawPile: InterventionCard list
       InterventionDiscardPile: InterventionCard list
       Situations: Situation list }

type InterventionResponse =
    | ThankYou of Player * WitnessIntervention 
    | MindYourOwnBusiness of Player * WitnessIntervention 


type Step =
    { Strategy: Strategy
      OriginalStrategy: Strategy
      Response: InterventionResponse option
      Alea: int }

module Interventions =
    let draw (interventions, cards) =
        match cards with
        | Green :: tail ->
            { interventions with Green = interventions.Green+1 }, tail
        | Red :: tail ->
            { interventions with Red = interventions.Red+1 }, tail
        | [] -> failwith "Not enough cards"

    let take card interventions =
        match card with
        | Green -> { interventions with Green = interventions.Green + 1}
        | Red -> { interventions with Red = interventions.Red + 1}

    let discard card interventions =
        match card with
        | Green -> { interventions with Green = interventions.Green - 1}
        | Red -> { interventions with Red = interventions.Red - 1}


    let setup cards =
        ({ Green = 0; Red = 0}, cards)
        |> draw
        |> draw
        |> draw

module Player =
    let setup id cards =
        let interventions, cards = Interventions.setup cards
        { Id = id
          Confidence = 3
          Interventions = interventions
        }, cards

module Table =
    let empty = { Players = Array.empty; Current = 0}
    let addPlayer player table =
        { table with Players = table.Players |> Array.insertAt table.Players.Length player }

    let next (table: Table) =
        { table with Current = (table.Current + 1) % table.Players.Length }

    let current (table: Table) =
        table.Players[table.Current]

    let observer (table: Table) =
        table.Players[(table.Current+ table.Players.Length - 1 ) % table.Players.Length]

    let witnesses (table: Table) =
        let current = current table
        let observer = observer table
        table.Players
        |> Seq.filter (fun p -> p <> current && p <> observer)
        |> Seq.toList

    let updatePlayer id f table =
        { table with 
            Players = table.Players |> Array.map (fun p -> if p.Id = id then f p else p) }




module Range =
    let contains x (r: Range) =
        x >= r.Min && x <= r.Max

module Game =
    let addPlayer game =
        let player, drawPile  = Player.setup game.Table.Players.Length game.InterventionDrawPile
        { game with
            Table =  Table.addPlayer player game.Table
            InterventionDrawPile = drawPile }
    
    let rec addPlayers n game  =
        if n = 0 then
            game
        else
            addPlayers (n-1) (addPlayer game)

    let shuffle game =
        { game with InterventionDrawPile = game.InterventionDrawPile |> List.sortBy (fun _ -> Random.Shared.Next() )
                    Situations = game.Situations |> List.sortBy (fun _ -> Random.Shared.Next() )}

    
    let empty situations =
        { Table = Table.empty
          Turn = 1
          System = 0
          InterventionDrawPile = [ for _ in 1 ..14 do
                                        Green
                                        Red ]
          InterventionDiscardPile = []
                                        
          Situations = situations }

    type WitnessStrategy = Strategy list -> Player -> (Player * WitnessIntervention) option
    type ResponseStrategy = Player -> Player -> Game -> InterventionCard
    type EndStrategy = Game -> bool

    type GameStrategies =
        { WitnessStrategy: WitnessStrategy
          ResponseStrategy: ResponseStrategy
          EndStrategy: EndStrategy
           }

    let witnessStrategy playPercent otherStrategies (witness: Player) =
        if Random.Shared.Next(0,100) < playPercent then
            let choice =
                    if Random.Shared.Next(2) = 0 then
                        Green
                    else
                        Red
            match choice with
            | Green -> Some (witness,Support)
            | Red ->

                let chosen = Random.Shared.Next(List.length otherStrategies)
                if chosen >= List.length otherStrategies then
                    // there is no other strategies to chose from
                    None
                else
                    Some (witness, BeCareful (otherStrategies[chosen]))
        else
            None

    let responseStrategy (witness: Player) (player: Player) (game: Game) =
        [ for _ in 1 .. player.Interventions.Green do
            Green
          for _ in 1 .. player.Interventions.Red do
            Red
        ]
        |> List.sortBy (fun _ -> Random.Shared.Next())
        |> List.head

    let responseStrategy3 probaAccept probaAccept2 systemBlock (witness: Player) (player: Player) (game: Game) =
        if player.Interventions.Green = 0 then
            Red
        elif player.Interventions.Red = 0 then
            Green
        else
            let p =
                if game.System >= systemBlock then
                    probaAccept2
                else
                    probaAccept

            if Random.Shared.Next(0,100) <= p then
                Green
            else
                Red

    type InterventionState =
        | NoIntervention of  witnesses: Player list
        | Intervened of  InterventionResponse

    let rec playSituation' (gameStrategies: GameStrategies) (situation: Situation) player (interventionState: InterventionState) (strategies: Strategy list) score steps game =

        let chosenIndex = Random.Shared.Next(strategies.Length)
        let originalyChosen = strategies[chosenIndex]

        let chosen, interventionState =
            match interventionState with
            | NoIntervention witnesses ->

                let otherStrategies = strategies |> List.filter (fun s -> s <> originalyChosen)

                let intervention =
                    witnesses
                    |> List.choose (fun w ->
                        try
                            gameStrategies.WitnessStrategy otherStrategies w
                        with
                        | ex -> raise (Exception($"No strategy left in situation {situation.Id} {situation.Title}"))
                            )
                    |> List.sortBy (fun _ -> Random.Shared.Next())
                    |> List.tryHead
                
                let proposed, witness =
                    match intervention with
                    | None -> None, None
                    | Some (witness,Support) -> None, Some ({ witness with Player.Interventions.Green = witness.Interventions.Green - 1 }, Support)
                    | Some (witness, BeCareful strategy ) -> Some strategy, Some ({ witness with Player.Interventions.Red = witness.Interventions.Red - 1}, BeCareful strategy)


                let response =
                    match witness with
                    | None -> None
                    | Some (w,_) -> Some (
                            try
                                gameStrategies.ResponseStrategy w player game
                            with
                            | ex -> raise (Exception($"Error at situation {situation.Id} {situation.Title}",ex))
                                )

                let chosen =
                    match proposed, response with
                    | Some p, Some Green -> p
                    | _ -> originalyChosen
                chosen, 
                    match response, witness with
                    | Some Green, Some (w, i) -> Intervened (ThankYou(w,i))
                    | Some Red, Some (w, i) -> Intervened (MindYourOwnBusiness(w,i))
                    | None, _ -> interventionState
                    | _ -> failwith "Unknown case"
            | Intervened _ -> 
                originalyChosen, interventionState

        let alea = Random.Shared.Next(10) + 1

        let consequence = chosen.Consequences |> List.find (fun c -> Range.contains alea c.Range)

        let step = { Strategy = chosen
                     OriginalStrategy = originalyChosen
                     Response = match interventionState with
                                | NoIntervention _ -> None
                                | Intervened r -> Some r
        
                  ; Alea = alea}

        match consequence.Score with
        | Score (Some (n, ""), []) -> score + n, interventionState, List.rev (step::steps)
        | Score (Some (n, "ET choisis une autre Stratégie"), escalades)
        | Score (Some (n, "OU choisis une autre Stratégie"), escalades)
        | Score (Some (n, "ET choisis une autre Escalade"), escalades) ->
            playSituation' gameStrategies situation player interventionState (List.filter (fun s -> s <> chosen) strategies @ [ for e in escalades -> situation.Escalades[e]] ) (score + n) (step :: steps) game
        | Score (None, escalades) ->
            playSituation' gameStrategies situation player interventionState [ for e in escalades -> situation.Escalades[e]] score (step :: steps) game
        | s -> failwith $"Unknown score %A{s}"



    let applyScore player score intervention game =
        match intervention with 
        | NoIntervention _ -> 
            if score >= 0 then
                { game with Table = game.Table |> Table.updatePlayer player.Id (fun p -> { p with Confidence = player.Confidence + score}) }
            else
                { game with System = game.System + abs score}
        | Intervened (ThankYou(witness,i)) ->
            if score >= 0 then
                { game with
                    Table =
                        game.Table
                        |> Table.updatePlayer player.Id (fun p -> { p with Confidence = p.Confidence + score + 2 })
                        |> Table.updatePlayer witness.Id (fun p -> { p with Confidence = p.Confidence + 2 }) 
                    System = max 0 (game.System - score) }
            else
                { game with 
                    System = game.System + abs score + 1 }
        | Intervened (MindYourOwnBusiness(witness,i)) ->
            { game with
                Table =
                    game.Table
                    |> Table.updatePlayer player.Id (fun p -> { p with Confidence =  p.Confidence + if score >= 0 then  score else 0 })
                    |> Table.updatePlayer witness.Id (fun p -> { p with Confidence = p.Confidence + if score <= 0 then abs score else 0 })
                System = game.System + 1 }

    let drawInterventions player intervention game =
        match intervention with 
        | NoIntervention _ -> game
        | Intervened response ->
            let playerCard, witnessIntervention, witness =
                match response with
                | ThankYou (w, wi) -> Green, wi, w
                | MindYourOwnBusiness(w,wi) -> Red, wi, w
            let witnessCard =
                match witnessIntervention with
                | Support -> Green
                | BeCareful _ -> Red

            let discardPile = playerCard :: witnessCard :: game.InterventionDiscardPile
            let pi,wi, drawPile, discardPile =
                match game.InterventionDrawPile with
                | x :: y :: tail -> x, y, tail, discardPile
                | tail -> 
                    let newDiscardPile = tail @ discardPile |> List.sortBy (fun _ -> Random.Shared.Next())
                    match newDiscardPile with
                    | x :: y :: tail ->
                        x,y, tail, []
                    | _ -> failwith "Not enough cards"

            { game with
                Table =
                    game.Table
                    |> Table.updatePlayer player.Id (fun p -> { p with Player.Interventions = p.Interventions |> Interventions.discard playerCard |> Interventions.take pi })
                    |> Table.updatePlayer witness.Id (fun p -> { p with Player.Interventions = witness.Interventions |> Interventions.take wi }) 
                InterventionDrawPile = drawPile
                InterventionDiscardPile = discardPile }

    let playSituation gameStrategies game =
        let situation = game.Situations |> List.head
        let player = Table.current game.Table
        let witnesses = Table.witnesses game.Table
        let score, intervention, steps = playSituation' gameStrategies situation player (NoIntervention witnesses) situation.Strategies 0 [] game
        applyScore player score intervention game  
        |> drawInterventions player intervention, steps

    let next game =
        { game with Table = Table.next game.Table
                    Turn = game.Turn + 1
                    Situations = 
                        match game.Situations with
                        | head :: tail -> tail @ [head]
                        | _ -> failwith "Nope"}

    let finished systemEnd playerEnd game =
        game.System >= systemEnd || (game.Table.Players |> Array.exists (fun p -> p.Confidence >= playerEnd))

    let finishedWithBlocking systemBlock systemEnd playerEnd game =
        game.System >= systemEnd || (game.System < systemBlock && (game.Table.Players |> Array.exists (fun p -> p.Confidence >= playerEnd)))
    let run gameStrategies situations players =
        let rec loop game =
            if gameStrategies.EndStrategy game then
                game
            else
                let game, steps = playSituation gameStrategies game 
                loop (next game)


        loop (empty situations |> shuffle |> addPlayers players)


    // let turn game =
    //     match game.Situations with
    //     | situation :: rest ->


            
    //         match consequence.Score with
    //         |
        
    //     | _ -> failwith "No sitation left"

let game =
    Game.empty champigny
    |> Game.shuffle
    |> Game.addPlayers 4

game.Table.Players
fsi.PrintDepth <- 5

let gameStrategies = 
    { Game.WitnessStrategy = Game.witnessStrategy 50
      Game.ResponseStrategy =  Game.responseStrategy3 50 90 7 //Game.responseStrategy
      Game.EndStrategy = Game.finishedWithBlocking 7 10 10}

let printGame (game: Game)= 
    [
        sprintf "Turn: %d" game.Turn
        sprintf "System Score: %d" game.System
        let current = Table.current game.Table
        for p in game.Table.Players do
            let pfx = 
                if p.Id = current.Id then
                    "> "
                else
                    "  "
            let ints =
                [ for _ in 1 .. p.Interventions.Green do
                    "G" 
                  for _ in 1 .. p.Interventions.Red do
                    "R"
                ] |> String.concat ""
            sprintf "%s Player %d: %d %s" pfx p.Id p.Confidence ints
    ] |> String.concat "\n"

fsi.AddPrinter printGame


let game, steps = Game.playSituation gameStrategies game
let game = Game.next game
game.InterventionDrawPile
game.InterventionDiscardPile

let games =
    [ for i in 0 .. 100000 do
        Game.run gameStrategies champigny 4
    ]

#r "nuget: XPlot.Plotly"

open XPlot.Plotly
let systemMax = 10
let systemWin = games |> List.filter (fun g -> g.System >= systemMax) |> List.length 
let playersWin = games |> List.filter (fun g -> g.System < systemMax) |> List.countBy (fun g -> g.Table.Players |> Array.maxBy (fun g -> g.Confidence) |> _.Id) |> Map.ofList
[games  |> List.filter (fun g -> g.System >= systemMax) |> List.countBy  (fun g -> g.Turn) |> List.sortBy fst
 for pId in 0 .. Map.count playersWin - 1 do
    games  |> List.filter (fun g -> g.System < systemMax) |> List.filter (fun g -> g.Table.Players |> Array.maxBy (fun p -> p.Confidence) |> fun p -> p.Id = pId ) |>  List.countBy  (fun g -> g.Turn) |> List.sortBy fst
] |> Chart.Column |> Chart.WithLabels ([$"System ({systemWin})"; for pId,_ in Map.toSeq playersWin do $"Players {pId} ({playersWin[pId]})"]) |> Chart.WithLayout (Layout(barmode = "stack")) |> Chart.WithTitle "Turns historgram" |> Chart.Show


games |> List.filter (fun g -> g.System < 15) |> List.countBy (fun g -> g.System) |> Chart.Column |> Chart.WithTitle "System score when player wins"  |> Chart.Show
games |> List.filter (fun g -> g.System >= 15) |> List.countBy (fun g -> g.Table.Players |> Array.maxBy _.Confidence |> _.Confidence) |> Chart.Column |> Chart.WithTitle "Best player score when system wins" |> Chart.Show


let g= 
    Game.empty champigny
    |> Game.addPlayers 4

let t = g.Table
t |> Table.current |> _.Id
t |> Table.observer |> _.Id
t |> Table.witnesses |> List.map _.Id
let t = t |> Table.next


