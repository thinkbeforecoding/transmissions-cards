#load "model.fsx" "qr.fsx"
#r "nuget: Feliz.ViewEngine"


open Model
open Feliz.ViewEngine


let colorProp = function
    | Blue -> "blue"
    | Red -> "red"
    | Green -> "green"
    | Yellow -> "yellow"
    | Purple -> "purple"

type Discrimination =
| Handicap
| Gender
| SocialClass
| SkinColor
| SexualOrientation

type Card =
| Alea of int
| Situation of  Situation
| Strategy of situationNumber:int * Situation * Strategy
| Escalade of char * situationNumber:int * Situation * Strategy
| Empty 
| Help
| SystemHelp
| Support
| Warning
| Discrimination of Discrimination

let pos n =
    let c = 1+n%3;
    let r = 1+n/3;
    $"c{c} r{r}"

let inclusiveRx = System.Text.RegularExpressions.Regex(@"\w+·\w+(·\w+)?")
let inclspan (prefix: string) (fem: string) (masc: string) =
    [
        Html.span [
            prop.className "inclusive"
            prop.children [
                if prefix.Length > 0 then
                    Html.text prefix
                Html.span [
                    prop.className "supsub"
                    prop.children [
                        Html.sup fem
                        Html.sub (if masc <> "" then masc else "\xA0")
                    ]
                ] 
            ]
        ]
    ]
let (|EndsWith|_|) (str: string) (input: string) = 
    if input.EndsWith(str) then
        Some(input.Substring(0,input.Length-str.Length))
    else
        None

let rec inclusive' (text: string) pos (matches: System.Text.RegularExpressions.Match list) =
    [
        match matches with
        | [] -> 
            let remaining = text.Substring(pos)
            if remaining <> "" then
                Html.text remaining
        | m :: tail -> 
            if m.Index > pos then
                Html.text (text.Substring(pos, m.Index - pos))
            match m.Value with
            | EndsWith "·e" prefix ->  
                yield! inclspan prefix "e" ""
            | EndsWith "·e·s" prefix 
            | EndsWith "·es" prefix ->
                yield! inclspan prefix "es" "s"
            | EndsWith "x·se" prefix ->
                yield! inclspan prefix "se" "x"
            | EndsWith "eur·ice" prefix ->
                yield! inclspan prefix "rice" "eur"
            | EndsWith "eur·ice·s" prefix ->
                yield! inclspan prefix "rices" "eurs"
            | EndsWith "r·se" prefix->
                yield! inclspan prefix "se" "r"
            | EndsWith "r·se·s" prefix
            | EndsWith "r·ses" prefix->
                yield! inclspan prefix "ses" "rs"
            | EndsWith "le·a" _
            | EndsWith "le·la" _ ->
                yield! inclspan "l" "a" "e"
            | EndsWith "Le·la" _ -> 
                yield! inclspan "L" "a" "e"
            | EndsWith "ton·ta" _ -> 
                yield! inclspan "t" "a" "on"
            | EndsWith "s·tes" prefix ->
                yield! inclspan prefix "tes" "s"
            | EndsWith "seul·e" _ ->
                yield! inclspan "seul" "e" "" 
            | EndsWith "il·elle" _ ->
                yield! inclspan "" "elle" "il"
            | EndsWith "Il·Elle" _ 
            | EndsWith "Il·elle" _ ->
                yield! inclspan "" "Elle" "Il"
            | EndsWith "·ne" prefix ->
                yield! inclspan prefix "ne" ""
            | _ ->
                printfn $"❌ {m.Value}"
                Html.text m.Value
            yield! inclusive' text (m.Index + m.Length) tail
            
    ]

let inclusive (text: string) =
    let matches = inclusiveRx.Matches(text) |> Seq.toList
    inclusive' text 0 matches

let renderLine line =
    [ for style, text in line do
        match style with
        | Regular -> yield! (inclusive text)
        | Italic -> Html.em (inclusive text)
        | Bold -> Html.strong (inclusive text) ]

let renderDescription text =
            Html.div [
                prop.className "description"
                prop.children [
                    for line in text do
                        Html.p (renderLine line)
                ]
            ]

let renderSituationRecto n (situation: Situation) =

    Html.div [
        prop.className $"card recto situation { colorProp situation.Color } {pos n}"
        if System.IO.File.Exists($"./cards/img/illustrations/situation-{situation.Illustration}.webp") then
            prop.style [style.custom("--illustration", $"url(/img/illustrations/situation-{situation.Illustration}.webp)") ]
        prop.children [
            Html.h1 $"Situation {situation.Number}"
            renderDescription situation.Text
        ]
    ]

let renderSituationVerso n id  =
    Html.div [
        prop.className $"card verso situation {pos n}"
        prop.children [
            Html.div [ prop.className "logo-champigny"]
            Html.rawText (Qr.situation id)
        ]
    ]

let renderStrategyRecto (n: int) (r: int) key (situation: Situation) (strategy: Strategy) =
    Html.div [
        let cls = match key with None -> "strategy" | Some _ -> "escalade"
        prop.className $"card recto {cls} {colorProp situation.Color } {pos n}"
        // match key with
        // | None ->
        if key.IsSome && System.IO.File.Exists($"./cards/img/illustrations/strategie-e-{situation.Illustration}.webp") then
            prop.style [style.custom("--illustration", $"url(/img/illustrations/strategie-e-{situation.Illustration}.webp)") ]
        // | Some _ -> ()
        elif System.IO.File.Exists($"./cards/img/illustrations/strategie-{situation.Illustration}.webp") then
                prop.style [style.custom("--illustration", $"url(/img/illustrations/strategie-{situation.Illustration}.webp)") ]
        // | Some _ -> ()

        prop.children [
            Html.h1 (
                match key with
                | None -> $"Stratégie {situation.Number}"
                | Some c -> $"Escalade {situation.Number} {c}")
            renderDescription strategy.Text
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
let scoreText txt =
    if txt = "" then
        txt
    else
         " " + txt


let startWithSpace (consequence: Consequence) = 
    if consequence.Text.Length = 0 then
        false
    else
        let _,txt = consequence.Text[0]
        txt.Length > 0 && match txt[0] with '\xA0' | ' ' -> true | _ -> false

let endsWithSpace (consequence: Consequence) =
    if consequence.Text.Length = 0 then
        false
    else
        let _,txt = consequence.Text[0]
        txt.Length > 0 && match txt[txt.Length-1] with '\xA0' | ' ' -> true | _ -> false


let renderStrategyVerso n key (situation: Situation) (strategy: Strategy) =
    Html.div [
        let cls = match key with None -> "strategy" | Some _ -> "escalade"
        prop.className $"card verso {cls} {colorProp situation.Color } {pos n}"
        // match key with
        // | None ->
        if key.IsSome && System.IO.File.Exists($"./cards/img/illustrations/consequences-e-{situation.Illustration}.webp") then
            prop.style [style.custom("--illustration", $"url(/img/illustrations/consequences-e-{situation.Illustration}.webp)") ]
        elif System.IO.File.Exists($"./cards/img/illustrations/consequences-{situation.Illustration}.webp") then
            prop.style [style.custom("--illustration", $"url(/img/illustrations/consequences-{situation.Illustration}.webp)") ]
        // | Some _ -> ()

        prop.children [
            Html.h1 (
                match key with
                | None -> $"Conséquences {situation.Number}"
                | Some c -> $"Conséquences {situation.Number} {c}")
            Html.div [
                prop.className "consequences"
                prop.children [
                    for consequence in strategy.Consequences do
                        Html.p [
                            Html.span [
                                prop.className "dice"
                                if consequence.Range.Min = consequence.Range.Max then
                                    prop.text $"{consequence.Range.Min}\xA0:"
                                else
                                    prop.text $"{consequence.Range.Min} à {consequence.Range.Max}\xA0:"
                            ]
                            if (not (startWithSpace consequence)) then
                                Html.text " "
                            yield! renderLine consequence.Text

                            Html.span [
                                let space = 
                                    if endsWithSpace consequence then "" else " "
                                match consequence.Score with
                                | Score (Some(score,txt),ids) when score > 0 ->
                                    prop.className "score player"
                                    prop.text $"%s{space}(J%d{score}%s{scoreText txt}%s{plusEscalade ids})\n"
                                | Score(Some(score,txt), ids) when score < 0 ->
                                    prop.className "score system"
                                    prop.text $"%s{space}(S%d{-score}%s{scoreText txt}%s{plusEscalade ids})\n"
                                | Score (Some(_,txt), ids) ->
                                    prop.className "score zero"
                                    prop.text $"%s{space}(0%s{scoreText txt}%s{plusEscalade ids})\n"
                                | Score(None, ids) ->
                                    prop.className "score escalade"
                                    let list = ids |> List.map string |> String.concat ""

                                    prop.text $"%s{space}(Escalade %s{list})\n"
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
        prop.children [ 
            Html.div [
                prop.className "logo-champigny"
            ]
        ]
    ]

let renderHelpRecto i =
    Html.div [
        prop.className $"card recto help {pos i}"
    ]

let renderHelpVerso i  =
    Html.div [
        prop.className $"card verso help {pos i}"
    ]

let renderSystemHelpRecto i =
    Html.div [
        prop.className $"card recto system {pos i}"
    ]

let renderSystemHelpVerso i  =
    Html.div [
        prop.className $"card verso system {pos i}"
    ]

let renderSupportRecto i =
    Html.div [
        prop.className $"card recto soutien {pos i}"
    ]

let renderSupportVerso i  =
    Html.div [
        prop.className $"card verso soutien {pos i}"
        prop.children [ 
            Html.div [
                prop.className "logo-champigny"
            ]
        ]
    ]

let renderWarningRecto i =
    Html.div [
        prop.className $"card recto attention {pos i}"
    ]

let renderWarningVerso i  =
    Html.div [
        prop.className $"card verso attention {pos i}"
        prop.children [ 
            Html.div [
                prop.className "logo-champigny"
            ]
        ]
    ]

let renderDiscriminationRecto i discrimination =
    let disc =
        match discrimination with
        | Handicap -> "handicap"
        | Gender -> "genre"
        | SocialClass -> "classe-sociale"
        | SkinColor -> "couleur"
        | SexualOrientation -> "orientation"

    Html.div [
        prop.className $"card recto norm {disc} {pos i}"
    ]

let renderDiscriminationVerso i  =
    Html.div [
        prop.className $"card verso norm {pos i}"
        prop.children [ 
            Html.div [
                prop.className "logo-champigny"
            ]
        ]
    ]

let stylesheets cards =
    cards
    |> Seq.choose (function
        | Help 
        | SystemHelp -> Some "/stylesheets/aides-de-jeu.css"
        | Support 
        | Warning -> Some "/stylesheets/intervention.css"
        | _ -> None
    )
    |> Seq.distinct

let cropmarks =
    [ Html.div [ prop.className "mark m-left m-top"]
      Html.div [ prop.className "mark m-right m-top"]
      Html.div [ prop.className "mark m-left m-bottom"]
      Html.div [ prop.className "mark m-right m-bottom"] 
      ]
let alignmarks =
    [ Html.div [ prop.className "align-mark am-top"]
      Html.div [ prop.className "align-mark am-bottom"] ]

let render (title: string) classes (cards: Card list) =
    Html.html [
        prop.lang "fr"
        prop.children [
            Html.head [
                Html.meta [ prop.charset "utf-8" ]
                Html.title [ prop.text title ]
                Html.link [prop.href "/stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
                Html.link [ prop.href "/stylesheets/cards.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
                for stylesheet in stylesheets cards do
                    Html.link [ prop.href stylesheet; prop.rel "stylesheet"; prop.type' "text/css" ]


                Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
                Html.script [ prop.src "/js/anchor.js" ]
            ]
            Html.body [
                prop.classes classes
                prop.children [
                    for page in cards |> List.chunkBySize 9 do
                        Html.section [
                            prop.className "recto"
                            prop.children [
                                // Divs for the cricut cut marks
                                yield! cropmarks
                                yield! alignmarks

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
                                    | Empty -> ()
                                    | Help -> renderHelpRecto n
                                    | SystemHelp -> renderSystemHelpRecto n
                                    | Support -> renderSupportRecto n
                                    | Warning -> renderWarningRecto n
                                    | Discrimination d -> renderDiscriminationRecto n d
                            ]
                        ]
                        Html.section [
                            prop.className "verso"
                            prop.children [
                                yield! alignmarks
                                for n,card in List.indexed page do
                                    match card with
                                    | Alea _ ->
                                        renderAleaVerso n
                                    | Situation s ->
                                        renderSituationVerso n s.Id
                                    | Strategy(_,situation, strategy) ->
                                        renderStrategyVerso n None situation strategy
                                    | Escalade(c,_, situation, strategy) ->
                                        renderStrategyVerso n (Some c) situation strategy
                                    | Empty -> ()
                                    | Help -> renderHelpVerso n
                                    | SystemHelp -> renderSystemHelpVerso n
                                    | Support -> renderSupportVerso n
                                    | Warning -> renderWarningVerso n
                                    | Discrimination _ -> renderDiscriminationVerso n
                            ]
                        ]
                    ]
            ]
        ]
    ]
    |> Render.htmlDocument

let renderA7 (cards: Card list) =
    Html.html [
        Html.head [
            Html.meta [ prop.charset "utf-8" ]
            Html.title [ prop.text "Transmission(s)" ]
            Html.link [prop.href "/stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
            Html.link [ prop.href "/stylesheets/cards-a7.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
            Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
        ]
        Html.body [
                for card in cards  do
                    Html.section [
                        prop.className "recto"
                        prop.children [
                            // Divs for the cricut cut marks

                            match card with
                            | Alea x ->
                                renderAleaRecto 0 x
                            | Situation sit ->
                                renderSituationRecto 0 sit
                            | Strategy(r,situation, strategy) ->
                                renderStrategyRecto 0 r None situation strategy
                            | Escalade(c, r, situation, strategy) ->
                                renderStrategyRecto 0 r (Some c) situation strategy
                            | Empty -> ()
                        ]
                    ]
                    Html.section [
                        prop.className "verso"
                        prop.children [
                                match card with
                                | Alea _ ->
                                    renderAleaVerso 0
                                | Situation s ->
                                    renderSituationVerso 0 s.Id
                                | Strategy(_,situation, strategy) ->
                                    renderStrategyVerso 0 None situation strategy
                                | Escalade(c,_, situation, strategy) ->
                                    renderStrategyVerso 0 (Some c) situation strategy
                                | Empty -> ()
                        ]
                    ]
        ]
    ]
    |> Render.htmlDocument

let renderA7recto (cards: Card list) =
    Html.html [
        prop.lang "fr"
        prop.children [
            Html.head [
                Html.meta [ prop.charset "utf-8" ]
                Html.title [ prop.text "Transmission(s)" ]
                Html.link [prop.href "/stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
                Html.link [ prop.href "/stylesheets/cards-a7.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
                Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
            ]
            Html.body [
                    for card in cards  do
                        Html.section [
                            prop.className "recto"
                            prop.children [
                                // Divs for the cricut cut marks

                                match card with
                                | Alea x ->
                                    renderAleaRecto 0 x
                                | Situation sit ->
                                    renderSituationRecto 0 sit
                                | Strategy(r,situation, strategy) ->
                                    renderStrategyRecto 0 r None situation strategy
                                | Escalade(c, r, situation, strategy) ->
                                    renderStrategyRecto 0 r (Some c) situation strategy
                                | Empty -> ()
                            ]
                        ]
                        match card with 
                        | Situation _ -> ()
                        | _ -> 
                            Html.section [
                                prop.className "verso"
                                prop.children [
                                        match card with
                                        | Alea _ ->
                                            renderAleaVerso 0
                                        | Situation s ->
                                            renderSituationVerso 0 s.Id
                                        | Strategy(_,situation, strategy) ->
                                            renderStrategyVerso 0 None situation strategy
                                        | Escalade(c,_, situation, strategy) ->
                                            renderStrategyVerso 0 (Some c) situation strategy
                                        | Empty -> ()
                                ]
                            ]
            ]
        ]
    ]
    |> Render.htmlDocument 

let renderA6 (cards: Card list) =
    Html.html [
        prop.lang "fr"
        prop.children [
            Html.head [
                Html.meta [ prop.charset "utf-8" ]
                Html.title [ prop.text "Transmission(s)" ]
                Html.link [prop.href "/stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
                Html.link [ prop.href "/stylesheets/cards-a6.css"; prop.rel "stylesheet"; prop.type' "text/css" ]
                Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
            ]
            Html.body [
                    for card in cards  do
                        Html.section [
                            Html.div [
                                prop.className "recto"
                                prop.children [
                                    // Divs for the cricut cut marks

                                    match card with
                                    | Alea x ->
                                        renderAleaRecto 0 x
                                    | Situation sit ->
                                        renderSituationRecto 0 sit
                                    | Strategy(r,situation, strategy) ->
                                        renderStrategyRecto 0 r None situation strategy
                                    | Escalade(c, r, situation, strategy) ->
                                        renderStrategyRecto 0 r (Some c) situation strategy
                                    | Empty -> ()
                                ]
                            ]
                            Html.div [
                                prop.className "verso"
                                prop.children [
                                        match card with
                                        | Alea _ ->
                                            renderAleaVerso 0
                                        | Situation s ->
                                            renderSituationVerso 0 s.Id
                                        | Strategy(_,situation, strategy) ->
                                            renderStrategyVerso 0 None situation strategy
                                        | Escalade(c,_, situation, strategy) ->
                                            renderStrategyVerso 0 (Some c) situation strategy
                                        | Empty -> ()
                                ]
                            ]
                        ]
            ]
        ]
    ]
    |> Render.htmlDocument

open System.IO
let save (path: string) (html: string) =
    let dir = System.IO.Path.GetDirectoryName(path)
    if not (Directory.Exists dir) then
        Directory.CreateDirectory(dir) |> ignore

    System.IO.File.WriteAllText(path, html)

