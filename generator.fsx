#r "nuget: FSharp.Formatting"
#I "scripts"
#load "update.fsx" "model.fsx" "parsing.fsx" "shuffle.fsx" "render.fsx" "validation.fsx" "stats.fsx" "printf.fsx"

#nowarn "57"

open System
open Model
open Render
open Validation
open Printf

fsi.PrintDepth <- 1
let (</>) x y = System.IO.Path.Combine(x,y)
Update.updateMarkdown Update.champignyUrl "champigny.md"
Update.updateMarkdown Update.genericUrl "generic.md"

// this is the champigny cards starting at 14
let champigny =
    Parsing.parse @"champigny.md"
    |> check
    |> Shuffle.shuffleAll



// this is the champigny cards, starting at 1
let champigny2 = champigny |> List.mapi (fun i s -> {s with Number = i+1})

let generic =
    Parsing.parse "generic.md"
    |> check
    |> Shuffle.shuffleAll
    |> List.mapi (fun i s -> {s with Number = i+1})


let safe = ["safe"; "champigny"]
let cameo = ["champigny"; "cameo" ;"map-printer"]


let helps =
    [ for i in 1 .. 8 do
        Help
      SystemHelp ]

let interventions =
    [ for i in 1 .. 9 do
        Support
      for i in 1 .. 9 do
        Warning ]

let discriminations =
    [  Discrimination SkinColor
       Discrimination SocialClass
       Discrimination SexualOrientation
       Discrimination SkinColor
       Discrimination SocialClass
       Discrimination SexualOrientation
       Discrimination SkinColor
       Discrimination SocialClass
       Discrimination SexualOrientation

       Discrimination Handicap
       Help
       Warning
       Discrimination Handicap
       Help
       Warning
       Discrimination Handicap
       Help
       Warning

       Discrimination Gender
       Help
       Warning
       Discrimination Gender
       Help
       Warning
       Discrimination Gender
       Help
       Warning
     ]


let alea = [ for i in 1 .. 10 do Alea i ]


let renderCards cards path classes =
    let cards =
        [ for situation in cards do
            Situation situation
            for n,strategy in situation.Strategies |> Seq.indexed do
                Strategy (n, situation, strategy)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
        ]


    render classes cards |> save ( path </> "situations.html")
    render classes helps |> save (path </> "aides-de-jeu.html")
    render classes interventions |> save (path </> "interventions.html")
    render classes discriminations |> save ( path </> "discriminations.html" )
    render classes alea |> save ( path </> "alea.html")

renderCards champigny2 "./cards/cameo" cameo

renderCards champigny2 "./cards/champigny" safe

renderCards generic "./cards/cannes" ["safe"]

let situationsA6 =
    champigny
                            //  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29
    // |> List.permute (fun i -> [ 12; 10;  1;  6;  8; 13; 11; 14;  0;  2;  4;  3;  5;  9;  7; 15][i]) 
    |> List.mapi (fun i s -> {s with Number = i+1})

let cardsA6 =
    [ for situation in situationsA6 do
        Situation( situation)
        // for n,strategy in situation.Strategies |> Seq.indexed do
        //     Strategy (n, situation, strategy)
        // for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
        //     Escalade (key, n, situation, escalade)
    ]
renderA6 cardsA6
|> fun html -> System.IO.File.WriteAllText("./cards/champigny-a6.html", html)

let cardsRetry =
    [ for situation in champigny |> List.mapi (fun i s -> {s with Number = i+1}) do
        if situation.Number = 11 then
            Situation( situation)
        for n,strategy in situation.Strategies |> Seq.indexed do
            if (situation.Number = 10 && (n+1) = 4) ||
                (situation.Number = 11 && match (n+1) with 1 | 2 | 3 | 4 -> true | _ -> false ) then
                Strategy (n, situation, strategy)
        for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
            if situation.Number = 11 && key = 'D' then 
                Escalade (key, n, situation, escalade)
                Escalade (key, n, situation, escalade)
                Escalade (key, n, situation, escalade)
    ]

let htmlRetry =
    render safe cardsRetry

System.IO.File.WriteAllText("./cards/champigny-retry.html", htmlRetry)


// SITUATION 1
let situation1 =
    [ for situation in champigny2 do
        if situation.Number = 1 then
            Situation situation
            for n,strategy in situation.Strategies |> Seq.indexed do     
                Strategy (n, situation, strategy)
    ]
    |> List.replicate 6
    |> List.concat
let htmlSituation1 =
    render safe situation1

System.IO.File.WriteAllText("./cards/situation1.html", htmlSituation1)

[ for situation in champigny do
    Situation situation
    for n,strategy in situation.Strategies |> Seq.indexed do
        Strategy (n, situation, strategy)
     ]
|> renderA7 
|> fun html -> System.IO.File.WriteAllText("./cards/champigny-a7.html", html)

[ for situation in champigny do
    if situation.Id <> 14 then
        Situation situation ]
|> renderA7recto 
|> fun html -> System.IO.File.WriteAllText("./cards/champigny-a7-recto.html", html)

[ for situation in champigny do
    if situation.Id <> 14 && situation.Id <> 18 then
        Situation( situation)

  for situation in champigny do
        if situation.Id = 18 then
            Situation( situation)
            for n,strategy in situation.Strategies |> Seq.indexed do
                Strategy (n, situation, strategy)
            for n,(key,escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade (key, n, situation, escalade)
    ]
|> renderA7recto 
|> fun html -> System.IO.File.WriteAllText("./cards/situation18-a7", html)


let aleahtml =
    render safe alea 
System.IO.File.WriteAllText("./cards/alea.html", aleahtml)

alea
|> renderA7 
|> fun html -> System.IO.File.WriteAllText("./cards/alea-a7.html", html)


(* Contournerment de problemes d'imprimante*)
module By2 =
    let strategies = [
        for situation in champigny2 do
            if situation.Number <> 1 then
                Situation situation
                for n,strategy in situation.Strategies |> Seq.indexed do
                    Strategy(n, situation, strategy)
    ]

    let escalades =
        [ for situation in champigny2 do
            for  n, (key, escalade) in Map.toSeq situation.Escalades |> Seq.indexed do
                Escalade(key, n, situation, escalade)
        ]



    let escaladeBy2 = 
        [ for es in List.chunkBySize 2 escalades do
            yield es.Head
            yield Empty
            yield! es.Tail
        ]

    let htmlEscaladesBy2 =
        render safe escaladeBy2


    System.IO.File.WriteAllText("./cards/escalades-by-2.html", htmlEscaladesBy2)



    let strategiesBy2 = ( List.chunkBySize 2 strategies)
    let escaladesFor2 = escalades |> List.take strategiesBy2.Length
    let otherEscalades = escalades |> List.skip strategiesBy2.Length


    let cards =
        [ for ss, e in List.zip strategiesBy2 escaladesFor2 do
            ss.Head
            e
            yield! ss.Tail
            yield! otherEscalades 
        ]


(* Statistics *)

// count total number of paths
Stats.printStats champigny


open Feliz.ViewEngine

type Token =
| Empty
| OnePoint
| FivePoints
| Skills
| Discrimination of Discrimination
| Sympathizer

let tokenPos i =
    let r0 = i % 27
    let c0 = i / 27
    let r,c,alt =
        if r0 < 13 then
            r0, c0*2,false
        else
            r0-13,c0*2+1,true

    if alt then
        $"r{r+1} c{c+1} a"
    else
        $"r{r+1} c{c+1}"

let tokenClass = function
    | Empty -> ""
    | OnePoint -> "onepoint"
    | FivePoints -> "fivepoints"
    | Skills -> "skills"
    | Discrimination Gender -> "gender"
    | Discrimination SocialClass -> "social-class"
    | Discrimination Handicap  -> "handicap"
    | Discrimination SkinColor  -> "color"
    | Discrimination SexualOrientation  -> "orientation"
    | Sympathizer -> "sympathizer"

let renderTokenRecto i token =
    Html.div [
        prop.classes ["token"; tokenClass token; tokenPos i]
    ]

let renderTokenVerso i token =
    Html.div [
        prop.classes ["token"; tokenClass token; tokenPos i]
    ]



let render tokens classes  =
    Html.html [
        prop.lang "fr"
        prop.children [
            Html.head [
                Html.meta [ prop.charset "utf-8" ]
                Html.title [ prop.text "Transmission(s)" ]
                Html.link [prop.href "/stylesheets/interface.css"; prop.rel "stylesheet"; prop.type' "text/css"]
                Html.link [ prop.href "/stylesheets/tokens.css"; prop.rel "stylesheet"; prop.type' "text/css" ]


                Html.script [ prop.src "https://unpkg.com/pagedjs/dist/paged.polyfill.js" ]
                Html.script [ prop.src "/js/anchor.js" ]
            ]
            Html.body [
                prop.classes ("tokens" :: classes)
                prop.children [
                    for group in tokens |> Seq.chunkBySize 148 do
                        Html.section [
                            prop.className "recto"
                            prop.children [
                                // Divs for the cricut cut marks
                                yield! cropmarks
                                yield! alignmarks
                                for i,token in Seq.indexed group do
                                    renderTokenRecto i token
                            ]
                        ]
                        Html.section [
                            prop.className "verso"
                            prop.children [
                                yield! alignmarks
                                for i,token in Seq.indexed group do
                                    renderTokenVerso i token
                            ]
                        ]
                ]
            ]
        ]
    ]
    |> Render.htmlDocument

let discriminationTokens = 
    [  Handicap; SkinColor; SexualOrientation; Gender; SocialClass]
    |> List.map Discrimination
let tokens = 
    [
     for i in 1 .. 60 do OnePoint
     for i in 1 .. 3 do FivePoints
     for i in 1 .. 20 do Sympathizer
     Skills 
     yield! discriminationTokens |> List.collect (List.replicate 2)
     ]
render tokens cameo |> save "./cards/cameo/jetons.html"
render tokens [] |> save "./cards/cannes/jetons.html"