#load "model.fsx" "printf.fsx"
open Model
open Printf


let rec path (situation: Situation) (strategies: Strategy list)  =

    [ 
        for strategy in strategies do
            for c in strategy.Consequences do
                match c.Score with
                | Score(Some(n,""),[]) -> n
                | Score(Some(_,"ET choisis une autre Stratégie"), escalades)
                | Score(Some(_,"ET choisis une autre Escalade"), escalades) ->
                    let other = strategies |> List.filter (fun s -> s <> strategy)
                    yield! path situation (other @ [ for e in escalades -> situation.Escalades[e] ])
                | Score(None, escalades) ->
                        yield! path situation [ for e in escalades -> situation.Escalades[e] ]
                | s -> failwith $"Unknown score {s}"
    ]

let rec strategyScoreSystem  s (escalades: Map<char,Strategy>) (strategies: Strategy list) (strategy: Strategy) = 
    [ for cons in strategy.Consequences do
            match cons.Score with
            | Score(Some(n,""),[]) ->
                if n = s then
                    cons.Range.Probability 
                else
                    0m
            | Score(None,[]) ->
                if s = 0 then
                    cons.Range.Probability 
                else
                    0m
            | Score(Some(n,l),es) -> 
                let newSituations =  (List.filter (fun s -> s <> strategy) strategies) @ [ for e in es -> escalades[e] ]
                cons.Range.Probability * (situationScoreSystem s escalades newSituations )
            | Score(None, es) ->
                cons.Range.Probability * situationScoreSystem s escalades [ for e in es -> escalades[e] ]
    ]
    |> List.sum

and situationScoreSystem s (escalades: Map<char,Strategy>) (strategies: Strategy list) : decimal =
    if List.isEmpty strategies then
        failwith "❌ Plus de stratégies"
    
    [ for strategy in strategies do
            strategyScoreSystem s escalades strategies strategy
    ]
    |> List.average 

let scoreSystem s (situation: Situation) =
    try
        situationScoreSystem s situation.Escalades situation.Strategies
    with
    | ex ->
        printfn $"❌ Erreur calcul score situation S{situation.Id} {situation.Title}"
        0m

let scoreSystemAll s (situations: Situation list) =
    [ for situation in situations do
        scoreSystem s situation
    ] |> List.average


let printStats situations =
    let pathCount =
        [ for situation in situations do
            yield! path situation situation.Strategies 
        ] |> List.length

    // count total number of consequences
    let consequenceCount =
        [ for situation in  situations do
            for strategy in situation.Strategies do
                strategy.Consequences.Length
            for _,escalade in situation.Escalades |> Map.toSeq do
                escalade.Consequences.Length
        ] |> List.sum


    let strategyCount = 
        situations |> List.sumBy (fun s -> s.Strategies.Length)
    let escaladeCount =
        situations |> List.sumBy (fun s -> s.Escalades.Count)

    printfn ""
    cprintfn PColors.orange "📊 Stats"
    printfn ""
    printf "situations"
    cprintfn PColors.green $"{List.length situations}"
    printf "strategies:    "
    cprintfn PColors.green $"{strategyCount}"
    printf "escalades:     "
    cprintfn PColors.green $"{escaladeCount}"
    printf "consequences: "
    cprintfn PColors.green $"{consequenceCount}"
    printf "chemins:       "
    cprintfn PColors.green $"{pathCount}"
    printfn ""


    printfn "💥 score system"
    
    for systemScore in -3 .. 3 do
        printfn $"Score {systemScore}: %.2f{scoreSystemAll systemScore situations}"

    