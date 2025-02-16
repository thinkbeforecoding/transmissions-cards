#load "model.fsx" "printf.fsx"
open Model
open Printf

let cut len (s: string) =
    if s.Length >= len-1 then
        s.Substring(0,len-1) + "…"
    else
        s


let situationCards situation =
    1 + situation.Strategies.Length + situation.Escalades.Count


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



let checkStrategyScores (situation: Situation) =
    for strategy in situation.Strategies do
        let score = strategyScore situation.Escalades situation.Strategies strategy
        if abs score > 1.21m then
            cprintfn PColors.gray $"  ⚡ Score déséquilibré %.2f{score} { cut 40 strategy.Title}"
        if (strategy.Consequences |> List.forall (fun c -> match c.Score with Score(_,[]) -> true | _ -> false ) ) then
            if (strategy.Consequences |> List.filter (fun c -> match c.Score with Score(Some(n,_),_) when n > 0 -> true | _ -> false ) |> List.length) = 1 then
                cprintfn PColors.gray $"  ⚡ 1 seul positif %.2f{score} { cut 40 strategy.Title}"
            if (strategy.Consequences |> List.filter (fun c -> match c.Score with Score(Some(n,_),_) when n < 0 -> true | _ -> false ) |> List.length) = 1 then
                cprintfn PColors.gray$"  ⚡ 1 seul negatif %.2f{score} { cut 40 strategy.Title}"

let check (situations: Situation list) =
    let checks =
        [
            for situation in situations |> Seq.sortBy (fun s -> s.Id) do
                let errors = ResizeArray()
                let warn (s:string) =
                    errors.Add("\x1b[33m" + s + "\x1b[0m")

                let checkRanges name strategy =
                    if strategy.Consequences = [] then
                        warn $"  [{name}] consequences manquantes \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"
                    let ranges =
                        [ for cons in strategy.Consequences do
                            yield! [cons.Range.Min .. cons.Range.Max] ]
                    if List.contains 0 ranges then
                        warn $"  [{name}] pourcentage non spécifée \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"

                    let toHigh = ranges |> List.filter (fun x -> x > 10)

                    for n in toHigh do
                        warn $"  [{name}] pourcentage non trop haut ({n}) \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"

                    let missing = (set [1..10] - set ranges)
                    if not missing.IsEmpty then
                        if missing = set [1..10] then
                            warn $"  [{name}] pourcentages manquants \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"
                        else
                            let m = missing |> Seq.map string |> String.concat ", "
                            warn $"  [{name}] pourcentages {m} manquants \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"
                    let multi = ranges |> List.countBy id |> List.filter (fun (_,c) -> c > 1) |> List.map fst
                    if not multi.IsEmpty then
                        for n in multi do
                            warn $"  [{name}] valeur {n} multiple \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"

                let checkScores name (strategy: Strategy)  =
                    let duplicates =
                        strategy.Consequences
                        |> List.choose (fun c -> 
                            match c.Score with
                            | Score(Some(n), []) -> Some n
                            | _ -> None)
                        |> List.countBy id
                        |> List.filter (fun (_,c) -> c > 1)
                    for (score, txt), _ in duplicates do
                            warn $"  [{name}] Score {score} {txt} multiple \x1b[38;2;128;128;128m/ { cut 40 strategy.Title}"


                if situation.Text = [] then
                    warn "  texte situation manquant"
                if situation.Strategies = [] then
                    warn "  stratégies manquantes"
                else
                    for i,strategy in situation.Strategies |> Seq.indexed do
                        if strategy.Text = [] then
                            warn $"  [stratégies {i+1}] texte manquant"
                        checkRanges $"stratégie {i+1}" strategy
                        checkScores $"stratégie {i+1}" strategy

                        for cons in strategy.Consequences do
                            match cons.Score with
                            | Score(None, []) ->
                                    warn $"  [stratégie {i+1}] score manquant \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"
                            | Score(None, es) -> 
                                for k in es do
                                    if not (Map.containsKey k situation.Escalades) then
                                        warn $"  [stratégie {i+1}] escalade non trouvée {k} \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"
                            | Score(Some(n,txt),es) ->
                                if n > 3 then
                                    warn $"  [stratégie {i+1}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"
                                elif n < -3 then
                                    warn $"  [stratégie {i+1}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"

                                match txt with
                                | ""
                                | "ET choisis une autre Stratégie"
                                | "OU choisis une autre Stratégie" -> ()
                                | _ -> warn $"  [stratégie {i+1}] text score inconnu ({txt}) \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"

                                for k in es do
                                    if not (Map.containsKey k situation.Escalades) then
                                        warn $"  [stratégie {i+1}] escalade non trouvée {k} \x1b[38;2;128;128;128m/ {cut 40 cons.Title}"
                    for k,escalade in situation.Escalades |> Map.toSeq do
                        if escalade.Text = [] then
                            warn $"  [escalade {k}] texte manquant"
                        checkRanges $"escalade {k}" escalade
                        checkScores $"escalade {k}" escalade
                        
                        for c in escalade.Consequences do
                            match c.Score with
                            | Score(Some(n,txt),_) ->
                                if n > 3 then
                                    warn $"  [escalade {k}] score trop grand ({n}) \x1b[38;2;128;128;128m/ {cut 40 c.Title}"
                                elif n < -3 then
                                    warn $"  [escalade {k}] score trop petit ({n}) \x1b[38;2;128;128;128m/ {cut 40 c.Title}"

                                match txt with
                                | ""
                                | "ET choisis une autre Escalade"
                                | "OU choisis une autre Escalade" -> ()
                                | "ET choisis une autre Stratégie"
                                | "OU choisis une autre Stratégie" -> ()
                                | _ -> warn $"  [escalade {k}] text score inconnu ({txt}) \x1b[38;2;128;128;128m/ {cut 40 c.Title}"
                            | Score(None,[]) ->
                                    warn $"  [escalade {k}] score manquant \x1b[38;2;128;128;128m/ {cut 40 c.Title}"

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

                            warn $"  escalades {e} non utilisées \x1b[38;2;128;128;128m/ {cut 40 escalade.Title }"
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
            printf "✅ S%d %s " situation.Id title
            cprintfn PColors.gray "(%d stratégies / %d escalades / %d cards) \x1b[32m(score %.2f)"  situation.Strategies.Length situation.Escalades.Count cards score
            checkStrategyScores situation
        | Error errors when situation.Id <> 0 ->
            printf "❌ S%d %s " situation.Id title
            cprintfn PColors.gray "(%d stratégies / %d escalades / %d cards)\x1b[0m" situation.Strategies.Length situation.Escalades.Count cards
            for error in errors do
                printfn $"%s{error}"
        | _ -> ()


    [ for situation,result in checks do
        match result with
        | Ok _ -> situation
        | Error _ -> ()
    ]