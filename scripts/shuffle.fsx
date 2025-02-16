#load "model.fsx"
open System
open Model



let shuffleConsequences (rand: Random) (conseqs: Consequence list) =
    let newList = conseqs |> List.sortBy (fun _ -> rand.Next() )
    
    let rec reattribute i (conseqs: Consequence list) result =
        match conseqs with
        | [] -> result |> List.rev
        | conseq :: rest ->
            let count = conseq.Range.Count
            let newRange = { Min = i; Max = i + count - 1}
            reattribute (i+count) rest ({conseq with Range = newRange} :: result )
    reattribute 1 newList []

let shuffleAll situations =
    let rand = Random(12345)

    situations
    |> List.map (fun (situation: Situation) ->
        { situation with
                Strategies = 
                
                    situation.Strategies
                    |> List.map (fun strategy ->
                        { strategy 
                            with 
                                Consequences = shuffleConsequences rand strategy.Consequences }
                    )
                Escalades =
                    situation.Escalades
                    |> Map.map (fun _ escalade ->
                        { escalade with
                            Consequences = shuffleConsequences rand escalade.Consequences }
                    )
        }
    )



