type Colors =
    | Blue
    | Red
    | Yellow
    | Purple
    | Green

type FontStyle =
    | Regular
    | Italic
    | Bold

type Situation =
    { Id: int
      Number: int
      Title: string
      Color: Colors
      Illustration: int
      Text: (FontStyle * string) list list
      Strategies: Strategy list
      Escalades: Map<char, Strategy> }
and Strategy =
    { Title: string
      Text: (FontStyle * string) list list
      Consequences: Consequence list }
and Consequence =
    { Range: Range
      Title: string
      Text: (FontStyle * string) list
      Score: Score }
and Range = { Min: int; Max: int}
    with
        member this.Count = this.Max + 1 - this.Min
        member this.Percents = (this.Max + 1 - this.Min) * 10
        member this.Probability = decimal this.Percents / 100m
and Score =
    | Score of (int * string) option * char list