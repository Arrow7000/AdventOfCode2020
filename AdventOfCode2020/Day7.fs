module Day7

module Parser =

    /// Every colour is a string contained two words
    type Colour = Colour of string

    type BagContents = { Colour : Colour; Count : int }

    type BagRule =
        { Colour : Colour
          Bags : BagContents list }




    type BagParser =
        | ColourDescription of string
        | ColourNoun of string * string
        | Bags of Colour

    type CountBagParser =
        | Count of int
        | ColourDescription of int * string
        | ColourNoun of int * string * string
        | Bags of int * Colour

    type BagRuleParserState =
        | NotStartedYet // at start of line
        | Container of BagParser // should go up to after the word 'bags' of the container
        | Contain of Colour // after word 'contain' for container bag
        | ContainedBags of containerClr : Colour * BagContents list * CountBagParser
        | Comma of containerClr : Colour * BagContents list
        | FullStop of containerClr : Colour * BagContents list // and terminate here
        | Failed of reason : string

    let splitIntoWords (str : string) =
        str.Replace(",", " ,") // to ensure breaks for commas
           .Replace(".", " .") // to ensure breaks for fullstops
           .Split " "
        |> List.ofArray



    let parseBag line =
        let words = splitIntoWords line

        words
        |> List.fold
            (fun state word ->
                match state with
                | NotStartedYet ->
                    Container (BagParser.ColourDescription word)
                | Container bagState ->
                    match bagState with
                    | BagParser.ColourDescription descr ->
                        BagParser.ColourNoun (descr, word) |> Container
                    | BagParser.ColourNoun (descr,word) ->
                        BagParser.Bags (Colour <| descr + " " + word) |> Container
                    | BagParser.Bags clr -> Contain clr
                | Contain clr ->
                    ContainedBags (clr, List.empty, Count <| int word)
                | ContainedBags (clr, list, bagState) ->
                    let cont state = ContainedBags (clr, list, state)

                    match bagState with
                    | Count cnt ->
                        ColourDescription (cnt, word) |> cont
                    | ColourDescription (cnt, descr) ->
                        ColourNoun (cnt, descr, word) |> cont
                    | ColourNoun (cnt,descr,word) ->
                        Bags (cnt, (Colour <| descr + " " + word)) |> cont
                    | Bags (cnt,bagClr) ->
                        if word = "," then
                            Comma (clr, {Count=cnt;Colour=bagClr} :: list)
                        else if word = "." then
                            FullStop (clr, {Count=cnt;Colour=bagClr} :: list)
                        else Failed $"""Expected "," or "." but got "{word}" instead"""
                | Comma (clr,bags) -> ContainedBags (clr, bags, Count <| int word)
                | FullStop (clr, bags) -> FullStop (clr, bags)
                | Failed reason -> Failed reason

            )
            NotStartedYet
        |> function
           | FullStop (colour, bags) -> colour, bags
           | Failed reason -> failwith $"Parsing failed because of reason: {reason}"
           | state -> failwith $"Got stuck at state {state}"




let parserTest =
    Parser.parseBag "light magenta bags contain 4 clear bronze bags, 4 dull teal bags, 4 posh salmon bags."



let part1 = parserTest
