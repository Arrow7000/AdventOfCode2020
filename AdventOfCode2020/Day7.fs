module Day7

/// Every colour is a string contained two words
type Colour = Colour of string

type BagContents = { Colour : Colour; Count : int }

type BagRule =
    { Colour : Colour
      Bags : BagContents list }


module Parser =

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
        | FullStop of BagRule // and terminate here
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
                    if word = "no" then
                        FullStop {Colour = clr; Bags = List.empty } // terminate here
                    else
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
                            FullStop { Colour = clr; Bags = {Count=cnt;Colour=bagClr} :: list}
                        else Failed $"""Expected "," or "." but got "{word}" instead"""
                | Comma (clr,bags) -> ContainedBags (clr, bags, Count <| int word)
                | FullStop state -> FullStop state
                | Failed reason -> Failed reason

            )
            NotStartedYet
        |> function
           | FullStop rule -> rule
           | Failed reason -> failwith $"Parsing failed because of reason: {reason}"
           | state -> failwith $"Got stuck at state {state}"



let input =
    readAllLines "day7.txt"

let allRules =
    input
    |> List.map Parser.parseBag

type BagTree =
    { Colour : Colour
      Children : BagTree list }



let rec getAllColours (tree : BagTree) =
    match tree.Children with
    | [] -> Set.singleton tree.Colour
    | children ->
        children
        |> List.map getAllColours
        |> Set.unionMany
        |> Set.add tree.Colour



let constructBagTree (rules : BagRule list) =
    let map =
        rules
        |> List.map (fun rule -> rule.Colour, rule.Bags |> List.map (fun bag -> bag.Colour))
        |> Map.ofList

    let rec makeTree containerClr =
        let childClrs =
            Map.tryFind containerClr map
            |> Option.defaultValue List.empty

        { Colour = containerClr
          Children = childClrs |> List.map makeTree }

    let trees =
        map
        |> Map.map (fun containerClr _ -> makeTree containerClr)

    let allSubClrs =
        map
        |> Map.fold (fun set _ clrs -> clrs |> Set.ofList |> Set.union set) Set.empty

    let topLevelTrees = 
        trees
        |> Map.filter (fun clr _ -> Set.contains clr allSubClrs |> not)
        |> Map.values
        |> Seq.toList

    topLevelTrees


let rec countNodes (tree : BagTree) =
    1 + (List.map countNodes tree.Children |> List.sum)

let rec countNodesExcept clrToIgnore (tree : BagTree) =
    if tree.Colour = clrToIgnore then 0
    else
        1 + (List.map (countNodesExcept clrToIgnore) tree.Children |> List.sum)

/// Prune all branches that don't ultimately contain a clrToFind
let rec pruneTree clrToFind (tree : BagTree) =
    let fulsomeChildren =
        tree.Children
        |> List.choose (pruneTree clrToFind)

    match fulsomeChildren with
    | [] ->
        if tree.Colour = clrToFind then
            Some { tree with Children = List.empty }
        else None
    | fsc ->
        { tree with Children = fsc } |> Some


let ruleTrees =
    allRules
    |> constructBagTree

let shinyGold = Colour "shiny gold"

let part1 =
    ruleTrees
    |> List.choose
        (pruneTree shinyGold >> Option.map getAllColours)
    |> List.fold Set.union Set.empty
    |> Set.remove shinyGold
    |> Set.count




type Count = Cnt of int | TopLevel


type BagTreeWithCount =
    { Colour : Colour
      Count : Count
      Children : BagTreeWithCount list }


/// So that top level items can be compared
let resetTopLevelCount item =
    { item with BagTreeWithCount.Count = TopLevel }


let constructBagTreeWithCount (rules : BagRule list) : BagTreeWithCount list =
    let map =
        rules
        |> List.map
            (fun rule -> rule.Colour, rule.Bags |> List.map (fun bag -> bag.Colour, bag.Count))
        |> Map.ofList

    let rec makeTree count clr =
        let countsAndClrs =
            Map.tryFind clr map
            |> Option.defaultValue List.empty

        { Colour = clr
          Count = count
          Children = countsAndClrs |> List.map (fun (clr',n) -> makeTree (Cnt n) clr' ) }

    let trees =
        map
        |> Map.map (fun containerClr _ -> makeTree TopLevel containerClr)

    let allSubClrs =
        map
        |> Map.fold (fun set _ clrs -> clrs |> List.map fst |> Set.ofList |> Set.union set) Set.empty

    let topLevelTrees = 
        trees
        |> Map.filter (fun clr _ -> Set.contains clr allSubClrs |> not)
        |> Map.values
        |> Seq.toList
        |> List.map resetTopLevelCount

    topLevelTrees


let rec findSubtrees clr (tree : BagTreeWithCount) =
    if tree.Colour = clr then List.singleton tree
    else
        tree.Children
        |> List.collect (findSubtrees clr)



let rec countBags (tree : BagTreeWithCount) =
    let selfCount =
        match tree.Count with
        | TopLevel -> 1
        | Cnt n -> n

    selfCount + selfCount * (List.map countBags tree.Children |> List.sum)



type Comparison<'a when 'a : comparison> =
    | NotStartedYet
    | Equal of last : 'a
    | NotEqual


let areAllEqual xs =
    xs
    |> List.fold
        (fun state thisOne ->
            match state with
            | NotStartedYet -> Equal thisOne
            | Equal x -> if x = thisOne then Equal thisOne else NotEqual
            | NotEqual -> NotEqual)
        NotStartedYet
    |> function
       | NotStartedYet -> true
       | Equal _ -> true
       | NotEqual -> false

let part2 =
    allRules
    |> constructBagTreeWithCount
    |> List.collect (findSubtrees shinyGold)
    |> List.map resetTopLevelCount
    |> List.head
    |> fun sg -> sg.Children // to remove the shiny gold container itself
    |> List.map countBags
    |> List.sum

