module Day6

let input = readAllLines "day6.txt"


let getAnyYesCount lines =
    let sets,lastSet =
        lines
        |> List.fold
            (fun (sealedSets,currSet) thisStr ->
                if thisStr = "" then
                    currSet :: sealedSets,
                    Set.empty
                else
                    sealedSets,
                    thisStr
                    |> Seq.fold (fun set char -> Set.add char set) currSet
            )
            (List.empty, Set.empty)

    lastSet :: sets
    |> List.map Set.count
    |> List.sum



let part1 =
    getAnyYesCount input


type State = NewSetStarted | ExistingSet of Set<char>

let getAllYesCount lines =
    let sets,lastSet =
        lines
        |> List.fold
            (fun (sealedSets,state) thisStr ->
                match state with
                | ExistingSet currSet ->
                    if thisStr = "" then
                        currSet :: sealedSets,
                        NewSetStarted
                    else
                        let currUserSet = Set.ofSeq thisStr
                        sealedSets,
                        ExistingSet <| Set.intersect currUserSet currSet
                | NewSetStarted ->
                    sealedSets,
                    ExistingSet <| Set.ofSeq thisStr
            )
            (List.empty, NewSetStarted)

    (match lastSet with NewSetStarted -> Set.empty | ExistingSet set -> set)
    :: sets
    |> List.map Set.count
    |> List.sum


let part2 =
    getAllYesCount input