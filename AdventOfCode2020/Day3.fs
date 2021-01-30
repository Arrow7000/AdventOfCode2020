module Day3


type TreeOrNot = Tree | NonTree

let len = 31

let lineToRow (line : string) =
    line
    |> List.ofSeq
    |> List.map (fun c -> if c = '#' then Tree else NonTree)


let traverseAndCountTrees lines =
    let rec traverser leftOffset treesSoFar linesLeft =
        match linesLeft with
        | [] -> treesSoFar
        | thisLine :: rest ->
            let treesToAdd =
                match List.item (leftOffset % List.length thisLine) thisLine with
                | Tree -> 1
                | NonTree -> 0

            traverser (leftOffset + 3) (treesSoFar + treesToAdd) rest

    traverser 0 0 lines




let part2traverseAndCountTrees rightNum downNum lines =
    let startingDownOffset = downNum - 1

    let rec traverser downOffset leftOffset treesSoFar linesLeft =
            match linesLeft with
            | [] -> treesSoFar
            | thisLine :: rest ->
                let offsetInThisLine = leftOffset % List.length thisLine
                let treesInThisPos =
                    match List.item offsetInThisLine thisLine with
                    | Tree -> 1
                    | NonTree -> 0

                let currInNonSkippedRow = downOffset = 0
                let treesToAdd =
                    if currInNonSkippedRow then treesInThisPos else 0
                let nextDownOffset =
                    if currInNonSkippedRow then startingDownOffset else downOffset - 1
                let nextLeftoffset =
                    if currInNonSkippedRow then leftOffset + rightNum else leftOffset
                traverser nextDownOffset nextLeftoffset (treesSoFar + treesToAdd) rest

    traverser 0 0 0 lines


let input = readAllLines "day3.txt"


let lines = List.map lineToRow input

let part1 = traverseAndCountTrees lines




let right1down1 = part2traverseAndCountTrees 1 1 lines
let right3down1 = part2traverseAndCountTrees 3 1 lines // should be the same as part 1
let right5down1 = part2traverseAndCountTrees 5 1 lines
let right7down1 = part2traverseAndCountTrees 7 1 lines
let right1down2 = part2traverseAndCountTrees 1 2 lines


let part2 =
    [ right1down1
      right3down1
      right5down1
      right7down1
      right1down2 ]
    |> List.fold (fun a b -> a * int64 b) 1L
