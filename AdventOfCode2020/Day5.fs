module Day5


type Vertical = F | B
type Horizontal = L | R

type Seat = { Row : int; Col : int }



let getId {Row=row;Col=col} = row * 8 + col


let parseRowChar =
    function
    | 'F' -> F
    | 'B' -> B
    | c -> failwithf $"""Row char was "{c}" """

let parseColChar =
    function
    | 'L' -> L
    | 'R' -> R
    | c -> failwithf $"""Col char was "{c}" """





let splitV str =
    let verts = Seq.map parseRowChar str
    verts
    |> Seq.fold
        (fun currVal vert ->
            let incrementedOrNot =
                match vert with
                | B -> currVal + 1
                | F -> currVal

            incrementedOrNot <<< 1
        )
        0
    |> fun v -> v >>> 1


let splitH str =
    let horizs = Seq.map parseColChar str
    horizs
    |> Seq.fold
        (fun currVal vert ->
            let incrementedOrNot =
                match vert with
                | R -> currVal + 1
                | L -> currVal

            incrementedOrNot <<< 1
        )
        0
    |> fun v -> v >>> 1


let getSeat str =
    let row = Seq.take 7 str |> splitV
    let col = Seq.skip 7 str |> splitH
    { Row = row; Col = col }



let input =
    Helpers.readAllLines "day5.txt"

let allSeats =
    input
    |> List.map getSeat

let part1 =
    allSeats
    |> List.map getId
    |> List.max



let allRows = [0..0b1111111]
let allCols = [0..0b111]

let allPossibleSeats =
    List.allPairs allRows allCols
    |> List.map (fun (r,c) -> {Row = r; Col = c})

let takenSeatsSet = Set.ofList allSeats



type SearchStatus =
    | StillEmpty
    | FilledSeats
    | FoundSeat of Seat

let missingSeat =
    allPossibleSeats
    |> List.fold
        (fun status currentSeat ->
            match status with
            | FoundSeat seat -> FoundSeat seat
            | StillEmpty ->
                let isSeatFilled = Set.contains currentSeat takenSeatsSet
                if isSeatFilled then FilledSeats else StillEmpty
            | FilledSeats -> 
                let isSeatFilled = Set.contains currentSeat takenSeatsSet
                if isSeatFilled then FilledSeats else FoundSeat currentSeat)
        StillEmpty
    |> function
       | FoundSeat seat -> seat
       | _ -> failwithf "Couldn't find seat :("

let part2 =
    missingSeat
    |> getId

