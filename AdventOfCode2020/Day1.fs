module Day1



let input = readAllLines "day1.txt"

let getTwoNums numToTotalTo inputNums =
    let inputSet = set inputNums

    inputNums
    |> List.fold
        (fun foundNums thisNum ->
            match foundNums with
            | Some nums -> Some nums
            | None ->
                let targetNum = numToTotalTo - thisNum
                if Set.contains targetNum inputSet then Some (thisNum, targetNum)
                else None)
        None



let getThreeNums numToTotalTo inputNums =
    inputNums
    |> List.fold
        (fun foundNums thisNum ->
            match foundNums with
            | Some nums -> Some nums
            | None ->
                let targetNum = numToTotalTo - thisNum

                match getTwoNums targetNum inputNums with
                | Some (a,b) -> Some (thisNum,a,b)
                | None -> None)
        None




let inputNums =
    input
    |> List.map int

let part1 =
    getTwoNums 2020 inputNums
    |> Option.get
    |> fun (a,b) -> a * b

let part2 =
    getThreeNums 2020 inputNums
    |> Option.get
    |> fun (a,b,c) -> a * b * c
