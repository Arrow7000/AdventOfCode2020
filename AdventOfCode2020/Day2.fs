module Day2

open FSharp.Text.RegexProvider


type PwRegex = Regex<"^(?<min>\d\d?)-(?<max>\d\d?) (?<char>\w): (?<pw>\w+)$">



let pwIsRightPart1 pwRow =
    let m = PwRegex().TypedMatch(pwRow)

    let min = int m.min.Value
    let max = int m.max.Value
    let char = char m.char.Value

    let pw = m.pw.Value

    let countOfChar =
        pw
        |> Seq.filter (fun c -> c = char)
        |> Seq.length

    countOfChar >= min && countOfChar <= max



let pwIsRightPart2 pwRow =
    let m = PwRegex().TypedMatch(pwRow)

    let pos1 = int m.min.Value
    let pos2 = int m.max.Value
    let char = char m.char.Value

    let pw = m.pw.Value

    let firstChar = Seq.item (pos1 - 1) pw
    let sndChar = Seq.item (pos2 - 1) pw

    firstChar = char <> (sndChar = char)



let input = readAllLines "day2.txt"

let part1 =
    input
    |> List.filter pwIsRightPart1
    |> List.length



let part2 =
    input
    |> List.filter pwIsRightPart2
    |> List.length

