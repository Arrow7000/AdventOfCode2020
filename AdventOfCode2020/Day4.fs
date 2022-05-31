module Day4

open FSharp.Text.RegexProvider


type PassportFieldRegex = Regex<"(?<code>[a-z]{3}):(?<fieldValue>#?\w+)">

let byr = "byr"
let iyr = "iyr"
let eyr = "eyr"
let hgt = "hgt"
let hcl = "hcl"
let ecl = "ecl"
let pid = "pid"

let cid = "cid"


type PassportValidState = Valid | Invalid
type PassportContent<'a> = Validated of 'a | Invalidated




let lineToPassPortFields str =
    let matches = PassportFieldRegex().TypedMatches(str)
    matches
    |> Seq.map (fun m -> m.code.Value, m.fieldValue.Value)
    |> List.ofSeq


let findMatch keyToFind (key, _) = key = keyToFind


let emptyFields = None, None, None, None, None, None, None

let tryFindAllFields (eclOpt, pidOpt, eyrOpt, hclOpt, byrOpt, iyrOpt, hgtOpt) matches =
    let findForKey pastOpt key =
        match pastOpt with
        | Some past -> Some past
        | None ->
            matches
            |> List.tryFind (findMatch key)
            |> Option.map snd


    findForKey eclOpt ecl,
    findForKey pidOpt pid,
    findForKey eyrOpt eyr,
    findForKey hclOpt hcl,
    findForKey byrOpt byr,
    findForKey iyrOpt iyr,
    findForKey hgtOpt hgt


let getAllPassports lines =
    lines @ List.singleton List.empty // needs to end  with a blank line or else the last passport doesn't get sealed 
    |> List.fold
        (fun (donePassports,currPassport) thisLine ->
            match thisLine with
            | [] ->
                let thisPassport =
                    match currPassport with
                    | Some _, Some _, Some _, Some _, Some _, Some _, Some _ ->
                        Valid
                    | _ -> Invalid
                
                thisPassport :: donePassports, emptyFields
            | matches -> donePassports, tryFindAllFields currPassport matches)
        (List.empty, emptyFields)
    |> fst




let tryParseInt (str : string) =
    match System.Int32.TryParse str with
    | true, int -> Some int
    | _ -> None

let between min max int = int >= min && int <= max

type HeightRegex = Regex<"(?<height>\d+)(?<unit>\w{2})">
type ColourRegex = Regex<"^#[a-f0-9]{6}$">
type PidRegex = Regex<"^\d{9}$">



let byrValidator = tryParseInt >> Option.filter (between 1920 2002)
let iyrValidator = tryParseInt >> Option.filter (between 2010 2020)
let eyrValidator = tryParseInt >> Option.filter (between 2020 2030)
let hgtValidator str =
    let m = HeightRegex().TryTypedMatch(str)
    match m with
    | None -> None
    | Some match' ->
        match tryParseInt match'.height.Value with
        | Some height ->
            match match'.unit.Value with
            | "cm" -> if between 150 193 height then Some (height,"cm") else None
            | "in" -> if between 59 76 height then Some (height,"in") else None
            | _ -> None
        | None -> None

let hclValidator str =
    match ColourRegex().TryTypedMatch(str) with
    | Some v -> Some v.Value
    | None -> None
    
let eclValidator v =
    if List.contains v [ "amb";"blu";"brn";"gry";"grn";"hzl";"oth" ] then
        Some v
    else None

let pidValidator str =
    match PidRegex().TryTypedMatch(str) with
    | Some v -> Some v.Value
    | None -> None



let tryValidateAllFields (byrOpt,iyrOpt,eyrOpt,hgtOpt,hclOpt,eclOpt,pidOpt) matches =
    let validateForKey key validator pastOpt =
        match pastOpt with
        | Some past -> Some past
        | None ->
            matches
            |> List.tryFind (findMatch key)
            |> Option.bind (snd >> validator)

    validateForKey byr byrValidator byrOpt,
    validateForKey iyr iyrValidator iyrOpt,
    validateForKey eyr eyrValidator eyrOpt,
    validateForKey hgt hgtValidator hgtOpt,
    validateForKey hcl hclValidator hclOpt,
    validateForKey ecl eclValidator eclOpt,
    validateForKey pid pidValidator pidOpt

let getAllValidatedPassports lines =
    lines @ List.singleton List.empty // needs to end  with a blank line or else the last passport doesn't get sealed 
    |> List.fold
        (fun (donePassports,currPassport) thisLine ->
            match thisLine with
            | [] ->
                let thisPassport =
                    match currPassport with
                    | Some a, Some b, Some c, Some d, Some e, Some f, Some g ->
                        Validated (a,b,c,d,e,f,g)
                    | _ -> Invalidated
                
                thisPassport :: donePassports, emptyFields
            | matches -> donePassports, tryValidateAllFields currPassport matches)
        (List.empty, emptyFields)
    |> fst


let input = readAllLines "day4.txt"



let lines =
    input
    |> List.map lineToPassPortFields


let part1 =
    getAllPassports lines
    |> List.choose (function Valid -> Some () | _ -> None)
    |> List.length


let part2 =
    getAllValidatedPassports lines
    |> List.choose (function Validated x -> Some x | _ -> None)
    |> List.length
