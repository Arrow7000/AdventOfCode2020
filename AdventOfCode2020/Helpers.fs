[<AutoOpen>]
module Helpers

open System.IO


let readAllLines = File.ReadAllLines >> List.ofArray
let readAllText = File.ReadAllText

let tee f x =
    f x
    x
