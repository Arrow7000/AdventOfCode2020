[<AutoOpen>]
module Helpers

open System.IO


let readAllLines = File.ReadAllLines >> List.ofArray
let readAllText = File.ReadAllText

