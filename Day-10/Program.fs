open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

[<EntryPoint>]
let main argv =
    let input = readlines "input.txt"
    0 // return an integer exit code