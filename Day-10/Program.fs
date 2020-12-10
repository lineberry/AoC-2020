open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList |> List.map int |> List.sort

let generateAcc elem1 elem2 (tup:(int*int)) =
    if elem2 - elem1 = 3 then
        (fst tup, snd tup+1)
    elif elem2 - elem1 = 1 then
        (fst tup+1, snd tup)
    else tup

[<EntryPoint>]
let main argv =
    let originalJoltageList = readlines "input.txt"
    let builtInAdapter = List.max originalJoltageList + 3
    let chainToDevice = originalJoltageList @ [builtInAdapter]
    let chainFromOutlet = 0::originalJoltageList

    let joltageDifferences = List.fold2 (fun (acc:(int*int)) elem1 elem2 ->  generateAcc elem1 elem2 acc) (0,0) chainFromOutlet chainToDevice
    printfn "The answer to part 1 is %i." (fst joltageDifferences * snd joltageDifferences)

    //List.iter (fun x -> printfn "%" x) test
    0 // return an integer exit code