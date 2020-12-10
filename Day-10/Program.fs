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

let rec removeSuccessive3JoltAdapters (joltList:list<int>) =
    if joltList.Length = 1 then
        joltList
    else
        match joltList with
            | head::tail ->
                if tail.Head - head = 3 then removeSuccessive3JoltAdapters joltList.Tail
                else tail
            | [] -> []

let getPermLengthMultiplier (len:int64) =
    match len with
        | 3L -> 7L
        | 2L -> 4L
        | 1L -> 2L
        | 0L -> 1L
        | _ -> failwith "Invalid permutation length used."

//Find sequences where the difference between successive elements is 3
//For any sequence in the original list excluding the above get the length of that sequence
//Multiply the acc by the following value depending on the length of the sequence from above
// Length = 3 -> acc * 7
// Length = 2 -> acc * 4
// Length = 1 -> acc * 2
let rec solvePart2 (joltList:list<int>) (permLengthAcc:int64) (acc:int64) = 
    if joltList.Length = 1 then
        acc
    else
        match joltList with
            | head::tail ->
                if tail.Head - head = 3 then
                    let remainderOfList = removeSuccessive3JoltAdapters joltList
                    let permutationMultiplier = getPermLengthMultiplier permLengthAcc
                    let accumulatorValue = if acc = 0L then permutationMultiplier else (acc * permutationMultiplier)
                    solvePart2 remainderOfList 0L accumulatorValue
                elif tail.Head - head = 1 then
                    solvePart2 tail (permLengthAcc+1L) acc
                else
                    failwith "I'm lost!"
            | [] -> acc

[<EntryPoint>]
let main argv =
    let originalJoltageList = readlines "input.txt"
    let builtInAdapter = List.max originalJoltageList + 3
    let chainToDevice = originalJoltageList @ [builtInAdapter]
    let chainFromOutlet = 0::originalJoltageList

    //Solve Part 1
    let joltageDifferences = List.fold2 (fun (acc:(int*int)) elem1 elem2 ->  generateAcc elem1 elem2 acc) (0,0) chainFromOutlet chainToDevice
    printfn "The answer to part 1 is %i." (fst joltageDifferences * snd joltageDifferences)

    //Solve Part 2
    let answerPart2 = solvePart2 chainToDevice 0L 0L
    printfn "The answer to part 2 is %A." answerPart2

    0 // return an integer exit code