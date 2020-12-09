open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map int64 |> Array.toList

//Get a 25 element slice of the input starting at the specified index
let getPreable (input:list<int64>) startingIndex =
    input.[startingIndex..(startingIndex+24)]

//Create a list of all the sums of permutations of the preamble
let getPreambleSums (preamble:list<int64>) =
    seq{
        for num in preamble do
            for num2 in preamble do
                if(num <> num2) then
                    yield num + num2
    }

//Recurse the input looking for the record that solves part 1
let rec traverseInputPart1 (input:list<int64>) indexAcc =
    match input with
        | _::tail ->
            let preamble = getPreable input indexAcc
            let preambleSums = preamble |> getPreambleSums
            if Seq.exists (fun x -> x = input.[indexAcc+25]) preambleSums then
                traverseInputPart1 tail (indexAcc+1)
            else
                input.[indexAcc+25]
        | [] -> failwith "Didn't find an answer."

//Recurse the outerLoopInput looking for a contiguous list of numbers that add up to the magic number to solve part 2
let rec traverseInputPart2 (outerLoopInput:list<int64>) (input:list<int64>) (rangeAcc:list<int64>) targetNumber =
    match input with
        | head::tail ->
            let listSoFar = rangeAcc @ [head]
            let sumSoFar = List.sum listSoFar
            if sumSoFar = targetNumber then
                (List.min listSoFar) + (List.max listSoFar)
            elif sumSoFar > targetNumber then
                let newListToTraverse = outerLoopInput.[1..]
                traverseInputPart2 newListToTraverse newListToTraverse [] targetNumber
            else 
                traverseInputPart2 outerLoopInput tail listSoFar targetNumber
        | [] -> failwith "Didn't find an answer."

[<EntryPoint>]
let main argv =
    let input = readlines "input.txt"

    //Solve part 1
    let answer = traverseInputPart1 input 0
    printfn "The answer for part 1 is %i." answer

    let answer2 = traverseInputPart2 input input [] answer
    printfn "The answer for part 2 is %i." answer2

    0 // return an integer exit code