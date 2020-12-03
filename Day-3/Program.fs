// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO


//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

//positionNeeded is the 0 based index of the string we need to check
let expandLine (inputString:string) positionNeeded =
    let copiesNeeded = int(ceil ((float positionNeeded + 1.0) / float inputString.Length))

    //printfn "Copies of this line needed is %i." copiesNeeded

    let rec loop i (expandedLine:string) =
        if i < copiesNeeded then 
            loop (i+1) expandedLine + inputString
        else expandedLine
    loop 1 inputString


let rec ski (skiMap:list<string>) horizontalMove verticalMove rowAcc (treeAcc:int64) =
    match skiMap with
        | head :: tail ->
            let positionNeeded = rowAcc * horizontalMove
            let mapRow = expandLine head positionNeeded

            //printfn "%s" mapRow

            if mapRow.[positionNeeded] = '#' then
                ski tail.[verticalMove-1..] horizontalMove verticalMove (rowAcc+1) (treeAcc+1L)
            else
                ski tail.[verticalMove-1..] horizontalMove verticalMove (rowAcc+1) treeAcc
        | [] -> treeAcc


[<EntryPoint>]
let main argv =
    let inputMap = readlines "input.txt"

    //Solve Part 1
    let treeCount = ski inputMap 3 1 0 0L
    printfn "The number of trees you hit for part 1 is %i." treeCount

    //Solve Part 2
    let treeCountIter1 = ski inputMap 1 1 0 0L
    let treeCountIter3 = ski inputMap 5 1 0 0L
    let treeCountIter4 = ski inputMap 7 1 0 0L
    let treeCountIter5 = ski inputMap 1 2 0 0L

    printfn "The number of trees you hit for iter1 in part 2 is %i." treeCountIter1
    printfn "The number of trees you hit for iter3 in part 2 is %i." treeCountIter3
    printfn "The number of trees you hit for iter4 in part 2 is %i." treeCountIter4
    printfn "The number of trees you hit for iter5 in part 2 is %i." treeCountIter5

    let partTwoTreeCount = treeCountIter1 * treeCount * treeCountIter3 * treeCountIter4 * treeCountIter5
    printfn "The product of trees encountered for part two is %i." partTwoTreeCount


    0 // return an integer exit code