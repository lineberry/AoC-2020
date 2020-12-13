// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let readlines (filePath:string) = 
    File.ReadAllLines(filePath)

let rec findAnswerPart1 (busIds:list<int>) targetTime minWait (busAndWait:(int*int)) =
    match busIds with
    | busId::remainingBusIds -> 
        if busId - targetTime % busId < minWait then
            findAnswerPart1 remainingBusIds targetTime (busId - targetTime % busId) (busId,(busId - targetTime % busId))
        else
            findAnswerPart1 remainingBusIds targetTime minWait busAndWait
    | [] -> busAndWait

let rec getBusIdsAndOffsets (busIds:list<string>) (busIdsAndOffsets:list<(int*int)>) offsetAcc =
    match busIds with
    | busId::remainingBusIds -> 
        let newList = if busId <> "x" then busIdsAndOffsets @ [(int busId,offsetAcc)] else busIdsAndOffsets
        getBusIdsAndOffsets remainingBusIds newList (offsetAcc+1)
    | [] -> busIdsAndOffsets

[<EntryPoint>]
let main argv =
    let busSchedule = readlines "input.txt"
    let targetTime = int busSchedule.[0]
    let rawBusIds = busSchedule.[1].Split(',', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let busIds = rawBusIds 
                 |> Seq.ofList 
                 |> Seq.filter (fun x -> x <> "x")
                 |> Seq.map int
                 |> List.ofSeq

    //Answer part 1
    let answer = findAnswerPart1 busIds targetTime (targetTime - 1) (0,0)
    printfn "The answer to part 1 is %i." (fst answer * snd answer)

    //Answer part 2
    let busIdsAndOffsets = getBusIdsAndOffsets rawBusIds List.empty 0
    printfn "%A" busIdsAndOffsets
    
    // 17 -> 17
    // 17,13 -> (17*6) + 2 (time that works for first 2 busses is 102)
    // LCM of 17 and 13 is 221
    // 17,13,19 -> (221*15) + 102  + 3 
     
    // (221*15) + 102 = 3417
     
     
    // 67 -> 67
    // 67,7 -> (67*3) + 2 (time that works for 2 busses is 201)
    // LCM of 67 and 7 is 469
    // 67,7,59 -> (469*9) + 201 + 3 (time that works for 3 busses is 4422)
    // LCM of 67,7,59 is 27,671
    // 67,7,59,61 -> (27,671*28) + 4422 + 4


    0 // return an integer exit code