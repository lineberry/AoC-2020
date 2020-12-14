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

let testTime (timeToTest:int64) (busIdsAndOffset:(int64*int64)) =
    if (timeToTest + snd busIdsAndOffset) % (fst busIdsAndOffset) = 0L then 0
    else 1

let getLCM (busIdsAndOffsets:list<(int64*int64)>) = 
    List.fold (fun acc elem -> acc * (fst elem)) 1L busIdsAndOffsets

let rec findAnswerPart2 (busIdsAndOffsets:list<(int64*int64)>) (headAccumulator:Set<(int64*int64)>) lcmAcc lastIterTime iterationCount =
    match busIdsAndOffsets with
    | head::tail ->      
        let newTimeToTest = (lcmAcc * iterationCount) + lastIterTime
        let traversedSoFar = headAccumulator.Add(head)
        let traversedSoFarList = traversedSoFar |> List.ofSeq

        let totalRemainderCount = List.sumBy (testTime newTimeToTest) traversedSoFarList
        if totalRemainderCount > 0 then
            findAnswerPart2 busIdsAndOffsets traversedSoFar lcmAcc lastIterTime (iterationCount + 1L)
        else
            printfn "Time that works for %A is %i." traversedSoFar newTimeToTest
            let lcm = getLCM traversedSoFarList
            findAnswerPart2 tail traversedSoFar lcm newTimeToTest 1L
    | [] -> lastIterTime

let rec getBusIdsAndOffsets (busIds:list<string>) (busIdsAndOffsets:list<(int64*int64)>) offsetAcc =
    match busIds with
    | busId::remainingBusIds -> 
        let newList = if busId <> "x" then busIdsAndOffsets @ [(int64 busId,offsetAcc)] else busIdsAndOffsets
        getBusIdsAndOffsets remainingBusIds newList (offsetAcc+1L)
    | [] -> busIdsAndOffsets


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
    let busIdsAndOffsets = getBusIdsAndOffsets rawBusIds List.empty 0L
    let answer2 = findAnswerPart2 busIdsAndOffsets Set.empty 1L 41L 1L
    printfn "The answer to part 2 is %i." answer2
    
    0 // return an integer exit code