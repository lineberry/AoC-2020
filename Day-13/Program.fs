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

[<EntryPoint>]
let main argv =
    let busSchedule = readlines "input.txt"
    let targetTime = int busSchedule.[0]
    let busIds = busSchedule.[1].Split(',', StringSplitOptions.RemoveEmptyEntries) 
                 |> Seq.ofArray 
                 |> Seq.filter (fun x -> x <> "x")
                 |> Seq.map int
                 |> List.ofSeq

    //Answer part 1
    let answer = findAnswerPart1 busIds targetTime (targetTime - 1) (0,0)
    printfn "The answer to part 1 is %i." (fst answer * snd answer)

    //Answer part 2

    0 // return an integer exit code