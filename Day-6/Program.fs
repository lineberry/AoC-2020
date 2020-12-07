// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

//Helper function to just print every character in a set
let printSet (setToPrint:Set<char>) =
    Set.iter (fun x -> printfn "%c," x) setToPrint

//Add all characters on the line to a Set so you are only left with the unique characters
let rec parseLine (charList:List<char>) (setAcc:Set<char>) =
    match charList with
        | head::tail -> parseLine tail (setAcc.Add(head))
        | [] ->  setAcc                  

//Parse the answers for part 1
let rec parseAnswers (answerList:List<string>) (setAcc:Set<char>) (answerAcc:int) =
    match answerList with
        | head::tail -> 
            if head.Length = 0 then //Start a new group.  The setAcc here has the uniqe answers for the group
                parseAnswers tail Set.empty (answerAcc+setAcc.Count) 
            else
                let answersSoFar = parseLine (head.ToCharArray() |> Array.toList) setAcc
                parseAnswers tail answersSoFar answerAcc                        
        | [] -> (answerAcc+setAcc.Count)

//Parse the answers for part 2
let rec parsePart2 (answerList:List<string>) (setAcc:List<Set<char>>) (answerAcc:int) =
    match answerList with
        | head::tail ->
            if head.Length = 0 then //Start a new group.  The setAcc here has the answers for each person in a Set
                let groupAnswer = Set.intersectMany setAcc
                parsePart2 tail [] (answerAcc + groupAnswer.Count)
            else
                let answers = parseLine (head.ToCharArray() |> Array.toList) Set.empty
                let answerList = List.append setAcc [answers]
                parsePart2 tail answerList answerAcc 
        | [] -> 
            let groupAnswer = Set.intersectMany setAcc
            (answerAcc + groupAnswer.Count)


[<EntryPoint>]
let main argv =
    let questionAnswers = readlines "input.txt"
    let answer = parseAnswers questionAnswers Set.empty 0
    printfn "The answer for part 1 is %i." answer

    let answer2 = parsePart2 questionAnswers [] 0
    printfn "The answer for part 2 is %i." answer2

    0 // return an integer exit code