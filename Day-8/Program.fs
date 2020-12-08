open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

let parseLine (instructionLine:string) = 
    let splitLine = instructionLine.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let instruction = splitLine.[0]
    let modifier = int splitLine.[1]
    (instruction, modifier)

let transformInput (instructionList:list<string>) =
    List.map parseLine instructionList

let rec traverseInput (instructionList:list<(string*int)>) (indexAcc:Set<int>) currentIndex (acc:int) =
    if (indexAcc.Contains currentIndex) then
        acc
    else
        let instructionTuple = instructionList.[currentIndex]
        let instruction = fst instructionTuple
        let modifier = snd instructionTuple
        let indexesVisited = indexAcc.Add(currentIndex)

        match instruction with
            | "nop" -> traverseInput instructionList indexesVisited (currentIndex+1)  acc
            | "jmp" -> traverseInput instructionList indexesVisited (currentIndex+modifier)  acc
            | "acc" -> traverseInput instructionList indexesVisited (currentIndex+1) (acc+modifier)
            | _ -> acc

let getNonAccInstructions (instructionList:list<(string*int)>) =
    seq {
            for i in 0..instructionList.Length-1 do
                if (fst instructionList.[i]) <> "acc" then
                    (i, (fst instructionList.[i]), (snd instructionList.[i]))
    } |> Seq.toList

let swapOutPart2Instruction (instructionList:list<(string*int)>) indexToReplace oldInstruction originalModifier =
    let newInstruction = if oldInstruction = "nop" then "jmp" else "nop"
    instructionList.[0..indexToReplace-1] @ [(newInstruction, originalModifier)] @ instructionList.[indexToReplace+1..]

let rec traversePart2 (originalInstructionList:list<(string*int)>) (modifiedInstructionList:list<(string*int)>) (nonAccInstructionList:list<(int*string*int)>) (indexAcc:Set<int>) currentIndex (acc:int) indAcc =
    if (indexAcc.Contains currentIndex) then
        printfn "Loop found. Visited %i lines." indexAcc.Count
        let indexToReplace, oldInstruction, modifer = nonAccInstructionList.[indAcc]
        let newInstructionListToTest = swapOutPart2Instruction originalInstructionList indexToReplace oldInstruction modifer

        //Reset and try with a new input list
        traversePart2 originalInstructionList newInstructionListToTest nonAccInstructionList Set.empty 0 0 (indAcc+1)
    elif currentIndex = 638 then
        printfn "Program terminated."
        acc
    else
        let instructionTuple = modifiedInstructionList.[currentIndex]
        let instruction = fst instructionTuple
        let modifier = snd instructionTuple
        let indexesVisited = indexAcc.Add(currentIndex)

        match instruction with
            | "nop" -> traversePart2 originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+1)  acc indAcc
            | "jmp" -> traversePart2 originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+modifier)  acc indAcc
            | "acc" -> traversePart2 originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+1) (acc+modifier) indAcc
            | _ -> acc

[<EntryPoint>]
let main argv =
    let instructions = readlines "input.txt" |> transformInput

    //Solve part 1
    let answer = traverseInput instructions Set.empty 0 0 
    printfn "The answer for part 1 is %i." answer

    //Solve part 2
    let nonAccInstructionsToTest = getNonAccInstructions instructions
    let answer2 = traversePart2 instructions instructions nonAccInstructionsToTest Set.empty 0 0 0
    printfn "The answer for part 2 is %i." answer2

    //List.iter (fun x -> printfn "%A" (parseLine x)) instructions

    0 // return an integer exit code