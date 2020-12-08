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

let getNonAccInstructions (instructionList:list<(string*int)>) =
    seq {
            for i in 0..instructionList.Length-1 do
                if (fst instructionList.[i]) <> "acc" then
                    (i, (fst instructionList.[i]), (snd instructionList.[i]))
    } |> Seq.toList

let swapOutPart2Instruction (instructionList:list<(string*int)>) indexToReplace oldInstruction originalModifier =
    let newInstruction = if oldInstruction = "nop" then "jmp" else "nop"
    instructionList.[0..indexToReplace-1] @ [(newInstruction, originalModifier)] @ instructionList.[indexToReplace+1..]

let rec traverseInput (originalInstructionList:list<(string*int)>) (modifiedInstructionList:list<(string*int)>) (nonAccInstructionList:list<(int*string*int)>) (indexAcc:Set<int>) currentIndex (acc:int) loopAcc =
    if (indexAcc.Contains currentIndex) then
        printfn "Loop found on iteration %i." loopAcc
        if loopAcc = 0 then printfn "Answer for part 1 is %i." acc
        let indexToReplace, oldInstruction, modifer = nonAccInstructionList.[loopAcc]
        let newInstructionListToTest = swapOutPart2Instruction originalInstructionList indexToReplace oldInstruction modifer

        //Reset and try with a new input list
        traverseInput originalInstructionList newInstructionListToTest nonAccInstructionList Set.empty 0 0 (loopAcc+1)
    elif currentIndex = 638 then
        printfn "Program terminated."
        acc
    else
        let instruction, modifier = modifiedInstructionList.[currentIndex]
        let indexesVisited = indexAcc.Add(currentIndex)

        match instruction with
            | "nop" -> traverseInput originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+1)  acc loopAcc
            | "jmp" -> traverseInput originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+modifier)  acc loopAcc
            | "acc" -> traverseInput originalInstructionList modifiedInstructionList nonAccInstructionList indexesVisited (currentIndex+1) (acc+modifier) loopAcc
            | _ -> acc

[<EntryPoint>]
let main argv =
    let instructions = readlines "input.txt" |> transformInput

    //Solve part 1 & 2
    let nonAccInstructionsToTest = getNonAccInstructions instructions
    let answer2 = traverseInput instructions instructions nonAccInstructionsToTest Set.empty 0 0 0
    printfn "The answer for part 2 is %i." answer2

    0 // return an integer exit code