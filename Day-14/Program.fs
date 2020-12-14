open System
open System.IO

let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> List.ofArray

let getMaskedCharacter maskChar numChar = 
    match maskChar with
    | 'X' -> numChar
    | x when x = '1' || x = '0' -> x
    | _ -> failwithf "Invalid character is mask. %c" maskChar

let applyMask bitmask binarystring =
    Seq.map2 (getMaskedCharacter) bitmask binarystring |> Seq.map string |> String.concat ""

let getPaddedBinaryString (num:int64) = 
    let unpaddedString = Convert.ToString(num, 2)
    unpaddedString.PadLeft(36, '0')

let rec parsePart1 (dockingProgram:list<string>) (programAcc:Map<int,int64>) maskAcc =
    match dockingProgram with
    | head::tail ->
        let splitInput = head.Split(" = ", StringSplitOptions.RemoveEmptyEntries)
        if splitInput.[0] = "mask" then
            parsePart1 tail programAcc splitInput.[1]
        else
            let memoryLocation = int (splitInput.[0].Replace("mem[","").Replace("]",""))
            let binaryNumberString = getPaddedBinaryString (int64 splitInput.[1])
            let maskedNumberString = applyMask maskAcc binaryNumberString
            let decimalNumber = Convert.ToInt64(maskedNumberString, 2)
            parsePart1 tail (programAcc.Add(memoryLocation, decimalNumber)) maskAcc
    | [] -> Map.fold (fun acc k v -> acc + v) 0L programAcc

[<EntryPoint>]
let main argv =
    let dockingProgram = readlines "input.txt"
    let answer = parsePart1 dockingProgram Map.empty ""

    printfn "The answer to part 1 is %i." answer
    0 // return an integer exit code