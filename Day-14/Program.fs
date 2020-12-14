open System
open System.IO

let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> List.ofArray

let getMaskedCharacterPart1 maskChar numChar = 
    match maskChar with
    | 'X' -> numChar
    | x when x = '1' || x = '0' -> x
    | _ -> failwithf "Invalid character is mask. %c" maskChar

let getMaskedCharacterPart2 maskChar numChar = 
    match maskChar with
    | '0' -> numChar
    | x when x = '1' || x = 'X' -> x
    | _ -> failwithf "Invalid character is mask. %c" maskChar

let applyMaskPart1 bitmask binarystring =
    Seq.map2 (getMaskedCharacterPart1) bitmask binarystring |> Seq.map string |> String.concat ""

let applyMaskPart2 bitmask binarystring =
    Seq.map2 (getMaskedCharacterPart2) bitmask binarystring |> Seq.map string |> String.concat ""

let getPaddedBinaryString (num:int64) = 
    let unpaddedString = Convert.ToString(num, 2)
    unpaddedString.PadLeft(36, '0')

let rec replaceXsWithPermutationValues (maskedMemoryAddress:list<char>) (permutation:string) (addressAcc:string) posAcc =
    match maskedMemoryAddress with
    | head::tail ->
        if head = 'X' then replaceXsWithPermutationValues tail permutation (addressAcc + permutation.[posAcc].ToString()) (posAcc + 1)
        else replaceXsWithPermutationValues tail permutation (addressAcc + head.ToString()) posAcc
    | [] -> addressAcc

let generateMemoryAddressPermutations memoryLocation (bitmask:string) =
    let binaryMemoryLocation = getPaddedBinaryString memoryLocation
    let maskedMemoryAddress = applyMaskPart2 bitmask binaryMemoryLocation
    let floatingCount = Seq.fold (fun acc x -> if x = 'X' then (acc + 1) else acc) 0 maskedMemoryAddress
    let maxNum = Convert.ToInt64("".PadRight(floatingCount, '1'),2)
    let maskedMemoryAddressCharList = maskedMemoryAddress |> Seq.toList

    let permutations = seq {
        for i = 0L to maxNum do
            yield Convert.ToString(i,2).PadLeft(floatingCount,'0')
    }
    seq {
        for perm in permutations do
            yield replaceXsWithPermutationValues maskedMemoryAddressCharList perm "" 0
    }

let rec parsePart1 (dockingProgram:list<string>) (programAcc:Map<int,int64>) maskAcc =
    match dockingProgram with
    | head::tail ->
        let splitInput = head.Split(" = ", StringSplitOptions.RemoveEmptyEntries)
        if splitInput.[0] = "mask" then
            parsePart1 tail programAcc splitInput.[1]
        else
            let memoryLocation = int (splitInput.[0].Replace("mem[","").Replace("]",""))
            let binaryNumberString = getPaddedBinaryString (int64 splitInput.[1])
            let maskedNumberString = applyMaskPart1 maskAcc binaryNumberString
            let decimalNumber = Convert.ToInt64(maskedNumberString, 2)
            parsePart1 tail (programAcc.Add(memoryLocation, decimalNumber)) maskAcc
    | [] -> Map.fold (fun acc k v -> acc + v) 0L programAcc

let rec parsePart2 (dockingProgram:list<string>) (programAcc:Map<string,int64>) maskAcc =
    match dockingProgram with
    | head::tail ->
        let splitInput = head.Split(" = ", StringSplitOptions.RemoveEmptyEntries)
        if splitInput.[0] = "mask" then
            parsePart2 tail programAcc splitInput.[1]
        else
            let memoryLocation = int64 (splitInput.[0].Replace("mem[","").Replace("]",""))
            let memoryAddressPerms = generateMemoryAddressPermutations memoryLocation maskAcc |> List.ofSeq
            let memoryValue = int64 splitInput.[1]

            //Each memory instruction can write to hundreds of locations
            let rec loop (addresses:list<string>) (valueToWrite:int64) (progAcc:Map<string,int64>) =
                match addresses with
                | h::t -> loop t valueToWrite (progAcc.Add(h,valueToWrite))
                | [] -> progAcc

            parsePart2 tail (loop memoryAddressPerms memoryValue programAcc) maskAcc
    | [] -> Map.fold (fun acc k v -> acc + v) 0L programAcc

[<EntryPoint>]
let main argv =
    let dockingProgram = readlines "input.txt"
    
    //Solve part 1
    let answer = parsePart1 dockingProgram Map.empty ""
    printfn "The answer to part 1 is %i." answer

    //Solve part 2
    let answer2 = parsePart2 dockingProgram Map.empty ""
    printfn "The answer to part 2 is %i." answer2

    0 // return an integer exit code