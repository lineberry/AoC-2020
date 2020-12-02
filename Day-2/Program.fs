// Learn more about F# at http://fsharp.org

open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

//Tail optimized recusive function to get count of the specified character in the string
let countCharFromNth (getStr : string)(chkdChar : char) = 
    let rec loop i count =
        if i < getStr.Length then 
            if getStr.[i] = chkdChar then loop (i+1) (count+1)
            else loop (i+1) count
        else count
    loop 0 0

//Validate the password matches the policy for part two of day two
let validateCharPositions (password:string) (charToCheck:char) firstPos secondPos =
    let charAtFirstPos = password.[firstPos-1] //Convert to 0 based index
    let charAtSecondPos = password.[secondPos-1] //Convert to 0 based index
    
    (charAtFirstPos = charToCheck || charAtSecondPos = charToCheck) && (charAtFirstPos <> charAtSecondPos)

//Parse a password policy and then validate a password matches the given policy
let validatePasswordPart1 (policy:string) (password:string) =
    let splitPolicy = policy.Split ' '
    let charCount = splitPolicy.[0]
    let charToCheck = char splitPolicy.[1]

    let splitCharCount = charCount.Split '-'
    let lowerBound = int splitCharCount.[0]
    let upperBound = int splitCharCount.[1]

    let charCount = countCharFromNth password charToCheck

    not (charCount < lowerBound || charCount > upperBound)

//Parse a password policy and then validate a password matches the given policy
let validatePasswordPart2 (policy:string) (password:string) =
    let splitPolicy = policy.Split ' '
    let charCount = splitPolicy.[0]
    let charToCheck = char splitPolicy.[1]

    let splitCharPositions = charCount.Split '-'
    let firstPosition = int splitCharPositions.[0]
    let secondPosition = int splitCharPositions.[1]

    validateCharPositions password charToCheck firstPosition secondPosition

//Recursively check all the passwords in the input list and return a tuple that contains (validPasswordcount, invalidPasswordCount)
//Based on the part 1 password policy
let rec checkPasswordsPart1 (passwordList:list<string>) acc1 acc2 =
    match passwordList with
        | head :: tail ->  
            let splitHead = head.Split ':'
            let policy = splitHead.[0]
            let password = splitHead.[1].Trim()
            if(validatePasswordPart1 policy password) then
                checkPasswordsPart1 tail (acc1+1) acc2
            else
                checkPasswordsPart1 tail acc1 (acc2+1)
        | [] -> (acc1, acc2)

//Recursively check all the passwords in the input list and return a tuple that contains (validPasswordcount, invalidPasswordCount)
//Based on the part 2 password policy
let rec checkPasswordsPart2 (passwordList:list<string>) acc1 acc2 =
    match passwordList with
        | head :: tail ->
            let splitHead = head.Split ':'
            let policy = splitHead.[0]
            let password = splitHead.[1].Trim()
            if(validatePasswordPart2 policy password) then
                checkPasswordsPart2 tail (acc1+1) acc2
            else
                checkPasswordsPart2 tail acc1 (acc2+1)
        | [] -> (acc1, acc2)

[<EntryPoint>]
let main argv =
    let passwordList = readlines "input.txt"

    //Solve part1
    let passwordCount = checkPasswordsPart1 passwordList 0 0
    let validPasswords = fst passwordCount
    let invalidPasswords = snd passwordCount
    printfn "Part 1: Valid password count is %i. Invalid password count is %i." validPasswords invalidPasswords

    //Solve part 2
    let passwordCount2 = checkPasswordsPart2 passwordList 0 0
    let validPasswords2 = fst passwordCount2
    let invalidPasswords2 = snd passwordCount2
    printfn "Part 2: Valid password count is %i. Invalid password count is %i." validPasswords2 invalidPasswords2

    0 // return an integer exit code
