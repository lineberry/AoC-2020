// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Collections.Generic

[<Measure>] type cm

type Passport =
    {
        BirthYear:int option
        IssueYear:int option
        ExpirationYear:int option
        //Height:int<cm> option
        Height:int option
        HairColor:string option
        EyeColor:string option
        PassportId:string option
        CountryId:int option
    }


//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

let isValidPassportPart1 (passport:Passport) =
    passport.BirthYear.IsSome 
    && passport.IssueYear.IsSome 
    && passport.ExpirationYear.IsSome 
    && passport.Height.IsSome 
    && passport.HairColor.IsSome 
    && passport.EyeColor.IsSome 
    && passport.PassportId.IsSome

let isValidPassportPart2 (passport:Passport) =
    passport.BirthYear.IsSome && passport.BirthYear.Value >= 1920 && passport.BirthYear.Value <= 2002
    && passport.IssueYear.IsSome && passport.IssueYear.Value >= 2010 && passport.IssueYear.Value <= 2020
    && passport.ExpirationYear.IsSome && passport.ExpirationYear.Value >= 2020 && passport.ExpirationYear.Value <= 2030
    && passport.Height.IsSome 
    && passport.HairColor.IsSome 
    && passport.EyeColor.IsSome 
    && passport.PassportId.IsSome && passport.PassportId.Value.Length = 9

let getDictOption (dict:IDictionary<string,string>) (key:string) =
    match dict.TryGetValue key with
      | true, value -> value
      | _           -> ""

//Split the input into 1 line per passport since the elfs are horrible at formatting their data
let rec splitRecords (inputLines:list<string>) (passportLineAcc:string) (outputAcc:list<string>)=
    match inputLines with
        | head::tail ->
            if head.Length = 0 then //start new passport
                let trimmedPassportLine = passportLineAcc.Trim()
                splitRecords tail "" (outputAcc @ [trimmedPassportLine])
            else
                let lineToAdd = (passportLineAcc+head+" ")
                splitRecords tail lineToAdd outputAcc
        | [] -> 
            let trimmedPassportLine = passportLineAcc.Trim()
            (outputAcc @ [trimmedPassportLine])

//Turn the parsed string records into passport POFOs
let rec parseRecords (passportRecords:list<string>) (passportsAcc:list<Passport>) =
    match passportRecords with
        | head::tail ->
            let attributes = head.Split ' ' |> Array.map (fun x -> x.Split ':')
            let attDict = Dictionary<string,string>()
            
            for row in attributes do
                attDict.Add(row.[0], row.[1])

            let safeByr = getDictOption attDict "byr"
            let safeIyr = getDictOption attDict "iyr"
            let safeEyr = getDictOption attDict "eyr"
            let safeHgt = (getDictOption attDict "hgt").Replace("cm", "").Replace("in", "")
            let safeHcl = getDictOption attDict "hcl"
            let safeEcl = getDictOption attDict "ecl"
            let safePid = getDictOption attDict "pid"
            let safeCid = getDictOption attDict "cid"

            let byr = if safeByr.Length = 0 then None else Some(int safeByr)
            let iyr = if safeIyr.Length = 0 then None else Some(int safeIyr)
            let eyr = if safeEyr.Length = 0 then None else Some(int safeEyr)
            let hgt = if safeHgt.Length = 0 then None else Some(int safeHgt)
            //let hgt = if safeHgt.Length = 0 then None else Some(LanguagePrimitives.Int32WithMeasure<cm> (int safeHgt))
            let hcl = if safeHcl.Length = 0 then None else Some(safeHcl)
            let ecl = if safeEcl.Length = 0 then None else Some(safeEcl)
            let pid = if safePid.Length = 0 then None else Some(safePid)
            let cid = if safeCid.Length = 0 then None else Some(int safeCid)

            let passport = 
                { 
                    Passport.BirthYear = byr
                    IssueYear = iyr
                    ExpirationYear = eyr
                    Height = hgt
                    HairColor = hcl
                    EyeColor = ecl
                    PassportId = pid
                    CountryId = cid 
                }

            parseRecords tail (passportsAcc @ [passport])
        | [] -> passportsAcc

[<EntryPoint>]
let main argv =
    let passportBatch = readlines "input.txt"
    let passportLines = splitRecords passportBatch "" []
    let passports = parseRecords passportLines []

    //Solve Part 1
    let validPassports = Seq.filter isValidPassportPart1 passports |> Seq.length
    printfn "There are %i valid passports for part 1." validPassports

    //Solve Part 2

    0 // return an integer exit code