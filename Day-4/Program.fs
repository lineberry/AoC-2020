// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Passport =
    {
        BirthYear:int option
        IssueYear:int option
        ExpirationYear:int option
        Height:int option
        HeightUnit: string option
        HairColor:string option
        EyeColor:string option
        PassportId:string option
        CountryId:int option
    }


//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

//#region field validation

let isBirthYearValid (birthYear:int option) =
    birthYear.IsSome && birthYear.Value >= 1920 && birthYear.Value <= 2002

let isIssueYearValid (issueYear:int option) =
    issueYear.IsSome && issueYear.Value >= 2010 && issueYear.Value <= 2020

let isExpirationYearValid (expirationYear:int option) =
    expirationYear.IsSome && expirationYear.Value >= 2020 && expirationYear.Value <= 2030

let isHairColorValid (hairColor:string option) =
    hairColor.IsSome && Regex.IsMatch(hairColor.Value, "^#[a-z0-9]{6}$")

let isEyeColorValid (eyeColor:string option) =
    match eyeColor with
    | Some("amb") | Some("blu") | Some("brn") -> true
    | Some("gry") | Some("grn") | Some("hzl") -> true
    | Some("oth") -> true
    | _ -> false

let isHeightValid (heightNumber:int option) (heightUnit:string option) =
    match (heightNumber, heightUnit) with
    | (None, _) -> false
    | (_, None) -> false
    | (hn, Some("in")) when hn.Value >= 59 && hn.Value <= 76 -> true
    | (hn, Some("cm")) when hn.Value >= 150 && hn.Value <= 193 -> true
    | _ -> false

let isPassportIdValid (passportId:string option) =
    passportId.IsSome && Regex.IsMatch(passportId.Value, "^[0-9]{9}$")

//#endregion

//Valid passports for part 1 just have some data for every field other than countryId
let isValidPassportPart1 (passport:Passport) =
    passport.BirthYear.IsSome 
    && passport.IssueYear.IsSome 
    && passport.ExpirationYear.IsSome 
    && passport.Height.IsSome 
    && passport.HairColor.IsSome 
    && passport.EyeColor.IsSome 
    && passport.PassportId.IsSome

//Validate a passport using the more complex part 2 logic
let isValidPassportPart2 (passport:Passport) =
    isBirthYearValid passport.BirthYear
    && isIssueYearValid passport.IssueYear
    && isExpirationYearValid passport.ExpirationYear
    && isHeightValid passport.Height passport.HeightUnit
    && isHairColorValid passport.HairColor
    && isEyeColorValid passport.EyeColor
    && isPassportIdValid passport.PassportId

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

            let safeFullHeight = (getDictOption attDict "hgt")
            let safeByr = getDictOption attDict "byr"
            let safeIyr = getDictOption attDict "iyr"
            let safeEyr = getDictOption attDict "eyr"
            let safeHgt = safeFullHeight.Replace("cm", "").Replace("in", "")
            let safeHightUnit = if safeFullHeight.EndsWith("cm") then Some("cm") 
                                elif safeFullHeight.EndsWith("in") then Some("in") 
                                else None
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
                    BirthYear = byr
                    IssueYear = iyr
                    ExpirationYear = eyr
                    Height = hgt
                    HeightUnit = safeHightUnit
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
    let validPassports2 = Seq.filter isValidPassportPart2 passports |> Seq.length
    printfn "There are %i valid passports for part 2." validPassports2

    0 // return an integer exit code