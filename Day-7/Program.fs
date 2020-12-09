open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

let inline charToInt c = int c - int '0'

//Extract the color bag that the rule applies to
let extractRuleColor (rule:string) = 
    rule.Split([|" bags"|], StringSplitOptions.RemoveEmptyEntries).[0]

//Extract the rules that apply to each bag from the line
let extractRules (rule:string) = 
    rule.Split([|"contain "|], StringSplitOptions.RemoveEmptyEntries).[1].Split([|", "|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun x -> x.Replace(" bags", "").Replace(" bag","").Replace(".", ""))
        |> Array.fold (fun (acc:Map<string,int>) elem -> 
            if elem.[0] = 'n' then
                Map.empty
            else
                acc.Add(elem.[2..], (elem.[0] |> charToInt ))) Map.empty

let parseInput (bagRules:list<string>) =
    List.fold (fun (acc:Map<string,Map<string,int>>) elem -> acc.Add(extractRuleColor elem, extractRules elem)) Map.empty bagRules

let findBagsThatCanContainBag (bagColor:string) (parsedInput:Map<string,Map<string,int>>) =
    Map.fold (fun (acc:Set<string>) k (v:Map<string,int>) ->  if v.ContainsKey(bagColor) then acc.Add(k) else acc) Set.empty parsedInput


let rec recurseBagsUp (bagSequence:seq<string>) (allBags:Map<string,Map<string,int>>) (bagAcc:list<string>) =
    let thisLevel = seq {
        for color in bagSequence do
            yield! findBagsThatCanContainBag color allBags
    }

    let bagList = List.ofSeq thisLevel 

    if bagList.Length <> 0 then
        recurseBagsUp thisLevel allBags (bagAcc @ bagList)
    else bagAcc

let rec recurseBagsDown (bagSequence:seq<string>) (allBags:Map<string,Map<string,int>>) (bagAcc:list<string>) =
    let thisLevel = seq {
        for color in bagSequence do
            yield! allBags.[color]
    }

    let levelSeqRepresentation = seq{
        for kvp in thisLevel do
            for i = 1 to kvp.Value do
                yield kvp.Key
    }

    let levelListRespresentation = List.ofSeq levelSeqRepresentation
    let soFar = bagAcc @ levelListRespresentation

    if levelListRespresentation.Length = 0 then
        soFar
    else
        recurseBagsDown levelSeqRepresentation allBags soFar

[<EntryPoint>]
let main argv =
    let bagRules = readlines "input.txt"
    let allColors = parseInput bagRules
    let shinyGoldContainers = recurseBagsUp (Seq.singleton "shiny gold") allColors list.Empty |> Seq.distinct |> List.ofSeq

    //Answer part 1
    printfn "Answer for part 1 is %i." shinyGoldContainers.Length

    //Answer part 2
    let shinyGoldContents = recurseBagsDown (Seq.singleton "shiny gold") allColors list.Empty
    printfn "Answer for part 2 is %i." shinyGoldContents.Length

    //Map.iter (fun k v -> printfn "Rules for %s bags: %A" k v) allColors
    0 // return an integer exit code