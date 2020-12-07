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
        |> Array.fold (fun (acc:Map<string,int>) elem -> acc.Add(elem.[2..], (elem.[0] |> charToInt ))) Map.empty

let parseInput (bagRules:list<string>) =
    List.fold (fun (acc:Map<string,Map<string,int>>) elem -> acc.Add(extractRuleColor elem, extractRules elem)) Map.empty bagRules

let findBagsThatCanContainBag (bagColor:string) (parsedInput:Map<string,Map<string,int>>) =
    Map.fold (fun (acc:Set<string>) k (v:Map<string,int>) ->  if v.ContainsKey(bagColor) then acc.Add(k) else acc) Set.empty parsedInput

// let rec recursiveFindBags (bagColors:list<string>) (parsedInput:Map<string,Map<string,int>>) (recAcc:Set<string>) =
//     match bagColors with
//         | head::tail ->
//             let thisBagContainers = findBagsThatCanContainBag head parsedInput
//             let rec loop (c:Set<string>) recAcc =
//                 match c with
//                     | h::t -> 
//                         let tbc = findBagsThatCanContainBag h parsedInput
//                         loop t recAcc
//                     | [] -> recAcc

//             recursiveFindBags tail parsedInput (recAcc.(head))
//         | [] -> recAcc

[<EntryPoint>]
let main argv =
    let bagRules = readlines "input.txt"
    let allColors = parseInput bagRules
    let shinyGoldContainers = findBagsThatCanContainBag "shiny gold"  allColors |> Set.toList

    //let answer = recursiveFindBags shinyGoldContainers allColors Set.empty
    //printfn "The answer is %i." answer.Count
    0 // return an integer exit code