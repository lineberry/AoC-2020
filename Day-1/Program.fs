// Learn more about F# at http://fsharp.org

open System
open System.IO


let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map int |> Array.toList

let rec testSubList firstItem (sublist:list<int>) =
    match sublist with
    | [] -> 
        (0,0)
    | [x] -> 
        if(firstItem + x = 2020) then (firstItem, x)
        else (0,0)
    | head :: tail ->
        if(firstItem + head = 2020) then (firstItem, head)
        else testSubList firstItem tail

let rec testList (expenseReport:list<int>) =
    match expenseReport with
    | [] -> 
        (0,0)
    | [x] -> 
        (0,0)
    | head :: tail ->
        let result = testSubList head tail
        if result = (0,0) then testList tail
        else result

[<EntryPoint>]
let main argv =
    let expenseReport = readlines "input.txt"
    let targetNums = testList expenseReport
    let firstNumber = fst targetNums
    let secondNumber = snd targetNums
    let answer = firstNumber * secondNumber
    printfn "The numbers you are looking for are %i and %i." firstNumber secondNumber
    printfn "The answer to enter into the box is %i." answer
    0 // return an integer exit code