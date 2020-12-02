// Learn more about F# at http://fsharp.org

open System
open System.IO

//Read the input file and turn it into an array of integers
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map int |> Array.toList

//Test all the entries in the sublist against the first item to see if the sum is 2020
let rec testSubList firstItem (sublist:list<int>) =
    match sublist with
    | [] -> (0,0)
    | [x] -> 
        if(firstItem + x = 2020) then (firstItem, x)
        else (0,0)
    | head :: tail ->
        if(firstItem + head = 2020) then (firstItem, head)
        else testSubList firstItem tail

//Test all the sublists of the input looking for two items where the sum 2020
let rec testList (expenseReport:list<int>) =
    match expenseReport with
    | [] -> (0,0)
    | [x] -> (0,0)
    | head :: tail ->
        let result = testSubList head tail
        if result = (0,0) then testList tail
        else result

//Return a copy of the original list that has the value n removed
let rec remove n lst = 
    match lst with
    | head :: tail when head = n -> tail
    | head :: tail -> head :: (remove n tail)
    | [] -> []

//Given a number and a list, determine if a value exists in the list that when added to partOne equals 2020
let searchForAnswer partOne (expenseReport:list<int>) =
    let lookingFor = 2020 - partOne

    if List.exists((=)lookingFor) expenseReport then
        lookingFor
    else 0

let rec recurseList (expenseReport:list<int>) =
    match expenseReport with
        | head :: tail -> 
            let listToSearch = remove head tail
            let result = searchForAnswer head listToSearch
            match result with
                | 0 ->  recurseList tail
                | _ -> (head, result)
        | [] -> (0,0)

let sortTest (expenseReport:list<int>) = 
    let sortedList = List.sort expenseReport
    let sortedList2 = sortedList
    let sortedList3 = sortedList
    
    for item1 in sortedList do
        for item2 in sortedList2 do
            for item3 in sortedList3 do
                if item1 + item2 + item3 = 2020 then
                    printfn "Your three numbers are %i, %i, and %i. And the product is %i." item1 item2 item3 (item1 * item2 * item3)


[<EntryPoint>]
let main argv =
    let expenseReport = readlines "input.txt"

    //First Implementation
    // let targetNums = testList expenseReport
    // let firstNumber = fst targetNums
    // let secondNumber = snd targetNums
    // let answer = firstNumber * secondNumber
    // printfn "The numbers you are looking for are %i and %i." firstNumber secondNumber
    // printfn "The answer to enter into the box is %i." answer

    //Second implementation
    let targetNums2 = recurseList expenseReport
    let firstNumber2 = fst targetNums2
    let secondNumber2 = snd targetNums2
    let answer2 = firstNumber2 * secondNumber2
    printfn "The numbers you are looking for are %i and %i." firstNumber2 secondNumber2
    printfn "The answer to enter into the box is %i." answer2

    sortTest expenseReport

    0 // return an integer exit code