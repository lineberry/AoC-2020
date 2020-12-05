// Learn more about F# at http://fsharp.org

open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.toList

//Calculate the seat id given the row and column position
let calcSeatId row column =
    row * 8 + column

//Extract the portion of the string that encodes the row and turn it into an array of characters
let getRowInput (encodedRow:string) =
    encodedRow.Substring(0, 7).ToCharArray() |> Array.toList

//Extract the portion of the string that encodes the column and turn it into an array of characters
let getColInput (encodedRow:string) =
    encodedRow.Substring(7, 3).ToCharArray() |> Array.toList

//Gets the index of either the row or column
let rec findIndex (input:List<char>) (startIndex:int, endIndex:int) =
    let recordsRemaining = (endIndex - startIndex)+1
    let firstHalfIndexEnd = startIndex + (recordsRemaining/2)-1
    let backHalfIndexStart = startIndex + (recordsRemaining/2)
    
    match input with
        | head::tail ->
            match head with
                | 'F' | 'L' ->  findIndex tail (startIndex, firstHalfIndexEnd)
                | 'B' | 'R' ->  findIndex tail (backHalfIndexStart, endIndex)
                | _ -> findIndex [] (0,0)
        | [] -> startIndex

//Parses a row and returns the seatId
let getSeatId (encodedRow:string) =
    let rowInput = getRowInput encodedRow
    let colInput = getColInput encodedRow
    let row = findIndex rowInput (0,127)
    let col = findIndex colInput (0,7)
    calcSeatId row col

//Compare two adjacent seat numbers and print out your seat number if the difference is larger than 1
let printMySeatId seatId1 seatId2 =
    if seatId2 - seatId1 > 1 then
        printfn "Your seat Id is %i." (seatId1 + 1)
    

[<EntryPoint>]
let main argv =
    let boardingPasses = readlines "input.txt"
    let seatIds = boardingPasses |> List.map getSeatId |> List.sort

    //Solve part 1
    let maxSeatId = seatIds |> List.max
    printfn "The Max SeatId is %i" maxSeatId

    //Solve part 2
    let seatOffset1 = seatIds.[1..] @ [maxSeatId]
    List.map2 printMySeatId seatIds seatOffset1 |> ignore
    
    0 // return an integer exit code
