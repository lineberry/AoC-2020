open System
open System.IO

//Read the input file and turn it into an array of strings
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map (fun x -> x.ToCharArray()) |> array2D

//Helper function to visualize the seating chart
let printGrid (grid:char[,]) =
    let inputWidth = Array2D.length1 grid
    let inputHeight = Array2D.length2 grid

    for x = 0 to (inputWidth-1) do
        printfn ""
        for y = 0 to (inputHeight-1) do
            printf "%c" grid.[x,y]

//0,0 in the top left
//y increasing down
let getNeighborIndexes x y =
    [x-1,y-1; x,y-1; x+1,y-1;
     x-1,y;            x+1,y;
     x-1,y+1; x,y+1;  x+1,y+1]

let isSeatIndexValid x y gridWidth gridHeight =
    not (x < 0 || y < 0 || x > gridWidth - 1 || y > gridHeight - 1)

let isSeatOccupied x y (grid:char[,]) =
    if not (isSeatIndexValid x y (Array2D.length1 grid) (Array2D.length2 grid)) then 0
    else
        match grid.[x,y] with
        | '#' -> 1
        | _ -> 0

let getCountOfOccupiedNeighbors (neighborIndexes:list<(int*int)>) (grid:char[,]) =
    List.sumBy (fun elem -> isSeatOccupied (fst elem) (snd elem) grid) neighborIndexes 

let getNewSeatStatePart1 x y (grid:char[,]) =
    let currentState = grid.[x,y]
    let neighborIndexes = getNeighborIndexes x y
    let countOfOccupiedNeighbors = getCountOfOccupiedNeighbors neighborIndexes grid

    match countOfOccupiedNeighbors with
    | x when currentState = '#' && x >= 4 -> 'L'
    | x when currentState = 'L' && x = 0 -> '#'
    | _ -> currentState

let calcGameRoundPart1 (grid:char[,]) =
    Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (fun x y -> getNewSeatStatePart1 x y grid)

let getTotalOccupiedSeats (grid:char[,]) =
    grid |> Seq.cast |> Seq.filter (fun x -> x = '#') |> Seq.length

let rec gameLoopPart1 (prevRound:char[,]) =
    let nextRound = calcGameRoundPart1 prevRound
    if prevRound = nextRound then
        getTotalOccupiedSeats nextRound
    else gameLoopPart1 nextRound


[<EntryPoint>]
let main argv =
    let input = readlines "input.txt"
    let answer = gameLoopPart1 input

    printfn "The answer to part 1 is %i." answer
    0 // return an integer exit code