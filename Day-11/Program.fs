open System
open System.IO

//Read the input file and turn it into a matrix of characters
let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map (fun x -> x.ToCharArray()) |> array2D

let isSeatIndexInvalid x y gridWidth gridHeight =
    x < 0 || y < 0 || x > gridWidth - 1 || y > gridHeight - 1

let rec findNearestSeat x y radius (slope:(int*int)) (grid:char[,]) =
    let gridWidth = Array2D.length1 grid
    let gridHeight = Array2D.length2 grid
    let xSlope = fst slope * radius
    let ySlope = snd slope * radius

    if isSeatIndexInvalid (x+xSlope) (y+ySlope) gridWidth gridHeight || grid.[x+xSlope,y+ySlope] <> '.' then (x+xSlope,y+ySlope)
    else findNearestSeat x y (radius+1) slope grid

//0,0 in the top left
//y increasing down
let getNeighborIndexes x y (grid:char[,]) partId =
    if partId = 1 then
        [x-1,y-1; x,y-1; x+1,y-1;
         x-1,y;            x+1,y;
         x-1,y+1; x,y+1;  x+1,y+1]
    else
        [(-1,-1); (0,-1); (1,-1);
         (-1,0);           (1,0);
         (-1,1);  (0,1);   (1,1)] |>
        List.map (fun s -> findNearestSeat x y 1 s grid) 

let isSeatOccupied x y (grid:char[,]) =
    if isSeatIndexInvalid x y (Array2D.length1 grid) (Array2D.length2 grid) then 0
    else
        match grid.[x,y] with
        | '#' -> 1
        | _ -> 0

let getCountOfOccupiedNeighbors (neighborIndexes:list<(int*int)>) (grid:char[,]) =
    List.sumBy (fun elem -> isSeatOccupied (fst elem) (snd elem) grid) neighborIndexes 

let getNewSeatState x y (grid:char[,]) partId =
    let currentState = grid.[x,y]
    let neighborIndexes = getNeighborIndexes x y grid partId
    let countOfOccupiedNeighbors = getCountOfOccupiedNeighbors neighborIndexes grid
    let tooManyNeighbors = if partId = 1 then 4 else 5

    match countOfOccupiedNeighbors with
    | count when currentState = '#' && count >= tooManyNeighbors -> 'L'
    | count when currentState = 'L' && count = 0 -> '#'
    | _ -> currentState

let calcGameRound (grid:char[,]) partId =
    Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (fun x y -> getNewSeatState x y grid partId)

let getTotalOccupiedSeats (grid:char[,]) =
    grid |> Seq.cast |> Seq.filter (fun x -> x = '#') |> Seq.length

let rec gameLoop (currentGridState:char[,]) partId =
    let nextRound = calcGameRound currentGridState partId
    if currentGridState = nextRound then
        getTotalOccupiedSeats nextRound
    else gameLoop nextRound partId

[<EntryPoint>]
let main argv =
    let input = readlines "input.txt"

    //Solve part 1
    let answer = gameLoop input 1
    printfn "The answer to part 1 is %i." answer

    //Solve part 2
    let answer2 = gameLoop input 2
    printfn "The answer to part 2 is %i." answer2
    0 // return an integer exit code