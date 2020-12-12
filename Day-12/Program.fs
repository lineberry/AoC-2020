open System
open System.IO

type Ferry =
    {
        Facing:char
        XPos:int
        YPos:int
        WaypointXPos:int
        WaypointYPos:int
    }

    static member Default =
        { 
          Facing = 'E'
          XPos = 0
          YPos = 0
          WaypointXPos = 10
          WaypointYPos = 1
        }

let getManhattanDistance (ferry:Ferry) =
    Math.Abs ferry.XPos + Math.Abs ferry.YPos

let parseLine (lineToParse:string) = 
    (lineToParse.[0], int lineToParse.[1..])

let readlines (filePath:string) = 
    File.ReadAllLines(filePath) |> Array.map parseLine |> Array.toList

let moveFerry (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',_) -> { ferry with XPos = ferry.XPos + snd instruction }
    | ('S',_) -> { ferry with XPos = ferry.XPos - snd instruction }
    | ('E',_) -> { ferry with YPos = ferry.YPos + snd instruction }
    | ('W',_) -> { ferry with YPos = ferry.YPos - snd instruction }
    | _ -> failwith "Encountered invalid instruction in moveFerry."

let advanceFerry (ferry:Ferry) (instruction:(char*int)) =
    moveFerry ferry (ferry.Facing,snd instruction)

//Find a better algorithm for this
let turnFerry (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('L',deg) -> 
        match ferry.Facing with
        | 'N' when deg = 90 -> {ferry with Facing = 'W'}
        | 'N' when deg = 180 -> {ferry with Facing = 'S'}
        | 'N' when deg = 270 -> {ferry with Facing = 'E'}
        | 'S' when deg = 90 -> {ferry with Facing = 'E'}
        | 'S' when deg = 180 -> {ferry with Facing = 'N'}
        | 'S' when deg = 270 -> {ferry with Facing = 'W'}
        | 'E' when deg = 90 -> {ferry with Facing = 'N'}
        | 'E' when deg = 180 -> {ferry with Facing = 'W'}
        | 'E' when deg = 270 -> {ferry with Facing = 'S'}
        | 'W' when deg = 90 -> {ferry with Facing = 'S'}
        | 'W' when deg = 180 -> {ferry with Facing = 'E'}
        | 'W' when deg = 270 -> {ferry with Facing = 'N'}
        | _ -> ferry
    | ('R',deg) ->
        match ferry.Facing with
        | 'N' when deg = 90 -> {ferry with Facing = 'E'}
        | 'N' when deg = 180 -> {ferry with Facing = 'S'}
        | 'N' when deg = 270 -> {ferry with Facing = 'W'}
        | 'S' when deg = 90 -> {ferry with Facing = 'W'}
        | 'S' when deg = 180 -> {ferry with Facing = 'N'}
        | 'S' when deg = 270 -> {ferry with Facing = 'E'}
        | 'E' when deg = 90 -> {ferry with Facing = 'S'}
        | 'E' when deg = 180 -> {ferry with Facing = 'W'}
        | 'E' when deg = 270 -> {ferry with Facing = 'N'}
        | 'W' when deg = 90 -> {ferry with Facing = 'N'}
        | 'W' when deg = 180 -> {ferry with Facing = 'E'}
        | 'W' when deg = 270 -> {ferry with Facing = 'S'}
        | _ -> ferry
    | _ -> failwith "Encountered invalid instruction in turnFerry."

let getNewFerryState (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',_) | ('S',_) | ('E',_) | ('W',_) -> moveFerry ferry instruction
    | ('L',_) | ('R',_) -> turnFerry ferry instruction
    | ('F',_) -> advanceFerry ferry instruction
    | (_,_) -> failwith "Encountred invalid instruction in getNewFerryState."

let rec eventLoop (ferry:Ferry) (instructions:list<(char*int)>) =
    match instructions with 
    | head::tail -> 
        let newFerryState = getNewFerryState ferry head
        eventLoop newFerryState tail
    | [] -> ferry

[<EntryPoint>]
let main argv =
    let movementInstructions = readlines "input.txt"

    let startingFerryState = Ferry.Default

    //Solve part 1
    let finalFerryState = eventLoop startingFerryState movementInstructions
    printfn "The answer to part 1 is %i." (getManhattanDistance finalFerryState)

    0 // return an integer exit code