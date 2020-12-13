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
    | ('N',y) -> { ferry with YPos = ferry.YPos + y }
    | ('S',y) -> { ferry with YPos = ferry.YPos - y }
    | ('E',x) -> { ferry with XPos = ferry.XPos + x }
    | ('W',x) -> { ferry with XPos = ferry.XPos - x }
    | _ -> failwith "Encountered invalid instruction in moveFerry."

let moveWaypoint (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',y) -> { ferry with WaypointYPos = ferry.WaypointYPos + y }
    | ('S',y) -> { ferry with WaypointYPos = ferry.WaypointYPos - y }
    | ('E',x) -> { ferry with WaypointXPos = ferry.WaypointXPos + x }
    | ('W',x) -> { ferry with WaypointXPos = ferry.WaypointXPos - x }
    | _ -> failwith "Encountered invalid instruction in moveWaypoint."

let advanceFerry (ferry:Ferry) (instruction:(char*int)) =
    moveFerry ferry (ferry.Facing,snd instruction)

let advanceSystem (ferry:Ferry) (instruction:(char*int)) =
    let timesToAdvance = snd instruction
    let xToAdvance = ferry.WaypointXPos * timesToAdvance
    let yToAdvance = ferry.WaypointYPos * timesToAdvance
    { ferry with XPos = ferry.XPos + xToAdvance; YPos = ferry.YPos + yToAdvance }

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

let rotateWaypoint (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('L',90) | ('R',270) ->  {ferry with WaypointXPos = -1 * ferry.WaypointYPos; WaypointYPos = ferry.WaypointXPos}
    | ('R',90) | ('L',270) -> {ferry with WaypointXPos = ferry.WaypointYPos; WaypointYPos = -1 * ferry.WaypointXPos}
    | (_,180) -> {ferry with WaypointXPos = -1 * ferry.WaypointXPos; WaypointYPos = -1 * ferry.WaypointYPos}
    | _ -> failwith "Encountered invalid instruction in rotateWaypoint"

let getNewFerryState (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',_) | ('S',_) | ('E',_) | ('W',_) -> moveFerry ferry instruction
    | ('L',_) | ('R',_) -> turnFerry ferry instruction
    | ('F',_) -> advanceFerry ferry instruction
    | (_,_) -> failwith "Encountred invalid instruction in getNewFerryState."

let getNewSystemState (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',_) | ('S',_) | ('E',_) | ('W',_) -> moveWaypoint ferry instruction
    | ('L',_) | ('R',_) -> rotateWaypoint ferry instruction
    | ('F',_) -> advanceSystem ferry instruction
    | (_,_) -> failwith "Encountred invalid instruction in getNewSystenState."

let rec eventLoop (ferry:Ferry) (instructions:list<(char*int)>) =
    match instructions with 
    | head::tail -> 
        let newFerryState = getNewFerryState ferry head
        eventLoop newFerryState tail
    | [] -> ferry

let rec systemLoop (ferry:Ferry) (instructions:list<(char*int)>) =
    match instructions with 
    | head::tail -> 
        let newSystemState = getNewSystemState ferry head
        systemLoop newSystemState tail
    | [] -> ferry

[<EntryPoint>]
let main argv =
    let movementInstructions = readlines "input.txt"

    let startingFerryState = Ferry.Default

    //Solve part 1
    let finalFerryState = eventLoop startingFerryState movementInstructions
    printfn "The answer to part 1 is %i." (getManhattanDistance finalFerryState)

    //Solve part 2
    let finalSystemState = systemLoop startingFerryState movementInstructions
    printfn "The answer to part 2 is %i." (getManhattanDistance finalSystemState)

    0 // return an integer exit code