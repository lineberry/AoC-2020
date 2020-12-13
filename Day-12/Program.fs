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

let advanceFerry (ferry:Ferry) spotsToAdvance =
    moveFerry ferry (ferry.Facing,spotsToAdvance)

let advanceSystem (ferry:Ferry) timesToAdvance =
    { ferry with XPos = ferry.XPos + ferry.WaypointXPos * timesToAdvance; YPos = ferry.YPos + ferry.WaypointYPos * timesToAdvance }

let turnFerry (ferry:Ferry) (instruction:(char*int)) =
    let compass = [| 'N'; 'E'; 'S'; 'W' |]
    let currentHeadingIndex = Array.findIndex (fun x -> x = ferry.Facing) compass
    let turnModifier = if fst instruction = 'L' then -1 else 1
    let turnsToMake = snd instruction / 90 * turnModifier
    let newIndex = (4 + currentHeadingIndex + turnsToMake) % 4
    {ferry with Facing = compass.[newIndex]}

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
    | ('F',x) -> advanceFerry ferry x
    | _ -> failwith "Encountred invalid instruction in getNewFerryState."

let getNewSystemState (ferry:Ferry) (instruction:(char*int)) =
    match instruction with
    | ('N',_) | ('S',_) | ('E',_) | ('W',_) -> moveWaypoint ferry instruction
    | ('L',_) | ('R',_) -> rotateWaypoint ferry instruction
    | ('F',x) -> advanceSystem ferry x
    | _ -> failwith "Encountred invalid instruction in getNewSystenState."

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