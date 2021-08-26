module GameController

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input
open System.Drawing
open System.IO

let highScoreFile = "highscore.txt"
let checkpointFile = "checkpoint.txt"
let timeToLoad = 3000.
let bubbleSpeed = 0.02
let bubbleHeight = 1.
let maxLevel = 8

let DialoguePaths = 
    Map.empty.
        Add(1, "./Content/Dialogue/level1.txt").
        Add(2, "./Content/Dialogue/level2.txt").
        Add(3, "./Content/Dialogue/level3.txt").
        Add(4, "./Content/Dialogue/level4.txt").
        Add(5, "./Content/Dialogue/level5.txt").
        Add(6, "./Content/Dialogue/level6.txt").
        Add(7, "./Content/Dialogue/level7.txt").
        Add(8, "./Content/Dialogue/level8.txt");;

let mapKey = [
    (Color.FromArgb(128, 128, 128), Block)
    (Color.FromArgb(255, 0, 0), Spikes)
    (Color.FromArgb(38, 127, 0), EntryPortal)
    (Color.FromArgb(0, 38, 255), ExitPortal)
    (Color.FromArgb(255, 216, 0), Coin)
    (Color.FromArgb(0, 0, 0), Orc)
]

let getLevel num = 
    use bitmap = Bitmap.FromFile <| sprintf "./Content/Maps/map%i.bmp" num :?> Bitmap
    [0..bitmap.Height-1] |> Seq.collect (fun y -> [0..bitmap.Width-1] |> Seq.map (fun x ->
        let color = bitmap.GetPixel (x, y)
        match Seq.tryFind (fun (c,_) -> c = color) mapKey with
        | Some t -> Some (x, y, t |> snd)
        | _ -> None)) |> Seq.choose id |> Seq.toList

let levels = [1..maxLevel] |> List.map (fun i -> (i, getLevel i)) |> Map.ofList

let hasReset (runState : RunState) worldState = 
    worldState.knight.state = Dead && runState.WasJustPressed Keys.R 

let hasWarpedOut (runState : RunState) worldState = 
    match worldState.knight.state with
    | WarpingOut t when runState.elapsed - t > warpTime -> true 
    | _ -> false

let processBubbles worldState = 
    { worldState with 
        bubbles =
            worldState.bubbles 
                |> List.map (fun b -> 
                    let (x, y) = b.position
                    { b with position = x, y - bubbleSpeed })
                |> List.filter (fun b -> 
                    let (_, y) = b.position
                    (b.startY - y) < bubbleHeight) }

let loadHighScore () =
    if File.Exists highScoreFile then
        let text = File.ReadAllText highScoreFile
        match System.Int32.TryParse text with
        | true, v -> v
        | _ -> 0
    else 0

let loadCheckpoint () =
    if File.Exists checkpointFile then
        let text = File.ReadAllText checkpointFile
        match System.Int32.TryParse text with
        | true, v -> v
        | _ -> 1
    else 1

let loadDialogue level =
    if DialoguePaths.ContainsKey level then
        let path = DialoguePaths.Item level

        if File.Exists path then
            File.ReadAllText path
        else "Get Ready!"
    else "You will find a way!"
    

let saveHighScore score =
    File.WriteAllText (highScoreFile, string score)

let saveCheckpoint level = 
    File.WriteAllText (checkpointFile, string level)


let advanceGame (runState : RunState) =
    let elapsed = runState.elapsed
    function
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> 
        let highScore = loadHighScore ()
        Title highScore |> Some
    | Some (Title _) when runState.WasJustPressed Keys.Enter ->
        let startingLevel = loadCheckpoint ()
        let dialogue = loadDialogue(startingLevel)
        Loading (elapsed, startingLevel, maxLevel, 0, dialogue) |> Some
    | Some (Loading (t, l, _, score, dialogue)) when elapsed - t > timeToLoad ->
        getLevelModel levels.[l] l score runState.elapsed |> Some 
    | Some (Playing worldState) when hasReset runState worldState -> 
        Some <| getLevelModel 
            levels.[worldState.level] 
            worldState.level 
            worldState.knight.startScore 
            runState.elapsed
    | Some (Playing worldState) when hasWarpedOut runState worldState && worldState.level = maxLevel -> 
        let score = worldState.knight.score
        let highScore = loadHighScore ()
        if score > highScore then saveHighScore score |> ignore
        saveCheckpoint (1)
        Some <| Victory (score, max score highScore)
    | Some (Playing worldState) when hasWarpedOut runState worldState ->
        saveCheckpoint (worldState.level + 1)
        let dialogue = loadDialogue(worldState.level + 1)
        Loading (elapsed, worldState.level + 1, maxLevel, worldState.knight.score, dialogue) |> Some
    | Some (Playing worldState) -> 
        { worldState with events = [] }
        |> KnightController.processKnight runState
        |> OrcController.processOrcs runState
        |> processBubbles
        |> Playing |> Some
    | Some (Victory (_, highScore)) when runState.WasJustPressed Keys.R -> 
        Some <| Title highScore
    | other -> other