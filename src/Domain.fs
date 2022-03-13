module Domain
let rand = System.Random()
type MazeElement = Open | Closed
type Maze = {
    size: int * int
    grid: MazeElement[][]
    }
let inbounds maze x y =
    0 <= x && x < maze.grid.Length && 0 <= y && y < maze.grid[0].Length
let isPotentialConnection x y = x % 2 <> y % 2
let isPotentialPoint x y = x % 2 = 1 && y % 2 = 1
type Point = Point of x: int * y:int * context: Maze
    with
    member this.isValid() =
        let (Point(x,y,maze)) = this
        isPotentialPoint x y && inbounds maze x y
type Connection = Connection of x: int * y: int * context: Maze
    with
    member this.isValid() =
        let (Connection(x,y,maze)) = this
        isPotentialConnection x y && inbounds maze x y 

type Direction = Up | Down | Left | Right

let moveTo direction (Point(x, y, ctx)) =
    match direction with
        | Down -> x, y+2
        | Up -> x, y-2
        | Left -> x-2, y
        | Right -> x+2, y
    |> fun (x,y) -> Point(x,y,ctx)

let connectionTo direction (Point(x, y, ctx)) =
    match direction with
        | Down -> x, y+1
        | Up -> x, y-1
        | Left -> x-1, y
        | Right -> x+1, y
    |> fun (x,y) -> Connection(x,y,ctx)

let newMaze (width, height, initialConnection) =
    let grid = Array.init (width*2+1) (fun _ -> Array.create (height*2+1) Closed)
    // tunnel out all of the "rooms"
    for x in [1..2..width*2] do
        for y in [1..2..height*2] do
            grid[x][y] <- Open
    if initialConnection then
        // tunnel out all of the left/right corridors (but not the outside walls)
        for x in [2..2..width*2-2] do
            for y in [1..2..height*2] do
                grid[x][y] <- Open
        // tunnel out all of the up/down corridors (but not the outside walls)
        for x in [1..2..width*2] do
            for y in [2..2..height*2-2] do
                grid[x][y] <- Open
    { size = (width, height); grid = grid }

let map f maze =
    let grid' =
        maze.grid |> Array.mapi (fun x column ->
            column |> Array.mapi (fun y state -> f x y state))
    { maze with grid = grid' }

let permute percent maze =
    let grid = maze.grid
    let interior x y =
        // Exclude the first and last element in each array because those are the outer walls
        let (xBound, yBound) = grid[0].Length - 2, grid.Length - 2
        0 < x && x < xBound && 0 < y && y < yBound
    maze |> map (fun x y state ->
                if (Connection(x,y,maze).isValid()) && interior x y && rand.Next(100) < percent then
                    match state with
                    | Open -> Closed
                    | Closed -> Open
                else state
                )

let carve percent maze =
    let grid = maze.grid
    let interior x y =
        // Exclude the first and last element in each array because those are the outer walls
        let (xBound, yBound) = grid[0].Length - 2, grid.Length - 2
        0 < x && x < xBound && 0 < y && y < yBound
    maze |> map (fun x y state ->
        if (Connection(x,y,maze).isValid()) && interior x y && rand.Next(100) < percent then
            match state with
            | Open -> Open
            | Closed -> Open
        else state
        )

let randomConnected maze =
    let x, y = maze.size
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1,maze)
        if not <| p.isValid() then failwith $"Invalid point: {p}"
        p
    let nodes = [|
        for y in 1..y do
            for x in 1..x do
                (x,y) |> toPoint
        |]
    let mutable connectedNodes = Set.ofSeq [(1,1) |> toPoint]
    let mutable tunnels = Set.empty<int*int>
    let directions = [Up;Down;Left;Right]
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1,maze)
        if not <| p.isValid() then failwith $"Invalid point: {p}"
        p
    while connectedNodes.Count < nodes.Length do
        let nextIx = rand.Next(nodes.Length)
        let candidate = nodes[nextIx]
        if connectedNodes.Contains(candidate) |> not then
            let start = candidate
            // make sure the new node is connected to an existing node via some direction
            match connectedNodes
                |> Seq.tryPick (
                    fun dest -> directions |> List.tryPick (fun dir -> if ((start |> moveTo dir) = dest) then Some (dir, dest) else None)) with
            | Some (direction, dest) ->
                let (Connection(x,y,maze)) = start |> connectionTo direction
                tunnels <- tunnels |> Set.add (x,y)
                connectedNodes <- connectedNodes |> Set.add candidate
            | None ->
                ()
    maze |> map (fun x y state -> if tunnels |> Set.contains (x,y) then Open else state)

let toAscii maze =
    let s = System.Text.StringBuilder()
    s.Append "\n" |> ignore
    for y in 0..maze.grid[0].Length - 1  do
        for x in 0..maze.grid.Length - 1 do
            (if maze.grid[x][y] = Closed then "x" else " ") |> s.Append |> ignore
        s.Append "\n" |> ignore
    s.ToString()

let aldousBroder maze =
    let x, y = maze.size
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1, maze)
        if not <| p.isValid() then failwith $"Invalid point: {p}"
        p
    let nodes = [|
        for x in 1..x do
            for y in 1..y do
                (x,y) |> toPoint
        |]
    let mutable currentNode = (1,1) |> toPoint
    let mutable connectedNodes = Set.ofSeq [currentNode]
    let mutable tunnels = Set.empty<int*int>
    let directions = [Up;Down;Left;Right]
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1, maze)
        if not <| p.isValid() then failwith $"Invalid point: {p}"
        p
    while connectedNodes.Count < nodes.Length do
        let rec next() =
            let direction = directions[rand.Next(directions.Length)]
            let candidate = currentNode |> moveTo direction
            if candidate.isValid() then
                candidate, direction
            else next()
        let candidate, direction = next()

        if connectedNodes.Contains(candidate) |> not then
            let (Connection(x,y,maze)) = currentNode |> connectionTo direction
            tunnels <- tunnels |> Set.add (x,y)
            currentNode <- candidate
            connectedNodes <- connectedNodes |> Set.add candidate
        else
            // don't add a tunnel, just move
            currentNode <- candidate
    let perimeter =
        [|
            for y in 0..maze.grid.Length - 1 do
                if (y = 0 || y = maze.grid.Length - 1) then
                    for x in 0..maze.grid[y].Length - 1 do
                        x, y
                else
                    for x in [0; maze.grid[y].Length - 1] do
                        x, y
            |]
        |> Array.filter (fun (x,y) -> (Connection(x,y,maze)).isValid())
    let entry = perimeter[rand.Next(perimeter.Length)]
    let exit =
        // Don't let the entrance and exit be too close to each other
        let dist (x1, y1) (x2, y2) =
            abs (x1 - x2) + abs (y1 - y2)
        let rec retry() =
            let candidate = perimeter[rand.Next(perimeter.Length)]
            let x, y = maze.size
            if dist candidate entry < (min x y) / 2 then
                retry()
            else
                candidate
        retry()
    tunnels <- tunnels |> Set.add entry |> Set.add exit
    maze |> map (fun x y state -> if tunnels |> Set.contains (x,y) then Open else state)

let normalize maze =
    let every f seq = seq |> Seq.exists (not << f) |> not
    maze |> map (fun x y state ->
        // for all of the corners, they become open if all the connections around them are open
        if not (Connection(x,y,maze).isValid() || Point(x,y,maze).isValid()) then
            let within start finish n = start <= n && n < finish
            let get(x,y) =
                if within 0 maze.grid.Length x && within 0 maze.grid[x].Length y then
                    maze.grid[x][y] |> Some
                else None
            if [x+1,y;x-1,y;x,y+1;x,y-1]
                |> List.map get
                |> List.exists (function Some(Closed) -> true | _ -> false)
                then
                    Closed
                else
                    Open
        else state
        )

