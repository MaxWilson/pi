module Domain
let rand = System.Random()
type MazeElement = Open | Closed
type Maze = {
    size: int * int
    grid: MazeElement[][]
    }
    with
    member this.bounds =
        this.grid.Length, this.grid[0].Length
let inbounds (xBound, yBound) x y =
    0 <= x && x < xBound && 0 <= y && y < yBound
let isPotentialConnection x y = x % 2 <> y % 2
let isPotentialPoint x y = x % 2 = 1 && y % 2 = 1
type Point = Point of x: int * y:int * context: (int*int)
    with
    member this.isValid() =
        let (Point(x,y,ctx)) = this
        isPotentialPoint x y && inbounds ctx x y
type Connection = Connection of x: int * y: int * context: (int*int)
    with
    member this.isValid() =
        let (Connection(x,y,ctx)) = this
        isPotentialConnection x y && inbounds ctx x y

type Direction = Up | Down | Left | Right
    with static member All = [Up;Down;Left;Right]

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

let neighborsOf (p:Point) =
    Direction.All |> List.map (fun d -> moveTo d p) |> List.filter (fun dest -> dest.isValid())

let chooseFrom (choices: 't list) =
    choices[rand.Next choices.Length]

let nextTo (lhs: Point) (rhs: Point) =
    Direction.All |> List.exists (fun d -> moveTo d lhs = rhs)

let where = List.filter

type Bag<'t when 't: comparison>(items: 't seq) =
    let mutable queue = items |> List.ofSeq
    let mutable set = items |> Set.ofSeq
    member this.first = queue.Head
    member this.isEmpty = queue.IsEmpty
    member this.contains item = set.Contains item
    member this.add item =
        queue <- queue@[item]
        set <- set |> Set.add item
    member this.addFront item =
        queue <- item::queue
        set <- set |> Set.add item
    member this.popFront() =
        let v = queue.Head
        queue <- queue.Tail
        set <- Set.remove v set
        v
    member this.remove item =
        queue <- queue |> List.filter ((<>) item)
        set <- Set.remove item set
    member this.random = queue.[rand.Next(queue.Length)]
    member this.any f = queue |> List.exists f
    member this.every f = not (queue |> List.exists (not << f))
    member this.firstWhere f = queue |> List.find f
    member this.where f = queue |> List.filter f

let nodesOf (maze:Maze) =
    let x, y = maze.size
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1,maze.bounds)
        if not <| p.isValid() then failwith $"Invalid point: {p}"
        p
    [|
        for x in 1..x do
            for y in 1..y do
                (x,y) |> toPoint
        |]


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
                if (Connection(x,y,maze.bounds).isValid()) && interior x y && rand.Next(100) < percent then
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
        if (Connection(x,y,maze.bounds).isValid()) && interior x y && rand.Next(100) < percent then
            match state with
            | Open -> Open
            | Closed -> Open
        else state
        )

let randomConnected maze =
    let x, y = maze.size
    let toPoint (x,y) =
        let p = Point(x*2-1, y*2-1,maze.bounds)
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
        let p = Point(x*2-1, y*2-1,maze.bounds)
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

let doAlgorithm algorithm maze =
    let nodes = nodesOf maze |> Bag
    let mutable tunnels = algorithm nodes |> Seq.choose (function Connection(x,y,_) as c when c.isValid() -> Some (x,y) | _ -> None) |> Set.ofSeq
    let perimeter =
        [|
            for x in 0..maze.grid.Length - 1 do
                if (x = 0 || x = maze.grid.Length - 1) then
                    for y in 0..maze.grid[x].Length - 1 do
                        x, y
                else
                    for y in [0; maze.grid[x].Length - 1] do
                        x, y
            |]
        |> Array.filter (fun (x,y) -> (Connection(x,y,maze.bounds)).isValid())
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

let connect (Point(x1,y1,maze)) (Point(x2,y2,_)) =
    Connection((x1+x2)/2, (y1+y2)/2, maze)

let aldousBroder (unvisited: Bag<Point>) = seq {
    let mutable currentNode = unvisited.popFront()
    while not unvisited.isEmpty do
        let dest = chooseFrom (neighborsOf currentNode)
        if (unvisited.contains dest) then
            unvisited.remove dest
            connect currentNode dest
        currentNode <- dest
    }

type HunterKillerMode = Hunt | Kill
let hunterKiller (unvisited: Bag<Point>) = [
    let mutable mode = Kill
    let mutable currentNode = unvisited.popFront()
    let visited = Bag[currentNode]
    while not unvisited.isEmpty do
        match mode with
        | Kill ->
            // "kill" mode, go back to random walk
            let candidate = chooseFrom (neighborsOf currentNode)
            if unvisited.contains candidate then
                connect currentNode candidate
                unvisited.remove candidate
                visited.add candidate
                currentNode <- candidate
            else
                mode <- Hunt
        | Hunt ->
            // search for an unvisited node that's next to a visited node and make it the new starting position
            currentNode <- visited.random
            match neighborsOf currentNode |> where (fun n -> unvisited.contains n) with
            | [] -> () // try again
            | dests ->
                let dest = chooseFrom dests
                connect currentNode dest
                unvisited.remove dest
                visited.add dest
                currentNode <- dest
                mode <- Kill
    ]

let normalize maze =
    let every f seq = seq |> Seq.exists (not << f) |> not
    maze |> map (fun x y state ->
        // for all of the corners, they become open if all the connections around them are open
        if not (Connection(x,y,maze.bounds).isValid() || Point(x,y,maze.bounds).isValid()) then
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
