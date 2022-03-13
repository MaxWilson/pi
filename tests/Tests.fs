module Tests
open Expecto
open Hedgehog
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote
open Domain
open UI.Components

module Array =
    let every f arr = arr |> Array.exists (not << f) |> not
let add x y = x + y

let appTests = testList "App tests" [
    testCase "add works" <| fun _ ->
        let result = add 2 3
        Expect.equal result 5 "Result must be 5"
]

let chooseFrom inputs =
    inputs |> List.map Gen.constant |> Gen.choice

[<Tests>]
let allTests = testList "All" [
    appTests
    testCase "checkMazeDimensions" <| fun _ ->
        let flip f x y = f y x

        property {
            let! w = Range.linear 1 100 |> Gen.int32
            let! h = Range.linear 1 100 |> Gen.int32
            let! connected = Gen.bool
            let maze = Domain.newMaze(w,h,connected)
            // E.g. a 5 x 5 maze is represented as an 11 x 11 grid where zero-based odd numbers are points you can stand on,
            // and odd/even pairs are intersections between those points. The min and max rows (0 and 10) are connections to
            // the outside.
            test <@ maze.grid.Length = w*2 + 1 && (maze.grid |> Array.every (fun (row: _ array) -> row.Length = h*2 + 1)) @>
        } |> Property.check
    testCase "checkNavigationDirections" <| fun _ ->
        property {
            let! x = Range.linear 0 49 |> Gen.int32
            let! y = Range.linear 0 49 |> Gen.int32
            // ensure an odd number for both x and y
            let x = x * 2 + 1
            let y = y * 2 + 1
            let reverse = function Up -> Down | Down -> Up | Left -> Right | Right -> Left
            let! direction = chooseFrom [Up;Down;Left;Right]
            let startPoint = Point(x,y,(100, 100))
            let endPoint = moveTo direction startPoint
            test <@ (connectionTo direction startPoint) = (connectionTo (reverse direction) endPoint) @>
            test <@ startPoint.isValid() @>
            where (not(x = 1 && direction = Left || y = 1 && direction = Up))
            test <@ endPoint.isValid() && (connectionTo direction startPoint).isValid() @>

        } |> Property.check
    testCase "checkNearestIntersection" <| fun _ ->
        property {
            let! xPixel = Range.linear 0 100 |> Gen.int32
            let! yPixel = Range.linear 0 100 |> Gen.int32
            let maze = (newMaze(100, 100, true))
            let (Connection(x2, y2, _)) = Maze.nearestIntersection maze (xPixel,yPixel)
            let closeTo n m = abs(n-m) <= 20 || (x2 = 0 || y2 = 0) && abs(n-m) <= 30 // in corner cases the nearest connection could be up to 30 pixels away
            let yBase = (maze.grid[0].Length - 1) * 20
            test <@ closeTo (x2*20+10) xPixel && closeTo (yBase - (y2*20+10)) yPixel @>
            }
        |> Property.check
]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args allTests
