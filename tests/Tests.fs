module Tests
open Expecto
open Hedgehog
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote

let appTests = testList "App tests" [
    testCase "add works" <| fun _ ->
        let result = (+) 2 3
        Expect.equal result 5 "Result must be 5"
]

let chooseFrom inputs =
    inputs |> List.map Gen.constant |> Gen.choice

[<Tests>]
let allTests = testList "All" [
    appTests

    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args allTests
