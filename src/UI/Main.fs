module UI.Main

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz

importSideEffects "./styles/global.scss"

type State = { input: string; showAnswer: bool }
type Msg =
    | Reset
    | Enter
    | ChangeInput of string
let fresh = { input = ""; showAnswer = false }
let init _ = fresh
let update msg state =
    match msg with
    | Reset -> fresh
    | Enter -> { state with showAnswer = true }
    | ChangeInput input -> { state with input = input; showAnswer = false }

let answer = "3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067"
type MatchResult =
    | ExactMatch of string
    | Mistakes of correctPrefix: string * rest: string
    | NotEnough of correctPrefix: string * rest: string
let getMatch (input:string) (sourceOfTruth:string) =
    let n = min input.Length sourceOfTruth.Length
    let matchLength =
        Seq.zip (input.Substring(0, n)) (sourceOfTruth.Substring(0, n))
        |> Seq.takeWhile (fun (lhs, rhs) -> lhs = rhs)
        |> Seq.length
    if matchLength = sourceOfTruth.Length && matchLength = input.Length then
        // exact match! Only correct inputs, no wrong values, no missing values
        ExactMatch input
    elif matchLength < input.Length then
        Mistakes(input.Substring(0, matchLength), input.Substring(matchLength, input.Length - matchLength))
    else
        NotEnough(input, sourceOfTruth.Substring(matchLength, sourceOfTruth.Length - matchLength))
let view state dispatch =
    let enter _ =
        if state.showAnswer = false then dispatch Enter
        else dispatch Reset
    Html.div [
        prop.style [style.padding 20; style.fontSize 30; style.maxWidth (Browser.Dom.window.innerWidth - 60. |> int)]
        prop.children [
            Html.div [
                Html.text "Type pi in the input box and see how many digits you can get correct!"
                ]
            Html.div [
                prop.children [
                    Html.span [
                        Html.text "π = "
                        ]
                    Html.input [
                        prop.autoFocus true
                        prop.style [style.fontSize 30]
                        prop.size 60
                        prop.value state.input
                        prop.onChange (ChangeInput >> dispatch)
                        prop.onKeyDown (fun e -> if e.code = "Enter" then
                                                        enter())
                        ]
                    Html.button [
                        prop.style [style.fontSize 30]
                        prop.text (if state.showAnswer then "RESET" else "Enter")
                        prop.onClick (fun e -> enter())
                        ]
                    ]
                ]
            Html.h1 [
                if state.showAnswer then
                    match getMatch state.input answer with
                    | ExactMatch input ->
                        let decimals = input.Length - 2 |> max 0
                        Html.text $"Good job! You know pi out to {decimals} decimal places!"
                        Html.div [
                            Html.span [
                                prop.style [style.color color.black]
                                prop.text input
                                ]
                            ]
                    | Mistakes(input, errors) ->
                        let decimals = input.Length - 2 |> max 0
                        Html.div [
                            prop.style [style.display.block]
                            prop.children [
                                Html.span [
                                    prop.style [style.color color.black]
                                    prop.text input
                                    ]
                                Html.span [
                                    prop.style [style.color color.red]
                                    prop.text (answer.Substring(input.Length, min 7 (max (answer.Length - input.Length) 0)))
                                    ]
                                ]
                            ]
                    | NotEnough(input, rest) ->
                        let decimals = input.Length - 2 |> max 0
                        if decimals > 1 then
                            Html.text $"You know pi out to {decimals} decimal places!"
                        Html.div [
                            Html.span [
                                prop.style [style.color color.black]
                                prop.text input
                                ]
                            Html.span [
                                prop.style [style.color color.red]
                                prop.text (rest.Substring(0, min rest.Length 7))
                                ]
                            ]
                else
                    match getMatch state.input answer with
                    | ExactMatch input | NotEnough(input, _) ->
                        Html.span [
                            prop.style [style.color color.black]
                            prop.text input
                            ]
                    | Mistakes(input, rest) ->
                        Html.div [
                            Html.span [
                                prop.style [style.color color.black]
                                prop.text input
                                ]
                            Html.span [
                                prop.style [style.color color.red]
                                prop.text rest
                                ]
                            ]
                ]
            ]
        ]

Program.mkSimple init update view
|> Program.withReactSynchronous "feliz-app"
|> Program.run
