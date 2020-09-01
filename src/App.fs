module App

open Elmish
open Elmish.React
open Feliz

module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type HackernewsItem = {
  id : int
  title : string
  url : string option
}

type State =
    { StoryItems : Deferred<Result<HackernewsItem list, string>> }

type Msg =
    | LoadStoryItems of AsyncOperationStatus<Result<HackernewsItem list, string>>

let init() =
    let initialState = { StoryItems = HasNotStartedYet }
    let initialCmd = Cmd.ofMsg (LoadStoryItems Started)
    initialState, initialCmd

let loadStoryItems = async {
  do! Async.Sleep 1500
  let storyItems = [ { id = 1; title = "Example title"; url = None } ]
  return LoadStoryItems (Finished (Ok storyItems))
}

let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | LoadStoryItems Started ->
        let nextState = { state with StoryItems = InProgress }
        nextState, Cmd.fromAsync loadStoryItems

    | LoadStoryItems (Finished (Ok storyItems)) ->
        let nextState = { state with StoryItems = Resolved (Ok storyItems) }
        nextState, Cmd.none

    | LoadStoryItems (Finished (Error error)) ->
        let nextState = { state with StoryItems = Resolved (Error error) }
        nextState, Cmd.none

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]
  
let renderItem item =
  Html.div [
    prop.key item.id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      match item.url with
      | Some url ->
        Html.a [
          prop.style [ style.textDecoration.underline ]
          prop.target.blank
          prop.href url
          prop.text item.title
        ]
      | None ->
        Html.p item.title
    ]
  ]

let spinner =
  Html.div [
    prop.style [ style.textAlign.center; style.marginTop 20 ]
    prop.children [
      Html.i [
        prop.className "fa fa-cog fa-spin fa-2x"
      ]
    ]
  ]

let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) -> React.fragment [ for item in items -> renderItem item ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Html.h1 [
        prop.className "title"
        prop.text "Elmish Hackernews"
      ]

      renderItems state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run