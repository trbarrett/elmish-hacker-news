module App

open Elmish
open Elmish.React
open Feliz
open Thoth.Json
open Fable.SimpleHttp

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
  score : int
}

type DeferredResult<'t> = Deferred<Result<'t, string>>

type DeferredStoryItem = DeferredResult<HackernewsItem>

[<RequireQualifiedAccess>]
type Stories =
    | New
    | Top
    | Best
    | Job

type State =
    { CurrentStories: Stories 
      StoryItems : DeferredResult<Map<int, DeferredStoryItem>> }

type Msg =
    | LoadStoryItems of AsyncOperationStatus<Result<int list, string>>
    | LoadedStoryItem of int * Result<HackernewsItem, string>
    | ChangeStories of Stories

let init() =
    { CurrentStories = Stories.New
      StoryItems = HasNotStartedYet }, Cmd.ofMsg (LoadStoryItems Started)

let storiesEndpoint stories =
  let fromBaseUrl = sprintf "https://hacker-news.firebaseio.com/v0/%sstories.json"
  match stories with
  | Stories.Best -> fromBaseUrl "best"
  | Stories.Top -> fromBaseUrl "top"
  | Stories.New -> fromBaseUrl "new"
  | Stories.Job -> fromBaseUrl "job"

let itemDecoder : Decoder<HackernewsItem> =
    Decode.object (fun fields -> {
        id = fields.Required.At [ "id" ] Decode.int
        title = fields.Required.At [ "title" ] Decode.string
        url = fields.Optional.At [ "url" ] Decode.string
        score = fields.Required.At [ "score" ] Decode.int
    })

let rnd = System.Random()

let loadStoryItem (itemId: int) = async {
    // simulate high network latency
    do! Async.Sleep (rnd.Next(1000, 3000))
    let endpoint = sprintf "https://hacker-news.firebaseio.com/v0/item/%d.json" itemId
    let! (status, responseText) = Http.get endpoint
    match status with
    | 200 ->
        match Decode.fromString itemDecoder responseText with
        | Ok storyItem -> return LoadedStoryItem (itemId, Ok storyItem)
        | Error parseError -> return LoadedStoryItem (itemId, Error parseError)
    | _ ->
      return LoadedStoryItem (itemId, Error ("Http error while loading " + string id))
}

let loadStoryItems stories = async {
    let endpoint = storiesEndpoint stories
    let! (status, responseText) = Http.get endpoint
    match status with
    | 200 ->
        // parse the response text as a list of IDs (integers)
        let storyIds = Decode.fromString (Decode.list Decode.int) responseText
        match storyIds with
        | Ok storyIds ->
            // take the first 10 IDs
            let firstTenStoryIds = storyIds |> List.truncate 10
            return LoadStoryItems (Finished (Ok firstTenStoryIds))
        | Error errorMsg ->
            // could not parse the array of story ID's
            return LoadStoryItems (Finished (Error errorMsg))
    | _ ->
        // non-OK response goes finishes with an error
        return LoadStoryItems (Finished (Error responseText))
}


let update (msg: Msg) (state: State) : State * Cmd<Msg> =
    match msg with
    | ChangeStories stories ->
        let nextState = { state with StoryItems = InProgress; CurrentStories = stories }
        let nextCmd = Cmd.fromAsync (loadStoryItems stories)
        nextState, nextCmd

    | LoadStoryItems Started ->
        let nextState = { state with StoryItems = InProgress }
        let nextCmd = Cmd.fromAsync (loadStoryItems state.CurrentStories)
        nextState, nextCmd

    | LoadStoryItems (Finished (Ok storyIds)) ->
        let storiesMap = Map.ofList [ for id in storyIds -> id, Deferred.InProgress ]
        let nextState = { state with StoryItems = Resolved (Ok storiesMap) }
        nextState, Cmd.batch [ for id in storyIds -> Cmd.fromAsync (loadStoryItem id) ]

    | LoadStoryItems (Finished (Error error)) ->
        let nextState = { state with StoryItems = Resolved (Error error) }
        nextState, Cmd.none

    | LoadedStoryItem (itemId, Ok item) ->
      match state.StoryItems with
      | Resolved (Ok storiesMap) ->
        let modifiedStoriesMap =
          storiesMap
          |> Map.add itemId (Resolved (Ok item))

        let nextState = { state with StoryItems = Resolved (Ok modifiedStoriesMap) }
        nextState, Cmd.none

      | _ ->
        state, Cmd.none

    | LoadedStoryItem (itemId, Error error) ->
      match state.StoryItems with
      | Resolved (Ok storiesMap) ->
        let modifiedStoriesMap =
          storiesMap
          |> Map.add itemId (Resolved (Error error))

        let nextState = { state with StoryItems = Resolved (Ok modifiedStoriesMap) }
        nextState, Cmd.none

      | _ ->
        state, Cmd.none

let storyCategories =
  [ Stories.New
    Stories.Top
    Stories.Best
    Stories.Job ]

let storiesName = function
  | Stories.New -> "New"
  | Stories.Best -> "Best"
  | Stories.Job -> "Job"
  | Stories.Top -> "Top"

let renderTab currentStories stories dispatch =
  let switchStories stories =
    if currentStories <> stories
    then dispatch (ChangeStories stories)

  Html.li [
    prop.className [ currentStories = stories, "is-active" ]
    prop.onClick (fun _ -> switchStories stories)
    prop.children [
      Html.a [ Html.span (storiesName stories) ]
    ]
  ]

let renderTabs currentStories dispatch =
  Html.div [
    prop.className [ "tabs"; "is-toggle"; "is-fullwidth" ]
    prop.children [
      Html.ul [
        for story in storyCategories ->
          renderTab currentStories story dispatch
      ]
    ]
  ]

let renderError (errorMsg: string) =
  Html.h1 [
    prop.style [ style.color.red ]
    prop.text errorMsg
  ]

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
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

  
let renderItemContent item =
  Html.div [
    prop.key item.id
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15 ]
    prop.children [
      div [ "columns"; "is-mobile" ] [
        div [ "column"; "is-narrow" ] [
          Html.div [
            prop.className ["icon"]
            prop.style [ style.marginLeft 20 ]
            prop.children [
              Html.i [ prop.className "fa fa-poll fa-2x"]
              Html.span [
                prop.style [ style.marginLeft 10; style.marginRight 10 ]
                prop.text item.score
              ]
            ]
          ]
        ]

        div [ "column"] [
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
    ]
  ]

let renderStoryItem (itemId: int) storyItem =
  let renderedItem =
    match storyItem with
    | HasNotStartedYet -> Html.none
    | InProgress -> spinner
    | Resolved (Error error) -> renderError error
    | Resolved (Ok storyItem) -> renderItemContent storyItem

  Html.div [
    prop.key itemId
    prop.className "box"
    prop.style [ style.marginTop 15; style.marginBottom 15]
    prop.children [ renderedItem ]
  ]


let renderItems = function
  | HasNotStartedYet -> Html.none
  | InProgress -> spinner
  | Resolved (Error errorMsg) -> renderError errorMsg
  | Resolved (Ok items) ->
    items
    |> Map.toList
    |> List.map (fun (id, storyItem) -> renderStoryItem id storyItem)
    |> Html.div

let title =
  Html.h1 [
    prop.className "title"
    prop.text "Elmish Hackernews"
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      title
      renderTabs state.CurrentStories dispatch
      renderItems state.StoryItems
    ]
  ]

Program.mkProgram init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run