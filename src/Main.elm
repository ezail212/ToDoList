port module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "To Do List", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

-- MODEL


-- The full application state of our todo app.
type alias Model = 
    {   entries : List Entry,
        field : String,
        uid : Int,
        visibility : String
    }

type alias Entry = 
    {   content : String,
        isCompleted : Bool,
        isEditing : Bool,
        id : Int
    }


emptyModel : Model
emptyModel = 
    {   entries = [],
        field = "",
        uid = 0,
        visibility = "All"
    }


newEntry : String -> Int -> Entry
newEntry content id =
    {   content = content,
        isCompleted = False,
        isEditing = False,
        id = id
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
  ( Maybe.withDefault emptyModel maybeModel,
    Cmd.none
  )


-- UPDATE

type Msg
    = NothingToDo
    | UpdateField String
    | AddEntry
    | ClearCompleted
    | CheckAll Bool
    | Check Int Bool
    | Delete Int
    | UpdateEntry Int String
    | EditingEntry Int Bool
    | ChangeVisibility String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NothingToDo ->
            (model, Cmd.none)

        UpdateField newField->
            ( {model | field = newField}, Cmd.none)

        AddEntry ->
            ( {model | field = "", uid = model.uid + 1,
                entries = 
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [newEntry model.field model.uid] }
            , Cmd.none
            )

        ClearCompleted ->
            ( {model | entries = List.filter (not << .isCompleted) model.entries}
            , Cmd.none
            )

        CheckAll isAllCompleted ->
            let 
                changeStatus entry = 
                    {entry | isCompleted = isAllCompleted}
            in
            (  {model | entries = List.map changeStatus model.entries}
               , Cmd.none
                )

        Check id isCompleted ->
            let 
                updateEntry entry = 
                    if entry.id == id then
                        {entry | isCompleted = isCompleted}
                    else
                        entry
            in
            (   {model | entries = List.map updateEntry model.entries}
                , Cmd.none
                )

        Delete id ->
            (   {model | entries = List.filter (\entry -> entry.id /= id) model.entries}
                , Cmd.none
            ) 

        UpdateEntry id content->
            let
                updateEntry entry =
                    if entry.id == id then
                        {entry | content = content}
                    else
                        entry
            in
            (   {model | entries = List.map updateEntry model.entries}
                , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry entry =
                    if entry.id == id then
                        { entry | isEditing = isEditing }
                    else
                        entry

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Task.attempt (\_ -> NothingToDo) focus
            )

        ChangeVisibility visibility ->
            ( {model | visibility = visibility}
              , Cmd.none
            )


-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper" , style "visibility" "hidden"]
        [ section
            [ class "todoapp" ]
            [ lazy viewInput model.field,
              lazy2 viewEntries model.visibility model.entries,
              lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput field = 
    header
        [ class "header" ]
        [ h1 [] [ text "Todo List" ]
        , input
            [ class "new-todo"
            , placeholder "Write your todo list here"
            , autofocus True
            , value field
            , name "newTodo"
            , onInput UpdateField
            , onEnter AddEntry
            ]
            []
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "Not enter"
    in
        on "keydown" (Json.andThen isEnter keyCode)


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries = 
    let 
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.isCompleted

                "Active" ->
                    not todo.isCompleted

                _ ->
                    True

        isAllCompleted = 
            List.all .isCompleted entries

        cssVisibility = 
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style "visibility" cssVisibility
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked isAllCompleted
                , onClick (CheckAll (not isAllCompleted))
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ] <|
                List.map viewAllEntry (List.filter isVisible entries)
            ]


viewAllEntry : Entry -> ( String, Html Msg )
viewAllEntry entry =
    ( String.fromInt entry.id, lazy viewSingleEntry entry )


viewSingleEntry : Entry -> Html Msg
viewSingleEntry entry = 
    li
        [ classList [ ( "completed", entry.isCompleted ), ( "editing", entry.isEditing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked entry.isCompleted
                , onClick (Check entry.id (not entry.isCompleted))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry entry.id True) ]
                [ text entry.content ]
            , button
                [ class "destroy"
                , onClick (Delete entry.id)
                ]
                []
            ]
        , input
            [ class "edit"
            , value entry.content
            , name "title"
            , id ("todo-" ++ String.fromInt entry.id)
            , onInput (UpdateEntry entry.id)
            , onBlur (EditingEntry entry.id False)
            , onEnter (EditingEntry entry.id False)
            ]
            []
        ]


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .isCompleted entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty entries)
            ]
            [ lazy numberOfActiveEntries entriesLeft,
              lazy clearCompletedEntries entriesCompleted,
              lazy viewControlsFilters visibility
            ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility = 
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]

visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap childPageLink visibilityType visibility = 
        li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href childPageLink, classList [ ( "selected", visibilityType == visibility ) ] ]
            [ text visibilityType ]
        ]

numberOfActiveEntries : Int -> Html Msg
numberOfActiveEntries entriesLeft = 
    let
        itemString =
            if (entriesLeft == 1 || entriesLeft == 0) then
                "todo item"
            else
                "todo items"
    in
        span
            [ class "todo-count" ]
            [ strong [] [ text (String.fromInt entriesLeft) ]
            , text (" " ++ itemString ++ " left")
            ]

clearCompletedEntries : Int -> Html Msg
clearCompletedEntries entriesCompleted = 
    button [class "clear-completed", hidden (entriesCompleted == 0), onClick ClearCompleted]
           [text "Clear completed tasks"]



infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/ezail212" ] [ text "Ezail212" ]
            ]
        , p []
            [ text "PPTang has ugly hair"]
        ]