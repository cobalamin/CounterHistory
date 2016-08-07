module HistoryCounter exposing ( main, init, view, update, subscriptions )

import Html exposing (Html, div, span, text, button)
import Html.Attributes exposing (class, style, disabled)
import Html.Events exposing (onClick)
import Html.App as App

import HistoryTree as History exposing (HistoryTree) 
import Counter


type alias Model =
    { history : HistoryTree Counter.Model
    , error : Maybe String
    }


type Msg
    = Undo
    | Redo History.Index
    | ModelMsg Counter.Msg


main : Program Never
main =
    App.program { init = init 0
                , view = view
                , update = update
                , subscriptions = subscriptions
                }


(=>) : a -> b -> (a, b)
(=>) = (,)


init : Int -> (Model, Cmd Msg)
init i =
    let
        (childModel, childCmd) =
            Counter.init i
    in
        { history = History.init childModel
        , error = Nothing
        } ! [Cmd.map ModelMsg childCmd]


view : Model -> Html Msg
view {history, error} =
    div []
        ([ history |> History.current |> Counter.view |> App.map ModelMsg
        , button
              [ onClick Undo,
                disabled (not <| History.canUndo history)
              ]
              [ text "Undo" ]
        ]
          ++
        List.map undoButton [0..History.branchCount history - 1]
          ++
        [ div [ style ["color" => "red"] ] [ maybeError error ] ])



undoButton : Int -> Html Msg
undoButton i =
    button
        [ onClick (Redo i) ]
        [ text <| "Redo (branch " ++ toString i ++ ")" ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Undo ->
            case History.undo model.history of
                Just prevTree ->
                    { model | history = prevTree, error = Nothing } ! []
                Nothing ->
                    { model | error = Just "Can't undo, no previous point in time" } ! []

        Redo index ->
            case History.redo index model.history of
                Just nextTree ->
                    { model | history = nextTree, error = Nothing } ! []
                Nothing ->
                    { model | error = Just "Can't redo, no next point in time" } ! []

        ModelMsg msg ->
            let
                (newModel, childCmd) =
                    Counter.update msg (History.current model.history)
            in 
                { history = History.push newModel model.history
                , error = Nothing
                } ! [Cmd.map ModelMsg childCmd]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


maybeError : Maybe String -> Html Msg
maybeError maybeStr =
    case maybeStr of
        Nothing ->
            span [ class "error" ] []
        Just str ->
            span [ class "error "] [ text str ]
