module Counter exposing ( Model, Msg, main, init, view, update, subscriptions )

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Html.App as App


type alias Model = Int


type Msg = Increment | Decrement


main : Program Never
main =
    App.program { init = init 0, view = view, update = update, subscriptions = subscriptions }


init : Int -> (Model, Cmd Msg)
init i = i ! []


view : Model -> Html Msg
view count =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (toString count)
        , button [ onClick Increment ] [ text "+" ]
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
            (model + 1) ! []

        Decrement ->
            (model - 1) ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
