module DrawTree exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (Svg, svg, circle, line, g, text', text)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import TreeDiagram exposing (drawSvg, node, defaultTreeLayout)
import MyTree exposing (Tree(..), traverseWithPath)
import MyTree as Tree
import Array exposing (Array, toList)


(=>) prop value =
    prop (toString value)


daTree : Tree Int
daTree =
    Tree.node 61
        [ Tree.node 84 [ Tree.node 22 [], Tree.node 38 [] ]
        , Tree.node 72
            [ Tree.node 3 [ Tree.node 59 [], Tree.node 29 [], Tree.node 54 [] ]
            , Tree.node 25 []
            , Tree.node 49 []
            ]
        , Tree.node 24 [ Tree.node 2 [] ]
        , Tree.node 17
            [ Tree.node 26 []
            , Tree.node 68
                [ Tree.node 13 []
                , Tree.node 36 []
                ]
            , Tree.node 86 []
            ]
        ]


coolTree : TreeDiagram.Tree ( Int, Array Int )
coolTree =
    traverseWithPath (,) (\v trees -> node v (toList trees)) daTree


drawLine : ( Float, Float ) -> ( Float, Float ) -> Svg msg
drawLine ( sourceX, sourceY ) ( targetX, targetY ) =
    line
        [ x1 => sourceX, y1 => sourceY, x2 => targetX, y2 => targetY, stroke "black" ]
        []


getLast : Array a -> Maybe a
getLast arr =
    Array.get (Array.length arr - 1) arr


arrayRest : Array a -> Array a
arrayRest arr =
    Array.slice 1 (Array.length arr) arr


drawNode : ( Int, Array Int ) -> Svg Msg
drawNode ( n, path ) =
    g [ onClick (GoDown (arrayRest path)) ]
        [ circle [ r "16", stroke "black", fill "white", cx "0", cy "0" ] []
        , text' [ textAnchor "middle", alignmentBaseline "middle" ] [ text (toString n) ]
        ]



--main =
--    drawSvg defaultTreeLayout drawNode drawLine coolTree
--


type alias Model =
    Tree Int


type Msg
    = GoDown (Array Int)


init =
    daTree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg tree =
    case msg of
        GoDown path ->
            (Tree.traverseDownwards path tree
                |> Maybe.withDefault tree
            )
                ! []


view : Model -> Svg Msg
view tree =
    let
        drawnTree =
            traverseWithPath (,) (\v trees -> node v (toList trees)) tree
    in
        drawSvg defaultTreeLayout drawNode drawLine drawnTree



--drawSvg : TreeLayout -> SvgNode a b -> SvgEdge b -> Tree a -> Svg b


main =
    App.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
