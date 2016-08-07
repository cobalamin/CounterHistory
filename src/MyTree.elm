module MyTree exposing (Index, Tree(..), getValue, leaf, node, getSubtreeAt, getSubtrees, map, mapWithPath, traverseWithPath, traverseDownwards, goDown)

import Array exposing (Array, empty, fromList)
import Maybe

type alias Index = Int

type Tree a
    = Node a (Array (Tree a))


getValue : Tree a -> a
getValue (Node value _) =
    value


getSubtrees : Tree a -> Array (Tree a)
getSubtrees (Node _ subtrees) =
    subtrees


leaf : a -> Tree a
leaf value =
    Node value Array.empty


node : a -> List (Tree a) -> Tree a
node v children =
    Node v (fromList children)


getSubtreeAt : Array (Tree a) -> Index -> Maybe (Tree a, Array (Tree a))
getSubtreeAt subtrees index =
    let
        maybeTree =
            Array.get index subtrees

        leftTrees =
            Array.slice 0 index subtrees

        rightTrees =
            Array.slice (index+1) (Array.length subtrees) subtrees

        otherTrees =
            Array.append leftTrees rightTrees
    in
        Maybe.map (\tree -> (tree, otherTrees)) maybeTree


goDown : Int -> Tree a -> Maybe (Tree a)
goDown index (Node value subtrees) =
    getSubtreeAt subtrees index
        |> Maybe.map fst


traverseDownwards : Array Int -> Tree a -> Maybe (Tree a)
traverseDownwards path tree =
    let
        go index maybeTree =
            maybeTree `Maybe.andThen` \tree ->
                goDown index tree
    in
        Array.foldl go (Just tree) path


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
        Node v children ->
            Node (f v) (Array.map (map f) children)


mapWithPath : (a -> Array Int -> b) -> Tree a -> Tree b
mapWithPath f tree = traverseWithPath f Node tree


traverseWithPath : (a -> Array Int -> b) -> (b -> Array c -> c) -> Tree a -> c
traverseWithPath f nodefn tree =
    let
        go f t idxs idx =
            case t of
                Node v children ->
                    let
                        idxs' = Array.push idx idxs
                    in
                        nodefn (f v idxs')
                            (Array.indexedMap (\i child -> go f child idxs' i) children)
    in
        go f tree Array.empty 0
