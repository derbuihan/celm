module Typed.Pattern exposing (TypedPattern(..), fromNodePattern)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Meta, Type(..), TypedNode(..))


type TypedPattern
    = TypedIntPattern Int


fromPattern : Range -> Pattern -> Result (List DeadEnd) TypedPattern
fromPattern range_ pattern =
    case pattern of
        IntPattern int ->
            Ok (TypedIntPattern int)

        _ ->
            Err [ DeadEnd range_.start.row range_.start.column (Problem "Unsupported pattern") ]


fromNodePattern : Node Pattern -> Result (List DeadEnd) (TypedNode TypedPattern)
fromNodePattern (Node range_ node) =
    let
        meta_ : Result (List DeadEnd) Meta
        meta_ =
            getMetaNodePattern (Node range_ node)

        typedPattern : Result (List DeadEnd) TypedPattern
        typedPattern =
            fromPattern range_ node
    in
    Result.map2 (\m p -> TypedNode m p) meta_ typedPattern


getMetaNodePattern : Node Pattern -> Result (List DeadEnd) Meta
getMetaNodePattern (Node r p) =
    case p of
        IntPattern _ ->
            Ok { range = r, type_ = Int }

        _ ->
            Err [ DeadEnd r.start.row r.start.column (Problem "Unsupported pattern") ]
