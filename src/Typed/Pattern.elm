module Typed.Pattern exposing (TypedPattern(..), fromNodePattern)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Meta, Type(..), TypedNode(..), meta)


type TypedPattern
    = TypedIntPattern Int


fromNodePattern : Meta -> Node Pattern -> Result (List DeadEnd) ( Meta, TypedNode TypedPattern )
fromNodePattern meta_ (Node range_ node) =
    let
        { row, column } =
            range_.start

        lastMeta : Meta
        lastMeta =
            { meta_ | label = meta_.label + 1 }
    in
    case node of
        IntPattern int ->
            Ok
                ( lastMeta, TypedNode { lastMeta | range = range_ } (TypedIntPattern int) )

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported pattern") ]
