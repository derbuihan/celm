module Typed.Pattern exposing (TypedPattern(..), fromNodePattern)

import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Parser exposing (DeadEnd, Problem(..))
import Typed.Node exposing (Env, Type(..), TypedNode(..), countEnv)


type TypedPattern
    = TypedIntPattern Int


fromNodePattern : Env -> Node Pattern -> Result (List DeadEnd) (TypedNode TypedPattern)
fromNodePattern env_ (Node range_ node) =
    let
        { row, column } =
            range_.start
    in
    case node of
        IntPattern int ->
            Ok (TypedNode { range = range_, type_ = Int, env = countEnv env_ } (TypedIntPattern int))

        _ ->
            Err [ DeadEnd row column (Problem "Unsupported pattern") ]
