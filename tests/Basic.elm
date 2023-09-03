module Basic exposing (..)

import Evaluate exposing (evaluate)
import Expect exposing (equal)
import Parse exposing (parse)
import Test exposing (..)
import Type exposing (Expr(..))


tests : Test
tests =
    describe "Basic tests"
        [ test "int" <|
            \_ -> equal (parse "123") (Ok (Int 123))
        , test "int+int" <|
            \_ -> equal (parse "11+22") (Ok (Add (Int 11) (Int 22)))
        , test "int-int" <|
            \_ -> equal (parse "11-22") (Ok (Sub (Int 11) (Int 22)))
        , test "int*int" <|
            \_ -> equal (parse "11*22") (Ok (Mul (Int 11) (Int 22)))
        , test "int/int" <|
            \_ -> equal (parse "11//22") (Ok (Div (Int 11) (Int 22)))
        , test "int-int-int" <|
            \_ -> equal (parse "11-22-33") (Ok (Sub (Sub (Int 11) (Int 22)) (Int 33)))
        , test "int+int*int" <|
            \_ -> equal (parse "11+22*33") (Ok (Add (Int 11) (Mul (Int 22) (Int 33))))
        , test "-int" <|
            \_ -> equal (parse "-123") (Ok (Neg (Int 123)))
        , test "-(int + int)" <|
            \_ -> equal (parse "-(-11+22)") (Ok (Neg (Add (Neg (Int 11)) (Int 22))))
        , test "evaluate" <|
            \_ -> equal (Result.map evaluate (parse "30 + (4 - 2) * -5")) (Ok 20)
        ]
