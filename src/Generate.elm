module Generate exposing (generate)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))


push : String
push =
    "    str x0, [sp, -16]!"


pop : String -> String
pop reg =
    "    ldr " ++ reg ++ ", [sp], 16"


genExpr : Range -> Expression -> Result (List DeadEnd) String
genExpr range expr =
    case expr of
        Integer val ->
            Ok ("    mov x0, " ++ String.fromInt val)

        Negation node ->
            genNodeExpr node
                |> Result.map (\x -> x ++ "\n    neg x0, x0")

        OperatorApplication opName _ lhsNode rhsNode ->
            let
                lhsExpr : Result (List DeadEnd) String
                lhsExpr =
                    genNodeExpr lhsNode

                rhsExpr : Result (List DeadEnd) String
                rhsExpr =
                    genNodeExpr rhsNode

                opExpr : Result (List DeadEnd) String
                opExpr =
                    case opName of
                        "+" ->
                            Ok "    add x0, x0, x1"

                        "-" ->
                            Ok "    sub x0, x0, x1"

                        "*" ->
                            Ok "    mul x0, x0, x1"

                        "/" ->
                            Ok "    sdiv x0, x0, x1"

                        "==" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, eq"
                            ]
                                |> String.join "\n"
                                |> Ok

                        "/=" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, ne"
                            ]
                                |> String.join "\n"
                                |> Ok

                        "<=" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, le"
                            ]
                                |> String.join "\n"
                                |> Ok

                        "<" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, lt"
                            ]
                                |> String.join "\n"
                                |> Ok

                        ">=" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, ge"
                            ]
                                |> String.join "\n"
                                |> Ok

                        ">" ->
                            [ "    cmp x0, x1"
                            , "    cset x0, gt"
                            ]
                                |> String.join "\n"
                                |> Ok

                        _ ->
                            Err [ DeadEnd range.start.row range.start.column (Problem "Unknown operator") ]
            in
            Result.map3 (\lhs op rhs -> [ rhs, push, lhs, pop "x1", op ] |> String.join "\n") lhsExpr opExpr rhsExpr

        ParenthesizedExpression node ->
            genNodeExpr node

        _ ->
            Err
                [ DeadEnd range.start.row range.start.column (Problem "Unsupported expression") ]


genNodeExpr : Node Expression -> Result (List DeadEnd) String
genNodeExpr (Node range node) =
    genExpr range node


genNodeFuncImpl : Node FunctionImplementation -> Result (List DeadEnd) String
genNodeFuncImpl (Node _ func) =
    genNodeExpr func.expression


genFunc : Function -> Result (List DeadEnd) String
genFunc func =
    genNodeFuncImpl func.declaration


genDecl : Range -> Declaration -> Result (List DeadEnd) String
genDecl range declaration =
    case declaration of
        FunctionDeclaration func ->
            genFunc func

        Destructuring _ node ->
            genNodeExpr node

        _ ->
            Err [ DeadEnd range.start.row range.start.column (Problem "Unsupported declaration") ]


genNodeDecl : Node Declaration -> Result (List DeadEnd) String
genNodeDecl (Node range decl) =
    genDecl range decl


generate : File -> Result (List DeadEnd) String
generate ast =
    let
        declarations : Result (List DeadEnd) String
        declarations =
            case ast.declarations of
                [] ->
                    Err [ DeadEnd 0 0 UnexpectedChar ]

                x :: _ ->
                    genNodeDecl x
    in
    Result.map
        (\x ->
            [ ".text"
            , "    .globl _main"
            , "    .align 2"
            , "_main:"
            , x
            , "    ret"
            ]
                |> String.join "\n"
        )
        declarations
