module Generate exposing (generate)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Parser exposing (DeadEnd, Problem(..))
import TypedAST exposing (Meta, TypedDeclaration(..), TypedExpression(..), TypedFile, TypedFunction, TypedFunctionImplementation, TypedNode(..), range)


push : String
push =
    "    str x0, [sp, -16]!"


pop : String -> String
pop reg =
    "    ldr " ++ reg ++ ", [sp], 16"


genExpr : Meta -> TypedExpression -> Result (List DeadEnd) String
genExpr meta expr =
    let
        range =
            meta.range
    in
    case expr of
        TypedInteger val ->
            Ok ("    mov x0, " ++ String.fromInt val)

        TypedNegation node ->
            genNodeExpr node
                |> Result.map (\x -> x ++ "\n    neg x0, x0")

        TypedOperatorApplication opName _ lhsNode rhsNode ->
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

        TypedParenthesizedExpression node ->
            genNodeExpr node

        _ ->
            Err
                [ DeadEnd range.start.row range.start.column (Problem "Unsupported expression") ]


genNodeExpr : TypedNode TypedExpression -> Result (List DeadEnd) String
genNodeExpr (TypedNode meta node) =
    genExpr meta node


genNodeFuncImpl : TypedNode TypedFunctionImplementation -> Result (List DeadEnd) String
genNodeFuncImpl (TypedNode _ func) =
    genNodeExpr func.expression


genFunc : TypedFunction -> Result (List DeadEnd) String
genFunc func =
    genNodeFuncImpl func.declaration


genDecl : Meta -> TypedDeclaration -> Result (List DeadEnd) String
genDecl meta declaration =
    let
        range =
            meta.range
    in
    case declaration of
        TypedFunctionDeclaration func ->
            genFunc func

        TypedDestructuring _ node ->
            genNodeExpr node


genNodeDecl : TypedNode TypedDeclaration -> Result (List DeadEnd) String
genNodeDecl (TypedNode meta decl) =
    genDecl meta decl


generate : TypedFile -> Result (List DeadEnd) String
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
