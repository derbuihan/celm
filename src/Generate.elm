module Generate exposing (generate)

import Parser exposing (DeadEnd, Problem(..))
import Result.Extra as Result
import Typed.Declaration exposing (TypedDeclaration(..))
import Typed.Expression exposing (TypedExpression(..), TypedFunctionImplementation, TypedLetDeclaration(..))
import Typed.File exposing (TypedFile)
import Typed.Node exposing (TypedNode(..), env, value)


push : String
push =
    "    str x0, [sp, -16]!"


pop : String -> String
pop reg =
    "    ldr " ++ reg ++ ", [sp], 16"


genNodeLetDeclaration : TypedNode TypedLetDeclaration -> Result (List DeadEnd) String
genNodeLetDeclaration (TypedNode _ decl) =
    case decl of
        TypedLetFunction func ->
            let
                genNodeFuncImpl : TypedNode TypedFunctionImplementation -> Result (List DeadEnd) String
                genNodeFuncImpl (TypedNode _ funcImpl) =
                    let
                        name : Char
                        name =
                            funcImpl.name |> value |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault 'x'

                        offset : Int
                        offset =
                            (Char.toCode name - 97 + 1) * 16

                        expr : TypedNode TypedExpression
                        expr =
                            funcImpl.expression

                        codeExpr : Result (List DeadEnd) String
                        codeExpr =
                            genNodeExpression expr
                    in
                    Result.map
                        (\expr_ ->
                            [ "    sub x0, x29, " ++ (offset |> String.fromInt)
                            , "    str x0, [sp, -16]!"
                            , expr_
                            , "    ldr x1, [sp], 16"
                            , "    str x0, [x1]"
                            ]
                                |> String.join "\n"
                        )
                        codeExpr

                code =
                    genNodeFuncImpl func.declaration
            in
            code


genNodeExpression : TypedNode TypedExpression -> Result (List DeadEnd) String
genNodeExpression (TypedNode meta expr) =
    let
        { row, column } =
            meta.range.start
    in
    case expr of
        TypedOperatorApplication opName _ lhsNode rhsNode ->
            let
                lhsExpr : Result (List DeadEnd) String
                lhsExpr =
                    genNodeExpression lhsNode

                rhsExpr : Result (List DeadEnd) String
                rhsExpr =
                    genNodeExpression rhsNode

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
                            Err [ DeadEnd row column (Problem "Gen: Unknown operator") ]
            in
            Result.map3 (\lhs op rhs -> [ rhs, push, lhs, pop "x1", op ] |> String.join "\n") lhsExpr opExpr rhsExpr

        TypedFunctionOrValue moduleName name ->
            let
                name_ : Char
                name_ =
                    name |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault 'x'

                offset : Int
                offset =
                    (Char.toCode name_ - 97 + 1) * 16
            in
            [ "    sub x0, x29, " ++ (offset |> String.fromInt)
            , "    ldr x0, [x0]"
            ]
                |> String.join "\n"
                |> Ok

        TypedIfBlock cond then_ else_ ->
            let
                condExpr : Result (List DeadEnd) String
                condExpr =
                    genNodeExpression cond

                thenExpr : Result (List DeadEnd) String
                thenExpr =
                    genNodeExpression then_

                endLabel : String
                endLabel =
                    ".L.end."
                        ++ (cond |> env |> .label |> String.fromInt)

                elseExpr : Result (List DeadEnd) String
                elseExpr =
                    genNodeExpression else_

                elseLabel : String
                elseLabel =
                    ".L.else."
                        ++ (else_ |> env |> .label |> String.fromInt)
            in
            Result.map3
                (\cond_ then__ else__ ->
                    [ cond_
                    , "    cbz x0, " ++ elseLabel
                    , then__
                    , "    b " ++ endLabel
                    , elseLabel ++ ":"
                    , else__
                    , endLabel ++ ":"
                    ]
                        |> String.join "\n"
                )
                condExpr
                thenExpr
                elseExpr

        TypedInteger val ->
            Ok ("    mov x0, " ++ String.fromInt val)

        TypedNegation node ->
            genNodeExpression node
                |> Result.map (\x -> x ++ "\n    neg x0, x0")

        TypedParenthesizedExpression node ->
            genNodeExpression node

        TypedLetExpression letblock ->
            let
                declarations : Result (List DeadEnd) (List String)
                declarations =
                    Result.combineMap
                        genNodeLetDeclaration
                        letblock.declarations

                expression : Result (List DeadEnd) String
                expression =
                    genNodeExpression letblock.expression
            in
            Result.map2
                (\decls expr_ ->
                    decls
                        ++ [ expr_ ]
                        |> String.join "\n"
                )
                declarations
                expression

        _ ->
            Err
                [ DeadEnd row column (Problem "Gen: Unsupported expression") ]


genNodeFunctionImplementation : TypedNode TypedFunctionImplementation -> Result (List DeadEnd) String
genNodeFunctionImplementation (TypedNode _ func) =
    genNodeExpression func.expression


genNodeDeclaration : TypedNode TypedDeclaration -> Result (List DeadEnd) String
genNodeDeclaration (TypedNode _ decl) =
    case decl of
        TypedFunctionDeclaration func ->
            genNodeFunctionImplementation func.declaration

        TypedDestructuring _ node ->
            genNodeExpression node


generate : TypedFile -> Result (List DeadEnd) String
generate ast =
    let
        declarations : Result (List DeadEnd) String
        declarations =
            case ast.declarations of
                [] ->
                    Err [ DeadEnd 0 0 UnexpectedChar ]

                x :: _ ->
                    genNodeDeclaration x
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
