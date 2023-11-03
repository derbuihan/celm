module Generate exposing (generate)

import Dict
import Parser exposing (DeadEnd, Problem(..))
import Result.Extra as Result
import Typed.Declaration exposing (TypedDeclaration(..))
import Typed.Expression exposing (TypedExpression(..), TypedFunctionImplementation, TypedLetDeclaration(..))
import Typed.File exposing (TypedFile)
import Typed.Node exposing (TypedNode(..), meta, value)


push : String
push =
    "    str x0, [sp, -16]!"


pop : String -> String
pop reg =
    "    ldr " ++ reg ++ ", [sp], 16"


genNodeLetDeclaration : TypedNode TypedLetDeclaration -> Result (List DeadEnd) String
genNodeLetDeclaration (TypedNode meta_ decl) =
    let
        { row, column } =
            meta_.range.start
    in
    case decl of
        TypedLetFunction func ->
            let
                genNodeFuncImpl : TypedNode TypedFunctionImplementation -> Result (List DeadEnd) String
                genNodeFuncImpl (TypedNode _ funcImpl) =
                    let
                        name : String
                        name =
                            func.declaration |> value |> .name |> value

                        offset : Result (List DeadEnd) Int
                        offset =
                            Dict.get name meta_.variables
                                |> Maybe.map (\varInfo -> varInfo.offset)
                                |> Result.fromMaybe [ DeadEnd row column (Problem "Gen: Unknown variable") ]

                        expression : TypedNode TypedExpression
                        expression =
                            funcImpl.expression

                        codeExpr : Result (List DeadEnd) String
                        codeExpr =
                            genNodeExpression expression
                    in
                    Result.map2
                        (\expr offset_ ->
                            [ "    sub x0, x29, " ++ (offset_ |> String.fromInt)
                            , "    str x0, [sp, -16]!"
                            , expr
                            , "    ldr x1, [sp], 16"
                            , "    str x0, [x1]"
                            ]
                                |> String.join "\n"
                        )
                        codeExpr
                        offset

                code : Result (List DeadEnd) String
                code =
                    genNodeFuncImpl func.declaration
            in
            code


genNodeExpression : TypedNode TypedExpression -> Result (List DeadEnd) String
genNodeExpression (TypedNode meta_ expr) =
    let
        { row, column } =
            meta_.range.start
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
            if name == "True" then
                Ok "    mov x0, 1"

            else if name == "False" then
                Ok "    mov x0, 0"

            else
                let
                    offset : Result (List DeadEnd) Int
                    offset =
                        Dict.get name meta_.variables
                            |> Maybe.map (\varInfo -> varInfo.offset)
                            |> Result.fromMaybe [ DeadEnd row column (Problem "Gen: Unknown variable") ]
                in
                Result.map
                    (\offset_ ->
                        [ "    sub x0, x29, " ++ (offset_ |> String.fromInt)
                        , "    ldr x0, [x0]"
                        ]
                            |> String.join "\n"
                    )
                    offset

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
                        ++ (cond |> meta |> .label |> String.fromInt)

                elseExpr : Result (List DeadEnd) String
                elseExpr =
                    genNodeExpression else_

                elseLabel : String
                elseLabel =
                    ".L.else."
                        ++ (else_ |> meta |> .label |> String.fromInt)
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
                { declarations, expression } =
                    letblock

                stack_size : Int
                stack_size =
                    (declarations |> List.length) * 16

                declarationsCode : Result (List DeadEnd) (List String)
                declarationsCode =
                    declarations |> Result.combineMap genNodeLetDeclaration

                expressionCode : Result (List DeadEnd) String
                expressionCode =
                    genNodeExpression expression
            in
            Result.map2
                (\decls expr_ ->
                    ("    sub sp, sp, " ++ (stack_size |> String.fromInt))
                        :: decls
                        ++ [ expr_ ]
                        |> String.join "\n"
                )
                declarationsCode
                expressionCode

        _ ->
            Err
                [ DeadEnd row column (Problem "Gen: Unsupported expression") ]


genNodeFunctionImplementation : TypedNode TypedFunctionImplementation -> Result (List DeadEnd) String
genNodeFunctionImplementation (TypedNode _ func) =
    let
        name : String
        name =
            func.name |> value

        expression : TypedNode TypedExpression
        expression =
            func.expression

        exprCode : Result (List DeadEnd) String
        exprCode =
            genNodeExpression expression
    in
    exprCode
        |> Result.map
            (\expr ->
                [ "    .text"
                , "    .globl _" ++ name
                , "    .align 2"
                , "_" ++ name ++ ":"
                , "    stp x29, x30, [sp, -16]!"
                , "    mov x29, sp"
                , expr
                , "    mov sp, x29"
                , "    ldp x29, x30, [sp], 16"
                , "    ret"
                ]
                    |> String.join "\n"
            )


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
        declarations : Result (List DeadEnd) (List String)
        declarations =
            ast.declarations
                |> Result.combineMap genNodeDeclaration
    in
    declarations |> Result.map (\decls -> decls |> String.join "\n")
