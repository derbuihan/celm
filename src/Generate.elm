module Generate exposing (generate)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))


genExpr : Expression -> String
genExpr expr =
    case expr of
        Integer val ->
            "    mov x0, " ++ String.fromInt val ++ "\n"

        _ ->
            ""


genNodeExpr : Node Expression -> String
genNodeExpr node =
    case node of
        Node _ expr ->
            genExpr expr


genNodeFuncImpl : Node FunctionImplementation -> String
genNodeFuncImpl node =
    case node of
        Node _ func ->
            genNodeExpr func.expression


genFunc : Function -> String
genFunc func =
    let
        funcImpl =
            func.declaration
    in
    genNodeFuncImpl funcImpl


genDeclaration : Declaration -> String
genDeclaration declaration =
    case declaration of
        FunctionDeclaration func ->
            genFunc func

        Destructuring _ node ->
            genNodeExpr node

        _ ->
            ""


genNodeDecl : Node Declaration -> String
genNodeDecl node =
    case node of
        Node _ decl ->
            genDeclaration decl


generate : File -> String
generate ast =
    let
        declarations =
            ast.declarations
    in
    """.text
    .globl _main
    .align 2
_main:
"""
        ++ (case declarations of
                [] ->
                    ""

                x :: _ ->
                    genNodeDecl x
           )
        ++ "    ret\n"
