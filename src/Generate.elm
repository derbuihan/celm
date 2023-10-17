module Generate exposing (generate)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node(..))


push : String
push =
    "    str x0, [sp, -16]!"


pop : String -> String
pop reg =
    "    ldr " ++ reg ++ ", [sp], 16"


genExpr : Expression -> String
genExpr expr =
    case expr of
        Integer val ->
            "    mov x0, " ++ String.fromInt val

        Negation node ->
            [ genNodeExpr node, "    neg x0, x0" ]
                |> String.join "\n"

        OperatorApplication opName _ lhsNode rhsNode ->
            let
                lhs =
                    genNodeExpr lhsNode

                rhs =
                    genNodeExpr rhsNode

                op =
                    case opName of
                        "+" ->
                            "add"

                        "-" ->
                            "sub"

                        "*" ->
                            "mul"

                        "/" ->
                            "sdiv"

                        _ ->
                            "; unknown operator"
            in
            [ rhs, push, lhs, pop "x1", "    " ++ op ++ " x0, x0, x1" ]
                |> String.join "\n"

        ParenthesizedExpression node ->
            genNodeExpr node

        _ ->
            "; unknown expression"


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


genDecl : Declaration -> String
genDecl declaration =
    case declaration of
        FunctionDeclaration func ->
            genFunc func

        Destructuring _ node ->
            genNodeExpr node

        _ ->
            "; unknown declaration"


genNodeDecl : Node Declaration -> String
genNodeDecl node =
    case node of
        Node _ decl ->
            genDecl decl


generate : File -> String
generate ast =
    let
        declarations =
            case ast.declarations of
                [] ->
                    "; no declarations"

                x :: _ ->
                    genNodeDecl x
    in
    [ ".text"
    , "    .globl _main"
    , "    .align 2"
    , "_main:"
    , declarations
    , "    ret"
    ]
        |> String.join "\n"
