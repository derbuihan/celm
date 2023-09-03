module Generate exposing (generate)

import Type exposing (Expr(..))


push : String
push =
    "  str x0, [sp, -16]!\n"


pop : String -> String
pop reg =
    "  ldr " ++ reg ++ ", [sp], 16\n"


gen_expr : Expr -> String
gen_expr e =
    case e of
        Int x ->
            "  mov x0, " ++ String.fromInt x ++ "\n"

        Neg x ->
            gen_expr x ++ "  neg x0, x0\n"

        Add x y ->
            gen_expr y ++ push ++ gen_expr x ++ pop "x1" ++ "  add x0, x0, x1\n"

        Sub x y ->
            gen_expr y ++ push ++ gen_expr x ++ pop "x1" ++ "  sub x0, x0, x1\n"

        Mul x y ->
            gen_expr y ++ push ++ gen_expr x ++ pop "x1" ++ "  mul x0, x0, x1\n"

        Div x y ->
            gen_expr y ++ push ++ gen_expr x ++ pop "x1" ++ "  sdiv x0, x0, x1\n"


generate : Expr -> String
generate e =
    "  .text\n"
        ++ "  .globl _main\n"
        ++ "  .p2align 2\n"
        ++ "_main:\n"
        ++ gen_expr e
        ++ "  ret\n"
