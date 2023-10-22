# celm: Elm compiler written in Elm

celm is an experimental compiler that parses Elm code and generates ARM64 assembly code. celm is written in Elm and developed on Apple Silicon.

## How does it work?

Take this Elm expression for example:

```elm
module Main exposing (main)
main = 3 + 4 * 3
```

celm compiles the above code into the following assembly instruction sequence:

```
.text
    .globl _main
    .align 2
_main:
    mov x0, 3
    str x0, [sp, -16]!
    mov x0, 4
    ldr x1, [sp], 16
    mul x0, x0, x1
    str x0, [sp, -16]!
    mov x0, 3
    ldr x1, [sp], 16
    add x0, x0, x1
    ret
```

## Getting Started

You can get started with the celm experiment by first cloning the necessary repositories.

```bash
git clone git@github.com:derbuihan/elm-syntax.git
git clone git@github.com:derbuihan/celm.git
cd celm
npm install
```

After installing, you can build with the following command:

```bash
npm run build
```

Now, you are ready to write your Elm code, please save it as tmp/tmp.elm. For example, the code can be a single instruction like:

```elm
module Main exposing (main)

main = 1
```

To compile the Elm code to an executable and run it, use the following commands:

```bash
npm run compile "tmp/tmp.elm"
clang output/tmp.s -o output/tmp; ./output/tmp; echo $?;
```

If you want to run tests, execute the following command:

```bash
npm run test
```

Happy Coding!
