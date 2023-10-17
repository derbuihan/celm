function test() {
    elm_file=$(mktemp)

    cat > ${elm_file}
    node ./src/app.js ${elm_file}
    gcc -o ./output/tmp ./output/tmp.s
    ./output/tmp;
    ret=$?

    if [ "$ret" == "$1" ]; then
        echo "ok $1 = $ret"
        echo "---"
    else
        echo "ng $1 != $ret"
        exit 1
    fi

    rm ${elm_file}
}

test 42 <<EOF
module Main exposing (main)
main = 42
EOF

test 33 <<EOF
module Main exposing (main)
main = 11 + 22
EOF

test 11 <<EOF
module Main exposing (main)
main = 22 - 11
EOF

test 15 <<EOF
module Main exposing (main)
main = 1 + 2 + 3 + 4 + 5
EOF

test 5 <<EOF
module Main exposing (main)
main = 10 - 2 - 3
EOF

test 7 <<EOF
module Main exposing (main)
main = 1 + 2 * 3
EOF

test 5 <<EOF
module Main exposing (main)
main = 1 * 2 + 3
EOF

test 4 <<EOF
module Main exposing (main)
main = 12 / 3
EOF

test 3 <<EOF
module Main exposing (main)
main = 1 + 4 * 3 / 6
EOF

test 9 <<EOF
module Main exposing (main)
main = (1 + 2) * 3
EOF

test 15 <<EOF
module Main exposing (main)
main = (1 + 2 + 3) * (2 + 4 - 3) - 3 + (12 + 18) / 3 - 10
EOF

test 84 <<EOF
module Main exposing (main)
main = 12 * (3 + 4)
EOF

echo "ALL OK"
