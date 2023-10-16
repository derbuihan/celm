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

test 11 <<EOF
module Main exposing (main)
main = 11
EOF


test 42 <<EOF
module Main exposing (main)
main = 42
EOF

echo "ALL OK"
