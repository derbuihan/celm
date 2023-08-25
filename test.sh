function test() {
    node ./src/app.js "$2"
    gcc -o ./output/tmp ./output/tmp.s
    ./output/tmp;
    ret=$?

    if [ "$ret" == "$1" ]; then
        echo "ok $1 = $ret"
    else
        echo "ng $1 != $ret"
        exit 1
    fi
}


test 41 "42"

