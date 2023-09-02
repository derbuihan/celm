function test() {
    node ./src/app.js "$2"
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
}

test 11 "11"
test 33 "22+11"
test 11 "22-11"
test 66 "22+11+33"
test 44 "22+11+33-22"
test 55 "22+11+33-22+11"
test 12 "3*4"
test 23 "3+4*5"
test 17 "3*4+5"
test 3 "18//6"
test 2 "48//1//2//3//4"

test 9 "(1+2)*3"
test 15 "(1+2+3)*(2+4-3)-3+(12+18)//3-10"
test 84 "12*(3+4)"

echo "ALL OK"
