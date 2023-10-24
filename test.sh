function test() {
    elm_file=$(mktemp)

    cat > ${elm_file}
    node ./src/app.js ${elm_file}

    filname=$(basename ${elm_file} | sed -e "s/\.elm//g")
    gcc -o ./output/${filname} ./output/${filname}.s
    ./output/${filname};
    ret=$?

    if [ "$ret" == "$1" ]; then
        echo "ok $1 = $ret"
        echo "---"
    else
        echo "ng $1 != $ret"
        exit 1
    fi

    rm ${elm_file} ./output/${filname}*;
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

test 11 <<EOF
module Main exposing (main)
main = -11 +22
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 == 11
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 == 22
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 /= 11
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 /= 22
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 < 11
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 < 22
EOF

test 0 <<EOF
module Main exposing (main)
main = 22 < 11
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 <= 11
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 <= 22
EOF

test 0 <<EOF
module Main exposing (main)
main = 22 <= 11
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 > 11
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 > 22
EOF

test 1 <<EOF
module Main exposing (main)
main = 22 > 11
EOF

test 1 <<EOF
module Main exposing (main)
main = 11 >= 11
EOF

test 0 <<EOF
module Main exposing (main)
main = 11 >= 22
EOF

test 1 <<EOF
module Main exposing (main)
main = 22 >= 11
EOF

test 1 <<EOF
module Main exposing (main)
main = if 1 == 1 then 1 else 2
EOF

test 2 <<EOF
module Main exposing (main)
main = if 1 /= 1 then 1 else 2
EOF

test 6 <<EOF
module Main exposing (main)
main = if 0 /= 0 then if 0 /= 0 then 3 else 4 else if 0 /= 0 then 5 else 6
EOF

test 5 <<EOF
module Main exposing (main)
main = if 0 /= 0 then if 0 /= 0 then 3 else 4 else if 0 == 0 then 5 else 6
EOF

test 6 <<EOF
module Main exposing (main)
main = if 0 /= 0 then if 0 == 0 then 3 else 4 else if 0 /= 0 then 5 else 6
EOF

test 4 <<EOF
module Main exposing (main)
main = if 0 == 0 then if 0 /= 0 then 3 else 4 else if 0 /= 0 then 5 else 6
EOF

test 5 <<EOF
module Main exposing (main)
main = if 0 /= 0 then if 0 == 0 then 3 else 4 else if 0 == 0 then 5 else 6
EOF

test 4 <<EOF
module Main exposing (main)
main = if 0 == 0 then if 0 /= 0 then 3 else 4 else if 0 == 0 then 5 else 6
EOF

test 3 <<EOF
module Main exposing (main)
main = if 0 == 0 then if 0 == 0 then 3 else 4 else if 0 /= 0 then 5 else 6
EOF

test 3 <<EOF
module Main exposing (main)
main = if 0 == 0 then if 0 == 0 then 3 else 4 else if 0 == 0 then 5 else 6
EOF

test 11 <<EOF
module Main exposing (main)
main = let
    a = 11
in
    a
EOF

test 33 <<EOF
module Main exposing (main)
main = let
    d = 11 + 22
in
    d
EOF

test 110 <<EOF
module Main exposing (main)
main = let
    a = 11 + 22
    b = 33 + 44
in
    a + b
EOF

test 30 <<EOF
module Main exposing (main)
main = let
    a = 11
    b = a + 22
    c = b - 13
    d = c / 2
    e = d * 3
in
    e
EOF

test 60 <<EOF
module Main exposing (main)
main = let
    c = 10
    b = 20
    a = 30
in
    a + b + c
EOF

test 11 <<EOF
module Main exposing (main)
main = if True then 11 else 22
EOF

test 22 <<EOF
module Main exposing (main)
main = if False then 11 else 22
EOF

test 41 <<EOF
module Main exposing (main)
main = let
    e = d * 3 + a
    b = a + 22
    a = 11
    d = c / 2 
    c = b - 13
in
    e
EOF


echo "ALL OK"
