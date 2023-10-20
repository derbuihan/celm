const Elm = require("../output/main").Elm;
const fs = require("fs");

const main = Elm.Main.init();
const args = process.argv.slice(2);
if (args.length < 1) {
    console.log("Usage: node app.js <filename>");
    process.exit(1);
}

const filename = args[0];
const input = fs.readFileSync(filename, "utf-8");

main.ports.get.send(input);

main.ports.putAST.subscribe(function (ast) {
    fs.writeFileSync("output/tmp.json", ast);
});

main.ports.putTypedAST.subscribe(function (typedast) {
    fs.writeFileSync("output/tmp.typed.json", typedast);
});

main.ports.putCode.subscribe(function (code) {
    fs.writeFileSync("output/tmp.s", code);
});

main.ports.debug.subscribe(function (status) {
    console.log(status);
    const [_, col, problem, row] = status.match(
        /{ col = (\d+), problem = (.*?), row = (\d+) }/
    );
    const line = input.split("\n")[row - 1];
    console.log(row + ": " + line);
    console.log(" ".repeat(col - 1 + row.length + 2) + "^");
    console.log(problem);
});
