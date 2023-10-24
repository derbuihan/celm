const Elm = require("../output/main").Elm;
const path = require("path");
const fs = require("fs");

const main = Elm.Main.init();
const args = process.argv.slice(2);
if (args.length < 1) {
    console.log("Usage: node app.js <filepath>");
    process.exit(1);
}

const filepath = args[0];
const input = fs.readFileSync(filepath, "utf-8");
const filename = path.basename(filepath, ".elm");

main.ports.get.send(input);

main.ports.putAST.subscribe(function (ast) {
    const output = path.join("output", filename + ".elm");
    fs.writeFileSync(output, ast);
});

main.ports.putTypedAST.subscribe(function (typedast) {
    const output = path.join("output", filename + ".typed.elm");
    fs.writeFileSync(output, typedast);
});

main.ports.putCode.subscribe(function (code) {
    const output = path.join("output", filename + ".s");
    fs.writeFileSync(output, code);
});

main.ports.debug.subscribe(function (status) {
    console.log(filename, ":", status);
    const [_, col, problem, row] = status.match(
        /{ col = (\d+), problem = (.*?), row = (\d+) }/
    );
    const line = input.split("\n")[row - 1];
    console.log(row + ": " + line);
    console.log(" ".repeat(col - 1 + row.length + 2) + "^");
    console.log(problem);
});
