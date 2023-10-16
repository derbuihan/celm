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
main.ports.put.subscribe(function (data) {
    fs.writeFileSync("output/tmp.s", data);
});

main.ports.debug.subscribe(function (data) {
    console.log(input);
    console.log(data);
});
