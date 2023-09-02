var Elm = require("./main").Elm;
var main = Elm.Main.init();

var args = process.argv.slice(2);
if (args.length < 1) {
    console.log("   Usage: node app.js <input>");
    process.exit(1);
}

var input = args[0];
main.ports.get.send(input);
main.ports.put.subscribe(function (data) {
    var fs = require("fs");
    fs.writeFileSync("output/tmp.s", data);
});
main.ports.debug.subscribe(function (data) {
    console.log(input);
    console.log(data);
});
