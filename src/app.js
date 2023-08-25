var Elm = require("./main").Elm;
var main = Elm.Main.init();

var args = process.argv.slice(2);
var input = parseInt(args[0]);
console.log("\n   Input: ", input);

main.ports.get.send(input);
main.ports.put.subscribe(function (data) {
    console.log("   Output: " + JSON.stringify(data) + "\n");
});
