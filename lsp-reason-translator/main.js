const prompt = require('prompt-sync')();

let input = prompt("input\n");


let presetInput = `
interface RequestMessage extends Message {

	/**
	 * The request id.
	 */
	id: integer | string;

	/**
	 * The method to be invoked.
	 */
	method: string;

	/**
	 * The method's params.
	 */
	params?: array | object;
}
`;

if (input == "cli") {
    while (input != "exit") {
        console.log(translate(input));
        input = prompt("input\n");
    }
}
else {
    console.log(translate(presetInput));
}




function startsWith(str, test) {
    return str.indexOf(test) === 0;
}

function endsWith(str, test) {
    return str.indexOf(test) === str.length - test.length;
}

function translate (input) {
    let lines = input.split("\n");

    let output = "";

    for (var line of lines) {

        //skip the comments for now
        if (line.includes("/*") || line.includes("*") || line.includes("*/")) {
            continue;
        }

        //skip the empty lines
        if (line.trim() === "") {
            continue;
        }

        if (startsWith(line, "interface")) {
            if (line.includes("extends")) {
            let extended = line.substring(line.indexOf("extends") + 8, line.indexOf("{") - 1);
            output += line.replace("interface", "module").replace("{", "= {").replace("extends", "").replace(extended, "") + `
    include ${extended}; \n
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
`;
            }

            else {
            output += line.replace("interface", "module").replace("{", "= {") + "\n" + `
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
`;
            }

            continue;
        }

        if (line.includes("|")) {
            let indexOfColon = line.indexOf(":");

            line = line.substring(0, indexOfColon + 1) + " lspAny,";
        } 

        if (line.includes("?")) {
            line.replace("?", "");

            let indexOfColon = line.indexOf(":");
            let nextWord = line.substring(indexOfColon + 2, line.length - 1);

            line = line.substring(0, indexOfColon + 1) + ` option(${nextWord}),`;
        }
        
        output += line.replace(";", ",") + "\n";

    }

    output += "};";

    return output;
}
