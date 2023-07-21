const prompt = require('prompt-sync')();

let input = prompt("input\n");


// let presetInput = `
// interface ClientCapabilities {
// 	/**
// 	 * Workspace specific client capabilities.
// 	 */
// 	workspace?: {
// 		/**
// 		 * The client supports applying batch edits
// 		 * to the workspace by supporting the request
// 		 * 'workspace/applyEdit'
// 		 */
// 		applyEdit?: boolean;
//
// 		/**
// 		 * Capabilities specific to WorkspaceEdits
// 		 */
// 		workspaceEdit?: WorkspaceEditClientCapabilities;
//
// 		/**
// 		 * Capabilities specific to the workspace/didChangeConfiguration
// 		 * notification.
// 		 */
// 		didChangeConfiguration?: DidChangeConfigurationClientCapabilities;
//
//         foo?: {
//             bar?: {
//                 why?: string;
//             }
//         }
//     }
// }
// `;

let presetInput = `
interface ClientCapabilities {
	/**
	 * Workspace specific client capabilities.
	 */
	workspace?: {
		/**
		 * The client supports applying batch edits
		 * to the workspace by supporting the request
		 * 'workspace/applyEdit'
		 */
		applyEdit?: boolean;

		/**
		 * Capabilities specific to WorkspaceEdits
		 */
		workspaceEdit?: WorkspaceEditClientCapabilities;

		/**
		 * Capabilities specific to the workspace/didChangeConfiguration
		 * notification.
		 */
		didChangeConfiguration?: DidChangeConfigurationClientCapabilities;

		/**
		 * Capabilities specific to the workspace/didChangeWatchedFiles
		 * notification.
		 */
		didChangeWatchedFiles?: DidChangeWatchedFilesClientCapabilities;

		/**
		 * Capabilities specific to the workspace/symbol request.
		 */
		symbol?: WorkspaceSymbolClientCapabilities;

		/**
		 * Capabilities specific to the workspace/executeCommand request.
		 */
		executeCommand?: ExecuteCommandClientCapabilities;

		/**
		 * The client has support for workspace folders.
		 *
		 * @since 3.6.0
		 */
		workspaceFolders?: boolean;

		/**
		 * The client supports workspace/configuration requests.
		 *
		 * @since 3.6.0
		 */
		configuration?: boolean;

		/**
		 * Capabilities specific to the semantic token requests scoped to the
		 * workspace.
		 *
		 * @since 3.16.0
		 */
		 semanticTokens?: SemanticTokensWorkspaceClientCapabilities;

		/**
		 * Capabilities specific to the code lens requests scoped to the
		 * workspace.
		 *
		 * @since 3.16.0
		 */
		codeLens?: CodeLensWorkspaceClientCapabilities;

		/**
		 * The client has support for file requests/notifications.
		 *
		 * @since 3.16.0
		 */
		fileOperations?: {
			/**
			 * Whether the client supports dynamic registration for file
			 * requests/notifications.
			 */
			dynamicRegistration?: boolean;

			/**
			 * The client has support for sending didCreateFiles notifications.
			 */
			didCreate?: boolean;

			/**
			 * The client has support for sending willCreateFiles requests.
			 */
			willCreate?: boolean;

			/**
			 * The client has support for sending didRenameFiles notifications.
			 */
			didRename?: boolean;

			/**
			 * The client has support for sending willRenameFiles requests.
			 */
			willRename?: boolean;

			/**
			 * The client has support for sending didDeleteFiles notifications.
			 */
			didDelete?: boolean;

			/**
			 * The client has support for sending willDeleteFiles requests.
			 */
			willDelete?: boolean;
		};

		/**
		 * Client workspace capabilities specific to inline values.
		 *
		 * @since 3.17.0
		 */
		inlineValue?: InlineValueWorkspaceClientCapabilities;

		/**
		 * Client workspace capabilities specific to inlay hints.
		 *
		 * @since 3.17.0
		 */
		inlayHint?: InlayHintWorkspaceClientCapabilities;

		/**
		 * Client workspace capabilities specific to diagnostics.
		 *
		 * @since 3.17.0.
		 */
		diagnostics?: DiagnosticWorkspaceClientCapabilities;
	};

	/**
	 * Text document specific client capabilities.
	 */
	textDocument?: TextDocumentClientCapabilities;

	/**
	 * Capabilities specific to the notebook document support.
	 *
	 * @since 3.17.0
	 */
	notebookDocument?: NotebookDocumentClientCapabilities;

	/**
	 * Window specific client capabilities.
	 */
	window?: {
		/**
		 * It indicates whether the client supports server initiated
		 * progress using the window/workDoneProgress/create request.
		 *
		 * The capability also controls Whether client supports handling
		 * of progress notifications. If set servers are allowed to report a
		 * workDoneProgress property in the request specific server
		 * capabilities.
		 *
		 * @since 3.15.0
		 */
		workDoneProgress?: boolean;

		/**
		 * Capabilities specific to the showMessage request
		 *
		 * @since 3.16.0
		 */
		showMessage?: ShowMessageRequestClientCapabilities;

		/**
		 * Client capabilities for the show document request.
		 *
		 * @since 3.16.0
		 */
		showDocument?: ShowDocumentClientCapabilities;
	};

	/**
	 * General client capabilities.
	 *
	 * @since 3.16.0
	 */
	general?: {
		/**
		 * Client capability that signals how the client
		 * handles stale requests (e.g. a request
		 * for which the client will not process the response
		 * anymore since the information is outdated).
		 *
		 * @since 3.17.0
		 */
		staleRequestSupport?: {
			/**
			 * The client will actively cancel the request.
			 */
			cancel: boolean;

			/**
			 * The list of requests for which the client
			 * will retry the request if it receives a
			 * response with error code ContentModified
			 */
			 retryOnContentModified: string[];
		}

		/**
		 * Client capabilities specific to regular expressions.
		 *
		 * @since 3.16.0
		 */
		regularExpressions?: RegularExpressionsClientCapabilities;

		/**
		 * Client capabilities specific to the client's markdown parser.
		 *
		 * @since 3.16.0
		 */
		markdown?: MarkdownClientCapabilities;

		/**
		 * The position encodings supported by the client. Client and server
		 * have to agree on the same position encoding to ensure that offsets
		 * (e.g. character position in a line) are interpreted the same on both
		 * side.
		 *
		 * To keep the protocol backwards compatible the following applies: if
		 * the value 'utf-16' is missing from the array of position encodings
		 * servers can assume that the client supports UTF-16. UTF-16 is
		 * therefore a mandatory encoding.
		 *
		 * If omitted it defaults to ['utf-16'].
		 *
		 * Implementation considerations: since the conversion from one encoding
		 * into another requires the content of the file / line the conversion
		 * is best done where the file is read which is usually on the server
		 * side.
		 *
		 * @since 3.17.0
		 */
		positionEncodings?: PositionEncodingKind[];
	};

	/**
	 * Experimental client capabilities.
	 */
	experimental?: LSPAny;
}
`

if (input == "cli") {
    while (input != "exit") {
        let out = translate(input).combined;
        console.log(out);
        input = prompt("input\n");
    }
}
else {
    let out = translate(presetInput).combined
    console.log("translated:\n" + out);
}


function startsWith(str, test) {
    return str.indexOf(test) === 0;
}

function endsWith(str, test) {
    return str.indexOf(test) === str.length - test.length;
}

function translate(input, split = true, stop = "") {
    let lines;
    if (split) {
        lines = input.split("\n");
    }
    else {
        lines = input;
    }

    let output = "";

    let ctx = "";

    let loopIndex = 0;

    for (let i = 0; i < lines.length; i++) {
        loopIndex = i;
        let line = lines[i];

        //skip the comments for now
        if (line.includes("/*") || line.includes("*") || line.includes("*/")) {
            continue;
        }

        //skip the empty lines
        if (line.trim() === "") {
            continue;
        }

        if (line.includes(stop) && stop != "") {
            break;
        }

        if (startsWith(line, "interface")) {
            if (line.includes("extends")) {
                let extended = line.substring(line.indexOf("extends") + 8, line.indexOf("{") - 1);
                let idxOfExtended = line.indexOf("extends") + 8;

                let extendedRemoved = line.substring(0, idxOfExtended) + line.substring(idxOfExtended + extended.length, line.length)

                ctx += extendedRemoved.replace("interface", "module").replace("{", "= {").replace("extends  ", "") + `
    include ${extended}; \n `
                output += `
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
`;
            }

            else {
                ctx += line.replace("interface", "module").replace("{", "= {") + "\n"

                output += `
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
            line = line.replace("?", "");

            let indexOfColon = line.indexOf(":");

            //handling of optional nested records
            if (line.includes("{")) {
                let name = line.substring(0, indexOfColon).trim();
                line = line.substring(0, indexOfColon + 1) + ` option(${name}_ty),`;

                let header = `
    [@deriving (show({with_path: false}), sexp, yojson)]
    type ${name}_ty = {
`
                let nextInput = lines.slice(i + 1, lines.length);
                let returnedTranslate = translate(nextInput, false, "}");

                let headerAndOutput = header + returnedTranslate.output;

                ctx += returnedTranslate.ctx + headerAndOutput;

                i = returnedTranslate.idx + i + 1;
            }

            else {
                let nextWord = line.substring(indexOfColon + 2, line.length - 1);

                line = line.substring(0, indexOfColon + 1) + ` option(${nextWord}),`;
            }

        }


        else if (line.includes("{")) {
            typeString = line.substring(0, line.indexOf(":")).trim();
            console.log("typestring: " + typeString);
            let header = `
    [@deriving (show({with_path: false}), sexp, yojson)]
    type ${typeString}_ty = {
`

            let nextInput = lines.slice(i + 1, lines.length);
            line = line.replace("{", `${typeString}_ty;`)

            let returnedTranslate = translate(nextInput, false, "}");

            let headerAndOutput = header + returnedTranslate.output;

            ctx += returnedTranslate.ctx + headerAndOutput;


            i = returnedTranslate.idx + i + 1;
        }


        if (line.trim() == "};") {
            continue;
        }

        output += line.replace(";", ",") + "\n";
    }


    output += "};";

    // console.log("end of translate ctx: \n" + ctx);

    let ret = {
        ctx: ctx,
        output: output,
        combined: ctx + output,
        idx: loopIndex,
    }

    return ret;
}
