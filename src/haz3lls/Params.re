open Sexplib.Std;

module WorkspaceFolder = {

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
        uri: string,
        name: string,
    };
}

module InitializeParams = {

    [@deriving (show({with_path: false}), sexp, yojson)]
    type clientInfo = {
        name: string,
        version: option(string),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
        processId: option(int),
        clientInfo: option(clientInfo),
        locale: option(string),
        rootPath: option(string),
        rootUri: option(string),
        initializationOptions: option(Messages.Message.lspAny),
        capabilities: Capabilities.ClientCapabilities.t,
        traceValue: option(string),
        workspaceFolders: option(list(WorkspaceFolder.t)),
    };

}
