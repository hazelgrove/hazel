open Sexplib.Std;

module Position = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    line: int,
    char: int,
  };
};

module Range = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    start: Position.t,
    _end: Position.t,
  };
};

module TextDocumentItem = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    uri: string,
    languageId: string,
    version: int,
    text: string,
  };
};

module TextEdit = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    range: Range.t,
    newText: string,
  };   
};

module Diagnostic = {

    module DiagnosticSeverities = {
        let error = 1;
        let warning = 2;
        let information = 3;
        let hint = 4;
    }

    [@deriving (show({with_path: false}), sexp, yojson)]
    type code_ty = 
        | Int(int)
        | String(string);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
        range: Range.t,
        severity: option(int),
        code: option(code_ty),
        codeDescription: option(string),
        source: option(string),
        message: string,
    };
};
