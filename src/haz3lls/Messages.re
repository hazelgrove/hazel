open Sexplib.Std;

module Message = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {jsonrpc: string};

  [@deriving (show({with_path: false}), sexp, yojson)]
  type lspAny =
    | LspObject(list((string, lspAny)))
    | LspArray(list(lspAny))
    | LspString(string)
    | LspInt(int)
    | LspUint(int)
    | LspDecimal(float)
    | LspBool(bool)
    | LspNull;
};

module RequestMessage = {
  include Message;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type id_ty =
    | Int
    | String;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: id_ty,
    method: string,
    params: option(lspAny),
  };
};

module ResponseMessage = {
  include Message;

  module ResponseError = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      code: int,
      message: string,
      data: option(lspAny),
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type id_ty =
    | Int
    | String;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: id_ty,
    result: option(lspAny),
    error: option(ResponseError.t),
  };
};

module NotificationMessage = {
  include Message;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    method: string,
    params: option(lspAny),
  };
};

module ProgressParams = {
  include Message;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type progressToken = 
      | Integer(int)
      | String(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = {
    token: progressToken,
    value: 'a,
  };
};
