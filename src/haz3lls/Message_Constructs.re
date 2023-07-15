open Sexplib.Std;
open Util;

module Message = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
      jsonrpc: string
  };

};

module RequestMessage = {
  include Message;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type id_ty =
    | Int(int)
    | String(string);

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
    | Int(int)
    | String(string);

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
      | Int(int)
      | String(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = {
    token: progressToken,
    value: 'a,
  };
};
