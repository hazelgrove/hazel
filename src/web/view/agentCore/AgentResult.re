open Util_web;
open Ppx_yojson_conv_lib.Yojson_conv;

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Info(string);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};
