module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;
open Util.OptUtil.Syntax;
open Js_of_ocaml;
open Js_of_ocaml.Dom_html;

module Model = AssistantModel;

module F =
  Store.F({
    [@deriving (show({with_path: false}), yojson, sexp)]
    type t = Model.t;
    let default = () => {
      Model.init;
    };
    let key = Store.Assistant;
  });
