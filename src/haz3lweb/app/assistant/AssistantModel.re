module Sexp = Sexplib.Sexp;
open Haz3lcore;
open Util;

type t = {none: bool};

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    show: bool,
    human: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleShow
    | ToggleHuman;
};
