open Util;

/**
 * Internal extension of client-facing Grammar module.
 * Exists separately to minimize client spec reqs.
 */
include Grammar;

module Zipper = {
  type t('subj) = {
    sort: Sort.t,
    prec: Prec.t,
    zipper: Regex.Zipper.t('subj),
  };

  let map = failwith("todo");
  let map_opt = failwith("todo");
};

[@warning "-27"]
let enter = (~from: Dir.t, ~bound=Prec.min, s: Sort.t) =>
  failwith("todo Gram.enter");
