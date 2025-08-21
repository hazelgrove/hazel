include Haz3lcorep;
open Util;
module Fresh = Language.IdTagged.FreshGrammar;

let projector_init = (_, _, _) => None;
let seg_of_projector = _ => [];
let update_projector = (~sort as _, ~id as _, _, p) => p;
let livelit_projectors = [];
let projector_to_term = (~sort as _, ~id as _, _) => Language.Grammar.Any();
let make_term_prj = (~sort as _, p) => (
  p,
  Calc.OldValue(Language.Grammar.Exp(Fresh.Exp.empty_hole())),
);
let shape_of_projector = (~common as _, _) => ProjectorShape.default;
let calculate_projector = (~common as _, x) => x;
let of_projector = (~sort as _, ~id as _, _) => Language.Grammar.Any();

module Printer = {
  open Printer;
  let of_segment =
    of_segment(~projector_to_segment=default_projector_to_segment);
  let of_zipper =
    of_zipper(~projector_to_segment=default_projector_to_segment);
};

module Editor = {
  open Editor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t;
  module Model = {
    open Model;
    let mk_uncalculated: ZipperBase.t => t = Editor.Model.mk_uncalculated(_);
    let init = (~common, z: ZipperBase.t) =>
      mk_uncalculated(z)
      |> Editor.Update.make_term(~sort=Exp, ~make_term_prj)
      |> fst
      |> Editor.Update.calculate(
           ~common,
           ~projector_init,
           ~seg_of_projector,
           ~update_projector,
           ~livelit_projectors,
           ~projector_to_term,
           ~shape_of_projector,
           ~calculate_projector,
         );

    let to_move_s: t => 'a = to_move_s;
  };

  module Update = {
    open Update;
    let calculate = (~common, ed) =>
      calculate(
        ~common,
        ~projector_init,
        ~seg_of_projector,
        ~update_projector,
        ~livelit_projectors,
        ~projector_to_term,
        ~shape_of_projector,
        ~calculate_projector,
        ed,
      );

    let make_term = (~sort, ed) => make_term(~make_term_prj, ~sort, ed);
  };
};

module MakeTerm = {
  open MakeTerm;
  let from_zip_for_sem = from_zip_for_sem(~of_projector);
};
