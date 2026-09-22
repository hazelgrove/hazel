open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* Shared implementation of the numeric slider projectors. SliderProj and
   SliderFProj were byte-identical apart from the literal they edit, so the
   only thing a variant supplies is how to read that literal out of an atom,
   how to build one back from the slider's string value, and how to render it. */
module type PARAMS = {
  /* Prefixes the failure messages, e.g. "Slider: Get: ...". */
  let name: string;
  /* Names the literal in those messages, e.g. "integer". */
  let literal: string;
  /* The OCaml type of the literal being edited. */
  type t;
  let of_atom: Language.Atom.t => option(t);
  let to_atom: string => Language.Atom.t;
  let to_string: t => string;
};

module Make = (P: PARAMS) : Projector => {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let value_of = (any: Language.Any.t): option(P.t) =>
    switch (any) {
    | Exp({term: Atom(a), _}) => P.of_atom(a)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (value_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get = (info: info): P.t =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(value_of)
    ) {
    | Some(v) => v
    | None => failwith(P.name ++ ": Get: not " ++ P.literal ++ " literal")
    };

  let put = (info: info, v: string): Base.segment =>
    switch (
      info.utility.lift_syntax(
        ~inline=true,
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(P.to_atom(v)),
          })
        | _ => failwith(P.name ++ ": Put: not " ++ P.literal ++ " literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith(P.name ++ ": Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({info, parent, _}: View.args(model, action)) =>
    View.mk(
      Util.WebUtil.range(
        ~attrs=[Attr.on_input((_, v) => parent(SetSyntax(put(info, v))))],
        info |> get |> P.to_string,
      ),
    );
};
