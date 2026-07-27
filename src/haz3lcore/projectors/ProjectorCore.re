open Util;

/* Projector dependencies are currently somewhat convoluted.
 * This is the lowermost projectors module; Base depends on
 * this (specifically, it parameterizes the type t below over piece).
 *
 * ProjectorBase then depends on this and on Base.piece,
 * and also on Vdom, necessitating its inclusion in Core.
 * The individual projector implementations depend on ProjectorBase.
 * ProjectorInit then depends on the projector implementations.
 *
 * ProjectorInfo depends on ProjectorBase but not on ProjectorInit
 * (to avoid cyclical dependencies due to MakeTerm and ExpToSegment) */

/* Kind and Model are defined in src/language/ to allow sharing with
 * Grammar.re (which is in the language library); see ProjectorModel.re
 * for why the model types live below their implementations. */
module Kind = Language.ProjectorKind;
module Model = Language.ProjectorModel;

module Sexp = Sexplib.Sexp;

/* Projectors in syntax.
 *
 * There is deliberately no `kind` field: the model is a tagged union, so
 * `Model.kind(model)` recovers the kind and the two cannot drift. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('syntax) = {
  id: Id.t,
  syntax: 'syntax,
  model: Model.t,
};

let mk = (~id=Id.mk(), syntax, model) => {
  id,
  syntax,
  model,
};

let kind = (p: t('a)): Kind.t => Model.kind(p.model);

/* Pre-migration projectors serialized as
 *   ((id _) (kind K) (syntax _) (model "<sexp string>"))
 * where the model was an opaque sexp string and the sibling `kind` was
 * what told you how to read it. Rather than regenerate the ~240 embedded
 * models in the doc slides and discard users' persisted state, rewrite
 * that shape into the current one and let the derived decoder do the
 * rest. The presence of a `kind` field is an exact marker, since current
 * writes never emit one.
 *
 * Shared with Refractors.entry, which serialized the same pair. */
module Legacy = {
  let migrate = (sexp: Sexp.t): Sexp.t =>
    switch (sexp) {
    | Sexp.List(fields) =>
      let legacy_kind =
        List.find_map(
          fun
          | Sexp.List([Sexp.Atom("kind"), k]) => Some(Kind.t_of_sexp(k))
          | _ => None,
          fields,
        );
      switch (legacy_kind) {
      | None => sexp
      | Some(kind) =>
        Sexp.List(
          List.filter_map(
            fun
            | Sexp.List([Sexp.Atom("kind"), _]) => None
            | Sexp.List([Sexp.Atom("model"), model]) =>
              Some(
                Sexp.List([
                  Sexp.Atom("model"),
                  Model.of_legacy_sexp(kind, model) |> Model.sexp_of_t,
                ]),
              )
            | field => Some(field),
            fields,
          ),
        )
      };
    | _ => sexp
    };
};

let t_of_sexp = (syntax_of_sexp, sexp: Sexp.t) =>
  t_of_sexp(syntax_of_sexp, Legacy.migrate(sexp));

module Shape = Util.ProjectorShape;
/* Projectors currently are all convex */
let shapes = (_: t('a)): Nibs.shapes => Nib.Shape.(Convex, Convex);
