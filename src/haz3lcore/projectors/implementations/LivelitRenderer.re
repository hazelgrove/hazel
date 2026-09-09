open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* LivelitRenderer — RichProbe renderer that shows a sampled value through
   the VIEW of a user-defined livelit whose expansion type is the value's
   type. Type-directed: `let ^curve = {...}` with `expand : Model -> Curve`
   makes every probed `Curve` render as the curve widget, no syntax at the
   use site. The livelit rebuilds a display model from the value through
   its `wrap : Expansion -> Model` member (or, when Model IS the expansion
   type, the value itself). Inert: handlers in the view dispatch nothing.

   Resolution: the livelits in the probed site's ctx, innermost binding
   first; the first whose expansion type equals the site's type (up to
   aliases) AND whose view renders the value wins, so a nearer definition
   shadows an outer one. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type action = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = {
  ll_name: string,
  rows: int, /* the livelit's own block height, for the drawer */
  raw: Exp.t /* the raw sample term: the render memo's key */
};

let update = (m: model, _: action) => m;
let empty = ();
let init = (_: value) => ();

let is_unknown = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Unknown(_) => true
  | _ => false
  };

let member = (record: Exp.t, label: string): option(Exp.t) =>
  switch (MvuShape.of_tuple(MvuShape.strip_wrappers(record))) {
  | Some(fs) =>
    List.find_map(
      f =>
        switch (MvuShape.of_field(f)) {
        | Some((l, v)) when l == label => Some(v)
        | _ => None
        },
      fs,
    )
  | None => None
  };

/* the definition record evaluates to closures over the builtin env; one
   evaluation per definition, keyed by its elaboration's identity */
let record_memo: ref(list((Exp.t, Exp.t))) = ref([]);
let record_of = (def_elab: Exp.t): option(Exp.t) =>
  switch (List.find_opt(((k, _)) => k === def_elab, record_memo^)) {
  | Some((_, r)) => Some(r)
  | None =>
    switch (MvuShape.safe_evaluate(def_elab)) {
    | Ok(r) =>
      record_memo := [(def_elab, r), ...ListUtil.take(15, record_memo^)];
      Some(r);
    | Error(_) => None
    }
  };

/* the probed site's (ctx, type): an expression's, or a pattern's — a
   parameter well is a Pat probe whose samples are the bound values */
let site = (statics: option(Info.t)): option((Ctx.t, Typ.t)) =>
  switch (statics) {
  | Some(Info.InfoExp({ctx, ty, _}))
  | Some(Info.InfoPat({ctx, ty, _})) => Some((ctx, ty))
  | _ => None
  };

/* livelits in scope whose expansion type is the site's type */
let rec strip = (t: Typ.t): Typ.t =>
  switch (Typ.term_of(t)) {
  | Parens(inner) => strip(inner)
  | _ => t
  };

let candidates = (statics: option(Info.t)): list(LivelitCtx.raw_livelit) =>
  switch (site(statics)) {
  | Some((ctx, ty)) when !is_unknown(ty) =>
    List.filter_map(
      (e: Ctx.entry) =>
        switch (e) {
        /* NOMINAL: the livelit renders the type it expands to BY NAME
           (`expand : Model -> Point` takes sites typed `Point`, not
           every (Int, Int)); a structural site only matches a livelit
           that expands to that structure. Aliases are not unfolded. */
        | LivelitEntry({user_def: Some(_), expansion_t, _} as ll)
            when
              !is_unknown(expansion_t)
              && Typ.fast_equal(strip(expansion_t), strip(ty)) =>
          Some(ll)
        | _ => None
        },
      ctx.entries,
    )
  | _ => []
  };

/* the display model for a value: `wrap(value)`, or the value itself when
   the livelit's Model IS its expansion type */
let model_of =
    (~ctx: Ctx.t, ll: LivelitCtx.raw_livelit, record: Exp.t, v: Exp.t)
    : option(Exp.t) =>
  switch (member(record, "wrap")) {
  | Some(wrap) =>
    switch (
      MvuShape.safe_evaluate(IdTagged.FreshGrammar.Exp.ap(Forward, wrap, v))
    ) {
    | Ok(m) => Some(m)
    | Error(_) => None
    }
  | None =>
    Typ.equal_up_to_aliases(ctx, ll.model_t, ll.expansion_t) ? Some(v) : None
  };

/* (sample identity, livelit) -> rendered html. Keyed on the RAW sample
   term (a stable object across renders); parse and render both go
   through here, so a value is admitted only if its view really renders.
   A livelit use's own stream mixes its HTML view samples with its
   values — HTML is never wrapped, and a view that comes back stuck (a
   wrap on the wrong shape) is rejected. */
let html_memo: ref(list(((Exp.t, string), option(Exp.t)))) = ref([]);
let html_of = (~ctx, ll: LivelitCtx.raw_livelit, raw: Exp.t): option(Exp.t) =>
  switch (
    List.find_opt(
      ((k, _)) => fst(k) === raw && snd(k) == ll.name,
      html_memo^,
    )
  ) {
  | Some((_, h)) => h
  | None =>
    let v = MvuShape.close_value(raw);
    let h =
      if (MvuShape.is_html(v)) {
        None;
      } else {
        switch (ll.user_def) {
        | None => None
        | Some(def_elab) =>
          switch (record_of(def_elab)) {
          | None => None
          | Some(record) =>
            switch (member(record, "view"), model_of(~ctx, ll, record, v)) {
            | (None, _) => None
            | (_, None) =>
              print_endline(
                "LivelitRenderer: ^" ++ ll.name ++ " wrap failed",
              );
              None;
            | (Some(view), Some(m)) =>
              switch (
                MvuShape.safe_evaluate(
                  IdTagged.FreshGrammar.Exp.ap(Forward, view, m),
                )
              ) {
              | Ok(html) when MvuShape.is_html(html) => Some(html)
              | Ok(other) =>
                print_endline(
                  "LivelitRenderer: ^"
                  ++ ll.name
                  ++ ".view gave non-HTML: "
                  ++ String.sub(
                       Exp.show(other),
                       0,
                       min(300, String.length(Exp.show(other))),
                     ),
                );
                None;
              | Error(e) =>
                print_endline(
                  "LivelitRenderer: ^" ++ ll.name ++ ".view failed: " ++ e,
                );
                None;
              }
            }
          }
        };
      };
    html_memo := [((raw, ll.name), h), ...ListUtil.take(63, html_memo^)];
    h;
  };

let rows_of = (ll: LivelitCtx.raw_livelit): int =>
  switch (ll.shape.vertical) {
  | Inline => 1
  | Tab(n)
  | Block(n) => n + 1
  };

let parse = (~statics, sort: Sort.t, exp: Exp.t): option(value) =>
  switch (sort, site(statics)) {
  | (Sort.Exp | Sort.Pat, Some((ctx, _))) =>
    List.find_map(
      (ll: LivelitCtx.raw_livelit) =>
        switch (html_of(~ctx, ll, exp)) {
        | Some(_) =>
          Some({
            ll_name: ll.name,
            rows: rows_of(ll),
            raw: exp,
          })
        | None => None
        },
      candidates(statics),
    )
  | _ => None
  };

let drawer_rows = (v: value): int => v.rows;

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model as _: model,
      ~local as _: action => Ui_effect.t(unit),
      ~parent as _: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      _: unit,
    )
    : Node.t => {
  let view_term = term =>
    Exp(term) |> info.utility.term_to_seg(~inline=true) |> view_seg(Exp);
  let seed: HazelDOM.t = {
    inject: (_gesture, _msg) => Ui_effect.Ignore,
    view_term,
    commit: HazelDOM.Syntax,
  };
  let html =
    switch (site(info.statics)) {
    | Some((ctx, _)) =>
      switch (Ctx.lookup_livelit(ctx, value.ll_name)) {
      | Some(ll) => html_of(~ctx, ll, value.raw)
      | None => None
      }
    | _ => None
    };
  switch (html) {
  | Some(html) =>
    Node.div(
      ~attrs=[
        Attr.classes(["rich-html-view", "rich-livelit-view"]),
        Attr.title("^" ++ value.ll_name ++ " view of this value"),
      ],
      [HazelDOM.go(seed, html)],
    )
  | None =>
    Node.div(
      ~attrs=[Attr.classes(["rich-livelit-view", "rich-livelit-failed"])],
      [Node.text("^" ++ value.ll_name ++ ": view failed")],
    )
  };
};

let badge =
  Node.span(
    ~attrs=[
      Attr.classes(["html-badge", "livelit-badge"]),
      Attr.title("View through the livelit defined for this type"),
    ],
    [Node.text("^")],
  );
