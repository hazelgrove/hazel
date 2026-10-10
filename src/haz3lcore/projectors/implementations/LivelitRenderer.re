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
   type, the value itself). Inert: handlers in the view dispatch nothing,
   and a view that takes a ViewContext is told it is not editable and
   whether it draws offside or in the drawer.

   Resolution: the livelits in the probed site's ctx, innermost binding
   first; the first whose expansion type equals the site's type (up to
   aliases) AND whose view renders the value wins, so a nearer definition
   shadows an outer one. The others that render it are offered in the
   sample menu's "View as" list; the model records the one chosen there. */

/* the livelit chosen in the "View as" list; None: the first that renders */
[@deriving (show({with_path: false}), sexp, yojson)]
type model = option(string);
[@deriving (show({with_path: false}), sexp, yojson)]
type action = unit;
[@deriving (show({with_path: false}), sexp, yojson)]
type value = {
  ll_name: string,
  rows: int, /* the livelit's own block height, for the drawer */
  raw: Exp.t, /* the raw sample term: the render memo's key */
  /* a LIST of the livelit's type: each element renders through the
     livelit, in a row (Garden = [Plant] shows a row of plants) */
  [@default false]
  as_list: bool,
  /* the other livelits whose views render the value, in the same order
     (innermost first), with their rows */
  [@default []]
  alts: list((string, int)),
};

let update = (m: model, _: action) => m;
let empty = None;
let init = (_: value) => None;

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

/* Does a type mention something NAMED — an alias or a constructor? A
   livelit that expands to `Plan = (Mode, Temp)` may also take a site
   typed structurally `(Mode, Temp)` (a returned tuple literal
   synthesizes its structure, not the annotation's name): the names
   inside make the shape specific. A bare `(Int, Int)` is not a Point
   until the program says so. */
let rec names_something = (ty: Typ.t): bool =>
  switch (Typ.term_of(ty)) {
  | Var(_)
  | Sum(_) => true
  | Parens(t)
  | List(t) => names_something(t)
  | Prod(ts) => List.exists(names_something, ts)
  | Arrow(a, b) => names_something(a) || names_something(b)
  | _ => false
  };

/* the livelit's expansion type takes the site's type: by NAME, or by
   the alias's body when that body is itself specific */
let expands_to = (ctx: Ctx.t, expansion_t: Typ.t, ty: Typ.t): bool =>
  Typ.fast_equal(strip(expansion_t), strip(ty))
  || (
    switch (Typ.term_of(strip(expansion_t)), Typ.term_of(strip(ty))) {
    | (Var(_), Var(_)) => false /* two different names */
    | (Var(_), _) =>
      let body = Typ.weak_head_normalize(ctx, expansion_t);
      names_something(body) && Typ.fast_equal(strip(body), strip(ty));
    | _ => false
    }
  );

let candidates_for = (ctx: Ctx.t, ty: Typ.t): list(LivelitCtx.raw_livelit) =>
  is_unknown(ty)
    ? []
    : List.filter_map(
        (e: Ctx.entry) =>
          switch (e) {
          | LivelitEntry({user_def: Some(_), expansion_t, _} as ll)
              when
                !is_unknown(expansion_t) && expands_to(ctx, expansion_t, ty) =>
            Some(ll)
          | _ => None
          },
        ctx.entries,
      );

let candidates = (statics: option(Info.t)): list(LivelitCtx.raw_livelit) =>
  switch (site(statics)) {
  | Some((ctx, ty)) => candidates_for(ctx, ty)
  | None => []
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

/* (sample, livelit definition, place) -> rendered html. Keyed on the RAW
   sample term (a stable object across renders); parse and render both go
   through here, so a value is admitted only if its view really renders
   (offside, the place parse checks). The definition is in the key, so an
   edit to the livelit redraws its samples. A livelit use's own stream
   mixes its HTML view samples with its values — HTML is never wrapped,
   and a view that comes back stuck (a wrap on the wrong shape) is
   rejected. */
type html_key = {
  raw: Exp.t,
  def: Exp.t,
  name: string,
  place: UserLivelit.place,
};
let html_memo: ref(list((html_key, option(Exp.t)))) = ref([]);
/* by identity, else by value: a re-evaluation hands out fresh sample
   objects for unchanged values (and a fresh elaboration of an unchanged
   definition), and re-running the view for each of them on every
   keystroke is the cost */
let same = (a: Exp.t, b: Exp.t): bool => a === b || Exp.fast_equal(a, b);
let html_of =
    (
      ~ctx,
      ~place: UserLivelit.place=Offside,
      ll: LivelitCtx.raw_livelit,
      raw: Exp.t,
    )
    : option(Exp.t) =>
  switch (ll.user_def) {
  | None => None
  | Some(def_elab) =>
    switch (
      List.find_opt(
        ((k, _)) =>
          k.name == ll.name
          && k.place == place
          && same(k.raw, raw)
          && same(k.def, def_elab),
        html_memo^,
      )
    ) {
    | Some((_, h)) => h
    | None =>
      let v = MvuShape.close_value(raw);
      let h =
        /* A sample with a hole stays text: its view would come back
           stuck, and substituting out a stuck result can blow up
           exponentially (Piano Roll's folds over a hole hung the page) */
        if (MvuShape.is_html(v) || !MvuShape.is_settled(v)) {
          None;
        } else {
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
                  IdTagged.FreshGrammar.Exp.ap(
                    Forward,
                    view,
                    UserLivelit.view_arg(
                      ~takes_ctx=ll.view_takes_ctx,
                      ~place,
                      m,
                    ),
                  ),
                )
              ) {
              | Ok(html)
                  when MvuShape.is_html(html) && MvuShape.is_settled(html) =>
                Some(html)
              /* a stuck view (an app livelit's view reads the program,
                 which the closed evaluation cannot see; a sample with a
                 hole in it): not this livelit's value to render, so the
                 sample falls back to text */
              | Ok(_) => None
              | Error(e) =>
                print_endline(
                  "LivelitRenderer: ^" ++ ll.name ++ ".view failed: " ++ e,
                );
                None;
              }
            }
          };
        };
      html_memo :=
        [
          (
            {
              raw,
              def: def_elab,
              name: ll.name,
              place,
            },
            h,
          ),
          ...ListUtil.take(95, html_memo^),
        ];
      h;
    }
  };

let rows_of = (ll: LivelitCtx.raw_livelit): int =>
  switch (ll.shape.vertical) {
  | Inline => 1
  | Tab(n)
  | Block(n) => n + 1
  };

/* the elements of a list value (closed), if it is one */
let list_elems = (exp: Exp.t): option(list(Exp.t)) =>
  switch (Exp.term_of(MvuShape.close_value(exp))) {
  | ListLit(items) when items != [] => Some(items)
  | _ => None
  };

/* a shadowed livelit cannot be named, so only the innermost of a name */
let innermost =
    (lls: list(LivelitCtx.raw_livelit)): list(LivelitCtx.raw_livelit) =>
  List.fold_left(
    (acc, ll: LivelitCtx.raw_livelit) =>
      List.exists((l: LivelitCtx.raw_livelit) => l.name == ll.name, acc)
        ? acc : acc @ [ll],
    [],
    lls,
  );

let parse = (~statics, sort: Sort.t, exp: Exp.t): option(value) =>
  switch (sort, site(statics)) {
  | (Sort.Exp | Sort.Pat, Some((ctx, ty))) =>
    let found = (~as_list, lls: list(LivelitCtx.raw_livelit)) =>
      switch (lls) {
      | [ll, ...others] =>
        Some({
          ll_name: ll.name,
          rows: rows_of(ll),
          raw: exp,
          as_list,
          alts:
            List.map(
              (ll: LivelitCtx.raw_livelit) => (ll.name, rows_of(ll)),
              others,
            ),
        })
      | [] => None
      };
    let direct =
      innermost(candidates(statics))
      |> List.filter(ll => html_of(~ctx, ll, exp) != None);
    switch (direct) {
    | [_, ..._] => found(~as_list=false, direct)
    | [] =>
      /* a list of a type with a view: every element must render */
      switch (
        Typ.term_of(Typ.weak_head_normalize(ctx, ty)),
        list_elems(exp),
      ) {
      | (List(elem), Some(items)) =>
        innermost(candidates_for(ctx, elem))
        |> List.filter(ll =>
             List.for_all(it => html_of(~ctx, ll, it) != None, items)
           )
        |> found(~as_list=true)
      | _ => None
      }
    };
  | _ => None
  };

/* A parse already means a user-defined livelit views this exact type (and
   for a list, that every element renders — `list_elems` rejects the empty
   one), so a match is always real evidence for an automatic pick. */
let auto_applies = (_: value): bool => true;

/* The livelit a model draws a value with, and its rows: the chosen one
   while it still renders the value, else the first */
let chosen = (m: model, v: value): (string, int) =>
  switch (m) {
  | Some(name) when name != v.ll_name =>
    switch (List.assoc_opt(name, v.alts)) {
    | Some(rows) => (name, rows)
    | None => (v.ll_name, v.rows)
    }
  | _ => (v.ll_name, v.rows)
  };

let drawer_rows = (m: model, v: value): int => snd(chosen(m, v));

let views = (v: value): list(model) => [
  Some(v.ll_name),
  ...List.map(((name, _)) => Some(name), v.alts),
];

/* named as written, in the code font */
let label = (m: model, v: value): RichProbe.view_label => {
  name: "^" ++ fst(chosen(m, v)),
  code: true,
};

let render =
    (
      ~info: info,
      ~exp as _: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local as _: action => Ui_effect.t(unit),
      ~parent as _: external_action => Ui_effect.t(unit),
      ~sort as _: Sort.t,
      ~place: RichProbe.place,
      _: unit,
    )
    : Node.t => {
  let (ll_name, _) = chosen(model, value);
  let view_term = term =>
    Exp(term) |> info.utility.term_to_seg(~inline=true) |> view_seg(Exp);
  let seed: HazelDOM.t = {
    inject: (_gesture, _msg) => Ui_effect.Ignore,
    view_term,
    commit: HazelDOM.Syntax,
  };
  let htmls: option(list(Exp.t)) =
    switch (site(info.statics)) {
    | Some((ctx, _)) =>
      switch (Ctx.lookup_livelit(ctx, ll_name)) {
      | Some(ll) when value.as_list =>
        Option.bind(list_elems(value.raw), items =>
          List.fold_right(
            (it, acc) =>
              switch (acc, html_of(~ctx, ~place, ll, it)) {
              | (Some(hs), Some(h)) => Some([h, ...hs])
              | _ => None
              },
            items,
            Some([]),
          )
        )
      | Some(ll) =>
        Option.map(h => [h], html_of(~ctx, ~place, ll, value.raw))
      | None => None
      }
    | _ => None
    };
  switch (htmls) {
  | Some(htmls) =>
    Node.div(
      ~attrs=[
        Attr.classes(
          ["rich-html-view", "rich-livelit-view"]
          @ (value.as_list ? ["rich-livelit-list"] : []),
        ),
        Attr.title("^" ++ ll_name ++ " view of this value"),
      ],
      List.map(HazelDOM.go(seed), htmls),
    )
  | None =>
    Node.div(
      ~attrs=[Attr.classes(["rich-livelit-view", "rich-livelit-failed"])],
      [Node.text("^" ++ ll_name ++ ": view failed")],
    )
  };
};

let badge =
  Node.span(~attrs=[Attr.classes(["livelit-badge"])], [Node.text("^")]);
