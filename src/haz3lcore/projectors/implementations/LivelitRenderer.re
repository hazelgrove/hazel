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
   and a view that takes a ViewContext is told it is not editable, and its
   room (UserLivelit.room): the line's lines and the sample's width in a
   chip on the line, Free in the drawer.

   Resolution: the livelits in the probed site's ctx, innermost binding
   first; the first whose expansion type is the site's type by name (see
   expands_to) AND whose view renders the value wins, so a nearer
   definition shadows an outer one. The others that render it are offered
   in the sample menu's "View as" list, and after them, on request only,
   every livelit whose expansion type merely fits the site's (fits_for);
   the model records the one chosen there. */

/* the livelit chosen in the "View as" list; None: the first that renders */
[@deriving (show({with_path: false}), sexp, yojson)]
type model = option(string);
[@deriving (show({with_path: false}), sexp, yojson)]
type action = unit;
/* A livelit that draws the value, and the lines it takes: where its room is
   Free (its shape's, in the drawer) and in a chip on the line, where a
   view told its room fits itself into the line's (line_rows_of) */
[@deriving (show({with_path: false}), sexp, yojson)]
type drawn = {
  name: string,
  rows: int,
  line_rows: int,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type value = {
  first: drawn,
  raw: Exp.t, /* the raw sample term: the render memo's key */
  /* a LIST of the livelit's type: each element renders through the
     livelit, in a row (Garden = [Plant] shows a row of plants) */
  [@default false]
  as_list: bool,
  /* the other livelits whose views render the value, in the same order
     (innermost first) */
  [@default []]
  alts: list(drawn),
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

/* The user-defined livelits in scope that declare their expansion type,
   innermost binding first. A shadowed livelit cannot be named (views are
   named `^name`, and drawing looks the name up), so only the innermost
   binding of a name counts. */
let in_scope = (ctx: Ctx.t): list(LivelitCtx.raw_livelit) =>
  List.fold_left(
    ((seen, acc), e: Ctx.entry) =>
      switch (e) {
      | LivelitEntry(ll) when !List.mem(ll.name, seen) => (
          [ll.name, ...seen],
          switch (ll.user_def) {
          | Some(_) when !is_unknown(ll.expansion_t) => acc @ [ll]
          | _ => acc
          },
        )
      | _ => (seen, acc)
      },
    ([], []),
    ctx.entries,
  )
  |> snd;

let candidates_for = (ctx: Ctx.t, ty: Typ.t): list(LivelitCtx.raw_livelit) =>
  is_unknown(ty)
    ? []
    : List.filter(
        (ll: LivelitCtx.raw_livelit) => expands_to(ctx, ll.expansion_t, ty),
        in_scope(ctx),
      );

/* The livelits "View as" offers on request only, which an automatic pick
   never shows: those whose expansion type fits the site's once aliases
   are unfolded, but not by name (a plain [Int] can be shown as a
   `Trace = [Int]`). Every type fits an unknown one, so at a site of
   unknown type these are all of them; the list keeps only the ones whose
   view draws the value (on_request). */
let fits_for = (ctx: Ctx.t, ty: Typ.t): list(LivelitCtx.raw_livelit) =>
  List.filter(
    (ll: LivelitCtx.raw_livelit) =>
      Typ.is_consistent(ctx, ll.expansion_t, ty)
      && (is_unknown(ty) || !expands_to(ctx, ll.expansion_t, ty)),
    in_scope(ctx),
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

/* (sample, livelit definition, room) -> rendered html. Keyed on the RAW
   sample term (a stable object across renders); parse and render both go
   through here, so a value is admitted only if its view really renders
   (in Free room, the one parse checks). The definition is in the key, so
   an edit to the livelit redraws its samples, and so is the room of a view
   told its room (a one-argument view draws the same in every room). A
   livelit use's own stream mixes its HTML view samples with its values —
   HTML is never wrapped, and a view that comes back stuck (a wrap on the
   wrong shape) is rejected. */
type html_key = {
  raw: Exp.t,
  def: Exp.t,
  name: string,
  room: UserLivelit.room,
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
      ~room: UserLivelit.room=UserLivelit.Free,
      ll: LivelitCtx.raw_livelit,
      raw: Exp.t,
    )
    : option(Exp.t) =>
  switch (ll.user_def) {
  | None => None
  | Some(def_elab) =>
    let room: UserLivelit.room = ll.view_takes_ctx ? room : UserLivelit.Free;
    switch (
      List.find_opt(
        ((k, _)) =>
          k.name == ll.name
          && k.room == room
          && same(k.raw, raw)
          && same(k.def, def_elab),
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
                      ~editable=false,
                      ~room,
                      m,
                    ),
                  ),
                )
              ) {
              | Ok(html) when MvuShape.is_html(html) => Some(html)
              /* a stuck view (an app livelit's view reads the program,
                 which the closed evaluation cannot see): not this
                 livelit's value to render */
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
              room,
            },
            h,
          ),
          ...ListUtil.take(95, html_memo^),
        ];
      h;
    };
  };

let rows_of = (ll: LivelitCtx.raw_livelit): int =>
  switch (ll.shape.vertical) {
  | Inline => 1
  | Tab(n)
  | Block(n) => n + 1
  };

/* On the line a view told its room fits itself into the line's lines, so
   it is drawn there whatever its shape; a one-argument view is as tall as
   its shape, and waits for the drawer if that is taller than the line's
   room. */
let line_rows_of = (ll: LivelitCtx.raw_livelit): int =>
  ll.view_takes_ctx
    ? min(rows_of(ll), RichProbe.lines_on_line) : rows_of(ll);

let drawn = (ll: LivelitCtx.raw_livelit): drawn => {
  name: ll.name,
  rows: rows_of(ll),
  line_rows: line_rows_of(ll),
};

/* the elements of a list value (closed), if it is one */
let list_elems = (exp: Exp.t): option(list(Exp.t)) =>
  switch (Exp.term_of(MvuShape.close_value(exp))) {
  | ListLit(items) when items != [] => Some(items)
  | _ => None
  };

let parse = (~statics, sort: Sort.t, exp: Exp.t): option(value) =>
  switch (sort, site(statics)) {
  | (Sort.Exp | Sort.Pat, Some((ctx, ty))) =>
    let found = (~as_list, lls: list(LivelitCtx.raw_livelit)) =>
      switch (lls) {
      | [ll, ...others] =>
        Some({
          first: drawn(ll),
          raw: exp,
          as_list,
          alts: List.map(drawn, others),
        })
      | [] => None
      };
    let direct =
      candidates(statics)
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
        candidates_for(ctx, elem)
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

/* The livelit a model draws a value with: the chosen one while it still
   renders the value, else the first */
let chosen = (m: model, v: value): drawn =>
  switch (m) {
  | Some(name) =>
    switch (List.find_opt((d: drawn) => d.name == name, v.alts)) {
    | Some(d) => d
    | None => v.first
    }
  | None => v.first
  };

let drawer_rows = (m: model, v: value): int => chosen(m, v).rows;
let line_rows = (m: model, v: value): int => chosen(m, v).line_rows;

let views = (v: value): list(model) =>
  List.map((d: drawn) => Some(d.name), [v.first, ...v.alts]);

/* A livelit offered on request that draws the value, by name */
let requested =
    (~statics, sort: Sort.t, exp: Exp.t, name: string)
    : option(LivelitCtx.raw_livelit) =>
  switch (sort, site(statics)) {
  | (Sort.Exp | Sort.Pat, Some((ctx, ty))) =>
    List.find_opt(
      (ll: LivelitCtx.raw_livelit) =>
        ll.name == name && html_of(~ctx, ll, exp) != None,
      fits_for(ctx, ty),
    )
  | _ => None
  };

/* The value as the livelit a model names draws it: as `parse` finds it,
   or a livelit offered only on request, while its view draws the value
   (otherwise, like a chosen livelit that no longer draws it, the first) */
let parse_chosen = (~statics, sort: Sort.t, exp: Exp.t, m: model) => {
  let found = parse(~statics, sort, exp);
  switch (m, found) {
  | (Some(_), Some(v)) when List.mem(m, views(v)) => found
  | (Some(name), _) =>
    switch (requested(~statics, sort, exp, name)) {
    | Some(ll) =>
      Some({
        first: drawn(ll),
        raw: exp,
        as_list: false,
        alts: [],
      })
    | None => found
    }
  | (None, _) => found
  };
};

let on_request = (~statics, sort: Sort.t, exp: Exp.t): list(model) =>
  switch (sort, site(statics)) {
  | (Sort.Exp | Sort.Pat, Some((ctx, ty))) =>
    let drawn =
      switch (parse(~statics, sort, exp)) {
      | Some(v) => views(v)
      | None => []
      };
    List.filter_map(
      (ll: LivelitCtx.raw_livelit) =>
        !List.mem(Some(ll.name), drawn) && html_of(~ctx, ll, exp) != None
          ? Some(Some(ll.name)) : None,
      fits_for(ctx, ty),
    );
  | _ => []
  };

/* named as written, in the code font */
let label = (m: model, v: value): RichProbe.view_label => {
  name: "^" ++ chosen(m, v).name,
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
      ~room: RichProbe.room,
      _: unit,
    )
    : Node.t => {
  let ll_name = chosen(model, value).name;
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
        Option.bind(
          list_elems(value.raw),
          items => {
            /* the row's elements share the sample's width */
            let room: RichProbe.room =
              switch (room) {
              | UserLivelit.Lines(lines, cols) =>
                UserLivelit.Lines(lines, max(1, cols / List.length(items)))
              | UserLivelit.Free => UserLivelit.Free
              };
            List.fold_right(
              (it, acc) =>
                switch (acc, html_of(~ctx, ~room, ll, it)) {
                | (Some(hs), Some(h)) => Some([h, ...hs])
                | _ => None
                },
              items,
              Some([]),
            );
          },
        )
      | Some(ll) =>
        Option.map(h => [h], html_of(~ctx, ~room, ll, value.raw))
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
