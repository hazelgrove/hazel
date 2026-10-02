open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

/* Bumped on every optimistic-table change. ProjectorView's ViewCache keys
   livelit entries on this (like AppBridge.version for apps): the underlying
   syntax only reaches the cache key via statics_map identity, which the
   statics debounce holds stale on the commit frame — without this bump the
   event-time optimistic view would sit invisible until the deferred statics
   refresh. */
let optimistic_version: ref(int) = ref(0);

/* Commit-vs-ephemeral decision for an event-time update result: a model
   persists to the program text only when it is checkpointable (carries no
   captured environment); otherwise it lives solely in the optimistic table
   and the syntax commit is skipped (see opt_ephemeral below). */
let commit_decision =
    (new_model: TermBase.Exp.t)
    : [
        | `Commit
        | `Ephemeral
      ] =>
  MvuShape.is_checkpointable(new_model) ? `Commit : `Ephemeral;

/* --- Splices in a livelit's model ---------------------------------
 *
 * A splice is a region of the CLIENT's program held inside the widget:
 * edited in place, and typed in the surrounding scope, since splices
 * are transparent to statics. new_splice is the only command that makes
 * one (Sec. 3.2.1); the program text is where it lives. The commit
 * (SpliceStore.write_model) writes each ref in a committed model as its
 * splice, in PARENS, at the ref's position:
 *
 *     ^color((r = (0), g = (0), b = (0), a = (100)))
 *     ^cells((orient = Row, refs = [(?), (3)]))
 *
 * The parens are the durable form, and they have to be, because a splice
 * piece prints as nothing but its content and a slide loads from text --
 * so a splice cannot round-trip by itself, and `init` below rebuilds each
 * one from its parens on every load. The client's code stays INSIDE the
 * parens, which is what makes the rewrap idempotent.
 *
 * Eligible: a labeled field whose value is exactly one parenthesized
 * expression, or a list literal whose elements are. Which of those are
 * refs is decided later, by the Model type (UserLivelit.expose_splice_refs);
 * a splice anywhere else is simply the client's code in that place. */

/* Split a segment into (leading secondary, core, trailing secondary). */
let split_outer_secondary =
    (seg: Base.segment): (Base.segment, Base.segment, Base.segment) => {
  let (lead, rest) = Segment.take_while_secondary(seg);
  let (rev_trail, rev_core) = Segment.take_while_secondary(List.rev(rest));
  (lead, List.rev(rev_core), List.rev(rev_trail));
};

/* Split a field at its tuple-label separator, returning the prefix
 * through the "=" tile and the value pieces after it. */
let split_at_label_sep =
    (field: Base.segment): option((Base.segment, Base.segment)) => {
  let rec go = (prefix, ps: Base.segment) =>
    switch (ps) {
    | [] => None
    | [Base.Tile({label: ["="], _}) as eq, ...rest] =>
      Some((List.rev([eq, ...prefix]), rest))
    | [p, ...rest] => go([p, ...prefix], rest)
    };
  go([], field);
};

let map_comma_groups =
    (f: Base.segment => Base.segment, seg: Base.segment): Base.segment =>
  Segment.split_at_commas(seg)
  |> Aba.map_a(f)
  |> Aba.join(Fun.id, p => [p])
  |> List.concat;

/* A parenthesized expression: Convex on both sides. The application's
 * own argument tile carries the same ["(", ")"] label but a postfix
 * mold, so the nibs are what tell them apart. */
let as_parens = (p: Base.piece): option((Base.tile, Base.segment)) =>
  switch (p) {
  | Tile(
      {
        label: ["(", ")"],
        mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
        children: [inner],
        _,
      } as t,
    ) =>
    Some((t, inner))
  | _ => None
  };

/* `(<code>)` -> `(<splice code>)`. None when [seg] is not exactly one
 * parenthesized expression, or already holds a splice. */
let wrap_parens = (seg: Base.segment): option(Base.segment) => {
  open OptUtil.Syntax;
  let (lead, core, trail) = split_outer_secondary(seg);
  let* (t, inner) =
    switch (core) {
    | [p] => as_parens(p)
    | _ => None
    };
  let (ilead, icore, itrail) = split_outer_secondary(inner);
  /* `(code : T)`: a splice with its declared type. Only the code is
     spliced; the ascription stays outside it, so the splice editor shows
     the client's code alone. Only when the ascription is the ROOT of the
     parens' content: it binds tighter than `*`, so `x * 10 : Int` is
     `x * (10 : Int)`, and splitting at its `:` would change the term. It
     is the root when no top-level operator before it binds more loosely
     (a higher Precedence number) than it does. Compound typed code is
     written `((x * 10) : Int)` (SpliceStore.mk_typed_splice). */
  let looser_than_asc = (p: Base.piece) =>
    switch (p) {
    | Tile({mold: {nibs: (l, r), _}, _}) =>
      List.exists(
        (n: Nib.t) =>
          switch (n.shape) {
          | Concave(prec) => prec > Precedence.asc
          | Convex => false
          },
        [l, r],
      )
    | _ => false
    };
  let rec last_colon = (i, found, ps: Base.segment) =>
    switch (ps) {
    | [] => found
    | [Base.Tile({label: [":"], _}), ...rest] =>
      last_colon(i + 1, Some(i), rest)
    | [_, ...rest] => last_colon(i + 1, found, rest)
    };
  let (code, typed) =
    switch (last_colon(0, None, icore)) {
    | Some(k)
        when
          k > 0
          && !List.exists(looser_than_asc, fst(ListUtil.split_n(k, icore))) =>
      let (code, typed) = ListUtil.split_n(k, icore);
      let (clead, ccore, ctrail) = split_outer_secondary(code);
      (clead @ ccore, ctrail @ typed);
    | _ => (icore, [])
    };
  let (clead, ccore, _) = split_outer_secondary(code);
  switch (ccore) {
  | [Base.Splice(_)] => None /* idempotent: already spliced */
  | _ =>
    let inner' = ilead @ clead @ [Piece.mk_splice(ccore)] @ typed @ itrail;
    Some(
      lead
      @ [
        Base.Tile({
          ...t,
          children: [inner'],
        }),
      ]
      @ trail,
    );
  };
};

let as_list_lit = (p: Base.piece): option((Base.tile, Base.segment)) =>
  switch (p) {
  | Tile({label: ["[", "]"], children: [inner], _} as t) =>
    Some((t, inner))
  | _ => None
  };

/* `lo=(0)` -> `lo=(<splice 0>)`, and `refs=[(1), (2)]` -> each element
 * spliced. Returns None when the field is not marked, or is already
 * spliced. */
let wrap_marked_field = (field: Base.segment): option(Base.segment) => {
  open OptUtil.Syntax;
  let* (label_prefix, value) = split_at_label_sep(field);
  switch (wrap_parens(value)) {
  | Some(value') => Some(label_prefix @ value')
  | None =>
    let (lead, core, trail) = split_outer_secondary(value);
    let* (t, inner) =
      switch (core) {
      | [p] => as_list_lit(p)
      | _ => None
      };
    let wrapped = ref(false);
    let inner' =
      map_comma_groups(
        el =>
          switch (wrap_parens(el)) {
          | Some(el') =>
            wrapped := true;
            el';
          | None => el
          },
        inner,
      );
    wrapped^
      ? Some(
          label_prefix
          @ lead
          @ [
            Base.Tile({
              ...t,
              children: [inner'],
            }),
          ]
          @ trail,
        )
      : None;
  };
};

/* `Live((0))` -> `Live((<splice 0>))`: a constructor's parenthesized
 * arguments, each spliced as a marked field's value is, for a Model like
 * `+ Frozen(Int, SpliceRef) + Live(SpliceRef)`. None when none is. */
let wrap_ctor_args = (arg: Base.segment): option(Base.segment) => {
  let (lead, core, trail) = split_outer_secondary(arg);
  let is_ctor = (c: string) =>
    String.length(c) > 0
    && Char.uppercase_ascii(c.[0]) == c.[0]
    && Char.lowercase_ascii(c.[0]) != c.[0];
  switch (core) {
  | [
      Base.Tile({label: [c], _}) as ctor,
      Base.Tile({label: ["(", ")"], children: [inner], _} as ap),
    ]
      when is_ctor(c) =>
    let wrapped = ref(false);
    let inner' =
      map_comma_groups(
        el =>
          switch (wrap_parens(el)) {
          | Some(el') =>
            wrapped := true;
            el';
          | None => el
          },
        inner,
      );
    wrapped^
      ? Some(
          lead
          @ [
            ctor,
            Base.Tile({
              ...ap,
              children: [inner'],
            }),
          ]
          @ trail,
        )
      : None;
  | _ => None
  };
};

/* Rewrite the model tuple's marked fields. [seg] is the whole
 * invocation: the `^name` tile followed by the application's
 * argument tile. Returns None when nothing was marked, so an
 * unmarked livelit installs no syntax override at all. */
let splice_marked_fields = (seg: Base.segment): option(Base.segment) => {
  let wrapped = ref(0);
  let wrap = (field: Base.segment) =>
    switch (wrap_marked_field(field)) {
    | Some(field') =>
      incr(wrapped);
      field';
    | None => field
    };
  /* `^name((a=1, b=(2)))` puts the tuple's own parens inside the
   * application's, so descend one layer when there is one. */
  let rewrite_arg = (arg: Base.segment): Base.segment =>
    switch (wrap_ctor_args(arg)) {
    | Some(arg') =>
      incr(wrapped);
      arg';
    | None =>
      switch (arg) {
      | [p] =>
        switch (as_parens(p)) {
        | Some((t, inner)) => [
            Base.Tile({
              ...t,
              children: [map_comma_groups(wrap, inner)],
            }),
          ]
        | None => map_comma_groups(wrap, arg)
        }
      | _ => map_comma_groups(wrap, arg)
      }
    };
  let seg' =
    List.map(
      (p: Base.piece) =>
        switch (p) {
        | Tile({label: ["(", ")"], children: [arg], _} as t)
            when Option.is_none(as_parens(p)) =>
          Base.Tile({
            ...t,
            children: [rewrite_arg(arg)],
          })
        | _ => p
        },
      seg,
    );
  wrapped^ > 0 ? Some(seg') : None;
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* The statics at the projector's id describe the Projector node itself,
     and a slide's ^^livelit(...) invoke adds a Parens layer — look through
     both to find the application. */
  let rec strip_wrappers = (term: TermBase.Exp.term): TermBase.Exp.term =>
    switch (term) {
    | Parens({term, _})
    /* A use showing its syntax holds it as one splice (show_syntax). */
    | Splice({term, _})
    | Projector(_, {term, _}) => strip_wrappers(term)
    | term => term
    };

  /* A use's livelit name, its parameters if it is a direct use with them
     (`^a(args)(model)`), and its model. */
  let get_use = (info: info) =>
    switch (info.statics) {
    | Some(InfoExp({user_term, _})) =>
      switch (strip_wrappers(user_term.term)) {
      | Ap(_dir, {term: LivelitName(llname), _}, model) =>
        Some((llname, None, model))
      | Ap(_dir, fn, model) =>
        switch (strip_wrappers(fn.term)) {
        | Ap(_, {term: LivelitName(llname), _}, args) =>
          Some((llname, Some(args), model))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    };

  /* The livelit a use is of: the one its name finds, applied to the use's
     parameters when it gives them. The arguments are the use's own terms;
     they are closed, as statics requires, so they mean here what they
     mean there. */
  let lookup_use = (ctx: Ctx.t, llname: string, args) =>
    switch (Ctx.lookup_livelit(ctx, llname), args) {
    | (Some(ll), None) => Some(ll)
    | (Some(ll), Some(args)) =>
      UserLivelit.apply_args(
        ~name=llname,
        ~id=ll.id,
        ~args,
        ~runtime=
          IdTagged.FreshGrammar.Exp.ap(
            Operators.Forward,
            IdTagged.FreshGrammar.Exp.var("^" ++ llname),
            args,
          ),
        ll,
      )
    | (None, _) => None
    };

  let init = (any: Language.Any.t, seg: Base.segment) =>
    switch (any) {
    | Exp({term: Ap(_, {term: LivelitName(_), _}, _), _})
    | Exp({
        term: Parens({term: Ap(_, {term: LivelitName(_), _}, _), _}),
        _,
      })
    /* A direct use with parameters, `^a(args)(model)`. */
    | Exp({
        term: Ap(_, {term: Ap(_, {term: LivelitName(_), _}, _), _}, _),
        _,
      })
    | Exp({
        term:
          Parens({
            term: Ap(_, {term: Ap(_, {term: LivelitName(_), _}, _), _}, _),
            _,
          }),
        _,
      }) =>
      Some(((), splice_marked_fields(seg) |> Option.map(s => Syntax(s))))
    | _ => None
    };

  /* Shape analogue of last_good_view: if statics info or the livelit
     entry is transiently unavailable (mid-commit), falling back to the
     default inline shape would collapse a Block-sized placeholder and
     jump the layout. Reuse the last known shape instead. */
  let last_good_shape: Hashtbl.t(Id.t, ProjectorCore.Shape.t) =
    Hashtbl.create(16);

  /* Widen the author's declared shape by what its splices actually hold.
     The author sizes the widget for its controls; the client decides how
     much code is in a splice, and only the editor knows that. Getting
     this wrong is not cosmetic: `.livelit > *:not(svg)` sets overflow
     hidden, so an undersized box does not overlap, it CLIPS -- content
     becomes invisible and unclickable while keyboard navigation still
     enters it, leaving an invisible caret. Over-reserving only wastes
     space, so err that way.

     BOTH axes. Columns for a long expression, and rows for one that
     spans lines: press Enter inside a splice and its content needs a
     second row, which an Inline shape does not have. That clipped the
     whole widget -- slider included -- into a sliver you could no longer
     edit.

     Promoting Inline to Block does move the surrounding text down, which
     is why this was left out at first. That was the wrong call: reflowing
     is a visible, recoverable consequence of what you typed, and clipping
     the widget out of existence is neither. */
  /* A use showing its own syntax holds it as one splice (show_syntax, the
     `_syntax` invoke suffix); its model's splices are inside that one. */
  let syntax_splice = (syntax: Base.segment): option(Base.splice) =>
    switch (syntax) {
    | [Splice(s)] => Some(s)
    | _ => None
    };

  /* The rows the syntax pane takes below the GUI: one per line of the
     syntax, at least as wide as its longest line. */
  let with_syntax_pane =
      (size: Util.Point.t, shape: ProjectorCore.Shape.t)
      : ProjectorCore.Shape.t => {
    let rows = size.row + 1;
    {
      /* Seven columns past the longest line: the box's padding and border
         take about one (sized to the line alone, the pane scrolled and the
         scroll bar hid the model), and the panel labels at the right edge
         take about four, which would otherwise sit on the code. */
      horizontal: max(shape.horizontal, size.col + 7),
      vertical:
        switch (shape.vertical) {
        | Inline => Block(rows)
        | Block(n) => Block(n + rows)
        | Tab(n) => Tab(n + rows)
        },
    };
  };

  let widen_for_splices =
      (
        syntax: Base.segment,
        splice_size: View.splice_size,
        shape: ProjectorCore.Shape.t,
      ) => {
    let (extra_cols, extra_rows) =
      List.fold_left(
        ((cols, rows), s: Base.splice) => {
          let size: Util.Point.t = splice_size(s.id);
          /* segment_bbox reports `row` as the greatest row INDEX, not a
             count: single-line content is 0, two lines is 1. So the
             value already IS the number of rows beyond the first. */
          (cols + size.col, max(rows, size.row));
        },
        (0, 0),
        Segment.direct_splices(syntax),
      );
    let vertical: ProjectorCore.Shape.vertical =
      extra_rows <= 0
        ? shape.vertical
        : (
          switch (shape.vertical) {
          | Inline => Block(extra_rows)
          | Block(n) => Block(n + extra_rows)
          | Tab(n) => Tab(n + extra_rows)
          }
        );
    extra_cols == 0 && extra_rows <= 0
      ? shape
      : {
        horizontal: shape.horizontal + extra_cols,
        vertical,
      };
  };

  /* A livelit may make its footprint a function of its model --
     `let shape = fun m : Model -> ...` -- when its layout depends on it,
     as a row of splices that grows does. Evaluated with the use's latest
     model sample, and cached per projector on that sample. None for a
     constant shape (read statically: UserLivelit.shape_of), and before any
     sample exists; the static shape stands in then. */
  let shape_samples: Hashtbl.t(Id.t, (int, option(ProjectorShape.t))) =
    Hashtbl.create(16);
  let model_shape =
      (info: info, def_elab: TermBase.Exp.t, model: TermBase.Exp.t)
      : option(ProjectorShape.t) => {
    let latest =
      switch (info.dynamics_at(Exp.rep_id(model))) {
      | None => None
      | Some(samples) =>
        List.fold_left(
          (acc, s: Sample.t) =>
            switch (acc) {
            | Some(best: Sample.t) when best.seq >= s.seq => acc
            | _ => Some(s)
            },
          None,
          samples,
        )
      };
    switch (latest) {
    | None => None
    | Some(s) =>
      switch (Hashtbl.find_opt(shape_samples, info.id)) {
      | Some((seq, shape)) when seq == s.seq => shape
      | _ =>
        let shape =
          switch (MvuShape.safe_evaluate(def_elab)) {
          | Error(_) => None
          | Ok(record) =>
            switch (MvuShape.record_field(record, "shape")) {
            | Some(f) =>
              switch (MvuShape.strip_wrappers(f).term) {
              | Fun(_)
              | FixF(_) =>
                switch (
                  MvuShape.safe_evaluate(
                    IdTagged.FreshGrammar.Exp.ap(
                      Forward,
                      f,
                      MvuShape.close_value(s.value),
                    ),
                  )
                ) {
                | Ok(v) => UserLivelit.shape_of(v)
                | Error(_) => None
                }
              | _ => None
              }
            | None => None
            }
          };
        Hashtbl.replace(shape_samples, info.id, (s.seq, shape));
        shape;
      }
    };
  };

  /* Whether the use's livelit defines both params members, so the pane
     has a params line (params_panel, below). */
  let has_params_panel = (info: ProjectorBase.info): bool =>
    switch (get_use(info), info.statics) {
    | (Some((llname, args, _)), Some(InfoExp(exp))) =>
      switch (lookup_use(exp.ctx, llname, args)) {
      | Some({user_def: Some(def_elab), _}) =>
        switch (MvuShape.safe_evaluate(def_elab)) {
        | Ok(record) =>
          Option.is_some(MvuShape.record_field(record, "params_from_model"))
          && Option.is_some(
               MvuShape.record_field(record, "init_from_params"),
             )
        | Error(_) => false
        }
      | _ => false
      }
    | _ => false
    };

  /* The GUI's own footprint: the definition's shape, widened for its
     splices, before any syntax pane is added. */
  let gui_shape = (info, splice_size): ProjectorCore.Shape.t => {
    let looked_up =
      switch (get_use(info), info.statics) {
      | (Some((llname, args, model)), Some(InfoExp(exp))) =>
        switch (lookup_use(exp.ctx, llname, args)) {
        | Some(ll) =>
          let dynamic =
            switch (ll.user_def) {
            | Some(def_elab) => model_shape(info, def_elab, model)
            | None => None
            };
          Some(Option.value(dynamic, ~default=ll.shape));
        | None => None
        }
      | _ => None
      };
    let shape =
      switch (looked_up) {
      | Some(shape) =>
        Hashtbl.replace(last_good_shape, info.id, shape);
        shape;
      | None =>
        switch (Hashtbl.find_opt(last_good_shape, info.id)) {
        | Some(shape) => shape
        | None => ProjectorCore.Shape.inline(32)
        }
      };
    switch (syntax_splice(info.syntax)) {
    | None => widen_for_splices(info.syntax, splice_size, shape)
    /* Widened for the model's splices as before, which are inside the
       syntax splice. */
    | Some(s) => widen_for_splices(s.content, splice_size, shape)
    };
  };

  let placeholder = (_model, info, splice_size) => {
    let shape = gui_shape(info, splice_size);
    switch (syntax_splice(info.syntax)) {
    | None => shape
    /* Given the pane's rows, and one more for a params line. */
    | Some(s) =>
      let size: Util.Point.t = splice_size(s.id);
      let size =
        has_params_panel(info)
          ? {
            ...size,
            row: size.row + 1,
          }
          : size;
      with_syntax_pane(size, shape);
    };
  };

  let replace_model_term =
      (updated_model_term: TermBase.Exp.t, start_term: TermBase.Any.t)
      : TermBase.Any.t => {
    /* The model is the argument of the outermost application, under any
       parens and, while the use shows its syntax, its splice. For a
       direct use with parameters, `^a(args)(model)`, that outer
       application is the one holding the model. */
    let rec replace = (e: TermBase.Exp.t): option(TermBase.Exp.t) =>
      switch (e.term) {
      | Ap(dir, name, _model) =>
        Some({
          ...e,
          term: Ap(dir, name, updated_model_term),
        })
      | Parens(inner) =>
        Option.map(
          inner =>
            {
              ...e,
              term: (Parens(inner): TermBase.Exp.term),
            },
          replace(inner),
        )
      | Splice(inner) =>
        Option.map(
          inner =>
            {
              ...e,
              term: (Splice(inner): TermBase.Exp.term),
            },
          replace(inner),
        )
      | _ => None
      };
    switch (start_term) {
    | Exp(e) =>
      switch (replace(e)) {
      | Some(e) => Exp(e)
      | None =>
        print_endline("Warning - LivelitProj.replace_model_term: not an Ap");
        start_term;
      }
    | _ =>
      print_endline("Warning - LivelitProj.replace_model_term: not an Ap");
      start_term;
    };
  };
  let splice_rows = (_, _, _) => Id.Map.empty;
  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  /* Absent when the projector isn't drawn at the code site (docked to the
     sidebar, or culled from the viewport) */
  /* Focus the container — but never steal focus from a control INSIDE
     the livelit's own GUI (a text input keeps focus across the click). */
  let focus_pointer = (id: Id.t) =>
    switch (JsUtil.get_elem_by_id_opt(Id.cls(id))) {
    | None => ()
    | Some(el) =>
      let inside =
        switch (
          Js_of_ocaml.Js.Opt.to_option(
            Js_of_ocaml.Dom_html.document##.activeElement,
          )
        ) {
        | Some(active) =>
          Js_of_ocaml.Js.to_bool(
            Js_of_ocaml.Js.Unsafe.meth_call(
              el,
              "contains",
              [|Js_of_ocaml.Js.Unsafe.inject(active)|],
            ),
          )
        | None => false
        };
      if (!inside) {
        el##focus;
      };
    };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: None,
    };

  /* Running a view is TWO steps now. `view` returns a ViewCmd, so the
     evaluator builds the command tree and ViewCmdRunner performs it down to
     the Html. Both of this module's view sites go through here so the two
     steps cannot drift apart. */
  let eval_view = (e: DHExp.t): result(DHExp.t, string) =>
    switch (MvuShape.safe_evaluate(e)) {
    | Error(_) as err => err
    | Ok(cmd) => ViewCmdRunner.run(cmd)
    };

  /* Dynamics on: the view fold-in (Statics' Projector case) samples the
     live HTML of a user-defined livelit at this projector's id */
  let dynamics = true;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;
  let context_actions = (_, _, ~splice as _) => [];

  /* The projector's sample stream carries both the view's HTML and the
     use's own value; the live view is the latest HTML-shaped sample.
     Samples are mid-run values — close_value, not strip_wrappers, so the
     HTML's handlers keep their definitions. */
  let live_html = (info: ProjectorBase.info): option(TermBase.Exp.t) =>
    switch (info.dynamics) {
    | None => None
    | Some({samples, _}) =>
      List.fold_left(
        (acc, s: Sample.t) => {
          let v = MvuShape.close_value(s.value);
          /* The fold-in samples `view(model)`, which is a ViewCmd now, so
             a sample has to be RUN before it can be recognised as Html.
             A sample that is already Html is left alone: the stream also
             carries the use's own value, and running is only meaningful
             for the ones that are commands. */
          let v =
            switch (ViewCmdRunner.run(v)) {
            | Ok(html) => html
            | Error(_) => v
            };
          if (MvuShape.is_html(v)) {
            switch (acc) {
            | Some((best, _)) when best >= s.seq => acc
            | _ => Some((s.seq, v))
            };
          } else {
            acc;
          };
        },
        None,
        samples,
      )
      |> Option.map(snd)
    };

  let record_field = MvuShape.record_field;

  /* The latest sampled value at some id (e.g. the model argument) */
  let latest_value = (samples: list(Sample.t)): option(TermBase.Exp.t) =>
    List.fold_left(
      (acc, s: Sample.t) =>
        switch (acc) {
        | Some((best, _)) when best >= s.seq => acc
        | _ => Some((s.seq, s.value))
        },
      None,
      samples,
    )
    |> Option.map(((_, v)) => MvuShape.close_value(v));

  /* An action from a handler. With the model's live value in hand, commit
     the TRANSITION — ^name.update(model, action) — as the new argument:
     the main evaluation normalizes it, so the last interaction stays in the
     program where update's probes (and the stepper) can see it, and the
     next commit collapses it to its value again. Without a sampled model
     value (dynamics off), fall back to evaluating update here, at event
     time, in the builtin environment, and committing the result. */
  /* Optimistic rendering for discrete actions. The authoritative
     pipeline (SetSyntax -> statics -> worker eval -> fresh sample) takes
     on the order of a second; the next model and its view are computable
     at event time in milliseconds, since update/view evaluate in the
     builtin env. Each action commits to the syntax immediately (every
     discrete action is its own history step) while the widget renders
     the optimistic view NOW, with handlers bound to the optimistic model
     so rapid successive actions chain correctly. The entry yields to the
     authoritative sample when the sample's content converges with it —
     guaranteed while definitions are closed, because both sides evaluate
     the same view on the same model — and is dropped whenever the syntax
     model stops matching what we committed (external edit, undo). An
     update result that cannot be checkpointed skips the commit entirely
     and lives only here (see opt_ephemeral below). */
  type optimistic_entry = {
    opt_model: TermBase.Exp.t, /* the newest local model, already a value */
    opt_html: TermBase.Exp.t, /* view(opt_model), evaluated at event time */
    /* Squished prints of every syntax state that is legitimately "ours"
       while commits are in flight: the pre-burst base, then each commit,
       oldest first. During a rapid burst the syntax lags the local model
       by several commits, so renders may see ANY of these. Printed text
       is the identity: the syntax literally came from printing the
       committed terms, and structural comparison trips over
       print/reparse asymmetries (LivelitName vs Var, evaluated negative
       atoms vs unary minus). */
    opt_outstanding: list(string),
    /* Highest outstanding index observed in the syntax; a later render
       matching an EARLIER index means the syntax rewound (undo), which
       drops the entry. */
    mutable opt_matched: int,
    /* Transient (gesture) updates or an uncommittable model have changed
       the model since the last commit; the next committing event must
       flush even if its own update is a no-op. */
    opt_dirty: bool,
    /* The model is not checkpointable, so it never committed: this entry
       is the state's only home. Never yield it to sample convergence
       (samples keep coming from the unchanged stale syntax); undo and
       external edits still drop it — inherent, nothing to restore from. */
    opt_ephemeral: bool,
  };
  let optimistic: Hashtbl.t(Id.t, optimistic_entry) = Hashtbl.create(16);

  /* One console warning per projector id: an unserializable model is a
     standing property of the definition, not per-event news. */
  let warned_ephemeral: Hashtbl.t(Id.t, unit) = Hashtbl.create(4);
  let warn_ephemeral = (~id: Id.t, ~ll_name: string) =>
    if (!Hashtbl.mem(warned_ephemeral, id)) {
      Hashtbl.add(warned_ephemeral, id, ());
      Js_of_ocaml.Firebug.console##warn(
        Js_of_ocaml.Js.string(
          "livelit ^"
          ++ ll_name
          ++ ": model is not serializable; state will not persist to the program text",
        ),
      );
    };

  let squish = str =>
    String.to_seq(str)
    |> Seq.filter(c => c != ' ' && c != '\n' && c != '\t')
    |> String.of_seq;

  let event_inject =
      (
        ~id: Id.t,
        ~print_term: TermBase.Exp.t => string,
        ~ll_name: string,
        ~def_elab: TermBase.Exp.t,
        ~model: TermBase.Exp.t,
        ~model_value: option(TermBase.Exp.t),
        ~commit_model:
           (~effects: list(SpliceStore.effect), TermBase.Exp.t) =>
           Ui_effect.t(unit),
        ~repaint: unit => Ui_effect.t(unit),
        gesture: HazelDOM.gesture,
        action: TermBase.Exp.t,
      )
      : Ui_effect.t(unit) => {
    let fail = msg => {
      print_endline("LivelitProj: " ++ msg);
      Ui_effect.Ignore;
    };
    let ap = IdTagged.FreshGrammar.Exp.ap;
    /* Base model for this action. The optimistic entry wins over the
       handler's captured model: a rapid successor event fires from a DOM
       still showing the PREVIOUS render, whose handlers close over the
       pre-commit model — composing from there would silently stomp the
       in-flight action. The optimistic table is the newest local truth. */
    let base_value =
      switch (Hashtbl.find_opt(optimistic, id)) {
      | Some(e) => Some(e.opt_model)
      | None => model_value
      };
    let base = Option.value(base_value, ~default=model);
    /* ~committed=None: an ephemeral store — the syntax is not changing
       (uncommittable model), so like a transient event it leaves the
       set of "ours" syntax states alone. */
    let store_entry = (new_model, record, ~committed) =>
      switch (record_field(record, "view")) {
      | Some(view_fn) =>
        switch (eval_view(ap(Forward, view_fn, new_model))) {
        | Ok(html) when MvuShape.is_html(html) =>
          let prior = Hashtbl.find_opt(optimistic, id);
          /* Transient and ephemeral events change nothing in the syntax,
             so the set of syntax states that count as "ours" is
             unchanged; only committing events append their commit. */
          let outstanding =
            switch (committed, gesture, prior) {
            | (None, _, Some(e))
            | (Some(_), HazelDOM.Transient, Some(e)) => e.opt_outstanding
            | (None, _, None)
            | (Some(_), HazelDOM.Transient, None) => [
                squish(print_term(model)),
              ]
            | (Some(c), HazelDOM.Commit, Some(e)) =>
              e.opt_outstanding @ [squish(print_term(c))]
            | (Some(c), HazelDOM.Commit, None) => [
                squish(print_term(model)),
                squish(print_term(c)),
              ]
            };
          /* cap the ring; a burst outrunning this many in-flight
             commits falls back to the authoritative path */
          let outstanding = {
            let n = List.length(outstanding);
            n > 64
              ? List.filteri((i, _) => i >= n - 64, outstanding)
              : outstanding;
          };
          incr(optimistic_version);
          Hashtbl.replace(
            optimistic,
            id,
            {
              opt_model: new_model,
              opt_html: html,
              opt_outstanding: outstanding,
              opt_matched:
                switch (prior) {
                | Some(e) => e.opt_matched
                | None => 0
                },
              opt_dirty:
                Option.is_none(committed) || gesture == HazelDOM.Transient,
              opt_ephemeral: Option.is_none(committed),
            },
          );
        | _ =>
          if (Hashtbl.mem(optimistic, id)) {
            incr(optimistic_version);
            Hashtbl.remove(optimistic, id);
          }
        }
      | None => ()
      };
    /* Event-time evaluation of the next model (and, best-effort, its
       view for the optimistic entry). `Skip: the update was a no-op, so
       neither commit nor store — without this, the click the browser
       fires after every drag (and any handler returning the model
       unchanged) would pollute history with identity steps. A committing
       no-op still flushes when transient updates left the entry dirty. */
    let next_model =
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => `Error("definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "update")) {
        | None => `Error("definition is missing update")
        | Some(update_fn) =>
          /* Curried, as Figure 3 curries it, and the result is a
             command: `update(m)(a)` describes the transition and
             UpdateCmdRunner performs it. */
          let applied = ap(Forward, ap(Forward, update_fn, base), action);
          switch (
            switch (MvuShape.safe_evaluate(applied)) {
            | Error(_) as err => err
            | Ok(cmd) => UpdateCmdRunner.run(cmd)
            }
          ) {
          | Error(e) => `Error("update error: " ++ e)
          | Ok((new_model, _)) when commit_decision(new_model) == `Ephemeral =>
            /* The model carries a closure, so it cannot live in the
               syntax tree. Degrade gracefully instead of wedging: keep
               the widget running off the optimistic entry and skip the
               syntax commit. Warned once; undo/external edits drop
               the ephemeral state. */
            warn_ephemeral(~id, ~ll_name);
            store_entry(new_model, record, ~committed=None);
            `Ephemeral;
          | Ok((new_model, effects)) =>
            /* A splice effect is a change even when the model is not:
               set_splice rewrites the client's code, not the refs. */
            let unchanged =
              effects == []
              && squish(print_term(new_model)) == squish(print_term(base));
            let dirty_prior =
              switch (Hashtbl.find_opt(optimistic, id)) {
              | Some(e) => e.opt_dirty
              | None => false
              };
            if (unchanged && (gesture == HazelDOM.Transient || !dirty_prior)) {
              `Skip;
            } else {
              /* What goes in the syntax is the performed update's VALUE.
                 It used to be a redex, ^name.update((value, action)), left
                 in the text so probes and the stepper could see the last
                 transition; but update returns a command now, so that redex
                 would hold a command tree where a model belongs. The cost:
                 the transition is no longer visible in the text, and a
                 definition that is not closed -- which used to fall back
                 to the redex and the program's own run -- now fails at the
                 event instead. */
              let committed = new_model;
              store_entry(new_model, record, ~committed=Some(committed));
              `Ok((committed, effects));
            };
          };
        }
      };
    switch (gesture, next_model) {
    | (_, `Skip) => Ui_effect.Ignore
    | (_, `Ephemeral) =>
      /* Nothing committed; repaint so the optimistic view shows. */
      repaint()
    | (Transient, `Ok(_)) =>
      /* Live preview only: the optimistic entry above is the whole
         effect; a quiet non-historic action makes the frame repaint. */
      repaint()
    | (Transient, `Error(e)) => fail(e)
    | (Commit, `Ok(committed, effects)) => commit_model(~effects, committed)
    | (Commit, `Error(e)) => fail(e)
    };
  };

  /* User-defined livelit: render the view, a Hazel HTML value, via
     HazelDOM. Preferred source is the live sample the view fold-in recorded
     during the main evaluation; without one (dynamics off, or not yet
     evaluated) the captured definition is evaluated here instead, which
     requires it to be closed. Actions run through update and commit to the
     syntax, so each use's model lives in its own Ap argument, like builtin
     livelits. */
  /* Last successfully rendered view per projector instance. After a
     commit, until the main evaluation delivers a fresh sample, the
     render-time fallback has only the surface model, whose splices are
     their code rather than refs (eval_splice needs a ref), so the view
     would flash an error. Instead, show the last
     good render, dimmed and inert (its handlers close over the stale
     model, so letting clicks through could silently drop the in-flight
     edit). Display-only cache; entries overwrite on every successful
     render. */
  let last_good_view: Hashtbl.t(Id.t, Node.t) = Hashtbl.create(16);

  let user_view =
      (
        ~id: Id.t,
        ~print_term: TermBase.Exp.t => string,
        ~ll_name: string,
        ~def_elab: TermBase.Exp.t,
        ~model: TermBase.Exp.t,
        ~model_value: option(TermBase.Exp.t),
        ~commit_model:
           (~effects: list(SpliceStore.effect), TermBase.Exp.t) =>
           Ui_effect.t(unit),
        ~repaint: unit => Ui_effect.t(unit),
        ~view_term: TermBase.Exp.t => Node.t,
        ~splice_view_at: string => option(Node.t),
        ~live: option(TermBase.Exp.t),
      )
      : Node.t => {
    let err = msg =>
      switch (Hashtbl.find_opt(last_good_view, id)) {
      | Some(node) =>
        Node.div(~attrs=[Attr.classes(["livelit-pending"])], [node])
      | None =>
        Node.div(
          ~attrs=[Attr.classes(["livelit-user-error"])],
          [Node.text(msg)],
        )
      };
    let ok = (node: Node.t): Node.t => {
      Hashtbl.replace(last_good_view, id, node);
      node;
    };
    /* From the model in the SYNTAX -- `seed` shadows `model` below, and
       the optimistic path deliberately passes the evaluated value. */
    let seed = (~model, ~model_value): HazelDOM.t => {
      inject:
        event_inject(
          ~id,
          ~print_term,
          ~ll_name,
          ~def_elab,
          ~model,
          ~model_value,
          ~commit_model,
          ~repaint,
        ),
      view_term,
      splice_view: splice_view_at,
      commit: HazelDOM.State,
    };
    /* Optimistic entry: render it (interactive, full brightness) until
       the authoritative sample content-converges with it or the syntax
       model stops matching what we committed (external edit / undo). */
    let opt =
      switch (Hashtbl.find_opt(optimistic, id)) {
      | None => None
      | Some(entry) =>
        /* An ephemeral entry never converges: no commit is in flight, so
           the sample forever reflects the stale syntax — yielding to it
           would silently revert the state. Converging also needs the
           evaluated model: once the entry is gone, handlers compose from
           model_value, which is keyed by the model term's id, and every
           commit renews that id. Until the evaluator has run on the new
           text it is None, and an event in that gap composes from the
           SYNTAX model, whose splices are code rather than refs. A slow
           browser lands events in the gap. */
        /* Converged means the run has caught up with the optimistic
           MODEL, not only its picture: a transient step that changes
           nothing visible (a drag's Down, which only records that the
           pointer is held) draws the same HTML as the stale sample, and
           yielding to it then threw the step away -- the next MoveTo
           composed from the pre-drag model, and the drag did nothing. */
        let converged =
          !entry.opt_ephemeral
          && (
            switch (model_value) {
            | Some(mv) =>
              squish(print_term(mv)) == squish(print_term(entry.opt_model))
            | None => false
            }
          )
          && (
            switch (live) {
            | Some(l) => Exp.fast_equal(l, entry.opt_html)
            | None => false
            }
          );
        let model_print = squish(print_term(model));
        let idx = {
          let rec find = (i, xs) =>
            switch (xs) {
            | [] => None
            | [x, ..._] when x == model_print => Some(i)
            | [_, ...rest] => find(i + 1, rest)
            };
          find(0, entry.opt_outstanding);
        };
        let drop = () => {
          incr(optimistic_version);
          Hashtbl.remove(optimistic, id);
          None;
        };
        switch (idx) {
        | _ when converged => drop()
        | None =>
          /* syntax shows something we never committed: external edit */
          drop()
        | Some(i) when i < entry.opt_matched =>
          /* syntax rewound to an earlier state: undo */
          drop()
        | Some(i) =>
          entry.opt_matched = i;
          Some(entry);
        };
      };
    switch (opt, live) {
    | (Some(entry), _) =>
      ok(
        HazelDOM.go(
          ~elide_errors=true,
          seed(~model=entry.opt_model, ~model_value=Some(entry.opt_model)),
          entry.opt_html,
        ),
      )
    | (None, Some(html)) =>
      ok(HazelDOM.go(~elide_errors=true, seed(~model, ~model_value), html))
    | (None, None) =>
      let ap = IdTagged.FreshGrammar.Exp.ap;
      switch (MvuShape.safe_evaluate(def_elab)) {
      | Error(e) => err("livelit definition error: " ++ e)
      | Ok(record) =>
        switch (record_field(record, "view")) {
        | None => err("livelit definition is missing view")
        | Some(view_fn) =>
          /* The run's model value when there is one: in the surface term
             a marked field is only its code, not the ref it becomes. The
             view is drawn from that value, but the handlers are seeded
             with the SYNTAX model, as on the live path: a transient
             event records the seed's print as the syntax state that is
             "ours", and the evaluated value prints its splices as
             SpliceRef(id, value) where the syntax prints their code. Seeded
             with the value, that record never matched, so every preview
             was dropped as an external edit -- and a drag, whose release
             is a no-op on the pre-drag model, was lost entirely. */
          let view_model = Option.value(model_value, ~default=model);
          switch (eval_view(ap(Forward, view_fn, view_model))) {
          | Error(e) => err("livelit view error: " ++ e)
          | Ok(html) when MvuShape.is_html(html) =>
            ok(
              HazelDOM.go(
                ~elide_errors=true,
                seed(~model, ~model_value),
                html,
              ),
            )
          | Ok(_) => err("livelit view did not produce HTML")
          };
        }
      };
    };
  };

  /* The params panel, shown with the model panel while a use shows its
     syntax: EXPERIMENTAL, not in the paper. A livelit that defines
     params_from_model (Model -> Params) and init_from_params
     ((Params, Model) -> Model) gets a line holding its params as code;
     editing them re-runs init_from_params, and the model it makes
     replaces the current one. init_from_params is handed the old model
     too, so a livelit holding splices keeps them: a params edit rebuilds
     the livelit's own data and leaves the client's code in its cells
     alone. So it makes no splices, and is a plain function, not a
     command -- which also lets its body be checked: an optional member
     has no type from the signature, so a bare Pure in it would not know
     which command it built. Params are never stored: the text holds
     only the model, and the panel recomputes them from it. A livelit
     that defines neither has nothing here, its params being its model. */
  let params_panel =
      (
        ~def_elab: TermBase.Exp.t,
        ~base: TermBase.Exp.t,
        ~print_term: TermBase.Exp.t => string,
        ~parse: string => option(TermBase.Exp.t),
        ~commit_model:
           (~effects: list(SpliceStore.effect), TermBase.Exp.t) =>
           Ui_effect.t(unit),
      )
      : option(Node.t) => {
    let ap = IdTagged.FreshGrammar.Exp.ap;
    switch (MvuShape.safe_evaluate(def_elab)) {
    | Error(_) => None
    | Ok(record) =>
      switch (
        record_field(record, "params_from_model"),
        record_field(record, "init_from_params"),
      ) {
      | (Some(from_model), Some(init_from)) =>
        let text =
          switch (MvuShape.safe_evaluate(ap(Forward, from_model, base))) {
          | Ok(params) => print_term(params)
          | Error(e) => "params_from_model error: " ++ e
          };
        let reinit = (typed: string) =>
          switch (parse(typed)) {
          | None =>
            print_endline("LivelitProj: params do not parse: " ++ typed);
            Ui_effect.Ignore;
          | Some(params) =>
            /* A plain function: it keeps the old model's splices rather
               than making new ones, so it needs no command. */
            switch (
              MvuShape.safe_evaluate(
                ap(
                  Forward,
                  init_from,
                  IdTagged.FreshGrammar.Exp.tuple([params, base]),
                ),
              )
            ) {
            | Ok(new_model) => commit_model(~effects=[], new_model)
            | Error(e) =>
              print_endline("LivelitProj: init_from_params error: " ++ e);
              Ui_effect.Ignore;
            }
          };
        Some(
          Node.div(
            ~attrs=[Attr.class_("livelit-params")],
            [
              Node.input(
                ~attrs=[
                  Attr.class_("livelit-params-input"),
                  Attr.value(text),
                  Attr.on_keydown(_ => Effect.Stop_propagation),
                  Attr.on_keyup(_ => Effect.Stop_propagation),
                  Attr.on_pointerdown(_ => Effect.Stop_propagation),
                  Attr.on_change((_, typed) =>
                    typed == text ? Ui_effect.Ignore : reinit(typed)
                  ),
                ],
                (),
              ),
              Node.span(
                ~attrs=[
                  Attr.class_("livelit-panel-label"),
                  Attr.title(
                    "experimental: params_from_model of the model; an edit re-runs init_from_params",
                  ),
                ],
                [Node.text("params")],
              ),
            ],
          ),
        );
      | _ => None
      }
    };
  };

  let view =
      (
        {
          info,
          parent,
          local_quiet,
          view_seg,
          splices,
          splice_view,
          splice_size,
          _,
        }:
          View.args(model, action),
      ) => {
    let ctx =
      switch (info.statics) {
      | Some(InfoExp(exp)) => exp.ctx
      | _ => Ctx.empty
      };

    /* Set while drawing a user livelit's GUI, for the pane below. */
    let params = ref(None);
    let node =
      switch (get_use(info)) {
      | Some((ll_name, args, model)) =>
        let ll = lookup_use(ctx, ll_name, args);

        /* Write an updated model back into the Ap's argument position,
           with its splices: SpliceStore.write_model turns each SpliceRef
           in the value into the splice it names, and applies what
           new_splice and set_splice did. A model with no splices before
           or after keeps the original SetSyntax path. One with splices
           cannot: SetSyntax reprints the projector's whole segment, and a
           splice prints as nothing but its content, so the client's code
           would be flattened into the model. SetTerm regenerates the
           segment from the term and re-attaches existing splices by id.
           A splice the new model no longer reaches is not written back;
           that is deletion, implicit, as the paper has no command for it.
           The caret is not preserved on either path (SetTerm rebuilds the
           zipper from the root), so a widget action still evicts the
           caret from a splice being edited. */
        let commit_model =
            (~effects: list(SpliceStore.effect), new_model: TermBase.Exp.t) => {
          let existing = SpliceStore.splice_ids(model);
          let written =
            SpliceStore.write_model(
              ~effects,
              ~existing,
              ~declared=SpliceStore.declared_types(model),
              new_model,
            );
          /* An update cannot legitimately drop EVERY splice without an
             effect saying so; when it seems to, the model it started from
             held no refs (the syntax model, splices as code), and
             committing it would write each splice back as plain code.
             Drop that one event instead. */
          if (existing != []
              && effects == []
              && SpliceStore.splice_ids(written) == []) {
            print_endline(
              "Warning - LivelitProj: dropped a commit that would flatten "
              ++ string_of_int(List.length(existing))
              ++ " splices into code",
            );
            Ui_effect.Ignore;
          } else if (existing == [] && SpliceStore.splice_ids(written) == []) {
            switch (
              info.utility.lift_syntax(
                ~inline=true,
                replace_model_term(written),
                info.syntax,
              )
            ) {
            | Some(s) => parent(SetSyntax(s))
            | None =>
              print_endline("Warning - LivelitProj.view: lift_syntax failed");
              Ui_effect.Ignore;
            };
          } else {
            switch (info.utility.seg_to_term(info.syntax)) {
            | Some(t) =>
              parent(SetTerm(replace_model_term(written, t), true))
            | None =>
              print_endline("Warning - LivelitProj.view: seg_to_term failed");
              Ui_effect.Ignore;
            };
          };
        };

        switch (ll) {
        | Some({user_def: Some(def_elab), _}) =>
          let view_term = term =>
            Exp(term)
            |> info.utility.term_to_seg(~inline=true)
            |> view_seg(~background=false, Exp);
          let model_value =
            Option.bind(info.dynamics_at(Exp.rep_id(model)), latest_value);
          /* Model terms contain no projectors or refractors, so trivial
             handlers suffice (the real ones live above this module in the
             dependency order). */
          let print_term = term =>
            Segment.to_string(
              ~refractor_seg_to_seg=(rs, seg) => (rs, seg),
              ~projector_to_segment=_ => [],
              info.utility.term_to_seg(~inline=true, Exp(term)),
            );
          if (Option.is_some(syntax_splice(info.syntax))) {
            params :=
              params_panel(
                ~def_elab,
                ~base=Option.value(model_value, ~default=model),
                ~print_term,
                ~parse=info.utility.string_to_exp,
                ~commit_model,
              );
          };
          Node.div(
            ~attrs=[
              Attr.classes([ll_name, "user-livelit"]),
              Attr.id(Id.cls(info.id)),
            ],
            [
              user_view(
                ~id=info.id,
                ~ll_name,
                ~print_term,
                ~def_elab,
                ~model,
                ~model_value,
                ~commit_model,
                /* Non-historic Layout-level no-op: repaints the frame so a
                   transient (drag) update becomes visible without an edit. */
                ~repaint=() => local_quiet(),
                ~view_term,
                /* Resolve a splice by the id its SpliceRef carries. Only
                   this livelit's own splices are reachable: a ref naming
                   someone else's, or a stale one, finds nothing and
                   renders as an error rather than another widget's hole. */
                ~splice_view_at=
                  id =>
                    List.find_opt(
                      (s: Base.splice) => Id.to_string(s.id) == id,
                      splices,
                    )
                    |> Option.map((s: Base.splice) => splice_view(s.id)),
                ~live=live_html(info),
              ),
            ],
          );
        | Some(ll) =>
          let action_callback = (action: LivelitCtx.action_exp) =>
            commit_model(~effects=[], ll.update(action, model));

          let list_contents = ll.view(model, action_callback);
          Node.div(
            ~attrs=[Attr.class_(ll_name), Attr.id(Id.cls(info.id))],
            [list_contents],
          );
        | None =>
          print_endline("Warning - LivelitProj.view: not found in context");
          Node.text("No livelit found");
        };
      | None =>
        print_endline("Warning - LivelitProj.view: get is empty");
        Node.text("No livelit found");
      };

    /* The syntax toggle, at the livelit's top left, and while the use shows
       its syntax, that syntax as an editor in the rows below the GUI (see
       placeholder). An overlay, so the livelit's own markup, which CSS
       reaches as `.livelit > ...`, is left as it was. */
    let shown = syntax_splice(info.syntax);
    let toggle =
      Node.div(
        ~attrs=[
          Attr.classes(
            ["livelit-syntax-toggle"]
            @ (Option.is_some(shown) ? ["on"] : []),
          ),
          Attr.title(
            Option.is_some(shown)
              ? "hide this use's syntax" : "show this use's syntax, to edit",
          ),
          Attr.on_pointerdown(_ =>
            Effect.Many([
              Effect.Stop_propagation,
              Effect.Prevent_default,
              parent(ToggleSyntax),
            ])
          ),
        ],
        /* An eye, hanging in the margin to the GUI's left like a hanging
           indent: it shows the use's syntax, which is its model. */
        [Node.text("\xF0\x9F\x91\x81\xEF\xB8\x8F")],
      );
    let pane =
      switch (shown) {
      | Some(s) =>
        let size: Util.Point.t = splice_size(s.id);
        let rows = size.row + 1 + (Option.is_some(params^) ? 1 : 0);
        Some(
          Node.div(
            ~attrs=[
              Attr.classes(
                ["livelit-syntax"]
                @ (Option.is_some(params^) ? ["with-params"] : []),
              ),
              Attr.create(
                "style",
                Printf.sprintf(
                  "height: calc(%d * var(--row-height-px));",
                  rows,
                ),
              ),
            ],
            Option.to_list(params^)
            @ [
              Node.span(
                ~attrs=[
                  Attr.classes([
                    "livelit-panel-label",
                    "livelit-model-label",
                  ]),
                  Attr.title("this use's syntax: an edit changes the model"),
                ],
                [Node.text("model")],
              ),
              splice_view(s.id),
            ],
          ),
        );
      | None => None
      };
    /* With the syntax shown, the GUI and the pane share the livelit's
       box, which the placeholder grew by the pane's rows: a column whose
       GUI part takes what is left, centered as before, and whose pane
       sits at the bottom. In flow rather than overlaid, because a Tab
       GUI hangs below the use's row in its own box, the tab extent, and
       centers in all of it: an overlay at a computed offset covered it. */
    let node =
      switch (pane) {
      | Some(pane) =>
        Node.div(
          ~attrs=[Attr.class_("livelit-with-syntax")],
          [Node.div(~attrs=[Attr.class_("livelit-gui")], [node]), pane],
        )
      | None => node
      };
    View.mk(
      ~overlay=
        Some(
          Node.div(~attrs=[Attr.class_("livelit-syntax-layer")], [toggle]),
        ),
      node,
    );
  };
};
