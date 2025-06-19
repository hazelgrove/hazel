open Util;
open ProjectorInterface;
open Virtual_dom.Vdom;
open Node;
open Js_of_ocaml;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type closure = Dynamics.Probe.Closure.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed_m) = {ed: 'ed_m};

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | ChangeLength(int, int)
  | MoveCursor(int)
  | ToggleShowAllVals(int)
  | NoOp
  | PinAp;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let len_of_exp = (exp: Exp.t): int =>
    //TODO(andrew): get this more performantly
    Exp(exp) |> Editor.Model.mk |> Editor.View.print_string |> String.length;

  let abbreviated_exp = (available: int, exp: Exp.t): Exp.t => {
    //TODO(andrew): cleanup
    let (abbr_exp, _length) =
      exp |> DHExp.strip_casts |> Abbreviate.abbreviate_exp(~available);
    abbr_exp;
  };

  module Window = {
    type mode =
      | Single
      | Many;

    let mode = ref(Many);

    let offset = Hashtbl.create(100);

    let reset = () => {
      Hashtbl.clear(offset);
      mode := Many;
    };

    let max_closures = () =>
      switch (mode^) {
      | Single => 1
      | Many => 30
      };

    let get_mode = () => mode^;

    let toggle_mode = () =>
      switch (mode^) {
      | Single => mode := Many
      | Many => mode := Single
      };

    let get_offset = (k: Id.t): int =>
      switch (Hashtbl.find_opt(offset, k)) {
      | Some(v) => v
      | None => 0
      };

    /* We are displaying a certain window of closures near the closure cursor.
     * If the closure cursor moves, we want to readjust this window to show the
     * cursor, but only if necessary. Thus we compare the cursor position to the
     * current window bounds, and make the minimum change to the window necessary
     * to show the cursor. As an edge case, if there are less total closures than
     * the window size, we set the window to begin at zero. */
    let new_offest =
        (cursor_idx: int, home: int, max_closures: int, all_closures: int)
        : int =>
      if (all_closures <= max_closures) {
        0;
      } else if (cursor_idx < home) {
        cursor_idx;
      } else if (cursor_idx >= home + max_closures) {
        cursor_idx - max_closures + 1;
      } else {
        home;
      };

    let set_offset = (k: Id.t, v: int) => Hashtbl.add(offset, k, v);

    let reform = (id, all_closures, cursor_idx): (int, int) => {
      let max = max_closures();
      let new_home =
        new_offest(cursor_idx, get_offset(id), max, all_closures);
      set_offset(id, new_home);
      (new_home, max);
    };
  };

  let is_value = (exp: Exp.t) =>
    ValueChecker.check_value((), ClosureEnvironment.empty, exp) == Value;

  module ClosureLength = {
    let lengths: Hashtbl.t(int, int) = Hashtbl.create(100);

    let reset = () => {
      Hashtbl.clear(lengths);
    };

    let get = (closure: closure): int =>
      Hashtbl.find_opt(lengths, closure.closure_id)
      |> Option.value(
           ~default=
             !is_value(closure.value)
               ? 5 : Window.get_mode() == Single ? 36 : 12,
         );

    let set = (id: int, length: int): unit =>
      Hashtbl.add(lengths, id, length);
  };

  /* Remove opaque values like function literals */
  let rm_opaques:
    list(Dynamics.Probe.Env.entry) => list(Dynamics.Probe.Env.entry) =
    List.filter_map((en: Dynamics.Probe.Env.entry) =>
      switch (en.value) {
      | Opaque => None
      | Val(_) => Some(en)
      }
    );

  /* Don't redundantly show an env for variable references, patterns */
  let hide_env = (statics: option(Info.t)): bool =>
    switch (statics) {
    | Some(
        InfoExp({
          term: {term: Var(_) | Probe({term: Var(_), _}, _), _},
          _,
        }),
      ) =>
      true
    | Some(InfoPat(_)) => true
    | _ => false
    };

  let show_purps = ref(false);

  let cur_ap = (statics: option(Info.t)) =>
    switch (statics) {
    | Some(InfoExp({term: {term: Ap(_), _} as ap, _}))
    | Some(
        InfoExp({term: {term: Probe({term: Ap(_), _} as ap, _), _}, _}),
      )
        when show_purps^ =>
      Some(Term.Exp.rep_id(ap))
    | _ => None
    };

  module DynCursor = {
    /* Manages shared state between probes */
    type t = {
      mutable call_cursor: Probe.call_stack,
      mutable indicated_call: option(Id.t),
      mutable pinned_call: option(Probe.call_stack),
    };

    let s: t = {
      call_cursor: [],
      indicated_call: None,
      pinned_call: None,
    };

    let reset = () => {
      s.call_cursor = [];
      s.indicated_call = None;
      s.pinned_call = None;
    };

    let capture_cursor = (closure: closure): unit => {
      s.call_cursor = closure.call_stack;
    };

    let capture_ap = (statics): unit => {
      s.indicated_call = cur_ap(statics);
    };

    let capture = (statics, closure: closure): unit => {
      capture_cursor(closure);
      capture_ap(statics);
    };

    let is_in = (closures: list(closure)): option(closure) =>
      List.find_opt(
        (closure: closure) => s.call_cursor == closure.call_stack,
        closures,
      );

    /* If one of the current probe's cells is not already selected,
     * select the first one */
    let probe_default = (statics, dynamics): unit =>
      switch (dynamics) {
      | Some(closures) when is_in(closures) != None => capture_ap(statics)
      | Some(closures) =>
        capture_ap(statics);
        switch (closures) {
        | [fst, ..._] => capture_cursor(fst)
        | [] => ()
        };
      | None => ()
      };

    /* If the closure cursor is on a call, and the provided
     * call stack is downstream of that call, return how many
     * aps downstream it is */
    let _depth_in_indicated_calls_stack =
        (call_stack: Probe.call_stack): option(int) => {
      open OptUtil.Syntax;
      let* cur_ap = s.indicated_call;
      ListUtil.suffix_at_depth([cur_ap] @ s.call_cursor, call_stack);
    };

    type relative_level =
      | Above(int)
      | Below(int)
      | Same;

    /* How is the current closure related to the closure cursor? */
    type relation = {
      /* Is the current closure the call cursor? */
      is_call_cursor: bool,
      relative_level_to_cursor: relative_level,
      /* Is the current closure a call directly above the call cursor? */
      is_call_above_call_cursor: option(int),
      /* Is the current closure below the call cursor, and if so, by how much? */
      is_below_indicated_call: option(int),
    };

    let is_below = ListUtil.suffix_at_depth;

    let relative_level = (cs1, cs2): relative_level =>
      switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
      | (Some(0), Some(0)) => Same
      | (Some(n), None) => Below(n)
      | (None, Some(n)) => Above(n)
      | (_, _) => Same
      };

    let cur_call = (statics, closure: closure) => {
      open OptUtil.Syntax;
      let* lex = cur_ap(statics);
      let dyn = closure.call_stack;
      Some([lex, ...dyn]);
    };

    let relation = (statics, closure: closure): relation => {
      open OptUtil.Syntax;
      let this = closure.call_stack;
      let cursor = s.call_cursor;
      {
        is_call_cursor: cursor == this,
        relative_level_to_cursor: relative_level(cursor, this),
        is_call_above_call_cursor: {
          let* cur_call = cur_call(statics, closure);
          is_below(cur_call, cursor);
        },
        is_below_indicated_call: {
          let* cur_ap = s.indicated_call;
          is_below([cur_ap] @ cursor, this);
        },
      };
    };

    let clss = (statics, closure: closure): list(string) => {
      let relation = relation(statics, closure);
      (
        switch (
          relation.is_call_cursor,
          relation.is_call_above_call_cursor,
          relation.is_below_indicated_call,
        ) {
        | (true, _, _) => ["cursor"]
        | (_, Some(0), _) => ["cursor-caller", "direct"]
        | (_, Some(_), _) => ["cursor-caller", "indirect"]
        | (_, _, Some(0)) => ["cursor-callee", "direct"]
        | (_, _, Some(_)) => ["cursor-callee", "indirect"]
        | (_, _, None) => ["cursor-unrelated"]
        }
      )
      @ (
        switch (relation.relative_level_to_cursor) {
        | Same => ["level0"]
        | Below(n) => ["below", "L" ++ string_of_int(n)]
        | Above(n) => ["above", "L" ++ string_of_int(n)]
        }
      );
    };

    let first_index_of_interest =
        (info, closures: list(closure)): option(int) => {
      let find = (rel: relation => bool): option(int) =>
        List.find_index(
          (closure: closure) => rel(relation(info, closure)),
          closures,
        );
      switch (find(relation => relation.is_call_cursor)) {
      | Some(idx) => Some(idx)
      | None =>
        switch (find(relation => relation.is_below_indicated_call == Some(0))) {
        | Some(idx) => Some(idx)
        | None => find(relation => relation.is_below_indicated_call != None)
        }
      };
    };
    let first_cursor_closure =
        (statics, closures: list(closure)): option(closure) => {
      let find_cursor =
        List.find_opt(
          (closure: closure) => relation(statics, closure).is_call_cursor,
          closures,
        );
      switch (find_cursor) {
      | Some(closure) => Some(closure)
      | None => None
      };
    };

    let show_pin = info => {
      s.pinned_call != None
      && s.pinned_call
      |> OptUtil.and_then(ListUtil.hd_opt) == cur_ap(info);
    };

    let is_pinned = (statics, dynamics): bool =>
      switch (OptUtil.and_then(is_in, dynamics)) {
      | Some(closure_cursor) =>
        s.pinned_call == cur_call(statics, closure_cursor)
      | _ => false
      };

    let pin_call = (statics, dynamics): unit =>
      switch (OptUtil.and_then(is_in, dynamics)) {
      | Some(closure_cursor) =>
        s.pinned_call = cur_call(statics, closure_cursor)
      | _ => ()
      };

    let unpin_call = (): unit => {
      s.pinned_call = None;
    };

    let toggle_pinned_call = (statics, dynamics) =>
      switch (s.pinned_call) {
      | Some(pinned_ap) when ListUtil.hd_opt(pinned_ap) == cur_ap(statics) =>
        /* already pinned case */
        unpin_call()
      | Some(_)
      | None => pin_call(statics, dynamics)
      };

    module Debug = {
      let stack = (stack: Probe.call_stack): string =>
        stack |> List.map(Id.str3) |> String.concat("\n");

      let _str = (info, closure: closure): string =>
        "ap:"
        ++ (
          switch (cur_call(info, closure)) {
          | Some([ap_id, ..._]) => Id.str3(ap_id)
          | _ => "None"
          }
        )
        ++ "\nvalue:\n"
        ++ DHExp.show(closure.value)
        ++ "\nstack:\n"
        ++ stack(closure.call_stack);
      // ++ "\ntime: "
      // ++ string_of_float(closure.time /. 10000.0);
    };
  };

  module Closures = {
    let filter_frames_by_pin =
        (statics, frames: list(closure)): list(closure) =>
      switch (DynCursor.s.pinned_call) {
      | Some(pinned_ap) =>
        List.filter(
          (closure: closure) =>
            ListUtil.hd_opt(pinned_ap) == cur_ap(statics)
            || ListUtil.is_suffix_of(pinned_ap, closure.call_stack),
          frames,
        )
      | None => frames
      };

    let total = (statics, dynamics): int =>
      switch (dynamics) {
      | Some(closures) =>
        List.length(filter_frames_by_pin(statics, closures))
      | None => 0
      };

    let select_frames =
        (id, statics, closures: list(closure)): list(closure) => {
      let closures = filter_frames_by_pin(statics, closures);
      let cursor_idx =
        switch (DynCursor.first_index_of_interest(statics, closures)) {
        | Some(idx) => idx
        | None => 0
        };
      let all_closures = List.length(closures);
      let (l, r) = Window.reform(id, all_closures, cursor_idx);
      ListUtil.slice(l, r, closures);
    };

    let group_by_predicate =
        /* Precondition: Items to be grouped are contigious in list */
        (should_group: ('a, 'a) => bool, xs: list('a)): list(list('a)) => {
      List.fold_left(
        (acc: list(list('a)), item: 'a) => {
          switch (acc) {
          | [] => [[item]]
          | [[rep, ..._] as first, ...init] when should_group(rep, item) => [
              first @ [item],
              ...init,
            ]
          | _ => [[item]] @ acc
          }
        },
        [],
        xs,
      );
    };

    let is_same_call = ((_, c1: closure), (_, c2: closure)): bool => {
      switch (List.rev(c2.call_stack), List.rev(c1.call_stack)) {
      | ([], _)
      | (_, []) => false
      | ([f1, ..._], [f2, ..._]) => f1 == f2
      };
    };

    let group =
        (closures: list((int, closure))): list(list((int, closure))) => {
      let grouped =
        closures |> group_by_predicate(is_same_call) |> List.map(List.rev);
      /* Flatten if all groups are singletons */
      List.for_all(group => List.length(group) == 1, grouped)
        ? [List.concat(grouped)] : grouped;
    };

    let collate =
        (closures: list(closure)): (int, list(list((int, closure)))) => {
      let numbered_closures =
        List.mapi((i, c) => (List.length(closures) - i - 1, c), closures);
      (List.length(closures), group(numbered_closures));
    };
  };

  let pos_rel_to_target = (e: Js.t(Dom_html.mouseEvent)): Point.t => {
    open Float;
    let row_height = 10.0;
    let col_width = 10.0;
    let text_box =
      e##.currentTarget
      |> Js.Opt.get(_, _ => failwith(""))
      |> JsUtil.get_child_with_class(_, "code")
      |> Option.get;
    let x_rel = of_int(e##.clientX) -. text_box##getBoundingClientRect##.left;
    let y_rel = of_int(e##.clientY) -. text_box##getBoundingClientRect##.top;
    let row = to_int(y_rel /. row_height);
    let col = to_int(round(x_rel /. col_width));
    {
      row,
      col,
    };
  };

  let length_cls = (length: int): string =>
    if (length > 10) {
      "extra";
    } else if (length > 9) {
      "s6";
    } else if (length > 8) {
      "s5";
    } else if (length > 7) {
      "s4";
    } else if (length > 6) {
      "s3";
    } else if (length > 5) {
      "s2";
    } else if (length > 4) {
      "s1";
    } else {
      "s0";
    };

  module ValueState = {
    let mousedown: ref(option(Js.t(Dom_html.element))) = ref(Option.None);
    let click_coords: ref(option(Point.t)) = ref(Option.None);
  };

  let value_view = (~common, statics, local, closure: closure, index: int) => {
    let val_pointerdown = (e: Js.t(Dom_html.pointerEvent)) => {
      if (Js.to_bool(e##.shiftKey)) {
        let target =
          e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
        JsUtil.setPointerCapture(target, e##.pointerId);
        ValueState.mousedown := Some(target);
        ValueState.click_coords :=
          Some({
            row: e##.clientY,
            col: e##.clientX,
          });
      };
      DynCursor.capture(statics, closure);
      Effect.Ignore;
    };

    let val_pointerup = (e: Js.t(Dom_html.pointerEvent)) => {
      let target =
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("no target"));
      if (JsUtil.hasPointerCapture(target, e##.pointerId)) {
        JsUtil.releasePointerCapture(target, e##.pointerId);
      };
      ValueState.mousedown := None;
      ValueState.click_coords := None;
      Effect.Ignore;
    };

    let val_mousemove = (e: Js.t(Dom_html.mouseEvent)) => {
      switch (ValueState.mousedown^) {
      | Some(_) when Js.to_bool(e##.shiftKey) =>
        let goal = pos_rel_to_target(e);
        local(ChangeLength(closure.closure_id, goal.col));
      | _ => Effect.Ignore
      };
    };

    let term = abbreviated_exp(ClosureLength.get(closure), closure.value);
    let (view, Point.{row: length, col: _}) =
      Editor.View.term(~common, Grammar.Exp(term));

    div(
      ~attrs=[
        //Attr.title(DynCursor.Debug.str(info, closure)),
        Attr.classes(
          ["value", length_cls(length)]
          @ DynCursor.clss(statics, closure)
          @ (Option.is_some(cur_ap(statics)) ? ["ap"] : [])
          @ (!is_value(closure.value) ? ["indet"] : []),
        ),
        Attr.on_double_click(_ => local(ToggleShowAllVals(index))),
        Attr.on_pointerdown(val_pointerdown),
        Attr.on_pointerup(val_pointerup),
        Attr.on_mousemove(val_mousemove),
      ],
      [view],
    );
  };

  let env_val = (~common, closure, en: Dynamics.Probe.Env.entry): Node.t => {
    Node.div(
      ~attrs=[Attr.classes(["live-env-entry"])],
      [
        Node.text(en.binding.name ++ " ≡ "),
        switch (en.value) {
        | Opaque => Node.text("Opaque")
        | Val(d) =>
          let term = abbreviated_exp(ClosureLength.get(closure), d);
          Editor.View.term(~common, Grammar.Exp(term)) |> fst;
        },
      ],
    );
  };

  let env_view = (~common, closure: closure): Node.t =>
    Node.div(
      ~attrs=[Attr.classes(["live-env"])],
      closure.env
      |> ListUtil.dedup
      |> rm_opaques
      |> List.map(env_val(~common, closure)),
    );

  let closure_view =
      (~common, statics, local, (index: int, closure: closure)) =>
    div(
      ~attrs=[Attr.classes(["closure"])],
      [value_view(~common, statics, local, closure, index)]
      @ (hide_env(statics) ? [] : [env_view(~common, closure)]),
    );

  let closure_group_view =
      (~common, statics, local, groups: list(list((int, closure)))) => {
    let group_views =
      List.map(
        closures =>
          Node.div(
            ~attrs=[Attr.classes(["closure-group"])],
            List.map(closure_view(~common, statics, local), closures),
          ),
        groups,
      );
    group_views == []
      ? [] : [div(~attrs=[Attr.classes(["closure-groups"])], group_views)];
  };

  let ellipsis_view = (local): Node.t =>
    div(
      ~attrs=[
        Attr.classes(["ellipsis"]),
        Attr.on_double_click(_ => {local(ToggleShowAllVals(0))}),
      ],
      [text("⋯")],
    );

  let nav_bar_view = (num_total: int, local) => {
    let nav_arrow = (cond: bool, offset: int): Node.t =>
      Node.div(
        ~attrs=[
          Attr.classes(["nav-arrow"] @ (cond ? ["disabled"] : [])),
          Attr.on_click(_ => local(MoveCursor(offset))),
        ],
        [],
      );
    let show_left = num_total < Window.max_closures();
    let show_right = num_total < Window.max_closures();
    div(
      ~attrs=[Attr.classes(["nav-bar"])],
      [nav_arrow(show_left, 1), nav_arrow(show_right, -1)],
    );
  };

  let equals_view =
    div(~attrs=[Attr.classes(["live-equals"])], [text("≡")]);

  let offside_view =
      (
        ~common,
        id,
        statics,
        dynamics,
        local: action(_) => Ui_effect.t(unit),
      ) =>
    Node.div(
      ~attrs=[Attr.classes(["live-offside"])],
      switch (dynamics) {
      | Some(closures) =>
        let num_total = Closures.total(statics, dynamics);
        let closures = Closures.select_frames(id, statics, closures);
        let (num_shown, groups) = Closures.collate(closures);
        let is_cut_off = num_shown != num_total && num_shown > 0;
        let extras = [nav_bar_view(num_total, local), ellipsis_view(local)];
        (num_shown > 0 ? [equals_view] : [])
        @ closure_group_view(~common, statics, local, groups)
        @ (is_cut_off ? extras : []);
      | _ => []
      },
    );

  let num_closures_view = (statics, dynamics) => {
    let num_closures = Closures.total(statics, dynamics);
    let description =
      num_closures < 1000 ? string_of_int(num_closures) : "1k+";
    div(
      ~attrs=[
        Attr.title(string_of_int(num_closures)),
        Attr.classes(["num-closures"]),
      ],
      [text(description)],
    );
  };

  let pin_view = statics =>
    DynCursor.show_pin(statics)
      ? [div(~attrs=[Attr.classes(["pin"])], [])] : [];

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let move_cursor = (statics, dynamics, offset: int): unit =>
    switch (dynamics) {
    | Some(closures) =>
      let closures = Closures.filter_frames_by_pin(statics, closures);
      let cursor_idx = DynCursor.first_index_of_interest(statics, closures);
      switch (cursor_idx) {
      /* Cursor would be outside window, reset to next visible closure */
      | Some(idx) =>
        let next_idx_maybe = idx - offset;
        if (next_idx_maybe >= 0 && next_idx_maybe < List.length(closures)) {
          DynCursor.capture_cursor(List.nth(closures, next_idx_maybe));
        };
      | _ => ()
      };
    | None => ()
    };

  let round_up = (len_of_exp, closure: closure): unit => {
    let cur =
      len_of_exp(
        abbreviated_exp(ClosureLength.get(closure), closure.value),
      );
    let goal = cur + 1;
    let max_len = len_of_exp(DHExp.strip_casts(closure.value));
    let rec find_target = (target: int): int => {
      let attempt_len = len_of_exp(abbreviated_exp(target, closure.value));
      if (attempt_len < goal && target <= max_len) {
        find_target(target + 1);
      } else {
        target;
      };
    };
    ClosureLength.set(closure.closure_id, find_target(goal));
  };

  let round_down = (len_of_exp, closure: closure): unit => {
    let cur =
      len_of_exp(
        abbreviated_exp(ClosureLength.get(closure), closure.value),
      );
    let goal = cur - 1;
    let rec find_target = (target: int): int => {
      let attempt_len = len_of_exp(abbreviated_exp(target, closure.value));
      if (attempt_len > goal && target > 0) {
        find_target(target - 1);
      } else {
        target;
      };
    };
    ClosureLength.set(closure.closure_id, find_target(goal));
  };

  let indicated_closure = (statics, dynamics): option(closure) =>
    OptUtil.and_then(DynCursor.first_cursor_closure(statics), dynamics);

  let key_handler = (len_of_exp, local, id, statics, dynamics, _, evt) => {
    open Effect;
    /* PLAN: inter-probe navigation
        ultimately need to be able to issue a parent action to move to and focus on
       another projector. for now, should be able to use the Project(Focus(id)) action
       to do both in one; will need to rethink when we want to /create/ probes as well.
       the probe that we want to move to is going to depend on the closure cursor, but
       also maybe the row of the closure we're on. alternatively, can maybe avoid
       row based logic by using closure creation time instead. In any case, want a function
       that takes the closure cursor and emits a new closure cursor and the id of a
       probe to jump to. Not sure this is the best approach at all, but for now maybe
       we could add all probe data to a common mutable structure in this module, when
       projectorview.all is called, and use this to calculate the probe id to jump to.
       like basically we're going to treat this mutable cache as a db, and do certain
       queries. specifically, return all probe_ids that have a closure with equal
       closure cursor to current, and take the one with the timestamp closet to but
       before/after the current closure cursor closure timestamp. */
    let key = Key.mk(KeyDown, evt);

    switch (key.key) {
    | D("Escape") when key.shift == Down =>
      JsUtil.get_elem_by_id(Id.cls(id))##blur;
      DynCursor.reset();
      Window.reset();
      ClosureLength.reset();
      local(NoOp);
    | D("Escape") =>
      JsUtil.get_elem_by_id(Id.cls(id))##blur;
      Ignore;
    | D("ArrowRight") when key.shift == Down =>
      switch (indicated_closure(statics, dynamics)) {
      | Some(closure) => round_up(len_of_exp, closure)
      | None => ()
      };
      Many([local(NoOp), Stop_propagation, Prevent_default]);
    | D("ArrowLeft") when key.shift == Down =>
      switch (indicated_closure(statics, dynamics)) {
      | Some(closure) => round_down(len_of_exp, closure)
      | None => ()
      };
      Many([local(NoOp), Stop_propagation, Prevent_default]);
    | D("ArrowRight") =>
      move_cursor(statics, dynamics, -1);
      // hack: Prevent_default below stops aggressive horizontal scroll
      // noop to trigger redraw
      Many([local(NoOp), Stop_propagation, Prevent_default]);
    | D("ArrowLeft") =>
      move_cursor(statics, dynamics, 1);
      Many([local(NoOp), Stop_propagation, Prevent_default]);
    | D(" ") =>
      Window.toggle_mode();
      Many([local(NoOp), Stop_propagation, Prevent_default]); // trigger redraw
    | _ => Many([Stop_propagation])
    };
  };

  let view =
      (
        ~common: ProjectorInterface.common,
        local: action(_) => Ui_effect.t(unit),
        parent: external_action => Ui_effect.t(unit),
        id,
        statics,
        dynamics,
        m: model(_),
      )
      : Node.t =>
    div(
      ~attrs=[
        Attr.id(Id.cls(id)),
        Attr.tabindex(0),
        Attr.on_keydown(
          key_handler(len_of_exp, local, id, statics, dynamics, parent),
        ),
        Attr.classes(
          ["main"]
          @ (Option.is_some(cur_ap(statics)) ? ["ap"] : [])
          @ (DynCursor.is_pinned(statics, dynamics) ? ["pinned"] : []),
        ),
        Attr.on_double_click(_ => local(PinAp)),
        Attr.on_pointerdown(_ => {
          /* Select a default cell if one is not already selected */
          DynCursor.probe_default(statics, dynamics);
          Effect.Ignore;
        }),
        Attr.on_pointerup(_ => {
          JsUtil.get_elem_by_id(Id.cls(id))##blur;
          Effect.Ignore;
        }),
        Attr.on_mouseenter(_ => {
          show_purps := true;
          local(NoOp);
        }),
        Attr.on_mouseleave(_ => {
          show_purps := false;
          local(NoOp);
        }),
      ],
      //TODO(andrew): accurate sort, abbreviate, styling
      [
        Editor.View.view(
          ~font_metrics=common.font_metrics,
          ~secondary_icons=common.secondary_icons,
          ~sort=Any,
          m.ed,
        ),
        icon,
      ],
    );

  let overlay_view = (statics, dynamics): Node.t =>
    div(
      ~attrs=[
        Attr.classes(
          ["overlay"]
          @ (Option.is_some(cur_ap(statics)) ? ["ap"] : [])
          @ (DynCursor.is_pinned(statics, dynamics) ? ["pinned"] : []),
        ),
      ],
      [num_closures_view(statics, dynamics)] @ pin_view(statics),
    );

  let mk = (term: Term.Any.t, ed) => {
    OptUtil.Syntax.(
      switch (term) {
      | Exp(_)
      | Pat(_)
      | Any(_) =>
        let+ ed = ed();
        {ed: ed};
      | _ => None
      }
    );
  };

  let dynamics = true;

  let placeholder = (~common as _, ~id as _, model) =>
    ProjectorShape.inline(2 + Editor.View.get_dimensions(model.ed).row);

  let update = (~common, ~sort as _, ~id, model, action) => {
    switch (action) {
    | ChangeLength(id, len) => ClosureLength.set(id, len)
    | ToggleShowAllVals(_) => Window.toggle_mode()
    | MoveCursor(offset) =>
      let statics = Statics.Map.lookup(id, common.statics.info_map);
      let dynamics = Dynamics.Map.lookup(id, common.dynamics);
      move_cursor(statics, dynamics, offset);
    | PinAp =>
      let statics = Statics.Map.lookup(id, common.statics.info_map);
      let dynamics = Dynamics.Map.lookup(id, common.dynamics);
      DynCursor.toggle_pinned_call(statics, dynamics);
    | NoOp => ()
    };
    model;
  };

  let mk_term = (~sort, ~prev, {ed}) => {
    let (ed, term: Calc.t(Any.t)) = Editor.Update.make_term(~sort, ed);
    let term =
      prev
      |> {
        open Calc.Syntax;
        let.calc term = term;
        switch (term) {
        | Exp(term) => (
            Exp({
              annotation: {
                ids: [Id.mk()],
              },
              term: Probe(term, Probe.empty),
            }): Any.t
          )
        | Pat(term) =>
          Pat({
            annotation: {
              ids: [Id.mk()],
            },
            term: Probe(term, Probe.empty),
          })
        | a => a
        };
      };
    ({ed: ed}, term);
  };

  let calculate = (~common, {ed}) => {
    ed: Editor.Update.calculate(~common, ed),
  };

  let get_cursor_info = ProjectorInterface.Defaults.get_cursor_info;

  let view =
      (~common, ~inject, ~escape, ~take_focus as _, ~focus as _, ~id, model) => {
    let statics = Statics.Map.lookup(id, common.statics.info_map);
    let dynamics = Dynamics.Map.lookup(id, common.dynamics);

    View.{
      inline: view(~common, inject, escape, id, statics, dynamics, model),
      overlay: Some(overlay_view(statics, dynamics)),
      offside: Some(offside_view(~common, id, statics, dynamics, inject)),
      enter_left: None,
      enter_right: None,
    };
  };
};
