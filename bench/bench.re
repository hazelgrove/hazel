/* Performance benchmarks for Hazel core pipeline.
   Run: dune build bench && node _build/default/bench/bench.bc.js */

open Language;

let now_ms = Util.TimeUtil.now_ms;

type bench_result = {
  name: string,
  parse_ms: float,
  statics_ms: float,
  elab_ms: float,
  eval_ms: float,
  post_eval_statics_ms: float,
  post_eval_elab_ms: float,
  exp_to_segment_ms: float,
  /* statics prof */
  statics_meet_calls: int,
  statics_meet_sum_calls: int,
  statics_meet_sum_ms: float,
  statics_rec_rec: int,
  statics_sum_from_rec: int,
  statics_var_eq: int,
  /* elab prof */
  elab_meet_calls: int,
  elab_meet_sum_calls: int,
  elab_meet_sum_ms: float,
  elab_rec_rec: int,
  elab_sum_from_rec: int,
  elab_var_eq: int,
  elab_norm_calls: int,
  elab_norm_ms: float,
  /* elab breakdown */
  elab_type_ms: float,
  elab_match_synswitch_ms: float,
  elab_all_ids_temp_ms: float,
  elab_fix_typ_ids_ms: float,
  /* post-eval statics prof */
  post_meet_calls: int,
  post_meet_sum_calls: int,
  post_meet_sum_ms: float,
  post_rec_rec: int,
  post_sum_from_rec: int,
  post_var_eq: int,
  /* new diagnostic counters */
  statics_phys_eq: int,
  statics_var_expand: int,
  statics_unknown: int,
  post_phys_eq: int,
  post_var_expand: int,
  post_unknown: int,
};

let bench = (~iterations=3, name: string, program: string): bench_result => {
  let parse_start = now_ms();
  let term =
    switch (Haz3lcore.Parser.to_term(program)) {
    | Some(e) => e
    | None => failwith("Failed to parse: " ++ name)
    };
  let parse_ms = now_ms() -. parse_start;

  /* Warm up */
  Typ.reset_normalize_stats();
  let info_map =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let (elaborated, _ty) = Elaborator.elaborate(info_map, term);
  let _ = Evaluator.evaluate(~env=Builtins.env_init, elaborated);

  /* Time statics with meet stats */
  Typ.reset_meet_stats();
  let start = now_ms();
  for (_ in 1 to iterations) {
    Typ.reset_normalize_stats();
    Typ.reset_meet_stats();
    let _ = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    ();
  };
  let statics_ms = (now_ms() -. start) /. float_of_int(iterations);
  let statics_meet_calls = Typ.meet_calls^;
  let statics_meet_sum_calls = Typ.meet_sum_calls^;
  let statics_meet_sum_ms = Typ.meet_sum_time_ms^;
  let statics_rec_rec = Typ.meet_rec_rec^;
  let statics_sum_from_rec = Typ.meet_sum_from_rec^;
  let statics_var_eq = Typ.meet_var_eq^;
  let statics_phys_eq = Typ.meet_phys_eq^;
  let statics_var_expand = Typ.meet_var_expand^;
  let statics_unknown = Typ.meet_unknown^;

  /* Time elaboration with meet stats */
  Typ.reset_meet_stats();
  Typ.reset_normalize_stats();
  Elaborator.reset_elab_stats();
  let start = now_ms();
  let elab_result = ref(elaborated);
  for (_ in 1 to iterations) {
    Typ.reset_meet_stats();
    Typ.reset_normalize_stats();
    Elaborator.reset_elab_stats();
    let (d, _) = Elaborator.elaborate(info_map, term);
    elab_result := d;
    ();
  };
  let elab_ms = (now_ms() -. start) /. float_of_int(iterations);
  let elab_meet_calls = Typ.meet_calls^;
  let elab_meet_sum_calls = Typ.meet_sum_calls^;
  let elab_meet_sum_ms = Typ.meet_sum_time_ms^;
  let elab_norm_calls = Typ.normalize_calls^;
  let elab_norm_ms = Typ.normalize_total_ms^;
  let elab_rec_rec = Typ.meet_rec_rec^;
  let elab_sum_from_rec = Typ.meet_sum_from_rec^;
  let elab_var_eq = Typ.meet_var_eq^;
  let elab_type_ms = Elaborator.elab_type_time^;
  let elab_match_synswitch_ms = Elaborator.match_synswitch_time^;
  let elab_all_ids_temp_ms = Elaborator.all_ids_temp_time^;

  /* Time fix_typ_ids separately */
  let fix_start = now_ms();
  for (_ in 1 to iterations) {
    let _ = Elaborator.fix_typ_ids(elab_result^);
    ();
  };
  let elab_fix_typ_ids_ms =
    (now_ms() -. fix_start) /. float_of_int(iterations);

  let eval_result = ref(elaborated);
  let start = now_ms();
  for (_ in 1 to iterations) {
    let (result, _state) =
      Evaluator.evaluate(~env=Builtins.env_init, elaborated);
    eval_result := result;
  };
  let eval_ms = (now_ms() -. start) /. float_of_int(iterations);

  /* Diagnostic: count expression shape for source and eval result */
  let evaluated_exp = eval_result^;
  let count_shape = (label: string, exp: Exp.t) => {
    let total_exp = ref(0);
    let total_typ = ref(0);
    let closures = ref(0);
    let constructors = ref(0);
    let aps = ref(0);
    let funs = ref(0);
    let ascs = ref(0);
    let lets = ref(0);
    let tuples = ref(0);
    let lists = ref(0);
    let atoms = ref(0);
    let vars = ref(0);
    let ifs = ref(0);
    let matches = ref(0);
    let other_exp = ref(0);
    let asc_var = ref(0);
    let asc_rec = ref(0);
    let asc_arrow = ref(0);
    let asc_list = ref(0);
    let asc_prod = ref(0);
    let asc_sum = ref(0);
    let asc_atom = ref(0);
    let asc_unknown = ref(0);
    let asc_other = ref(0);
    let asc_has_rec = ref(0);
    let ctr_var = ref(0);
    let ctr_arrow = ref(0);
    let ctr_has_rec = ref(0);
    /* Check if a type contains Rec anywhere in its structure */
    let rec has_rec = (ty: Typ.t): bool => {
      switch (Typ.term_of(ty)) {
      | Rec(_, _) => true
      | Arrow(t1, t2) => has_rec(t1) || has_rec(t2)
      | List(t) => has_rec(t)
      | Prod(ts) => List.exists(has_rec, ts)
      | Sum(ctrs) =>
        List.exists(
          fun
          | ConstructorMap.Variant(_, _, Some(t)) => has_rec(t)
          | _ => false,
          ctrs,
        )
      | TupLabel(_, t) => has_rec(t)
      | Parens(t) => has_rec(t)
      | _ => false
      };
    };
    let _: Exp.t = {
      let f_any:
        'a.
        (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) =>
        IdTagged.t('a)
       =
        (continue, node) => continue(node);
      let f_exp = (continue, node: Exp.t) => {
        incr(total_exp);
        switch (node.term) {
        | Closure(_, _) => incr(closures)
        | Constructor(_, _) =>
          incr(constructors);
          switch (node.term) {
          | Constructor(_, Some(Some(ty))) =>
            switch (Typ.term_of(ty)) {
            | Var(_) => incr(ctr_var)
            | Arrow(_, _) => incr(ctr_arrow)
            | _ => ()
            };
            if (has_rec(ty)) {
              incr(ctr_has_rec);
            };
          | _ => ()
          };
        | Ap(_, _, _) => incr(aps)
        | Fun(_, _, _, _) => incr(funs)
        | Asc(_, ty) =>
          incr(ascs);
          switch (Typ.term_of(ty)) {
          | Var(_) => incr(asc_var)
          | Rec(_) => incr(asc_rec)
          | Arrow(_) => incr(asc_arrow)
          | List(_) => incr(asc_list)
          | Prod(_) => incr(asc_prod)
          | Sum(_) => incr(asc_sum)
          | Atom(_) => incr(asc_atom)
          | Unknown(_) => incr(asc_unknown)
          | _ => incr(asc_other)
          };
          if (has_rec(ty)) {
            incr(asc_has_rec);
          };
        | Let(_, _, _) => incr(lets)
        | Tuple(_) => incr(tuples)
        | ListLit(_) => incr(lists)
        | Atom(_) => incr(atoms)
        | Var(_) => incr(vars)
        | If(_, _, _) => incr(ifs)
        | Match(_, _) => incr(matches)
        | _ => incr(other_exp)
        };
        continue(node);
      };
      let f_typ = (continue, node: Typ.t) => {
        incr(total_typ);
        continue(node);
      };
      Exp.map_term(
        ~f_exp,
        ~f_pat=f_any,
        ~f_typ,
        ~f_tpat=f_any,
        ~f_rul=f_any,
        exp,
      );
    };
    Printf.printf(
      "[SHAPE] %-20s  %s: exp=%d typ=%d | closure=%d ctr=%d(var=%d arrow=%d has_rec=%d) ap=%d fun=%d asc=%d(var=%d rec=%d arrow=%d list=%d prod=%d sum=%d atom=%d unk=%d other=%d has_rec=%d) let=%d tuple=%d list=%d atom=%d var=%d if=%d match=%d other=%d\n",
      name,
      label,
      total_exp^,
      total_typ^,
      closures^,
      constructors^,
      ctr_var^,
      ctr_arrow^,
      ctr_has_rec^,
      aps^,
      funs^,
      ascs^,
      asc_var^,
      asc_rec^,
      asc_arrow^,
      asc_list^,
      asc_prod^,
      asc_sum^,
      asc_atom^,
      asc_unknown^,
      asc_other^,
      asc_has_rec^,
      lets^,
      tuples^,
      lists^,
      atoms^,
      vars^,
      ifs^,
      matches^,
      other_exp^,
    );
  };
  count_shape("src", term);
  count_shape("elab", elab_result^);
  count_shape("eval", evaluated_exp);

  /* Time post-eval statics: run statics on the evaluated result */
  Typ.reset_meet_stats();
  Typ.reset_normalize_stats();
  let start = now_ms();
  for (_ in 1 to iterations) {
    Typ.reset_meet_stats();
    Typ.reset_normalize_stats();
    let _ =
      Statics.mk(
        CoreSettings.on,
        Builtins.ctx_init(Some(Int)),
        evaluated_exp,
      );
    ();
  };
  let post_eval_statics_ms = (now_ms() -. start) /. float_of_int(iterations);
  let post_meet_calls = Typ.meet_calls^;
  let post_meet_sum_calls = Typ.meet_sum_calls^;
  let post_meet_sum_ms = Typ.meet_sum_time_ms^;
  let post_rec_rec = Typ.meet_rec_rec^;
  let post_sum_from_rec = Typ.meet_sum_from_rec^;
  let post_var_eq = Typ.meet_var_eq^;
  let post_phys_eq = Typ.meet_phys_eq^;
  let post_var_expand = Typ.meet_var_expand^;
  let post_unknown = Typ.meet_unknown^;

  /* Time post-eval elaboration: run Elaborator.uexp_elab on the evaluated result
     using the post-eval info_map */
  let post_info_map =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      evaluated_exp,
    );
  let start = now_ms();
  for (_ in 1 to iterations) {
    let _ = Elaborator.uexp_elab(post_info_map, evaluated_exp);
    ();
  };
  let post_eval_elab_ms = (now_ms() -. start) /. float_of_int(iterations);

  /* Time ExpToSegment: convert evaluated result to displayable segments */
  let e2s_settings =
    Haz3lcore.ExpToSegment.Settings.of_core(~inline=false, CoreSettings.on);
  let start = now_ms();
  for (_ in 1 to iterations) {
    let _ =
      Haz3lcore.ExpToSegment.exp_to_segment(
        ~settings=e2s_settings,
        evaluated_exp,
      );
    ();
  };
  let exp_to_segment_ms = (now_ms() -. start) /. float_of_int(iterations);

  {
    name,
    parse_ms,
    statics_ms,
    elab_ms,
    eval_ms,
    post_eval_statics_ms,
    post_eval_elab_ms,
    exp_to_segment_ms,
    statics_meet_calls,
    statics_meet_sum_calls,
    statics_meet_sum_ms,
    statics_rec_rec,
    statics_sum_from_rec,
    statics_var_eq,
    elab_meet_calls,
    elab_meet_sum_calls,
    elab_meet_sum_ms,
    elab_rec_rec,
    elab_sum_from_rec,
    elab_var_eq,
    elab_norm_calls,
    elab_norm_ms,
    elab_type_ms,
    elab_match_synswitch_ms,
    elab_all_ids_temp_ms,
    elab_fix_typ_ids_ms,
    post_meet_calls,
    post_meet_sum_calls,
    post_meet_sum_ms,
    post_rec_rec,
    post_sum_from_rec,
    post_var_eq,
    statics_phys_eq,
    statics_var_expand,
    statics_unknown,
    post_phys_eq,
    post_var_expand,
    post_unknown,
  };
};

let print_result = (r: bench_result): unit => {
  Printf.printf(
    "[BENCH] %-20s  parse:%7.1fms  statics:%7.1fms  elab:%7.1fms  eval:%7.1fms  post_statics:%7.1fms  post_elab:%7.1fms  e2s:%7.1fms  total:%7.1fms\n%!",
    r.name,
    r.parse_ms,
    r.statics_ms,
    r.elab_ms,
    r.eval_ms,
    r.post_eval_statics_ms,
    r.post_eval_elab_ms,
    r.exp_to_segment_ms,
    r.parse_ms
    +. r.statics_ms
    +. r.elab_ms
    +. r.eval_ms
    +. r.post_eval_statics_ms
    +. r.post_eval_elab_ms
    +. r.exp_to_segment_ms,
  );
  Printf.printf(
    "[PROF]  %-20s  statics: meet=%d sum=%d(from_rec=%d) var_eq=%d rec_rec=%d sum_ms=%.1f phys_eq=%d var_exp=%d unk=%d | elab: meet=%d sum=%d var_eq=%d rec_rec=%d\n%!",
    r.name,
    r.statics_meet_calls,
    r.statics_meet_sum_calls,
    r.statics_sum_from_rec,
    r.statics_var_eq,
    r.statics_rec_rec,
    r.statics_meet_sum_ms,
    r.statics_phys_eq,
    r.statics_var_expand,
    r.statics_unknown,
    r.elab_meet_calls,
    r.elab_meet_sum_calls,
    r.elab_var_eq,
    r.elab_rec_rec + r.elab_sum_from_rec + int_of_float(r.elab_meet_sum_ms) /* use to avoid unused field */
  );
  let other_ms = r.elab_ms -. r.elab_type_ms;
  Printf.printf(
    "[ELAB]  %-20s  elab_type:%.1fms (norm:%d/%.1f synswitch:%.1f all_ids_temp:%.1f) fix_typ_ids:%.1f other:%.1f\n%!",
    r.name,
    r.elab_type_ms,
    r.elab_norm_calls,
    r.elab_norm_ms,
    r.elab_match_synswitch_ms,
    r.elab_all_ids_temp_ms,
    r.elab_fix_typ_ids_ms,
    other_ms,
  );
  Printf.printf(
    "[POST]  %-20s  post_statics: meet=%d sum=%d(from_rec=%d) var_eq=%d rec_rec=%d sum_ms=%.1f phys_eq=%d var_exp=%d unk=%d\n%!",
    r.name,
    r.post_meet_calls,
    r.post_meet_sum_calls,
    r.post_sum_from_rec,
    r.post_var_eq,
    r.post_rec_rec,
    r.post_meet_sum_ms,
    r.post_phys_eq,
    r.post_var_expand,
    r.post_unknown,
  );
  Printf.printf(
    "[PELAB] %-20s  post_eval_elab:%7.1fms\n%!",
    r.name,
    r.post_eval_elab_ms,
  );
  Printf.printf(
    "[E2S]   %-20s  exp_to_segment:%7.1fms\n%!",
    r.name,
    r.exp_to_segment_ms,
  );
};

let simple_let = {|let x = 1 in let y = 2 in x + y|};

let counter = {|
let update = fun (msg, model) ->
  case msg
  | Increment -> model + 1
  | Decrement -> model - 1
  end
in let view = fun n ->
  Div([], [
    H2([], [Text("Counter")]),
    Div([], [Int(n)]),
    Div([], [
      Button([OnClick(Decrement)], [Text("-")]),
      Button([OnClick(Increment)], [Text("+")])])])
in (0, update, view, fun _model -> SubNone)
|};

let fibonacci = {|
let fib = fun n ->
  if n < 2 then n
  else fib(n - 1) + fib(n - 2)
in fib(8)
|};

let mvu_counter = {|
let update : (Int, Int) -> Int = fun (msg, model) -> model + msg in
let view : Int -> HTML = fun model -> Div(
  [Class("counter"), Style([("text-align", "center"), ("padding", "20px")])],
  [
    H2([], [Text("MVU Counter")]),
    Div(
      [Style([("font-size", "48px"), ("margin", "20px")])],
      [Int(model)]
    ),
    Div(
      [],
      [
        Button([
          OnClick(-1),
          Style([("font-size", "24px"), ("margin", "10px"), ("padding", "10px 20px")])
          ],
          [Text("-")]
          ),
        Button([
              OnClick(1),
              Style([("font-size", "24px"), ("margin", "10px"), ("padding", "10px 20px")])
            ],
            [Text("+")]
          )
        ]
      ),
      P(
        [Style([("color", "#666"), ("font-size", "12px")])],
        [Text("Click buttons to change count")]
      )
    ]
  )
in
let subs : Int -> Sub = fun _model -> SubNone in
(0, update, view, subs)
|};

let keyboard_game = {|
let max : (Int, Int) -> Int = fun (a, b) -> if a > b then a else b in
let min : (Int, Int) -> Int = fun (a, b) -> if a < b then a else b in
let step : Int = 20 in
let update = fun (msg, model) ->
  let x = fst(model) in
  let y = snd(model) in
  if msg == "ArrowUp" then
    (x, max(0, y - step))
  else if msg == "ArrowDown" then
    (x, min(360, y + step))
  else if msg == "ArrowLeft" then
    (max(0, x - step), y)
  else if msg == "ArrowRight" then
    (min(360, x + step), y)
  else
    model
in
let view : (Int, Int) -> HTML = fun model ->
  let x = fst(model) in
  let y = snd(model) in
  Div(
    [
      Id("game"),
      Style([
        ("width", "400px"),
        ("height", "400px"),
        ("position", "relative"),
        ("background", "#f0f0f0"),
        ("border", "2px solid #333"),
        ("margin", "20px auto")
      ])
    ],
    [
      Div(
        [Style([("text-align", "center"), ("padding", "10px"), ("color", "#666")])],
        [Text("Use arrow keys to move the box")]
      ),
      Div(
        [Style([
          ("width", "40px"),
          ("height", "40px"),
          ("background", "#4CAF50"),
          ("position", "absolute"),
          ("left", string_of_int(x) ++ "px"),
          ("top", string_of_int(y) ++ "px"),
          ("border-radius", "4px"),
          ("transition", "all 0.1s ease")
        ])],
        []
      ),
      Div(
        [Style([("position", "absolute"), ("bottom", "10px"), ("left", "10px"), ("color", "#333")])],
        [Text("Position: (" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")")]
      )
    ]
  )
in
let subs : (Int, Int) -> Sub = fun _model ->
  OnDocumentKeyDown(fun (key, _code, _ctrl, _shift, _alt, _meta) -> key)
in
((180, 180), update, view, subs)
|};

let animation = {|
let gravity : Float = 0.3 in
let bounce : Float = 0.8 in
let friction : Float = 0.99 in
let update = fun (msg, model) ->
  let x = fst(model) in
  let y = fst(snd(model)) in
  let vx = fst(snd(snd(model))) in
  let vy = snd(snd(snd(model))) in
  let new_vy = vy +. gravity in
  let new_x = x +. vx in
  let new_y = y +. new_vy in
  let final_x =
    if new_x <. 0.0 then 0.0
    else if new_x >. 370.0 then 370.0
    else new_x
  in
  let final_vx =
    if new_x <. 0.0 then 0.0 -. vx *. bounce
    else if new_x >. 370.0 then 0.0 -. vx *. bounce
    else vx *. friction
  in
  let final_y =
    if new_y >. 230.0 then 230.0
    else new_y
  in
  let final_vy =
    if new_y >. 230.0 then 0.0 -. new_vy *. bounce
    else new_vy
  in
  (final_x, (final_y, (final_vx, final_vy)))
in
let view = fun model ->
  let x = fst(model) in
  let y = fst(snd(model)) in
  let vx = fst(snd(snd(model))) in
  let vy = snd(snd(snd(model))) in
  Div(
    [
      Id("animation"),
      Style([
        ("width", "400px"),
        ("height", "300px"),
        ("position", "relative"),
        ("background", "linear-gradient(to bottom, #87CEEB, #4169E1)"),
        ("border", "2px solid #333"),
        ("margin", "20px auto"),
        ("overflow", "hidden")
      ])
    ],
    [
      Div(
        [Style([
          ("width", "30px"),
          ("height", "30px"),
          ("background", "radial-gradient(circle at 30% 30%, #ff6666, #cc0000)"),
          ("position", "absolute"),
          ("left", string_of_float(x) ++ "px"),
          ("top", string_of_float(y) ++ "px"),
          ("border-radius", "50%"),
          ("box-shadow", "2px 2px 8px rgba(0,0,0,0.3)")
        ])],
        []
      ),
      Div(
        [Style([
          ("position", "absolute"),
          ("bottom", "0"),
          ("left", "0"),
          ("right", "0"),
          ("height", "40px"),
          ("background", "#228B22")
        ])],
        []
      ),
      Div(
        [Style([("position", "absolute"), ("top", "10px"), ("left", "10px"), ("color", "white"), ("font-size", "12px")])],
        [Text("Velocity: (" ++ string_of_float(vx) ++ ", " ++ string_of_float(vy) ++ ")")]
      )
    ]
  )
in
let subs = fun _model ->
  AnimationFrame(fun timestamp -> timestamp)
in
((50.0, (50.0, (3.0, 0.0))), update, view, subs)
|};

let full_app = {|
let update = fun (msg, model) ->
  let active_tab = fst(model) in
  let form_name = fst(snd(model)) in
  let form_message = fst(snd(snd(model))) in
  let saved_message = snd(snd(snd(model))) in
  let tag = fst(msg) in
  let value = snd(msg) in
  if tag == "tab" then
    (value, (form_name, (form_message, saved_message)))
  else if tag == "name" then
    (active_tab, (value, (form_message, saved_message)))
  else if tag == "message" then
    (active_tab, (form_name, (value, saved_message)))
  else if tag == "submit" then
    let new_saved = form_name ++ ": " ++ form_message in
    ((active_tab, ("", ("", new_saved))),
     CmdBatch([Log("Form submitted: " ++ new_saved), Focus("name-input")]))
  else
    model
in
let view : (String, (String, (String, String))) -> HTML = fun model ->
  let active_tab = fst(model) in
  let form_name = fst(snd(model)) in
  let form_message = fst(snd(snd(model))) in
  let saved_message = snd(snd(snd(model))) in
  let tabs : [String] = ["Home", "Form", "About"] in
  Div(
    [
      Id("app"),
      Style([
        ("max-width", "500px"),
        ("margin", "20px auto"),
        ("font-family", "Arial, sans-serif"),
        ("border", "1px solid #ddd"),
        ("border-radius", "8px"),
        ("overflow", "hidden")
      ])
    ],
    [
      Div(
        [Style([("display", "flex"), ("background", "#333")])],
        map(fun tab ->
          Button(
            [
              OnClick(("tab", tab)),
              Style([
                ("flex", "1"),
                ("padding", "15px"),
                ("border", "none"),
                ("background", if tab == active_tab then "#4CAF50" else "#333"),
                ("color", "white"),
                ("cursor", "pointer"),
                ("font-size", "14px")
              ])
            ],
            [Text(tab)]
          ),
          tabs
        )
      ),
      Div(
        [Style([("padding", "20px"), ("min-height", "200px")])],
        [
          if active_tab == "Home" then
            Div([], [
              H2([], [Text("Welcome to HazelHtml!")]),
              P([], [Text("This is a demo app showing various features:")]),
              Ul([], [
                Li([], [Text("Tab navigation")]),
                Li([], [Text("Form handling with OnInput")]),
                Li([], [Text("Commands (Focus, Log)")]),
                Li([], [Text("Responsive styling")])
              ]),
              if saved_message != "" then
                Div(
                  [Style([("background", "#e8f5e9"), ("padding", "10px"), ("margin-top", "20px"), ("border-radius", "4px")])],
                  [Text("Last saved: " ++ saved_message)]
                )
              else
                Div([], [])
            ])
          else if active_tab == "Form" then
            Div([], [
              H2([], [Text("Contact Form")]),
              Div(
                [Style([("margin-bottom", "15px")])],
                [
                  Label([Style([("display", "block"), ("margin-bottom", "5px")])], [Text("Name:")]),
                  Input([
                    Id("name-input"),
                    Type("text"),
                    Value(form_name),
                    Placeholder("Enter your name"),
                    OnInput(fun value -> ("name", value)),
                    Style([("width", "100%"), ("padding", "8px"), ("box-sizing", "border-box")])
                  ])
                ]
              ),
              Div(
                [Style([("margin-bottom", "15px")])],
                [
                  Label([Style([("display", "block"), ("margin-bottom", "5px")])], [Text("Message:")]),
                  TextArea(
                    [
                      Value(form_message),
                      Placeholder("Enter your message"),
                      OnInput(fun value -> ("message", value)),
                      Style([("width", "100%"), ("padding", "8px"), ("height", "100px"), ("box-sizing", "border-box")])
                    ],
                    form_message
                  )
                ]
              ),
              Button(
                [
                  OnClick(("submit", "")),
                  Style([
                    ("background", "#4CAF50"),
                    ("color", "white"),
                    ("padding", "10px 20px"),
                    ("border", "none"),
                    ("cursor", "pointer"),
                    ("border-radius", "4px")
                  ])
                ],
                [Text("Submit")]
              )
            ])
          else
            Div([], [
              H2([], [Text("About HazelHtml")]),
              P([], [Text("HazelHtml is a web app library for Hazel.")]),
              Ul([], [
                Li([], [Text("HTML element types (Div, Button, Input, etc.)")]),
                Li([], [Text("Attribute types (Class, Style, OnClick, etc.)")]),
                Li([], [Text("Command types (Focus, Delay, Log, etc.)")]),
                Li([], [Text("Subscription types (OnResize, Every, etc.)")])
              ])
            ])
        ]
      ),
      Div(
        [Style([("background", "#f5f5f5"), ("padding", "10px"), ("text-align", "center"), ("color", "#666"), ("font-size", "12px")])],
        [Text("Built with HazelHtml")]
      )
    ]
  )
in
let subs : (String, (String, (String, String))) -> Sub = fun _model -> SubNone in
(("Home", ("", ("", ""))), update, view, subs)
|};

let programs = [
  ("simple_let", simple_let),
  ("fibonacci", fibonacci),
  ("counter", counter),
  ("mvu_counter", mvu_counter),
  ("keyboard_game", keyboard_game),
  ("animation", animation),
  ("full_app", full_app),
];

let () = {
  Printf.printf("Hazel Benchmark Suite\n%!");
  Printf.printf("%s\n%!", String.make(100, '='));
  Printf.printf(
    "[BENCH] %-20s  %7s  %9s  %7s  %7s  %14s  %11s  %7s  %7s\n%!",
    "program",
    "parse",
    "statics",
    "elab",
    "eval",
    "post_statics",
    "post_elab",
    "e2s",
    "total",
  );
  Printf.printf("[BENCH] %s\n%!", String.make(75, '-'));
  List.iter(
    ((name, program)) => {
      let r = bench(name, program);
      print_result(r);
    },
    programs,
  );
  Printf.printf("\nDone.\n%!");
};
