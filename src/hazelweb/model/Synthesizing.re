open OptUtil.Syntax;

module HoleMap =
  Map.Make({
    type t = CursorPath.steps;
    let compare = CursorPath.compare_steps;
  });

type filled_holes =
  // need constructor to prevent type synonym cycle
  | F(HoleMap.t((UHExp.t, filled_holes)));

let filled_holes_of_sexp = _ =>
  failwith("Synthesizing.filled_holes_of_sexp todo");
let sexp_of_filled_holes = _ =>
  failwith("Synthesizing.sexp_of_filled_holes todo");

/**
 * Top-down zipper representing synthesis navigation
 */
type t = (CursorPath.steps, z)
and z =
  | Filling(ZList.t(UHExp.t, UHExp.t), Shmyth.h_constraints)
  | Filled(UHExp.t, filled_holes, t);

let t_of_sexp = _ => failwith("Synthesizing.t_of_sexp todo");
let sexp_of_t = _ => failwith("Synthesizing.sexp_of_t todo");

let rec erase = (z: z): option((UHExp.t, filled_holes)) =>
  switch (z) {
  | Filling(_) => None
  | Filled(e, filled_holes, (steps, z)) =>
    let filled_holes =
      switch (erase(z)) {
      | None => filled_holes
      | Some((e, f)) =>
        let F(filled_map) = filled_holes;
        F(HoleMap.add(steps, (e, f), filled_map));
      };
    Some((e, filled_holes));
  };

let scroll = (up: bool, (steps, z): t) => {
  let rec go = (z: z) =>
    switch (z) {
    | Filling((before, selected, after), constraints) =>
      if (up) {
        let+ (before, new_selected) = ListUtil.split_last_opt(before);
        Filling((before, new_selected, [selected, ...after]), constraints);
      } else {
        let+ (new_selected, after) = ListUtil.split_first_opt(after);
        Filling((before @ [selected], new_selected, after), constraints);
      }
    | Filled(e, filled_holes, (steps, z)) =>
      let+ z = go(z);
      Filled(e, filled_holes, (steps, z));
    };
  let+ z = go(z);
  (steps, z);
};

let mk = (u: MetaVar.t, e: UHExp.t): option(t) => {
  switch (Shmyth.solve(e, u)) {
  | None =>
    print_endline("synth error");
    None;
  | Some((es, constraints)) =>
    switch (ZList.split_at(0, es)) {
    | None =>
      print_endline("no synth results");
      None;
    | Some(zes) =>
      let holes = CursorPath_Exp.holes(e, [], []);
      let hole_steps =
        CursorPath_common.steps_to_hole(holes, u)
        |> OptUtil.get(() => failwith("hole not found"));
      Some((hole_steps, Filling(zes, constraints)));
    }
  };
};

let get_meta_var = (steps: CursorPath.steps, e: UHExp.t) => {
  CursorPath_Exp.holes(e, [], [])
  |> List.find((hole_info: CursorPath.hole_info) => hole_info.steps == steps)
  |> (
    fun
    | CursorPath.{sort: TypHole | PatHole(_), _} =>
      failwith("expected exp hole")
    | {sort: ExpHole(u, _), _} => u
  );
};

let rec sketch_of_filled_holes = (e: UHExp.t, F(map): filled_holes) => {
  map
  |> HoleMap.bindings
  |> List.map(((steps, (e', map))) => {
       let u = get_meta_var(steps, e);
       let e' = sketch_of_filled_holes(e', map);
       (u, e');
     })
  |> List.fold_left((e, (u, e')) => UHExp.fill_hole(u, e', e), e);
};
let rec mk_sketch = (e: UHExp.t, (steps, z): t): UHExp.t =>
  switch (z) {
  | Filling(_) => e
  | Filled(e', filled, synthesizing) =>
    let e' = sketch_of_filled_holes(mk_sketch(e', synthesizing), filled);
    let u = get_meta_var(steps, e);
    UHExp.fill_hole(u, e', e);
  };

let rec accept = (e: UHExp.t, (steps, z): t) => {
  let accepted =
    switch (z) {
    | Filling((_, selected, _), _) => selected
    | Filled(filled, filled_holes, synthesizing) =>
      sketch_of_filled_holes(accept(filled, synthesizing), filled_holes)
    };
  UHExp.fill_hole(get_meta_var(steps, e), accepted, e);
};

let mk_zholes = (steps: CursorPath.steps, e: UHExp.t): CursorPath.zhole_list => {
  let holes = CursorPath_Exp.holes(e, [], []);
  let (i, _) =
    holes
    |> List.mapi((i, hole) => (i, hole))
    |> List.find(((_, hole_info: CursorPath.hole_info)) =>
         hole_info.steps == steps
       );
  let (holes_before, selected, holes_after) =
    Option.get(ZList.split_at(i, holes));
  {holes_before, hole_selected: Some(selected), holes_after};
};

let rec move_to_first_hole =
        (~sketch, e: UHExp.t, F(filled_map): filled_holes): option(t) => {
  let holes = CursorPath_Exp.holes(e, [], []);
  // assuming all holes encountered here will be empty exp holes
  switch (holes) {
  | [{steps, sort: ExpHole(u, _)}, ..._] =>
    switch (HoleMap.find_opt(steps, filled_map)) {
    | None =>
      let+ (_, filling) = mk(u, sketch);
      (steps, filling);
    | Some((e, filled_holes)) => move_to_first_hole(~sketch, e, filled_holes)
    }
  | _ => None
  };
};

let rec move_to_last_hole =
        (~sketch, e: UHExp.t, F(filled_map): filled_holes): option(t) => {
  let holes = CursorPath_Exp.holes(e, [], []);
  // assuming all holes encountered here will be empty exp holes
  switch (List.rev(holes)) {
  | [{steps, sort: ExpHole(u, _)}, ..._] =>
    switch (HoleMap.find_opt(steps, filled_map)) {
    | None =>
      let+ (_, filling) = mk(u, sketch);
      (steps, filling);
    | Some((e, filled_holes)) => move_to_last_hole(~sketch, e, filled_holes)
    }
  | _ => None
  };
};

let move_to_prev_hole = (e: UHExp.t, synthesizing: t): option(t) => {
  let sketch = mk_sketch(e, synthesizing);
  let rec go = ((ss, z): t): option(t) =>
    switch (z) {
    | Filling(_) => None
    | Filled(e, F(filled_map) as filled, (steps, z) as synthesizing) =>
      switch (go(synthesizing)) {
      | Some(synthesizing) => Some((ss, Filled(e, filled, synthesizing)))
      | None =>
        let erased =
          switch (erase(z)) {
          | None => filled_map
          | Some(f) => HoleMap.add(steps, f, filled_map)
          };
        let zholes = mk_zholes(steps, e);
        let* (prev_u, prev_steps) =
          // assuming all holes encountered here will be empty exp holes
          switch (List.rev(zholes.holes_before)) {
          | [{steps, sort: ExpHole(u, _)}, ..._] => Some((u, steps))
          | _ => None
          };
        let synthesize_prev = () => {
          let+ (_, filling) = mk(prev_u, sketch);
          (prev_steps, filling);
        };
        switch (HoleMap.find_opt(prev_steps, erased)) {
        | None => synthesize_prev()
        | Some((_, F(filled_map))) when HoleMap.is_empty(filled_map) =>
          synthesize_prev()
        | Some((e, filled_holes)) =>
          move_to_last_hole(~sketch, e, filled_holes)
        };
      }
    };
  go(synthesizing);
};

let move_to_next_hole = (e: UHExp.t, synthesizing: t): option(t) => {
  let sketch = mk_sketch(e, synthesizing);
  let rec go = ((ss, z): t): option(t) =>
    switch (z) {
    | Filling(_) => None
    | Filled(e, F(filled_map) as filled, (steps, z) as synthesizing) =>
      switch (go(synthesizing)) {
      | Some(synthesizing) => Some((ss, Filled(e, filled, synthesizing)))
      | None =>
        let erased =
          switch (erase(z)) {
          | None => filled_map
          | Some(f) => HoleMap.add(steps, f, filled_map)
          };
        let zholes = mk_zholes(steps, e);
        let* (next_u, next_steps) =
          // assuming all holes encountered here will be empty exp holes
          switch (zholes.holes_after) {
          | [{steps, sort: ExpHole(u, _)}, ..._] => Some((u, steps))
          | _ => None
          };
        let synthesize_next = () => {
          let+ (_, filling) = mk(next_u, sketch);
          (next_steps, filling);
        };
        switch (HoleMap.find_opt(next_steps, erased)) {
        | None => synthesize_next()
        | Some((_, F(filled_map))) when HoleMap.is_empty(filled_map) =>
          synthesize_next()
        | Some((e, filled_holes)) =>
          move_to_first_hole(~sketch, e, filled_holes)
        };
      }
    };
  go(synthesizing);
};

let step_in = (e: UHExp.t, synthesizing: t): option(t) => {
  let mk_sketch = (u, e') =>
    mk_sketch(e, synthesizing) |> UHExp.fill_hole(u, e');
  let rec go = (e: UHExp.t, (ss, z): t) =>
    switch (z) {
    | Filling((_, selected, _), _) =>
      let sketch = mk_sketch(get_meta_var(ss, e), selected);
      let holes = CursorPath_Exp.holes(selected, [], []);
      switch (holes) {
      | [{steps, sort: ExpHole(u, _)}, ..._] =>
        let+ (_, filling) = mk(u, sketch);
        (ss, Filled(selected, F(HoleMap.empty), (steps, filling)));
      | _ => None
      };
    | Filled(e, filled_holes, synthesizing) =>
      let+ synthesizing = go(e, synthesizing);
      (ss, Filled(e, filled_holes, synthesizing));
    };
  go(e, synthesizing);
};

let step_out = (_e: UHExp.t, _synthesizing: t): option(t) =>
  failwith("Synthesizing.step_out todo");
// let move_out = (e: UHExp.t, synthesizing: t): option(t) => {
//   let rec go = (ee, (ss, z): t) =>
//     switch (z) {
//     | Filling(_) => None
//     | Filled(e, filled_holes, (steps, z)) =>
//       switch (z) {
//       | Filling(_) =>
//         let u = get_meta_var(ss, ee);

//       }
//     }
// }
