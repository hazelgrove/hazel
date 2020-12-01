open Sexplib.Std;
open OptUtil.Syntax;

module HoleMap = {
  include Map.Make({
    type t = CursorPath.steps;
    let compare = CursorPath.compare_steps;
  });

  let sexp_of_t = (sexp_of_v, map): Sexplib.Sexp.t =>
    bindings(map)
    |> sexp_of_list(((k, v)) =>
         Sexplib.Sexp.List([CursorPath.sexp_of_steps(k), sexp_of_v(v)])
       );
  let t_of_sexp = sexp => {
    failwith(
      "Synthesizing.HoleMap.t_of_sexp " ++ Sexplib.Sexp.to_string(sexp),
    );
  };
};

type filled_holes =
  // need constructor to prevent type synonym cycle
  | F(HoleMap.t((UHExp.t, filled_holes)));

let filled_holes_of_sexp = _ =>
  failwith("Synthesizing.filled_holes_of_sexp todo");
let rec sexp_of_filled_holes = (F(map): filled_holes) =>
  HoleMap.sexp_of_t(
    ((e, filled_holes)) =>
      Sexplib.Sexp.List([
        UHExp.sexp_of_t(e),
        sexp_of_filled_holes(filled_holes),
      ]),
    map,
  );

/**
 * Top-down zipper representing synthesis navigation
 */
[@deriving sexp]
type t = (CursorPath.steps, z)
and z =
  | Filling(ZList.t(UHExp.t, UHExp.t), Shmyth.h_constraints)
  | Filled(UHExp.t, filled_holes, t);

let rec erase = (z: z): (UHExp.t, filled_holes) =>
  switch (z) {
  | Filling((_, selected, _), _) => (selected, F(HoleMap.empty))
  | Filled(e, F(filled_map), (steps, z)) =>
    let filled_holes = F(HoleMap.add(steps, erase(z), filled_map));
    (e, filled_holes);
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
    let es = UHExp.Set.elements(UHExp.Set.of_list(es));
    let zes =
      Option.get(
        ZList.split_at(0, [UHExp.Block.wrap(EmptyHole(u)), ...es]),
      );
    let holes = CursorPath_Exp.holes(e, [], []);
    let hole_steps =
      CursorPath_common.steps_to_hole(holes, u)
      |> OptUtil.get(() =>
           failwith("hole " ++ string_of_int(u) ++ " not found")
         );
    Some((hole_steps, Filling(zes, constraints)));
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

let rec sketch_of_filled_holes =
        (
          ~preserve_hole: option(MetaVar.t)=?,
          e: UHExp.t,
          F(map): filled_holes,
        ) => {
  map
  |> HoleMap.bindings
  |> List.map(((steps, (e', map))) => {
       let u = get_meta_var(steps, e);
       let e' = sketch_of_filled_holes(e', map);
       (u, e');
     })
  |> List.fold_left(
       (e, (u, e')) =>
         switch (preserve_hole) {
         | Some(v) when v == u => e
         | _ => UHExp.fill_hole(u, e', e)
         },
       e,
     );
};
let rec mk_sketch = (e: UHExp.t, (steps, z): t): UHExp.t =>
  switch (z) {
  | Filling(_) => e
  | Filled(e', filled, synthesizing) =>
    let e' = sketch_of_filled_holes(mk_sketch(e', synthesizing), filled);
    let u = get_meta_var(steps, e);
    UHExp.fill_hole(u, e', e);
  };

type rev_sketch = list((UHExp.t, filled_holes, CursorPath.steps));
let sketch_of_rev_sketch = (~preserve_hole: option(MetaVar.t)=?, rev_sketch) => {
  let ((root_e, _, root_steps), ancestors) =
    ListUtil.split_first(List.rev(rev_sketch));
  let inner_sketch =
    List.fold_right(
      ((e, filled, steps), inner_sketch) => {
        let sketch_filled_holes =
          sketch_of_filled_holes(~preserve_hole?, e, filled);
        switch (inner_sketch) {
        | None => Some(sketch_filled_holes)
        | Some(inner_sketch) =>
          Some(
            UHExp.fill_hole(
              get_meta_var(steps, e),
              inner_sketch,
              sketch_filled_holes,
            ),
          )
        };
      },
      ancestors,
      None,
    );
  switch (inner_sketch) {
  | None => root_e
  | Some(sketch) =>
    UHExp.fill_hole(get_meta_var(root_steps, root_e), sketch, root_e)
  };
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
        (~rev_sketch, e: UHExp.t, F(filled_map): filled_holes): option(t) => {
  let holes = CursorPath_Exp.holes(e, [], []);
  // assuming all holes encountered here will be empty exp holes
  switch (holes) {
  | [{steps, sort: ExpHole(u, _)}, ..._] =>
    switch (HoleMap.find_opt(steps, filled_map)) {
    | None =>
      let+ (_, filling) = mk(u, sketch_of_rev_sketch(rev_sketch));
      (steps, filling);
    | Some((e, filled_holes)) =>
      move_to_first_hole(~rev_sketch, e, filled_holes)
    }
  | _ => None
  };
};

let rec move_to_last_hole =
        (~rev_sketch, e: UHExp.t, F(filled_map): filled_holes): option(t) => {
  let holes = CursorPath_Exp.holes(e, [], []);
  // assuming all holes encountered here will be empty exp holes
  switch (List.rev(holes)) {
  | [{steps, sort: ExpHole(u, _)}, ..._] =>
    switch (HoleMap.find_opt(steps, filled_map)) {
    | None =>
      let+ (_, filling) = mk(u, sketch_of_rev_sketch(rev_sketch));
      (steps, filling);
    | Some((e, filled_holes)) =>
      move_to_last_hole(~rev_sketch, e, filled_holes)
    }
  | _ => None
  };
};

let move_to_prev_hole =
    (e: UHExp.t, (steps, _) as synthesizing: t): option(t) => {
  let rec go = (~rev_sketch, (ss, z): t): option(t) =>
    switch (z) {
    | Filling(_) => None
    | Filled(e, F(filled_map) as filled, (steps, z) as synthesizing) =>
      let rev_sketch = [(e, filled, steps), ...rev_sketch];
      switch (go(~rev_sketch, synthesizing)) {
      | Some(synthesizing) => Some((ss, Filled(e, filled, synthesizing)))
      | None =>
        let erased = HoleMap.add(steps, erase(z), filled_map);
        let zholes = mk_zholes(steps, e);
        let* (prev_u, prev_steps) =
          // assuming all holes encountered here will be empty exp holes
          switch (List.rev(zholes.holes_before)) {
          | [{steps, sort: ExpHole(u, _)}, ..._] => Some((u, steps))
          | _ => None
          };
        let synthesize_prev = () => {
          let sketch =
            sketch_of_rev_sketch(~preserve_hole=prev_u, rev_sketch);
          let+ (_, filling) = mk(prev_u, sketch);
          (prev_steps, filling);
        };
        let+ synthesizing =
          switch (HoleMap.find_opt(prev_steps, erased)) {
          | None => synthesize_prev()
          | Some((_, F(filled_map))) when HoleMap.is_empty(filled_map) =>
            synthesize_prev()
          | Some((e, filled_holes)) =>
            move_to_last_hole(~rev_sketch, e, filled_holes)
          };
        let removed = HoleMap.remove(prev_steps, erased);
        (ss, Filled(e, F(removed), synthesizing));
      };
    };
  go(~rev_sketch=[(e, F(HoleMap.empty), steps)], synthesizing);
};

let move_to_next_hole =
    (e: UHExp.t, (steps, _) as synthesizing: t): option(t) => {
  let rec go = (~rev_sketch, (ss, z): t): option(t) =>
    switch (z) {
    | Filling(_) => None
    | Filled(e, F(filled_map) as filled, (steps, z) as synthesizing) =>
      let rev_sketch = [(e, filled, steps), ...rev_sketch];
      switch (go(~rev_sketch, synthesizing)) {
      | Some(synthesizing) => Some((ss, Filled(e, filled, synthesizing)))
      | None =>
        let erased = HoleMap.add(steps, erase(z), filled_map);
        let zholes = mk_zholes(steps, e);
        let* (next_u, next_steps) =
          // assuming all holes encountered here will be empty exp holes
          switch (zholes.holes_after) {
          | [{steps, sort: ExpHole(u, _)}, ..._] => Some((u, steps))
          | _ => None
          };
        let synthesize_next = () => {
          let sketch =
            sketch_of_rev_sketch(~preserve_hole=next_u, rev_sketch);
          let+ (_, filling) = mk(next_u, sketch);
          (next_steps, filling);
        };
        let+ synthesizing =
          switch (HoleMap.find_opt(next_steps, erased)) {
          | None => synthesize_next()
          | Some((_, F(filled_map))) when HoleMap.is_empty(filled_map) =>
            synthesize_next()
          | Some((e, filled_holes)) =>
            move_to_first_hole(~rev_sketch, e, filled_holes)
          };
        let removed = HoleMap.remove(next_steps, erased);
        (ss, Filled(e, F(removed), synthesizing));
      };
    };
  go(~rev_sketch=[(e, F(HoleMap.empty), steps)], synthesizing);
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

let step_out = (e: UHExp.t, (steps, z): t): option(t) => {
  let rec go = (~rev_sketch, z: z) => {
    switch (z) {
    | Filling(_) => None
    | Filled(e, filled, (steps, z)) =>
      switch (go(~rev_sketch=[(e, filled, steps), ...rev_sketch], z)) {
      | Some(stepped_out) => Some(Filled(e, filled, (steps, stepped_out)))
      | None =>
        // need sketch up to this point
        let synth_sketch = sketch_of_rev_sketch(rev_sketch);
        let+ (_, filling) = {
          let u = {
            let (e, _, steps) = List.hd(rev_sketch);
            get_meta_var(steps, e);
          };
          mk(u, synth_sketch);
        };
        filling;
      }
    };
  };
  let+ z = go(~rev_sketch=[(e, F(HoleMap.empty), steps)], z);
  (steps, z);
};
