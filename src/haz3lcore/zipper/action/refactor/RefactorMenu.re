/* Context-menu / command-palette front-end: what to offer at the
 * caret and how to label it. */
open Language;
open RefactorBase;
open RefactorInline;
open RefactorMove;
open RefactorRegistry;

/* Program-dependent labels for static kinds (rename already
 * enumerates its own). A one-pat print is bounded — unlike the
 * whole-program prints the gating rule bans — keep it that way. */
let label_override =
    (
      kind: Action.refactor,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      term: Exp.t,
    )
    : option(string) =>
  switch (kind) {
  | MergeUp =>
    switch (merge_site_up(~target, term)) {
    | Some((p, _)) =>
      switch (IdTagged.term_of(p)) {
      | Let(sp, _, _) =>
        let_head_name(sp) |> Option.map(n => "Merge into " ++ n)
      | _ => None
      }
    | None => None
    }
  | MergeDown =>
    switch (merge_site_down(~target, term)) {
    | Some(l) =>
      switch (IdTagged.term_of(l)) {
      | Let(_, _, lbody) =>
        switch (IdTagged.term_of(lbody)) {
        | Let(sp, _, _) =>
          let_head_name(sp) |> Option.map(n => "Merge into " ++ n)
        | _ => None
        }
      | _ => None
      }
    | None => None
    }
  | AddCaseArm =>
    switch (find_hit(~hit=hit_node(target), term)) {
    | Some(e) =>
      match_witness(~info_map, e)
      |> Option.map(w => {
           let text =
             Printer.of_segment(
               ~holes="?",
               ~refractors=[],
               ExpToSegment.pat_to_segment(
                 ~settings=roundtrip_settings,
                 wildify(w),
               ),
             );
           "Add arm | " ++ text;
         })
    | None => None
    }
  | RemoveParameter =>
    switch (find_hit(~hit=hit_param(target), term)) {
    | Some(l) =>
      param_items(l)
      |> List.find_opt((it: Pat.t) => List.mem(target, IdTagged.ids(it)))
      |> Option.map((it: Pat.t) =>
           "Remove Parameter"
           ++ (
             switch (var_pat_name(it)) {
             | Some(n) => " " ++ n
             | None => ""
             }
           )
         )
    | None => None
    }
  | AddParameter =>
    switch (find_hit(~hit=hit_let(target), term)) {
    | Some(e) =>
      switch (IdTagged.term_of(e)) {
      | Let(p, _, _) =>
        (
          switch (sugar_fn_name(p)) {
          | Some(f) => Some(f)
          | None => let_head_name(p)
          }
        )
        |> Option.map(f => "Add param to " ++ f)
      | _ => None
      }
    | None => None
    }
  | _ => None
  };

/* Menu support: which refactorings apply at the current indication */
/* Payload kinds can't come from filtering the static `all` list: the
 * entries themselves depend on the program (one per candidate free
 * name), with labels naming names */
let rename_items =
    (~info_map: Statics.Map.t, ~target: Id.t, term: Exp.t)
    : list((Action.refactor, string, string)) => {
  let exp_items =
    switch (find_hit(~hit=hit_rename(target), term)) {
    | Some(e) =>
      rename_pairs(~info_map, ~target, e)
      |> List.map(((x, y)) =>
           (
             Action.RenameFree(x, y),
             "Rename " ++ x ++ " to " ++ y,
             "Bind free occurrences of " ++ x ++ " at this binding",
           )
         )
    | None => []
    };
  let typ_items =
    rename_typ_pairs(~target, term)
    |> List.map(((x, t)) =>
         (
           Action.RenameTypFree(x, t),
           "Rename " ++ x ++ " to " ++ t,
           "Bind free type mentions of " ++ x ++ " at this alias",
         )
       );
  exp_items @ typ_items;
};

let menu_items =
    (~info_map: Statics.Map.t, ~term: Exp.t, z: Zipper.t)
    : list((Action.refactor, string, string)) =>
  switch (Indicated.index(z)) {
  | None => []
  | Some(target) =>
    let static =
      all
      |> List.filter_map(kind => {
           let i = impl(kind);
           if (applies(kind, ~info_map, ~target, term)) {
             let label =
               label_override(kind, ~info_map, ~target, term)
               |> Option.value(~default=i.label);
             Some((kind, label, i.tooltip));
           } else {
             None;
           };
         });
    let swaps =
      switch (swap_site(~info_map, ~target, term)) {
      | Some((l, _)) =>
        let names = swap_param_names(l);
        List.init(max(List.length(names) - 1, 0), i => i)
        |> List.filter(i =>
             Option.is_some(swap_params_rewrite(~fixup=false, i, l))
           )
        |> List.map(i =>
             (
               Action.SwapParams(i),
               "Swap "
               ++ List.nth(names, i)
               ++ " ↔ "
               ++ List.nth(names, i + 1),
               "Swap these parameters at the definition and all call sites",
             )
           );
      | None => []
      };
    let arms =
      switch (find_hit(~hit=hit_arm(target), term)) {
      | Some(m) =>
        switch (arm_index_at(target, m)) {
        | Some(j) =>
          let mk = (i, label) =>
            Option.is_some(swap_arms_rewrite(~fixup=false, ~target, i, m))
              ? [
                (
                  Action.SwapArms(i),
                  label,
                  "Swap this arm with its neighbor (order-safe: patterns are disjoint)",
                ),
              ]
              : [];
          mk(j - 1, "Move arm up") @ mk(j, "Move arm down");
        | None => []
        }
      | None => []
      };
    let tuple_swaps =
      switch (find_hit(~hit=hit_tuple_swap(target), term)) {
      | Some(l) =>
        let items =
          switch (IdTagged.term_of(l)) {
          | Let(p, _, _) => tuple_pat_items(p)
          | _ => []
          };
        let name = (j: int) =>
          switch (var_pat_name(List.nth(items, j))) {
          | Some(n) => n
          | None => "#" ++ string_of_int(j + 1)
          };
        List.init(max(List.length(items) - 1, 0), i => i)
        |> List.filter(i =>
             Option.is_some(
               swap_tuple_pat_rewrite(~fixup=false, ~target, i, l),
             )
           )
        |> List.map(i =>
             (
               Action.SwapTuplePat(i),
               "Swap " ++ name(i) ++ " ↔ " ++ name(i + 1),
               "Swap these tuple components in the pattern and the definition",
             )
           );
      | None => []
      };
    static
    @ rename_items(~info_map, ~target, term)
    @ swaps
    @ tuple_swaps
    @ arms;
  };
