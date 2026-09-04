open Util_web;
open ProjectorBase;
open Language;

type transform =
  | Rowwise(Exp.t)
  | Listwise(Exp.t);

/* Type utilities for column operations */
let get_column_type_from_ty = (ty: Typ.t, column: string) => {
  switch (ty.term) {
  | List({term: Prod(tys), _}) =>
    List.find_map(
      ty => {
        open OptUtil.Syntax;
        let* (label, value_ty) = Typ.match_tup_label(ty);
        if (label == column) {
          Some(value_ty);
        } else {
          None;
        };
      },
      tys,
    )
  | _ => None
  };
};

let get_columns = (ty: Typ.t): option(list(string)) => {
  switch (ty.term) {
  | List({term: Prod(tys), _}) =>
    OptUtil.traverse(
      ty => {
        open OptUtil.Syntax;
        let* (label, _value_ty) = Typ.match_tup_label(ty);
        Some(label);
      },
      tys,
    )
  | _ => None
  };
};

let is_option_type = (ty: Typ.t): bool => {
  let ctx = Builtins.ctx_init(Some(Int));
  Typ.is_consistent(ctx, ty, BuiltinsADT.Option.t)
  && Typ.is_more_precise(ctx, ty, BuiltinsADT.Option.t);
};

let strip_parens =
  Exp.map_term(~f_exp=(continue, e) =>
    switch (e.term) {
    | Parens(inner) => continue(inner)
    | _ => continue(e)
    }
  );

let get_type_from_info = (info: info): option(Typ.t) =>
  switch (info.statics) {
  | Some(InfoExp({ty, ctx, _})) =>
    let ty = Typ.normalize(ctx, ty);
    Typ.contains_unknown(ty) ? None : Some(ty);
  | _ => None
  };

let get_dynamic_type = (exp: Exp.t): option(Typ.t) => {
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
  IdTagged.rep_id(exp)
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => {
           Some(e.ty);
         }
       | _ => None,
     );
};

let can_move_column =
    (columns_opt: option(list(string)), column: string, left: bool) =>
  switch (columns_opt) {
  | Some(columns) =>
    switch (List.find_index(x => x == column, columns)) {
    | Some(idx) => left ? idx > 0 : idx < List.length(columns) - 1
    | None => false
    }
  | None => false
  };

let atom_cls_of_typ = (ty: Typ.t): option(Atom.cls) =>
  switch (Typ.cls_of_term(ty.term)) {
  | Typ.Atom(cls) => Some(cls)
  | _ => None
  };

/* Apply a list of transforms to a base expression, producing the
 * piped result: base |> transform1 |> transform2 |> ... */
let apply_transforms = (base: Exp.t, transforms: list(transform)): Exp.t => {
  open IdTagged.FreshGrammar;
  let to_listwise = (t: transform): Exp.t =>
    switch (t) {
    | Rowwise(row_fn) =>
      Exp.(deferred_ap(var("map"), [deferral(InAp), row_fn]))
    | Listwise(expr) => expr
    };
  let transformations = List.map(to_listwise, transforms);
  let base = strip_parens(base);
  List.fold_left(
    (acc, transformation) => Exp.ap(Reverse, transformation, acc),
    base,
    transformations,
  );
};

/* Single conversion point: transform list → Base.segment.
 * Returns None if the syntax isn't an expression or if lifting fails —
 * callers should treat that as "do nothing" rather than crashing. */
let to_segment =
    (info: info, transforms: list(transform)): option(Base.segment) => {
  let ok = ref(true);
  let lifted =
    info.utility.lift_syntax(
      ~inline=false,
      fun
      | Exp(exp) => Exp(apply_transforms(exp, transforms))
      | other => {
          ok := false;
          other;
        },
      info.syntax,
    );
  ok^ ? lifted : None;
};

/* Column transformation operations */
let drop_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        deferred_ap(var("omit_labels"), [deferral(InAp), label(column)])
      ),
    )
  );
};

let convert_column = (column: string, conversion_fn: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([
              tup_label(
                label(column),
                ap(
                  Forward,
                  var(conversion_fn),
                  dot(var("r"), label(column)),
                ),
              ),
            ]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

let rename_column = (old_name: string, new_name: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            ap(
              Forward,
              var("omit_labels"),
              tuple([var("r"), label(old_name)]),
            ),
            tuple([
              tup_label(label(new_name), dot(var("r"), label(old_name))),
            ]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

/* Insert a new column with both label and value as empty holes. The user
 * fills in the label and value directly in the editor. */
let add_column = (): transform =>
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([tup_label(empty_hole(), empty_hole())]),
          ),
          None,
          None,
        )
      ),
    )
  );

let clear_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([tup_label(label(column), empty_hole())]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

let noop_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("r"),
          tuple_extension(
            var("r"),
            tuple([
              tup_label(label(column), dot(var("r"), label(column))),
            ]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

let group_by_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Listwise(
      Exp.(
        deferred_ap(
          var("group_on_key"),
          [
            deferral(InAp),
            fn(
              Pat.var("row"),
              dot(var("row"), label(column)),
              None,
              None,
            ),
          ],
        )
      ),
    )
  );
};

let filter_by_column = (op, column: string): transform => {
  IdTagged.FreshGrammar.(
    Listwise(
      Exp.(
        deferred_ap(
          var("filter"),
          [
            deferral(InAp),
            fn(
              Pat.var("row"),
              bin_op(op, dot(var("row"), label(column)), empty_hole()),
              None,
              None,
            ),
          ],
        )
      ),
    )
  );
};

/* Filter with an open-ended predicate body. The whole predicate body is an
 * empty hole, so the user writes anything that returns Bool. */
let custom_filter = (): transform =>
  IdTagged.FreshGrammar.(
    Listwise(
      Exp.(
        deferred_ap(
          var("filter"),
          [deferral(InAp), fn(Pat.var("row"), empty_hole(), None, None)],
        )
      ),
    )
  );

/* String-specific filter: keeps rows whose column matches a regex pattern.
 * The pattern is an empty hole; the user fills it in. */
let string_match_filter = (column: string): transform =>
  IdTagged.FreshGrammar.(
    Listwise(
      Exp.(
        deferred_ap(
          var("filter"),
          [
            deferral(InAp),
            fn(
              Pat.var("row"),
              ap(
                Forward,
                var("string_match"),
                tuple([empty_hole(), dot(var("row"), label(column))]),
              ),
              None,
              None,
            ),
          ],
        )
      ),
    )
  );

let drop_nones_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Listwise(
      Exp.(
        deferred_ap(
          var("filter_map"),
          [
            deferral(InAp),
            fn(
              Pat.var("row"),
              ap(
                Forward,
                var("option_map"),
                tuple([
                  dot(var("row"), label(column)),
                  fn(
                    Pat.var("v"),
                    tuple_extension(
                      var("row"),
                      tuple([tup_label(label(column), var("v"))]),
                    ),
                    None,
                    None,
                  ),
                ]),
              ),
              None,
              None,
            ),
          ],
        )
      ),
    )
  );
};

let provide_default_column = (column: string): transform => {
  IdTagged.FreshGrammar.(
    Rowwise(
      Exp.(
        fn(
          Pat.var("row"),
          tuple_extension(
            var("row"),
            tuple([
              tup_label(
                label(column),
                match(
                  dot(var("row"), label(column)),
                  [
                    (Pat.constructor("None", None), empty_hole()),
                    (
                      Pat.ap(Pat.constructor("Some", None), Pat.var("v")),
                      var("v"),
                    ),
                  ],
                ),
              ),
            ]),
          ),
          None,
          None,
        )
      ),
    )
  );
};

let move_column =
    (dyn_type: option(Typ.t), column: string, left: bool): option(transform) => {
  let columns_opt = Option.bind(dyn_type, get_columns);
  switch (columns_opt) {
  | Some(columns) =>
    let idx_opt = List.find_index(x => x == column, columns);
    switch (idx_opt) {
    | Some(idx) =>
      let new_idx = left ? idx - 1 : idx + 1;
      if (new_idx < 0 || new_idx >= List.length(columns)) {
        None;
      } else {
        let new_columns =
          List.mapi(
            (i, x) =>
              if (i == idx) {
                List.nth(columns, new_idx);
              } else if (i == new_idx) {
                List.nth(columns, idx);
              } else {
                x;
              },
            columns,
          );
        Some(
          Rowwise(
            IdTagged.FreshGrammar.Exp.(
              deferred_ap(
                var("select_labels"),
                [deferral(InAp)] @ List.map(label, new_columns),
              )
            ),
          ),
        );
      };
    | None => None
    };
  | None => None
  };
};

let sort_column =
    (column_type: option(Typ.t), header: string, descending: bool)
    : option(list(transform)) => {
  let compare_fn =
    Option.bind(column_type, atom_cls_of_typ)
    |> Option.map(Atom.compare_builtin)
    |> Option.join;
  switch (compare_fn) {
  | Some(compare_fn_name) =>
    let cmp_call =
      IdTagged.FreshGrammar.(
        Exp.(
          ap(
            Forward,
            var(compare_fn_name),
            tuple([
              dot(var("r1"), label(header)),
              dot(var("r2"), label(header)),
            ]),
          )
        )
      );
    /* Descending wraps the comparator in invert_ord so one pass
     * of sort handles both directions — no separate reverse step. */
    let body =
      descending
        ? IdTagged.FreshGrammar.Exp.(
            ap(Forward, var(BuiltinsADT.invert_ord.name), cmp_call)
          )
        : cmp_call;
    let sort_transform =
      Listwise(
        IdTagged.FreshGrammar.(
          Exp.(
            deferred_ap(
              var("sort"),
              [
                fn(
                  Pat.tuple([Pat.var("r1"), Pat.var("r2")]),
                  body,
                  None,
                  None,
                ),
                deferral(InAp),
              ],
            )
          )
        ),
      );
    Some([sort_transform]);
  | None => None
  };
};
