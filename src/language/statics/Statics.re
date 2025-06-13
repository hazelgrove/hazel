/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the ANA and the SELF.

   (ana:Typ.t): Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Self.t type which is used to represent the
   type information derivable from the term itself.

   The point of STATICS.re itself is to derive a map between each
   term's unique id and that term's static INFO. The below functions
   are intended mostly as infrastructure: The point is to define a
   traversal through the syntax tree which, for each term, passes
   down the MODE, passes up the SELF, calculates the INFO, and adds
   it to the map.

   The architectural intention here is that most type-manipulation
   logic is defined in INFO, MODE, and SELF, and the STATICS module
   itself is dedicated to the piping necessary to (A) introduce and
   (B) propagate the necessary information through the syntax tree.

    */

module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type errors = Id.Map.t(Info.error);

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;

  let error_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Second clause is to eliminate non-representative ids,
         * which will not be found in the measurements map */
        Info.is_error(info) && id == Info.id_of(info) ? [id, ...acc] : acc,
      info_map,
      [],
    );

  let errors = (map: t): list((Id.t, Info.error)) =>
    Id.Map.fold(
      (id, info: Info.t, acc) =>
        Option.to_list(Info.error_of(info) |> Option.map(x => (id, x)))
        @ acc,
      map,
      [],
    );

  let collect_errors = (map: t): errors =>
    Id.Map.filter_map(
      (_: Uuidm.t, info: Info.t) => {Info.error_of(info)},
      map,
    );

  /* The ids of binding sites for for all references in term with `id` */
  let refs_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoExp({co_ctx, ctx, _})) =>
      co_ctx
      |> Util.VarMap.to_list
      |> List.map(((n, _)) => Ctx.binding_of(ctx, n))
    | _ => []
    };

  let bound_in = (m: t, id: Id.t): Binding.s =>
    switch (lookup(id, m)) {
    | Some(InfoPat({term, _})) => Term.Pat.bindings(term)
    | _ => []
    };
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let rec is_arrow_like = (t: Typ.t) => {
  switch (t |> Typ.term_of) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Forall(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: Typ.t) => {
  switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    let norm = Typ.normalize(ctx, syn);
    switch (norm |> Typ.term_of) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | _ when is_arrow_like(norm) => num_vars == 1
    | _ => false
    };
  | _ => false
  };
};

let syn = Unknown(SynSwitch) |> Typ.temp;

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, m) =
      uexp_to_info_map(
        ~ctx,
        ~ancestors,
        ~duplicates=[],
        ~expected_labels=None,
        ~label_sort=false,
        e,
        m,
      );
    (co_ctx, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~duplicates=[],
        ~ctx,
        p,
        m,
      )
      |> snd;
    (CoCtx.empty, m);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(_)
  | Any () => (CoCtx.empty, m)
  }
and multi = (~ctx, ~ancestors, m, tms) =>
  List.fold_left(
    ((co_ctxs, m), any) => {
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~ana=Unknown(SynSwitch) |> Typ.temp,
      ~is_in_filter=false,
      ~ancestors,
      ~duplicates: list(string),
      ~expected_labels: option(list(string)),
      ~override_self: option(Self.exp)=?,
      ~inferred_label: option(LabeledTuple.label)=?,
      ~label_sort,
      {annotation: {ids}, term} as uexp: Exp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  let add' = (~label_inference=?, ~self, ~co_ctx, m) => {
    let info =
      Info.derived_exp(
        ~uexp,
        ~ctx,
        ~ana,
        ~ancestors,
        ~self=Option.value(~default=self, override_self),
        ~co_ctx,
        ~label_inference,
        ~inferred_label,
        ~label_sort,
      );

    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m) => {
    add'(~self=Common(self), ~co_ctx, m);
  };
  let ancestors = [Exp.rep_id(uexp)] @ ancestors;
  let uexp_to_info_map =
      (
        ~ctx,
        ~ana=Unknown(SynSwitch) |> Typ.temp,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        ~duplicates=[],
        ~expected_labels=?,
        ~inferred_label: option(string)=?,
        ~override_self=?,
        ~label_sort=false,
        uexp: Exp.t,
        m: Map.t,
      ) => {
    uexp_to_info_map(
      ~ctx,
      ~ana,
      ~is_in_filter,
      ~ancestors,
      ~duplicates,
      ~expected_labels,
      ~override_self?,
      ~inferred_label?,
      ~label_sort,
      uexp,
      m,
    );
  };
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let map_m_go = (m, ~duplicates=[]) =>
    List.fold_left2(
      ((es, m), ana, e) =>
        go(~ana, ~duplicates, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors, ~duplicates);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors);

  // This lifts an expression into a singleton labeled tuple by rewriting the syntax in the Statics Map
  let autolabel_singleton_tuple = (uexp: Exp.t, inner_ty, l, m) => {
    let (term, rewrap) = Exp.unwrap(uexp);
    let original_expression = Exp.fresh(term);
    let (original_info, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=inner_ty,
        ~is_in_filter,
        ~ancestors,
        original_expression,
        m,
      );

    /* Special case for probes, which would otherwise lose their id association here */
    let elaborated_exp =
      switch (term) {
      | Probe(_, p) =>
        rewrap(
          Probe(
            Tuple([
              TupLabel(Label(l) |> Exp.fresh, original_expression)
              |> Exp.fresh,
            ])
            |> Exp.fresh,
            p,
          ),
        )
      | _ =>
        rewrap(
          Tuple([
            TupLabel(Label(l) |> Exp.fresh, original_expression) |> Exp.fresh,
          ]),
        )
      };

    // We need to reanalyze the elaborated expression to get the statics in the map for the label and tuple
    let (info, m) =
      uexp_to_info_map(~ctx, ~ana, ~ancestors, elaborated_exp, m);

    // We need to keep the original status of the expression to get error messages on the unelaborated expression
    let info = {
      ...info,
      status: original_info.status,
      label_inference:
        Some(
          SingletonLabelInference({
            label: l,
            pre_labeled_info: original_info,
          }),
        ),
    };

    (info, add_info(IdTagged.ids(elaborated_exp), InfoExp(info), m));
  };
  let atomic = self => {
    add(~self, ~co_ctx=CoCtx.empty, m);
  };

  // This is the case where we aren't a singleton labeled tuple
  let default_case = () => {
    switch (term) {
    | Closure(_, e) =>
      // TODO: implement closure type checking properly - see how dynamic type assignment does it
      let (e, m) = go(~ana, e, m);
      add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
    | MultiHole(tms) =>
      let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
      add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
    | Cast(e, _, t2)
    | FailedCast(e, _, t2) =>
      let (t, m) = go_typ(t2, ~expects=Info.TypeExpected, m);
      let (e, m) = go'(~ana=t.term, ~ctx=t.ctx, e, m);
      add(~self=Just(t.term), ~co_ctx=e.co_ctx, m);
    | Invalid(token) => atomic(BadToken(token))
    | EmptyHole => atomic(Just(Unknown(Internal) |> Typ.temp))
    | Deferral(position) =>
      add'(~self=IsDeferral(position), ~co_ctx=CoCtx.empty, m)
    | Undefined => atomic(Just(Unknown(Hole(EmptyHole)) |> Typ.temp))
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(c) =>
        let ty = Atom(Atom.cls_of_t(c)) |> Typ.temp;
        atomic(Just(ty));
      | R(BadInt(str)) => atomic(BadToken(str))
      };

    | LivelitName(name) =>
      add'(
        ~self=Self.of_exp_livelit_name(ctx, name),
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        m,
      )
    | ListLit(es) =>
      let ids = List.map(Exp.rep_id, es);
      let inner_ana_ty = Typ.matched_list(ctx, ana);
      let anas = List.init(List.length(es), _ => inner_ana_ty);
      let (es, m) = map_m_go(m, anas, es);
      let tys = List.map(Info.exp_ty, es);
      add(
        ~self=
          Self.listlit(~empty=Unknown(Internal) |> Typ.temp, ctx, tys, ids),
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
        m,
      );
    | Cons(hd, tl) =>
      let inner_ana_ty = Typ.matched_list(ctx, ana);
      let (hd, m) = go(~ana=inner_ana_ty, hd, m);
      let (tl, m) = go(~ana=List(inner_ana_ty) |> Typ.temp, tl, m);
      add(
        ~self=Just(List(hd.ty) |> Typ.temp),
        ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
        m,
      );
    | ListConcat(e1, e2) =>
      let inner_ana_ty = List(Typ.matched_list(ctx, ana)) |> Typ.temp;
      let ids = List.map(Exp.rep_id, [e1, e2]);
      let (e1, m) = go(~ana=inner_ana_ty, e1, m);
      let (e2, m) = go(~ana=inner_ana_ty, e2, m);
      add(
        ~self=Self.list_concat(ctx, [e1.ty, e2.ty], ids),
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Var(name) =>
      add'(
        ~self=Self.of_exp_var(ctx, name),
        ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), ana),
        m,
      )
    | DynamicErrorHole(e, _)
    | Parens(e)
    | Probe(e, _) =>
      let (e, m) = go(~ana, e, m);
      add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
    | UnOp(Meta(Unquote), e) when is_in_filter =>
      let e: Exp.t = {
        annotation: {
          ids: IdTagged.ids(e),
        },
        term:
          switch (e.term) {
          | Var("e") =>
            Constructor("$e", Some(Some(Unknown(Internal) |> Typ.fresh)))
          | Var("v") =>
            Constructor("$v", Some(Some(Unknown(Internal) |> Typ.fresh)))
          | _ => e.term
          },
      };
      let ty_in = Var("$Meta") |> Typ.temp;
      let ty_out = Unknown(Internal) |> Typ.temp;
      let (e, m) = go(~ana=ty_in, e, m);
      add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
    | UnOp(Meta(Unquote), e) =>
      let (e, m) = go(~ana=syn, e, m);
      add(~self=BadOperator("Unquote not in filter"), ~co_ctx=e.co_ctx, m);
    | UnOp(op, e) =>
      let op = Operators.replace_un_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_un_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (_, m) = go(~ana=syn, e, m);
        add(~self=BadOperator(msg), ~co_ctx=CoCtx.empty, m);
      | Defined(ty_in, ty_out, _) =>
        let ty_in = Atom(Atom.cls_of_kind(ty_in)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e, m) = go(~ana=ty_in, e, m);
        add(~self=Just(ty_out), ~co_ctx=e.co_ctx, m);
      };
    | BinOp(op, e1, e2) =>
      let op = Operators.replace_bin_op(op, ctx.use_mode); // Replace op if necessary due to `use`
      let op_semantics = Operators.semantics_of_bin_op(op);
      switch (op_semantics) {
      | Undefined(msg) =>
        let (_, m) = go(~ana=syn, e1, m);
        let (_, m) = go(~ana=syn, e2, m);
        add(~self=BadOperator(msg), ~co_ctx=CoCtx.empty, m);
      | Defined(ty1, ty2, ty_out, _) =>
        let ty1 = Atom(Atom.cls_of_kind(ty1)) |> Typ.temp;
        let ty2 = Atom(Atom.cls_of_kind(ty2)) |> Typ.temp;
        let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
        let (e1, m) = go(~ana=ty1, e1, m);
        let (e2, m) = go(~ana=ty2, e2, m);
        add(
          ~self=Just(ty_out),
          ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
          m,
        );
      };
    | Tuple(es) =>
      let expected_labels =
        switch (Typ.weak_head_normalize(ctx, ana).term) {
        | Prod(ts) =>
          Some(
            List.filter_map(
              t => Typ.match_tup_label(t) |> Option.map(fst),
              ts,
            ),
          )
        | _ => None
        };

      let original_labels =
        List.map(e => Exp.match_tup_label(e) |> Option.map(fst), es);

      let (inferred_es, ana_tys) =
        Typ.matched_prod(
          ctx,
          List.map(e => (None: option(string), e), es),
          ((inferred, e)) => {
            Exp.match_tup_label(e)
            |> Option.map(((label, element)) =>
                 (label, (inferred, element))
               )
          },
          ana,
          (name, (_, e)) =>
            (
              Some(name),
              TupLabel(Label(name) |> Exp.fresh, e) |> Exp.fresh,
            ),
        );
      let es = List.map(snd, inferred_es);
      let inferred = List.map(fst, inferred_es);

      let new_labels =
        List.map(e => Exp.match_tup_label(e) |> Option.map(fst), es);

      let duplicate_labels =
        LabeledTuple.get_duplicate_labels(Exp.match_tup_label, es);

      let (es', m) =
        List.fold_left2(
          ((es, m), ana, (inferred_label, e)) => {
            go(
              ~ana,
              ~inferred_label?,
              ~duplicates=duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (((e, m)) => (es @ [e], m))
          },
          ([], m),
          ana_tys,
          List.combine(inferred, es),
        );
      let ty_list = List.map(Info.exp_ty, es');

      let (malformed_labels, duplicate_labels, invalid_labels) =
        List.fold_left(
          ((a, b, c), e: Info.exp) => {
            switch (e.status) {
            | InHole(
                Common(
                  TupleLabelError({
                    malformed_labels,
                    duplicate_labels,
                    invalid_labels,
                    _,
                  }),
                ),
              ) => (
                a @ malformed_labels,
                b @ duplicate_labels,
                c @ invalid_labels,
              )
            | _ => (a, b, c)
            }
          },
          ([], [], []),
          es',
        );

      let ty_list = Typ.remove_duplicate_labels(~duplicate_labels, ty_list);

      let self =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? Self.Just(Prod(ty_list) |> Typ.temp)
          : Self.TupleLabelError({
              malformed_labels,
              duplicate_labels,
              invalid_labels,
              typ: Prod(ty_list) |> Typ.temp,
            });

      add'(
        ~self=Common(self),
        ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es')),
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        m,
      );
    | TupLabel(label, e) =>
      let (lab, e, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode)) =>
          let label_self: option(Self.exp) =
            switch (label.term) {
            | Label(_)
            | EmptyHole => None
            | _ => Some(Common(BadLabel(Exp(label))))
            };

          let (lab, m) =
            go(
              ~ana=labmode,
              ~override_self=?label_self,
              ~label_sort=true,
              ~duplicates,
              label,
              m,
            );
          let (e, m) = go(~ana=val_mode, ~inferred_label?, e, m);
          (lab, e, m);
        | _ =>
          let (lab, m) =
            go(
              ~ana=Unknown(Internal) |> Typ.temp,
              ~override_self=?
                switch (label.term, expected_labels) {
                | (Label(name), Some(expected_labels))
                    when !List.mem(name, expected_labels) =>
                  Some(Common(InvalidLabel(name)))
                | (Label(_), _)
                | (EmptyHole, _) => None
                | _ => Some(Common(BadLabel(Exp(label))))
                },
              ~duplicates,
              ~label_sort=true,
              label,
              m,
            );

          let (e, m) =
            go(~ana=Unknown(Internal) |> Typ.temp, ~inferred_label?, e, m);
          (lab, e, m);
        };

      let self =
        switch (lab.status) {
        | NotInHole(_) => Self.Just(TupLabel(lab.ty, e.ty) |> Typ.temp)
        | InHole(
            Common(
              Inconsistent(Expectation({syn: {term: Label(name), _}, _})),
            ),
          )
        | InHole(Common(NoType(InvalidLabel(name)))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [],
            invalid_labels: [name],
            typ: TupLabel(Label(name) |> Typ.temp, e.ty) |> Typ.temp,
          })
        | InHole(Common(DuplicateLabel(name, _))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [name],
            invalid_labels: [],
            typ: TupLabel(Label(name) |> Typ.temp, e.ty) |> Typ.temp,
          })
        | InHole(_) =>
          Self.TupleLabelError({
            malformed_labels: [Exp(label)],
            duplicate_labels: [],
            invalid_labels: [],
            typ: TupLabel(Unknown(Internal) |> Typ.temp, e.ty) |> Typ.temp,
          })
        };
      add(~self, ~co_ctx=CoCtx.union([lab.co_ctx, e.co_ctx]), m);
    | Label(name) =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicates)
        ? atomic(Duplicate(name, self)) : atomic(self);
    | BuiltinFun(string) =>
      add'(
        ~self=Self.of_exp_var(Builtins.ctx_init(None), string),
        ~co_ctx=CoCtx.empty,
        m,
      )

    | Dot(e1, e2) =>
      let (info_e1, m) = go(~ana=Unknown(SynSwitch) |> Typ.temp, e1, m);
      let (info_e2, m) = go(~ana=Label("") |> Typ.temp, e2, m);
      let (ty, m) = {
        switch (info_e1.ty.term, info_e2.ty.term) {
        | (Unknown(_), Label(name)) =>
          // This is so that the statics will result in Unknown(Internal)
          let ty =
            Prod([
              TupLabel(
                Label(name) |> Typ.temp,
                Unknown(Internal) |> Typ.temp,
              )
              |> Typ.temp,
            ])
            |> Typ.temp;
          let (_, m) = go(~ana=ty, e1, m);
          (ty, m);
        | (Var(_), _) => (Typ.weak_head_normalize(ctx, info_e1.ty), m)
        | _ => (info_e1.ty, m)
        };
      };
      switch (ty.term) {
      | Prod(ts) =>
        let labels =
          List.filter_map(Typ.match_tup_label, ts) |> List.map(fst);

        switch (e2.term) {
        | Label(name) =>
          let element: option(Typ.t) =
            LabeledTuple.find_label(Typ.match_tup_label, ts, name);
          switch (element) {
          | Some({term: TupLabel(_, typ), _})
          | Some(typ) => add(~self=Just(typ), ~co_ctx=info_e2.co_ctx, m)
          | None =>
            add(~self=LabelNotFound(name, labels), ~co_ctx=info_e2.co_ctx, m)
          };
        | EmptyHole =>
          add(
            ~self=Just(Unknown(Internal) |> Typ.temp),
            ~co_ctx=info_e2.co_ctx,
            m,
          )
        | _ => add(~self=BadLabel(Exp(e2)), ~co_ctx=info_e2.co_ctx, m)
        };
      | _ => add(~self=WantTuple, ~co_ctx=info_e2.co_ctx, m)
      };
    | Test(e) =>
      let (e, m) = go(~ana=Atom(Bool) |> Typ.temp, e, m);
      add(~self=Just(Prod([]) |> Typ.temp), ~co_ctx=e.co_ctx, m);
    | Filter(Filter({pat: cond, _}), body) =>
      let (cond, m) =
        go(~ana=Unknown(SynSwitch) |> Typ.temp, cond, m, ~is_in_filter=true);
      let (body, m) = go(~ana, body, m);
      add(
        ~self=Just(body.ty),
        ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
        m,
      );
    | Filter(Residue(_), body) =>
      let (body, m) = go(~ana, body, m);
      add(~self=Just(body.ty), ~co_ctx=CoCtx.union([body.co_ctx]), m);
    | Seq(e1, e2) =>
      let (e1, m) = go(~ana=Unknown(SynSwitch) |> Typ.temp, e1, m);
      let (e2, m) = go(~ana, e2, m);
      add(
        ~self=Just(e2.ty),
        ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
        m,
      );
    | Constructor(ctr, ty) => atomic(Self.of_ctr(ctx, ctr, ana, ty))
    | Ap(_, fn, arg) =>
      switch (fn.term) {
      | LivelitName(s) =>
        // refer to livelit context to find types
        switch (Ctx.lookup_livelit(ctx, s)) {
        | Some({expansion_t, model_t, expand, _}) =>
          let (fn, m) = go(~ana=expansion_t, fn, m);
          let (arg, m) = go(~ana=model_t, arg, m);

          // try to expand
          switch (expand(arg.term)) {
          | Some(_) =>
            add(
              ~self=Just(expansion_t),
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              m,
            )
          | None =>
            // if we can't expand, flag as improper model
            add(
              ~self=BadLivelitModel(expansion_t),
              ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
              m,
            )
          };

        | None =>
          let (fn, m) = go(~ana=Unknown(Internal) |> Typ.temp, fn, m);
          let (arg, m) = go(~ana=Unknown(Internal) |> Typ.temp, arg, m);
          add(
            ~self=Just(Unknown(Internal) |> Typ.temp),
            ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]),
            m,
          );
        }
      | _ =>
        /* This logic lets us treat constructors differently to functions in
           terms of error localization */
        let fn_ana =
          switch (Exp.ctr_name(fn)) {
          | Some(name) =>
            switch (Self.ctr_ana_typ(ctx, ana, name)) {
            | Some(ty_ana) =>
              switch (Typ.matched_arrow_strict(ctx, ty_ana)) {
              | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
              | None => Arrow(syn, syn) |> Typ.temp
              }
            | None => Arrow(syn, syn) |> Typ.temp
            }
          | None => Arrow(syn, syn) |> Typ.temp
          };
        let (fn, m) = go(~ana=fn_ana, fn, m);
        let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);

        let (arg, m) = go(~ana=ty_in, arg, m);
        let self: Self.t =
          Id.is_nullary_ap_flag(IdTagged.ids(arg.term))
          && !Typ.is_consistent(ctx, ty_in, Prod([]) |> Typ.temp)
            ? BadTrivAp(ty_in) : Just(ty_out);
        add(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]), m);
      }
    | TypAp(fn, utyp) =>
      let typfn_ana =
        Forall(EmptyHole |> TPat.fresh, Unknown(SynSwitch) |> Typ.temp)
        |> Typ.temp;
      let (fn, m) = go(~ana=typfn_ana, fn, m);
      let (_, m) = utyp_to_info_map(~ctx, ~ancestors, utyp, m);
      let (option_name, ty_body) = Typ.matched_forall(ctx, fn.ty);
      switch (option_name) {
      | Some(name) =>
        add(
          ~self=Just(Typ.subst(utyp, name, ty_body)),
          ~co_ctx=fn.co_ctx,
          m,
        )
      | None => add(~self=Just(ty_body), ~co_ctx=fn.co_ctx, m) /* invalid name matches with no free type variables. */
      };
    | DeferredAp(fn, args) =>
      /* This logic lets us treat constructors differently to functions in
         terms of error localization */
      let fn_ana =
        switch (Exp.ctr_name(fn)) {
        | Some(name) =>
          switch (Self.ctr_ana_typ(ctx, ana, name)) {
          | Some(ty_ana) =>
            switch (Typ.matched_arrow_strict(ctx, ty_ana)) {
            | Some((ty1, ty2)) => Arrow(ty1, ty2) |> Typ.temp
            | None => Arrow(syn, syn) |> Typ.temp
            }
          | None => Arrow(syn, syn) |> Typ.temp
          }
        | None => Arrow(syn, syn) |> Typ.temp
        };
      let (fn, m) = go(~ana=fn_ana, fn, m);
      let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn.ty);
      let num_args = List.length(args);
      switch (Typ.matched_args_strict(ctx, ty_in, num_args)) {
      | L(ty_ins) =>
        let (args_infos, m) = map_m_go(m, ty_ins, args);
        let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args_infos));
        let ty_in' =
          List.combine(ty_ins, args)
          |> List.filter(((_, e)) => Exp.is_deferral(e))
          |> List.map(fst)
          |> (
            fun
            | [x] => x
            | xs => Prod(xs) |> Typ.temp
          );
        add(
          ~self=Just(Arrow(ty_in', ty_out) |> Typ.temp),
          ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
          m,
        );
      | R(expected) =>
        let ty_ins = List.init(num_args, _ => Unknown(Internal) |> Typ.temp);
        let (args, m) = map_m_go(m, ty_ins, args);
        let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
        add'(
          ~self=
            IsBadPartialAp(
              ArityMismatch({
                expected,
                actual: num_args,
              }),
            ),
          ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]),
          m,
        );
      };
    | Fun(p, e, typ, _) =>
      let (mode_pat, mode_body) = Typ.matched_arrow(ctx, ana);
      let mode_pat = Option.value(~default=mode_pat, typ);
      let (p', _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=mode_pat, p, m);
      let (e, m) = go'(~ctx=p'.ctx, ~ana=mode_body, e, m);
      /* add co_ctx to pattern */
      let (p, m) =
        go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~ana=mode_pat, p, m);
      // TODO: factor out code
      let unwrapped_self: Self.exp =
        Common(Just(Arrow(p.ty, e.ty) |> Typ.temp));
      let Coverage.{is_exhaustive, _} =
        Coverage.check([Info.pat_constraint(p)], Typ.normalize(ctx, p.ty));
      let self =
        is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
      add'(~self, ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx), m);
    | TypFun(utpat, body, _) =>
      let (name_expected_opt, item) = Typ.matched_forall(ctx, ana);
      let (mode_body, ctx_body) =
        switch (TPat.tyvar_of_utpat(utpat)) {
        | Some(name) when !Ctx.shadows_typ(ctx, name) =>
          let mode_body = {
            switch (name_expected_opt) {
            | Some(name_expected) =>
              Typ.subst(Var(name) |> Typ.temp, name_expected, item)
            | _ => item
            };
          };
          let ctx_body =
            Ctx.extend_tvar(
              ctx,
              {
                name,
                id: TPat.rep_id(utpat),
                kind: Abstract,
              },
            );
          (mode_body, ctx_body);
        | Some(_)
        | None => (item, ctx)
        };
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      let (body, m) = go'(~ctx=ctx_body, ~ana=mode_body, body, m);
      add(
        ~self=Just(Forall(utpat, body.ty) |> Typ.temp),
        ~co_ctx=body.co_ctx,
        m,
      );
    | Let(p, def, body) =>
      let (p_syn, _) =
        go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~ana=syn, p, m);
      let (def, p_ana_ctx, m, ty_p_ana) =
        if (!is_recursive(ctx, p, def, p_syn.ty)) {
          let (def, m) = go(~ana=p_syn.ty, def, m);
          let ty_p_ana = def.ty;
          let (p_ana', _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          (def, p_ana'.ctx, m, ty_p_ana);
        } else {
          let (def_base, _) = go'(~ctx=p_syn.ctx, ~ana=p_syn.ty, def, m);
          let ty_p_ana = def_base.ty;
          /* Analyze pattern to incorporate def type into ctx */
          let (p_ana', _) =
            go_pat(
              ~is_synswitch=false,
              ~co_ctx=CoCtx.empty,
              ~ana=ty_p_ana,
              p,
              m,
            );
          let def_ctx = p_ana'.ctx;
          let (def_base2, _) = go'(~ctx=def_ctx, ~ana=p_syn.ty, def, m);
          let ana_ty_fn = ((ty_fn1, ty_fn2), ty_p) => {
            Typ.term_of(ty_p) == Unknown(SynSwitch)
            && !Typ.equal(ty_fn1, ty_fn2)
              ? ty_fn1 : ty_p;
          };
          let ana =
            switch (
              (def_base.ty |> Typ.term_of, def_base2.ty |> Typ.term_of),
              p_syn.ty |> Typ.term_of,
            ) {
            | ((Prod(ty_fns1), Prod(ty_fns2)), Prod(ty_ps)) =>
              let tys =
                List.map2(ana_ty_fn, List.combine(ty_fns1, ty_fns2), ty_ps);
              Prod(tys) |> Typ.temp;
            | ((_, _), _) =>
              ana_ty_fn((def_base.ty, def_base2.ty), p_syn.ty)
            };
          let (def, m) = go'(~ctx=def_ctx, ~ana, def, m);
          (def, def_ctx, m, ty_p_ana);
        };
      let (body, m) = go'(~ctx=p_ana_ctx, ~ana, body, m);
      /* add co_ctx to pattern */
      let (p_ana, m) =
        go_pat(~is_synswitch=false, ~co_ctx=body.co_ctx, ~ana=ty_p_ana, p, m);
      // TODO: factor out code
      let unwrapped_self: Self.exp = Common(Just(body.ty));
      let Coverage.{is_exhaustive, _} =
        Coverage.check(
          [Info.pat_constraint(p_ana)],
          Typ.normalize(ctx, p_ana.ty),
        );
      let self =
        is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
      add'(
        ~self,
        ~co_ctx=
          CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
        m,
      );
    | FixF(p, e, _) =>
      let (p', _) =
        go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana, p, m);
      let (e', m) = go'(~ctx=p'.ctx, ~ana=p'.ty, e, m);
      let (p'', m) =
        go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~ana, p, m);
      add(
        ~self=Just(p'.ty),
        ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx)]),
        m,
      );
    | If(e0, e1, e2) =>
      let branch_ids = List.map(Exp.rep_id, [e1, e2]);
      let (cond, m) = go(~ana=Atom(Bool) |> Typ.temp, e0, m);
      let (cons, m) = go(~ana, e1, m);
      let (alt, m) = go(~ana, e2, m);
      add(
        ~self=Self.match(ctx, [cons.ty, alt.ty], branch_ids),
        ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
        m,
      );
    | Match(scrut, rules) =>
      let (scrut, m) = go(~ana=syn, scrut, m);
      let (ps, es) = List.split(rules);
      let branch_ids = List.map(Exp.rep_id, es);
      let (ps', _) =
        map_m(
          go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~ana=scrut.ty),
          ps,
          m,
        );
      let p_ctxs = List.map(Info.pat_ctx, ps');
      let (es, m) =
        List.fold_left2(
          ((es, m), e, ctx) =>
            go'(~ctx, ~ana, e, m) |> (((e, m)) => (es @ [e], m)),
          ([], m),
          es,
          p_ctxs,
        );
      let e_tys = List.map(Info.exp_ty, es);
      let e_co_ctxs =
        List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
      let unwrapped_self: Self.exp =
        Common(Self.match(ctx, e_tys, branch_ids));
      let (constraints, m) =
        List.fold_left(
          (
            (constraints: list(Coverage.Constraint.t), m: Map.t),
            (p, co_ctx),
          ) => {
            let (info, m) =
              go_pat(~is_synswitch=false, ~co_ctx, ~ana=scrut.ty, p, m);
            let p_constraint = Info.pat_constraint(info);
            ([p_constraint, ...constraints], m);
          },
          ([], m),
          List.combine(ps, e_co_ctxs),
        );
      let constraints = List.rev(constraints);

      let normalized_scrut_ty = Typ.normalize(ctx, scrut.ty);
      let Coverage.{is_exhaustive, redundant_rows} =
        Coverage.check(constraints, normalized_scrut_ty);
      let self =
        is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
      let add_redundancy = (ps: list(TermBase.pat_t), redundant_rows, m) => {
        List.fold_left(
          (m, row) => {
            let p = List.nth(ps, row);
            switch (Id.Map.find(IdTagged.rep_id(p), m)) {
            | Info.InfoPat(info) =>
              let info =
                Info.derived_pat(
                  ~upat=info.term,
                  ~ctx=info.ctx,
                  ~co_ctx=info.co_ctx,
                  ~prev_synswitch=info.prev_synswitch,
                  ~ana=info.ana,
                  ~ancestors=info.ancestors,
                  ~self=Self.Redundant(info.self),
                  ~constraint_=info.constraint_,
                  ~label_inference=info.label_inference,
                  ~inferred_label=info.inferred_label,
                  ~label_sort=info.label_sort,
                );
              add_info(IdTagged.ids(p), InfoPat(info), m);
            | _ => failwith("Invalid sort for pattern.")
            };
          },
          m,
          redundant_rows,
        );
      };
      let m = add_redundancy(ps, redundant_rows, m);
      add'(~self, ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs), m);
    | TyAlias(typat, utyp, body) =>
      let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
      switch (typat.term) {
      | Var(name) when !Ctx.shadows_typ(ctx, name) =>
        /* Currently we disallow all type shadowing */
        /* NOTE(andrew): Currently, Typ.to_typ returns Unknown(TypeHole)
           for any type variable reference not in its ctx. So any free variables
           in the definition would be obliterated. But we need to check for free
           variables to decide whether to make a recursive type or not. So we
           tentatively add an abtract type to the ctx, representing the
           speculative rec parameter. */
        let (ty_def, ctx_def, ctx_body) = {
          switch (utyp.term) {
          | Sum(_) when List.mem(name, Typ.free_vars(utyp)) =>
            /* NOTE: When debugging type system issues it may be beneficial to
               use a different name than the alias for the recursive parameter */
            //let ty_rec = Typ.Rec("α", Typ.subst(Var("α"), name, ty_pre));
            let ty_rec = Rec(Var(name) |> TPat.fresh, utyp) |> Typ.temp;
            let ctx_def =
              Ctx.extend_alias(ctx, name, TPat.rep_id(typat), ty_rec);
            (ty_rec, ctx_def, ctx_def);
          | _ => (
              utyp,
              ctx,
              Ctx.extend_alias(ctx, name, TPat.rep_id(typat), utyp),
            )
          /* NOTE(yuchen): Below is an alternative implementation that attempts to
             add a rec whenever type alias is present. It may cause trouble to the
             runtime, so precede with caution. */
          // Typ.lookup_surface(ty_pre)
          //   ? {
          //     let ty_rec = Typ.Rec({item: ty_pre, name});
          //     let ctx_def = Ctx.add_alias(ctx, name, utpat_id(typat), ty_rec);
          //     (ty_rec, ctx_def, ctx_def);
          //   }
          //   : {
          //     let ty = Term.Typ.to_typ(ctx, utyp);
          //     (ty, ctx, Ctx.add_alias(ctx, name, utpat_id(typat), ty));
          //   };
          };
        };
        let ctx_body =
          switch (Typ.get_sum_constructors(ctx, ty_def)) {
          | Some(sm) => Ctx.add_ctrs(ctx_body, name, Typ.rep_id(utyp), sm)
          | None => ctx_body
          };
        let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
          go'(~ctx=ctx_body, ~ana, body, m);
        /* Make sure types don't escape their scope */
        let ty_escape = Typ.subst(ty_def, typat, ty_body);
        let m = utyp_to_info_map(~ctx=ctx_def, ~ancestors, utyp, m) |> snd;
        add(~self=Just(ty_escape), ~co_ctx, m);
      | Var(_)
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) =>
        let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
          go'(~ctx, ~ana, body, m);
        let m = utyp_to_info_map(~ctx, ~ancestors, utyp, m) |> snd;
        add(~self=Just(ty_body), ~co_ctx, m);
      };
    | Use(typ, body) =>
      let (typ, m) = utyp_to_info_map(~ctx, ~ancestors, typ, m);
      let use_mode: option(Operators.mode) =
        switch (typ.term |> Typ.weak_head_normalize(ctx) |> Typ.term_of) {
        | Atom(Nat) => Some(Nat)
        | Atom(Int) => Some(Int)
        | Atom(Float) => Some(Float)
        | Atom(SInt) => Some(SInt)
        | _ => None
        };
      let ctx' =
        switch (use_mode) {
        | Some(mode) => Ctx.set_use_mode(ctx, Some(mode))
        | None => ctx
        };
      let (body, m) = go'(~ctx=ctx', ~ana, body, m);
      let self: Self.exp =
        switch (use_mode) {
        | Some(_) => Common(Just(body.ty))
        | None when Typ.fast_equal(Unknown(Internal) |> Typ.temp, typ.term) =>
          Common(Just(body.ty))
        | None =>
          InvalidUseMode({
            bad_typ: typ.term,
            inner_typ: body.ty,
          })
        };
      add'(~self, ~co_ctx=body.co_ctx, m);
    };
  };

  // This is for lifting single values into a singleton labeled tuple when the label is not present

  switch (Typ.weak_head_normalize(ctx, ana).term) {
  | Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
    // We can flatten this by pulling it up on the case match but since OCaml is strict it'll be evaluated.
    // So for performance reasons we'll just do it here.
    let (e, m) = go(~ana=syn, uexp, m);

    switch (Typ.weak_head_normalize(ctx, e.ty).term) {
    | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
      default_case()
    | _ => autolabel_singleton_tuple(uexp, ana_ty, l1, m)
    };
  | _ => default_case()
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~duplicates: list(string),
      ~expected_labels=?,
      ~ana: Typ.t=Unknown(Internal) |> Typ.temp,
      ~under_ascription: bool=false,
      ~override_self: option(Self.t)=?,
      ~inferred_label=?,
      ~label_sort=false,
      {annotation: {ids, _}, term} as upat: Pat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, ~constraint_, ~label_inference=?, m) => {
    let prev_synswitch =
      switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
      | Some(Info.InfoPat({ana, ty, _})) when Typ.is_syn_plus(ana) =>
        Some(ty)
      | Some(Info.InfoPat({prev_synswitch, _})) => prev_synswitch
      | Some(_)
      | None => None
      };
    let info =
      Info.derived_pat(
        ~prev_synswitch,
        ~upat,
        ~ctx,
        ~co_ctx,
        ~ana,
        ~ancestors,
        ~self=Common(Option.value(~default=self, override_self)),
        ~constraint_,
        ~label_inference,
        ~inferred_label,
        ~label_sort,
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let upat_to_info_map =
      (
        ~is_synswitch,
        ~ctx,
        ~co_ctx,
        ~ancestors,
        ~duplicates=[],
        ~expected_labels=?,
        ~ana,
        ~under_ascription=false,
        ~override_self=?,
        ~inferred_label=?,
        ~label_sort=false,
        upat: Pat.t,
        m: Map.t,
      ) => {
    upat_to_info_map(
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors,
      ~duplicates,
      ~ana,
      ~under_ascription,
      ~override_self?,
      ~inferred_label?,
      ~expected_labels?,
      ~label_sort,
      upat,
      m: Map.t,
    );
  };
  let atomic = (self, constraint_) => add(~self, ~ctx, ~constraint_, m);
  let ancestors = [Pat.rep_id(upat)] @ ancestors;
  let go = (~under_ascription=false) =>
    upat_to_info_map(~under_ascription, ~is_synswitch, ~ancestors, ~co_ctx);
  let unknown = Unknown(is_synswitch ? SynSwitch : Internal) |> Typ.temp;
  let ctx_fold = (ctx: Ctx.t, m, ~duplicates=[]) =>
    List.fold_left2(
      ((ctx, tys, cons, m, info_all), e, ana) =>
        go(~ctx, ~ana, ~duplicates, ~inferred_label?, e, m)
        |> (
          ((info, m)) => (
            info.ctx,
            tys @ [info.ty],
            cons @ [info.constraint_],
            m,
            info_all @ [info],
          )
        ),
      (ctx, [], [], m, []),
    );
  let hole = self => atomic(self, Coverage.Constraint.Hole);

  let elaborate_singleton_tuple = (upat: Pat.t, inner_ty, l, m) => {
    let (term, rewrap) = Pat.unwrap(upat);
    let original_expression = Pat.fresh(term);
    let (original_info, m) =
      upat_to_info_map(
        ~ctx,
        ~co_ctx,
        ~is_synswitch,
        ~ancestors,
        ~ana=inner_ty,
        original_expression,
        m,
      );
    /* Special case for probes, which would otherwise lose their id association here */
    let elaborated_pat =
      switch (term) {
      | Probe(_, p) =>
        rewrap(
          Probe(
            Tuple([
              TupLabel(Label(l) |> Pat.fresh, original_expression)
              |> Pat.fresh,
            ])
            |> Pat.fresh,
            p,
          ),
        )
      | _ =>
        rewrap(
          Tuple([
            TupLabel(Label(l) |> Pat.fresh, original_expression) |> Pat.fresh,
          ]),
        )
      };
    let (info, m) =
      upat_to_info_map(
        ~ctx,
        ~co_ctx,
        ~is_synswitch,
        ~ancestors,
        ~ana,
        elaborated_pat,
        m,
      );

    // We need to keep the original status of the expression to get error messages on the unelaborated expression
    let info = {
      ...info,
      status: original_info.status,
      label_inference:
        Some(
          SingletonLabelInference({
            label: l,
            pre_labeled_info: original_info,
          }),
        ),
    };

    (info, add_info(IdTagged.ids(elaborated_pat), InfoPat(info), m));
  };

  let default_case = () =>
    switch (term) {
    | MultiHole(tms) =>
      let (_, m) = multi(~ctx, ~ancestors, m, tms);
      add(~self=IsMulti, ~ctx, ~constraint_=Coverage.Constraint.Hole, m);
    | Invalid(token) => hole(BadToken(token))
    | EmptyHole => hole(Just(unknown))
    | Atom(c) =>
      let c =
        Operators.replace_literal(c, Typ.is_ana_atom(ana), ctx.use_mode); // Replace literal if necessary due to `use`
      switch (c) {
      | L(Nat(nat)) =>
        atomic(
          Just(Atom(Nat) |> Typ.temp),
          Coverage.Constraint.BigInt(nat),
        )
      | L(Int(int)) =>
        atomic(
          Just(Atom(Int) |> Typ.temp),
          Coverage.Constraint.BigInt(int),
        )
      | L(SInt(int)) =>
        atomic(Just(Atom(SInt) |> Typ.temp), Coverage.Constraint.SInt(int))
      | L(Float(float)) =>
        atomic(
          Just(Atom(Float) |> Typ.temp),
          Coverage.Constraint.Float(float),
        )
      | L(Bool(bool)) =>
        atomic(
          Just(Atom(Bool) |> Typ.temp),
          bool ? Coverage.Constraint.true_ : Coverage.Constraint.false_,
        )
      | L(String(string)) =>
        atomic(
          Just(Atom(String) |> Typ.temp),
          Coverage.Constraint.String(string),
        )
      | R(BadInt(str)) => hole(BadToken(str))
      };
    | ListLit(ps) =>
      let ids = List.map(Pat.rep_id, ps);
      let mode = Typ.matched_list(ctx, ana);
      let modes = List.init(List.length(ps), _ => mode);
      let (ctx, tys, cons, m, _) = ctx_fold(ctx, m, ps, modes);
      let rec cons_fold_list = cs =>
        switch (cs) {
        | [] => Coverage.Constraint.nil
        | [hd, ...tl] => Coverage.Constraint.cons(hd, cons_fold_list(tl))
        };
      add(
        ~self=Self.listlit(~empty=unknown, ctx, tys, ids),
        ~ctx,
        ~constraint_=cons_fold_list(cons),
        m,
      );
    | Cons(hd, tl) =>
      let inner_ty = Typ.matched_list(ctx, ana);
      let (hd, m) = go(~ctx, ~ana=inner_ty, hd, m);
      let (tl, m) =
        go(~ctx=hd.ctx, ~ana=List(inner_ty) |> Typ.fresh, tl, m);
      add(
        ~self=Just(List(hd.ty) |> Typ.temp),
        ~ctx=tl.ctx,
        ~constraint_=Coverage.Constraint.cons(hd.constraint_, tl.constraint_),
        m,
      );
    | Wild => atomic(Just(unknown), Coverage.Constraint.Truth)
    | Var(name) =>
      /* NOTE: The self type assigned to pattern variables (Unknown)
         may be SynSwitch, but SynSwitch is never added to the context;
         Unknown(Internal) is used in this case */
      let ctx_typ =
        Info.fixed_typ_pat(
          ctx,
          ana,
          Common(Just(Unknown(Internal) |> Typ.temp)),
        );
      let entry =
        Ctx.VarEntry({
          name,
          id: Pat.rep_id(upat),
          typ: ctx_typ,
        });
      add(
        ~self=Just(unknown),
        ~ctx=Ctx.extend(ctx, entry),
        ~constraint_=Coverage.Constraint.Truth,
        m,
      );
    | TupLabel(label, p) =>
      let (lab, p, m) =
        switch (Typ.matched_label(ctx, ana)) {
        | Some((labmode, val_mode)) =>
          let label_self: option(Self.t) =
            switch (label.term) {
            | Label(_) => None
            | EmptyHole => None
            | _ => Some(BadLabel(Pat(label)))
            };

          let (lab, m) =
            go(
              ~ctx,
              ~ana=labmode,
              ~override_self=?label_self,
              ~duplicates,
              ~label_sort=true,
              label,
              m,
            );
          let (p, m) = go(~ctx, ~ana=val_mode, ~inferred_label?, p, m);
          (lab, p, m);
        | _ =>
          let (lab, m) =
            go(
              ~ctx,
              ~ana=Unknown(Internal) |> Typ.temp,
              ~label_sort=true,
              ~override_self=?
                switch (label.term, expected_labels) {
                | (Label(name), Some(expected_labels))
                    when !List.mem(name, expected_labels) =>
                  Some(InvalidLabel(name))
                | (Label(_), _)
                | (EmptyHole, _) => None
                | _ => Some(BadLabel(Pat(label)))
                },
              ~duplicates,
              label,
              m,
            );

          let (p, m) =
            go(
              ~ctx,
              ~ana=Unknown(Internal) |> Typ.temp,
              ~inferred_label?,
              p,
              m,
            );
          (lab, p, m);
        };

      let self =
        switch (lab.status) {
        | NotInHole(_) => Self.Just(TupLabel(lab.ty, p.ty) |> Typ.temp)
        | InHole(
            Common(
              Inconsistent(Expectation({syn: {term: Label(name), _}, _})),
            ),
          )
        | InHole(Common(NoType(InvalidLabel(name)))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [],
            invalid_labels: [name],
            typ: TupLabel(Label(name) |> Typ.temp, p.ty) |> Typ.temp,
          })
        | InHole(Common(DuplicateLabel(name, _))) =>
          Self.TupleLabelError({
            malformed_labels: [],
            duplicate_labels: [name],
            invalid_labels: [],
            typ: TupLabel(Label(name) |> Typ.temp, p.ty) |> Typ.temp,
          })
        | InHole(_) =>
          Self.TupleLabelError({
            malformed_labels: [Pat(label)],
            duplicate_labels: [],
            invalid_labels: [],
            typ: TupLabel(Unknown(Internal) |> Typ.temp, p.ty) |> Typ.temp,
          })
        };
      add(
        ~self,
        ~ctx=p.ctx,
        ~constraint_=Coverage.Constraint.Tuple([p.constraint_]),
        m,
      );
    | Tuple(ps) =>
      let expected_labels =
        switch (Typ.weak_head_normalize(ctx, ana).term) {
        | Prod(ts) =>
          Some(
            List.filter_map(
              t => Typ.match_tup_label(t) |> Option.map(fst),
              ts,
            ),
          )
        | _ => None
        };

      let original_labels =
        List.map(p => Pat.match_tup_label(p) |> Option.map(fst), ps);

      let (inferred_ps, modes) =
        Typ.matched_prod(
          ctx,
          List.map(p => (None: option(string), p), ps),
          ((inferred, p)) => {
            Pat.match_tup_label(p)
            |> Option.map(((label, value)) => (label, (inferred, value)))
          },
          ana,
          (name, (_, p)) =>
            (
              Some(name),
              TupLabel(Label(name) |> Pat.fresh, p) |> Pat.fresh,
            ),
        );
      let ps = List.map(snd, inferred_ps);
      let inferred = List.map(fst, inferred_ps);

      let new_labels =
        List.map(p => Pat.match_tup_label(p) |> Option.map(fst), ps);
      let duplicate_labels =
        LabeledTuple.get_duplicate_labels(Pat.match_tup_label, ps);
      let (ctx, tys, cons, m, info_pats) =
        List.fold_left2(
          ((ctx, tys, cons, m, info_all), (inferred_label, e), ana) =>
            go(
              ~ctx,
              ~ana,
              ~inferred_label?,
              ~duplicates=duplicate_labels,
              ~expected_labels?,
              e,
              m,
            )
            |> (
              ((info, m)) => (
                info.ctx,
                tys @ [info.ty],
                cons @ [info.constraint_],
                m,
                info_all @ [info],
              )
            ),
          (ctx, [], [], m, []),
          List.combine(inferred, ps),
          modes,
        );
      let constraint_ = Coverage.Constraint.Tuple(cons);
      let (malformed_labels, duplicate_labels, invalid_labels) =
        List.fold_left(
          ((a, b, c), e: Info.pat) => {
            switch (e.status) {
            | InHole(
                Common(
                  TupleLabelError({
                    malformed_labels,
                    duplicate_labels,
                    invalid_labels,
                    _,
                  }),
                ),
              ) => (
                a @ malformed_labels,
                b @ duplicate_labels,
                c @ invalid_labels,
              )
            | _ => (a, b, c)
            }
          },
          ([], [], []),
          info_pats,
        );
      let self =
        List.is_empty(malformed_labels)
        && List.is_empty(duplicate_labels)
        && List.is_empty(invalid_labels)
          ? Self.Just(Prod(tys) |> Typ.temp)
          : Self.TupleLabelError({
              malformed_labels,
              duplicate_labels,
              invalid_labels,
              typ: Prod(tys) |> Typ.temp,
            });

      add(
        ~self,
        ~ctx,
        ~constraint_,
        ~label_inference=
          Info.derive_label_inference_info(original_labels, new_labels),
        m,
      );
    | Label(name) =>
      let self = Self.Just(Label(name) |> Typ.temp);
      List.exists(l => name == l, duplicates)
        ? atomic(Duplicate(name, self), Coverage.Constraint.Truth)
        : atomic(self, Coverage.Constraint.Truth);
    | Parens(p)
    | Probe(p, _) =>
      let (p, m) = go(~ctx, ~ana, p, m);
      add(~self=Just(p.ty), ~ctx=p.ctx, ~constraint_=p.constraint_, m);
    | Constructor(ctr, ty) =>
      let self = Self.of_ctr(ctx, ctr, ana, ty);
      atomic(self, Coverage.Constraint.Ap(ctr, None));
    | Ap(fn, arg) =>
      let ctr = Pat.ctr_name(fn);
      let fn_ana = Arrow(Unknown(SynSwitch) |> Typ.temp, ana) |> Typ.temp;
      let (fn', m) = go(~ctx, ~ana=fn_ana, fn, m);
      let m = {
        switch (fn |> Pat.term_of) {
        | Constructor(_) => m
        | _ =>
          let info =
            Info.derived_pat(
              ~upat=fn'.term,
              ~ctx=fn'.ctx,
              ~co_ctx=fn'.co_ctx,
              ~prev_synswitch=fn'.prev_synswitch,
              ~ana=fn'.ana,
              ~ancestors=fn'.ancestors,
              ~self=Self.ExpectedConstructor(fn'.self),
              ~constraint_=fn'.constraint_,
              ~label_inference=fn'.label_inference,
              ~inferred_label=fn'.inferred_label,
              ~label_sort=fn'.label_sort,
            );
          add_info(IdTagged.ids(fn), InfoPat(info), m);
        };
      };
      let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn'.ty);
      let (arg, m) = go(~ctx, ~ana=ty_in, arg, m);
      let constraint_ =
        switch (ctr) {
        | Some(ctr) => Coverage.Constraint.Ap(ctr, Some(arg.constraint_))
        | None => Coverage.Constraint.Hole
        };
      add(~self=Just(ty_out), ~ctx=arg.ctx, ~constraint_, m);
    | Cast(p, ann, _) =>
      let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
      let (p, m) = go(~ctx, ~under_ascription=true, ~ana=ann.term, p, m);
      add(~self=Just(ann.term), ~ctx=p.ctx, ~constraint_=p.constraint_, m);
    };

  // This is to allow lifting single values into a singleton labeled tuple when the label is not present
  if (under_ascription) {
    default_case();
  } else {
    switch (Typ.weak_head_normalize(ctx, ana).term) {
    | Prod([{term: TupLabel({term: Label(l1), _}, ana_ty), _}]) =>
      // We can flatten this by pulling it up on the case match but since OCaml is strict it'll be evaluated.
      // So for performance reasons we'll just do it here.
      let (e, m) = go(~ana=syn, ~ctx, upat, m);

      switch (Typ.weak_head_normalize(ctx, e.ty).term) {
      | Prod([{term: TupLabel({term: Label(l2), _}, _), _}]) when l1 == l2 =>
        default_case()
      | _ => elaborate_singleton_tuple(upat, ana_ty, l1, m)
      };
    | _ => default_case()
    };
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {annotation: {ids, _}, term} as utyp: Typ.t,
      m: Map.t,
    )
    : (Info.typ, Map.t) => {
  let add' = (~expects=expects, ~utyp=utyp, m) => {
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  };
  let add = (~utyp=utyp, m) => add'(~utyp, m);
  let ancestors = [Typ.rep_id(utyp)] @ ancestors;
  let go' = utyp_to_info_map(~ctx, ~ancestors);
  let go = go'(~expects=TypeExpected);
  switch (term) {
  | Unknown(Hole(MultiHole(tms))) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Unknown(_)
  | Atom(_) => add(m)
  | Var(_) =>
    /* Names are resolved in Info.status_typ */
    add(m)
  | List(t)
  | Parens(t) => add(go(t, m) |> snd)
  | Arrow(t1, t2) =>
    let m = go(t1, m) |> snd;
    let m = go(t2, m) |> snd;
    add(m);
  | Prod(ts) =>
    let duplicate_labels =
      LabeledTuple.get_duplicate_labels(Typ.match_tup_label, ts);
    let m =
      List.is_empty(duplicate_labels)
        ? map_m(go, ts, m) |> snd
        : map_m(
            go'(~expects=LabelExpected(Duplicate, duplicate_labels)),
            ts,
            m,
          )
          |> snd;
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  | TupLabel(label, t) =>
    let expects_label =
      switch (expects) {
      | LabelExpected(_) => expects
      | _ => LabelExpected(Unique, [])
      };
    let m = go'(~expects=expects_label, label, m) |> snd;
    let m = go(t, m) |> snd;
    add'(~expects=TypeExpected, m);
  | Label(_) => add(m)
  | Ap(t1, t2) =>
    let t1_mode: Info.typ_expects =
      switch (expects) {
      | VariantExpected(m, sum_ty) =>
        ConstructorExpected(m, Arrow(t2, sum_ty) |> Typ.temp)
      | _ =>
        ConstructorExpected(
          Unique,
          Arrow(t2, Unknown(Internal) |> Typ.temp) |> Typ.temp,
        )
      };
    let m = go'(~expects=t1_mode, t1, m) |> snd;
    let m = go'(~expects=TypeExpected, t2, m) |> snd;
    add(m);
  | Sum(variants) =>
    let (m, _) =
      List.fold_left(
        variant_to_info_map(~ctx, ~ancestors, ~ty_sum=utyp),
        (m, []),
        variants,
      );
    add(m);
  | Forall({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Abstract,
        },
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Forall(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec({term: Var(name), _} as utpat, tbody) =>
    let body_ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Singleton(utyp),
        },
      );
    let m =
      utyp_to_info_map(
        tbody,
        ~ctx=body_ctx,
        ~ancestors,
        ~expects=TypeExpected,
        m,
      )
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  | Rec(utpat, tbody) =>
    let m =
      utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
      |> snd;
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    add(m); // TODO: check with andrew
  };
}
and utpat_to_info_map =
    (
      ~ctx,
      ~ancestors,
      {annotation: {ids, _}, term} as utpat: TPat.t,
      m: Map.t,
    )
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [TPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  };
}
and variant_to_info_map =
    (
      ~ctx,
      ~ancestors,
      ~ty_sum,
      (m, ctrs),
      uty: ConstructorMap.variant(Typ.t),
    ) => {
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, ctrs);
  | Variant(ctr, ids, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {
          term: Var(ctr),
          annotation: {
            ids: ids,
          },
        },
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [ctr, ...ctrs]);
  };
};

let mk =
  Core.Memo.general(~cache_size_bound=1000, (ctx, e) => {
    uexp_to_info_map(
      ~ctx,
      ~ancestors=[],
      ~duplicates=[],
      ~expected_labels=None,
      ~label_sort=false,
      e,
      Id.Map.empty,
    )
    |> snd
  });

let mk = (core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ctx, exp) : Id.Map.empty;

let get_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};

let get_pat_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoPat(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};
