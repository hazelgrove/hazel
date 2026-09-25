open Util;

/* User-defined livelits. A definition is a module declaring three types
   and four members:

     let ^name = {
       type Model = ...;                the livelit's internal state
       type Action = ...;               what the GUI emits
       type Expansion = ...;            what a use MEANS to the program
       let init : Model = ...;          initial model, inserted on ^name<space>
       let update = fun (m, a) -> ...;  (Model, Action) => Model
       let view = fun m -> ...;         Model => HTML, handlers emit Actions
       let expand = Functional(fun m -> ...)   or Macro(...)
     } in ...

   The three type members are the livelit's interface, and all three are
   required. `Expansion` in particular is what clients type against: a use
   of ^name synthesizes Expansion whatever the expansion turns out to be,
   which is the abstract reasoning principle of the livelits paper. The
   obligation that buys it is discharged at each use, where statics types
   the expansion and marks the use with BadLivelitExpansion if its type is
   inconsistent with the declared one (the LivelitName case of Statics.re).
   Checking per use rather than once per definition is the paper's own
   strategy (PLDI 2021, S3.2.5), not an approximation of it: the expansion
   is validated at each invocation site, with errors reported to the client.

   Splices are the part of the paper still absent. When they arrive as a
   SpliceRef type with operations over it, `expand` extends to return a
   pair whose second component is the list of SpliceRefs, and the check
   here becomes a check of that pair's parameterized first component. With
   the splice list empty it degenerates to what this file does.

   Optional member `shape = Inline(w) | Block(w, h) | Tab(w, h)` (a
   LivelitShape) sets the projector's footprint in character cells. Helpers
   are ordinary additional members.

   Expansion and view instrumentation are built syntactically here — no
   evaluation during statics. A projected use's view runs in the main
   evaluation (instrument_view below) and the projector renders the sampled
   HTML; `update` runs at event time in the builtin environment via
   `user_def`, so definitions should be closed, with helpers among their
   members. */

let is_livelit_name = (name: string): bool =>
  String.length(name) > 1 && name.[0] == '^';

/* The livelit bound by this let pattern, if any (bare name, no caret) */
let rec binder_name = (p: TermBase.Pat.t): option(string) =>
  switch (p.term) {
  | Parens(p)
  | Asc(p, _) => binder_name(p)
  | Var(name) when is_livelit_name(name) =>
    Some(String.sub(name, 1, String.length(name) - 1))
  | _ => None
  };

let rec strip_parens = (e: TermBase.Exp.t): TermBase.Exp.t =>
  switch (e.term) {
  | Parens(e) => strip_parens(e)
  | _ => e
  };

let rec pat_name = (p: TermBase.Pat.t): option(string) =>
  switch (p.term) {
  | Parens(p)
  | Asc(p, _) => pat_name(p)
  /* funlet member (`let view(m) = ...`): the head var names the member;
     constructor heads fall through to None */
  | Ap(fn, _) => pat_name(fn)
  | Var(name) => Some(name)
  | _ => None
  };

/* A well-formed definition: the four required members (plus any helpers and
   the optional `shape`) and the three declared interface types. */
[@deriving show({with_path: false})]
type def = {
  members: list((string, TermBase.Exp.t)), /* member -> bound syntax */
  model_t: TermBase.Typ.t,
  action_t: TermBase.Typ.t,
  expansion_t: TermBase.Typ.t,
};

let required_members = ["init", "update", "view", "expand"];
let required_types = ["Model", "Action", "Expansion"];

/* Module members, in order; a repeated name keeps the LAST binding, matching
   module shadowing semantics. */
let module_members =
    (items: list(TermBase.Mod.t)): list((string, TermBase.Exp.t)) =>
  List.fold_left(
    (acc, item: TermBase.Mod.t) =>
      switch (item.term) {
      | ModLet(p, e) =>
        switch (pat_name(p)) {
        | Some(name) => [(name, e), ...List.remove_assoc(name, acc)]
        | None => acc
        }
      | _ => acc
      },
    [],
    items,
  );

let missing = (required: list(string), have: list((string, 'a))) =>
  List.filter(r => !List.mem_assoc(r, have), required);

/* Check the definition's members against the builtin `Livelit` signature
   (BuiltinsADT.livelit, in scope as the type alias `Livelit`), which is the
   one place the livelit interface is written down. There is ONE signature:
   whether a livelit is functional or macro is carried by which arm of the
   `expand` sum it inhabits, not by which signature it answers to.

   The signature declares Model, Action and Expansion abstract; here they are
   REALIZED by this definition's own manifest types, and each required value
   member is then checked at the type the signature gives it. So `expand` is
   checked as `Model -> Expansion` with this livelit's actual types -- the
   definition-site half of the obligation that PLDI 2021 discharges only at
   each use. The use-site check stays: it is what catches an expansion whose
   type depends on the model VALUE, which no definition-site check can see.

   Consistency, not equality: a member may be more precise than declared, and
   a member still containing holes must not be reported as wrong. */
/* `init` supplies VALUES; the use site supplies REFS.

   A spliced model field has type (ref=SpliceRef, value=t), but `init` is
   written before any splice exists -- there is nothing for it to name, and
   making it a command so it could is precisely what Figure 3 does and we
   have not. So when checking `init` against Model, a spliced field is
   compared at its value type alone. Every other member (update, view,
   expand) sees the pair, because by then the use site has made it. */
let rec strip_splice_refs = (ty: Typ.t): Typ.t => {
  let is_ref = (t: Typ.t) =>
    switch (Typ.term_of(t)) {
    | Var("SpliceRef") => true
    | _ => false
    };
  /* (ref=SpliceRef, value=t)  ~>  t */
  let value_of = (t: Typ.t): option(Typ.t) =>
    switch (Typ.term_of(t)) {
    | Prod(fields) =>
      let named = n =>
        List.find_map(
          (f: Typ.t) =>
            switch (Typ.term_of(f)) {
            | TupLabel(l, v) =>
              switch (Typ.term_of(l)) {
              | Label(x) when x == n => Some(v)
              | _ => None
              }
            | _ => None
            },
          fields,
        );
      switch (named("ref"), named("value")) {
      | (Some(r), Some(v)) when is_ref(r) => Some(v)
      | _ => None
      };
    | _ => None
    };
  switch (Typ.term_of(ty)) {
  | Prod(fields) =>
    Typ.fresh(
      Prod(
        List.map(
          (f: Typ.t) =>
            switch (Typ.term_of(f)) {
            | TupLabel(l, v) =>
              switch (value_of(v)) {
              | Some(inner) => Typ.fresh(TupLabel(l, inner))
              | None => f
              }
            | _ => f
            },
          fields,
        ),
      ),
    )
  | Parens(inner) => Typ.fresh(Parens(strip_splice_refs(inner)))
  | _ => ty
  };
};

/* The `Livelit` signature with THIS definition's Model, Action and
   Expansion made MANIFEST rather than abstract.

   Analyzing a livelit definition against the signature as written would
   SEAL those three, and a use of ^name must keep synthesizing Expansion
   concretely or clients cannot reason about what a use means. Realizing
   them first keeps the concrete types visible while still putting every
   member in ANALYTIC position, which is the whole point: a member is then
   checked where it is written, by the ordinary type machinery, rather
   than synthesized and compared afterwards by a hand-rolled check.

   Two things fall out of the analytic position that were awkward without
   it. Constructors resolve -- `let expand = Functional(f)` needs
   `Functional` in scope, and analysis against the sum supplies it, so a
   livelit needs no `type Expand` member of its own. And a mismatched
   member reports as an ordinary inconsistency at the offending
   expression rather than as a livelit-specific mark on the whole
   definition. */
let realized_livelit_sig =
    (~ctx: Ctx.t, ~types: list((string, Typ.t))): option(Typ.t) =>
  switch (Ctx.lookup_alias(ctx, "Livelit")) {
  | Some(ty) =>
    switch (Typ.term_of(ty)) {
    | Sig(items) =>
      let realize = (t: Typ.t): Typ.t =>
        List.fold_left(
          (t, name) =>
            switch (List.assoc_opt(name, types)) {
            | Some(d) =>
              Typ.subst(d, IdTagged.FreshGrammar.TPat.var(name), t)
            | None => t
            },
          t,
          required_types,
        );
      let members =
        Sig.members(items)
        |> List.map((mem: Sig.member) =>
             switch (mem) {
             | TypeAbstract(n) =>
               switch (List.assoc_opt(n, types)) {
               | Some(d) => Sig.TypeManifest(n, d)
               | None => mem
               }
             | Val(n, t) => Sig.Val(n, realize(t))
             | _ => mem
             }
           );
      Some(
        IdTagged.FreshGrammar.Typ.sig_(
          List.map(Sig.item_of_member, members),
        ),
      );
    | _ => None
    }
  | None => None
  };

/* The definition is the trailing module, looking through helper bindings:
   `let helper = ... in {...}`. A helper type alias is brought into scope on
   the way down, so a member type may be stated in terms of it. */
let rec detect =
        (~ctx: Ctx.t, ~m: StaticsBase.Map.t, def: TermBase.Exp.t)
        : result(def, Mark.t) =>
  switch (strip_parens(def).term) {
  | Let(_, _, body) => detect(~ctx, ~m, body)
  | TyAlias(tp, ty, body) =>
    let ctx =
      switch (tp.term) {
      | Var(name) => Ctx.extend_alias(ctx, name, TPat.rep_id(tp), ty)
      | _ => ctx
      };
    detect(~ctx, ~m, body);
  | Module(items) =>
    let members = module_members(items);
    /* The module's members, read from the signature Modules II synthesizes
       for it, so a member type may name an earlier one (`type Expansion =
       Model`) exactly as it does for `M.Expansion`. The statics map is
       supplied so VALUE members carry their synthesized types too: without
       it they come out Unknown, and checking them against the Livelit
       signature would pass vacuously. */
    let sig_members =
      switch (ModuleHelpers.module_sig_type(~ctx, items, m).term) {
      | Sig(sig_items) => Sig.members(sig_items)
      | _ => []
      };
    let types =
      sig_members
      |> List.filter_map((mem: Sig.member) =>
           switch (mem) {
           | TypeManifest(n, ty) => Some((n, ty))
           | _ => None
           }
         );
    switch (
      missing(required_members, members),
      missing(required_types, types),
    ) {
    /* The module system already says this, and says it for value and type
       members alike -- ModuleHelpers.member_names is value_names @
       type_names. A livelit that lacks `update` is a module missing a
       member, and should read like one rather than like a livelit-specific
       diagnostic. Value members are named first so the message reads in
       the order an author would fix them. */
    | ([_, ..._] as ms, ts) => Error(Mark.ModuleMissingMembers(ms @ ts))
    | ([], [_, ..._] as ts) => Error(Mark.ModuleMissingMembers(ts))
    | ([], []) =>
      Ok({
        members,
        model_t: List.assoc("Model", types),
        action_t: List.assoc("Action", types),
        expansion_t: List.assoc("Expansion", types),
      })
    };
  | _ => Error(Mark.InvalidLivelitDef(DefNotModule))
  };

/* The type a livelit definition should be ANALYZED against, if it is
   well-formed enough to say. Statics uses this for the second pass over a
   `let ^name = ...` definition: the first pass synthesizes, which is what
   tells us the definition's own Model, Action and Expansion, and this
   turns those into the realized signature the second pass analyzes
   against. Returns None when the definition is too broken to realize --
   not a module, or missing a type member -- in which case the first
   pass's own marks are what the author gets. */
let livelit_ana_ty =
    (~ctx: Ctx.t, ~m: StaticsBase.Map.t, def: TermBase.Exp.t)
    : option(TermBase.Typ.t) =>
  switch (detect(~ctx, ~m, def)) {
  | Error(_) => None
  | Ok({model_t, action_t, expansion_t, _}) =>
    realized_livelit_sig(
      ~ctx,
      ~types=[
        ("Model", model_t),
        ("Action", action_t),
        ("Expansion", expansion_t),
      ],
    )
  };

let unknown = () => IdTagged.FreshGrammar.Typ.unknown(Internal);

/* The `shape` member: a LivelitShape constructor. Inline(w) is one
   line; Block(w, h) / Tab(w, h) are h LINES tall (the internal
   vertical counts linebreaks, hence h - 1). */
let shape_of = (e: TermBase.Exp.t): option(ProjectorShape.t) => {
  let int_of = w =>
    switch (strip_parens(w).term) {
    | Atom(Int(n)) => Bigint.to_int(n)
    | _ => None
    };
  let pair_of = arg =>
    switch (strip_parens(arg).term) {
    | Tuple([w, h]) =>
      switch (int_of(w), int_of(h)) {
      | (Some(w), Some(h)) => Some((w, h))
      | _ => None
      }
    | _ => None
    };
  switch (strip_parens(e).term) {
  | Ap(_, ctr, arg) =>
    switch (strip_parens(ctr).term) {
    | Constructor("Inline", _) =>
      int_of(arg)
      |> Option.map(w =>
           {
             ProjectorShape.horizontal: w,
             vertical: Inline,
           }
         )
    | Constructor("Block", _) =>
      pair_of(arg)
      |> Option.map(((w, h)) =>
           {
             ProjectorShape.horizontal: w,
             vertical: h <= 1 ? Inline : Block(h - 1),
           }
         )
    | Constructor("Tab", _) =>
      pair_of(arg)
      |> Option.map(((w, h)) =>
           {
             ProjectorShape.horizontal: w,
             vertical: h <= 1 ? Inline : Tab(h - 1),
           }
         )
    | _ => None
    }
  | _ => None
  };
};

let default_shape: ProjectorShape.t = {
  horizontal: 24,
  vertical: Inline,
};

/* The expansion of `^name(model)`: fetch the expand member from the runtime
   binding and apply it to the model. Scoping comes for free: `^name`
   resolves to the nearest enclosing livelit let. Note that this reaches the
   member through the ordinary `Var` binding, not the `^name.expand` surface
   form, so typing it consults the definition's ACTUAL expand member rather
   than the interface `member_ty` advertises — which is what makes the
   use-site expansion check below non-vacuous. */
/* A spliced model field carries its REF as well as its value.

   A field the author marked with parens holds a splice: the client's own
   code, living inside the widget. Figure 3 puts a HANDLE to that code in
   the model, so a marked field reads as

     (ref = SpliceRef("<id>"), value = <the code>)

   rather than just the code. A view can then place the splice by naming
   it -- `Html.splice(m.lo.ref)` -- instead of counting positions, and
   still read what it evaluates to as `m.lo.value`.

   This is a rewrite of the model ARGUMENT, applied before analysis, not a
   rule about splices. Splice transparency is load-bearing elsewhere (a
   table infers its headers through it) and is left alone. The value
   component keeps the splice, so the client's code is still typed in the
   client's scope and still evaluates in place.

   What this is NOT: the paper reads a splice with
   `eval_splice : SpliceRef -> ViewCmd(Maybe(Result))`, which can answer
   Indet for a bound that does not reduce. Here the value simply rides
   along, eagerly, and there is no way to say "this one has no value" --
   which is why an unreducible bound renders as a hole rather than as
   something the widget chose to show. */
let expose_splice_refs = (arg: TermBase.Exp.t): TermBase.Exp.t => {
  open IdTagged.FreshGrammar;
  let mk_ref = (id: Id.t): TermBase.Exp.t =>
    Exp.ap(
      Forward,
      Exp.constructor("SpliceRef", None),
      Exp.string(Id.to_string(id)),
    );
  /* The splice under any parens the author wrote, with its id. */
  let rec find_splice = (e: TermBase.Exp.t): option(Id.t) =>
    switch (e.term) {
    | Splice(_) => Some(IdTagged.rep_id(e))
    | Parens(inner) => find_splice(inner)
    | _ => None
    };
  let expose_field = (x: TermBase.Exp.t): TermBase.Exp.t =>
    switch (x.term) {
    | TupLabel(l, v) =>
      switch (find_splice(v)) {
      | None => x
      | Some(id) => {
          ...x,
          term:
            TupLabel(
              l,
              Exp.tuple([
                Exp.tup_label(Exp.label("ref"), mk_ref(id)),
                Exp.tup_label(Exp.label("value"), v),
              ]),
            ),
        }
      }
    | _ => x
    };
  let rec go = (e: TermBase.Exp.t): TermBase.Exp.t =>
    switch (e.term) {
    | Parens(inner) => {
        ...e,
        term: Parens(go(inner)),
      }
    | Tuple(xs) => {
        ...e,
        term: Tuple(List.map(expose_field, xs)),
      }
    | _ => e
    };
  go(arg);
};

/* The elaboration of a use: discriminate on which arm of `expand` this
   livelit committed to, then apply it.

   `expand` is a SUM now, not a function, so the use site cannot just
   apply it -- it has to ask which kind of livelit this is. That question
   used to be answered by which SIGNATURE the definition satisfied; it is
   now answered by the value, here.

   This is the elaboration, not program text (Statics.re threads it as
   ~elab_term), so the `case` is invisible to the author.

   The Macro arm elaborates to a hole ASCRIBED to Expansion. A Macro
   expansion cannot produce a value while `Exp` is an uninhabited
   placeholder, and a hole is the honest rendering of "committed to a kind
   that does not work yet" -- incomplete rather than ill-typed.

   The ascription is load-bearing, not decoration. A bare hole types as ?,
   the case's type is the join of its arms, and ? joins to ? -- so the
   whole elaboration became consistent with EVERY type and the use-site
   BadLivelitExpansion check silently stopped firing. Two tests caught
   that. Ascribing the hole keeps both arms at Expansion, which is what
   the use site is entitled to assume whichever arm ran. */
let mk_expand_dot =
    (~name: string, ~expansion_t: TermBase.Typ.t, model: TermBase.Exp.t) => {
  IdTagged.FreshGrammar.(
    Some(
      Exp.match(
        Exp.dot(Exp.var("^" ++ name), Exp.label("expand")),
        [
          (
            Pat.ap(Pat.constructor("Functional", None), Pat.var("f")),
            Exp.ap(Operators.Forward, Exp.var("f"), model),
          ),
          (
            Pat.ap(Pat.constructor("Macro", None), Pat.var("_g")),
            Exp.asc(Exp.empty_hole(), expansion_t),
          ),
        ],
      ),
    )
  );
};

let is_user_livelit = (ctx: Ctx.t, name: string): bool =>
  switch (Ctx.lookup_livelit(ctx, name)) {
  | Some({user_def: Some(_), _}) => true
  | _ => false
  };

/* Surface member access (^name.member) types against the livelit's DECLARED
   interface (Model, Action, Expansion), not against the definition's actual
   members — the same abstraction a use of ^name gets. */
let member_ty = (ctx: Ctx.t, name: string, member: string): TermBase.Typ.t =>
  switch (Ctx.lookup_livelit(ctx, name)) {
  | Some({model_t, action_t, expansion_t, _}) =>
    IdTagged.FreshGrammar.(
      switch (member) {
      /* Figure 3, and the same builders the signature uses, so the two
         cannot drift: update and view are commands now, not functions
         returning values. `view` is absent from this switch on purpose --
         it was never listed, and the fallthrough gave it `unknown`, which
         is why a wrong view went unreported. Listing it is the fix. */
      | "update" =>
        Typ.arrow(
          model_t,
          Typ.arrow(action_t, BuiltinsADT.update_cmd(model_t)),
        )
      | "view" =>
        Typ.arrow(
          model_t,
          BuiltinsADT.view_cmd(BuiltinsADT.HtmlModules.path("Html", "T")),
        )
      /* The sum itself, not one arm of it: ^name.expand is the value the
         definition committed with, and a client reading it sees which
         kind of livelit this is. Same builder as the signature, with this
         livelit's concrete types substituted for the abstract ones. */
      | "expand" =>
        BuiltinsADT.livelit_expand_typ(~model=model_t, ~expansion=expansion_t)
      | "init" => model_t
      | _ => unknown()
      }
    )
  | None => unknown()
  };

/* A projected use of a user-defined livelit: (bare name, model term) */
let use_parts =
    (ctx: Ctx.t, use: TermBase.Exp.t): option((string, TermBase.Exp.t)) =>
  switch (strip_parens(use).term) {
  | Ap(_, {term: LivelitName(name), _}, model) =>
    switch (Ctx.lookup_livelit(ctx, name)) {
    | Some({user_def: Some(_), _}) => Some((name, model))
    | _ => None
    }
  | _ => None
  };

/* View fold-in: a projected use also computes view(model) in the main run,
   discarded by the program but sampled at the projector's id — the same id
   the projector's dynamics probe watches — so the projector can render the
   live HTML without evaluating anything itself. The model is bound once
   (`%model`, not a lexable token) and shared between the view call and the
   expansion, so a committed ^name.update(m, a) transition runs — and its
   probes fire — exactly once. The model keeps its surface ids as the
   binding's definition, so its value samples at the model's own id. */
let instrument_view =
    (
      ~projector_id: Id.t,
      ~name: string,
      ~model: TermBase.Exp.t,
      body: TermBase.Exp.t,
    )
    : TermBase.Exp.t => {
  let model_id = Exp.rep_id(model);
  let m_var = "%model";
  let m_ref = () => IdTagged.FreshGrammar.Exp.var(m_var);
  let body =
    Exp.map_term(
      ~f_exp=
        (continue, e) => Exp.rep_id(e) == model_id ? m_ref() : continue(e),
      body,
    );
  IdTagged.FreshGrammar.(
    {
      let view_ap =
        IdTagged.mk_internal(
          [projector_id],
          Grammar.Ap(
            Operators.Forward,
            Exp.dot(Exp.var("^" ++ name), Exp.label("view")),
            m_ref(),
          ): TermBase.Exp.term,
        );
      Exp.let_(Pat.var(m_var), model, Exp.let_(Pat.wild(), view_ap, body));
    }
  );
};

let mk =
    (
      ~ctx: Ctx.t,
      ~m: StaticsBase.Map.t,
      ~name: string,
      ~id: Id.t,
      ~def_user: TermBase.Exp.t,
      ~def_elab: TermBase.Exp.t,
    )
    : (option(LivelitCtx.raw_livelit), list(Mark.t)) =>
  switch (detect(~ctx, ~m, def_user)) {
  | Error(mark) => (None, [mark])
  | Ok({members, model_t, action_t, expansion_t}) => (
      Some({
        LivelitCtx.name,
        id,
        model_t,
        model_default: Exp.replace_all_ids(List.assoc("init", members)),
        expansion_t,
        expand: mk_expand_dot(~name, ~expansion_t),
        action_t,
        update: (_action, model) => model,
        view: (_model, _send) =>
          Virtual_dom.Vdom.Node.text("user-defined livelit"),
        shape:
          switch (Option.bind(List.assoc_opt("shape", members), shape_of)) {
          | Some(shape) => shape
          | None => default_shape
          },
        user_def: Some(def_elab),
      }),
      /* A member whose type is wrong is reported by the second analytic
         pass, as an ordinary inconsistency where it is written, and does
         NOT stop the livelit being bound: its uses keep resolving and keep
         being checked themselves. */
      [],
    )
  };

/* The use-site expansion obligation: a use of ^name synthesizes the DECLARED
   expansion type, so statics owes a check that the expansion actually has
   that type. `actual` is the type the expansion synthesizes on its own; a
   mark is due when the two are inconsistent. Consistency, not equality, is
   the test: an expansion that synthesizes Unknown (an unannotated `expand`,
   or a builtin livelit generating a hole) stays gradual, exactly as it would
   anywhere else in the language. */
let expansion_mark =
    (ctx: Ctx.t, ~declared: TermBase.Typ.t, ~actual: TermBase.Typ.t)
    : list(Mark.t) =>
  Typ.is_consistent(ctx, declared, actual)
    ? []
    : [
      Mark.BadLivelitExpansion({
        declared,
        actual,
      }),
    ];
