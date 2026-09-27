open Util;

/* User-defined livelits. A definition is a module declaring three types
   and four members:

     let ^name = {
       type Model = ...;                the livelit's internal state
       type Action = ...;               what the GUI emits
       type Expansion = ...;            what a use MEANS to the program
       let init : Model = ...;          initial model, inserted on ^name<space>
       let update = fun m -> fun a -> ...;   Model -> Action -> UpdateCmd(Model)
       let view = fun m -> ...;         Model -> ViewCmd(Html.T)
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

   update and view answer with commands (Sec. 3.2.3-3.2.4), performed by
   UpdateCmdRunner and ViewCmdRunner. A model holds SpliceRefs, made by
   new_splice (Sec. 3.2.1) and kept in the program text as the splices
   themselves, at the refs' positions in the use's model argument:
   expose_splice_refs decodes them. The Macro arm returns quoted code and
   the splice list (Sec. 3.2.5); it cannot return yet, as Exp is empty.

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

/* The argument of an elaborated application, `^a(args)` elaborated,
   looking through parens. */
let rec ap_arg = (e: TermBase.Exp.t): option(TermBase.Exp.t) =>
  switch (e.term) {
  | Ap(_, _, arg) => Some(arg)
  | Parens(e) => ap_arg(e)
  | _ => None
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
  /* The type parameter of a `typfun A -> { ... }` definition. */
  tparam: option(string),
  /* Whether it is a `fun p -> { ... }` definition, taking values. */
  vparam: bool,
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
/* The two shapes a spliced model field can have.

   A bare SpliceRef is Figure 3's: the model holds only a handle (l.3-4),
   and the view reads the code behind it with eval_splice. The pair
   (ref=SpliceRef, value=t) is an older stopgap: the value rode beside the
   ref, from before a Macro could return quoted code, since a Functional
   expand cannot eval_splice. No shipped slide uses it now; it is kept so
   programs written that way still load. */
let rec is_splice_ref_ty = (t: Typ.t): bool =>
  switch (Typ.term_of(t)) {
  | Var("SpliceRef") => true
  | Parens(t) => is_splice_ref_ty(t)
  | _ => false
  };

let labeled_ty = (fields: list(Typ.t), n: string): option(Typ.t) =>
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

/* (ref=SpliceRef, value=t)  ~>  Some(t) */
let rec pair_value_ty = (t: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(t)) {
  | Parens(t) => pair_value_ty(t)
  | Prod(fields) =>
    switch (labeled_ty(fields, "ref"), labeled_ty(fields, "value")) {
    | (Some(r), Some(v)) when is_splice_ref_ty(r) => Some(v)
    | _ => None
    }
  | _ => None
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
  /* A type-parameterized definition: the module is the type function's
     body, read with A in scope as an abstract type, so member types may
     mention it (`type Expansion = A`). One parameter, at the top. */
  | TypFun(tp, body, _) =>
    switch (TPat.tyvar_of_utpat(tp)) {
    | Some(a) =>
      let ctx =
        Ctx.extend_tvar(
          ctx,
          {
            name: a,
            id: TPat.rep_id(tp),
            kind: Abstract,
          },
        );
      switch (detect(~ctx, ~m, body)) {
      | Ok(d) =>
        Ok({
          ...d,
          tparam: Some(a),
        })
      | Error(_) as e => e
      };
    | None => Error(Mark.InvalidLivelitDef(DefNotModule))
    }
  /* A definition taking value parameters (Sec. 2.4.1): the module is the
     function's body. Member types cannot mention a value, so they are
     read as for a plain module; the members' own types come from the
     statics map, where the parameter is bound. One parameter, possibly a
     tuple, inside any type parameter and not around one. */
  | Fun(_, body, _, _) =>
    switch (detect(~ctx, ~m, body)) {
    | Ok({tparam: None, vparam: false, _} as d) =>
      Ok({
        ...d,
        vparam: true,
      })
    | Ok(_) => Error(Mark.InvalidLivelitDef(DefNotModule))
    | Error(_) as e => e
    }
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
        tparam: None,
        vparam: false,
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
  | Ok({model_t, action_t, expansion_t, tparam, vparam, _}) =>
    realized_livelit_sig(
      ~ctx,
      ~types=[
        ("Model", model_t),
        ("Action", action_t),
        ("Expansion", expansion_t),
      ],
    )
    /* fun p -> { ... } is analyzed against ? -> <signature>: the
       parameter's type is whatever its pattern says. */
    |> Option.map(sig_ =>
         vparam
           ? (
               Arrow(IdTagged.FreshGrammar.Typ.unknown(Internal), sig_): Typ.term
             )
             |> Typ.temp
           : sig_
       )
    /* typfun A -> { ... } is analyzed against forall A. <signature>. */
    |> Option.map(sig_ =>
         switch (tparam) {
         | Some(a) =>
           (Poly(IdTagged.FreshGrammar.TPat.var(a), sig_): Typ.term)
           |> Typ.temp
         | None => sig_
         }
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
/* The refs a use's model argument holds.

   new_splice is the only thing that makes a splice (Sec. 3.2.1). What
   it makes is kept in the program text: the commit writes each ref in
   the model as the splice itself, in parens, at the ref's position, so
   the client's code lives in the client's program. This rewrite DECODES
   that, on every pass. Figure 3's model holds a HANDLE (l.3-4), so where
   Model says SpliceRef a parenthesized splice reads as

     SpliceRef(("<id>", <the code>))

   The id is what editor and Html.splice resolve to this projector's own
   splice. The code evaluates in place, in the client's scope, so the ref
   carries the value it had in this run, and that is what eval_splice
   reads (Sec. 3.2.3): the "selected closure" is the run the view sample
   came from.

   Where Model says the stopgap pair (ref=SpliceRef, value=t), the field
   reads as (ref=SpliceRef(...), value=<the code>), for a Functional
   expand, which cannot eval_splice. It goes when Macro can return quoted
   code.

   This is a rewrite of the model ARGUMENT, applied before analysis, not a
   rule about splices. Splice transparency is load-bearing elsewhere (a
   table infers its headers through it) and is left alone. */
let expose_splice_refs =
    (~ctx: Ctx.t, ~model_t: Typ.t, arg: TermBase.Exp.t): TermBase.Exp.t => {
  module F = IdTagged.FreshGrammar;
  /* SpliceRef((id, code)): the code evaluates in place, in the client's
     scope, and its value is what eval_splice reads. */
  let mk_ref = (id: Id.t, code: TermBase.Exp.t): TermBase.Exp.t =>
    F.Exp.ap(
      Forward,
      F.Exp.constructor("SpliceRef", None),
      F.Exp.tuple([F.Exp.string(Id.to_string(id)), code]),
    );
  let is_parens = (e: TermBase.Exp.t) =>
    switch (e.term) {
    | Parens(_) => true
    | _ => false
    };
  /* The splice under any parens the author wrote, with its id. */
  let rec find_splice = (e: TermBase.Exp.t): option(Id.t) =>
    switch (e.term) {
    | Splice(_) => Some(IdTagged.rep_id(e))
    | Parens(inner) => find_splice(inner)
    | _ => None
    };
  /* A value, rewritten for the type its position asks for, looking
     through tuples and lists to every position the Model gives a type.
     A splice where Model says SpliceRef becomes a ref; where it says the
     stopgap pair, the pair, naming the code once through a let so it
     runs once. A splice anywhere else is left alone: a splice is
     transparent, and is then simply the client's code in that place. */
  let rec expose = (ty: Typ.t, v: TermBase.Exp.t): TermBase.Exp.t =>
    switch (find_splice(v)) {
    | Some(id) when is_splice_ref_ty(ty) => mk_ref(id, v)
    | Some(id) when Option.is_some(pair_value_ty(ty)) =>
      let x = "$splice_value";
      F.Exp.let_(
        F.Pat.var(x),
        v,
        F.Exp.tuple([
          F.Exp.tup_label(F.Exp.label("ref"), mk_ref(id, F.Exp.var(x))),
          F.Exp.tup_label(F.Exp.label("value"), F.Exp.var(x)),
        ]),
      );
    | Some(_) => v
    /* Parens with no splice inside, where Model says SpliceRef: the text
       form of a splice. Parens are how a splice is written in program
       text -- a projected use is saved that way, and reloads that way --
       and the editor's Splice piece exists only inside a projector.
       Removing the projector unwraps each Splice back into its parens,
       so without this the unprojected use `^sheet((a = (price), ...))`
       would type its fields as Int against SpliceRef. The parens' own id
       names the splice. */
    | None when is_splice_ref_ty(ty) && is_parens(v) =>
      mk_ref(IdTagged.rep_id(v), v)
    | None =>
      switch (v.term, Typ.term_of(Typ.weak_head_normalize(ctx, ty))) {
      | (Parens(inner), _) => {
          ...v,
          term: (Parens(expose(ty, inner)): TermBase.Exp.term),
        }
      | (Tuple(xs), Prod(tys)) =>
        let labeled =
          List.exists(
            (x: TermBase.Exp.t) =>
              switch (x.term) {
              | TupLabel(_) => true
              | _ => false
              },
            xs,
          );
        let field = (i, x: TermBase.Exp.t) =>
          switch (x.term) {
          | TupLabel({term: Label(name), _} as l, xv) =>
            switch (labeled_ty(tys, name)) {
            | Some(t) => {
                ...x,
                term: (TupLabel(l, expose(t, xv)): TermBase.Exp.term),
              }
            | None => x
            }
          | _ when !labeled && List.length(xs) == List.length(tys) =>
            expose(List.nth(tys, i), x)
          | _ => x
          };
        {
          ...v,
          term: (Tuple(List.mapi(field, xs)): TermBase.Exp.term),
        };
      | (ListLit(xs), List(t)) => {
          ...v,
          term: (ListLit(List.map(expose(t), xs)): TermBase.Exp.term),
        }
      | _ => v
      }
    };
  expose(model_t, arg);
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

/* ==================== Macro expansion (Sec. 3.2.5) ====================
   A Macro livelit's use means its quoted function applied to the code of
   the splices it lists (Fig. 5). Finding that out means RUNNING expand on
   the model -- the paper's premise 3 -- which is done here, while the use
   is checked, from the definition's closed elaboration (the one init runs
   from too), in the builtin environment. */

/* Looking through what an evaluated value may be wrapped in. */
let rec strip_value = (d: TermBase.Exp.t): TermBase.Exp.t =>
  switch (d.term) {
  | Asc(inner, _)
  | Closure(_, inner)
  | Parens(inner) => strip_value(inner)
  | _ => d
  };

/* C(payload) ~> (C, payload) */
let of_ctr = (d: TermBase.Exp.t): option((string, TermBase.Exp.t)) =>
  switch (strip_value(d).term) {
  | Ap(Forward, fn, body) =>
    switch (strip_value(fn).term) {
    | Constructor(name, _) => Some((name, strip_value(body)))
    | _ => None
    }
  | _ => None
  };

/* The id a SpliceRef term or value names: SpliceRef(("<id>", _)). */
let ref_id = (d: TermBase.Exp.t): option(string) =>
  switch (of_ctr(d)) {
  | Some(("SpliceRef", body)) =>
    switch (strip_value(body).term) {
    | Tuple([id, _]) =>
      switch (strip_value(id).term) {
      | Atom(String(s)) => Some(s)
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

/* The model with every ref's code replaced by a hole. expand must treat
   splices parametrically -- it gets their identities, not their code --
   and the code is the client's, open in the client's scope, so it could
   not be evaluated here anyway. */
let rec blank_refs = (e: TermBase.Exp.t): TermBase.Exp.t =>
  switch (ref_id(e)) {
  | Some(id) =>
    IdTagged.FreshGrammar.(
      Exp.ap(
        Forward,
        Exp.constructor("SpliceRef", None),
        Exp.tuple([Exp.string(id), Exp.empty_hole()]),
      )
    )
  | None =>
    Exp.map_term(
      ~f_exp=
        (continue, e) =>
          switch (ref_id(e)) {
          | Some(_) => blank_refs(e)
          | None => continue(e)
          },
      e,
    )
  };

/* The code at the position of the ref naming [id] in a model: the second
   component of SpliceRef(("<id>", code)), as expose_splice_refs left it. */
let splice_code = (model: TermBase.Exp.t, id: string): option(TermBase.Exp.t) => {
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) =>
          switch (of_ctr(e), found^) {
          | (Some(("SpliceRef", body)), None) when ref_id(e) == Some(id) =>
            switch (body.term) {
            | Tuple([_, code]) =>
              found := Some(code);
              e;
            | _ => continue(e)
            }
          | _ => continue(e)
          },
      model,
    );
  found^;
};

/* What a Macro livelit's expand answers for this model: the body of its
   quotation, and the ids of the refs it lists, in order. None when the
   livelit is not a Macro one, or its expand does not answer
   (quote body end, [refs]) -- the use then keeps the ordinary path. */
let run_macro_expand =
    (~def_elab: TermBase.Exp.t, ~model: TermBase.Exp.t)
    : option((TermBase.Exp.t, list(string))) => {
  let eval = d =>
    switch (Evaluator.evaluate(~env=Builtins.env_init, d)) {
    | (v, _) => Some(v)
    | exception _ => None
    };
  let field = (record: TermBase.Exp.t, label: string) =>
    switch (strip_value(record).term) {
    | Module(items) =>
      List.fold_left(
        (acc, item: TermBase.Mod.t) =>
          switch (item.term) {
          | ModVal(x, v) when x == label => Some(v)
          | _ => acc
          },
        None,
        items,
      )
    | _ => None
    };
  open Util.OptUtil.Syntax;
  let* def = eval(def_elab);
  let* expand = field(def, "expand");
  let* g =
    switch (of_ctr(expand)) {
    | Some(("Macro", g)) => Some(g)
    | _ => None
    };
  let* answer =
    eval(IdTagged.FreshGrammar.Exp.ap(Forward, g, blank_refs(model)));
  switch (strip_value(answer).term) {
  | Tuple([code, refs]) =>
    /* A quotation (its antiquotes already filled when it was evaluated),
       or code spelled with Exp's constructors: Lambda, Ident, IntLit. */
    /* An Abs's function is run here, on a fresh variable, in the builtin
       environment: the decoding is what names its binder. */
    let apply = (f, x) => eval(IdTagged.FreshGrammar.Exp.ap(Forward, f, x));
    let* body = BuiltinsADT.code_of_exp_value(~apply, code);
    let* refs =
      switch (strip_value(refs).term) {
      | ListLit(items) => Util.OptUtil.sequence(List.map(ref_id, items))
      | _ => None
      };
    Some((body, refs));
  | _ => None
  };
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
      | "init" => BuiltinsADT.update_cmd(model_t)
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
   expansion, so the model's code, the splices it holds included, runs —
   and its probes fire — exactly once. The model keeps its surface ids as the
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
  | Ok({members, model_t, action_t, expansion_t, tparam, vparam}) => (
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
        tparam,
        vparam,
      }),
      /* A member whose type is wrong is reported by the second analytic
         pass, as an ordinary inconsistency where it is written, and does
         NOT stop the livelit being bound: its uses keep resolving and keep
         being checked themselves. */
      [],
    )
  };

/* An abbreviation, `let ^b = ^a@<T> in`, of a type-parameterized livelit
   ^a: the same livelit with T in place of its parameter, in each member
   type, under the new name. Its definition is ^a's applied to T, which
   stays closed, so the projector can still run it at event time. */
let instantiate =
    (
      ~name: string,
      ~id: Id.t,
      ~ty: TermBase.Typ.t,
      ll: LivelitCtx.raw_livelit,
    )
    : option(LivelitCtx.raw_livelit) =>
  switch (ll.tparam, ll.user_def) {
  | (Some(a), Some(def)) =>
    let sub = t => Typ.subst(ty, IdTagged.FreshGrammar.TPat.var(a), t);
    let expansion_t = sub(ll.expansion_t);
    Some({
      ...ll,
      name,
      id,
      model_t: sub(ll.model_t),
      action_t: sub(ll.action_t),
      expansion_t,
      expand: mk_expand_dot(~name, ~expansion_t),
      user_def: Some((TypAp(def, ty): TermBase.Exp.term) |> Exp.fresh),
      tparam: None,
    });
  | _ => None
  };

/* An abbreviation, `let ^b = ^a(args) in`, of a livelit ^a that takes
   value parameters: the same livelit under the new name, whose definition
   is ^a's applied to the arguments. The arguments were analyzed in the
   builtin context, so the application stays closed and the projector can
   still run it at event time. Member types cannot mention a value, so
   they carry over unchanged. */
let apply_args =
    (
      ~name: string,
      ~id: Id.t,
      ~args: TermBase.Exp.t,
      ll: LivelitCtx.raw_livelit,
    )
    : option(LivelitCtx.raw_livelit) =>
  switch (ll.vparam, ll.user_def) {
  | (true, Some(def)) =>
    Some({
      ...ll,
      name,
      id,
      expand: mk_expand_dot(~name, ~expansion_t=ll.expansion_t),
      user_def:
        Some((Ap(Forward, def, args): TermBase.Exp.term) |> Exp.fresh),
      vparam: false,
    })
  | _ => None
  };

/* A Macro use's expansion obligation (Fig. 5 premise 5), said in terms of
   Expansion. The code expand returned, of type `code`, must take each
   listed splice, at the type its code has (`splices`, in order), to the
   declared `expansion`. Rather than one consistency check against the
   arrow -- whose failure would name the arrow as if it were Expansion --
   walk the code's parameters against the splices and report the first
   thing that fails: too few parameters, a parameter that cannot take its
   splice, or a result that is not Expansion. When all of that fits but the
   code still has an error of its own (`code_has_error`), say so. An
   Unknown along the way stays gradual, as everywhere else. */
let macro_expansion_mark =
    (
      ctx: Ctx.t,
      ~expansion: TermBase.Typ.t,
      ~splices: list(TermBase.Typ.t),
      ~code: TermBase.Typ.t,
      ~code_has_error: bool,
    )
    : list(Mark.t) => {
  let rec walk = (i, splices, code): option(Mark.macro_expansion_problem) =>
    switch (splices) {
    | [] =>
      if (!Typ.is_consistent(ctx, code, expansion)) {
        Some(Result(code));
      } else if (code_has_error) {
        Some(ErrorInCode);
      } else {
        None;
      }
    | [splice, ...rest] =>
      switch (Typ.term_of(Typ.weak_head_normalize(ctx, code))) {
      | Arrow(param, result) =>
        Typ.is_consistent(ctx, param, splice)
          ? walk(i + 1, rest, result)
          : Some(
              SpliceParameter({
                index: i,
                param,
              }),
            )
      | Unknown(_) => code_has_error ? Some(ErrorInCode) : None
      | _ => Some(TooFewParameters)
      }
    };
  switch (walk(0, splices, code)) {
  | None => []
  | Some(problem) => [
      Mark.BadMacroExpansion({
        expansion,
        splices,
        code,
        problem,
      }),
    ]
  };
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
