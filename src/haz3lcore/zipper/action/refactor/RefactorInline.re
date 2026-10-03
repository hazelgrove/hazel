/* Inline/feed/extract refactorings and alias variants,
 * rename machinery, explode/implode. */
open Language;
open RefactorBase;
open RefactorParens;

let inline_matches = (p: Pat.t, def: Exp.t, body: Exp.t): bool => {
  let moved: Exp.t = fresh(Let(p, def, fresh(EmptyHole)));
  let ok = f =>
    !free_in(f, def)
    && occurrences_of(f, body)
    |> List.for_all(o => !typ_captured_by_use_at(Exp.rep_id(o), moved, body));
  (
    switch (let_head_name(p)) {
    | Some(f) => ok(f)
    | None => false
    }
  )
  || (
    switch (sugar_fn_name(p)) {
    | Some(f) => ok(f)
    | None => false
    }
  );
};

let inline_let_impl: impl = {
  label: "Inline",
  tooltip: "Replace this let by substituting its definition",
  /* also offered at occurrences of the bound var */
  prepare: (~info_map, ~target, program) => {
    let attempt = target =>
      rewrite_let(
        ~target,
        /* self-recursive defs are gated on BOTH paths: consuming
           the binding would orphan the copied body's self-
           references (feed still unfolds them one use at a time,
           where the binding survives) */
        ~matches=inline_matches,
        ~rewrite=
          (p, def, body) => {
            /* An annotated head's type is analysis context, not
               decoration (a bare lambda re-synthesizes ? -> ?), so
               substitution keeps it as an ascription — unless the
               def synthesizes the same hole-free type on its own. */
            let asc_def = (ann: Typ.t, def: Exp.t): Exp.t => {
              let base = strip_boundaries(def);
              let base = needs_parens(base) ? fresh(Parens(base)) : base;
              fresh(
                Asc(
                  with_secondary(([], space()), base),
                  with_secondary_typ(
                    (space(), []),
                    strip_typ_boundaries(refresh_typ_ids(ann)),
                  ),
                ),
              );
            };
            let redundant = (ann: Typ.t, def: Exp.t): bool => {
              let st =
                CachedStatics.init_from_term(
                  ~settings=CoreSettings.on,
                  ~is_dynamic_term=false,
                  def,
                );
              switch (Id.Map.find_opt(Exp.rep_id(def), st.info_map)) {
              | Some(InfoExp({ty, _})) =>
                typ_known(ty)
                && (
                  switch (Id.Map.find_opt(Exp.rep_id(def), info_map)) {
                  | Some(InfoExp({ctx, _})) =>
                    Typ.equal(
                      Typ.normalize(ctx, ann),
                      Typ.normalize(ctx, ty),
                    )
                  | _ => false
                  }
                )
              | _ => false
              };
            };
            /* f(x)-sugar inlines as a lambda (gated non-recursive) */
            let (x, def) =
              switch (let_head_name(p)) {
              | Some(x) =>
                switch (IdTagged.term_of(p)) {
                | Asc(_, ann) when !redundant(ann, def) => (
                    x,
                    asc_def(ann, def),
                  )
                | _ => (x, def)
                }
              | None =>
                let f = Option.get(sugar_fn_name(p));
                let rec arg_of = (p: Pat.t) =>
                  switch (IdTagged.term_of(p)) {
                  | Ap(_, argp) => Some(argp)
                  | Asc(inner, _) => arg_of(inner)
                  | _ => None
                  };
                let argp = Option.get(arg_of(p));
                let param: Pat.t =
                  switch (IdTagged.term_of(argp)) {
                  | Tuple(_) => pad(fresh_pat(Parens(argp)))
                  | _ => pad(argp)
                  };
                let lam = fresh(Fun(param, def, None, None));
                switch (IdTagged.term_of(p)) {
                | Asc(_, ret) =>
                  let arrow =
                    fresh_typ(
                      Arrow(
                        with_secondary_typ(
                          ([], space()),
                          fresh_typ(Unknown(Hole(EmptyHole))),
                        ),
                        with_secondary_typ(
                          (space(), []),
                          strip_typ_boundaries(refresh_typ_ids(ret)),
                        ),
                      ),
                    );
                  (f, asc_def(arrow, lam));
                | _ => (f, lam)
                };
              };
            let avoid = vars_of(def) |> List.filter(v => free_in(v, def));
            let used = ref(used_names(program));
            /* Per-occurrence parens: static precedence check at each
               splice point (splice_parens_needed) */
            let bare_ids =
              needs_parens(def)
                ? occurrences_of(x, body)
                  |> List.filter_map(occ =>
                       splice_parens_needed(
                         ~program,
                         ~at=Exp.rep_id(occ),
                         def,
                       )
                         ? None : Some(Exp.rep_id(occ))
                     )
                : occurrences_of(x, body) |> List.map(Exp.rep_id);
            let parens_for = occ =>
              needs_parens(def) && !List.mem(Exp.rep_id(occ), bare_ids);
            /* a crossed same-name alias is freshened rather than
               refused (see freshen_crossed_aliases) */
            let body = freshen_crossed_aliases(~x, ~moved=def, body);
            let body' =
              subst(
                ~parens_for,
                ~avoid,
                ~used,
                ~first=ref(true),
                x,
                def,
                body,
              );
            /* the def's BOUNDARY comments stay at the vacated line
               (andrew: comments live where they live) — the copies
               are stripped of them (never duplicate prose), so
               re-home them above the surviving body */
            let body' =
              switch (boundary_comments(def)) {
              | [] => body'
              | comments =>
                let (b, a) = body'.annotation.secondary;
                {
                  ...body',
                  annotation: {
                    ...body'.annotation,
                    secondary: (comments @ newline() @ b, a),
                  },
                };
              };
            /* caret follows the moved content (P2/P7): the def's
               ids travel into the first copy; later copies are fully
               fresh (they FLY as fan-out clones), so no occurrence
               id survives to focus on. */
            let focus =
              occurrences_of(x, body) == []
                ? Exp.rep_id(body') : Exp.rep_id(def);
            (body', focus);
          },
        program,
      );
    switch (attempt(target)) {
    | Some(r) => Some(r)
    | None =>
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) => attempt(binder)
      | None => None
      }
    };
  },
};

/* === Feed (per-occurrence inline) ===
 * Down's value-flow move (D2 locality): substitute the definition
 * into ONE use — the nearest below the def, or the invoked
 * occurrence — keeping the binding while other uses remain; the last
 * feed consumes it (delegates to full inline). The binder never
 * moves, so no scope gate is needed; capture of the def's free vars
 * by binders over the target occurrence is GATED (dead press) rather
 * than repaired — renames would be a distant surprise. */

/* the textually-first unshadowed occurrence (pre-order; children_of
 * yields children left-to-right) */
let first_occurrence = (x: string, body: Exp.t): option(Exp.t) => {
  let unshadowed = occurrences_of(x, body) |> List.map(Exp.rep_id);
  let rec go = (e: Exp.t): option(Exp.t) =>
    List.mem(Exp.rep_id(e), unshadowed)
      ? Some(e) : children_of(e) |> List.find_map(go);
  go(body);
};

/* names bound between the body root and the occurrence (conservative:
 * every binder on the path counts) */
let binders_over = (occ_id: Id.t, body: Exp.t): list(string) =>
  switch (find_path(~hit=e => Exp.rep_id(e) == occ_id, body)) {
  | None => []
  | Some(path) =>
    path
    |> List.concat_map((e: Exp.t) =>
         switch (IdTagged.term_of(e)) {
         | Let(p, _, _)
         | Fun(p, _, _, _)
         | FixF(p, _, _) => pat_var_names(p)
         | Match(_, rules) =>
           rules |> List.concat_map(((rp, _)) => pat_var_names(rp))
         | _ => []
         }
       )
  };

/* resolve a feed: the let, its bound name, and the occurrence to
 * feed (None = nearest) */
let feed_site =
    (
      ~prefer_def_host: bool=false,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      program: Exp.t,
    )
    : option((Exp.t, string, option(Id.t))) => {
  let of_let = (l: Exp.t, occ: option(Id.t)) =>
    switch (IdTagged.term_of(l)) {
    | Let(p, _, _) => let_head_name(p) |> Option.map(x => (l, x, occ))
    | _ => None
    };
  /* the innermost let whose DEF subtree holds the target: grabbing
     the value itself feeds it (the def is the persistent element —
     the D2-conformant handle); resolution order keeps occurrence
     targeting first so feed-at-use semantics are unchanged */
  let def_host = (): option(Exp.t) =>
    switch (find_path(~hit=hit_node(target), program)) {
    | None => None
    | Some(path) =>
      let rec scan = (best, path: list(Exp.t)) =>
        switch (path) {
        | [parent, child, ...rest] =>
          let best =
            switch (IdTagged.term_of(parent)) {
            | Let(_, def, _) when same_node(def, child) => Some(parent)
            | _ => best
            };
          scan(best, [child, ...rest]);
        | _ => best
        };
      scan(None, path);
    };
  /* the def ROOT's own delimiters count as "on the def" (andrew:
     the root term between = and in; its interior does NOT — a
     random subterm must not feed the enclosing let) */
  let hit_def_root = (e: Exp.t): bool =>
    switch (IdTagged.term_of(e)) {
    | Let(_, def, _) => List.mem(target, IdTagged.ids(def))
    | _ => false
    };
  switch (find_hit(~hit=hit_let(target), program)) {
  | Some(l) => of_let(l, None)
  | None =>
    /* a token can be BOTH inside a def and an occurrence of an
       outer binder. Keyboard keeps occurrence-first (feed-at-use);
       the drag prefers the def-host reading when the at-use track
       degenerates (grabbing the value should move the value). */
    let by_occurrence = () =>
      switch (binder_of_occurrence(~info_map, ~target, program)) {
      | Some(binder) =>
        switch (find_hit(~hit=hit_let(binder), program)) {
        | Some(l) => of_let(l, Some(target))
        | None => None
        }
      | None => None
      };
    let by_def_host = () =>
      switch (def_host()) {
      | Some(l) => of_let(l, None)
      | None => None
      };
    let by_def_root = () =>
      switch (find_hit(~hit=hit_def_root, program)) {
      | Some(l) => of_let(l, None)
      | None => None
      };
    /* drag grabs the def's interior (the value is the handle);
       the keyboard only honors the ROOT delimiters (a random
       subterm must not feed the enclosing let — andrew) */
    if (prefer_def_host) {
      switch (by_def_host()) {
      | Some(r) => Some(r)
      | None => by_occurrence()
      };
    } else {
      switch (by_occurrence()) {
      | Some(r) => Some(r)
      | None => by_def_root()
      };
    };
  };
};

/* the plan (print-free): which occurrence a feed would hit, or that
 * the sole remaining use consumes the binding */
type feed_plan =
  | Consume(Exp.t) /* last use: full inline (of this let) */
  | Feed(Exp.t, Exp.t, Exp.t) /* let, def, occurrence */;

let feed_plan =
    (
      ~prefer_def_host: bool=false,
      ~info_map: Statics.Map.t,
      ~target: Id.t,
      program: Exp.t,
    )
    : option(feed_plan) =>
  switch (feed_site(~prefer_def_host, ~info_map, ~target, program)) {
  | None => None
  | Some((l, x, occ_pref)) =>
    switch (IdTagged.term_of(l)) {
    | Let(_, def, body) =>
      let occs = occurrences_of(x, body);
      switch (occs) {
      | [] => None
      /* the last feed CONSUMES the binding — except for a
         self-recursive def, whose copied body's self-references
         would be orphaned: those feed WITHOUT consuming (the
         binding survives; unfolding never eliminates a recursive
         definition). `let f = ... in f(x)` steps this way. */
      | [_] when !free_in(x, def) => Some(Consume(l))
      | _ =>
        let occ =
          switch (occ_pref) {
          | Some(oid) =>
            occs
            |> List.find_opt((o: Exp.t) => List.mem(oid, IdTagged.ids(o)))
          | None => first_occurrence(x, body)
          };
        switch (occ) {
        | None => None
        | Some(occ) =>
          let free = vars_of(def) |> List.filter(v => free_in(v, def));
          let captured =
            binders_over(Exp.rep_id(occ), body)
            |> List.exists(b => List.mem(b, free))
            || typ_captured_by_use_at(Exp.rep_id(occ), def, body);
          captured ? None : Some(Feed(l, def, occ));
        };
      };
    | _ => None
    }
  };

let feed_prepare = (~prefer_def_host=false, ~info_map, ~target, program) =>
  switch (feed_plan(~prefer_def_host, ~info_map, ~target, program)) {
  | None => None
  | Some(Consume(l)) =>
    /* delegate at the LET, not the raw target — a def-interior
       grab resolves here but not in inline's own targeting */
    inline_let_impl.prepare(~info_map, ~target=Exp.rep_id(l), program)
  | Some(Feed(l, def, occ)) =>
    /* STATIC parens: precedence check at the splice point */
    let parens =
      needs_parens(def)
      && splice_parens_needed(~program, ~at=Exp.rep_id(occ), def);
    /* the copy is a SPAWNED CLONE (dragology's fruit-bowl: the def
       survives, so the copy is wholly new — fresh ids throughout,
       keep_ids so the root doesn't adopt the occurrence's). The
       occurrence properly EXITS; the clone's own identity is what
       emerge animation correlates on. */
    /* the def survives, so the copy sheds prose too */
    let copy =
      inserted(
        ~parens,
        ~keep_ids=true,
        strip_comments(refresh_ids(def)),
        occ,
      );
    let at_use = occ |> IdTagged.ids |> List.mem(target);
    let focus =
      at_use
        /* invoked at the use: caret follows the clone (the
           occurrence's ids are gone) */
        ? Exp.rep_id(copy)
        /* invoked at the let: caret stays for the next feed */
        : Exp.rep_id(l);
    rewrite_node(
      ~hit=same_node(l),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Let(p', def', body') =>
            /* a crossed same-name alias is freshened rather than
               refused (base parity) */
            let body' =
              switch (let_head_name(p')) {
              | Some(x) => freshen_crossed_aliases(~x, ~moved=def', body')
              | None => body'
              };
            let body'' =
              replace_node(~at=Exp.rep_id(occ), ~with_=copy, body');
            Some((
              {
                ...e,
                term: Let(p', def', body''),
              },
              focus,
            ));
          | _ => None
          },
      program,
    );
  };

/* === Inline type alias === */

let typ_mentions = (names: list(string), t: Typ.t): bool => {
  let found = ref(false);
  let _ =
    Typ.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          switch (IdTagged.term_of(t)) {
          | Var(y) when List.mem(y, names) => found := true
          | _ => ()
          };
          cont(t);
        },
      t,
    );
  found^;
};

let contains_use = (e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) => {
          switch (IdTagged.term_of(e)) {
          | Use(_) => found := true
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  found^;
};

let strip_typ_comments = (t: Typ.t): Typ.t => {
  let keep = (ws: list(Secondary.t)) =>
    ws |> List.filter(w => !is_comment_piece(w));
  Typ.map_term(
    ~f_typ=
      (cont, t: Typ.t) => {
        let (b, a) = t.annotation.secondary;
        cont({
          ...t,
          annotation: {
            ...t.annotation,
            secondary: (keep(b), keep(a)),
          },
        });
      },
    t,
  );
};

/* self-delimiting types substitute bare; everything else wraps */
let typ_atomic = (t: Typ.t): bool =>
  switch (IdTagged.term_of(t)) {
  | Var(_)
  | Atom(_)
  | Unknown(_)
  | List(_)
  | Parens(_)
  | Sig(_) => true
  | _ => false
  };

/* substitute the alias def for Var(t) at every unshadowed typ use.
   Mirrors exp inline: the copy's root adopts the use's ids (the
   occurrence id stays valid); the FIRST copy's interior travels
   (keeps the def's ids — P7), later copies are fresh and shed
   prose. */
let subst_alias = (~first: ref(bool), t: string, def: Typ.t, e: Exp.t): Exp.t => {
  let bare = strip_typ_boundaries(def);
  let at_use = (u: Typ.t): Typ.t => {
    let is_first = first^;
    first := false;
    let d = is_first ? bare : strip_typ_comments(refresh_typ_ids(bare));
    if (typ_atomic(d)) {
      let (before, after) = u.annotation.secondary;
      let (db, da) = d.annotation.secondary;
      {
        ...d,
        annotation: {
          ...d.annotation,
          ids: u.annotation.ids,
          secondary: (db @ before, da @ after),
        },
      };
    } else {
      {
        annotation: {
          ...IdTagged.IdTag.mk_internal(u.annotation.ids),
          secondary: u.annotation.secondary,
        },
        term: Parens(d),
      };
    };
  };
  let in_typ = (ty: Typ.t): Typ.t =>
    Typ.map_term(
      ~f_typ=
        (cont, ty: Typ.t) =>
          switch (IdTagged.term_of(ty)) {
          | Var(z) when z == t => at_use(ty)
          /* rebound below: uses under these binders are the inner
             alias's, not ours */
          | Rec(tp, _)
          | Poly(tp, _) when List.mem(t, tpat_names(tp)) => ty
          | _ => cont(ty)
          },
      ty,
    );
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        /* a rebinding tyalias shadows its body, but its own def
           still sees the outer name */
        | TyAlias(tp, d, b) when List.mem(t, tpat_names(tp)) => {
            ...e,
            term: TyAlias(tp, in_typ(d), b),
          }
        | TypFun(tp, _, _) when List.mem(t, tpat_names(tp)) => e
        | _ => cont(e)
        },
    ~f_typ=(_cont, ty: Typ.t) => in_typ(ty),
    e,
  );
};

let inline_alias_impl: impl = {
  label: "Inline",
  tooltip: "Replace this type alias by substituting its definition into every use",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_tyalias(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, ty, body) =>
            switch (tpat_names(tp)) {
            | [t] when !typ_mentions([t], ty) && !contains_use(body) =>
              let first = ref(true);
              let body' = subst_alias(~first, t, ty, body);
              /* the type line dissolves: its break dies with it (the
                 body sheds its own lead; comment leads stay) */
              let body' = strip_leading_ws(body');
              Some((body', Exp.rep_id(body')));
            | _ => None
            }
          | _ => None
          },
      program,
    ),
};

/* unshadowed typ uses of alias t in body (count + one-shot subst
   share the walk shape with subst_alias) */
let count_typ_uses = (t: string, body: Exp.t): int => {
  let n = ref(0);
  let in_typ = (ty: Typ.t): Typ.t =>
    Typ.map_term(
      ~f_typ=
        (cont, u: Typ.t) =>
          switch (IdTagged.term_of(u)) {
          | Var(z) when z == t =>
            n := n^ + 1;
            u;
          | Rec(tp, _)
          | Poly(tp, _) when List.mem(t, tpat_names(tp)) => u
          | _ => cont(u)
          },
      ty,
    );
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e: Exp.t) =>
          switch (IdTagged.term_of(e)) {
          | TyAlias(tp, d, b) when List.mem(t, tpat_names(tp)) =>
            let _ = in_typ(d);
            {
              ...e,
              term: TyAlias(tp, d, b),
            };
          | TypFun(tp, _, _) when List.mem(t, tpat_names(tp)) => e
          | _ => cont(e)
          },
      ~f_typ=(_cont, ty: Typ.t) => in_typ(ty),
      body,
    );
  n^;
};

/* substitute ONE use of the alias — the TEXTUALLY FIRST (map_term
   visits right-to-left, so that's the ~visit_target-th visit);
   the binding survives, so the copy is fully fresh and sheds prose */
let subst_alias_one =
    (~visit_target: int, t: string, def: Typ.t, body: Exp.t): Exp.t => {
  let bare = strip_typ_boundaries(def);
  let seen = ref(0);
  let at_use = (u: Typ.t): Typ.t => {
    let d = strip_typ_comments(refresh_typ_ids(bare));
    if (typ_atomic(d)) {
      let (before, after) = u.annotation.secondary;
      let (db, da) = d.annotation.secondary;
      {
        ...d,
        annotation: {
          ...d.annotation,
          ids: u.annotation.ids,
          secondary: (db @ before, da @ after),
        },
      };
    } else {
      {
        annotation: {
          ...IdTagged.IdTag.mk_internal(u.annotation.ids),
          secondary: u.annotation.secondary,
        },
        term: Parens(d),
      };
    };
  };
  let in_typ = (ty: Typ.t): Typ.t =>
    Typ.map_term(
      ~f_typ=
        (cont, u: Typ.t) =>
          switch (IdTagged.term_of(u)) {
          | Var(z) when z == t =>
            seen := seen^ + 1;
            seen^ == visit_target ? at_use(u) : u;
          | Rec(tp, _)
          | Poly(tp, _) when List.mem(t, tpat_names(tp)) => u
          | _ => cont(u)
          },
      ty,
    );
  Exp.map_term(
    ~f_exp=
      (cont, e: Exp.t) =>
        switch (IdTagged.term_of(e)) {
        | TyAlias(tp, d, b) when List.mem(t, tpat_names(tp)) => {
            ...e,
            term: TyAlias(tp, in_typ(d), b),
          }
        | TypFun(tp, _, _) when List.mem(t, tpat_names(tp)) => e
        | _ => cont(e)
        },
    ~f_typ=(_cont, ty: Typ.t) => in_typ(ty),
    body,
  );
};

/* feed for a type line: substitute one use per press; the last use
   consumes the alias (inline_alias). Mirrors let-feed semantics. */
let feed_alias_route =
    (~info_map: Statics.Map.t, ~target: Id.t, program: Exp.t)
    : option((Exp.t, Id.t)) =>
  switch (find_hit(~hit=hit_tyalias(target), program)) {
  | Some(e) =>
    switch (IdTagged.term_of(e)) {
    | TyAlias(tp, ty, body) =>
      switch (tpat_names(tp)) {
      | [t] when !typ_mentions([t], ty) && !contains_use(body) =>
        switch (count_typ_uses(t, body)) {
        | 0 => None
        | 1 => inline_alias_impl.prepare(~info_map, ~target, program)
        | _ =>
          rewrite_node(
            ~hit=same_node(e),
            ~rewrite=
              e' =>
                switch (IdTagged.term_of(e')) {
                | TyAlias(tp', ty', body') =>
                  let n = count_typ_uses(t, body');
                  Some((
                    {
                      ...e',
                      term:
                        TyAlias(
                          tp',
                          ty',
                          subst_alias_one(~visit_target=n, t, ty', body'),
                        ),
                    },
                    Exp.rep_id(e'),
                  ));
                | _ => None
                },
            program,
          )
        }
      | _ => None
      }
    | _ => None
    }
  | None => None
  };

let feed_let_impl: impl = {
  label: "Inline next use",
  tooltip: "Substitute the definition into its nearest use; the last use consumes the binding",
  prepare: (~info_map, ~target, program) =>
    switch (feed_prepare(~info_map, ~target, program)) {
    | Some(_) as r => r
    | None => feed_alias_route(~info_map, ~target, program)
    },
};

/* Statics-gated: the binding's pattern var carries an UnusedVar
 * warning (co-ctx says the body never uses it) */
let pat_unused = (~info_map: Statics.Map.t, p: Pat.t): bool =>
  switch (Id.Map.find_opt(Pat.rep_id(p), info_map)) {
  | Some(InfoPat({warnings, _})) =>
    warnings
    |> List.exists((w: Warning.list_item) =>
         switch (w) {
         | Pat(UnusedVar(_)) => true
         }
       )
  | _ => false
  };

let remove_unused_let_impl: impl = {
  label: "Remove unused",
  tooltip: "Delete this binding: its variable is never used",
  prepare: (~info_map, ~target, program) => {
    let as_let =
      rewrite_let(
        ~target,
        ~matches=
          (p, _, _) =>
            switch (head_var_pat(p)) {
            | Some(hv) => pat_unused(~info_map, hv)
            | None => false
            },
        ~rewrite=(_, _, body) => (body, Exp.rep_id(body)),
        program,
      );
    switch (as_let) {
    | Some(_) => as_let
    | None =>
      /* an alias with no mention anywhere below is dead
         (conservative: ANY mention keeps it) */
      rewrite_node(
        ~hit=hit_tyalias(target),
        ~rewrite=
          e =>
            switch (IdTagged.term_of(e)) {
            | TyAlias(tp, _, body)
                when
                  tpat_names(tp) != []
                  && !mentions_typ_names(tpat_names(tp), body) =>
              let body = strip_leading_ws(body);
              Some((body, Exp.rep_id(body)));
            | _ => None
            },
        program,
      )
    };
  },
};

let exp_ty = (~info_map: Statics.Map.t, e: Exp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let add_annotation_impl: impl = {
  label: "Annotate",
  tooltip: "Annotate this binding with its inferred type",
  prepare: (~info_map, ~target, program) => {
    /* a bare spliced type can change the reparse (a Prod's comma
       breaks the let: `let p : Int,Bool = ...`) — oracle-gated parens
       like inline/extract */
    let attempt = (~parens: bool) =>
      rewrite_node(
        ~hit=hit_let(target),
        ~rewrite=
          e =>
            switch (IdTagged.term_of(e)) {
            | Let(p, def, body) when var_pat_name(p) != None =>
              switch (exp_ty(~info_map, def)) {
              | Some(ty) when typ_known(ty) =>
                let bare = refresh_typ_ids(ty);
                let ty =
                  pad(
                    parens
                      ? fresh_typ(
                          Parens(with_secondary_typ(([], []), bare)),
                        )
                      : bare,
                  );
                /* p keeps its runs: its old pre-`=` space now sits
                   before the `:` */
                let p' = fresh_pat(Asc(p, ty));
                Some((
                  {
                    ...e,
                    term: Let(p', def, body),
                  },
                  Typ.rep_id(ty),
                ));
              | _ => None
              }
            | _ => None
            },
        program,
      );
    /* static: single-token types go bare; anything wider (Prod's
       comma especially breaks the let) takes parens */
    let simple =
      switch (find_hit(~hit=hit_let(target), program)) {
      | Some(e) =>
        switch (IdTagged.term_of(e)) {
        | Let(_, def, _) =>
          switch (exp_ty(~info_map, def)) {
          | Some(ty) =>
            switch (IdTagged.term_of(ty)) {
            | Unknown(_)
            | Atom(_)
            | Var(_) => true
            | _ => false
            }
          | None => false
          }
        | _ => false
        }
      | None => false
      };
    attempt(~parens=!simple);
  },
};

let extractable = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  /* bare references extract to a pointless alias; constructors are
     morally variables here. (Literals stay extractable: naming a
     magic number is a real refactoring.) */
  | Var(_)
  | Constructor(_)
  | EmptyHole
  | Let(_)
  /* a definition line is not an expression to name: extracting a
     TyAlias node would bind the whole rest of the program */
  | TyAlias(_)
  | Seq(_)
  | Filter(_) => false
  /* label syntax, not an expression to name: a use-var in a label slot
     reparses as the label itself (`baz=B`), and lifting a whole `l=e`
     entry out of its tuple both loses the label at the use site and
     leaves a singleton Tuple that prints as the different term `_=baz` */
  | Label(_)
  | TupLabel(_)
  /* likewise the `_`s: a deferral only means anything in the argument
     position it marks, and an explicit nonlabel is the label slot */
  | Deferral(_)
  | ExplicitNonlabel => false
  /* extracting a parens node is redundant with extracting its child,
     and the bare use-var can be load-bearing where the parens were
     (e.g. a Dot's lhs) */
  | Parens(_) => false
  | _ => true
  };

/* Resolve an extraction target. A bare var/ctor HEAD of an
 * application retargets to the whole application: the head is how
 * people refer to the call, the caret lands there naturally
 * (`Â¦f(x)` indicates f), and a bare head is otherwise a dead
 * press. Also home to the shared gates: extractable + no Dot/
 * MultiHole ancestors (a bare use-var as a Dot-rhs reparses as a
 * projection label). Returns the effective path. */
let extract_path = (~target: Id.t, program: Exp.t): option(list(Exp.t)) =>
  switch (find_path(~hit=hit_node(target), program)) {
  | None => None
  | Some(path) =>
    let n = List.length(path);
    let t = List.nth(path, n - 1);
    let head_of_ap =
      n >= 2
      && (
        switch (IdTagged.term_of(t)) {
        | Var(_)
        | Constructor(_) =>
          switch (IdTagged.term_of(List.nth(path, n - 2))) {
          | Ap(Forward, f, _) => same_node(f, t)
          | _ => false
          }
        | _ => false
        }
      );
    let path = head_of_ap ? List.filteri((i, _) => i < n - 1, path) : path;
    let t = List.nth(path, List.length(path) - 1);
    /* extracting a let's ENTIRE def just manufactures an alias pair
       (`let x = def in let orig = x`) — the whole-reference rule
       again. Parens wrappers are transparent for this check. */
    let whole_def = {
      let rec check = (k: int, child: Exp.t) =>
        k >= 0
        && (
          switch (IdTagged.term_of(List.nth(path, k))) {
          | Let(_, def, _) => same_node(def, child)
          | Parens(_) => check(k - 1, List.nth(path, k))
          | _ => false
          }
        );
      check(List.length(path) - 2, t);
    };
    let blocked_ancestor =
      path
      |> List.filteri((i, _) => i < List.length(path) - 1)
      |> List.exists((a: Exp.t) =>
           switch (IdTagged.term_of(a)) {
           | Dot(_)
           | MultiHole(_) => true
           | _ => false
           }
         );
    extractable(t) && !blocked_ancestor && !whole_def ? Some(path) : None;
  };

let extract_let_impl: impl = {
  label: "Extract",
  tooltip: "Bind this expression to a fresh variable at the enclosing line",
  prepare: (~info_map as _, ~target, program) => {
    let x = fresh_name(program);
    /* STATIC parens policy (the whole-program reparse oracle cost
       ~0.5s/invocation; reparse-safety is covered by tests instead):
       to_block always goes bare — lowest_line only yields line slots
       (chain slots, fun/arm/branch bodies, root), where a bare
       `let..in`'s rightward extent is exactly the intended body and
       arm/branch slots are delimiter-bounded. in_place goes bare only
       when extracting a whole line; the rec-blocked fallback sits at
       an arbitrary (possibly comma/operand) position, so it takes
       parens. */
    /* fallback, and the degenerate case of extracting a whole line */
    /* t is the extract_path-resolved node (which may be the whole
       application when invoked from its head) */
    let in_place = (~parens: bool, t: Exp.t) =>
      rewrite_node(
        ~hit=same_node(t),
        ~rewrite=
          e => {
            let def = pad(e |> strip_leading |> strip_trailing);
            /* focus the fresh binder: keeps the caret in the
               let's gesture zone (next Up = hoist) and ready for
               rename */
            let xp = pad(fresh_pat(Var(x)));
            let let_node = fresh(Let(xp, def, fresh(Var(x))));
            Some((
              parens ? fresh(Parens(let_node)) : let_node,
              Pat.rep_id(xp),
            ));
          },
        program,
      );
    /* GLOM (CSE): the landing line already binds this exact
       expression — reuse its binder instead of minting a duplicate
       (the extracted occurrence just becomes a use; nothing new is
       created). Gated on the name reaching the occurrence unshadowed. */
    let glom_at = (host: Exp.t, line: Exp.t, t: Exp.t) =>
      switch (IdTagged.term_of(host)) {
      | Let(lp, ldef, lbody) when same_node(lbody, line) && eq_defs(ldef, t) =>
        switch (let_head_name(lp)) {
        | Some(n) when !List.mem(n, binders_over(Exp.rep_id(t), lbody)) =>
          let s = Slot.of_exp(t);
          let use = Slot.give(s, fresh(Var(n)));
          rewrite_node(
            ~hit=same_node(line),
            ~rewrite=
              ln => {
                let ln' = replace_node(~at=Exp.rep_id(t), ~with_=use, ln);
                Some((ln', Exp.rep_id(use)));
              },
            program,
          );
        | _ => None
        }
      | _ => None
      };
    let glom = (~path: list(Exp.t), line: Exp.t, t: Exp.t) => {
      /* the definition the new binding would land directly UNDER:
         the line's parent when the line is its body */
      let host = {
        let rec find = (path: list(Exp.t)) =>
          switch (path) {
          | [parent, child, ..._] when same_node(child, line) =>
            Some(parent)
          | [_, ...rest] => find(rest)
          | [] => None
          };
        find(path);
      };
      switch (host) {
      | Some(host) => glom_at(host, line, t)
      | None => None
      };
    };
    /* the new binding lands at the nearest enclosing line; the use
       site takes over the extracted node's whitespace slot, and the
       line keeps its layout via a fresh copy of its leading run */
    let to_block = (path: list(Exp.t), line: Exp.t, t: Exp.t) => {
      let s = Slot.of_exp(t);
      let def = pad(Slot.drop(s, t));
      let use = Slot.give(s, fresh(Var(x)));
      /* an introduced let ALWAYS gets its own line (andrew: extract's
         directionality only holds if the displaced content actually
         moves). Line slots that already start a line keep their lead
         shape; INLINE sub-slots (fun/arm bodies, chain body slots)
         synthesize a break with the nearest enclosing line's indent
         + 2 — the ancestor path supplies the indent. */
      let sep = {
        let s = sep_like(Slot.of_exp(line).lead);
        if (has_newline(s)) {
          s;
        } else {
          /* nearest ancestor (deepest-first, above the line) whose
             slot starts a line: copy its break + indent, plus 2 */
          let rec prefix_to = (acc, path: list(Exp.t)) =>
            switch (path) {
            | [] => acc
            | [n, ..._] when same_node(n, line) => acc
            | [n, ...rest] => prefix_to([n, ...acc], rest)
            };
          let ancestor_sep =
            prefix_to([], path)
            |> List.find_map((a: Exp.t) => {
                 let al = sep_like(Slot.of_exp(a).lead);
                 has_newline(al) ? Some(al) : None;
               });
          switch (ancestor_sep) {
          | _ when same_node(line, program) =>
            /* root line: column 0, no indent */
            newline()
          | Some(al) => al @ space() @ space()
          | None => newline() @ space() @ space()
          };
        };
      };
      let build = (~parens: bool) =>
        rewrite_node(
          ~hit=same_node(line),
          ~rewrite=
            ln => {
              let body = replace_node(~at=Exp.rep_id(t), ~with_=use, ln);
              let (b, a) = body.annotation.secondary;
              let body = {
                ...body,
                annotation: {
                  ...body.annotation,
                  secondary: (sep @ b, a),
                },
              };
              let xp = pad(fresh_pat(Var(x)));
              let let_node = fresh(Let(xp, def, body));
              Some((
                parens ? fresh(Parens(let_node)) : let_node,
                Pat.rep_id(xp),
              ));
            },
          program,
        );
      build(~parens=false);
    };
    switch (extract_path(~target, program)) {
    | None => None
    | Some(path) =>
      let t = List.nth(path, List.length(path) - 1);
      let line = lowest_line(path);
      let blocked =
        crossed_rec_binders(line, path) |> List.exists(n => mentions(n, t));
      switch (blocked ? None : glom(~path, line, t)) {
      | Some(r) => Some(r)
      | None =>
        !blocked && !same_node(line, t)
          ? to_block(path, line, t)
          : in_place(~parens=!same_node(line, t), t)
      };
    };
  },
};

/* === RenameFree: repair-style rename ===
 * The user renames a binder by ordinary editing; the old uses go free
 * (statics marks them). This binds them to the indicated binder. It
 * only ever gives meaning to currently-unbound occurrences — never
 * re-points a bound one — so it needs no knowledge of the old name's
 * history. Blind spot: uses that silently rebound to an OUTER binder
 * of the old name are invisible here (no Free marks). */

let free_marked = (~info_map: Statics.Map.t, e: Exp.t): bool =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(InfoExp({marks, _})) =>
    marks
    |> List.exists((m: Mark.t) =>
         switch (m) {
         | Free(_) => true
         | _ => false
         }
       )
  | _ => false
  };

let free_vars_in = (~info_map: Statics.Map.t, e: Exp.t): list(string) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e': Exp.t) => {
          switch (IdTagged.term_of(e')) {
          | Var(x) when free_marked(~info_map, e') => acc := [x, ...acc^]
          | _ => ()
          };
          cont(e');
        },
      e,
    );
  acc^;
};

/* The rename sites a binder node offers: each bound name paired with
 * the regions it scopes over. Sugar params scope over the RHS only;
 * the sugar fn name (recursive) and fun-valued lets scope over def
 * and body; arm pats scope over their own body (arm chosen by
 * target). */
/* (name, own ids) for each var bound in a pattern: targeting a
 * specific binder token narrows the offer to that name */
let pat_var_id_sites = (p: Pat.t): list((string, list(Id.t))) => {
  let acc = ref([]);
  let _ =
    Pat.map_term(
      ~f_pat=
        (cont, p': Pat.t) => {
          switch (IdTagged.term_of(p')) {
          | Var(x) => acc := [(x, IdTagged.ids(p')), ...acc^]
          | _ => ()
          };
          cont(p');
        },
      p,
    );
  acc^;
};

let rename_sites_unfiltered =
    (~target: Id.t, e: Exp.t): list((string, list(Exp.t))) =>
  switch (IdTagged.term_of(e)) {
  | Let(p, def, body) =>
    switch (sugar_fn_name(p)) {
    | Some(f) =>
      let params =
        pat_var_names(p)
        |> List.sort_uniq(compare)
        |> List.filter(v => v != f);
      [(f, [def, body])] @ (params |> List.map(v => (v, [def])));
    | None =>
      switch (let_head_name(p)) {
      | Some(y) =>
        let regions =
          switch (IdTagged.term_of(def)) {
          | Fun(_) => [def, body]
          | _ => [body]
          };
        [(y, regions)];
      | None => []
      }
    }
  | Fun(p, body, _, _) =>
    pat_var_names(p)
    |> List.sort_uniq(compare)
    |> List.map(y => (y, [body]))
  | Match(_, rules) =>
    rules
    |> List.concat_map(((p, body)) =>
         List.mem(target, pat_subtree_ids(p))
           ? pat_var_names(p)
             |> List.sort_uniq(compare)
             |> List.map(y => (y, [body]))
           : []
       )
  | _ => []
  };

/* Targeting: the binder-name TOKEN is the only rename affordance —
 * not the construct's delimiters, not pattern punctuation (a caret on
 * the comma in `let f(x, y)` means neither f nor x). */
let rename_sites = (~target: Id.t, e: Exp.t): list((string, list(Exp.t))) => {
  let pats =
    switch (IdTagged.term_of(e)) {
    | Let(p, _, _)
    | Fun(p, _, _, _) => [p]
    | Match(_, rules) =>
      rules
      |> List.filter_map(((p, _)) =>
           List.mem(target, pat_subtree_ids(p)) ? Some(p) : None
         )
    | _ => []
    };
  switch (
    pats
    |> List.concat_map(pat_var_id_sites)
    |> List.find_opt(((_, ids)) => List.mem(target, ids))
  ) {
  | Some((y, _)) =>
    rename_sites_unfiltered(~target, e) |> List.filter(((y', _)) => y' == y)
  | None => []
  };
};

let hit_rename = (target: Id.t, e: Exp.t): bool =>
  hit_let(target, e) || hit_fun(target, e) || hit_match_pat(target, e);

/* (free name, binder name) pairs offered at the hit node */
let rename_pairs =
    (~info_map: Statics.Map.t, ~target: Id.t, e: Exp.t)
    : list((string, string)) =>
  rename_sites(~target, e)
  |> List.concat_map(((y, regions)) =>
       regions
       |> List.concat_map(free_vars_in(~info_map))
       |> List.sort_uniq(compare)
       |> List.filter(x => x != y)
       |> List.map(x => (x, y))
     )
  |> List.sort_uniq(compare);

/* rewrite free occurrences of x to y, skipping scopes where y is
 * rebound (they'd capture to the wrong binder); bound x's are already
 * excluded by the Free-mark filter */
let rename_free_in =
    (
      ~info_map: Statics.Map.t,
      ~count: ref(int),
      x: string,
      y: string,
      e: Exp.t,
    )
    : Exp.t =>
  map_unshadowed(
    ~skip=y,
    ~f_var=
      e' =>
        switch (IdTagged.term_of(e')) {
        | Var(z) when z == x && free_marked(~info_map, e') =>
          count := count^ + 1;
          Some({
            annotation: {
              ...e'.annotation,
              lexeme: None,
            },
            term: Var(y),
          });
        | _ => None
        },
    e,
  );

let rename_free_impl = (x: string, y: string): impl => {
  label: "Rename " ++ x ++ " to " ++ y,
  tooltip: "Bind free occurrences of " ++ x ++ " at this binding",
  prepare: (~info_map, ~target, program) =>
    x == y
      ? None
      : rewrite_node(
          ~hit=hit_rename(target),
          ~rewrite=
            e =>
              switch (
                rename_sites(~target, e)
                |> List.find_opt(((y', _)) => y' == y)
              ) {
              | Some((_, regions)) =>
                let count = ref(0);
                let ren = rename_free_in(~info_map, ~count, x, y);
                let e' =
                  regions
                  |> List.fold_left(
                       (e, r: Exp.t) =>
                         replace_node(~at=Exp.rep_id(r), ~with_=ren(r), e),
                       e,
                     );
                /* caret stays on the binder token (where the user
                   was editing in the repair flow) */
                let focus = {
                  let pats =
                    switch (IdTagged.term_of(e')) {
                    | Let(p, _, _)
                    | Fun(p, _, _, _) => [p]
                    | Match(_, rules) =>
                      rules
                      |> List.filter_map(((p, _)) =>
                           List.mem(target, pat_subtree_ids(p))
                             ? Some(p) : None
                         )
                    | _ => []
                    };
                  switch (
                    pats
                    |> List.concat_map(pat_var_id_sites)
                    |> List.find_opt(((n, _)) => n == y)
                  ) {
                  | Some((_, [id, ..._])) => id
                  | _ => Exp.rep_id(e')
                  };
                };
                count^ > 0 ? Some((e', focus)) : None;
              | None => None
              },
          program,
        ),
};

/* === Extract type alias === */

let contains_typ_id = (target: Id.t, e: Exp.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_typ=
        (cont, t: Typ.t) => {
          if (List.mem(target, IdTagged.ids(t))) {
            found := true;
          };
          cont(t);
        },
      e,
    );
  found^;
};

/* the exp node that DIRECTLY owns the typ containing target (no exp
   child's subtree contains it), plus the typ node itself */
let typ_extract_site =
    (~target: Id.t, program: Exp.t): option((list(Exp.t), Typ.t)) => {
  let owns = (e: Exp.t) =>
    contains_typ_id(target, e)
    && !(children_of(e) |> List.exists(contains_typ_id(target)));
  switch (find_path(~hit=owns, program)) {
  | None => None
  | Some(path) =>
    let host = List.nth(path, List.length(path) - 1);
    let t = ref(None);
    let _ =
      Exp.map_term(
        ~f_typ=
          (cont, ty: Typ.t) => {
            if (t^ == None && List.mem(target, IdTagged.ids(ty))) {
              t := Some(ty);
            };
            cont(ty);
          },
        host,
      );
    t^ |> Option.map(ty => (path, ty));
  };
};
let alias_extractable = (path: list(Exp.t), ty: Typ.t): bool => {
  let host = List.nth(path, List.length(path) - 1);
  let whole_alias_def =
    switch (IdTagged.term_of(host)) {
    | TyAlias(_, d, _) => IdTagged.rep_id(d) == IdTagged.rep_id(ty)
    | _ => false
    };
  (
    switch (IdTagged.term_of(ty)) {
    | Var(_)
    | Unknown(_) => false
    | _ => true
    }
  )
  && !whole_alias_def;
};

let extract_alias_impl: impl = {
  label: "Extract",
  tooltip: "Name this type with a fresh alias at the enclosing line",
  prepare: (~info_map as _, ~target, program) =>
    switch (typ_extract_site(~target, program)) {
    | None => None
    | Some((path, ty)) when alias_extractable(path, ty) =>
      let line = lowest_line(path);
      let blocked =
        crossed_typ_binders(line, path)
        |> List.exists(n => typ_mentions([n], ty));
      if (blocked) {
        None;
      } else {
        let name =
          pick_placeholder(
            ~transform=String.capitalize_ascii,
            used_names(program) @ typ_names_in(program),
          );
        let s = typ_slot(ty);
        let def =
          typ_slot_give(
            {
              Slot.lead: space(),
              trail: space(),
            },
            typ_slot_drop(s, ty),
          );
        /* fresh use var: the extracted content travels WHOLESALE to
           the def (ids kept — P7); focus goes to the new tpat */
        let use: Typ.t =
          typ_slot_give(
            s,
            {
              annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
              term: Var(name),
            },
          );
        let tp: TPat.t = {
          annotation: {
            ...IdTagged.IdTag.mk_internal([Id.mk()]),
            secondary: (space(), space()),
          },
          term: Var(name),
        };
        let sep = sep_like(Slot.of_exp(line).lead);
        rewrite_node(
          ~hit=same_node(line),
          ~rewrite=
            ln => {
              let body =
                replace_typ_node(~at=IdTagged.rep_id(ty), ~with_=use, ln);
              let (b, a) = body.annotation.secondary;
              let body = {
                ...body,
                annotation: {
                  ...body.annotation,
                  secondary: (sep @ b, a),
                },
              };
              let alias_node = fresh(TyAlias(tp, def, body));
              Some((alias_node, IdTagged.rep_id(tp)));
            },
          program,
        );
      };
    | Some(_) => None
    },
};

/* === Explode / Implode (definition normalization) ===
 * EXPLODE POLICY — tune here (andrew). Class-blind: operators,
 * calls, and builtins all count as ONE OPERATION (the operator/call
 * split is language trivia, not user knowledge). Atoms: variables,
 * literals, holes, bare constructors. A definition is REDUCED when
 * it is one operation over atoms; CONTAINERS (tuples, lists,
 * constructor wraps) carry no information of their own, nest
 * freely, and may hold one operation level inline in each
 * component; fun/case/if are opaque units (conditional or
 * binder-scoped interiors can't lift) whose scrutinee/condition is
 * an ordinary strict operand. Future knobs live here: a complexity
 * budget over operator chains, an "explode fully" variant. */

let rec x_atomic = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Var(_)
  | Atom(_)
  | EmptyHole
  | Label(_)
  | Undefined
  | Deferral(_)
  | BuiltinFun(_)
  | LivelitName(_)
  | Constructor(_, _) => true
  | Parens(x) => x_atomic(x)
  | TupLabel(l, x) => x_atomic(l) && x_atomic(x)
  | Tuple(xs)
  | ListLit(xs) => List.for_all(x_atomic, xs)
  | _ => false
  };

let ctor_headed = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | Ap(_, f, _) =>
    switch (IdTagged.term_of(f)) {
    | Constructor(_, _) => true
    | _ => false
    }
  | _ => false
  };

/* call args: unwrap the arg parens/tuple to the component list */
let arg_components = (arg: Exp.t): list(Exp.t) => {
  let rec inner = (e: Exp.t) =>
    switch (IdTagged.term_of(e)) {
    | Parens(x) => inner(x)
    | Tuple(xs) => xs
    | _ => [e]
    };
  inner(arg);
};

/* one operation over atoms (the reduced-line shape) */
let x_one_op = (e: Exp.t): bool =>
  switch (IdTagged.term_of(e)) {
  | BinOp(_, a, b)
  | Cons(a, b)
  | ListConcat(a, b)
  | Dot(a, b) => x_atomic(a) && x_atomic(b)
  | UnOp(_, a) => x_atomic(a)
  | TypAp(a, _) => x_atomic(a)
  | Ap(_, f, arg) =>
    x_atomic(f) && arg_components(arg) |> List.for_all(x_atomic)
  | _ => false
  };

/* already-reduced definition (the gate: explode offers only when
   this is false) */
let rec x_reduced = (e: Exp.t): bool =>
  x_atomic(e)
  || x_one_op(e)
  || (
    switch (IdTagged.term_of(e)) {
    | Parens(x) => x_reduced(x)
    /* containers: each component atomic or one op; containers of
       containers recurse */
    | Tuple(xs)
    | ListLit(xs) => xs |> List.for_all(x_component_reduced)
    | TupLabel(_, x) => x_component_reduced(x)
    | Ap(_, f, arg) when ctor_headed(e) =>
      ignore(f);
      arg_components(arg) |> List.for_all(x_component_reduced);
    /* opaque units: interiors untouched; strict head must be atomic */
    | Fun(_)
    | TypFun(_) => true
    | Match(scrut, _) => x_atomic(scrut)
    | If(c, _, _) => x_atomic(c)
    | _ => false
    }
  )
and x_component_reduced = (e: Exp.t): bool =>
  x_atomic(e)
  || x_one_op(e)
  || (
    switch (IdTagged.term_of(e)) {
    | Parens(x) => x_component_reduced(x)
    | Tuple(xs)
    | ListLit(xs) => xs |> List.for_all(x_component_reduced)
    | TupLabel(_, x) => x_component_reduced(x)
    | Ap(_, _, arg) when ctor_headed(e) =>
      arg_components(arg) |> List.for_all(x_component_reduced)
    | _ => false
    }
  );

let explode_impl: impl = {
  label: "Explode",
  tooltip: "Name every intermediate computation with its own binding",
  prepare: (~info_map as _, ~target, program) =>
    rewrite_node(
      ~hit=hit_let(target),
      ~rewrite=
        e =>
          switch (IdTagged.term_of(e)) {
          | Let(p, def, body)
              when
                let_head_name(p) != None
                && sugar_fn_name(p) == None
                && !x_reduced(def) =>
            let used = ref(used_names(program));
            let bindings: ref(list((string, Exp.t))) = ref([]);
            /* lift: the subtree travels wholesale (ids kept); the
               replacement var takes over its textual slot; the
               lifted def normalizes recursively (inner bindings
               emit first = evaluation order) */
            let rec lift = (sub: Exp.t): Exp.t => {
              let s = Slot.of_exp(sub);
              let d = norm_root(Slot.drop(s, sub));
              let name = pick_placeholder(used^);
              used := [name, ...used^];
              bindings := [(name, d), ...bindings^];
              Slot.give(s, fresh(Var(name)));
            }
            and ensure_atom = (x: Exp.t): Exp.t => x_atomic(x) ? x : lift(x)
            and op_rebuild = (x: Exp.t): Exp.t =>
              switch (IdTagged.term_of(x)) {
              | BinOp(op, a, b) => {
                  ...x,
                  term: BinOp(op, ensure_atom(a), ensure_atom(b)),
                }
              | Cons(a, b) => {
                  ...x,
                  term: Cons(ensure_atom(a), ensure_atom(b)),
                }
              | ListConcat(a, b) => {
                  ...x,
                  term: ListConcat(ensure_atom(a), ensure_atom(b)),
                }
              | Dot(a, b) => {
                  ...x,
                  term: Dot(ensure_atom(a), b),
                }
              | UnOp(op, a) => {
                  ...x,
                  term: UnOp(op, ensure_atom(a)),
                }
              | TypAp(a, t) => {
                  ...x,
                  term: TypAp(ensure_atom(a), t),
                }
              | Ap(dir, f, arg) => {
                  ...x,
                  term: Ap(dir, ensure_atom(f), args_rebuild(arg)),
                }
              | _ => x
              }
            and args_rebuild = (arg: Exp.t): Exp.t =>
              switch (IdTagged.term_of(arg)) {
              | Parens(x) => {
                  ...arg,
                  term: Parens(args_rebuild(x)),
                }
              | Tuple(xs) => {
                  ...arg,
                  term: Tuple(List.map(ensure_atom, xs)),
                }
              | _ => ensure_atom(arg)
              }
            and component = (x: Exp.t): Exp.t =>
              if (x_atomic(x)) {
                x;
              } else {
                switch (IdTagged.term_of(x)) {
                | Parens(inner) => {
                    ...x,
                    term: Parens(component(inner)),
                  }
                | Tuple(xs) => {
                    ...x,
                    term: Tuple(List.map(component, xs)),
                  }
                | ListLit(xs) => {
                    ...x,
                    term: ListLit(List.map(component, xs)),
                  }
                | TupLabel(l, inner) => {
                    ...x,
                    term: TupLabel(l, component(inner)),
                  }
                | Ap(_) when ctor_headed(x) => ctor_rebuild(x)
                | BinOp(_)
                | Cons(_)
                | ListConcat(_)
                | Dot(_)
                | UnOp(_)
                | TypAp(_)
                | Ap(_) => op_rebuild(x)
                | _ => lift(x)
                };
              }
            and ctor_rebuild = (x: Exp.t): Exp.t =>
              switch (IdTagged.term_of(x)) {
              | Ap(dir, f, arg) => {
                  ...x,
                  term: Ap(dir, f, ctor_args(arg)),
                }
              | _ => x
              }
            and ctor_args = (arg: Exp.t): Exp.t =>
              switch (IdTagged.term_of(arg)) {
              | Parens(x) => {
                  ...arg,
                  term: Parens(ctor_args(x)),
                }
              | Tuple(xs) => {
                  ...arg,
                  term: Tuple(List.map(component, xs)),
                }
              | _ => component(arg)
              }
            and norm_root = (x: Exp.t): Exp.t =>
              if (x_atomic(x)) {
                x;
              } else {
                switch (IdTagged.term_of(x)) {
                | Parens(inner) => {
                    ...x,
                    term: Parens(norm_root(inner)),
                  }
                | Tuple(_)
                | ListLit(_)
                | TupLabel(_) => component(x)
                | Ap(_) when ctor_headed(x) => ctor_rebuild(x)
                | BinOp(_)
                | Cons(_)
                | ListConcat(_)
                | Dot(_)
                | UnOp(_)
                | TypAp(_)
                | Ap(_) => op_rebuild(x)
                | Match(scrut, arms) => {
                    ...x,
                    term: Match(ensure_atom(scrut), arms),
                  }
                | If(c, t, alt) => {
                    ...x,
                    term: If(ensure_atom(c), t, alt),
                  }
                | _ => x
                };
              };
            let def' = norm_root(def);
            switch (List.rev(bindings^)) {
            | [] => None
            | bs =>
              /* one binding per line, always (the point is
                 probe-able intermediates) — inherit indent when the
                 line has one, else a bare break */
              let sep = {
                let s = sep_like(Slot.of_exp(e).lead);
                has_newline(s) ? s : newline();
              };
              let with_sep = (x: Exp.t): Exp.t => {
                let (b, a) = x.annotation.secondary;
                {
                  ...x,
                  annotation: {
                    ...x.annotation,
                    secondary: (copy_runs(sep) @ b, a),
                  },
                };
              };
              let inner: Exp.t = {
                ...e,
                term: Let(p, def', body),
              };
              let result =
                List.fold_right(
                  ((n, d), acc) =>
                    fresh(
                      Let(pad(fresh_pat(Var(n))), pad(d), with_sep(acc)),
                    ),
                  bs,
                  inner,
                );
              Some((result, Pat.rep_id(p)));
            };
          | _ => None
          },
      program,
    ),
};

/* IMPLODE: explode's inverse, conservative-transitive — repeatedly
   fold the binding DIRECTLY ABOVE into this definition when it is
   single-use and its use lies in the def region (not in the
   continuation below). Each step is a plain InlineLet, so capture
   freshening, comment rules, and id travel are inherited. */
/* the parent binding an implode step would fold in — the cheap
   shape check (gating-safe: no printing) */
let implode_parent = (~y_id: Id.t, program: Exp.t): option(Id.t) =>
  switch (find_path(~hit=e => Exp.rep_id(e) == y_id, program)) {
  | Some(path) when List.length(path) >= 2 =>
    let n = List.length(path);
    let y = List.nth(path, n - 1);
    let parent = List.nth(path, n - 2);
    switch (IdTagged.term_of(parent), IdTagged.term_of(y)) {
    | (Let(pp, _, pbody), Let(_, _, ybody))
        when same_node(pbody, y) && sugar_fn_name(pp) == None =>
      switch (let_head_name(pp)) {
      | Some(x) =>
        switch (occurrences_of(x, pbody)) {
        | [occ] when !List.mem(Exp.rep_id(occ), exp_subtree_ids(ybody)) =>
          Some(Exp.rep_id(parent))
        | _ => None
        }
      | None => None
      }
    | _ => None
    };
  | _ => None
  };

let implode_step = (~info_map, ~y_id: Id.t, program: Exp.t): option(Exp.t) =>
  switch (implode_parent(~y_id, program)) {
  | Some(pid) =>
    inline_let_impl.prepare(~info_map, ~target=pid, program)
    |> Option.map(fst)
  | None => None
  };

let implode_impl: impl = {
  label: "Implode",
  tooltip: "Fold the single-use bindings above back into this definition",
  prepare: (~info_map, ~target, program) =>
    switch (find_hit(~hit=hit_let(target), program)) {
    | Some(y) =>
      let y_id = Exp.rep_id(y);
      let rec go = (prog: Exp.t, k: int): option(Exp.t) =>
        switch (implode_step(~info_map, ~y_id, prog)) {
        | Some(prog') => go(prog', k + 1)
        | None => k > 0 ? Some(prog) : None
        };
      switch (go(program, 0)) {
      | Some(prog') =>
        let focus =
          switch (find_hit(~hit=e => Exp.rep_id(e) == y_id, prog')) {
          | Some(y') =>
            switch (IdTagged.term_of(y')) {
            | Let(p, _, _) => Pat.rep_id(p)
            | _ => y_id
            }
          | None => y_id
          };
        Some((prog', focus));
      | None => None
      };
    | None => None
    },
};
let rename_typ_pairs =
    (~target: Id.t, program: Exp.t): list((string, string)) =>
  switch (find_hit(~hit=hit_tyalias(target), program)) {
  | Some(e) =>
    switch (IdTagged.term_of(e)) {
    | TyAlias(tp, _, body) =>
      switch (tpat_names(tp)) {
      | [t] =>
        let enclosing =
          switch (find_path(~hit=same_node(e), program)) {
          | Some(path) =>
            path
            |> List.concat_map((a: Exp.t) =>
                 switch (IdTagged.term_of(a)) {
                 | TyAlias(atp, _, _)
                 | TypFun(atp, _, _) => tpat_names(atp)
                 | _ => []
                 }
               )
          | None => []
          };
        typ_names_mentioned(body)
        |> List.filter(x =>
             x != t && !List.mem(x, enclosing) && mentions_typ_free(x, body)
           )
        |> List.map(x => (x, t));
      | _ => []
      }
    | _ => []
    }
  | None => []
  };

let rename_typ_free_impl = (x: string, t: string): impl => {
  label: "Rename " ++ x ++ " to " ++ t,
  tooltip: "Bind free type mentions of " ++ x ++ " at this alias",
  prepare: (~info_map as _, ~target, program) =>
    x == t
      ? None
      : rewrite_node(
          ~hit=hit_tyalias(target),
          ~rewrite=
            e =>
              switch (IdTagged.term_of(e)) {
              | TyAlias(tp, d, body) when tpat_names(tp) == [t] =>
                Some((
                  {
                    ...e,
                    term: TyAlias(tp, d, rename_typ_var(x, t, body)),
                  },
                  IdTagged.rep_id(tp),
                ))
              | _ => None
              },
          program,
        ),
};
