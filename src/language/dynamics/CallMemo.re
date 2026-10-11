/* Reusing a function call's result from an earlier evaluation, when the
   call is the same: for evaluating a livelit's view again after one field
   of its model changed. The face on Kids' Choice is drawn by helpers
   (eye, mouth, teeth_of, sick_pile, ...), and a new eye size changes the
   arguments of only two of its calls; the rest are the calls of the last
   evaluation, made again.

   A call is the step from `f(arg)` to f's body under f's environment
   extended with the parameter: Closure(env, body). Its result depends on
   the body and on the values of the variables the body reads, and on
   nothing else -- for a pure evaluation, which a view's is (it builds a
   command; running it comes after). So the key is the body, by identity
   (it is the same object while the livelit's definition is), and the
   values of the variables it reads, compared by structure:

   - ids are left out: each evaluation mints new ones for what it builds;
   - a closure among those values is compared the same way, by its body
     and the values of the variables IT reads, not by its whole
     environment, which holds the model even for a helper that never
     reads it;
   - "the variables a body reads" is every name that occurs in it as a
     variable, including names it binds itself: more than it reads, which
     can only cost a reuse, never give a wrong one.

   Only calls that took at least [min_steps] steps last time are looked
   up or remembered, so a small call costs no more than a check. */
open Util;

let min_steps = 40;
let max_entries = 8;

/* every name occurring in a body as a variable, sorted */
let vars_of: Exp.t => list(Var.t) =
  IdentityMemo.memo((body: Exp.t) => {
    let names = ref([]);
    let _: Exp.t =
      Exp.map_term(
        ~f_exp=
          (continue, e: Exp.t) => {
            switch (e.term) {
            | Var(x) => names := [x, ...names^]
            | _ => ()
            };
            continue(e);
          },
        body,
      );
    List.sort_uniq(String.compare, names^);
  });

/* An id-free form of a value, for comparing by structure: every id the
   same, every closure its body and the values of its body's variables. */
let anon: 'a. IdTagged.t('a) => IdTagged.t('a) =
  t => {
    ...t,
    annotation: {
      ...IdTagged.IdTag.mk_internal([Id.invalid]),
      lexeme: t.annotation.lexeme,
    },
  };
let anon_f:
  'a.
  (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
 =
  (continue, t) => anon(continue(t));

/* Each node's form is remembered by the node's identity, not only a whole
   value's: a recursive helper walking a list is called on each of its
   tails, and the tails are shared, so each is put in form (and hashed)
   once, not once per call. */
let forms: Lazy.t(IdentityTable.t(Exp.t, Exp.t)) =
  lazy(IdentityTable.create());
let rec norm = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=norm_exp,
    ~f_pat=anon_f,
    /* types are kept as they are, ids and all: a view's types come from
       its livelit's elaboration, the same objects each time (and hashed
       once); putting them in form copied the Html type into every
       value with a cast. A type made afresh can only cost a reuse. */
    ~f_typ=(_, t) => t,
    ~f_tpat=anon_f,
    ~f_rul=anon_f,
    e,
  )
and norm_exp = (continue, e: Exp.t): Exp.t => {
  let forms = Lazy.force(forms);
  switch (IdentityTable.find_opt(forms, e)) {
  | Some(form) => form
  | None =>
    let form =
      switch (e.term) {
      | Closure(env, body) => closure(env, body)
      | FixF(p, body, Some(env)) =>
        closure(
          env,
          {
            ...e,
            term: FixF(p, body, None),
          },
        )
      | _ => anon_f(continue, e)
      };
    IdentityTable.replace(forms, e, form);
    form;
  };
}
/* a closure: its code, and what its code can read from its environment */
and closure = (env: Environment.t(Exp.t), code: Exp.t): Exp.t => {
  let inner: TermBase.Exp.term =
    Grammar.Tuple([norm(code), values(env, code)]);
  let term: TermBase.Exp.term =
    Grammar.Closure(
      Environment.empty,
      IdTagged.mk_internal([Id.invalid], inner),
    );
  IdTagged.mk_internal([Id.invalid], term);
}
/* the values of a body's variables in an environment, in name order */
and values = (env: Environment.t(Exp.t), body: Exp.t): Exp.t => {
  let undefined: TermBase.Exp.term = Grammar.Undefined;
  let term: TermBase.Exp.term =
    Grammar.Tuple(
      List.map(
        x =>
          switch (Environment.lookup(env, x)) {
          | Some(v) => norm(v)
          | None => IdTagged.mk_internal([Id.invalid], undefined)
          },
        vars_of(body),
      ),
    );
  IdTagged.mk_internal([Id.invalid], term);
};

type entry = {
  key: Exp.t,
  value: Exp.t,
};

type t = {
  /* bodies whose calls took at least min_steps steps */
  costly: IdentityTable.t(Exp.t, unit),
  /* each costly body's last calls, newest first */
  entries: IdentityTable.t(Exp.t, list(entry)),
  /* how many calls were reused, for tests and measuring */
  hits: ref(int),
};

let create = (): t => {
  costly: IdentityTable.create(),
  entries: IdentityTable.create(),
  hits: ref(0),
};

let is_costly = (memo: t, body: Exp.t): bool =>
  IdentityTable.find_opt(memo.costly, body) != None;

let key = (env: Environment.t(Exp.t), body: Exp.t): Exp.t =>
  values(env, body);

/* the result of an earlier call of [body] with this key */
let find = (memo: t, body: Exp.t, key: Exp.t): option(Exp.t) =>
  switch (IdentityTable.find_opt(memo.entries, body)) {
  | None => None
  | Some(entries) =>
    switch (
      List.find_opt((e: entry) => ValueHash.same(e.key, key), entries)
    ) {
    | Some(e) =>
      incr(memo.hits);
      Some(e.value);
    | None => None
    }
  };

/* After a call of [body] under [env] took [steps] steps and gave [value]:
   remember it, if it was costly. [key] is the call's key when the caller
   already computed it. */
let record =
    (
      memo: t,
      ~steps: int,
      ~key as k: option(Exp.t)=?,
      env: Environment.t(Exp.t),
      body: Exp.t,
      value: Exp.t,
    )
    : unit =>
  if (steps >= min_steps) {
    IdentityTable.replace(memo.costly, body, ());
    let k =
      switch (k) {
      | Some(k) => k
      | None => key(env, body)
      };
    let old =
      switch (IdentityTable.find_opt(memo.entries, body)) {
      | Some(es) => List.filter((e: entry) => !ValueHash.same(e.key, k), es)
      | None => []
      };
    IdentityTable.replace(
      memo.entries,
      body,
      ListUtil.take(
        max_entries,
        [
          {
            key: k,
            value,
          },
          ...old,
        ],
      ),
    );
  };
