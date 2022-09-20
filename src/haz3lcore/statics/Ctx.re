[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: TermCtx.t,
  typ: TypCtx.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co = {
  term: TermCtx.co,
  typ: TypCtx.co,
};

let empty: t = {term: TermCtx.empty, typ: TypCtx.empty};
let extend_term = (ctx: t, x: Token.t, term_entry): t => {
  term: TermCtx.extend(ctx.term, (x, term_entry)),
  typ: ctx.typ,
};
let extend_typ = (ctx: t, x: Token.t, typ_entry): t => {
  term: ctx.term,
  typ: TypCtx.extend(ctx.typ, (x, typ_entry)),
};
let lookup_term = (ctx: t, x) => TermCtx.lookup(ctx.term, x);
let lookup_typ = (ctx: t, x) => TypCtx.lookup(ctx.typ, x);
let union = (ctx1: t, ctx2: t): t => {
  term: TermCtx.union(ctx1.term, ctx2.term),
  typ: TermCtx.union(ctx1.typ, ctx2.typ),
};

let co_empty: co = {term: TermCtx.empty, typ: TypCtx.empty};
let co_extend_term = (ctx: co, x: Token.t, term_entry): co => {
  term: TermCtx.extend(ctx.term, (x, term_entry)),
  typ: ctx.typ,
};
let co_extend_typ = (ctx: co, x: Token.t, typ_entry): co => {
  term: ctx.term,
  typ: TypCtx.extend(ctx.typ, (x, typ_entry)),
};
let co_lookup_term = (ctx: co, x) => TermCtx.lookup(ctx.term, x);
let co_lookup_typ = (ctx: co, x) => TypCtx.lookup(ctx.typ, x);
let co_union = (ctxs: list(co)): co => {
  let co_union = CtxBase.co_union;
  let term = co_union(List.map(ctx => ctx.term, ctxs));
  let typ = co_union(List.map(ctx => ctx.typ, ctxs));
  {term, typ};
};

let subtract = (ctx: t, free: co): co => {
  term: TermCtx.subtract(ctx.term, free.term),
  typ: TypCtx.subtract(ctx.typ, free.typ),
};
let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  let term = TermCtx.subtract_prefix(ctx.term, prefix_ctx.term);
  Option.map(
    term => {
      let t: t = {term, typ: ctx.typ};
      t;
    },
    term,
  );
};
