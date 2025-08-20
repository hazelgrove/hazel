[@deriving (show({with_path: false}), sexp, yojson)]
type t = Grammar.code_slice;

// Structural equality
let equal = (c1: t, c2: t) => c1 == c2;

let empty = Grammar.empty_slice;
let union =
    ({term_ids, ctx_used}: t, {term_ids: term_ids', ctx_used: ctx_used'}: t)
    : t => {
  term_ids: term_ids @ term_ids',
  ctx_used: ctx_used @ ctx_used',
};
let of_ids: list(Id.t) => t =
  term_ids => {
    term_ids,
    ctx_used: [],
  };

let of_ctx: list(Grammar.var_cls) => t =
  ctx_used => {
    term_ids: [],
    ctx_used,
  };

let of_ids_ctx = (ids, ctx) => union(of_ids(ids), of_ctx(ctx));
let append_ids = term_ids => union(of_ids(term_ids));
let append_ctx = term_ids => union(of_ctx(term_ids));

let ids_of = ({term_ids, _}: t) => term_ids;
let ctx_of = ({ctx_used, _}: t) => ctx_used;

let count_term_ids = Fun.compose(List.length, ids_of);
let count_assumptions = Fun.compose(List.length, ctx_of);

// Approximate size
let size = c => count_term_ids(c) + count_assumptions(c);
