open ExplainThisForm;
open Example;

/* No triv_exp doc: `decide` routes Tuple([]) in expression position to the
   function/let pattern cases, and in pattern position to TerminalPat.triv, so
   an exp-side trivial-value group is unreachable. */

let deferral_exp_ex = {
  sub_id: Deferral,
  term:
    mk_example(
      "let plus = fun (x, y) -> x + y in\nlet incr = plus(_, 1) in\nincr(5)",
    ),
  message: "In the partial application plus(_, 1), the deferral expression marks the first argument, which was not applied until in the full function application incr(5).",
};
let deferral_exp: form = {
  id: DeferralExp,
  syntactic_form: [exp("_")],
  colorings: [],
  expandable_id: None,
  explanation: "Marks an argument that has not yet been applied in a partial application.",
  examples: [deferral_exp_ex],
};
let deferral_exps: group = {
  id: DeferralExp,
  forms: [deferral_exp],
};

let bool_exp = (b: bool): form => {
  id: BoolExp,
  syntactic_form: [exp(b |> string_of_bool)],
  colorings: [],
  expandable_id: None,
  explanation: "A boolean literal, either `true` or `false`.",
  examples: [],
};
let bool_exps = (b: bool): group => singleton(bool_exp(b));

let int_exp = (n: Bigint.t): form => {
  id: IntExp,
  syntactic_form: [n |> Bigint.to_string |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "A number literal.",
  examples: [],
};
let int_exps = (i: Bigint.t): group => singleton(int_exp(i));

let sint_exp = (n: int): form => {
  id: SIntExp,
  syntactic_form: [n |> string_of_int |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "A system integer literal.",
  examples: [],
};
let sint_exps = (i: int): group => singleton(sint_exp(i));

let nat_exp = (n: Bigint.t): form => {
  id: NatExp,
  syntactic_form: [n |> Bigint.to_string |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "A natural number literal.",
  examples: [],
};
let nat_exps = (i: Bigint.t): group => singleton(nat_exp(i));

let float_exp = (f: float): form => {
  id: FloatExp,
  syntactic_form: [f |> string_of_float |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "A floating-point literal.",
  examples: [],
};
let float_exps = (f: float): group => singleton(float_exp(f));

let string_exp = (s: string): form => {
  id: StringExp,
  syntactic_form: [s |> abbreviate |> Haz3lcore.Token.string_quote |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "A string literal. Any character besides double quotes (`\"`) can be used.",
  examples: [],
};
let string_exps = (s: string): group => singleton(string_exp(s));

let var_exp = (n: string): form => {
  id: VarExp,
  syntactic_form: [n |> abbreviate |> exp],
  colorings: [],
  expandable_id: None,
  explanation: "Takes the value of the expression that it was bound to.",
  examples: [],
};
let var_exps = (x: string): group => singleton(var_exp(x));

/* Most livelits are self-contained, and the generic explanation is the whole
   story. A livelit whose model names state living outside Hazel needs to say
   so, since that changes what editing and copying it mean. */
let livelit_name_explanation = (n: string): string =>
  switch (n) {
  | "fumola_thunk" => "Runs a Fumola program inside a named thunk and expands to its result. Its model is (instance, thunk name, program). Re-forcing the same thunk on an edit reuses its execution history, so state written in one edit is there in the next. The thunk name is Fumola source for a symbol -- `myThunk, 7, `a(`b) -- parsed by Fumola itself; two thunks in one runtime must not share a name. `pointer(s)`, `get(s)` and `peek(s)` are in scope, and the Fumola module library is bound at the top level. Note that `:=` turns its left side into a pointer but `@` does not, so read a cell back with `get`."
  | "fumola_editor" => "Runs a Fumola program at the top level of a Fumola runtime, with no thunk around it, and expands to its result. No incremental reuse, but bindings it makes outlive it and the operations a thunk forbids work here -- `Adapton.reset()` and `Adapton.peekForce` cannot run inside a force. Strictly more expressive than the thunk livelit, which just generates the wrapper and a distinct name for it. Livelits carrying the same instance id share one runtime, so an editor can set up state that a thunk reads."
  | _ => "Expands to some value, and when projected, creates an interactable GUI widget."
  };

let livelit_name_exp = (n: string): form => {
  id: LivelitName,
  syntactic_form: ["^" ++ n |> abbreviate |> exp],
  colorings: [],
  expandable_id: None,
  explanation: livelit_name_explanation(n),
  examples: [],
};
let livelit_name_exps = (x: string): group =>
  singleton(livelit_name_exp(x));

let ctr_exp = (c: string): form => {
  id: CtrExp,
  syntactic_form: [c |> abbreviate |> exp],
  colorings: [],
  expandable_id: None,
  explanation:
    Printf.sprintf("`%s` is a constructor for a sum type variant.", c),
  examples: [],
};
let ctr = (c: string): group => singleton(ctr_exp(c));
