open Util;
open BuiltinUtil;

/*
   Built-in functions for Hazel.
   Update src/menhirParser/Lexer.mll when any new builtin is added
 */

let builtins =
  BuiltinsBase.builtins
  |> concat(
       _,
       List.map(
         ((n, b)) => (n, of_atom_builtin(b)),
         Atom.converter_builtins,
       ),
     )
  |> concat(
       _,
       List.map(((n, b)) => (n, of_atom_builtin(b)), Operators.builtins),
     )
  |> concat(
       _,
       List.map(
         ({name, arg, ret, imp, _}: hazel_fn) =>
           (name, HazelFn(arg |> Typ.fresh, ret |> Typ.fresh, imp)),
         BuiltinsList.builtins @ BuiltinsADT.builtins,
       ),
     );

let entries =
  List.concat([
    List.map(
      fun
      | (name, Const(typ, _)) =>
        Ctx.VarEntry({
          name,
          typ,
          id: Id.invalid,
        })
      | (name, Fn(t1, t2, _))
      | (name, HazelFn(t1, t2, _)) =>
        Ctx.VarEntry({
          name,
          typ: Fresh.Typ.arrow(t1, t2),
          id: Id.invalid,
        }),
      builtins,
    ),
    List.map(entry => Ctx.LivelitEntry(entry), Livelit.livelits),
    BuiltinsADT.entries,
  ]);

let ctx_init: option(Operators.mode) => Ctx.t =
  use_mode => {
    use_mode,
    entries,
  };

let forms_init: forms =
  List.filter_map(
    fun
    | (_, Const(_)) => None
    | (name, Fn(_, _, f)) => Some((name, f))
    | (name, HazelFn(_, _, f)) =>
      Some((
        name,
        (
          (d: DHExp.t) => {
            Some(Fresh.Exp.ap(Forward, f, d));
          }
        ),
      )),
    builtins,
  );

let env_init: Environment.t =
  List.fold_left(
    env =>
      fun
      | (name, Const(_, d)) => Environment.extend(env, (name, d))
      | (name, Fn(_))
      | (name, HazelFn(_, _, _)) =>
        Environment.extend(env, (name, Fresh.Exp.builtin_fun(name))),
    Environment.empty,
    builtins,
  );
