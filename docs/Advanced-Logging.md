The js_of_ocaml devs recommended `logs` for performance and flexibility:

https://opam.ocaml.org/packages/logs/

We need to install a reporter during app initialization (see `init_log`):

https://github.com/hazelgrove/hazel/blob/type-alias-3/src/hazelweb/Logger.re

We also set the logging level here (to `Logs.Debug` at time of writing).

Some guidelines on choosing a level:

- All instrumented functions (see `Logs.fun_call` below) report invocations at level `Logs.Info`.
- Any instrumented functions on the watch list (see `watch_list` below) report arguments and return valuies at level `Logs.debug`
- Never use level `Logs.App` outside `hazelweb`.

We use a custom reporter to get timestamped messages going to the browser console:

https://github.com/hazelgrove/hazel/blob/type-alias-3/src/hazelcore/Log.re

To instrument a function for logging, wrap its body in a call to `Log.fun_call(__FUNCTION__)`.

For example, in `Action_Exp.re` we have:

```reason
let rec syn_perform =
        (
          ctx: Context.t,
          a: Action.t,
          (ze: ZExp.t, ty: HTyp.t, u_gen: MetaVarGen.t): Statics.edit_state,
        )
        : ActionOutcome.t(syn_done) => {
  Log.fun_call(
    __FUNCTION__,
    ~args=[
      ("ctx", () => Context.sexp_of_t(ctx)),
      ("a", () => Action.sexp_of_t(a)),
      ("ze", () => ZExp.sexp_of_t(ze)),
      ("ty", () => HTyp.sexp_of_t(ty)),
      ("u_gen", () => MetaVarGen.sexp_of_t(u_gen)),
    ],
    ~id=u_gen,
    ~result_sexp=ActionOutcome.sexp_of_t(sexp_of_syn_done),
    () => ...
```

Once this is done, invoking `Action_Exp.syn_perform` will record the invocation at level `Logs.Info` and arguments and return value at level `Logs.Debug`.

There is also a simple eDSL for configuring an optional "watch list" filter. To disable it, set `watch_list` to `None`:

```reason
// inside Log.re

let watch_list = None;
```

When the filter is disabled, every instrumented function will report its arguments and return value.

When enabled, the watch list consists of a series of rules for matching against module and function names. To watch a particular function, e.g., `Action_Exp.syn_perform`:

```reason
let watch_list =
  Some(Filter.(md(pre("Action_Exp"))) +^ fn(eq("syn_perform")));
```

Filtering works by splitting the __FUNCTION__ string into two parts - a "module" name and a "function" name - at the last dot (".").

Beware, this is a somewhat crude approximation. For example, tracing through an anonymous function call adds a ".(fun)" to the module name (which is why the example above uses string prefix equality (`pre`) on the module name (`md`), as opposed to whole-string equality (`eq`) on the function name (`fn`)).

As a more realistic example, here's how to trace action-drivens bugs introduced by new features to existing code paths:

```reason
let watch_list =
  Some(
    Filter.(
      fn(has("perform")) /^ fn(has("elab")) /^ fn(has("fix_holes"))
    ),
  );
```

And here's one for tracing action expression bugs that only arise in synthetic position:

```reason
let watch_list =
  Some(
    Filter.(
      md(pre("Action_Exp"))
      /^ md(pre("Elaborator_Exp"))
      /^ md(pre("Statics_Exp"))
      +^ fn(pre("syn_"))
    ),
  );
```

In addition to `eq` and `pre`, there's also:

- `suf` - string suffix matching
- `re` - Perl regular expression matching

And finally, `Log` provides a few other useful functions:

- `Log.debug_msg(msg)` simply writes the log level, a timestamp, and `msg`.
- `Log.debug_state(__FUNCTION__, str, sexp)` is similar, but writes something like `"str = " ++ Sexplib.Sexp.to_string_hum(sexp)`.
- `Log.debug_states(__FUNCTION__)` is just `debug_state` lifted to lists of (`str`, `sexp`) pairs.

All of these functions use level `Log.Debug` and none of them consult the watch list. They are intended for use as transient helpers and should not be committed to dev.

In the coming weeks, there's a good chance the filtering dsl will become significantly more expressive. For example, I'll probably add match negation, and a "call stack" to drive context-depdendent selections, such as:

- watch functions called by some (set of) parent function(s)
- report which functions call some other (set of) function(s)
