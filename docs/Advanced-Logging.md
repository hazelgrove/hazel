The js_of_ocaml devs recommended `logs` for performance and flexibility:

https://opam.ocaml.org/packages/logs/

To use it, we need to install a reporter during app initialization (see `init_log`):

https://github.com/hazelgrove/hazel/blob/type-alias-3/src/hazelweb/Logger.re

We also need to set the logging level here.
At time of writing, the logging level is set to `Logs.Debug`.
In the future, this should be changed to `Logs.Info` or something even less verbose in dev.

Some considerations when choosing a level:

- All instrumented functions (see `Log.fun_call` below) report invocations at level `Logs.Info`.
- Any instrumented functions on the watch list (see `watch_list` below) report arguments and return values at level `Logs.Debug`.

We use a custom reporter that produces timestamped, level-specific messages in the browser console:

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

Once this is done, invoking `Action_Exp.syn_perform` will record the invocation at level `Logs.Info` and the arguments and return value at level `Logs.Debug`.

The `Log` module also contains an optional "watch list" filter with a simple eDSL to configure it.
When the filter is disabled, all instrumented functions will report arguments and return values.
When enabled, invoking a watched function produces a more verbose log entry.

To disable the filter, open up `hazelcore/Log.re` and set `watch_list` to `None`:

```reason
let watch_list = None;
```

To watch a particular function, e.g., `Action_Exp.syn_perform`:

```reason
let watch_list =
  Some(Filter.(md(pre("Action_Exp"))) +^ fn(eq("syn_perform")));
```

Filtering works by splitting the `__FUNCTION__` string into two parts&mdash;a "module" name and a "function" name&mdash;at the last dot (".").

Beware, this is a somewhat crude approximation.
For example, tracing through an anonymous function call adds a ".(fun)" to the module name.
So, we use string prefix equality (`pre`) on the module name (`md`) instead of whole-string equality (`eq`).

As a realistic example, to trace an action-driven bug introduced by new features to existing code paths:

```reason
let watch_list =
  Some(
    Filter.(
      fn(has("perform")) /^ fn(has("elab")) /^ fn(has("fix_holes"))
    ),
  );
```

and to trace a action-driven bug that only arises in expressions in synthetic position:

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

and for comparison, a similar one where the `fn(pre("syn_"))` constraint only applies to functions in `Action_Exp`:

```reason

let watch_list =
  Some(
    Filter.(
      (md(pre("Action_Exp")) +^ fn(pre("syn_")))
      /^ md(pre("Elaborator_Exp"))
      /^ md(pre("Statics_Exp"))
    ),
  );
```

There are two operators for composing constraints:

- `+^` is conjunction (logical AND)
- `/^` is disjunction (logical OR)

and, in addition to the `eq` and `pre` string comparators, we have:

- `suf` is string suffix matching
- `has` is substring matching
- `re` is (Perl) regular expression matching

And finally, `Log` provides a few other useful functions:

- `Log.debug_msg(msg)` simply writes the log level, a timestamp, and `msg`.
- `Log.debug_state(__FUNCTION__, str, sexp)` is similar, but writes something like `"str = " ++ Sexplib.Sexp.to_string_hum(sexp)`.
- `Log.debug_states(__FUNCTION__)` is just `debug_state` lifted to lists of (`str`, `sexp`) pairs.

All three of these functions use level `Log.Debug` and none of them consult the watch list.
They are intended for use as transient helpers and should not be committed to dev.

The filtering dsl is easy to extend and will become significantly more expressive as I apply it to more complex scenarios.
For example, in the coming weeks I'll probably add match negation, and a "call stack" to drive context-dependent selections such as:

- any function that calls a particular function
- any function that is called by a particular function
- any function that calls into a particular module
- any function that is called from a particular module
