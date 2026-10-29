---
schemaVersion: 1
prNumber: 2339
prOwner: hazelgrove
prRepo: hazel
baseSha: a032a928ec9b6a8a54119717b4eb4a3b32dca45a
headSha: 98312d9feea30cd8d6f65de7fc9c668e64144fa6
---
# Evaluator streaming

This PR makes the evaluator **stream**: instead of waiting for a program to finish before showing anything, results appear cell-by-cell as the web worker grinds through them. Getting there meant three intertwined changes — making the evaluator *pausable* so the worker can stop and report progress, making evaluator state *append-only* so partial results from different slices can be merged, and rebuilding the worker into a long-lived service that ACKs instantly and emits a stream of completed sub-results.

Read it in roughly this order: the trampoline gains the ability to yield → state becomes a monoid you can append → the incremental cache becomes a stream → `evaluate` is rebuilt on top of all three → the worker and UI consume the stream.

## A pausable trampoline

The comment at the top of Trampoline.re explains.

<details open>
<summary><code>src/language/dynamics/evaluation/Trampoline.re</code> · /*</summary>

<!-- changetour:hunk file=src/language/dynamics/evaluation/Trampoline.re level=2 highlights=new:42-72 baseBlob=3e007cd7c9901f727d9eac1c740867981144dc97 -->

```diff
@@ -0,0 +1,113 @@
+/*
+  This module defines the custom stack machine for the evaluator (so that we can
+  we don't get OCaml stack overflows).
+
+  We also include the ability to yield a computation so that the webworker can
+  pause and communicate with the main thread.
+ */
+
+// Building blocks of the stack machine.
+
+type t('a) =
+  | Bind(t('b), 'b => t('a)): t('a)
+  | Next(unit => t('a)): t('a)
+  | Done('a): t('a);
+
+type callstack('a, 'b) =
+  | Finished: callstack('a, 'a)
+  | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);
+
+let return = x => Done(x);
+
+let bind = (t, f) => Bind(t, f);
+
+module Syntax = {
+  let (let.trampoline) = (x, f) => bind(x, f);
+};
+
+// Running the stack machine (without yielding).
+
+let rec run: type a b. (t(b), callstack(b, a)) => a =
+  (t: t(b), callstack: callstack(b, a)) => {
+    switch (t) {
+    | Bind(t, f) => run(t, Continue(f, callstack))
+    | Next(f) => run(f(), callstack)
+    | Done(x) =>
+      switch (callstack) {
+      | Finished => x
+      | Continue(f, callstack) => run(f(x), callstack)
+      }
+    };
+  };
+
+let run = t => run(t, Finished);
+
+// Running the stack machine (with yielding).
+
+module Yielding = {
+  type continuation('a) =
+    | Continuation(t('b), callstack('b, 'a), int): continuation('a);
+
+  type slice('a) =
+    | SliceDone('a)
+    | SliceYielded(continuation('a));
+
+  let start = t => Continuation(t, Finished, 0);
+
+  let rec run_slice:
+    type a b.
+      (
+        ~step_budget: int,
+        ~step_counter: int,
+        ~slice_counter: int,
+        t(b),
+        callstack(b, a)
+      ) =>
+      slice(a) =
+    (
+      ~step_budget,
+      ~step_counter,
+      ~slice_counter,
+      t: t(b),
+      callstack: callstack(b, a),
+    ) =>
+      if (slice_counter >= step_budget) {
+        SliceYielded(Continuation(t, callstack, step_counter));
+      } else {
+        switch (t) {
+        | Bind(t, f) =>
+          run_slice(
+            ~step_budget,
+            ~step_counter=step_counter + 1,
+            ~slice_counter=slice_counter + 1,
+            t,
+            Continue(f, callstack),
+          )
+        | Next(f) =>
+          run_slice(
+            ~step_budget,
+            ~step_counter=step_counter + 1,
+            ~slice_counter=slice_counter + 1,
+            f(),
+            callstack,
+          )
+        | Done(x) =>
+          switch (callstack) {
+          | Finished => SliceDone(x)
+          | Continue(f, callstack) =>
+            run_slice(
+              ~step_budget,
+              ~step_counter=step_counter + 1,
+              ~slice_counter=slice_counter + 1,
+              f(x),
+              callstack,
+            )
+          }
+        };
+      };
+
+  let run_slice = (~step_budget, continuation) => {
+    let Continuation(t, callstack, step_counter) = continuation;
+    run_slice(~step_budget, ~step_counter, ~slice_counter=0, t, callstack);
+  };
+};
```

</details>

<details open>
<summary><code>src/language/dynamics/Evaluator.re</code> · [@deriving (show({with_path: false}), eq)]</summary>

<!-- changetour:hunk file=src/language/dynamics/Evaluator.re level=2 baseBlob=219e290f9ef885e8ecc74979610e7d30ff6ce8f2 -->

```diff
@@ -1,69 +1,5 @@
 open Transition;
-
-[@deriving (show({with_path: false}), eq)]
-type step_constrained('a) =
-  | StepLimitExceeded
-  | Completed('a);
-
-// This module defines the stack machine for the evaluator.
-module Trampoline = {
-  type t('a) =
-    | Bind(t('b), 'b => t('a)): t('a)
-    | Next(unit => t('a)): t('a)
-    | Done('a): t('a);
-
-  type callstack('a, 'b) =
-    | Finished: callstack('a, 'a)
-    | Continue('a => t('b), callstack('b, 'c)): callstack('a, 'c);
-  let rec run:
-    type a b.
-      (~step_limit: int=?, ~step_counter: int=?, t(b), callstack(b, a)) =>
-      step_constrained(a) =
-    (
-      ~step_limit: option(int)=?,
-      ~step_counter=0,
-      t: t(b),
-      callstack: callstack(b, a),
-    ) => {
-      switch (step_limit) {
-      | Some(x) when x <= step_counter => StepLimitExceeded
-      | _ =>
-        switch (t) {
-        | Bind(t, f) =>
-          run(
-            ~step_limit?,
-            ~step_counter=step_counter + 1,
-            t,
-            Continue(f, callstack),
-          )
-        | Next(f) =>
-          run(~step_limit?, ~step_counter=step_counter + 1, f(), callstack)
-        | Done(x) =>
-          switch (callstack) {
-          | Finished => Completed(x)
-          | Continue(f, callstack) =>
-            run(
-              ~step_limit?,
-              ~step_counter=step_counter + 1,
-              f(x),
-              callstack,
-            )
-          }
-        }
-      };
-    };
-
-  let run = (~step_limit: option(int)=?, t) =>
-    run(~step_limit?, t, Finished);
-
-  let return = x => Done(x);
-
-  let bind = (t, f) => Bind(t, f);
-
-  module Syntax = {
-    let (let.trampoline) = (x, f) => bind(x, f);
-  };
-};
+open Trampoline.Syntax;
 
 module EvaluatorEVMode: {
   type status =
```

</details>

## Write-only evaluator state

EvaluatorState is now appendable. Comment at the top of EvaluatorState.re explains what it now does. 

We had to move some things out of the state - notably read-only information, and call stack.

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · /* Argument values for function applications, keyed by app_…</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 highlights=new:5-23 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -1,28 +1,38 @@
 open Util;
 
-/* Argument values for function applications, keyed by app_id.
- * Each entry is a list of (call_stack_before_entering, elided_arg_value).
- * The call_stack is the stack BEFORE entering the function, so we can match
- * samples taken inside the function with their calling arguments. */
-[@deriving (show({with_path: false}), sexp, yojson)]
-type app_args_t =
-  Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value)));
+/*
+   _____            _             _               ____  _        _
+  | ____|_   ____ _| |_   _  __ _| |_ ___  _ __  / ___|| |_ __ _| |_ ___
+  |  _| \ \ / / _` | | | | |/ _` | __/ _ \| '__| \___ \| __/ _` | __/ _ \
+  | |___ \ V / (_| | | |_| | (_| | || (_) | |     ___) | || (_| | ||  __/
+  |_____| \_/ \__,_|_|\__,_|\__,_|\__\___/|_|    |____/ \__\__,_|\__\___|
+
+ Hazel is a PURE LANGUAGE, there is NO STATE, NOTHING TO SEE HERE, PLEASE MOVE ALONG.
+
+ Ok so we have some state but it is all WRITE-ONLY** during evaluation, so it's
+ essentially just a log we can use to query what happened during evaluation.
+
+ ** Technically actually is't not write-only, we do read from it, but ONLY to get
+ the current step count in order to record information in this state, not to affect
+ evaluation in any way. You'll notice that this step count thing is what requires
+ most of the work in appending states.
+ */
 
 [@deriving (show({with_path: false}), sexp, yojson)]
 type t = {
+  initial_step_count: int,
   theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
   tests: TestMap.t,
   probes: Sample.Map.t,
-  app_args: app_args_t, /* Argument values for function applications */
   step_count: int,
-  pending_probe_starts: Id.Map.t(list(int)), /* Stack per probe_id; nested recursive calls push/pop */
-  targets: Sample.targets, /* IDs of expressions/patterns to sample */
-  incr_eval: IncrEval.t /* Per-id cache entries and reuse/recalc bookkeeping for the incremental evaluator */
-};
+  incr_eval,
+}
+
+// Note[Matt]: There are probably memory improvements to be made here by untying this knot.
+and incr_eval = IncrEval.t(t);
 
 type effect =
   | RecordTest(TestMap.instance_report)
-  | RecordExpProbe(Sample.capture_spec)
   | RecordStackFrame(option(string), option(DHExp.t), option(Id.t)) /* (fn_name, arg_value, fn_def_id) */
   /* A pattern was matched against a value during evaluation. Carries the
    * pat and rhs so the incremental evaluator can decide which body-scoped
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · let mk = (~targets: Sample.targets): t =&gt; {</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -35,52 +45,69 @@ type effect =
   | RecordTheorem(Id.t, string, Environment.t(Exp.t), Exp.t)
   | RecordPrint(DHExp.t); /* Println for probes study */
 
-let mk = (~targets: Sample.targets): t => {
+let empty: t = {
+  initial_step_count: 0,
   tests: TestMap.empty,
   probes: Sample.Map.empty,
-  app_args: Id.Map.empty,
   step_count: 0,
-  pending_probe_starts: Id.Map.empty,
-  targets,
   theorems: [],
   incr_eval: IncrEval.empty,
 };
 
-let init: t = mk(~targets=Sample.no_targets);
+let empty_at = (step_count: int): t => {
+  ...empty,
+  initial_step_count: step_count,
+  step_count,
+};
 
 let get_step_count = ({step_count, _}: t): int => step_count;
 
-let record_probe_start = (state: t, probe_id: Id.t): t => {
-  let stack =
-    Id.Map.find_opt(probe_id, state.pending_probe_starts)
-    |> Option.value(~default=[]);
-  {
-    ...state,
-    pending_probe_starts:
-      Id.Map.add(
-        probe_id,
-        [state.step_count, ...stack],
-        state.pending_probe_starts,
-      ),
-  };
+let shift_sample = (delta: int, s: Sample.t): Sample.t => {
+  ...s,
+  step_start: s.step_start + delta,
+  step_end: s.step_end + delta,
 };
 
-let get_probe_start = (state: t, probe_id: Id.t): option(int) =>
-  switch (Id.Map.find_opt(probe_id, state.pending_probe_starts)) {
-  | Some([head, ..._]) => Some(head)
-  | _ => None
-  };
-
-let clear_probe_start = (state: t, probe_id: Id.t): t => {
-  let pending =
-    switch (Id.Map.find_opt(probe_id, state.pending_probe_starts)) {
-    | Some([_, ...rest]) when rest != [] =>
-      Id.Map.add(probe_id, rest, state.pending_probe_starts)
-    | _ => Id.Map.remove(probe_id, state.pending_probe_starts)
-    };
+/* Merge `ext` into `base`, shifting probe step bounds when the timelines
+ * don't line up (base.step_count vs ext.initial_step_count). */
+let append = (base: t, ext: t): t => {
+  let delta = base.step_count - ext.initial_step_count;
+  let probes =
+    Id.Map.fold(
+      (id, ext_samples, acc) => {
+        let samples =
+          if (delta == 0) {
+            ext_samples;
+          } else {
+            List.map(shift_sample(delta), ext_samples);
+          };
+        let existing =
+          switch (Id.Map.find_opt(id, acc)) {
+          | Some(l) => l
+          | None => []
+          };
+        Id.Map.add(id, samples @ existing, acc);
+      },
+      ext.probes,
+      base.probes,
+    );
+  let tests =
+    List.fold_left(
+      (acc, (id, reports)) =>
+        List.fold_left(
+          (acc, report) => TestMap.extend((id, report), acc),
+          acc,
+          reports,
+        ),
+      base.tests,
+      ext.tests,
+    );
   {
-    ...state,
-    pending_probe_starts: pending,
+    ...base,
+    step_count: base.step_count + (ext.step_count - ext.initial_step_count),
+    probes,
+    tests,
+    theorems: base.theorems @ ext.theorems,
   };
 };
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · let get_app_args = ({app_args, _}) =&gt; app_args;</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -90,87 +117,13 @@ let get_probes = ({probes, _}) => probes;
 
 let get_theorems = ({theorems, _}) => theorems;
 
-let get_app_args = ({app_args, _}) => app_args;
-
 let get_incr_eval = ({incr_eval, _}: t) => incr_eval;
 
-let add_incr_entry = (state: t, id: Id.t, entry: IncrEval.entry): t => {
+let add_incr_entry = (state: t, id: Id.t, entry: IncrEval.entry(t)): t => {
   ...state,
   incr_eval: IncrEval.add_entry(id, entry, state.incr_eval),
 };
 
-let mark_incr_reused = (state: t, id: Id.t): t => {
-  ...state,
-  incr_eval: IncrEval.mark_reused(id, state.incr_eval),
-};
-
-let mark_incr_recalculated = (state: t, id: Id.t): t => {
-  ...state,
-  incr_eval: IncrEval.mark_recalculated(id, state.incr_eval),
-};
-
-/* Clear transient data that's only needed during evaluation.
- * Call this before sending EvaluatorState over postMessage
- * to avoid serializing massive amounts of unnecessary data.
- * - app_args: only needed to look up args during sample creation
- * - pending_probe_starts: only needed during evaluation
- * - targets: only needed during evaluation */
-let clear_transient = (state: t): t => {
-  ...state,
-  app_args: Id.Map.empty,
-  pending_probe_starts: Id.Map.empty,
-  targets: Id.Map.empty,
-};
-
-/* Elide arg value for storage (handles closures, etc.) */
-let elide_arg =
-    (env: Environment.t(Exp.t), d: DHExp.t): Sample.Env.elided_value =>
-  Sample.Env.elide(env, d);
-
-/* Add an argument value for an application */
-let add_app_arg =
-    (
-      state: t,
-      app_id: Id.t,
-      call_stack: Sample.call_stack,
-      arg: Sample.Env.elided_value,
-    )
-    : t => {
-  let existing =
-    Id.Map.find_opt(app_id, state.app_args) |> Option.value(~default=[]);
-  {
-    ...state,
-    app_args:
-      Id.Map.add(app_id, [(call_stack, arg), ...existing], state.app_args),
-  };
-};
-
-/* Look up argument value for an application at a specific call_stack.
- * Used when creating samples for probes on Ap expressions. */
-let lookup_app_arg =
-    (state: t, app_id: Id.t, call_stack: Sample.call_stack)
-    : option(Sample.Env.elided_value) => {
-  let call_stack_ids = Sample.ids_of_stack(call_stack);
-  switch (Id.Map.find_opt(app_id, state.app_args)) {
-  | None => None
-  | Some(entries) =>
-    List.find_map(
-      ((stored_stack, arg)) =>
-        Sample.ids_of_stack(stored_stack) == call_stack_ids
-          ? Some(arg) : None,
-      entries,
-    )
-  };
-};
-
-let add_test = (state: t, instance_report: TestMap.instance_report) => {
-  ...state,
-  tests:
-    TestMap.extend(
-      (DHExp.rep_id(instance_report.exp), instance_report),
-      state.tests,
-    ),
-};
 let add_sample = (state: t, sample: Sample.t) => {
   /* Deduplicate: skip recording if an existing sample for this
    * syntax_id makes the new one redundant.
```

</details>

`update` is where the per-step bookkeeping happens. It now receives the `info_map` (to know which app_ids are probe targets) and threads a `CallStack.t'` instead of a bare list — building call context through `CallStack` rather than mutating state — and the now-redundant `add_app_arg`/`lookup_app_arg`/`RecordExpProbe`/`capture_slice`/`replay_slice` helpers are deleted.

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · let add_theorem = ({theorems, _} as es, id, name, env, goal…</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -205,82 +158,74 @@ let add_sample = (state: t, sample: Sample.t) => {
   };
 };
 
-let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
-  {
-    ...es,
-    theorems: theorems |> List.append([(id, name, env, goal)]),
-  };
-};
-
 let update =
     (
+      info_map: EvalInfo.t,
       state: t,
-      call_stack: Sample.call_stack,
+      call_stack: CallStack.t',
       env: Environment.t(Exp.t),
       init: DHExp.t,
-      next: DHExp.t,
       side_effects: list(effect),
     )
-    : (Sample.call_stack, t) => {
+    : (CallStack.t', t) => {
+  /* Elide arg value for storage (handles closures, etc.) */
+  let elide_arg =
+      (env: Environment.t(Exp.t), d: DHExp.t): Sample.Env.elided_value =>
+    Sample.Env.elide(env, d);
+
+  let add_test = (state: t, instance_report: TestMap.instance_report) => {
+    ...state,
+    tests:
+      TestMap.extend(
+        (DHExp.rep_id(instance_report.exp), instance_report),
+        state.tests,
+      ),
+  };
+
+  let add_theorem = ({theorems, _} as es, id, name, env, goal) => {
+    {
+      ...es,
+      theorems: theorems |> List.append([(id, name, env, goal)]),
+    };
+  };
+
   /* Increment step count for this evaluation step */
   let state = {
     ...state,
     step_count: state.step_count + 1,
   };
 
   List.fold_left(
-    ((call_stack: Sample.call_stack, state: t), effect: effect) =>
+    ((call_stack: CallStack.t', state: t), effect: effect) =>
       switch (effect) {
       | RecordStackFrame(fn_name, arg_opt, fn_def_id) =>
         let app_id = DHExp.rep_id(init);
         /* Only store argument value if this app_id is a probe target.
          * This avoids accumulating massive app_args data for programs
          * with many function calls but no probes on those calls. */
-        let state =
+        let call_stack =
           switch (arg_opt) {
-          | Some(arg) when Id.Map.mem(app_id, state.targets) =>
+          | Some(arg) when Id.Map.mem(app_id, info_map.targets) =>
             let elided_arg = elide_arg(env, arg);
-            add_app_arg(state, app_id, call_stack, elided_arg);
+            CallStack.add_app_arg(call_stack, app_id, elided_arg);
           | Some(_)
-          | None => state
+          | None => call_stack
           };
         (
-          [
+          CallStack.add_entry(
+            call_stack,
             {
               id: app_id,
               name: fn_name,
               fn_def_id,
             },
-            ...call_stack,
-          ],
+          ),
           state,
         );
       | RecordTest(instance_report) => (
           call_stack,
           add_test(state, instance_report),
         )
-      | RecordExpProbe(pr) =>
-        let probe_id = DHExp.rep_id(init);
-        /* step_start is when we began evaluating the probe (recorded earlier)
-         * step_end is step_count - 1 because this step is the "strip probe" step */
-        let step_start =
-          get_probe_start(state, probe_id) |> Option.value(~default=0);
-        let step_end = state.step_count - 1;
-        /* Look up arg if this probe is on an Ap expression */
-        let args = lookup_app_arg(state, probe_id, call_stack);
-        let sample =
-          Sample.mk(
-            ~args,
-            ~step_start,
-            ~step_end,
-            probe_id,
-            next,
-            env,
-            call_stack,
-            pr,
-          );
-        let state = clear_probe_start(state, probe_id);
-        (call_stack, add_sample(state, sample));
       | RecordPatMatch({samples: sample_closures, _}) =>
         /* Pattern probes are recorded at the current step, then we
          * increment to ensure patterns don't share step boundaries
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · (</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -289,11 +234,11 @@ let update =
         let step = state.step_count;
         let state =
           List.fold_left(
-            (
-              state: t,
-              sample_closure: (Sample.call_stack, int, int) => Sample.t,
-            ) =>
-              add_sample(state, sample_closure(call_stack, step, step)),
+            (state: t, sample_closure: (CallStack.t, int, int) => Sample.t) =>
+              add_sample(
+                state,
+                sample_closure(call_stack.stack, step, step),
+              ),
             state,
             sample_closures,
           );
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · call_stack,</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -314,7 +259,7 @@ let update =
             DHExp.rep_id(init),
             value,
             env,
-            call_stack,
+            call_stack.stack,
             Sample.empty_capture_spec,
           );
         (call_stack, add_sample(state, sample));
```

</details>

<details open>
<summary><code>src/language/dynamics/state/EvaluatorState.re</code> · /* Capture the delta between `before` and `after` as a Stat…</summary>

<!-- changetour:hunk file=src/language/dynamics/state/EvaluatorState.re level=2 baseBlob=6bc83d879d601bde7cf8c1cb253c020c63266d2f -->

```diff
@@ -327,69 +272,3 @@ let update =
     side_effects,
   );
 };
-
-/* Capture the delta between `before` and `after` as a StateSlice. */
-let capture_slice = (~before: t, ~after: t): StateSlice.t => {
-  origin: before.step_count,
-  steps: after.step_count - before.step_count,
-  probes: StateSlice.diff_probes(~before=before.probes, ~after=after.probes),
-  tests: StateSlice.diff_tests(~before=before.tests, ~after=after.tests),
-  theorems:
-    StateSlice.diff_theorems(~before=before.theorems, ~after=after.theorems),
-  app_args:
-    StateSlice.diff_app_args(~before=before.app_args, ~after=after.app_args),
-};
-
-/* Replay a slice into `state`: add its sample/test/theorem/app_arg entries,
- * bump step_count by the slice's step delta. Probe step bounds are shifted
- * so they sit within the current step_count window. */
-let replay_slice = (slice: StateSlice.t, state: t): t => {
-  let delta = state.step_count - slice.origin;
-  let probes =
-    Id.Map.fold(
-      (id, new_samples, acc) => {
-        let shifted = List.map(StateSlice.shift_sample(delta), new_samples);
-        let existing =
-          switch (Id.Map.find_opt(id, acc)) {
-          | Some(l) => l
-          | None => []
-          };
-        Id.Map.add(id, shifted @ existing, acc);
-      },
-      slice.probes,
-      state.probes,
-    );
-  let tests =
-    List.fold_left(
-      (acc, (id, new_reports)) =>
-        List.fold_left(
-          (acc, report) => TestMap.extend((id, report), acc),
-          acc,
-          new_reports,
-        ),
-      state.tests,
-      slice.tests,
-    );
-  let theorems = state.theorems @ slice.theorems;
-  let app_args =
-    Id.Map.fold(
-      (id, new_entries, acc) => {
-        let existing =
-          switch (Id.Map.find_opt(id, acc)) {
-          | Some(l) => l
-          | None => []
-          };
-        Id.Map.add(id, new_entries @ existing, acc);
-      },
-      slice.app_args,
-      state.app_args,
-    );
-  {
-    ...state,
-    step_count: state.step_count + slice.steps,
-    probes,
-    tests,
-    theorems,
-    app_args,
-  };
-};
```

</details>

## The call stack moves out of Sample

This separates information relevant to the call stack, from write-only evaluator state logs.

<details open>
<summary><code>src/language/dynamics/CallStack.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/language/dynamics/CallStack.re level=2 baseBlob=e5eb32afc9f7d3159bc311689158e6d587a54ddd -->

```diff
@@ -0,0 +1,85 @@
+open Util;
+
+// This module defines the call stack representation used to record probe samples.
+
+/* A single frame in the call stack: app_id + optional function_name.
+ * function_name is extracted at evaluation time from the closure/function.
+ * fn_def_id is the definition-site ID of the function, extracted from the
+ * Closure at evaluation time. Enables jump-to-definition even when app_id
+ * comes from built-in internal code (not in user's info_map).
+ * The name and fn_def_id fields are purely informational; equality compares only id. */
+[@deriving (show({with_path: false}), sexp, yojson)]
+type frame = {
+  id: Id.t,
+  name: option(string),
+  fn_def_id: option(Id.t),
+};
+
+let equal_frame = (a: frame, b: frame): bool => a.id == b.id;
+
+/* Call context represented as a list of stack frames.
+ * The head is the most recent (innermost) call. */
+[@deriving (show({with_path: false}), sexp, yojson)]
+type t = list(frame);
+
+let equal = (a: t, b: t): bool => List.equal(equal_frame, a, b);
+
+/* Extract just the IDs from a call stack, discarding function names. */
+let ids_of_stack = (cs: t): list(Id.t) => List.map((f: frame) => f.id, cs);
+
+// This should really be defined in Sample.re
+[@deriving (show({with_path: false}), sexp, yojson, eq)]
+type elided_value =
+  | Opaque
+  | Val(DHExp.t);
+
+/* Argument values for function applications, keyed by app_id.
+ * Each entry is a list of (call_stack_before_entering, elided_arg_value).
+ * The call_stack is the stack BEFORE entering the function, so we can match
+ * samples taken inside the function with their calling arguments. */
+[@deriving (show({with_path: false}), sexp, yojson)]
+type app_args = Id.Map.t(list((t, elided_value)));
+
+type t' = {
+  stack: t,
+  app_args /* Argument values for function applications */,
+};
+
+/* Add an argument value for an application */
+let add_app_arg = (state: t', app_id: Id.t, arg: elided_value): t' => {
+  let existing =
+    Id.Map.find_opt(app_id, state.app_args) |> Option.value(~default=[]);
+  {
+    ...state,
+    app_args:
+      Id.Map.add(app_id, [(state.stack, arg), ...existing], state.app_args),
+  };
+};
+
+/* Look up argument value for an application at a specific call_stack.
+ * Used when creating samples for probes on Ap expressions. */
+let lookup_app_arg =
+    (state: t', app_id: Id.t, call_stack: t): option(elided_value) => {
+  let call_stack_ids = ids_of_stack(call_stack);
+  switch (Id.Map.find_opt(app_id, state.app_args)) {
+  | None => None
+  | Some(entries) =>
+    List.find_map(
+      ((stored_stack, arg)) =>
+        ids_of_stack(stored_stack) == call_stack_ids ? Some(arg) : None,
+      entries,
+    )
+  };
+};
+
+let add_entry = (state: t', frame: frame): t' => {
+  {
+    ...state,
+    stack: [frame, ...state.stack],
+  };
+};
+
+let empty = {
+  stack: [],
+  app_args: Id.Map.empty,
+};
```

</details>

<details open>
<summary><code>src/language/dynamics/Sample.re</code> · /* A single frame in the call stack: app_id + optional func…</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -7,34 +7,6 @@ type capture_spec = {refs: Binding.s};
 
 let empty_capture_spec: capture_spec = {refs: []};
 
-/* A single frame in the call stack: app_id + optional function_name.
- * function_name is extracted at evaluation time from the closure/function.
- * fn_def_id is the definition-site ID of the function, extracted from the
- * Closure at evaluation time. Enables jump-to-definition even when app_id
- * comes from built-in internal code (not in user's info_map).
- * The name and fn_def_id fields are purely informational; equality compares only id. */
-[@deriving (show({with_path: false}), sexp, yojson)]
-type stack_frame = {
-  id: Id.t,
-  name: option(string),
-  fn_def_id: option(Id.t),
-};
-
-let equal_stack_frame = (a: stack_frame, b: stack_frame): bool =>
-  a.id == b.id;
-
-/* Call context represented as a list of stack frames.
- * The head is the most recent (innermost) call. */
-[@deriving (show({with_path: false}), sexp, yojson)]
-type call_stack = list(stack_frame);
-
-let equal_call_stack = (a: call_stack, b: call_stack): bool =>
-  List.equal(equal_stack_frame, a, b);
-
-/* Extract just the IDs from a call stack, discarding function names. */
-let ids_of_stack = (cs: call_stack): list(Id.t) =>
-  List.map((f: stack_frame) => f.id, cs);
-
 /* Maps expression/pattern IDs to their capture specifications.
  * Presence in this map means "collect a sample when evaluated". */
 type targets = Id.Map.t(capture_spec);
```

</details>

## The incremental cache becomes a stream

The incremental evaluator's cache (`IncrEval`) changes two ways. First, its `entry` is now parametric over the state it carries (`entry('state)`). A cache entry needs to hold an `EvaluatorState.t`, but `EvaluatorState` already depends on `IncrEval` — so making the state abstract lets `IncrEval` stay free of that dependency, and `EvaluatorState` ties the recursive knot itself with `and incr_eval = IncrEval.t(t)` (see Matt's "untying this knot" note there). In practice `'state` is always `EvaluatorState.t`. Second, alongside the completed-entries map there's now an `outbox` — `completed` entries plus an optional `current` partial state — which is the unit the worker streams to the UI. 

The old `reused`/`recalculated` id lists are dropped entirely; the UI now derives "what was frozen" from the reuse plan instead.

<details open>
<summary><code>src/language/dynamics/IncrEval.re</code> · type entry = {</summary>

<!-- changetour:hunk file=src/language/dynamics/IncrEval.re level=2 highlights=new:34-90 baseBlob=80d3405cc127b1debd54ed4a4603cb4919f2e8ab -->

```diff
@@ -31,68 +31,115 @@ type provenance = {
 type reuse_map = VarMap.t_(provenance);
 
 [@deriving (show({with_path: false}), sexp, yojson)]
-type entry = {
+type entry('state) = {
   prev_elab: Exp.t,
   prev_reuse_map: reuse_map,
   prev_probe_targets: option(SubexpProbeTargets.t),
   value: DHExp.t,
-  state: StateSlice.t,
+  state: 'state,
 };
 
 [@deriving (show({with_path: false}), sexp, yojson)]
-type t = {
-  entries: Id.Map.t(entry),
-  /* Ids evaluated from scratch on this run (cache miss). UI tint. */
-  recalculated: list(Id.t),
-  /* Ids short-circuited via reuse_check (cache hit). Not the complement of
-   * `recalculated`: a recalculated parent can still contain reused children. */
-  reused: list(Id.t),
+type t('state) = {entries: Id.Map.t(entry('state))};
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type current('state) = {
+  id: Id.t,
+  state: 'state,
+};
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type outbox('state) = {
+  completed: t('state),
+  current: option(current('state)),
+};
+
+let empty: t('state) = {entries: Id.Map.empty};
+
+let empty_outbox: outbox('state) = {
+  completed: empty,
+  current: None,
 };
 
-let empty: t = {
-  entries: Id.Map.empty,
-  recalculated: [],
-  reused: [],
+let outbox_of_completed = (completed: t('state)): outbox('state) => {
+  completed,
+  current: None,
 };
 
-let is_empty = (incr: t): bool =>
-  Id.Map.is_empty(incr.entries)
-  && incr.recalculated == []
-  && incr.reused == [];
+let is_empty = (incr: t('state)): bool => Id.Map.is_empty(incr.entries);
+
+let outbox_is_empty = (outbox: outbox('state)): bool =>
+  is_empty(outbox.completed) && Option.is_none(outbox.current);
 
-let add_entry = (id: Id.t, entry: entry, incr: t): t => {
-  ...incr,
+let add_entry =
+    (id: Id.t, entry: entry('state), incr: t('state)): t('state) => {
   entries: Id.Map.add(id, entry, incr.entries),
 };
 
-let mark_recalculated = (id: Id.t, incr: t): t => {
-  ...incr,
-  recalculated: [id, ...incr.recalculated],
+let add_outbox_entry =
+    (id: Id.t, entry: entry('state), outbox: outbox('state))
+    : outbox('state) => {
+  ...outbox,
+  completed: add_entry(id, entry, outbox.completed),
 };
 
-let mark_reused = (id: Id.t, incr: t): t => {
-  ...incr,
-  reused: [id, ...incr.reused],
+let set_outbox_current =
+    (~id: Id.t, ~state: 'state, outbox: outbox('state)): outbox('state) => {
+  ...outbox,
+  current:
+    Some({
+      id,
+      state,
+    }),
+};
+
+let add_stream = (stream: t('state), incr: t('state)): t('state) => {
+  entries:
+    Id.Map.union(
+      (_, _old, new_) => Some(new_),
+      incr.entries,
+      stream.entries,
+    ),
+};
+
+let merge_outbox =
+    (stream: outbox('state), outbox: outbox('state)): outbox('state) => {
+  completed: add_stream(stream.completed, outbox.completed),
+  current: stream.current,
+};
+
+let copy_descendant_entries =
+    (~root_id: Id.t, ~root: Exp.t, ~prev: t('state), incr: t('state))
+    : t('state) => {
+  let acc = ref(incr);
+  let f_exp = (continue, e: Exp.t): Exp.t => {
+    let sub_id = Exp.rep_id(e);
+    if (!Id.equal(sub_id, root_id)) {
+      switch (Id.Map.find_opt(sub_id, prev.entries)) {
+      | Some(sub_entry) => acc := add_entry(sub_id, sub_entry, acc^)
+      | None => ()
+      };
+    };
+    continue(e);
+  };
+  let _ = TermBase.Exp.map_term(~f_exp, root);
+  acc^;
 };
 
 /* The set of ids the UI should paint as "frozen" this run.*/
-let frozen_ids = (incr: t): list(Id.t) => {
+let frozen_ids = (~ack_incr: t('state)): list(Id.t) => {
   let acc = ref([]);
   let collect_subtree = (root: Exp.t): unit => {
-    let f_exp = (continue, e: Exp.t) => {
+    let f_exp = (continue, e: Exp.t): Exp.t => {
       acc := [Exp.rep_id(e), ...acc^];
       continue(e);
     };
     let _ = TermBase.Exp.map_term(~f_exp, root);
     ();
   };
-  List.iter(
-    id =>
-      switch (Id.Map.find_opt(id, incr.entries)) {
-      | Some(entry) => collect_subtree(entry.prev_elab)
-      | None => acc := [id, ...acc^]
-      },
-    incr.reused,
+  Id.Map.iter(
+    (_, entry) => collect_subtree(entry.prev_elab),
+    ack_incr.entries,
   );
   acc^;
 };
```

</details>

<details open>
<summary><code>src/language/dynamics/IncrEval.re</code> · let was_reused = (id: Id.t, incr: t): bool =&gt; List.mem(id,…</summary>

<!-- changetour:hunk file=src/language/dynamics/IncrEval.re level=2 baseBlob=80d3405cc127b1debd54ed4a4603cb4919f2e8ab -->

```diff
@@ -229,8 +288,6 @@ let with_pat_provenance =
   pat_provenance(~source_id, ~flag, pat)
   @ remove_pat_bindings(pat, reuse_map);
 
-let was_reused = (id: Id.t, incr: t): bool => List.mem(id, incr.reused);
-
 let update_maps_after_binding =
     (~rhs_reused: bool, ~source_id: Id.t, pat: Pat.t, ~reuse_map: reuse_map)
     : reuse_map => {
```

</details>

<details open>
<summary><code>src/language/dynamics/IncrEval.re</code> · ~call_stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=src/language/dynamics/IncrEval.re level=2 baseBlob=80d3405cc127b1debd54ed4a4603cb4919f2e8ab -->

```diff
@@ -240,18 +297,18 @@ let update_maps_after_binding =
 
 let reuse_check =
     (
-      ~call_stack: Sample.call_stack,
-      ~prev: t,
+      ~call_stack: CallStack.t',
+      ~prev: t('state),
       ~reuse_map: reuse_map,
-      ~info_map: EvalInfoMap.t,
+      ~info_map: EvalInfo.t,
       ~id: Id.t,
     )
-    : option(entry) => {
+    : option(entry('state)) => {
   open OptUtil.Syntax;
 
-  let* () = OptUtil.some_if(call_stack == [] && !is_empty(prev), ());
+  let* () = OptUtil.some_if(call_stack.stack == [] && !is_empty(prev), ());
   let* entry = Id.Map.find_opt(id, prev.entries);
-  let* info = EvalInfoMap.find_opt(id, info_map);
+  let* info = EvalInfo.find_opt(id, info_map);
 
   let elab_same = Exp.fast_equal(entry.prev_elab, info.elab_term);
   let* () = OptUtil.some_if(elab_same, ());
```

</details>

A small correctness fix rides along: the `$hole` sentinel (a statics-only marker for unused-variable warnings) is not a runtime dependency, so it's excluded from reuse provenance — otherwise it would spuriously invalidate cache entries.

<details open>
<summary><code>src/language/dynamics/IncrEval.re</code> · /* `$hole` is a statics-only sentinel for unused-variable w…</summary>

<!-- changetour:hunk file=src/language/dynamics/IncrEval.re level=2 baseBlob=80d3405cc127b1debd54ed4a4603cb4919f2e8ab -->

```diff
@@ -124,12 +171,20 @@ let equal_reuse_map = (a: reuse_map, b: reuse_map): bool =>
        a,
      );
 
+/* `$hole` is a statics-only sentinel for unused-variable warnings. It is not
+ * a runtime dependency, so it should not participate in reuse provenance. */
+let is_runtime_dependency = (name: string): bool => name != "$hole";
+
 let restrict_to_co_ctx = (reuse_map: reuse_map, co_ctx: CoCtx.t): reuse_map =>
   List.fold_right(
     ((name, _), projected) =>
-      switch (VarMap.lookup(reuse_map, name)) {
-      | Some(prov) => [(name, prov), ...projected]
-      | None => projected
+      if (!is_runtime_dependency(name)) {
+        projected;
+      } else {
+        switch (VarMap.lookup(reuse_map, name)) {
+        | Some(prov) => [(name, prov), ...projected]
+        | None => projected
+        };
       },
     VarMap.to_list(co_ctx),
     [],
```

</details>

<details open>
<summary><code>src/language/dynamics/IncrEval.re</code> · switch (acc) {</summary>

<!-- changetour:hunk file=src/language/dynamics/IncrEval.re level=2 baseBlob=80d3405cc127b1debd54ed4a4603cb4919f2e8ab -->

```diff
@@ -139,13 +194,17 @@ let reuse_map_for_co_ctx =
     (reuse_map: reuse_map, co_ctx: CoCtx.t): option(reuse_map) =>
   List.fold_right(
     ((name, _), acc) =>
-      switch (acc) {
-      | None => None
-      | Some(projected) =>
-        switch (VarMap.lookup(reuse_map, name)) {
-        | Some(prov) => Some([(name, prov), ...projected])
+      if (!is_runtime_dependency(name)) {
+        acc;
+      } else {
+        switch (acc) {
         | None => None
-        }
+        | Some(projected) =>
+          switch (VarMap.lookup(reuse_map, name)) {
+          | Some(prov) => Some([(name, prov), ...projected])
+          | None => None
+          }
+        };
       },
     VarMap.to_list(co_ctx),
     Some([]),
```

</details>

`EvalInfoMap` is renamed to `EvalInfo` and absorbs the probe `targets` — so the single projected-statics value shipped to the worker now carries everything evaluation needs.

<details open>
<summary><code>src/language/dynamics/EvalInfo.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/language/dynamics/EvalInfo.re level=2 baseBlob=069429a611043b34487af2f7aa5962ddb4887209 -->

```diff
@@ -0,0 +1,43 @@
+open Util;
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type entry = {
+  elab_term: Exp.t,
+  co_ctx: CoCtx.t,
+  /* See `prev_probe_targets` in IncrEval — None under `probe_all`. */
+  probe_targets: option(SubexpProbeTargets.t),
+};
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type t = {
+  statics: Id.Map.t(entry),
+  targets: Sample.targets /* IDs of expressions/patterns to sample */
+};
+
+let empty: t = {
+  statics: Id.Map.empty,
+  targets: Sample.no_targets,
+};
+
+let find_opt = (id: Id.t, map: t): option(entry) =>
+  Id.Map.find_opt(id, map.statics);
+
+let of_info_map =
+    (~probe_all: bool, ~targets: Sample.targets, info_map: StaticsBase.Map.t)
+    : t => {
+  statics:
+    Id.Map.filter_map(
+      (_id, info) =>
+        switch (info) {
+        | Info.InfoExp({elab_term, co_ctx, probe_targets, _}) =>
+          Some({
+            elab_term,
+            co_ctx,
+            probe_targets: probe_all ? None : Some(probe_targets),
+          })
+        | _ => None
+        },
+      info_map,
+    ),
+  targets,
+};
```

</details>

## Predicting reuse, and rebuilding state from a stream

Two new passes bracket streaming evaluation. `ReusePass` walks the elaboration *before* evaluation and predicts which entries are reusable — this is what the worker can send back instantly as an ACK so the UI can tint frozen regions before a single step runs. `StreamCollector` does the inverse: given a streamed outbox, it reconstructs a full `EvaluatorState` (including the in-flight `current` slice) so the UI can render partial dynamics.

<details open>
<summary><code>src/language/dynamics/evaluation/ReusePass.re</code> · open Transition;</summary>

<!-- changetour:hunk file=src/language/dynamics/evaluation/ReusePass.re level=2 baseBlob=adcd01672d44470411f1624314754d42dee189ff -->

```diff
@@ -0,0 +1,141 @@
+open Transition;
+
+let stream_union =
+    (
+      left: IncrEval.t(EvaluatorState.t),
+      right: IncrEval.t(EvaluatorState.t),
+    )
+    : IncrEval.t(EvaluatorState.t) =>
+  IncrEval.add_stream(left, right);
+
+let reusable_entry =
+    (
+      ~prev: EvaluatorState.incr_eval,
+      ~info_map: EvalInfo.t,
+      ~reuse_map: IncrEval.reuse_map,
+      d: DHExp.t,
+    )
+    : option(IncrEval.entry(EvaluatorState.t)) =>
+  IncrEval.reuse_check(
+    ~call_stack=CallStack.empty,
+    ~prev,
+    ~reuse_map,
+    ~info_map,
+    ~id=DHExp.rep_id(d),
+  );
+
+module ReusePassEVMode: {
+  include
+    EV_MODE with
+      type inner_result = (IncrEval.t(EvaluatorState.t), rule) and
+      type result = (IncrEval.t(EvaluatorState.t), rule);
+} = {
+  type result = (IncrEval.t(EvaluatorState.t), rule);
+  type inner_result = result;
+  type requirement('a) = (IncrEval.t(EvaluatorState.t), 'a);
+  type requirements('a, 'b) = (IncrEval.t(EvaluatorState.t), 'a, 'b);
+
+  let req_final = (f, _, x) => {
+    let (stream, _) = f(x);
+    (stream, x);
+  };
+
+  let rec req_all_final = (f, i, xs) =>
+    switch (xs) {
+    | [] => (IncrEval.empty, [])
+    | [x, ...xs] =>
+      let (stream, x) = req_final(f, x => x, x);
+      let (streams, xs) = req_all_final(f, i, xs);
+      (stream_union(stream, streams), [x, ...xs]);
+    };
+
+  let otherwise = (_, c) => (IncrEval.empty, (), c);
+
+  let (and.) = ((stream1, x1, c1), (stream2, x2)) => (
+    stream_union(stream1, stream2),
+    (x1, x2),
+    c1(x2),
+  );
+
+  let (let.) = ((stream, x, _), s) => (stream, s(x));
+};
+
+module ReusePassTransition = Transition(ReusePassEVMode);
+
+let update_reuse_map_after_effects =
+    (
+      ~rhs_reused: Id.t => bool,
+      ~reuse_map: IncrEval.reuse_map,
+      effects: list(EvaluatorState.effect),
+    )
+    : IncrEval.reuse_map =>
+  List.fold_left(
+    (reuse_map, effect) =>
+      switch (effect) {
+      | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
+        let source_id = DHExp.rep_id(rhs);
+        IncrEval.update_maps_after_binding(
+          ~rhs_reused=rhs_reused(source_id),
+          ~source_id,
+          pat,
+          ~reuse_map,
+        );
+      | _ => reuse_map
+      },
+    reuse_map,
+    effects,
+  );
+
+let rec reuse_pass_for =
+        (
+          ~prev: EvaluatorState.incr_eval,
+          ~info_map: EvalInfo.t,
+          ~reuse_map: IncrEval.reuse_map,
+          d: DHExp.t,
+        )
+        : IncrEval.t(EvaluatorState.t) => {
+  let id = DHExp.rep_id(d);
+  switch (reusable_entry(~prev, ~info_map, ~reuse_map, d)) {
+  | Some(entry) => {entries: Id.Map.add(id, entry, Id.Map.empty)}
+  | None =>
+    let (req_stream, rule) =
+      ReusePassTransition.transition(
+        (~in_closure=?, _env, child) => {
+          ignore(in_closure);
+          (reuse_pass_for(~prev, ~info_map, ~reuse_map, child), Indet);
+        },
+        ~mode=`Environment,
+        ~targets=info_map.targets,
+        Builtins.env_init,
+        d,
+      );
+    switch (rule) {
+    | Step({expr, side_effects, is_value: false, _}) =>
+      let reuse_map =
+        update_reuse_map_after_effects(
+          ~rhs_reused=source_id => Id.Map.mem(source_id, req_stream.entries),
+          ~reuse_map,
+          side_effects,
+        );
+      stream_union(
+        req_stream,
+        reuse_pass_for(~prev, ~info_map, ~reuse_map, expr),
+      );
+    | Step({is_value: true, _})
+    | Constructor
+    | Value
+    | Indet => req_stream
+    };
+  };
+};
+
+let reuse_pass =
+    (
+      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
+      ~info_map: EvalInfo.t=EvalInfo.empty,
+      ~env,
+      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
+      d: DHExp.t,
+    )
+    : IncrEval.t(EvaluatorState.t) =>
+  reuse_pass_for(~prev, ~info_map, ~reuse_map, d);
```

</details>

<details open>
<summary><code>src/language/dynamics/evaluation/ReusePass.rei</code> · let update_reuse_map_after_effects:</summary>

<!-- changetour:hunk file=src/language/dynamics/evaluation/ReusePass.rei level=2 baseBlob=9fe813bca0a1b1664571cbf5b681289a3885de37 -->

```diff
@@ -0,0 +1,17 @@
+let update_reuse_map_after_effects:
+  (
+    ~rhs_reused: Id.t => bool,
+    ~reuse_map: IncrEval.reuse_map,
+    list(EvaluatorState.effect)
+  ) =>
+  IncrEval.reuse_map;
+
+let reuse_pass:
+  (
+    ~prev: EvaluatorState.incr_eval=?,
+    ~info_map: EvalInfo.t=?,
+    ~env: Environment.t(Exp.t),
+    ~reuse_map: IncrEval.reuse_map=?,
+    Exp.t
+  ) =>
+  IncrEval.t(EvaluatorState.t);
```

</details>

<details open>
<summary><code>src/language/dynamics/evaluation/StreamCollector.re</code> · open Transition;</summary>

<!-- changetour:hunk file=src/language/dynamics/evaluation/StreamCollector.re level=2 baseBlob=b0d88d3245f0f2fd46c490d9f5edddfaf7a16607 -->

```diff
@@ -0,0 +1,96 @@
+open Transition;
+
+module CollectStreamEVMode: {
+  include
+    EV_MODE with
+      type inner_result = (EvaluatorState.t, rule) and
+      type result = (EvaluatorState.t, rule);
+} = {
+  type result = (EvaluatorState.t, rule);
+  type inner_result = result;
+  type requirement('a) = (EvaluatorState.t, 'a);
+  type requirements('a, 'b) = (EvaluatorState.t, 'a, 'b);
+
+  let req_final = (f, _, x) => {
+    let (state, _) = f(x);
+    (state, x);
+  };
+
+  let rec req_all_final = (f, i, xs) =>
+    switch (xs) {
+    | [] => (EvaluatorState.empty, [])
+    | [x, ...xs] =>
+      let (state, x) = req_final(f, x => x, x);
+      let (states, xs) = req_all_final(f, i, xs);
+      (EvaluatorState.append(state, states), [x, ...xs]);
+    };
+
+  let otherwise = (_, c) => (EvaluatorState.empty, (), c);
+
+  let (and.) = ((state1, x1, c1), (state2, x2)) => (
+    EvaluatorState.append(state1, state2),
+    (x1, x2),
+    c1(x2),
+  );
+
+  let (let.) = ((state, x, _), s) => (state, s(x));
+};
+
+module CollectStreamTransition = Transition(CollectStreamEVMode);
+
+let rec collect_stream_state_for =
+        (stream: IncrEval.outbox(EvaluatorState.t), d: DHExp.t)
+        : EvaluatorState.t => {
+  let id = DHExp.rep_id(d);
+  switch (Id.Map.find_opt(id, stream.completed.entries)) {
+  | Some(entry) =>
+    let state = EvaluatorState.append(EvaluatorState.empty, entry.state);
+    let state = EvaluatorState.add_incr_entry(state, id, entry);
+    state;
+  | None =>
+    switch (stream.current) {
+    | Some({id: current_id, state}) when Id.equal(id, current_id) =>
+      EvaluatorState.append(EvaluatorState.empty, state)
+    | Some(_)
+    | None =>
+      let (req_state, rule) =
+        CollectStreamTransition.transition(
+          (~in_closure=?, _env, child) => {
+            ignore(in_closure);
+            (collect_stream_state_for(stream, child), Indet);
+          },
+          ~mode=`Environment,
+          ~targets=Sample.no_targets,
+          Builtins.env_init,
+          d,
+        );
+      switch (rule) {
+      | Step({expr, is_value: false, _}) =>
+        EvaluatorState.append(
+          req_state,
+          collect_stream_state_for(stream, expr),
+        )
+      | Step({is_value: true, _})
+      | Constructor
+      | Value
+      | Indet => req_state
+      };
+    }
+  };
+};
+
+let collect_stream_state =
+    (stream: IncrEval.outbox(EvaluatorState.t), d: DHExp.t): EvaluatorState.t => {
+  let state = collect_stream_state_for(stream, d);
+  {
+    ...state,
+    incr_eval: {
+      entries:
+        Id.Map.union(
+          (_, existing, _streamed) => Some(existing),
+          state.incr_eval.entries,
+          stream.completed.entries,
+        ),
+    },
+  };
+};
```

</details>

<details open>
<summary><code>src/language/dynamics/evaluation/StreamCollector.rei</code> · let collect_stream_state:</summary>

<!-- changetour:hunk file=src/language/dynamics/evaluation/StreamCollector.rei level=2 baseBlob=2a2b1a2268cf1888fbd4e72d74bb36928ef61e0b -->

```diff
@@ -0,0 +1,2 @@
+let collect_stream_state:
+  (IncrEval.outbox(EvaluatorState.t), DHExp.t) => EvaluatorState.t;
```

</details>

## Rebuilding `evaluate`

With the pieces in place, the core `evaluate` is restructured from one monolithic function into a stack of single-responsibility layers.

It threads an `outbox` ref and a `current_top_id` so partial results can be published mid-flight, and the public surface gains a yielding API: `start_yielding_evaluation` / `run_yielding_slice` / `drain_streaming_outbox`.

<details open>
<summary><code>src/language/dynamics/Evaluator.re</code> · type state = ref(EvaluatorState.t) and</summary>

<!-- changetour:hunk file=src/language/dynamics/Evaluator.re level=2 baseBlob=219e290f9ef885e8ecc74979610e7d30ff6ce8f2 -->

```diff
@@ -72,25 +8,22 @@ module EvaluatorEVMode: {
 
   include
     EV_MODE with
-      type state = ref(EvaluatorState.t) and
+      type inner_result = Trampoline.t(DHExp.t) and
       type result =
         Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
 } = {
-  open Trampoline.Syntax;
-
   type status =
     | Final
     | Uneval;
 
+  type inner_result = Trampoline.t(DHExp.t);
   type result =
     Trampoline.t((status, list(EvaluatorState.effect), DHExp.t));
   type requirement('a) = Trampoline.t('a);
   type requirements('a, 'b) = Trampoline.t(('a, 'b));
 
-  type state = ref(EvaluatorState.t);
-
   let req_final = (f, _, x) => {
-    let.trampoline (_, _, x) = Next(() => f(x));
+    let.trampoline x = Next(() => f(x));
     Trampoline.return(x);
   };
```

</details>

<details open>
<summary><code>src/language/dynamics/Evaluator.re</code> · // Constants</summary>

<!-- changetour:hunk file=src/language/dynamics/Evaluator.re level=2 highlights=new:60-180 baseBlob=219e290f9ef885e8ecc74979610e7d30ff6ce8f2 -->

```diff
@@ -126,280 +59,440 @@ module EvaluatorEVMode: {
 module Eval = Transition(EvaluatorEVMode);
 
 let rec evaluate =
+        // Constants
         (
-          ~reuse_map: IncrEval.reuse_map,
-          ~prev: IncrEval.t=IncrEval.empty,
-          ~info_map: EvalInfoMap.t,
+          ~prev: EvaluatorState.incr_eval=IncrEval.empty,
+          ~reused_ids: Id.Map.t(unit),
+          ~info_map: EvalInfo.t,
+          // Call Stack
           ~in_closure=?,
-          ~call_stack: Sample.call_stack,
-          state: EvaluatorEVMode.state,
+          ~call_stack: CallStack.t',
+          // Inputs
+          ~reuse_map: IncrEval.reuse_map,
           env,
-          init: DHExp.t,
+          exp: DHExp.t,
+          // Outputs
+          ~parent_state: ref(EvaluatorState.t),
+          ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t))),
+          ~current_top_id: option(Id.t),
         )
-        : EvaluatorEVMode.result => {
-  open Trampoline.Syntax;
+        : Trampoline.t(DHExp.t) => {
+  /* NOTE: This trampoline looks like it only returns an expression, but
+   * it also mutates the eval_state and outbox references while it's
+   * running. This is a bit of a hack, but it's necessary because the
+   * trampoline is used to implement the incremental evaluation algorithm. */
 
-  let expr_id = DHExp.rep_id(init);
+  let evaluate = evaluate(~prev, ~reused_ids, ~info_map, ~outbox);
+  let expr_id = DHExp.rep_id(exp);
+  let current_top_id =
+    call_stack.stack == [] ? Some(expr_id) : current_top_id;
+  let replay_state = (state: EvaluatorState.t): EvaluatorState.t => {
+    ...state,
+    incr_eval: IncrEval.empty,
+  };
+  let update_outbox_current = (state: EvaluatorState.t) =>
+    switch (outbox, current_top_id) {
+    | (Some(outbox), Some(id)) =>
+      outbox :=
+        IncrEval.set_outbox_current(~id, ~state=replay_state(state), outbox^)
+    | (None, _)
+    | (_, None) => ()
+    };
 
-  switch (
-    IncrEval.reuse_check(
-      ~call_stack,
-      ~prev,
-      ~reuse_map,
-      ~info_map,
-      ~id=expr_id,
-    )
-  ) {
-  | Some(entry) =>
-    state := EvaluatorState.replay_slice(entry.state, state^);
-    state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
-    /* Copy cache entries for every sub-id of the reused subtree from prev
-     * into curr. Without this, descendants of a reused ancestor are absent
-     * from the outgoing incr_eval (because the reuse short-circuits before
-     * we descend), and a later run that can't reuse the ancestor will
-     * cache-miss at the descendants — even though their values are still
-     * valid. Walks entry.prev_elab (which is the cached subtree's elab)
-     * and brings forward each sub-id's prev entry, if any. */
-    let f_exp = (continue, e: Exp.t): Exp.t => {
-      let sub_id = Exp.rep_id(e);
-      if (!Id.equal(sub_id, expr_id)) {
-        switch (Id.Map.find_opt(sub_id, prev.entries)) {
-        | Some(sub_entry) =>
-          state := EvaluatorState.add_incr_entry(state^, sub_id, sub_entry)
-        | None => ()
-        };
+  // Fully evaluate all children and take this expression one step forward
+  let eval_0_main =
+      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t)
+      : EvaluatorEVMode.result => {
+    Eval.transition(
+      (~in_closure=?, env, child) =>
+        evaluate(
+          ~reuse_map,
+          ~in_closure?,
+          ~call_stack,
+          ~parent_state=state,
+          ~current_top_id,
+          env,
+          child,
+        ),
+      ~mode=`Environment,
+      ~targets=info_map.targets,
+      ~in_closure?,
+      env,
+      exp,
+    );
+  };
+
+  // Do the above but also run side effects on state and stack
+  let eval_1_effects =
+      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t) => {
+    let.trampoline (is_finished, effects, next) =
+      eval_0_main(~reuse_map, ~in_closure?, ~call_stack, ~state, env, exp);
+
+    let (call_stack, new_state) =
+      EvaluatorState.update(info_map, state^, call_stack, env, exp, effects);
+
+    state := new_state;
+    update_outbox_current(state^);
+
+    /* Function bodies are not incremental-cache boundaries: we do not record
+     * entries while inside a call stack, and reuse_check also refuses reuse
+     * there. */
+    let body_reuse_map =
+      if (call_stack.stack != []) {
+        reuse_map;
+      } else {
+        ReusePass.update_reuse_map_after_effects(
+          ~rhs_reused=source_id => Id.Map.mem(source_id, reused_ids),
+          ~reuse_map,
+          effects,
+        );
       };
-      continue(e);
-    };
-    let _ = TermBase.Exp.map_term(~f_exp, entry.prev_elab);
-    state := EvaluatorState.mark_incr_reused(state^, expr_id);
-    Trampoline.return((EvaluatorEVMode.Final, [], entry.value));
-  | None =>
-    switch (Id.Map.find_opt(expr_id, state^.targets)) {
-    | Some(_) => state := EvaluatorState.record_probe_start(state^, expr_id)
-    | None => ()
-    };
 
-    let state_before = state^;
+    Trampoline.return((is_finished, call_stack, body_reuse_map, next));
+  };
+
+  // Do the above but until the expression is final
+  let eval_2_until_final =
+      (~reuse_map, ~in_closure=?, ~call_stack, ~state, env, exp: DHExp.t) => {
+    let.trampoline (is_finished, call_stack, body_reuse_map, next) =
+      eval_1_effects(~reuse_map, ~in_closure?, ~call_stack, ~state, env, exp);
 
-    let eval_core = () => {
-      let.trampoline (is_finished, effects, next) =
-        Eval.transition(
-          (~in_closure=?, env, init) =>
+    switch (is_finished) {
+    | Final => Trampoline.return((next, call_stack))
+    | Uneval =>
+      let.trampoline final_value =
+        Trampoline.Next(
+          () =>
             evaluate(
-              ~reuse_map,
-              ~prev,
-              ~info_map,
+              ~reuse_map=body_reuse_map,
               ~in_closure?,
               ~call_stack,
-              state,
+              ~parent_state=state,
+              ~current_top_id,
               env,
-              init,
+              next,
             ),
-          ~mode=`Environment,
-          ~targets=state^.targets,
-          ~in_closure?,
-          env,
-          init,
         );
+      Trampoline.return((final_value, call_stack));
+    };
+  };
 
-      /* If this expression is in the targets and evaluation is complete,
-       * emit RecordExpProbe effect */
-      let effects =
-        switch (is_finished, Id.Map.find_opt(expr_id, state^.targets)) {
-        | (Final, Some(pr)) => [
-            EvaluatorState.RecordExpProbe(pr),
-            ...effects,
-          ]
-        | _ => effects
-        };
+  // Do the above but also record probe samples if required
+  let eval_3_record_probe_sample =
+      (
+        ~call_stack,
+        ~state: ref(EvaluatorState.t),
+        ~expr_id,
+        env,
+        exp: DHExp.t,
+      ) => {
+    let current_step_count = state^.step_count;
 
-      /* Save original call_stack before update. For probed compound expressions
-       * (Uneval case), we need this because:
-       * - The updated call_stack (after RecordStackFrame) should be passed to
-       *   recursive evaluation so inner expressions see the app_id
-       * - But the probe sample for THIS expression should use the original
-       *   call_stack (what it was before entering the function) */
-      let original_call_stack = call_stack;
-      let (call_stack, new_state) =
-        EvaluatorState.update(state^, call_stack, env, init, next, effects);
-      state := new_state;
-
-      /* Binder body provenance map: RecordPatMatch describes `pat <- rhs`.
-       * We add pattern provenance only when the rhs value came from the
-       * previous cache. Otherwise the binding shadows any outer provenance
-       * for those names and dependents must be recalculated. */
-      let body_reuse_map =
-        List.fold_left(
-          (reuse_map, effect) =>
-            switch (effect) {
-            | EvaluatorState.RecordPatMatch({pat, rhs, _}) =>
-              let source_id = DHExp.rep_id(rhs);
-              IncrEval.update_maps_after_binding(
-                ~rhs_reused=IncrEval.was_reused(source_id, state^.incr_eval),
-                ~source_id,
-                pat,
-                ~reuse_map,
-              );
-            | _ => reuse_map
-            },
-          reuse_map,
-          effects,
-        );
+    /* Save original call_stack before update. For probed compound expressions
+     * (Uneval case), we need this because:
+     * - The updated call_stack (after RecordStackFrame) should be passed to
+     *   recursive evaluation so inner expressions see the app_id
+     * - But the probe sample for THIS expression should use the original
+     *   call_stack (what it was before entering the function) */
+    let original_call_stack = call_stack;
 
-      switch (is_finished) {
-      | Final => Trampoline.return((EvaluatorEVMode.Final, [], next))
-      | Uneval =>
-        /* Compound Expression Probe Capture via Trampoline.Bind
-         *
-         * Problem: Compound expressions (if, let, case, function application) step
-         * with is_finished=Uneval, meaning their result is a new expression with a
-         * different ID. Without special handling, we'd call evaluate(next) and lose
-         * the probe context since next.id != expr_id.
-         *
-         * Example: ^^probe(if true then 1 else 2)
-         *   1. expr_id = ID of the if expression, which is in targets
-         *   2. transition returns (Uneval, effects, next=1) - If stepped to branch
-         *   3. Without Bind: evaluate(1) runs, returns Final, but expr_id is lost
-         *   4. With Bind: we capture the final value when evaluate(1) completes,
-         *      then record the sample with the original expr_id
-         *
-         * Nested probes like ^^probe(if true then ^^probe(1) else 2) work correctly:
-         * each probe creates its own Bind continuation, and they're unwound in order.
-         * Trampoline.Bind creates a continuation that runs AFTER all recursive
-         * evaluation completes, at which point state^ reflects all step count
-         * mutations, but we still have expr_id in scope.
-         *
-         * Important: We use original_call_stack for the probe sample (the call_stack
-         * before RecordStackFrame), but call_stack (the updated one) for recursive
-         * evaluation. This ensures:
-         * - ^^probe(f(x)) records a sample with the call_stack BEFORE entering f
-         * - Expressions inside f see the app_id of f(x) in their call_stacks
-         */
-        switch (Id.Map.find_opt(expr_id, state^.targets)) {
-        | Some(probe) =>
-          let.trampoline (_, _, final_value) =
-            Trampoline.Next(
-              () =>
-                evaluate(
-                  ~reuse_map=body_reuse_map,
-                  ~prev,
-                  ~info_map,
-                  ~call_stack,
-                  state,
-                  env,
-                  next,
-                ),
-            );
-          let step_start =
-            EvaluatorState.get_probe_start(state^, expr_id)
-            |> Option.value(~default=0);
-          let step_end = state^.step_count - 1;
-          let args =
-            EvaluatorState.lookup_app_arg(
-              state^,
-              expr_id,
-              original_call_stack,
-            );
-          let sample =
-            Sample.mk(
-              ~args,
-              ~step_start,
-              ~step_end,
-              expr_id,
-              final_value,
-              env,
-              original_call_stack,
-              probe,
-            );
-          state := EvaluatorState.clear_probe_start(state^, expr_id);
-          state := EvaluatorState.add_sample(state^, sample);
-          Trampoline.return((EvaluatorEVMode.Final, [], final_value));
-        | None =>
-          Trampoline.Next(
-            () =>
-              evaluate(
-                ~reuse_map=body_reuse_map,
-                ~prev,
-                ~info_map,
-                ~call_stack,
-                state,
-                env,
-                next,
-              ),
-          )
-        }
-      };
+    let.trampoline (final_value, probe_call_stack) =
+      eval_2_until_final(
+        ~reuse_map,
+        ~in_closure?,
+        ~call_stack,
+        ~state,
+        env,
+        exp,
+      );
+
+    // Record probe sample if required
+    switch (Id.Map.find_opt(expr_id, info_map.targets)) {
+    | Some(probe) =>
+      let step_start = current_step_count;
+      let step_end = state^.step_count - 1;
+      let args =
+        CallStack.lookup_app_arg(
+          probe_call_stack,
+          expr_id,
+          original_call_stack.stack,
+        );
+      let sample =
+        Sample.mk(
+          ~args,
+          ~step_start,
+          ~step_end,
+          expr_id,
+          final_value,
+          env,
+          original_call_stack.stack,
+          probe,
+        );
+      state := EvaluatorState.add_sample(state^, sample);
+      update_outbox_current(state^);
+    | None => ()
     };
 
-    // Record incremental entry if required
-    let info_snapshot =
-      if (call_stack != []) {
-        None;
-      } else {
-        EvalInfoMap.find_opt(expr_id, info_map);
+    Trampoline.return(final_value);
+  };
+
+  // Do the above but also reuse the previous result if possible
+  let eval_4_reuse =
+      (
+        ~call_stack: CallStack.t',
+        ~state: ref(EvaluatorState.t),
+        ~expr_id,
+        env,
+        exp: DHExp.t,
+      )
+      : Trampoline.t(DHExp.t) => {
+    switch (
+      IncrEval.reuse_check(
+        ~call_stack,
+        ~prev,
+        ~reuse_map,
+        ~info_map,
+        ~id=expr_id,
+      )
+    ) {
+    | Some(entry) =>
+      // Evaluation cache hit: reuse previous result
+      state := EvaluatorState.append(state^, entry.state);
+      update_outbox_current(state^);
+      // Add the entry to the next incremental evaluation cache
+      state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
+      // Copy cache entries for every sub-id of the reused subtree from prev
+      let f_exp = (continue, e: Exp.t): Exp.t => {
+        let sub_id = Exp.rep_id(e);
+        if (!Id.equal(sub_id, expr_id)) {
+          switch (Id.Map.find_opt(sub_id, prev.entries)) {
+          | Some(sub_entry) =>
+            state := EvaluatorState.add_incr_entry(state^, sub_id, sub_entry)
+          | None => ()
+          };
+        };
+        continue(e);
       };
-    switch (info_snapshot) {
-    | None => eval_core()
-    | Some({
-        elab_term: prev_elab,
-        co_ctx,
-        probe_targets: prev_probe_targets,
-        _,
-      }) =>
-      let.trampoline (status, effects, final) = eval_core();
-      let state_slice =
-        EvaluatorState.capture_slice(~before=state_before, ~after=state^);
-      let entry: IncrEval.entry = {
-        prev_elab,
-        prev_reuse_map:
-          IncrEval.make_clean(
-            IncrEval.restrict_to_co_ctx(reuse_map, co_ctx),
-          ),
-        prev_probe_targets,
-        value: final,
-        state: state_slice,
+      let _ = TermBase.Exp.map_term(~f_exp, entry.prev_elab);
+      // Return
+      Trampoline.return(entry.value);
+    | None =>
+      // Evaluation cache miss: evaluate the expression from scratch
+      let.trampoline final_value =
+        eval_3_record_probe_sample(~call_stack, ~state, ~expr_id, env, exp);
+
+      // Record incremental entry if required
+      let info_snapshot =
+        if (call_stack.stack != []) {
+          None;
+        } else {
+          EvalInfo.find_opt(expr_id, info_map);
+        };
+      switch (info_snapshot) {
+      | None => Trampoline.return(final_value)
+      | Some({
+          elab_term: prev_elab,
+          co_ctx,
+          probe_targets: prev_probe_targets,
+          _,
+        }) =>
+        let entry: IncrEval.entry(EvaluatorState.t) = {
+          prev_elab,
+          prev_reuse_map:
+            IncrEval.make_clean(
+              IncrEval.restrict_to_co_ctx(reuse_map, co_ctx),
+            ),
+          prev_probe_targets,
+          value: final_value,
+          state: replay_state(state^),
+        };
+
+        // Return
+        switch (outbox) {
+        | Some(outbox) =>
+          outbox := IncrEval.add_outbox_entry(expr_id, entry, outbox^)
+        | None => ()
+        };
+        state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
+        Trampoline.return(final_value);
       };
-      state := EvaluatorState.add_incr_entry(state^, expr_id, entry);
-      state := EvaluatorState.mark_incr_recalculated(state^, expr_id);
-      Trampoline.return((status, effects, final));
     };
   };
+
+  // [PERF] We collect separate states for top-level expressions so we can replay those states.
+  let eval_5_state_merge =
+      (~call_stack: CallStack.t', ~state, ~expr_id, env, exp) =>
+    if (call_stack.stack == []) {
+      let inner_state =
+        ref(EvaluatorState.empty_at(parent_state^.step_count));
+      let.trampoline final_value =
+        eval_4_reuse(~call_stack, ~state=inner_state, ~expr_id, env, exp);
+      let new_state = EvaluatorState.append(state^, inner_state^);
+      state :=
+        {
+          ...new_state,
+          incr_eval:
+            IncrEval.add_stream(inner_state^.incr_eval, new_state.incr_eval),
+        };
+      update_outbox_current(inner_state^);
+      Trampoline.return(final_value);
+    } else {
+      eval_4_reuse(~call_stack, ~state, ~expr_id, env, exp);
+    };
+
+  eval_5_state_merge(~call_stack, ~state=parent_state, ~expr_id, env, exp);
 };
 
+[@deriving (show({with_path: false}), sexp, yojson)]
+type limited_result =
+  | LimitedCompleted((Exp.t, EvaluatorState.t))
+  | StepLimitExceeded;
+
 let evaluate_and_limit =
     (
-      ~step_limit: option(int)=?,
-      ~targets: Sample.targets=Sample.no_targets,
-      ~prev: IncrEval.t=IncrEval.empty,
-      ~info_map: EvalInfoMap.t=EvalInfoMap.empty,
+      ~step_limit: int,
+      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
+      ~info_map: EvalInfo.t=EvalInfo.empty,
       ~env,
       ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
+      ~outbox: option(ref(IncrEval.outbox(EvaluatorState.t)))=?,
       d: DHExp.t,
     )
-    : step_constrained((Exp.t, EvaluatorState.t)) => {
-  let state = ref(EvaluatorState.mk(~targets));
+    : limited_result => {
+  let state = ref(EvaluatorState.empty);
+  let reused_ids =
+    Id.Map.map(
+      _ => (),
+      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
+    );
   let result =
-    evaluate(~prev, ~info_map, ~call_stack=[], ~reuse_map, state, env, d);
-  let result = Trampoline.run(~step_limit?, result);
+    evaluate(
+      ~prev,
+      ~info_map,
+      ~call_stack=CallStack.empty,
+      ~reuse_map,
+      ~reused_ids,
+      ~parent_state=state,
+      ~outbox,
+      ~current_top_id=None,
+      env,
+      d,
+    );
+  let result =
+    Trampoline.Yielding.run_slice(
+      ~step_budget=step_limit,
+      result |> Trampoline.Yielding.start,
+    );
   switch (result) {
-  | Completed((_, _, x)) =>
-    Completed((x |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^))
-  | StepLimitExceeded => StepLimitExceeded
+  | SliceDone(x) =>
+    LimitedCompleted((
+      x |> Substitution.in_exp(env) |> Exp.replace_all_ids,
+      state^,
+    ))
+  | SliceYielded(_) => StepLimitExceeded
   };
 };
 
-let evaluate =
+type yielding_evaluation = {
+  env: Environment.t(Exp.t),
+  state: ref(EvaluatorState.t),
+  outbox: ref(IncrEval.outbox(EvaluatorState.t)),
+  continuation: Trampoline.Yielding.continuation(DHExp.t),
+};
+
+type yielding_result =
+  | EvaluationCompleted((Exp.t, EvaluatorState.t))
+  | EvaluationYielded(yielding_evaluation);
+
+let start_yielding_evaluation =
     (
-      ~targets: Sample.targets=Sample.no_targets,
-      ~prev: IncrEval.t=IncrEval.empty,
-      ~info_map: EvalInfoMap.t=EvalInfoMap.empty,
+      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
+      ~info_map: EvalInfo.t=EvalInfo.empty,
       ~env,
+      ~reuse_map: IncrEval.reuse_map=IncrEval.clean_reuse_map_of_env(env),
       d: DHExp.t,
     )
-    : (Exp.t, EvaluatorState.t) =>
-  switch (evaluate_and_limit(~targets, ~prev, ~info_map, ~env, d)) {
-  | Completed(x) => x
-  | StepLimitExceeded =>
-    raise(Failure("Impossible: Step limit exceeded when not set"))
+    : yielding_evaluation => {
+  let state = ref(EvaluatorState.empty);
+  let outbox = ref(IncrEval.empty_outbox);
+  let reused_ids =
+    Id.Map.map(
+      _ => (),
+      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
+    );
+  let result =
+    evaluate(
+      ~outbox=Some(outbox),
+      ~prev,
+      ~info_map,
+      ~call_stack=CallStack.empty,
+      ~reuse_map,
+      ~reused_ids,
+      ~parent_state=state,
+      ~current_top_id=None,
+      env,
+      d,
+    );
+  {
+    env,
+    state,
+    outbox,
+    continuation: Trampoline.Yielding.start(result),
   };
+};
+
+let drain_streaming_outbox =
+    (evaluation: yielding_evaluation): IncrEval.outbox(EvaluatorState.t) => {
+  let outbox = evaluation.outbox^;
+  evaluation.outbox := IncrEval.empty_outbox;
+  outbox;
+};
+
+let run_yielding_slice =
+    (~step_budget: int, evaluation: yielding_evaluation): yielding_result =>
+  switch (
+    Trampoline.Yielding.run_slice(~step_budget, evaluation.continuation)
+  ) {
+  | SliceDone(x) =>
+    EvaluationCompleted((
+      x |> Substitution.in_exp(evaluation.env) |> Exp.replace_all_ids,
+      evaluation.state^,
+    ))
+  | SliceYielded(continuation) =>
+    EvaluationYielded({
+      ...evaluation,
+      continuation,
+    })
+  };
+
+let evaluate =
+    (
+      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
+      ~info_map: EvalInfo.t=EvalInfo.empty,
+      ~env,
+      d: DHExp.t,
+    )
+    : (Exp.t, EvaluatorState.t) => {
+  let state = ref(EvaluatorState.empty);
+  let reuse_map = IncrEval.clean_reuse_map_of_env(env);
+  let reused_ids =
+    Id.Map.map(
+      _ => (),
+      ReusePass.reuse_pass(~prev, ~info_map, ~env, ~reuse_map, d).entries,
+    );
+  let result =
+    evaluate(
+      ~prev,
+      ~info_map,
+      ~call_stack=CallStack.empty,
+      ~reuse_map,
+      ~reused_ids,
+      ~parent_state=state,
+      env,
+      d,
+      ~outbox=None,
+      ~current_top_id=None,
+    );
+  let e = Trampoline.run(result);
+  (e |> Substitution.in_exp(env) |> Exp.replace_all_ids, state^);
+};
```

</details>

<details open>
<summary><code>src/language/dynamics/Evaluator.rei</code> · [@deriving (show({with_path: false}), eq)]</summary>

<!-- changetour:hunk file=src/language/dynamics/Evaluator.rei level=2 baseBlob=15fb08012098da0d43c4ddebcdfe0e77e35f50b3 -->

```diff
@@ -1,28 +1,49 @@
 // INVARIANT: this evaluate function should never return an expression with closures.
 
-[@deriving (show({with_path: false}), eq)]
-type step_constrained('a) =
-  | StepLimitExceeded
-  | Completed('a);
+type yielding_evaluation;
+
+type yielding_result =
+  | EvaluationCompleted((Exp.t, EvaluatorState.t))
+  | EvaluationYielded(yielding_evaluation);
+
+[@deriving (show({with_path: false}), sexp, yojson)]
+type limited_result =
+  | LimitedCompleted((Exp.t, EvaluatorState.t))
+  | StepLimitExceeded;
 
 let evaluate:
   (
-    ~targets: Sample.targets=?,
-    ~prev: IncrEval.t=?,
-    ~info_map: EvalInfoMap.t=?,
+    ~prev: EvaluatorState.incr_eval=?,
+    ~info_map: EvalInfo.t=?,
     ~env: Environment.t(Exp.t),
     Exp.t
   ) =>
   (Exp.t, EvaluatorState.t);
 
 let evaluate_and_limit:
   (
-    ~step_limit: int=?,
-    ~targets: Sample.targets=?,
-    ~prev: IncrEval.t=?,
-    ~info_map: EvalInfoMap.t=?,
+    ~step_limit: int,
+    ~prev: EvaluatorState.incr_eval=?,
+    ~info_map: EvalInfo.t=?,
+    ~env: Environment.t(Exp.t),
+    ~reuse_map: IncrEval.reuse_map=?,
+    ~outbox: ref(IncrEval.outbox(EvaluatorState.t))=?,
+    Exp.t
+  ) =>
+  limited_result;
+
+let start_yielding_evaluation:
+  (
+    ~prev: EvaluatorState.incr_eval=?,
+    ~info_map: EvalInfo.t=?,
     ~env: Environment.t(Exp.t),
     ~reuse_map: IncrEval.reuse_map=?,
     Exp.t
   ) =>
-  step_constrained((Exp.t, EvaluatorState.t));
+  yielding_evaluation;
+
+let run_yielding_slice:
+  (~step_budget: int, yielding_evaluation) => yielding_result;
+
+let drain_streaming_outbox:
+  yielding_evaluation => IncrEval.outbox(EvaluatorState.t);
```

</details>

## The transition interface

`Transition` gets some simple long-overdue cleanup:

 - we drop the state which wasn't being used
 - we add an inner_result because it's different to result (the types in transition remain confusing to understand)

<details open>
<summary><code>src/language/dynamics/transition/Transition.re</code> · type state;</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -155,17 +155,17 @@ let (let-unbox) = ((request, v), f) => {
   f(result);
 };
 module type EV_MODE = {
-  type state;
   type result;
+  type inner_result;
   type requirement('a);
   type requirements('a, 'b);
 
   let req_final:
-    (DHExp.t => result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
+    (DHExp.t => inner_result, EvalCtx.t => EvalCtx.t, DHExp.t) =>
     requirement(DHExp.t);
   let req_all_final:
     (
-      DHExp.t => result,
+      DHExp.t => inner_result,
       (EvalCtx.t, (list(DHExp.t), list(DHExp.t))) => EvalCtx.t,
       list(DHExp.t)
     ) =>
```

</details>

<details open>
<summary><code>src/language/dynamics/transition/Transition.re</code> · (~in_closure: unit =&gt; unit=?, Environment.t(Exp.t), DHExp.t…</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -396,7 +396,8 @@ module Transition = (EV: EV_MODE) => {
   let transition =
       (
         req:
-          (~in_closure: unit => unit=?, Environment.t(Exp.t), DHExp.t) => 'a,
+          (~in_closure: unit => unit=?, Environment.t(Exp.t), DHExp.t) =>
+          EV.inner_result,
         ~mode: [
            | `Substitution
            | `Environment
```

</details>

<details open>
<summary><code>src/language/dynamics/transition/Transition.re</code> · let generated = term =&gt;</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -414,6 +415,8 @@ module Transition = (EV: EV_MODE) => {
         term,
         ids: [rep_id(d)],
       });
+    let generated = term =>
+      Id.Map.is_empty(targets) ? Exp.temp(term) : Exp.fresh(term);
 
     let (let.wrap_closure) = ((env, d'), f: unit => rule) =>
       switch (mode) {
```

</details>

<details open>
<summary><code>src/language/dynamics/ValueChecker.re</code> · include EV_MODE with type result = t and type state = unit;</summary>

<!-- changetour:hunk file=src/language/dynamics/ValueChecker.re level=2 baseBlob=462eaa367d705ab6faecdc1ec453b34e8a1bb5e5 -->

```diff
@@ -6,10 +6,10 @@ type t =
   | Expr;
 
 module ValueCheckerEVMode: {
-  include EV_MODE with type result = t and type state = unit;
+  include EV_MODE with type inner_result = t and type result = t;
 } = {
-  type state = unit;
   type result = t;
+  type inner_result = result;
 
   type requirement('a) = ('a, result);
   type requirements('a, 'b) = ('a, result);
```

</details>

<details open>
<summary><code>src/language/dynamics/stepper/EvaluatorStep.re</code> · EV_MODE with</summary>

<!-- changetour:hunk file=src/language/dynamics/stepper/EvaluatorStep.re level=2 baseBlob=f0008d174646020360ec90b5768aeaa5a52a157a -->

```diff
@@ -255,13 +255,12 @@ module Decompose = {
 
   module DecomposeEVMode: {
     include
-      EV_MODE with
-        type result = Result.t and type state = ref(EvaluatorState.t);
+      EV_MODE with type inner_result = Result.t and type result = Result.t;
   } = {
-    type state = ref(EvaluatorState.t);
     type requirement('a) = (Result.t, 'a);
     type requirements('a, 'b) = ('b, Result.t, Environment.t(Exp.t), 'a);
     type result = Result.t;
+    type inner_result = result;
 
     let (&&&): (Result.t, Result.t) => Result.t =
       (u, v) =>
```

</details>

<details open>
<summary><code>src/language/dynamics/stepper/EvaluatorStep.re</code> · type result = option(DHExp.t) and type state = ref(Evaluato…</summary>

<!-- changetour:hunk file=src/language/dynamics/stepper/EvaluatorStep.re level=2 baseBlob=f0008d174646020360ec90b5768aeaa5a52a157a -->

```diff
@@ -349,12 +348,12 @@ module TakeStep = {
   module TakeStepEVMode: {
     include
       EV_MODE with
-        type result = option(DHExp.t) and type state = ref(EvaluatorState.t);
+        type inner_result = option(DHExp.t) and type result = option(DHExp.t);
   } = {
-    type state = ref(EvaluatorState.t);
     type requirement('a) = 'a;
     type requirements('a, 'b) = 'a;
     type result = option(DHExp.t);
+    type inner_result = result;
 
     // Assume that everything is either value or final as required.
     let req_final = (_, _, d) => d;
```

</details>

## The always-alive worker

Previously a fresh web worker was spawned per evaluation and torn down on timeout. Now there is one persistent worker driven by a small state machine. On a request it immediately ACKs with the predicted reuse plan, then evaluates the batch one item at a time in 5000-step async slices, streaming completed cache entries after each slice. A newer request supersedes the running one at the next slice boundary.

<details open>
<summary><code>src/web/util/WorkerServer.re</code> · module Js = Js_of_ocaml.Js;</summary>

<!-- changetour:hunk file=src/web/util/WorkerServer.re level=2 baseBlob=0b9503e8709d2a3902d229dc3d3a2ef3abc2f212 -->

```diff
@@ -1,22 +1,15 @@
 open Util;
+module Js = Js_of_ocaml.Js;
 
-[@deriving (sexp, yojson)]
+[@deriving (show, sexp, yojson)]
 type key = string;
 
 module Request = {
   [@deriving (show, sexp, yojson)]
   type value = {
     expr: Language.Exp.t,
-    targets: Language.Sample.targets,
-    /* Projected statics data used by the incremental driver to look up
-     * per-id sub-elaborations and co-ctxs. We ship this slice instead of
-     * the full StaticsBase.Map.t because the full map transitively contains
-     * LivelitCtx entries that embed OCaml closures, which the structured-
-     * clone algorithm postMessage uses rejects. Pass the empty slice to
-     * opt out of incremental reuse. */
-    eval_info_map: Language.EvalInfoMap.t,
-    /* Previous run's incremental map; pass IncrEval.empty on first run. */
-    prev: Language.IncrEval.t,
+    eval_info_map: Language.EvalInfo.t,
+    prev: Language.EvaluatorState.incr_eval,
   };
   [@deriving (show, sexp, yojson)]
   type t = list((string, value));
```

</details>

<details open>
<summary><code>src/web/util/WorkerServer.re</code> · let work = (req_value: Request.value): Response.value =&gt; {</summary>

<!-- changetour:hunk file=src/web/util/WorkerServer.re level=2 baseBlob=0b9503e8709d2a3902d229dc3d3a2ef3abc2f212 -->

```diff
@@ -36,11 +29,49 @@ module Response = {
     Util.StructureShareSexp.structure_share_in(sexp_of_t, t_of_sexp);
 };
 
-let work = (req_value: Request.value): Response.value => {
-  let Request.{expr, targets, eval_info_map, prev} = req_value;
+module ClientMessage = {
+  [@deriving (show, sexp, yojson)]
+  type evaluate = {
+    request_id: int,
+    batch: Request.t,
+  };
+
+  [@deriving (show, sexp, yojson)]
+  type t =
+    | Evaluate(evaluate);
+};
+
+module ServerMessage = {
+  [@deriving (show, sexp, yojson)]
+  type ack = {
+    request_id: int,
+    initial: list((key, Language.IncrEval.t(Language.EvaluatorState.t))),
+  };
+
+  [@deriving (show, sexp, yojson)]
+  type stream = {
+    request_id: int,
+    key,
+    update: Language.IncrEval.outbox(Language.EvaluatorState.t),
+  };
+
+  [@deriving (show, sexp, yojson)]
+  type result = {
+    request_id: int,
+    response: Response.t,
+  };
+
+  [@deriving (show, sexp, yojson)]
+  type t =
+    | Ack(ack)
+    | Stream(stream)
+    | Result(result);
+};
+
+let evaluate_sync = (req_value: Request.value): Response.value => {
+  let Request.{expr, eval_info_map, prev} = req_value;
   switch (
     Language.Evaluator.evaluate(
-      ~targets,
       ~prev,
       ~info_map=eval_info_map,
       ~env=Language.Builtins.env_init,
```

</details>

<details open>
<summary><code>src/web/util/WorkerServer.re</code> · | (result, state) =&gt;</summary>

<!-- changetour:hunk file=src/web/util/WorkerServer.re level=2 highlights=new:84-170 baseBlob=0b9503e8709d2a3902d229dc3d3a2ef3abc2f212 -->

```diff
@@ -53,16 +84,276 @@ let work = (req_value: Request.value): Response.value => {
   | exception exn =>
     print_endline("EXN:" ++ Printexc.to_string(exn));
     Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
-  | (result, state) =>
-    /* Clear transient data before sending to avoid serializing massive
-     * amounts of unnecessary data (e.g., app_args can be 100MB+). */
-    Ok((result, Language.EvaluatorState.clear_transient(state)))
+  | (result, state) => Ok((result, state))
   };
 };
 
-let on_request = (req: Request.t): unit => {
-  let resp: Response.t = req |> List.map(((k, v)) => (k, work(v)));
-  Js_of_ocaml.Worker.post_message(resp);
+type evaluation_start =
+  | Yielding(Language.Evaluator.yielding_evaluation)
+  | CompletedImmediately(Response.value);
+
+type running = {
+  request_id: int,
+  key,
+  remaining: Request.t,
+  completed: Response.t,
+  evaluation: Language.Evaluator.yielding_evaluation,
+};
+
+type runtime =
+  | Idle
+  | Starting
+  | Running(running);
+
+type model = {
+  latest_request: option(ClientMessage.evaluate),
+  runtime,
+  slice_already_scheduled: bool,
+};
+
+let slice_step_budget = 5000;
+let initial_model = {
+  latest_request: None,
+  runtime: Idle,
+  slice_already_scheduled: false,
+};
+
+/* Worker execution model:
+ * - `on_request` records only the newest batch and immediately ACKs with
+ *   predicted reusable entries for UI tinting.
+ * - The worker evaluates one batch item at a time in small async slices.
+ * - After each yielded slice, completed cache entries are streamed to the UI.
+ * - If a newer request arrives, the next scheduled slice abandons the stale
+ *   batch and begins the latest one. */
+
+let error_response = exn =>
+  switch (exn) {
+  | Language.EvaluatorError.Exception(reason) =>
+    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
+    Error(Language.ProgramResult.EvaulatorError(reason));
+  | exn =>
+    print_endline("EXN:" ++ Printexc.to_string(exn));
+    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
+  };
+
+let finish_success = ((result, state)): Response.value =>
+  Ok((result, state));
+
+let predict_reuse_for_request = ((key, req_value): (key, Request.value)) => {
+  let Request.{expr, eval_info_map, prev} = req_value;
+  let stream =
+    switch (
+      Language.ReusePass.reuse_pass(
+        ~prev,
+        ~info_map=eval_info_map,
+        ~env=Language.Builtins.env_init,
+        expr,
+      )
+    ) {
+    | exception _ => Language.IncrEval.empty
+    | stream => stream
+    };
+  (key, stream);
 };
 
-let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
+let start_evaluation = (req_value: Request.value): evaluation_start => {
+  let Request.{expr, eval_info_map, prev} = req_value;
+  switch (
+    Language.Evaluator.start_yielding_evaluation(
+      ~prev,
+      ~info_map=eval_info_map,
+      ~env=Language.Builtins.env_init,
+      expr,
+    )
+  ) {
+  | exception exn => CompletedImmediately(error_response(exn))
+  | evaluation => Yielding(evaluation)
+  };
+};
+
+let is_latest = (model, request_id) =>
+  switch (model.latest_request) {
+  | Some({request_id: latest_request_id, _}) =>
+    request_id == latest_request_id
+  | None => false
+  };
+
+let post_batch_result = (model, request_id, completed) =>
+  if (is_latest(model, request_id)) {
+    Js_of_ocaml.Worker.post_message(
+      ServerMessage.Result({
+        request_id,
+        response: List.rev(completed),
+      }),
+    );
+  };
+
+let post_stream_update =
+    (
+      model,
+      request_id,
+      key,
+      update: Language.IncrEval.outbox(Language.EvaluatorState.t),
+    ) =>
+  if (is_latest(model, request_id)
+      && !Language.IncrEval.outbox_is_empty(update)) {
+    Js_of_ocaml.Worker.post_message(
+      ServerMessage.Stream({
+        request_id,
+        key,
+        update,
+      }),
+    );
+  };
+
+let flush_stream_update = (model, request_id, key, evaluation) => {
+  let update = Language.Evaluator.drain_streaming_outbox(evaluation);
+  post_stream_update(model, request_id, key, update);
+};
+
+let post_ack = request =>
+  Js_of_ocaml.Worker.post_message(
+    ServerMessage.Ack({
+      request_id: request.ClientMessage.request_id,
+      initial: List.map(predict_reuse_for_request, request.batch),
+    }),
+  );
+
+let schedule_async = callback => {
+  ignore(
+    Js.Unsafe.meth_call(
+      Js.Unsafe.global,
+      "setTimeout",
+      [|
+        Js.Unsafe.inject(Js.wrap_callback(callback)),
+        Js.Unsafe.inject(0.),
+      |],
+    ),
+  );
+};
+
+let rec evaluate_next_batch_item = (model, request_id, completed, remaining) =>
+  switch (remaining) {
+  | [] =>
+    let model = {
+      ...model,
+      runtime: Idle,
+    };
+    post_batch_result(model, request_id, completed);
+    model;
+  | [(key, req_value), ...remaining] =>
+    switch (start_evaluation(req_value)) {
+    | CompletedImmediately(response) =>
+      evaluate_next_batch_item(
+        model,
+        request_id,
+        [(key, response), ...completed],
+        remaining,
+      )
+    | Yielding(evaluation) =>
+      let model = {
+        ...model,
+        runtime:
+          Running({
+            request_id,
+            key,
+            remaining,
+            completed,
+            evaluation,
+          }),
+      };
+      model;
+    }
+  }
+and begin_latest_batch = model =>
+  switch (model.latest_request) {
+  | None => {
+      ...model,
+      runtime: Idle,
+    }
+  | Some({request_id, batch}) =>
+    evaluate_next_batch_item(model, request_id, [], batch)
+  }
+and finish_current_item = (model, running, response) =>
+  evaluate_next_batch_item(
+    model,
+    running.request_id,
+    [(running.key, response), ...running.completed],
+    running.remaining,
+  )
+and run_scheduled_slice = model => {
+  let model = {
+    ...model,
+    slice_already_scheduled: false,
+  };
+  switch (model.runtime) {
+  | Idle => model
+  | Starting => begin_latest_batch(model)
+  | Running(running) when !is_latest(model, running.request_id) =>
+    begin_latest_batch(model)
+  | Running(running) =>
+    switch (
+      Language.Evaluator.run_yielding_slice(
+        ~step_budget=slice_step_budget,
+        running.evaluation,
+      )
+    ) {
+    | exception exn =>
+      finish_current_item(model, running, error_response(exn))
+    | EvaluationCompleted(value) =>
+      flush_stream_update(
+        model,
+        running.request_id,
+        running.key,
+        running.evaluation,
+      );
+      finish_current_item(model, running, finish_success(value));
+    | EvaluationYielded(evaluation) =>
+      flush_stream_update(model, running.request_id, running.key, evaluation);
+      let model = {
+        ...model,
+        runtime:
+          Running({
+            ...running,
+            evaluation,
+          }),
+      };
+      model;
+    }
+  };
+};
+
+let install_message_handler = () => {
+  let model = ref(initial_model);
+
+  let rec commit = next_model => {
+    let should_schedule_slice =
+      switch (next_model.runtime) {
+      | Idle => false
+      | Starting
+      | Running(_) => !next_model.slice_already_scheduled
+      };
+    model :=
+      should_schedule_slice
+        ? {
+          ...next_model,
+          slice_already_scheduled: true,
+        }
+        : next_model;
+    if (should_schedule_slice) {
+      schedule_async(() => commit(run_scheduled_slice(model^)));
+    };
+  };
+
+  let on_request = (msg: ClientMessage.t): unit => {
+    let ClientMessage.Evaluate(request) = msg;
+    post_ack(request);
+    commit({
+      ...model^,
+      latest_request: Some(request),
+      runtime: Starting,
+    });
+  };
+
+  Js_of_ocaml.Worker.set_onmessage(on_request);
+};
```

</details>

<details open>
<summary><code>src/web/util/WorkerClient.re</code> · let timeoutDuration = 20000; // Worker timeout in ms</summary>

<!-- changetour:hunk file=src/web/util/WorkerClient.re level=2 highlights=new:79-178 baseBlob=7922f02553881d83c905d6f02333c6a104e0958f -->

```diff
@@ -2,70 +2,178 @@ open Js_of_ocaml;
 open WorkerServer;
 
 let name = "worker.js"; // Worker file name
-let timeoutDuration = 20000; // Worker timeout in ms
+let ackTimeoutDuration = 1000; // Worker attention timeout in ms
+let evalTimeoutDuration = 20000; // Evaluation timeout in ms
 
-let initWorker: unit => Js.t(Worker.worker(Request.t, Response.t)) =
-  () => Worker.create(name);
+type callbacks = {
+  handler: Response.t => unit,
+  timeout: Request.t => unit,
+  on_ack:
+    list((key, Language.IncrEval.t(Language.EvaluatorState.t))) => unit,
+  on_stream:
+    (key, Language.IncrEval.outbox(Language.EvaluatorState.t)) => unit,
+};
 
-let workerRef: ref(Js.t(Worker.worker(Request.t, Response.t))) =
-  ref(initWorker());
+type latest = {
+  request_id: int,
+  batch: Request.t,
+  callbacks,
+};
 
-let timeoutId = ref(None);
+let nextRequestId = ref(0);
+let latestRequest: ref(option(latest)) = ref(None);
+let ackTimeoutId = ref(None);
+let evalTimeoutId = ref(None);
 
-let restart_worker = (): unit => {
-  workerRef.contents##terminate;
-  workerRef.contents = initWorker();
+let clear_timer = timer_ref => {
+  switch (timer_ref^) {
+  | Some(id) => Dom_html.window##clearTimeout(id)
+  | None => ()
+  };
+  timer_ref := None;
 };
 
-let request =
-    (
-      req: Request.t,
-      ~handler: Response.t => unit,
-      ~timeout: Request.t => unit,
-    )
-    : unit => {
-  let setupWorkerMessageHandler = worker => {
-    worker##.onmessage :=
-      Dom.handler(evt => {
-        switch (timeoutId.contents) {
-        | Some(id) => Dom_html.window##clearTimeout(id)
-        | None => ()
-        };
-        timeoutId.contents = None; /* Clear timeout after response */
-        evt##.data |> handler;
-        Js._true;
-      });
-  };
+let clear_timeouts = () => {
+  clear_timer(ackTimeoutId);
+  clear_timer(evalTimeoutId);
+};
 
-  /* If there's an ongoing request, terminate the worker and reinitialize */
-  switch (timeoutId.contents) {
-  | Some(id) =>
-    Dom_html.window##clearTimeout(id);
-    restart_worker();
-  | None => ()
+let is_latest = request_id =>
+  switch (latestRequest^) {
+  | Some({request_id: latest_request_id, _}) =>
+    request_id == latest_request_id
+  | None => false
   };
 
-  setupWorkerMessageHandler(workerRef.contents);
+let post_evaluate = (worker, request_id, batch) =>
+  worker##postMessage(
+    ClientMessage.Evaluate({
+      request_id,
+      batch,
+    }),
+  );
 
-  workerRef.contents##postMessage(req);
+let start_eval_timeout = latest => {
+  clear_timer(evalTimeoutId);
+  evalTimeoutId :=
+    Some(
+      Dom_html.window##setTimeout(
+        Js.wrap_callback(() =>
+          if (is_latest(latest.request_id)) {
+            clear_timeouts();
+            latestRequest := None;
+            latest.callbacks.timeout(latest.batch);
+          }
+        ),
+        float_of_int(evalTimeoutDuration),
+      ),
+    );
+};
 
-  let onTimeout = (): unit => {
-    restart_worker();
-    setupWorkerMessageHandler(workerRef.contents);
-    timeout(req);
+let handle_ack = ({ServerMessage.request_id, initial}: ServerMessage.ack) =>
+  if (is_latest(request_id)) {
+    clear_timer(ackTimeoutId);
+    switch (latestRequest^) {
+    | Some(latest) =>
+      latest.callbacks.on_ack(initial);
+      start_eval_timeout(latest);
+    | None => ()
+    };
+  }
+and handle_stream =
+    ({ServerMessage.request_id, key, update}: ServerMessage.stream) =>
+  if (is_latest(request_id)) {
+    switch (latestRequest^) {
+    | Some(latest) => latest.callbacks.on_stream(key, update)
+    | None => ()
+    };
+  }
+and handle_result = (request_id, response) =>
+  if (is_latest(request_id)) {
+    clear_timeouts();
+    switch (latestRequest^) {
+    | Some(latest) =>
+      latestRequest := None;
+      latest.callbacks.handler(response);
+    | None => ()
+    };
   };
 
-  timeoutId.contents =
+let setupWorkerMessageHandler = worker => {
+  worker##.onmessage :=
+    Dom.handler(evt => {
+      switch (evt##.data) {
+      | ServerMessage.Ack(ack) => handle_ack(ack)
+      | ServerMessage.Stream(stream) => handle_stream(stream)
+      | ServerMessage.Result({request_id, response}) =>
+        handle_result(request_id, response)
+      };
+      Js._true;
+    });
+};
+
+let initWorker: unit => Js.t(Worker.worker(ClientMessage.t, ServerMessage.t)) =
+  () => {
+    let worker = Worker.create(name);
+    setupWorkerMessageHandler(worker);
+    worker;
+  };
+
+let workerRef = ref(initWorker());
+
+let restart_worker = (): unit => {
+  workerRef.contents##terminate;
+  workerRef.contents = initWorker();
+};
+
+let rec start_ack_timeout = latest => {
+  clear_timer(ackTimeoutId);
+  ackTimeoutId :=
     Some(
       Dom_html.window##setTimeout(
-        Js.wrap_callback(onTimeout),
-        float_of_int(timeoutDuration),
+        Js.wrap_callback(() =>
+          if (is_latest(latest.request_id)) {
+            restart_worker();
+            post_evaluate(
+              workerRef.contents,
+              latest.request_id,
+              latest.batch,
+            );
+            start_ack_timeout(latest);
+          }
+        ),
+        float_of_int(ackTimeoutDuration),
       ),
     );
 };
 
-let request = (req, ~handler, ~timeout) =>
+let request =
+    (
+      req: Request.t,
+      ~handler: Response.t => unit,
+      ~timeout: Request.t => unit,
+      ~on_ack:
+         list((key, Language.IncrEval.t(Language.EvaluatorState.t))) => unit,
+      ~on_stream:
+         (key, Language.IncrEval.outbox(Language.EvaluatorState.t)) => unit,
+    )
+    : unit =>
   switch (req) {
   | [] => ()
-  | _ => request(req, ~handler, ~timeout)
+  | _ =>
+    clear_timeouts();
+    nextRequestId := nextRequestId^ + 1;
+    let latest = {
+      request_id: nextRequestId^,
+      batch: req,
+      callbacks: {
+        handler,
+        timeout,
+        on_ack,
+        on_stream,
+      },
+    };
+    latestRequest := Some(latest);
+    post_evaluate(workerRef.contents, latest.request_id, latest.batch);
+    start_ack_timeout(latest);
   };
```

</details>

<details open>
<summary><code>src/web/Worker.re</code> · WorkerServer.start();</summary>

<!-- changetour:hunk file=src/web/Worker.re level=2 baseBlob=b6bf31af4c31a835dacff6a0dc69bfc54c904fe0 -->

```diff
@@ -1,2 +1,2 @@
 /* Web worker thread */
-WorkerServer.start();
+WorkerServer.install_message_handler();
```

</details>

## Streaming into the UI

`ProgramResult.ResultPending` splits into two phases so the UI can distinguish "waiting for the worker to even acknowledge" from "actively evaluating" — they animate the spinner differently.

<details open>
<summary><code>src/language/dynamics/ProgramResult.re</code> · [@deriving (show({with_path: false}), sexp, yojson)]</summary>

<!-- changetour:hunk file=src/language/dynamics/ProgramResult.re level=2 baseBlob=7110fc114a1d25044bb74ce9921dce4d13a05d95 -->

```diff
@@ -17,11 +17,19 @@ type error =
   | EvaulatorError(EvaluatorError.t)
   | UnknownException(string);
 
+[@deriving (show({with_path: false}), sexp, yojson)]
+type pending_phase =
+  | AwaitingWorkerAck
+  | Evaluating;
+
 [@deriving (show({with_path: false}), sexp, yojson)]
 type t('a) =
   | ResultOk('a)
   | ResultFail(error)
-  | ResultPending;
+  | ResultPending(pending_phase);
+
+let awaiting_worker_ack = ResultPending(AwaitingWorkerAck);
+let evaluating = ResultPending(Evaluating);
 
 let get_dhexp = (r: inner) => r.result;
 let get_state = (r: inner) => r.state;
```

</details>

<details open>
<summary><code>src/language/dynamics/ProgramResult.re</code> · | ResultPending =&gt; ResultPending</summary>

<!-- changetour:hunk file=src/language/dynamics/ProgramResult.re level=2 baseBlob=7110fc114a1d25044bb74ce9921dce4d13a05d95 -->

```diff
@@ -30,5 +38,5 @@ let map = (f: 'a => 'b, r: t('a)) =>
   switch (r) {
   | ResultOk(a) => ResultOk(f(a))
   | ResultFail(e) => ResultFail(e)
-  | ResultPending => ResultPending
+  | ResultPending(phase) => ResultPending(phase)
   };
```

</details>

The result model grows a streaming pipeline: `streaming_outbox` accumulates what the worker sends, `streaming_state` materializes it via `StreamCollector`, and `pending_eval_ids` tracks the top-level leaves not yet completed (so they can be highlighted as in-progress). Two new actions — `UpdateStreamingEval` (the ACK's reuse plan) and `MergeStreamingEval` (each subsequent slice) — feed it.

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · incr_eval: Calc.saved(IncrEval.t),</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -22,7 +22,10 @@ module Model = {
     cached_targets: Calc.saved(Sample.targets), /* Input targets for cache invalidation */
     result: Calc.t(ProgramResult.t(ProgramResult.inner)),
     dynamics: Calc.saved(option(Dynamics.t)),
-    incr_eval: Calc.saved(IncrEval.t),
+    incr_eval: Calc.saved(EvaluatorState.incr_eval),
+    streaming_outbox: Calc.saved(option(IncrEval.outbox(EvaluatorState.t))),
+    streaming_state: Calc.saved(option(EvaluatorState.t)),
+    pending_eval_ids: list(Id.t),
     display,
     theorems: Theorems.Model.t,
   };
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · result: Calc.NewValue(ProgramResult.ResultPending),</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -37,9 +40,12 @@ module Model = {
     cached_settings: Calc.Pending,
     elab: Calc.Pending,
     cached_targets: Calc.Pending,
-    result: Calc.NewValue(ProgramResult.ResultPending),
+    result: Calc.NewValue(ProgramResult.awaiting_worker_ack),
     dynamics: Calc.Pending,
     incr_eval: Calc.Pending,
+    streaming_outbox: Calc.Pending,
+    streaming_state: Calc.Pending,
+    pending_eval_ids: [],
     display: Evaluation(Calc.Pending),
     theorems: Theorems.Model.init,
   };
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · result: Calc.NewValue(ProgramResult.ResultPending),</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -60,9 +66,12 @@ module Model = {
         cached_settings: Calc.Pending,
         elab: Calc.Pending,
         cached_targets: Calc.Pending,
-        result: Calc.NewValue(ProgramResult.ResultPending),
+        result: Calc.NewValue(ProgramResult.awaiting_worker_ack),
         dynamics: Calc.Pending,
         incr_eval: Calc.Pending,
+        streaming_outbox: Calc.Pending,
+        streaming_state: Calc.Pending,
+        pending_eval_ids: [],
         display: Stepper(StepperView.Model.unpersist(stepper)),
         theorems,
       }
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · let incr_eval = (model: t): IncrEval.t =&gt;</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 highlights=new:118-160 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -89,22 +98,115 @@ module Model = {
     | None => Dynamics.Map.mk(Sample.Map.empty)
     };
 
-  let incr_eval = (model: t): IncrEval.t =>
+  let incr_eval = (model: t): EvaluatorState.incr_eval =>
     model.incr_eval |> Calc.get_saved(IncrEval.empty);
 
+  let eval_is_pending = (model: t): bool =>
+    switch (Calc.get_value(model.result)) {
+    | ProgramResult.ResultPending(_) => true
+    | ProgramResult.ResultOk(_)
+    | ProgramResult.ResultFail(_) => false
+    };
+
+  let pending_eval_ids = (model: t): list(Id.t) =>
+    eval_is_pending(model) ? model.pending_eval_ids : [];
+
   let get_elaboration = (model: t): option(Exp.t) =>
     model.elab |> Calc.get_saved_opt;
 };
 
 module Update = {
   open Updated;
 
+  let is_chain = (exp: Exp.t) =>
+    switch (Exp.term_of(exp)) {
+    | Let(_)
+    | Seq(_) => true
+    | _ => false
+    };
+
+  let is_inside_function = (info_map, info) =>
+    Info.ancestors_of(info)
+    |> List.exists(ancestor_id =>
+         switch (Id.Map.find_opt(ancestor_id, info_map)) {
+         | Some(Info.InfoExp({user_term, _})) =>
+           switch (Exp.term_of(user_term)) {
+           | Fun(_)
+           | TypFun(_) => true
+           | _ => false
+           }
+         | _ => false
+         }
+       );
+
+  let is_top_level_leaf = (info_map, id, info) =>
+    switch (info) {
+    | Info.InfoExp({user_term, _}) when !is_inside_function(info_map, info) =>
+      switch (Info.parent_id_of(info)) {
+      | None => !is_chain(user_term)
+      | Some(parent_id) =>
+        switch (Id.Map.find_opt(parent_id, info_map)) {
+        | Some(Info.InfoExp({user_term: parent, _})) =>
+          switch (Exp.term_of(parent)) {
+          | Let(_, def, body) =>
+            Id.equal(id, Exp.rep_id(def))
+            || Id.equal(id, Exp.rep_id(body))
+            && !is_chain(user_term)
+          | Seq(d1, d2) =>
+            Id.equal(id, Exp.rep_id(d1))
+            || Id.equal(id, Exp.rep_id(d2))
+            && !is_chain(user_term)
+          | Test(_)
+          | HintedTest(_, _) => false
+          | _ => false
+          }
+        | _ => false
+        }
+      }
+    | _ => false
+    };
+
+  let pending_eval_worklist = (info_map: Statics.Map.t): list(Id.t) =>
+    Id.Map.fold(
+      (id, info, acc) =>
+        is_top_level_leaf(info_map, id, info) ? [id, ...acc] : acc,
+      info_map,
+      [],
+    );
+
+  let stream_visible_ids =
+      (stream: IncrEval.outbox(EvaluatorState.t)): list(Id.t) =>
+    Id.Map.fold(
+      (id, entry: IncrEval.entry(EvaluatorState.t), acc) => {
+        let subtree_ids = ref([]);
+        let f_exp = (continue, e: Exp.t): Exp.t => {
+          subtree_ids := [Exp.rep_id(e), ...subtree_ids^];
+          continue(e);
+        };
+        let _ = TermBase.Exp.map_term(~f_exp, entry.prev_elab);
+        [id, ...subtree_ids^] @ acc;
+      },
+      stream.completed.entries,
+      [],
+    );
+
+  let remove_streamed_ids = (stream, pending_eval_ids) => {
+    let completed_ids =
+      stream_visible_ids(stream) |> List.sort_uniq(Id.compare);
+    pending_eval_ids
+    |> List.filter(id =>
+         !List.exists(done_id => Id.equal(id, done_id), completed_ids)
+       );
+  };
+
   [@deriving (show({with_path: false}), sexp, yojson)]
   type t =
     | ToggleStepper
     | StepperAction(StepperView.Update.t)
     | EvalEditorAction(CodeSelectable.Update.t)
     | UpdateResult(ProgramResult.t(ProgramResult.inner))
+    | UpdateStreamingEval(IncrEval.outbox(EvaluatorState.t))
+    | MergeStreamingEval(IncrEval.outbox(EvaluatorState.t))
     | TheoremsAction(Theorems.Update.t);
 
   let can_undo = (action: t) => {
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · | UpdateStreamingEval(_)</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -113,6 +215,8 @@ module Update = {
     | StepperAction(action) => StepperView.Update.can_undo(action)
     | EvalEditorAction(action) => CodeSelectable.Update.can_undo(action)
     | UpdateResult(_) => false
+    | UpdateStreamingEval(_)
+    | MergeStreamingEval(_) => false
     | TheoremsAction(action) => Theorems.Update.can_undo(action)
     };
   };
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · pending_eval_ids:</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -160,8 +264,36 @@ module Update = {
       {
         ...model,
         result: Calc.NewValue(result),
+        pending_eval_ids:
+          switch (result) {
+          | ProgramResult.ResultPending(_) => model.pending_eval_ids
+          | ProgramResult.ResultOk(_)
+          | ProgramResult.ResultFail(_) => []
+          },
       }
       |> Updated.return_quiet
+    | (UpdateStreamingEval(stream), _) =>
+      {
+        ...model,
+        result: Calc.NewValue(ProgramResult.evaluating),
+        streaming_outbox: Calc.Calculated(Some(stream)),
+        streaming_state: Calc.Pending,
+        pending_eval_ids: remove_streamed_ids(stream, model.pending_eval_ids),
+      }
+      |> Updated.return_quiet
+    | (MergeStreamingEval(stream), _) =>
+      let current =
+        model.streaming_outbox
+        |> Calc.get_saved(None)
+        |> Option.value(~default=IncrEval.empty_outbox);
+      {
+        ...model,
+        streaming_outbox:
+          Calc.Calculated(Some(IncrEval.merge_outbox(stream, current))),
+        streaming_state: Calc.Pending,
+        pending_eval_ids: remove_streamed_ids(stream, model.pending_eval_ids),
+      }
+      |> Updated.return_quiet;
     };
 
   let calculate =
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · streaming_outbox,</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -177,6 +309,9 @@ module Update = {
           result,
           dynamics,
           incr_eval,
+          streaming_outbox,
+          streaming_state,
+          pending_eval_ids,
           display,
           theorems,
         }: Model.t,
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · EvalInfoMap.of_info_map(</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -201,8 +336,9 @@ module Update = {
      * needs. The raw info_map can't cross postMessage because LivelitCtx
      * entries contain OCaml closures. */
     let eval_info_map =
-      EvalInfoMap.of_info_map(
+      EvalInfo.of_info_map(
         ~probe_all=Calc.get_value(settings).probe_all,
+        ~targets=Calc.get_value(targets),
         statics.info_map,
       );
     let result =
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · and.calc targets = targets;</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -211,25 +347,23 @@ module Update = {
         let.calc_t elab = elab
         // TODO[Matt]: We could make this more fine-grained, we only care about one setting
         and.calc settings = settings
-        and.calc targets = targets;
+        and.calc _ = targets;
         switch (queue_worker) {
         // Dynamics is off:
-        | _ when !settings.dynamics => ProgramResult.ResultPending
+        | _ when !settings.dynamics => ProgramResult.awaiting_worker_ack
         // Using the webworker:
         | Some(queue_worker) =>
           queue_worker({
             expr: elab,
-            targets,
             eval_info_map,
             prev: prev_incr,
           });
-          ProgramResult.ResultPending;
+          ProgramResult.awaiting_worker_ack;
         // Using the main thread:
         | None =>
           switch (
-            WorkerServer.work({
+            WorkerServer.evaluate_sync({
               expr: elab,
-              targets,
               eval_info_map,
               prev: prev_incr,
             })
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · let streaming_outbox =</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 highlights=new:380-450 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -246,15 +380,71 @@ module Update = {
         };
       };
 
+    let streaming_outbox =
+      streaming_outbox
+      |> {
+        let.calc result = result;
+        switch (result) {
+        | ProgramResult.ResultPending(Evaluating) =>
+          streaming_outbox |> Calc.get_saved(None)
+        | ProgramResult.ResultPending(AwaitingWorkerAck)
+        | ProgramResult.ResultFail(_)
+        | ProgramResult.ResultOk(_) => None
+        };
+      };
+
+    let pending_eval_ids =
+      switch (result) {
+      | NewValue(ProgramResult.ResultPending(AwaitingWorkerAck)) =>
+        if (Calc.get_value(settings).dynamics) {
+          switch (queue_worker) {
+          | Some(_) => pending_eval_worklist(statics.info_map)
+          | None => []
+          };
+        } else {
+          [];
+        }
+      | NewValue(ProgramResult.ResultOk(_))
+      | NewValue(ProgramResult.ResultFail(_)) => []
+      | NewValue(ProgramResult.ResultPending(Evaluating))
+      | OldValue(ProgramResult.ResultPending(_)) => pending_eval_ids
+      | OldValue(ProgramResult.ResultOk(_))
+      | OldValue(ProgramResult.ResultFail(_)) => []
+      };
+
+    let streaming_state =
+      streaming_state
+      |> {
+        let.calc elab = elab
+        and.calc streaming_outbox = streaming_outbox;
+        switch (streaming_outbox) {
+        | Some(streaming_outbox) =>
+          Some(StreamCollector.collect_stream_state(streaming_outbox, elab))
+        | None => None
+        };
+      };
+
     // Turn state into dynamics map
     let dynamics =
       dynamics
       |> {
-        let.calc result = result;
-        switch (result) {
-        | ProgramResult.ResultPending => dynamics |> Calc.get_saved(None)
-        | ProgramResult.ResultFail(_) => dynamics |> Calc.get_saved(None)
-        | ProgramResult.ResultOk({state, _}) =>
+        let.calc result = result
+        and.calc streaming_state = streaming_state;
+        switch (result, streaming_state) {
+        | (ProgramResult.ResultPending(_), Some(state)) =>
+          Some(
+            Dynamics.{
+              probe_map: state |> EvaluatorState.get_probes,
+              test_results:
+                state |> EvaluatorState.get_tests |> TestResults.mk_results,
+              theorems: state |> EvaluatorState.get_theorems,
+            },
+          )
+        | (ProgramResult.ResultPending(_), None) =>
+          dynamics |> Calc.get_saved(None)
+        | (ProgramResult.ResultFail(_), _) =>
+          dynamics |> Calc.get_saved(None)
+        | (ProgramResult.ResultOk({state, _}), _) =>
           Some(
             Dynamics.{
               probe_map: state |> EvaluatorState.get_probes,
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · let.calc result = result;</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -269,12 +459,15 @@ module Update = {
     let incr_eval =
       incr_eval
       |> {
-        let.calc result = result;
-        switch (result) {
-        | ProgramResult.ResultPending =>
+        let.calc result = result
+        and.calc streaming_outbox = streaming_outbox;
+        switch (result, streaming_outbox) {
+        | (ProgramResult.ResultPending(_), Some(streaming_outbox)) =>
+          streaming_outbox.completed
+        | (ProgramResult.ResultPending(_), None) =>
           incr_eval |> Calc.get_saved(IncrEval.empty)
-        | ProgramResult.ResultFail(_) => IncrEval.empty
-        | ProgramResult.ResultOk({state, _}) => state.incr_eval
+        | (ProgramResult.ResultFail(_), _) => IncrEval.empty
+        | (ProgramResult.ResultOk({state, _}), _) => state.incr_eval
         };
       };
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · | ResultPending =&gt; ev_display |&gt; Calc.get_saved_opt |&gt; Opti…</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -297,7 +490,8 @@ module Update = {
                 exp |> CodeSelectable.Model.mk_from_exp(~settings, ~root=Exp),
               ))
             | ResultFail(_)
-            | ResultPending => ev_display |> Calc.get_saved_opt |> Option.join
+            | ResultPending(_) =>
+              ev_display |> Calc.get_saved_opt |> Option.join
             };
           };
         let result_changed = Calc.is_new(ev_calc);
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · streaming_outbox: streaming_outbox |&gt; Calc.save,</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -358,6 +552,9 @@ module Update = {
         result: result |> Calc.make_old,
         dynamics: dynamics |> Calc.save,
         incr_eval: incr_eval |> Calc.save,
+        streaming_outbox: streaming_outbox |> Calc.save,
+        streaming_state: streaming_state |> Calc.save,
+        pending_eval_ids,
         display,
         theorems,
       }: Model.t
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · let status_of: ProgramResult.t('a) =&gt; string =</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -420,12 +617,19 @@ module View = {
     | Timeout => "Evaluation timed out"
     };
 
-  let status_of: ProgramResult.t('a) => string =
+  let result_status_of: ProgramResult.t('a) => string =
     fun
-    | ResultPending => "pending"
+    | ResultPending(_) => "pending"
     | ResultOk(_) => "ok"
     | ResultFail(_) => "fail";
 
+  let status_classes_of: ProgramResult.t('a) => list(string) =
+    fun
+    | ResultPending(AwaitingWorkerAck) => ["pending", "pending-attention"]
+    | ResultPending(Evaluating) => ["pending", "pending-evaluating"]
+    | ResultOk(_) => ["ok"]
+    | ResultFail(_) => ["fail"];
+
   let live_eval =
       (
         ~globals: Globals.t,
```

</details>

<details open>
<summary><code>src/web/app/editors/result/EvalResult.re</code> · ~attrs=[Attr.classes(["status", status_of(result)])],</summary>

<!-- changetour:hunk file=src/web/app/editors/result/EvalResult.re level=2 baseBlob=f10e0161dad1e7f0024af5f7d5fd9e6d6161f4ed -->

```diff
@@ -473,14 +677,14 @@ module View = {
         exn_view
         @ [
           div(
-            ~attrs=[Attr.classes(["status", status_of(result)])],
+            ~attrs=[Attr.classes(["status"] @ status_classes_of(result))],
             [
               div(~attrs=[Attr.classes(["spinner"])], []),
               div(~attrs=[Attr.classes(["eq"])], [text("≡")]),
             ],
           ),
           div(
-            ~attrs=[Attr.classes(["result", status_of(result)])],
+            ~attrs=[Attr.classes(["result", result_status_of(result)])],
             Option.to_list(code_view),
           ),
         ]
```

</details>

The editor view paints the in-progress regions. `CodeEditable` keeps pending highlights on even when the cache-hit tint setting is off (progress feedback is transient, not a debug aid), and `Highlight` grows an `incremental-pending`/`incremental-active` decoration with an animated sweep on the currently-evaluating leaf.

<details open>
<summary><code>src/web/app/editors/cell/CellEditor.re</code> · ~pending_eval_ids=EvalResult.Model.pending_eval_ids(model.r…</summary>

<!-- changetour:hunk file=src/web/app/editors/cell/CellEditor.re level=2 baseBlob=4f020dff8dbfa4f46c832bb3fe356977d03060c1 -->

```diff
@@ -280,6 +280,8 @@ module View = {
           ~lines,
           ~dynamics=EvalResult.Model.dynamics(model.result),
           ~incr_eval=EvalResult.Model.incr_eval(model.result),
+          ~pending_eval_ids=EvalResult.Model.pending_eval_ids(model.result),
+          ~show_active_eval=EvalResult.Model.eval_is_pending(model.result),
           model.editor,
         ),
       ]
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeEditable.re</code> · ~incr_eval: Language.IncrEval.t=Language.IncrEval.empty,</summary>

<!-- changetour:hunk file=src/web/app/editors/code/CodeEditable.re level=2 baseBlob=aa7fb4bbcc64e5b8f4e984791ce51f7f8df685cc -->

```diff
@@ -534,7 +534,9 @@ module View = {
         ~overlays: list(Node.t)=[],
         ~lines: bool=false,
         ~dynamics: Language.Dynamics.Map.t,
-        ~incr_eval: Language.IncrEval.t=Language.IncrEval.empty,
+        ~incr_eval: option(Language.EvaluatorState.incr_eval)=?,
+        ~pending_eval_ids: list(Id.t)=[],
+        ~show_active_eval: bool=false,
         ~expand_selection=?,
         model: Model.t,
       ) => {
```

</details>

<details open>
<summary><code>src/web/app/editors/code/CodeEditable.re</code> · /* Tint the background behind ids reused from the last run…</summary>

<!-- changetour:hunk file=src/web/app/editors/code/CodeEditable.re level=2 baseBlob=aa7fb4bbcc64e5b8f4e984791ce51f7f8df685cc -->

```diff
@@ -665,26 +667,31 @@ module View = {
         model.editor.syntax.projector_list,
       );
     ProjectorView.ViewCache.log_frame();
-    /* Tint the background behind ids reused from the last run (cache hits)
-     * with an icy wash, so the user can see what the incremental evaluator
-     * is skipping. Gated behind a nut-menu setting because it's distracting
-     * during normal editing. */
+    /* The nut-menu setting only controls blue cache-hit highlights. Pending
+     * evaluation highlights are transient progress feedback, so keep them on
+     * while the worker is running. */
     let incr_eval_overlay =
-      if (globals.settings.show_incremental_deco) {
-        [
+      switch (
+        incr_eval,
+        globals.settings.show_incremental_deco || pending_eval_ids != [],
+      ) {
+      | (Some(incr_eval), true) => [
           Node.div(
             ~attrs=[Attr.classes(["code-deco", "incremental-deco"])],
             [
               Highlight.incr_eval(
                 ~font_metrics=globals.font_metrics,
                 ~syntax=model.editor.syntax,
+                ~pending_eval_ids,
+                ~show_active_eval,
+                ~show_frozen=globals.settings.show_incremental_deco,
                 incr_eval,
               ),
             ],
           ),
-        ];
-      } else {
-        [];
+        ]
+      | (None, _)
+      | (Some(_), false) => []
       };
     let overlays =
       incr_eval_overlay
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Highlight.re</code> · let active = List.mem("incremental-active", clss);</summary>

<!-- changetour:hunk file=src/web/app/editors/decoration/Highlight.re level=2 baseBlob=f62ce1b44ec09b7d616abfd0a32298461cce2661 -->

```diff
@@ -369,6 +369,50 @@ let svg_of_group =
 
     let path_cmds =
       outline_path(~origin_col=bb.min_col, ~origin_row=bb.min_row, rows);
+    let active = List.mem("incremental-active", clss);
+    let clip_id =
+      Printf.sprintf(
+        "incremental-active-%d-%d-%d-%d",
+        int_of_float(bb.min_col *. 10.0),
+        bb.min_row,
+        int_of_float(bb.max_col *. 10.0),
+        bb.max_row,
+      );
+    let sweep_width = max(1.0, width_f *. 0.45);
+    let active_sweep =
+      if (active) {
+        [
+          Node.create_svg(
+            "defs",
+            [
+              Node.create_svg(
+                "clipPath",
+                ~attrs=[Attr.create("id", clip_id)],
+                [SvgUtil.Path.view(~attrs=[], path_cmds)],
+              ),
+            ],
+          ),
+          Node.create_svg(
+            "g",
+            ~attrs=[Attr.create("clip-path", "url(#" ++ clip_id ++ ")")],
+            [
+              Node.create_svg(
+                "rect",
+                ~attrs=[
+                  Attr.classes(["incremental-sweep"]),
+                  Attr.create("x", "0"),
+                  Attr.create("y", "0"),
+                  Attr.create("width", Printf.sprintf("%f", sweep_width)),
+                  Attr.create("height", Printf.sprintf("%f", height_f)),
+                ],
+                [],
+              ),
+            ],
+          ),
+        ];
+      } else {
+        [];
+      };
 
     Some(
       Node.create_svg(
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Highlight.re</code> · [SvgUtil.Path.view(~attrs=[], path_cmds)],</summary>

<!-- changetour:hunk file=src/web/app/editors/decoration/Highlight.re level=2 baseBlob=f62ce1b44ec09b7d616abfd0a32298461cce2661 -->

```diff
@@ -391,7 +435,7 @@ let svg_of_group =
           ),
           Attr.create("preserveAspectRatio", "none"),
         ],
-        [SvgUtil.Path.view(~attrs=[], path_cmds)],
+        [SvgUtil.Path.view(~attrs=[], path_cmds)] @ active_sweep,
       ),
     );
   };
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Highlight.re</code> · incr: Language.IncrEval.t,</summary>

<!-- changetour:hunk file=src/web/app/editors/decoration/Highlight.re level=2 baseBlob=f62ce1b44ec09b7d616abfd0a32298461cce2661 -->

```diff
@@ -645,18 +689,17 @@ let incr_eval =
     (
       ~font_metrics: FontMetrics.t,
       ~syntax: CachedSyntax.t,
-      incr: Language.IncrEval.t,
+      ~pending_eval_ids: list(Id.t)=[],
+      ~show_active_eval: bool=false,
+      ~show_frozen: bool=true,
+      incr: Language.EvaluatorState.incr_eval,
     ) => {
-  /* `frozen_ids` walks each reused subtree's prev_elab and emits every
-   * rep_id encountered. Many of those ids have nested or duplicate
-   * source ranges; painting them each as its own SVG stacks the 0.55
-   * alpha and makes inner regions look darker than the surrounding
-   * tint. Keep one id per maximal (outermost) range so each visible
-   * region gets exactly one decoration. Ids without a measurable range
-   * (elab-internal, no segment) are dropped here — the surviving ids in
-   * the same subtree cover the visible portion. */
-  let ranged_ids =
-    Language.IncrEval.frozen_ids(incr)
+  let range_eq = ((o1, l1), (o2, l2)) =>
+    Point.equals(o1, o2) && Point.equals(l1, l2);
+  let range_contains = ((o1, l1), (o2, l2)) =>
+    Point.compare(o1, o2) <= 0 && Point.compare(l2, l1) <= 0;
+  let ranged_ids_of = ids =>
+    ids
     |> List.sort_uniq(Id.compare)
     |> List.filter_map(id =>
          switch (
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Highlight.re</code> · let range_eq = ((o1, l1), (o2, l2)) =&gt;</summary>

<!-- changetour:hunk file=src/web/app/editors/decoration/Highlight.re level=2 baseBlob=f62ce1b44ec09b7d616abfd0a32298461cce2661 -->

```diff
@@ -666,11 +709,12 @@ let incr_eval =
          | None => None
          }
        );
-  let range_eq = ((o1, l1), (o2, l2)) =>
-    Point.equals(o1, o2) && Point.equals(l1, l2);
-  let range_contains = ((o1, l1), (o2, l2)) =>
-    Point.compare(o1, o2) <= 0 && Point.compare(l2, l1) <= 0;
-  let outermost =
+  let range_compare = ((_, (o1, l1)), (_, (o2, l2))) =>
+    switch (Point.compare(o1, o2)) {
+    | 0 => Point.compare(l1, l2)
+    | cmp => cmp
+    };
+  let outermost = ranged_ids =>
     List.fold_left(
       (acc, (id, r)) =>
         if (List.exists(
```

</details>

<details open>
<summary><code>src/web/app/editors/decoration/Highlight.re</code> · let frozen_ids =</summary>

<!-- changetour:hunk file=src/web/app/editors/decoration/Highlight.re level=2 highlights=new:729-775 baseBlob=f62ce1b44ec09b7d616abfd0a32298461cce2661 -->

```diff
@@ -685,12 +729,47 @@ let incr_eval =
       [],
       ranged_ids,
     );
+  let frozen_ids =
+    show_frozen ? Language.IncrEval.frozen_ids(~ack_incr=incr) : [];
+  let pending_eval_ranges =
+    pending_eval_ids |> ranged_ids_of |> List.sort(range_compare);
+  let active_ids =
+    if (show_active_eval) {
+      pending_eval_ranges |> ListUtil.hd_opt |> Option.to_list;
+    } else {
+      [];
+    };
+  let pending_leaf_ids =
+    pending_eval_ranges
+    |> List.filter(((_, range)) =>
+         !
+           List.exists(
+             ((_, active_range)) => range_eq(active_range, range),
+             active_ids,
+           )
+       );
+  let frozen_outermost = frozen_ids |> ranged_ids_of |> outermost;
   div_c(
     "incremental-highlights",
     List.concat_map(
       ((id, _)) =>
         color(~syntax, ~font_metrics, ["incremental-frozen"], id),
-      outermost,
-    ),
+      frozen_outermost,
+    )
+    @ List.concat_map(
+        ((id, _)) =>
+          color(~syntax, ~font_metrics, ["incremental-pending"], id),
+        pending_leaf_ids,
+      )
+    @ List.concat_map(
+        ((id, _)) =>
+          color(
+            ~syntax,
+            ~font_metrics,
+            ["incremental-pending", "incremental-active"],
+            id,
+          ),
+        active_ids,
+      ),
   );
 };
```

</details>

<details open>
<summary><code>src/web/www/style/dynamics.css</code> · .cell-result .status.pending-attention .spinner {</summary>

<!-- changetour:hunk file=src/web/www/style/dynamics.css level=2 baseBlob=7cd3a8274f56c2aff0fe2a2d924e42d481965243 -->

```diff
@@ -51,6 +51,14 @@
   transition: all 0.2s linear;
 }
 
+.cell-result .status.pending-attention .spinner {
+  animation: eval-spinner-spin 1.6s linear infinite reverse;
+}
+
+.cell-result .status.pending-evaluating .spinner {
+  animation: none;
+}
+
 .cell-result .status .eq {
   position: absolute;
   opacity: 1;
```

</details>

<details open>
<summary><code>src/web/www/style/dynamics.css</code> · @keyframes eval-spinner-spin {</summary>

<!-- changetour:hunk file=src/web/www/style/dynamics.css level=2 baseBlob=7cd3a8274f56c2aff0fe2a2d924e42d481965243 -->

```diff
@@ -62,6 +70,12 @@
   transition: opacity 0.6s cubic-bezier(0.65, 0, 0.35, 1);
 }
 
+@keyframes eval-spinner-spin {
+  to {
+    transform: rotate(360deg);
+  }
+}
+
 .cell-result .result {
   padding-top: 0.1em;
   min-height: 1.6em;
```

</details>

<details open>
<summary><code>src/web/www/style/dynamics.css</code> · svg.shard.incremental-pending &gt; path {</summary>

<!-- changetour:hunk file=src/web/www/style/dynamics.css level=2 baseBlob=7cd3a8274f56c2aff0fe2a2d924e42d481965243 -->

```diff
@@ -166,3 +180,39 @@ svg.shard.incremental-frozen > path {
 svg.shard.incremental-frozen {
   filter: drop-shadow(0 0 1px rgba(160, 210, 255, 0.75));
 }
+
+svg.shard.incremental-pending > path {
+  fill: var(--incremental-pending, oklch(52% 0.03 220 / 11%));
+  stroke: var(--incremental-pending-edge, oklch(52% 0.03 220 / 16%));
+  stroke-width: 0.5px;
+  vector-effect: non-scaling-stroke;
+}
+
+svg.shard.incremental-active > path {
+  fill: var(--incremental-pending, oklch(52% 0.03 220 / 11%));
+}
+
+svg.shard.incremental-active .incremental-sweep {
+  fill: var(--incremental-active-sweep, oklch(100% 0 0 / 56%));
+  transform-box: fill-box;
+  transform-origin: center;
+  animation: incremental-active-sweep 1.35s cubic-bezier(0.4, 0, 0.2, 1)
+    infinite;
+}
+
+@keyframes incremental-active-sweep {
+  0% {
+    transform: translateX(-130%);
+    opacity: 0;
+  }
+  18% {
+    opacity: 1;
+  }
+  65% {
+    opacity: 0.9;
+  }
+  100% {
+    transform: translateX(240%);
+    opacity: 0;
+  }
+}
```

</details>

<details open>
<summary><code>src/web/www/style/variables.css</code> · --incremental-pending: oklch(from var(--STONE) l c h / 11%);</summary>

<!-- changetour:hunk file=src/web/www/style/variables.css level=2 baseBlob=a97200422e5eacef101c38fd4c00c616eb7189e8 -->

```diff
@@ -63,6 +63,9 @@
    * previous evaluation run (cache hit), left as-is by the worker. */
   --incremental-frozen: rgba(200, 230, 255, 0.55);
   --incremental-frozen-edge: rgba(140, 190, 240, 0.55);
+  --incremental-pending: oklch(from var(--STONE) l c h / 11%);
+  --incremental-pending-edge: oklch(from var(--STONE) l c h / 16%);
+  --incremental-active-sweep: oklch(100% 0 0 / 56%);
 
   /* MOSS - affirmations */
   --G0: oklch(70% 0.15 150); /* page title, passing tests */
```

</details>

## Wiring streaming through the editor modes

Each editor mode builds its own worker request and routes responses back as actions. They each gain `on_ack` (seed the streaming outbox from the predicted reuse plan) and `on_stream` (merge each slice). The shapes differ because the modes key their cells differently (single cell, position-keyed grid, or named theorem slots).

<details open>
<summary><code>src/web/view/ScratchMode.re</code> · ~on_ack=</summary>

<!-- changetour:hunk file=src/web/view/ScratchMode.re level=2 baseBlob=c900ed293d8f718b5ecdfeb147d950bc138ef111 -->

```diff
@@ -1109,6 +1109,26 @@ module Update = {
                   ResultAction(UpdateResult(ResultFail(Timeout))),
                 ),
               ),
+          ~on_ack=
+            initial =>
+              switch (initial |> List.hd |> snd) {
+              | stream =>
+                schedule_action(
+                  CellAction(
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                )
+              | exception _ => ()
+              },
+          ~on_stream=
+            (_, stream) =>
+              schedule_action(
+                CellAction(ResultAction(MergeStreamingEval(stream))),
+              ),
         )
       };
       let new_sp =
```

</details>

<details open>
<summary><code>src/web/view/CodeExerciseMode.re</code> · ~timeout=_ =&gt; {</summary>

<!-- changetour:hunk file=src/web/view/CodeExerciseMode.re level=2 baseBlob=fc0de98fa00a14157dd5c8e4ef7ccc3a0f203806 -->

```diff
@@ -498,20 +498,48 @@ module Update = {
             Editor(pos', ResultAction(UpdateResult(result'))),
           );
         }),
-      ~timeout=_ => {
-        let _ =
-          CodeExercise.map_stitched(
-            (pos, _) =>
-              schedule_action(
-                Editor(
-                  pos,
-                  ResultAction(UpdateResult(ResultFail(Timeout))),
+      ~timeout=
+        _ => {
+          let _ =
+            CodeExercise.map_stitched(
+              (pos, _) =>
+                schedule_action(
+                  Editor(
+                    pos,
+                    ResultAction(UpdateResult(ResultFail(Timeout))),
+                  ),
                 ),
-              ),
-            model.cells,
-          );
-        ();
-      },
+              model.cells,
+            );
+          ();
+        },
+      ~on_ack=
+        initial => {
+          let _ =
+            List.iter(
+              ((pos, stream)) =>
+                schedule_action(
+                  Editor(
+                    CodeExercise.pos_of_key(pos),
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                ),
+              initial,
+            );
+          ();
+        },
+      ~on_stream=
+        (pos, stream) =>
+          schedule_action(
+            Editor(
+              CodeExercise.pos_of_key(pos),
+              ResultAction(MergeStreamingEval(stream)),
+            ),
+          ),
     );
 
     /* The following section pulls statics back from cells into the editors
```

</details>

<details open>
<summary><code>src/web/view/TutorialMode.re</code> · ~timeout=_ =&gt; {</summary>

<!-- changetour:hunk file=src/web/view/TutorialMode.re level=2 baseBlob=04e584c7baef063a512ee3eccaad1e99a297f522 -->

```diff
@@ -297,20 +297,48 @@ module Update = {
             Editor(pos', ResultAction(UpdateResult(result'))),
           );
         }),
-      ~timeout=_ => {
-        let _ =
-          Tutorial.map_stitched(
-            (pos, _) =>
-              schedule_action(
-                Editor(
-                  pos,
-                  ResultAction(UpdateResult(ResultFail(Timeout))),
+      ~timeout=
+        _ => {
+          let _ =
+            Tutorial.map_stitched(
+              (pos, _) =>
+                schedule_action(
+                  Editor(
+                    pos,
+                    ResultAction(UpdateResult(ResultFail(Timeout))),
+                  ),
                 ),
-              ),
-            model.cells,
-          );
-        ();
-      },
+              model.cells,
+            );
+          ();
+        },
+      ~on_ack=
+        initial => {
+          let _ =
+            List.iter(
+              ((pos, stream)) =>
+                schedule_action(
+                  Editor(
+                    Tutorial.pos_of_key(pos),
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                ),
+              initial,
+            );
+          ();
+        },
+      ~on_stream=
+        (pos, stream) =>
+          schedule_action(
+            Editor(
+              Tutorial.pos_of_key(pos),
+              ResultAction(MergeStreamingEval(stream)),
+            ),
+          ),
     );
     /* The following section pulls statics back from cells into the editors
        There are many ad-hoc things about this code, including the fact that
```

</details>

<details open>
<summary><code>src/web/view/DerivationExerciseMode.re</code> · ~timeout=_ =&gt; {</summary>

<!-- changetour:hunk file=src/web/view/DerivationExerciseMode.re level=2 baseBlob=95fa288097b3de3d6f98d7d3fd54f070ff676047 -->

```diff
@@ -469,20 +469,48 @@ module Update = {
             Editor(pos', ResultAction(UpdateResult(result'))),
           );
         }),
-      ~timeout=_ => {
-        let _ =
-          DerivationExercise.map_stitched(
-            (pos, _) =>
-              schedule_action(
-                Editor(
-                  pos,
-                  ResultAction(UpdateResult(ResultFail(Timeout))),
+      ~timeout=
+        _ => {
+          let _ =
+            DerivationExercise.map_stitched(
+              (pos, _) =>
+                schedule_action(
+                  Editor(
+                    pos,
+                    ResultAction(UpdateResult(ResultFail(Timeout))),
+                  ),
                 ),
-              ),
-            model.cells,
-          );
-        ();
-      },
+              model.cells,
+            );
+          ();
+        },
+      ~on_ack=
+        initial => {
+          let _ =
+            List.iter(
+              ((pos, stream)) =>
+                schedule_action(
+                  Editor(
+                    DerivationExercise.pos_of_key(pos),
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                ),
+              initial,
+            );
+          ();
+        },
+      ~on_stream=
+        (pos, stream) =>
+          schedule_action(
+            Editor(
+              DerivationExercise.pos_of_key(pos),
+              ResultAction(MergeStreamingEval(stream)),
+            ),
+          ),
     );
     /* The following section pulls statics back from cells into the editors
        There are many ad-hoc things about this code, including the fact that
```

</details>

<details open>
<summary><code>src/web/view/TheoremExerciseMode.re</code> · ~timeout=_ =&gt; {</summary>

<!-- changetour:hunk file=src/web/view/TheoremExerciseMode.re level=2 baseBlob=58d95d3dca766fe3995b9a258b9c19fce2bd576f -->

```diff
@@ -397,25 +397,80 @@ module Update = {
           | _ => ()
           };
         }),
-      ~timeout=_ => {
-      List.iter(
-        fun
-        | "lemmas" => {
+      ~timeout=
+        _ => {
+          List.iter(
+            fun
+            | "lemmas" => {
+                schedule_action(
+                  Prelude(ResultAction(UpdateResult(ResultFail(Timeout)))),
+                );
+                schedule_action(
+                  Lemmas(ResultAction(UpdateResult(ResultFail(Timeout)))),
+                );
+              }
+            | "theorem" =>
+              schedule_action(
+                Theorem(ResultAction(UpdateResult(ResultFail(Timeout)))),
+              )
+            | _ => (),
+            List.map(((pos, _)) => pos, worker_request^),
+          )
+        },
+      ~on_ack=
+        initial =>
+          List.iter(
+            fun
+            | ("lemmas", stream) => {
+                schedule_action(
+                  Prelude(
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                );
+                schedule_action(
+                  Lemmas(
+                    ResultAction(
+                      UpdateStreamingEval(
+                        Language.IncrEval.outbox_of_completed(stream),
+                      ),
+                    ),
+                  ),
+                );
+              }
+            | ("theorem", stream) =>
+              schedule_action(
+                Theorem(
+                  ResultAction(
+                    UpdateStreamingEval(
+                      Language.IncrEval.outbox_of_completed(stream),
+                    ),
+                  ),
+                ),
+              )
+            | _ => (),
+            initial,
+          ),
+      ~on_stream=
+        (pos, stream) =>
+          switch (pos) {
+          | "lemmas" =>
             schedule_action(
-              Prelude(ResultAction(UpdateResult(ResultFail(Timeout)))),
+              Prelude(ResultAction(MergeStreamingEval(stream))),
             );
             schedule_action(
-              Lemmas(ResultAction(UpdateResult(ResultFail(Timeout)))),
+              Lemmas(ResultAction(MergeStreamingEval(stream))),
             );
-          }
-        | "theorem" =>
-          schedule_action(
-            Theorem(ResultAction(UpdateResult(ResultFail(Timeout)))),
-          )
-        | _ => (),
-        List.map(((pos, _)) => pos, worker_request^),
-      )
-    });
+          | "theorem" =>
+            schedule_action(
+              Theorem(ResultAction(MergeStreamingEval(stream))),
+            )
+          | _ => ()
+          },
+    );
 
     {
       ...model,
```

</details>

## Tests

The evaluator test suite is updated to the new API and gains coverage for the streaming/yielding paths. The `reused`-list assertions become explicit reuse-plan checks (since that list is gone), and there are new tests for the yielding runner, the streamed outbox, current-state collection, and reuse-plan shape. Most remaining test edits are mechanical (`Completed`→`LimitedCompleted`, `Sample.call_stack`→`CallStack.t`, wrapping eval calls in the new `EvalInfo` record) and are grouped under Miscellaneous.

<details open>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · * final EvaluatorState, and resulting IncrEval.t (for test-…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -40,14 +41,18 @@ let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
 let statics_of = (exp: Exp.t): Statics.Map.t => fst(statics_and_elab(exp));
 
 /* Run the incremental evaluator end-to-end, returning the evaluated Exp.t,
- * final EvaluatorState, and resulting IncrEval.t (for test-readability we
+ * final EvaluatorState, and resulting incr_eval map (for test-readability we
  * surface the incr map separately even though it also lives in state). */
 let eval_incr =
-    (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t)
-    : (Exp.t, EvaluatorState.t, IncrEval.t) => {
+    (~prev: EvaluatorState.incr_eval=IncrEval.empty, exp: Exp.t)
+    : (Exp.t, EvaluatorState.t, EvaluatorState.incr_eval) => {
   let (info_map, elab) = statics_and_elab(exp);
   let info_map =
-    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
+    EvalInfo.of_info_map(
+      ~probe_all=CoreSettings.on.probe_all,
+      ~targets=Id.Map.empty,
+      info_map,
+    );
   let (result, state) =
     Evaluator.evaluate(~prev, ~info_map, ~env=Builtins.env_init, elab);
   (result, state, state.incr_eval);
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 highlights=new:171-310 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -141,16 +171,269 @@ let test_reuse_same_program = () => {
   let src = "let x = 1 + 2 in let y = x + 10 in y";
   let exp = parse_exp(src);
   let (r1, _, incr1) = eval_incr(exp);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp);
   check(dhexp_typ, "Reuse preserves the result value", r1, r2);
   check(
     bool,
     "Second run actually reused entries (reused list non-empty)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp)),
+  );
+};
+
+let test_streaming_plan_reuses_root_without_descending = () => {
+  let src = "let x = 1 + 2 in let y = x + 10 in y";
+  let exp = parse_exp(src);
+  let (_, _, prev) = eval_incr(exp);
+  let (info_map, elab) = statics_and_elab(exp);
+  let info_map = eval_info_of_statics(info_map);
+  let stream =
+    ReusePass.reuse_pass(~prev, ~info_map, ~env=Builtins.env_init, elab);
+  let root_id = Exp.rep_id(elab);
+  check(
+    bool,
+    "streaming plan records reusable root",
+    true,
+    Id.Map.mem(root_id, stream.entries),
+  );
+  check(
+    int,
+    "streaming plan does not descend under reusable root",
+    1,
+    Id.Map.cardinal(stream.entries),
+  );
+};
+
+let test_streaming_plan_omits_misses_and_keeps_children = () => {
+  let src = "let x = 1 + 2 in let y = x + 10 in let z = y + 100 in z";
+  let exp1 = parse_exp(src);
+  let exp2 = replace_int_lit(~from=100, ~to_=200, exp1);
+  let (_, _, prev) = eval_incr(exp1);
+  let (info_map, elab) = statics_and_elab(exp2);
+  let info_map = eval_info_of_statics(info_map);
+  let stream =
+    ReusePass.reuse_pass(~prev, ~info_map, ~env=Builtins.env_init, elab);
+  let root_id = Exp.rep_id(elab);
+  check(
+    bool,
+    "streaming plan omits root cache miss",
+    false,
+    Id.Map.mem(root_id, stream.entries),
+  );
+  check(
+    bool,
+    "streaming plan still records reusable descendants",
+    true,
+    !Id.Map.is_empty(stream.entries),
+  );
+};
+
+let test_materialize_stream_state_marks_reused_entries = () => {
+  let src = "let x = 1 + 2 in let y = x + 10 in y";
+  let exp = parse_exp(src);
+  let (_, _, prev) = eval_incr(exp);
+  let (info_map, elab) = statics_and_elab(exp);
+  let info_map = eval_info_of_statics(info_map);
+  let stream =
+    ReusePass.reuse_pass(~prev, ~info_map, ~env=Builtins.env_init, elab);
+  let state =
+    StreamCollector.collect_stream_state(
+      IncrEval.outbox_of_completed(stream),
+      elab,
+    );
+  let root_id = Exp.rep_id(elab);
+  check(
+    bool,
+    "materialized stream preserves root entry",
+    true,
+    Id.Map.mem(root_id, state.incr_eval.entries),
+  );
+  check(
+    int,
+    "materialized stream preserves entry count",
+    Id.Map.cardinal(stream.entries),
+    Id.Map.cardinal(state.incr_eval.entries),
   );
 };
 
+let probe_targets = (z: Zipper.t, info_map: Statics.Map.t): Sample.targets => {
+  let probe_ids =
+    Id.Map.union(
+      (_, _, _) => Some(),
+      Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
+      Id.Map.map(_ => (), z.refractors.multis.ephemerals),
+    );
+  Id.Map.fold(
+    (id, (), acc) => {
+      let refs =
+        switch (Statics.Map.lookup_exp(id, info_map)) {
+        | Some(_) => Statics.Map.refs_in(info_map, id)
+        | None =>
+          switch (Statics.Map.lookup_pat(id, info_map)) {
+          | Some(_) => Statics.Map.bound_in(info_map, id)
+          | None => []
+          }
+        };
+      let spec: Sample.capture_spec = {refs: refs};
+      Id.Map.add(id, spec, acc);
+    },
+    probe_ids,
+    Id.Map.empty,
+  );
+};
+
+let eval_with_targets =
+    (~prev: EvaluatorState.incr_eval=IncrEval.empty, ~term, ~targets, ()) => {
+  let (info_map, elab) = statics_and_elab(term);
+  let info_map =
+    EvalInfo.of_info_map(
+      ~probe_all=CoreSettings.on.probe_all,
+      ~targets,
+      info_map,
+    );
+  Evaluator.evaluate(~prev, ~info_map, ~env=Builtins.env_init, elab);
+};
+
+let replace_first_u_plus_one = (exp: Exp.t): Exp.t => {
+  let changed = ref(false);
+  let f_exp = (continue, e: Exp.t): Exp.t =>
+    if (changed^) {
+      continue(e);
+    } else {
+      switch (e.term) {
+      | BinOp(Operators.Int(Operators.Plus), lhs, rhs) =>
+        switch (lhs.term, rhs.term) {
+        | (Var("u"), Atom(Int(n))) when Bigint.to_string(n) == "1" =>
+          changed := true;
+          lhs;
+        | _ => continue(e)
+        }
+      | _ => continue(e)
+      };
+    };
+  TermBase.Exp.map_term(~f_exp, exp);
+};
+
+let replace_first_int_lit = (~from: int, ~to_: int, exp: Exp.t): Exp.t => {
+  let changed = ref(false);
+  let f_exp = (continue, e: Exp.t): Exp.t =>
+    if (changed^) {
+      continue(e);
+    } else {
+      switch (e.term) {
+      | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) =>
+        changed := true;
+        {
+          ...e,
+          term: Atom(Int(Bigint.of_int(to_))),
+        };
+      | _ => continue(e)
+      };
+    };
+  TermBase.Exp.map_term(~f_exp, exp);
+};
+
+let test_reused_probe_samples_survive_final_incremental_result = () => {
+  let src = {|let u = 15 in
+let fib = fun x ->
+  if x < 2 then 1 else fib(x-1) + fib(x-2) in
+let x = ^^probe(fib(u)) in
+print(x);
+let y = ^^probe(fib(u+1)) in
+test y == 2582 end;
+let z = fib(u) in
+print(z);
+let w = ^^probe(fib(u+2)) in
+let u = ^^probe(fib(u+3)) in
+(x,y,z,w)|};
+  switch (Parser.to_zipper(~root=Exp, src)) {
+  | None => failwith("could not parse probe regression program")
+  | Some(z) =>
+    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
+    let (info_map1, _) = statics_and_elab(term);
+    let targets1 = probe_targets(z, info_map1);
+    let (_, state1) = eval_with_targets(~term, ~targets=targets1, ());
+    let edited = replace_first_u_plus_one(term);
+    let (info_map2, _) = statics_and_elab(edited);
+    let targets2 = probe_targets(z, info_map2);
+    let (_, fresh_state2) =
+      eval_with_targets(~term=edited, ~targets=targets2, ());
+    let (_, incr_state2) =
+      eval_with_targets(
+        ~prev=state1.incr_eval,
+        ~term=edited,
+        ~targets=targets2,
+        (),
+      );
+    check(
+      int,
+      "incremental final result preserves reused probe samples",
+      Id.Map.cardinal(EvaluatorState.get_probes(fresh_state2)),
+      Id.Map.cardinal(EvaluatorState.get_probes(incr_state2)),
+    );
+    check(
+      bool,
+      "each current probe target has a sample after final incremental result",
+      true,
+      Id.Map.for_all(
+        (id, _) => Id.Map.mem(id, EvaluatorState.get_probes(incr_state2)),
+        targets2,
+      ),
+    );
+  };
+};
+
+let test_test_literal_edit_preserves_downstream_reused_probes = () => {
+  let src = {|let u = 16 in
+let fib' = fun x ->
+  if x < 2 then 1 else fib'(x-1) + fib'(x-2) in
+let fib = fun ^^probe(x) -> fib'(x) in
+let x = ^^probe(fib(u)) in
+print(x);
+let y = ^^probe(fib(u+1)) in
+test y == 2584 end;
+let z = fib(u) in
+print(z);
+let w = ^^probe(fib(u-2)) in
+let u = ^^probe(fib(u+1)) in
+(x,y,z,w)|};
+  switch (Parser.to_zipper(~root=Exp, src)) {
+  | None => failwith("could not parse test-literal probe regression program")
+  | Some(z) =>
+    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
+    let (info_map1, _) = statics_and_elab(term);
+    let targets1 = probe_targets(z, info_map1);
+    let (_, state1) = eval_with_targets(~term, ~targets=targets1, ());
+    let edited = replace_first_int_lit(~from=2584, ~to_=2585, term);
+    let (info_map2, _) = statics_and_elab(edited);
+    let targets2 = probe_targets(z, info_map2);
+    let (_, fresh_state2) =
+      eval_with_targets(~term=edited, ~targets=targets2, ());
+    let (_, incr_state2) =
+      eval_with_targets(
+        ~prev=state1.incr_eval,
+        ~term=edited,
+        ~targets=targets2,
+        (),
+      );
+    check(
+      int,
+      "test literal edit preserves probe sample count",
+      Id.Map.cardinal(EvaluatorState.get_probes(fresh_state2)),
+      Id.Map.cardinal(EvaluatorState.get_probes(incr_state2)),
+    );
+    check(
+      bool,
+      "each current probe target has samples after test literal edit",
+      true,
+      Id.Map.for_all(
+        (id, _) => Id.Map.mem(id, EvaluatorState.get_probes(incr_state2)),
+        targets2,
+      ),
+    );
+  };
+};
+
 /* Non-deferred subtrees below the outer let should ALSO get cache entries.
  * Bug this pins: treating every Closure wrapper as a deferred boundary
  * causes let-bodies to be excluded from caching, leaving only the
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · test_case(</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1382,6 +1741,31 @@ let tests = (
       `Quick,
       test_reuse_same_program,
     ),
+    test_case(
+      "Streaming plan reuses root without descending",
+      `Quick,
+      test_streaming_plan_reuses_root_without_descending,
+    ),
+    test_case(
+      "Streaming plan omits misses and keeps children",
+      `Quick,
+      test_streaming_plan_omits_misses_and_keeps_children,
+    ),
+    test_case(
+      "Materialize streaming state marks reused entries",
+      `Quick,
+      test_materialize_stream_state_marks_reused_entries,
+    ),
+    test_case(
+      "Reused probe samples survive final incremental result",
+      `Quick,
+      test_reused_probe_samples_survive_final_incremental_result,
+    ),
+    test_case(
+      "Test literal edit preserves downstream reused probes",
+      `Quick,
+      test_test_literal_edit_preserves_downstream_reused_probes,
+    ),
     test_case(
       "Nested let-bodies below outer let populate cache entries",
       `Quick,
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · test_case(</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1412,6 +1796,11 @@ let tests = (
       `Quick,
       test_function_arg_edit_reuses_other_calls,
     ),
+    test_case(
+      "Function: seq edit before call reuses f(20)",
+      `Quick,
+      test_seq_edit_before_function_call_reuses_call,
+    ),
     test_case(
       "If: untaken-branch edit preserves result and reuses",
       `Quick,
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Prelude.re</code> · let step_limited = (t: Alcotest.testable('a)) =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Prelude.re level=2 baseBlob=391e3a4d14e8f0ad7e1d508debdb19001c445780 -->

```diff
@@ -140,10 +140,19 @@ let parse_and_evaluate_test =
     elaborate(parse_exp(actual)),
   );
 
-let step_limited = (t: Alcotest.testable('a)) =>
+let equal_limited_result =
+    (lr1: Evaluator.limited_result, lr2: Evaluator.limited_result) =>
+  switch (lr1, lr2) {
+  | (LimitedCompleted((exp1, _)), LimitedCompleted((exp2, _))) =>
+    Exp.equal(exp1, exp2)
+  | (StepLimitExceeded, StepLimitExceeded) => true
+  | _ => false
+  };
+
+let step_limited = (_: Alcotest.testable('a)) =>
   testable(
-    Fmt.using(Evaluator.show_step_constrained(pp(t)), Fmt.string),
-    Evaluator.equal_step_constrained(equal(t)),
+    Fmt.using(Evaluator.show_limited_result, Fmt.string),
+    equal_limited_result,
   );
 let single_step = (exp: Exp.t) => {
   let step =
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Prelude.re</code> · | Some(new_exp) =&gt; Completed(new_exp)</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Prelude.re level=2 baseBlob=391e3a4d14e8f0ad7e1d508debdb19001c445780 -->

```diff
@@ -174,7 +182,7 @@ let full_small_step_reduction =
 
   switch (go(~steps_counter=0, exp)) {
   | None => StepLimitExceeded
-  | Some(new_exp) => Completed(new_exp)
+  | Some(new_exp) => LimitedCompleted((new_exp, EvaluatorState.empty))
   };
 };
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · let rec finish_yielding = (~remaining_slices: int, evaluati…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 highlights=new:385-470 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -377,9 +385,217 @@ let qcheck_incremental_matches_fresh_after_edit =
     },
   );
 
+let rec finish_yielding = (~remaining_slices: int, evaluation) => {
+  if (remaining_slices <= 0) {
+    fail("Yielding evaluation did not complete");
+  };
+  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
+  | EvaluationCompleted(value) => value
+  | EvaluationYielded(evaluation) =>
+    finish_yielding(~remaining_slices=remaining_slices - 1, evaluation)
+  };
+};
+
+let rec finish_yielding_with_stream =
+        (~remaining_slices: int, ~stream, evaluation) => {
+  if (remaining_slices <= 0) {
+    fail("Yielding evaluation did not complete");
+  };
+  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
+  | EvaluationCompleted(value) =>
+    let stream =
+      IncrEval.add_stream(
+        Evaluator.drain_streaming_outbox(evaluation).completed,
+        stream,
+      );
+    (value, stream);
+  | EvaluationYielded(evaluation) =>
+    let stream =
+      IncrEval.add_stream(
+        Evaluator.drain_streaming_outbox(evaluation).completed,
+        stream,
+      );
+    finish_yielding_with_stream(
+      ~remaining_slices=remaining_slices - 1,
+      ~stream,
+      evaluation,
+    );
+  };
+};
+
+let finish_yielding_with_stream =
+    (~remaining_slices: int, ~stream, evaluation) => {
+  if (remaining_slices <= 0) {
+    fail("Yielding evaluation did not complete");
+  };
+  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
+  | EvaluationCompleted(value) =>
+    let stream =
+      IncrEval.add_stream(
+        Evaluator.drain_streaming_outbox(evaluation).completed,
+        stream,
+      );
+    (value, stream);
+  | EvaluationYielded(evaluation) =>
+    let stream =
+      IncrEval.add_stream(
+        Evaluator.drain_streaming_outbox(evaluation).completed,
+        stream,
+      );
+    finish_yielding_with_stream(
+      ~remaining_slices=remaining_slices - 1,
+      ~stream,
+      evaluation,
+    );
+  };
+};
+
+let yielding_evaluation_test =
+  test_case(
+    "Yielding evaluation resumes to the synchronous result",
+    `Quick,
+    () => {
+      let (_, exp) =
+        Statics.mk(
+          CoreSettings.on,
+          Builtins.ctx_init(Some(Int)),
+          parse_exp("let x = 1 in let y = 2 in x + y"),
+        );
+      let (sync_exp, _) = Evaluator.evaluate(~env=Builtins.env_init, exp);
+      let evaluation =
+        Evaluator.start_yielding_evaluation(~env=Builtins.env_init, exp);
+      let evaluation =
+        switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
+        | EvaluationYielded(evaluation) => evaluation
+        | EvaluationCompleted(_) =>
+          fail("Expected yielding evaluation to yield with a one-step budget")
+        };
+      let (yielded_exp, _) =
+        finish_yielding(~remaining_slices=1000, evaluation);
+      check(dhexp_typ, "yielding evaluation result", sync_exp, yielded_exp);
+    },
+  );
+
+let yielding_streaming_outbox_test =
+  test_case(
+    "Yielding evaluation streams completed incremental entries",
+    `Quick,
+    () => {
+      let (info_map, exp) =
+        Statics.mk(
+          CoreSettings.on,
+          Builtins.ctx_init(Some(Int)),
+          parse_exp("let x = 1 in let y = x + 2 in y"),
+        );
+      let info_map =
+        EvalInfo.of_info_map(
+          ~probe_all=CoreSettings.on.probe_all,
+          ~targets=Id.Map.empty,
+          info_map,
+        );
+      let evaluation =
+        Evaluator.start_yielding_evaluation(
+          ~info_map,
+          ~env=Builtins.env_init,
+          exp,
+        );
+      let ((_, final_state), stream) =
+        finish_yielding_with_stream(
+          ~remaining_slices=1000,
+          ~stream=IncrEval.empty,
+          evaluation,
+        );
+      check(
+        int,
+        "streamed entry count matches final entries",
+        Id.Map.cardinal(final_state.incr_eval.entries),
+        Id.Map.cardinal(stream.entries),
+      );
+      check(
+        bool,
+        "every streamed id appears in final entries",
+        true,
+        Id.Map.for_all(
+          (id, _) => Id.Map.mem(id, final_state.incr_eval.entries),
+          stream.entries,
+        ),
+      );
+    },
+  );
+
+let rec yield_until_current = (~remaining_slices: int, evaluation) => {
+  if (remaining_slices <= 0) {
+    fail("Yielding evaluation did not produce a current outbox state");
+  };
+  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
+  | EvaluationCompleted(_) =>
+    fail("Expected yielding evaluation to yield before completion")
+  | EvaluationYielded(evaluation) =>
+    let outbox = Evaluator.drain_streaming_outbox(evaluation);
+    switch (outbox.current) {
+    | Some(_) => outbox
+    | None =>
+      yield_until_current(~remaining_slices=remaining_slices - 1, evaluation)
+    };
+  };
+};
+
+let yielding_streaming_current_state_test =
+  test_case(
+    "Yielding evaluation streams current partial state",
+    `Quick,
+    () => {
+      let (info_map, exp) =
+        Statics.mk(
+          CoreSettings.on,
+          Builtins.ctx_init(Some(Int)),
+          parse_exp("let x = 1 + 2 in let y = x + 3 in y"),
+        );
+      let info_map =
+        EvalInfo.of_info_map(
+          ~probe_all=CoreSettings.on.probe_all,
+          ~targets=Id.Map.empty,
+          info_map,
+        );
+      let evaluation =
+        Evaluator.start_yielding_evaluation(
+          ~info_map,
+          ~env=Builtins.env_init,
+          exp,
+        );
+      let outbox = yield_until_current(~remaining_slices=1000, evaluation);
+      switch (outbox.current) {
+      | Some({state, _}) =>
+        let collected = StreamCollector.collect_stream_state(outbox, exp);
+        check(
+          bool,
+          "current state has dynamic work",
+          true,
+          state.step_count > 0,
+        );
+        check(
+          bool,
+          "collector includes current state",
+          true,
+          collected.step_count >= state.step_count,
+        );
+        check(
+          bool,
+          "current state does not recursively carry incr_eval",
+          true,
+          Id.Map.is_empty(state.incr_eval.entries),
+        );
+      | None => fail("Expected current outbox state")
+      };
+    },
+  );
+
 let tests = (
   "Evaluator.Properties",
   [
+    yielding_evaluation_test,
+    yielding_streaming_outbox_test,
+    yielding_streaming_current_state_test,
     QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
     QCheck_alcotest.to_alcotest(qcheck_stepper_confluence),
     QCheck_alcotest.to_alcotest(qcheck_pattern_equivalence_test),
```

</details>

<details open>
<summary><code>test/evaluator/Test_Evaluator_ProbeCallStack.re</code> · Evaluator.evaluate(~targets, ~env=Builtins.env_init, elabor…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeCallStack.re level=2 baseBlob=44addfc36a7b97d806a012c24c6a98f0a670e808 -->

```diff
@@ -19,22 +19,30 @@ open Test_Evaluator_Prelude;
 let get_all_samples = (code: string): list(Sample.t) => {
   let (_term, elaborated, _info_map, targets) = parse_with_probes(code);
   let (_, state) =
-    Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+    Evaluator.evaluate(
+      ~info_map=
+        EvalInfo.{
+          statics: Id.Map.empty,
+          targets,
+        },
+      ~env=Builtins.env_init,
+      elaborated,
+    );
   let probes = EvaluatorState.get_probes(state);
   Id.Map.bindings(probes) |> List.concat_map(snd);
 };
 
 /* Show call stack for debugging */
-let show_call_stack = (cs: Sample.call_stack): string =>
+let show_call_stack = (cs: CallStack.t): string =>
   "["
   ++ String.concat(
        ", ",
-       List.map((f: Sample.stack_frame) => Id.str3(f.id), cs),
+       List.map((f: CallStack.frame) => Id.str3(f.id), cs),
      )
   ++ "]";
 
 let call_stack_testable =
-  testable(Fmt.using(show_call_stack, Fmt.string), Sample.equal_call_stack);
+  testable(Fmt.using(show_call_stack, Fmt.string), CallStack.equal);
 
 /* Test that multiple top-level probed applications have the same (empty) call_stack.
  * This is the bug: if they have different call_stacks (containing their own app_ids),
```

</details>

## Miscellaneous

The remaining hunks are mechanical ripple from the changes above: the `Sample.call_stack`/`Sample.stack_frame` → `CallStack.t`/`CallStack.frame` rename threaded through `haz3lcore` and the probe UI, `Completed` → `LimitedCompleted` at the CLI/grading call sites, `ResultPending` → `ResultPending(_)` matches, the `generated(...)` applications in `Transition`, and the analogous API-shape updates across the remaining evaluator tests. None change behavior on their own.

<details>
<summary><code>src/CLI/Grade.re</code> · | Completed((_, evaluated)) =&gt;</summary>

<!-- changetour:hunk file=src/CLI/Grade.re level=2 baseBlob=445c99dcd602d4c94c30e9ba4642dceca458570c -->

```diff
@@ -77,7 +77,7 @@ let gen_code_grading_report = (exercise): report => {
              );
         switch (evaluated) {
         | StepLimitExceeded => None
-        | Completed((_, evaluated)) =>
+        | LimitedCompleted((_, evaluated)) =>
           evaluated
           |> EvaluatorState.get_tests
           |> TestResults.mk_results
```

</details>

<details>
<summary><code>src/CLI/Run.re</code> · (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t): (Exp.t, Inc…</summary>

<!-- changetour:hunk file=src/CLI/Run.re level=2 baseBlob=e6f4dd212a1082d226dbe692da9c496b23ea7f4d -->

```diff
@@ -19,10 +19,15 @@ let evaluate = (exp: Exp.t): Exp.t => {
 };
 
 let evaluate_incremental =
-    (~prev: IncrEval.t=IncrEval.empty, exp: Exp.t): (Exp.t, IncrEval.t) => {
+    (~prev: EvaluatorState.incr_eval=IncrEval.empty, exp: Exp.t)
+    : (Exp.t, EvaluatorState.incr_eval) => {
   let (info_map, elab) = statics_and_elab(exp);
   let info_map =
-    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
+    EvalInfo.of_info_map(
+      ~probe_all=CoreSettings.on.probe_all,
+      ~targets=Id.Map.empty,
+      info_map,
+    );
   let (result, state) =
     Evaluator.evaluate(~prev, ~info_map, ~env=Builtins.env_init, elab);
   (result, state.incr_eval);
```

</details>

<details>
<summary><code>src/CLI/Run.re</code> · ~targets=sample_map,</summary>

<!-- changetour:hunk file=src/CLI/Run.re level=2 baseBlob=e6f4dd212a1082d226dbe692da9c496b23ea7f4d -->

```diff
@@ -43,7 +48,11 @@ let evaluate_with_probe_map =
   let elaborated = elaborate(exp);
   let (result, state) =
     Evaluator.evaluate(
-      ~targets=sample_map,
+      ~info_map=
+        EvalInfo.{
+          statics: Id.Map.empty,
+          targets: sample_map,
+        },
       ~env=Builtins.env_init,
       elaborated,
     );
```

</details>

<details>
<summary><code>src/haz3lcore/ProbePerform.re</code> · ~call_stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=src/haz3lcore/ProbePerform.re level=2 baseBlob=22c491c2ac37d31b01b482337ad7fbe9e0553d00 -->

```diff
@@ -734,7 +734,7 @@ let is_jump_target = (info_map: Statics.Map.t, z: Zipper.t): option(Id.t) => {
 let step_into_call_stack =
     (
       ~syntax: CachedSyntax.t,
-      ~call_stack: Sample.call_stack,
+      ~call_stack: CallStack.t,
       ~ap_id: Id.t,
       info_map: Statics.Map.t,
       z: Zipper.t,
```

</details>

<details>
<summary><code>src/haz3lcore/ProbePerform.re</code> · let new_stack: Sample.call_stack = [</summary>

<!-- changetour:hunk file=src/haz3lcore/ProbePerform.re level=2 baseBlob=22c491c2ac37d31b01b482337ad7fbe9e0553d00 -->

```diff
@@ -780,7 +780,7 @@ let step_into_call_stack =
     };
 
   /* Set pin and dyn cursor using the call_stack */
-  let new_stack: Sample.call_stack = [
+  let new_stack: CallStack.t = [
     {
       id: ap_id,
       name: None,
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/ProbeProj.re</code> · let stack = (stack: Sample.call_stack): string =&gt;</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/ProbeProj.re level=2 baseBlob=33c1fe3c27a9f2ea109db517060d68468aaa89f9 -->

```diff
@@ -388,9 +388,9 @@ let cursor_clss =
 };
 
 module Debug = {
-  let stack = (stack: Sample.call_stack): string =>
+  let stack = (stack: CallStack.t): string =>
     stack
-    |> List.map((f: Sample.stack_frame) => Id.str3(f.id))
+    |> List.map((f: CallStack.frame) => Id.str3(f.id))
     |> String.concat("\n");
 
   let str = (~ap_id: option(Id.t), sample: Sample.t): string =>
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/ProbeProj.re</code> · {</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/ProbeProj.re level=2 baseBlob=33c1fe3c27a9f2ea109db517060d68468aaa89f9 -->

```diff
@@ -421,8 +421,8 @@ let pin_call = (ctx: probe_ctx) =>
   switch (ctx.ap_id, Dynamics.Info.is_in(ctx.dynamics)) {
   | (Some(ap_id), Some(sample)) =>
     let call_stack = [
-      {
-        Sample.id: ap_id,
+      CallStack.{
+        id: ap_id,
         name: None,
         fn_def_id: None,
       },
```

</details>

<details>
<summary><code>src/haz3lcore/projectors/implementations/ProbeProj.re</code> · Sample.ids_of_stack(pinned_stack)</summary>

<!-- changetour:hunk file=src/haz3lcore/projectors/implementations/ProbeProj.re level=2 baseBlob=33c1fe3c27a9f2ea109db517060d68468aaa89f9 -->

```diff
@@ -563,17 +563,17 @@ let show_pin = (ctx: probe_ctx, sample: Sample.t) => {
   switch (ctx.ap_id, ctx.dynamics.sample_focus.pinned_stack) {
   | (Some(ap_id), Some(pinned_stack)) =>
     /* Compare by ID only - function names may differ */
-    Sample.ids_of_stack(pinned_stack)
-    == [ap_id, ...Sample.ids_of_stack(sample.call_stack)]
+    CallStack.ids_of_stack(pinned_stack)
+    == [ap_id, ...CallStack.ids_of_stack(sample.call_stack)]
   | _ => false
   };
 };
 
 let show_focus = (ctx: probe_ctx, sample: Sample.t) =>
   switch (ctx.dynamics.sample_focus.pinned_stack) {
   | Some(pinned_stack) =>
-    Sample.ids_of_stack(pinned_stack)
-    == Sample.ids_of_stack(sample.call_stack)
+    CallStack.ids_of_stack(pinned_stack)
+    == CallStack.ids_of_stack(sample.call_stack)
   | _ => false
   };
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/Action.re</code> · | TogglePin(Language.Sample.call_stack)</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/Action.re level=2 baseBlob=cc8d42496303e2aaee510ce6b375c35d5908071c -->

```diff
@@ -55,7 +55,7 @@ type select =
 [@deriving (show({with_path: false}), sexp, yojson, eq)]
 type sample_focus =
   | Capture(Language.Sample.Capture.t, option(Id.t))
-  | TogglePin(Language.Sample.call_stack)
+  | TogglePin(Language.CallStack.t)
   | SetIndex(int) /* Navigate to a specific depth in the call stack */
   | Reset;
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/Action.re</code> · | StepInto(Language.Sample.call_stack, Id.t)</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/Action.re level=2 baseBlob=cc8d42496303e2aaee510ce6b375c35d5908071c -->

```diff
@@ -130,8 +130,8 @@ type probe =
   | ToggleManual
   | ToggleAuto
   | ToggleStatics
-  | StepInto(Language.Sample.call_stack, Id.t)
-  | Pin(Language.Sample.call_stack, Id.t)
+  | StepInto(Language.CallStack.t, Id.t)
+  | Pin(Language.CallStack.t, Id.t)
   | RemoveAll;
 
 [@deriving (show({with_path: false}), sexp, yojson, eq)]
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/SampleFocusPerform.re</code> · (z: Zipper.t, f: option(Sample.call_stack) =&gt; option(Sample…</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/SampleFocusPerform.re level=2 baseBlob=47b433f9fbb8674ee075a687949fab30d50b2829 -->

```diff
@@ -10,7 +10,7 @@ let update = (z: Zipper.t, f: Sample.Focus.t => Sample.Focus.t) =>
   );
 
 let update_pinned_call =
-    (z: Zipper.t, f: option(Sample.call_stack) => option(Sample.call_stack)) =>
+    (z: Zipper.t, f: option(CallStack.t) => option(CallStack.t)) =>
   update(z, sample_focus =>
     {
       ...sample_focus,
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/SampleFocusPerform.re</code> · let extended: Sample.call_stack = [</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/SampleFocusPerform.re level=2 baseBlob=47b433f9fbb8674ee075a687949fab30d50b2829 -->

```diff
@@ -44,7 +44,7 @@ let capture = (z: Zipper.t, data: Sample.Capture.t, id): Zipper.t => {
              call_stack tracks the call we're looking at, not just the
              calls we're inside of. Index stays at the original depth,
              so this frame appears "below" (ghosted) in the breadcrumbs. */
-          let extended: Sample.call_stack = [
+          let extended: CallStack.t = [
             {
               id: ap_id,
               name: None,
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/SampleFocusPerform.re</code> · ~eq=Sample.equal_stack_frame,</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/SampleFocusPerform.re level=2 baseBlob=47b433f9fbb8674ee075a687949fab30d50b2829 -->

```diff
@@ -56,7 +56,7 @@ let capture = (z: Zipper.t, data: Sample.Capture.t, id): Zipper.t => {
         | None =>
           !
             ListUtil.is_suffix_of(
-              ~eq=Sample.equal_stack_frame,
+              ~eq=CallStack.equal_frame,
               data.call_stack,
               sample_focus.call_stack,
             )
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/SampleFocusPerform.re</code> · when Sample.ids_of_stack(call_stack) == Sample.ids_of_stack…</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/SampleFocusPerform.re level=2 baseBlob=47b433f9fbb8674ee075a687949fab30d50b2829 -->

```diff
@@ -73,7 +73,9 @@ let toggle_pin_call = (z: Zipper.t, call_stack): Zipper.t =>
     /* Compare by ID only - function names may differ */
     switch (pinned_call) {
     | Some(existing)
-        when Sample.ids_of_stack(call_stack) == Sample.ids_of_stack(existing) =>
+        when
+          CallStack.ids_of_stack(call_stack)
+          == CallStack.ids_of_stack(existing) =>
       None
     | _ => Some(call_stack)
     }
```

</details>

<details>
<summary><code>src/haz3lcore/zipper/action/SampleFocusPerform.re</code> · (z: Zipper.t, samples: list(Sample.t), target_stack: Sample…</summary>

<!-- changetour:hunk file=src/haz3lcore/zipper/action/SampleFocusPerform.re level=2 baseBlob=47b433f9fbb8674ee075a687949fab30d50b2829 -->

```diff
@@ -86,13 +88,13 @@ let reset = (z: Zipper.t): Zipper.t =>
    the sample that matches the target stack. Called from Probes
    after it looks up the samples from dynamics. */
 let resolve_pending_focus =
-    (z: Zipper.t, samples: list(Sample.t), target_stack: Sample.call_stack)
+    (z: Zipper.t, samples: list(Sample.t), target_stack: CallStack.t)
     : Zipper.t => {
   /* Compare by ID only - target_stack may have None for function names */
-  let target_ids = Sample.ids_of_stack(target_stack);
+  let target_ids = CallStack.ids_of_stack(target_stack);
   let matching_sample =
     List.find_opt(
-      (s: Sample.t) => Sample.ids_of_stack(s.call_stack) == target_ids,
+      (s: Sample.t) => CallStack.ids_of_stack(s.call_stack) == target_ids,
       samples,
     );
   switch (matching_sample) {
```

</details>

<details>
<summary><code>src/language/dynamics/Dynamics.re</code> · Sample.ids_of_stack(Sample.Focus.effective_stack(di.sample_…</summary>

<!-- changetour:hunk file=src/language/dynamics/Dynamics.re level=2 baseBlob=4b3c80f6e7f943a017630cc52cfa4dc3aab1ace3 -->

```diff
@@ -20,10 +20,10 @@ module Info = {
 
   let is_in = (di: t): option(Sample.t) => {
     let cursor_ids =
-      Sample.ids_of_stack(Sample.Focus.effective_stack(di.sample_focus));
+      CallStack.ids_of_stack(Sample.Focus.effective_stack(di.sample_focus));
     List.find_opt(
       (sample: Sample.t) =>
-        Sample.ids_of_stack(sample.call_stack) == cursor_ids,
+        CallStack.ids_of_stack(sample.call_stack) == cursor_ids,
       di.samples,
     );
   };
```

</details>

<details>
<summary><code>src/language/dynamics/EvalInfoMap.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/language/dynamics/EvalInfoMap.re level=2 baseBlob=fd7fd27fc257260f7b80ab23e94f79d9af2982a2 -->

```diff
@@ -1,31 +0,0 @@
-open Util;
-
-[@deriving (show({with_path: false}), sexp, yojson)]
-type entry = {
-  elab_term: Exp.t,
-  co_ctx: CoCtx.t,
-  /* See `prev_probe_targets` in IncrEval — None under `probe_all`. */
-  probe_targets: option(SubexpProbeTargets.t),
-};
-
-[@deriving (show({with_path: false}), sexp, yojson)]
-type t = Id.Map.t(entry);
-
-let empty: t = Id.Map.empty;
-
-let find_opt = Id.Map.find_opt;
-
-let of_info_map = (~probe_all: bool, info_map: StaticsBase.Map.t): t =>
-  Id.Map.filter_map(
-    (_id, info) =>
-      switch (info) {
-      | Info.InfoExp({elab_term, co_ctx, probe_targets, _}) =>
-        Some({
-          elab_term,
-          co_ctx,
-          probe_targets: probe_all ? None : Some(probe_targets),
-        })
-      | _ => None
-      },
-    info_map,
-  );
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · type elided_value =</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -66,9 +38,7 @@ module Env = {
    * such as closures. Which values are made opaque can be modulated
    * via the below `elide` function */
   [@deriving (show({with_path: false}), sexp, yojson, eq)]
-  type elided_value =
-    | Opaque
-    | Val(DHExp.t);
+  type elided_value = CallStack.elided_value;
 
   /* A probe environment entry is a variable binding
    * along with its corresponding elided value */
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · let elide = (env: Environment.t(Exp.t), d: DHExp.t) =&gt;</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -88,7 +58,7 @@ module Env = {
   /* Selectively elide dynamic information not currently
    * being used in the live probe UI, for (putative, unbenchmarked)
    * performance purposes for worker de/serialization */
-  let elide = (env: Environment.t(Exp.t), d: DHExp.t) =>
+  let elide = (env: Environment.t(Exp.t), d: DHExp.t): elided_value =>
     switch ((d |> DHExp.strip_ascriptions).term) {
     | Fun(_)
     | FixF(_)
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · call_stack, /* Call stacks as ap ids */</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -136,7 +106,7 @@ type t = {
   syntax_id: Id.t, /* Syntax ID of probed expression */
   value: DHExp.t, /* Value of expression */
   env: Env.t, /* (Filtered) Environment Values  */
-  call_stack, /* Call stacks as ap ids */
+  call_stack: CallStack.t, /* Call stacks as ap ids */
   args: option(Env.elided_value), /* Argument value if probe is on an Ap */
   time: float, /* Time of evaluation */
   seq: int, /* Sequence number: a count index of each sample taken */
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · stack: call_stack,</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -156,7 +126,7 @@ let mk =
       syntax_id: Id.t,
       value: DHExp.t,
       env: Environment.t(Exp.t),
-      stack: call_stack,
+      stack: CallStack.t,
       spec: capture_spec,
     )
     : t => {
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · target_stack: call_stack /* The call stack to match */</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -300,7 +270,7 @@ module Focus = {
   [@deriving (show({with_path: false}), sexp, yojson, eq)]
   type pending_focus = {
     probe_id: Id.t, /* The probe we're stepping into */
-    target_stack: call_stack /* The call stack to match */
+    target_stack: CallStack.t /* The call stack to match */
   };
 
   /* Focus.t fields:
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · call_stack,</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -314,9 +284,9 @@ module Focus = {
    * - pending_focus: After step-into, where to focus when evaluation completes */
   [@deriving (show({with_path: false}), sexp, yojson, eq)]
   type t = {
-    call_stack,
+    call_stack: CallStack.t,
     index: int,
-    pinned_stack: option(call_stack),
+    pinned_stack: option(CallStack.t),
     indicated_call: option(Id.t),
     time: option(float),
     seq: int,
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · let effective_stack = (cursor: t): call_stack =&gt;</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -339,22 +309,22 @@ module Focus = {
    * index + 1 elements from the outer end. This is where you ARE —
    * the active position used for tier 1 alignment. The full call_stack
    * extends deeper (below-focus) for tier 2 alignment. */
-  let effective_stack = (cursor: t): call_stack =>
+  let effective_stack = (cursor: t): CallStack.t =>
     ListUtil.slice(0, cursor.index + 1, cursor.call_stack |> List.rev)
     |> List.rev;
 
   /* If the cursor is on a call, and the provided call stack is
    * downstream of that call, return how many aps downstream it is */
   let depth_in_indicated_calls_stack =
-      (cursor: t, call_stack: call_stack): option(int) => {
+      (cursor: t, call_stack: CallStack.t): option(int) => {
     let* cur_ap = cursor.indicated_call;
-    let cur_frame: stack_frame = {
+    let cur_frame: CallStack.frame = {
       id: cur_ap,
       name: None,
       fn_def_id: None,
     };
     ListUtil.suffix_at_depth(
-      ~eq=equal_stack_frame,
+      ~eq=CallStack.equal_frame,
       [cur_frame] @ effective_stack(cursor),
       call_stack,
     );
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · let is_below = ListUtil.suffix_at_depth(~eq=equal_stack_fra…</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -403,20 +373,20 @@ module Focus = {
     is_below_indicated_call: option(int),
   };
 
-  let is_below = ListUtil.suffix_at_depth(~eq=equal_stack_frame);
+  let is_below = ListUtil.suffix_at_depth(~eq=CallStack.equal_frame);
 
-  let relative_level = (cs1: call_stack, cs2: call_stack): relative_level =>
+  let relative_level = (cs1: CallStack.t, cs2: CallStack.t): relative_level =>
     switch (is_below(cs1, cs2), is_below(cs2, cs1)) {
     | (Some(0), Some(0)) => Same
     | (Some(n), None) => Below(n)
     | (None, Some(n)) => Above(n)
     | (_, _) => Unrelated
     };
 
-  let cur_call = (ap_id: option(Id.t), sample: sample): option(call_stack) => {
+  let cur_call = (ap_id: option(Id.t), sample: sample): option(CallStack.t) => {
     let* ap_id = ap_id;
     Some([
-      {
+      CallStack.{
         id: ap_id,
         name: None,
         fn_def_id: None,
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · is_call_cursor: equal_call_stack(cursor_stack, this),</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -441,7 +411,7 @@ module Focus = {
     let this = sample.call_stack;
     let cursor_stack = trimmed ? effective_stack(cursor) : cursor.call_stack;
     {
-      is_call_cursor: equal_call_stack(cursor_stack, this),
+      is_call_cursor: CallStack.equal(cursor_stack, this),
       is_more_precise_than_cursor:
         List.length(cursor.call_stack) > List.length(sample.call_stack),
       relative_level_to_cursor: relative_level(cursor_stack, this),
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · {</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -453,7 +423,7 @@ module Focus = {
         let* cur_ap = cursor.indicated_call;
         is_below(
           [
-            {
+            CallStack.{
               id: cur_ap,
               name: None,
               fn_def_id: None,
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · (~ap_id: option(Id.t), ~pinned: option(call_stack), samples…</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -504,20 +474,27 @@ module Selection = {
   /* Filter samples by pinned call stack.
    * Print-origin samples are excluded — they are only for the Printarium. */
   let filter_by_pin =
-      (~ap_id: option(Id.t), ~pinned: option(call_stack), samples: list(t))
+      (
+        ~ap_id: option(Id.t),
+        ~pinned: option(CallStack.t),
+        samples: list(t),
+      )
       : list(t) => {
     let samples = List.filter((s: t) => s.origin != Print, samples);
     switch (pinned) {
     | Some(pinned_stack) =>
       /* Extract just the Id.t from head of pinned_stack for comparison */
       let pinned_head_id =
-        Option.map((f: stack_frame) => f.id, ListUtil.hd_opt(pinned_stack));
+        Option.map(
+          (f: CallStack.frame) => f.id,
+          ListUtil.hd_opt(pinned_stack),
+        );
       /* Compare by ID only - pinned_stack may have None for function names
        * but actual samples have real names from evaluation */
-      let pinned_ids = ids_of_stack(pinned_stack);
+      let pinned_ids = CallStack.ids_of_stack(pinned_stack);
       List.filter(
         (sample: t) => {
-          let sample_ids = ids_of_stack(sample.call_stack);
+          let sample_ids = CallStack.ids_of_stack(sample.call_stack);
           pinned_head_id == ap_id
           /* Sample is at or below pin (current behavior) */
           || ListUtil.is_suffix_of(pinned_ids, sample_ids)
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · let suffix_scan = (stack: call_stack): option(int) =&gt;</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -560,14 +537,14 @@ module Selection = {
   let most_aligned_index =
       (~ap_id: option(Id.t), cursor: Focus.t, samples: list(t))
       : option(int) => {
-    let suffix_scan = (stack: call_stack): option(int) =>
+    let suffix_scan = (stack: CallStack.t): option(int) =>
       List.fold_left(
         (best: option((int, int)), (i, sample: t)) => {
           let slen = List.length(sample.call_stack);
           if (slen > 0
               && slen > (best |> Option.map(snd) |> Option.value(~default=0))
               && ListUtil.is_suffix_of(
-                   ~eq=equal_stack_frame,
+                   ~eq=CallStack.equal_frame,
                    sample.call_stack,
                    stack,
                  )) {
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · | ([f1, ..._], [f2, ..._]) =&gt; equal_stack_frame(f1, f2)</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -633,7 +610,7 @@ module Selection = {
     switch (List.rev(s2.call_stack), List.rev(s1.call_stack)) {
     | ([], _)
     | (_, []) => false
-    | ([f1, ..._], [f2, ..._]) => equal_stack_frame(f1, f2)
+    | ([f1, ..._], [f2, ..._]) => CallStack.equal_frame(f1, f2)
     };
 
   /* Group samples by function call, with indices */
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · ~pinned: option(call_stack),</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -660,7 +637,7 @@ module Selection = {
         ~mode: Window.mode,
         ~offset: int,
         ~ap_id: option(Id.t),
-        ~pinned: option(call_stack),
+        ~pinned: option(CallStack.t),
         ~cursor: Focus.t,
         samples: list(t),
       )
```

</details>

<details>
<summary><code>src/language/dynamics/Sample.re</code> · call_stack,</summary>

<!-- changetour:hunk file=src/language/dynamics/Sample.re level=2 baseBlob=9abb0a9ef50de632ebffe797d7ffd0e17b10e257 -->

```diff
@@ -697,7 +674,7 @@ module Capture = {
   type t = {
     time: float,
     seq: int,
-    call_stack,
+    call_stack: CallStack.t,
     step_start: int,
     step_end: int,
   };
```

</details>

<details>
<summary><code>src/language/dynamics/state/StateSlice.re</code> · open Util;</summary>

<!-- changetour:hunk file=src/language/dynamics/state/StateSlice.re level=2 baseBlob=f49beeecc4d5a50c0415874142f22f95e9e435c0 -->

```diff
@@ -1,119 +0,0 @@
-open Util;
-
-/* Captures the additive side-effects that a subtree's evaluation contributes to an EvaluatorState */
-
-[@deriving (show({with_path: false}), sexp, yojson)]
-type t = {
-  /* `origin` is the step_count at the moment of capture (used to shift probe step_start/end
-   * when replaying at a later step_count) */
-  origin: int,
-  steps: int,
-  probes: Sample.Map.t,
-  tests: list((Id.t, list(TestMap.instance_report))),
-  theorems: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
-  app_args: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
-};
-
-let empty: t = {
-  origin: 0,
-  steps: 0,
-  probes: Sample.Map.empty,
-  tests: [],
-  theorems: [],
-  app_args: Id.Map.empty,
-};
-
-let diff_probes = (~before: Sample.Map.t, ~after: Sample.Map.t): Sample.Map.t =>
-  Id.Map.fold(
-    (id, after_samples, acc) => {
-      let before_count =
-        switch (Id.Map.find_opt(id, before)) {
-        | Some(l) => List.length(l)
-        | None => 0
-        };
-      let after_count = List.length(after_samples);
-      let new_count = after_count - before_count;
-      if (new_count > 0) {
-        /* Take the first new_count elements (the newly prepended samples) */
-        let new_samples =
-          List.filteri((i, _) => i < new_count, after_samples);
-        Id.Map.add(id, new_samples, acc);
-      } else {
-        acc;
-      };
-    },
-    after,
-    Id.Map.empty,
-  );
-
-let diff_tests =
-    (~before: TestMap.t, ~after: TestMap.t)
-    : list((Id.t, list(TestMap.instance_report))) => {
-  List.filter_map(
-    ((id, after_reports)) => {
-      let before_reports =
-        switch (TestMap.lookup(id, before)) {
-        | Some(r) => r
-        | None => []
-        };
-      let before_count = List.length(before_reports);
-      let after_count = List.length(after_reports);
-      if (after_count > before_count) {
-        let new_reports =
-          List.filteri((i, _) => i >= before_count, after_reports);
-        Some((id, new_reports));
-      } else {
-        None;
-      };
-    },
-    after,
-  );
-};
-
-let diff_theorems =
-    (
-      ~before: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
-      ~after: list((Id.t, string, Environment.t(Exp.t), Exp.t)),
-    )
-    : list((Id.t, string, Environment.t(Exp.t), Exp.t)) => {
-  let before_len = List.length(before);
-  let after_len = List.length(after);
-  if (after_len > before_len) {
-    List.filteri((i, _) => i >= before_len, after);
-  } else {
-    [];
-  };
-};
-
-let diff_app_args =
-    (
-      ~before: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
-      ~after: Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))),
-    )
-    : Id.Map.t(list((Sample.call_stack, Sample.Env.elided_value))) =>
-  Id.Map.fold(
-    (id, after_entries, acc) => {
-      let before_count =
-        switch (Id.Map.find_opt(id, before)) {
-        | Some(l) => List.length(l)
-        | None => 0
-        };
-      let after_count = List.length(after_entries);
-      let new_count = after_count - before_count;
-      if (new_count > 0) {
-        let new_entries =
-          List.filteri((i, _) => i < new_count, after_entries);
-        Id.Map.add(id, new_entries, acc);
-      } else {
-        acc;
-      };
-    },
-    after,
-    Id.Map.empty,
-  );
-
-let shift_sample = (delta: int, s: Sample.t): Sample.t => {
-  ...s,
-  step_start: s.step_start + delta,
-  step_end: s.step_end + delta,
-};
```

</details>

<details>
<summary><code>src/language/dynamics/transition/PatternMatch.re</code> · type sample_closures = list((Sample.call_stack, int, int) =…</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/PatternMatch.re level=2 baseBlob=6bcf7b0b817721602ba3421b4992ce6608e7c60f -->

```diff
@@ -17,7 +17,7 @@ let combine_result = (r1: match_result, r2: match_result): match_result =>
 
 /* Sample closures take call_stack, step_start, and step_end.
  * Collected during pattern matching when patterns are targeted. */
-type sample_closures = list((Sample.call_stack, int, int) => Sample.t);
+type sample_closures = list((CallStack.t, int, int) => Sample.t);
 
 /* Core pattern matching logic - just a switch on pattern structure */
 let match_pattern =
```

</details>

<details>
<summary><code>src/language/dynamics/transition/PatternMatch.re</code> · (call_stack: Sample.call_stack, step_start: int, step_end:…</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/PatternMatch.re level=2 baseBlob=6bcf7b0b817721602ba3421b4992ce6608e7c60f -->

```diff
@@ -86,7 +86,7 @@ let record_sample =
   | (Some(spec), Matches(env)) =>
     sample_closures :=
       List.cons(
-        (call_stack: Sample.call_stack, step_start: int, step_end: int) =>
+        (call_stack: CallStack.t, step_start: int, step_end: int) =>
           Sample.mk(
             ~step_start,
             ~step_end,
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · | `Environment =&gt; Closure(env, d) |&gt; fresh</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -423,7 +426,7 @@ module Transition = (EV: EV_MODE) => {
 
     let subst_env = (env, d) =>
       switch (mode) {
-      | `Environment => Closure(env, d) |> fresh
+      | `Environment => generated(Closure(env, d))
       | `Substitution => d |> Substitution.in_exp(env)
       };
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · let env' = Environment.extend(env, (n, ProofObject(e') |&gt; E…</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -502,7 +505,7 @@ module Transition = (EV: EV_MODE) => {
     | Theorem({term: Var(n), _} as dp, e, d1) =>
       let. _ = otherwise(env, d);
       let e' = Substitution.in_exp(env, e);
-      let env' = Environment.extend(env, (n, ProofObject(e') |> Exp.fresh));
+      let env' = Environment.extend(env, (n, generated(ProofObject(e'))));
       Step({
         expr: subst_env(env', d1),
         side_effects: [
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · expr: Tuple([]) |&gt; fresh,</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -615,7 +618,7 @@ module Transition = (EV: EV_MODE) => {
         | _ => "No hint available."
         };
       Step({
-        expr: Tuple([]) |> fresh,
+        expr: generated(Tuple([])),
         side_effects: [
           RecordTest({
             exp: d,
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · Asc(Ap(Forward, d1'', Asc(d2', t1) |&gt; fresh) |&gt; fresh, t2)</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -668,8 +671,12 @@ module Transition = (EV: EV_MODE) => {
       | Asc(d1'', {term: Arrow(t1, t2), _}) =>
         Step({
           expr:
-            Asc(Ap(Forward, d1'', Asc(d2', t1) |> fresh) |> fresh, t2)
-            |> fresh,
+            generated(
+              Asc(
+                generated(Ap(Forward, d1'', generated(Asc(d2', t1)))),
+                t2,
+              ),
+            ),
           side_effects: [],
           kind: Ascription,
           is_value: false,
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · Atom(Atom.repack(out_ty, return_value)) |&gt; Exp.fresh</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -859,7 +866,7 @@ module Transition = (EV: EV_MODE) => {
           switch (f(n)) {
           | Either.L(return_value) =>
             // operator was successful
-            Atom(Atom.repack(out_ty, return_value)) |> Exp.fresh
+            generated(Atom(Atom.repack(out_ty, return_value)))
           | Either.R(error) =>
             // e.g. divide by zero
             dynamic_error_hole(UnOp(op, d1) |> rewrap, error)
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · expr: Atom(Bool(poly_op == Equals)) |&gt; fresh,</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -920,14 +927,14 @@ module Transition = (EV: EV_MODE) => {
           | None => Indet
           | Some(true) =>
             Step({
-              expr: Atom(Bool(poly_op == Equals)) |> fresh,
+              expr: generated(Atom(Bool(poly_op == Equals))),
               side_effects: [],
               kind: BinOp(op),
               is_value: true,
             })
           | Some(false) =>
             Step({
-              expr: Atom(Bool(poly_op != Equals)) |> fresh,
+              expr: generated(Atom(Bool(poly_op != Equals))),
               side_effects: [],
               kind: BinOp(op),
               is_value: false,
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · Atom(Atom.repack(out_ty, return_value)) |&gt; Exp.fresh</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -941,7 +948,7 @@ module Transition = (EV: EV_MODE) => {
           switch (f(n1, n2)) {
           | Either.L(return_value) =>
             // operator was successful
-            Atom(Atom.repack(out_ty, return_value)) |> Exp.fresh
+            generated(Atom(Atom.repack(out_ty, return_value)))
           | Either.R(error) =>
             // e.g. divide by zero
             dynamic_error_hole(BinOp(op, d1, d2) |> rewrap, error)
```

</details>

<details>
<summary><code>src/language/dynamics/transition/Transition.re</code> · List.map(d =&gt; Dot(d, lab |&gt; Exp.fresh) |&gt; Exp.fresh, ds);</summary>

<!-- changetour:hunk file=src/language/dynamics/transition/Transition.re level=2 baseBlob=093c8472bc852bea9e9e44ca53e3bb2e848ed714 -->

```diff
@@ -1002,8 +1009,8 @@ module Transition = (EV: EV_MODE) => {
               : Indet
           | ListLit(ds) =>
             let mapped =
-              List.map(d => Dot(d, lab |> Exp.fresh) |> Exp.fresh, ds);
-            let ls = ListLit(mapped) |> Exp.fresh;
+              List.map(d => generated(Dot(d, generated(lab))), ds);
+            let ls = generated(ListLit(mapped));
             Step({
               expr: ls,
               side_effects: [],
```

</details>

<details>
<summary><code>src/web/app/probesystem/ProbeSidebar.re</code> · ~cursor_stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=src/web/app/probesystem/ProbeSidebar.re level=2 baseBlob=f735871394c11396d331121a5bc45ca363fb55bd -->

```diff
@@ -60,8 +60,8 @@ let legend_sample =
       ~mode: Sample.Window.mode,
       ~ap_id: option(Id.t),
       ~indicated_call: option(Id.t),
-      ~cursor_stack: Sample.call_stack,
-      ~sample_stack: Sample.call_stack,
+      ~cursor_stack: CallStack.t,
+      ~sample_stack: CallStack.t,
       ~step_range: (int, int),
       ~focus_step_range: option((int, int)),
       ~caption: string,
```

</details>

<details>
<summary><code>src/web/app/probesystem/ProbeSidebar.re</code> · let f: Sample.stack_frame = {</summary>

<!-- changetour:hunk file=src/web/app/probesystem/ProbeSidebar.re level=2 baseBlob=f735871394c11396d331121a5bc45ca363fb55bd -->

```diff
@@ -145,7 +145,7 @@ let legend_view = (~globals as _: Globals.t, ~explain_this_inject) => {
   let mode = ProbeProj.Settings.s^.window;
   let color_scheme = ProbeProj.Settings.s^.sample_base;
   let focus = Some((10, 20));
-  let f: Sample.stack_frame = {
+  let f: CallStack.frame = {
     id: Id.invalid,
     name: None,
     fn_def_id: None,
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · let unpin = (~globals: Globals.t, pinned_stack: Sample.call…</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -67,7 +67,7 @@ let set_focus_index = (~globals: Globals.t, i: int, _) =>
   globals.inject_global(ActiveEditor(Project(SampleFocus(SetIndex(i)))));
 
 /* Remove a pin by toggling it off */
-let unpin = (~globals: Globals.t, pinned_stack: Sample.call_stack, _) =>
+let unpin = (~globals: Globals.t, pinned_stack: CallStack.t, _) =>
   globals.inject_global(
     ActiveEditor(Project(SampleFocus(TogglePin(pinned_stack)))),
   );
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · (</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -81,17 +81,13 @@ let has_probes = (refractors: Zipper.Refractor.t): bool =>
  * whose app_id is in user code. Used as a fallback for separator clicks
  * when the separator's own app_id comes from built-in internal code. */
 let find_nearest_user_app =
-    (
-      ~info_map: Statics.Map.t,
-      ~call_stack: Sample.call_stack,
-      ~from_index: int,
-    )
+    (~info_map: Statics.Map.t, ~call_stack: CallStack.t, ~from_index: int)
     : option(Id.t) => {
   let rec search = (i: int): option(Id.t) =>
     if (i < 0) {
       None;
     } else {
-      let frame: Sample.stack_frame = List.nth(call_stack, i);
+      let frame: CallStack.frame = List.nth(call_stack, i);
       is_in_user_code(~info_map, frame.id) ? Some(frame.id) : search(i - 1);
     };
   search(from_index);
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · (~info_map: Statics.Map.t, ~call_stack: Sample.call_stack,…</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -102,9 +98,9 @@ let find_nearest_user_app =
  * Otherwise, walk up the call stack to find the nearest user-visible
  * call site (e.g., for built-in internal calls). */
 let get_call_site_target =
-    (~info_map: Statics.Map.t, ~call_stack: Sample.call_stack, ~index: int)
+    (~info_map: Statics.Map.t, ~call_stack: CallStack.t, ~index: int)
     : option(Id.t) => {
-  let frame: Sample.stack_frame = List.nth(call_stack, index);
+  let frame: CallStack.frame = List.nth(call_stack, index);
   is_in_user_code(~info_map, frame.id)
     ? Some(frame.id)
     : find_nearest_user_app(~info_map, ~call_stack, ~from_index=index - 1);
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · (~info_map: Statics.Map.t, frame: Sample.stack_frame): stri…</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -200,7 +196,7 @@ type visible_item =
 
 /* Resolve the display name for a call stack frame */
 let resolve_display_name =
-    (~info_map: Statics.Map.t, frame: Sample.stack_frame): string =>
+    (~info_map: Statics.Map.t, frame: CallStack.frame): string =>
   switch (frame.name) {
   | Some(name) => name
   | None =>
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · ~call_stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -312,7 +308,7 @@ let key_handler =
       ~globals: Globals.t,
       ~index: int,
       ~max_index: int,
-      ~call_stack: Sample.call_stack,
+      ~call_stack: CallStack.t,
       ~info_map: Statics.Map.t,
       evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.keyboardEvent),
     ) => {
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · (f: Sample.stack_frame) =&gt; f.id,</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -367,7 +363,7 @@ let view =
     let pinned_head_id =
       Option.bind(pinned_stack, stack =>
         Option.map(
-          (f: Sample.stack_frame) => f.id,
+          (f: CallStack.frame) => f.id,
           Util.ListUtil.hd_opt(stack),
         )
       );
```

</details>

<details>
<summary><code>src/web/app/probesystem/SampleFocusBar.re</code> · let frame: Sample.stack_frame = List.nth(call_stack, i);</summary>

<!-- changetour:hunk file=src/web/app/probesystem/SampleFocusBar.re level=2 baseBlob=66f9672bfe697822b6bbe23d39405c3a5f0e8d42 -->

```diff
@@ -394,7 +390,7 @@ let view =
 
     /* Build a single breadcrumb entry (separator + entry node) for stack index i */
     let build_single_entry = (i: int): list(Node.t) => {
-      let frame: Sample.stack_frame = List.nth(call_stack, i);
+      let frame: CallStack.frame = List.nth(call_stack, i);
       let app_id = frame.id;
       let display_text = names[i];
       let is_unknown =
```

</details>

<details>
<summary><code>src/web/exercises/GradeExercise.re</code> · | Completed((result, _)) =&gt; Some(result)</summary>

<!-- changetour:hunk file=src/web/exercises/GradeExercise.re level=2 baseBlob=8fc3318abd7e9fe85fe143fc98af58ff83a997c5 -->

```diff
@@ -19,7 +19,7 @@ let evaluate_term = (term: Exp.t): option(Exp.t) => {
        );
   switch (evaluated) {
   | StepLimitExceeded => None
-  | Completed((result, _)) => Some(result)
+  | LimitedCompleted((result, _)) => Some(result)
   };
 };
```

</details>

<details>
<summary><code>src/web/view/DerivationExerciseMode.re</code> · | Some(ResultPending)</summary>

<!-- changetour:hunk file=src/web/view/DerivationExerciseMode.re level=2 baseBlob=95fa288097b3de3d6f98d7d3fd54f070ff676047 -->

```diff
@@ -13,7 +13,7 @@ let stitched_results =
       fun
       | Some(ProgramResult.ResultOk(r)) => Some(r.result)
       | Some(ResultFail(_))
-      | Some(ResultPending)
+      | Some(ResultPending(_))
       | None => None
     )
   );
```

</details>

<details>
<summary><code>src/web/view/TutorialMode.re</code> · | ResultPending =&gt;</summary>

<!-- changetour:hunk file=src/web/view/TutorialMode.re level=2 baseBlob=04e584c7baef063a512ee3eccaad1e99a297f522 -->

```diff
@@ -563,7 +591,7 @@ module View = {
             let inner_result = hidden_tests.result.result;
             let result = inner_result |> Util.Calc.get_value;
             switch (result) {
-            | ResultPending =>
+            | ResultPending(_) =>
               div(
                 ~attrs=[Attr.classes(["checkmark-grey", "pending"])],
                 [text("🤔")],
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · let frame = (~name=None, id: Id.t): Sample.stack_frame =&gt; {</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -20,14 +20,14 @@ open Language;
 /* --- Helpers --- */
 
 /* Make a stack frame. name defaults to None (as cursor/step-into constructs) */
-let frame = (~name=None, id: Id.t): Sample.stack_frame => {
+let frame = (~name=None, id: Id.t): CallStack.frame => {
   id,
   name,
   fn_def_id: None,
 };
 
 /* Make a named stack frame (as evaluator produces) */
-let named_frame = (id: Id.t, name: string): Sample.stack_frame => {
+let named_frame = (id: Id.t, name: string): CallStack.frame => {
   id,
   name: Some(name),
   fn_def_id: None,
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · (~seq=0, ~step_start=0, ~step_end=0, stack: Sample.call_sta…</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -44,7 +44,7 @@ let id_g = Id.mk();
 
 /* Make a minimal sample with the given call stack */
 let mk_sample =
-    (~seq=0, ~step_start=0, ~step_end=0, stack: Sample.call_stack): Sample.t => {
+    (~seq=0, ~step_start=0, ~step_end=0, stack: CallStack.t): Sample.t => {
   id: Hashtbl.hash((stack, Id.invalid)),
   syntax_id: Id.invalid,
   value: IdTagged.FreshGrammar.Exp.empty_hole(),
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -65,7 +65,7 @@ let mk_cursor =
       ~indicated_call=None,
       ~seq=0,
       ~step_range=None,
-      stack: Sample.call_stack,
+      stack: CallStack.t,
     )
     : Sample.Focus.t => {
   call_stack: stack,
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · check(bool, "should be equal", true, Sample.equal_stack_fra…</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -129,7 +129,7 @@ let equality_tests = [
     () => {
       let f1 = frame(id_a);
       let f2 = named_frame(id_a, "foo");
-      check(bool, "should be equal", true, Sample.equal_stack_frame(f1, f2));
+      check(bool, "should be equal", true, CallStack.equal_frame(f1, f2));
     },
   ),
   test_case(
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · Sample.equal_stack_frame(f1, f2),</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -142,7 +142,7 @@ let equality_tests = [
         bool,
         "should not be equal",
         false,
-        Sample.equal_stack_frame(f1, f2),
+        CallStack.equal_frame(f1, f2),
       );
     },
   ),
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · Sample.equal_call_stack(cursor_stack, eval_stack),</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -158,7 +158,7 @@ let equality_tests = [
         bool,
         "should be equal (id-only comparison)",
         true,
-        Sample.equal_call_stack(cursor_stack, eval_stack),
+        CallStack.equal(cursor_stack, eval_stack),
       );
     },
   ),
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · stack: Sample.call_stack,</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -593,7 +593,7 @@ let mk_cursor_at_index =
       ~seq=0,
       ~step_range=None,
       ~index: int,
-      stack: Sample.call_stack,
+      stack: CallStack.t,
     )
     : Sample.Focus.t => {
   call_stack: stack,
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · ~eq=Sample.equal_stack_frame,</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -923,7 +923,7 @@ let three_level_tests = [
       };
       let is_suffix =
         Util.ListUtil.is_suffix_of(
-          ~eq=Sample.equal_stack_frame,
+          ~eq=CallStack.equal_frame,
           mid_data.call_stack,
           cursor_inner.call_stack,
         );
```

</details>

<details>
<summary><code>test/Test_SampleSelection.re</code> · ~eq=Sample.equal_stack_frame,</summary>

<!-- changetour:hunk file=test/Test_SampleSelection.re level=2 baseBlob=44ae3c01e00640c12ef9392ed96c455947716139 -->

```diff
@@ -951,7 +951,7 @@ let three_level_tests = [
       };
       let is_suffix2 =
         Util.ListUtil.is_suffix_of(
-          ~eq=Sample.equal_stack_frame,
+          ~eq=CallStack.equal_frame,
           top_data.call_stack,
           cursor_mid.call_stack,
         );
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · open Haz3lcore;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1,5 +1,6 @@
 open Alcotest;
 open Language;
+open Haz3lcore;
 open Test_Evaluator_Prelude;
 
 /* Tests for the incremental evaluator. Exercises the three key mechanisms:
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · *   Each test that claims to test reuse / dirtying ALSO ass…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -23,9 +24,9 @@ open Test_Evaluator_Prelude;
  *   which walks a parsed Exp.t and replaces an Atom(Int(n)) payload
  *   in-place while keeping the surrounding IdTagged annotations untouched.
  *
- *   Each test that claims to test reuse / dirtying ALSO asserts that
- *   `incr.reused` is non-empty on the second run, so we can't silently
- *   regress into the "disjoint id spaces" failure mode again. */
+ *   Tests that claim to exercise reuse / dirtying also check the explicit
+ *   reuse plan for the second run, so we can't silently regress into the
+ *   "disjoint id spaces" failure mode again. */
 
 /* Statics.mk now returns the info_map AND the elaborated expression
  * together (Elaborator.re was merged into statics on dev), so we always
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let eval_info_of_statics = info_map =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -132,6 +137,31 @@ let test_populates_entries = () => {
   );
 };
 
+let eval_info_of_statics = info_map =>
+  EvalInfo.of_info_map(
+    ~probe_all=CoreSettings.on.probe_all,
+    ~targets=Id.Map.empty,
+    info_map,
+  );
+
+let reuse_plan =
+    (~prev: EvaluatorState.incr_eval=IncrEval.empty, exp: Exp.t)
+    : EvaluatorState.incr_eval => {
+  let (info_map, elab) = statics_and_elab(exp);
+  let info_map = eval_info_of_statics(info_map);
+  ReusePass.reuse_pass(~prev, ~info_map, ~env=Builtins.env_init, elab);
+};
+
+let has_reuse = (ack_incr: EvaluatorState.incr_eval): bool =>
+  !Id.Map.is_empty(ack_incr.entries);
+
+let directly_reused = (id: Id.t, ack_incr: EvaluatorState.incr_eval): bool =>
+  Id.Map.mem(id, ack_incr.entries);
+
+let frozen_ids_for =
+    (~prev: EvaluatorState.incr_eval, exp: Exp.t): list(Id.t) =>
+  IncrEval.frozen_ids(~ack_incr=reuse_plan(~prev, exp));
+
 /* Running twice with the SAME Exp.t (so ids are identical): the second run
  * should reuse lots of entries. Without replace_int_lit / id preservation
  * this works for the wrong reason (parse_exp twice on the same string
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -186,7 +469,7 @@ let test_partial_reuse_after_edit = () => {
     !Exp.fast_equal(exp1, exp2),
   );
   let (_, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(
     dhexp_typ,
     "Edit to z's rhs produces updated result",
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -200,7 +483,7 @@ let test_partial_reuse_after_edit = () => {
     bool,
     "Unchanged subtrees reused from prev map",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -225,7 +508,7 @@ let test_dirty_propagates_to_downstream_sum = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=77, ~to_=2, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "First run sum = 5 + 77 = 82", parse_exp("82"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -240,7 +523,7 @@ let test_dirty_propagates_to_downstream_sum = () => {
     bool,
     "Second run reuses at least some entries",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -271,7 +554,7 @@ let test_rhs_edit_after_body_edit_invalidates_body = () => {
   let exp3 = replace_int_lit(~from=77, ~to_=2, exp2);
   let (r1, _, incr1) = eval_incr(exp1);
   let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
-  let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);
+  let (r3, _, _) = eval_incr(~prev=incr2, exp3);
   check(dhexp_typ, "Run 1: 5 + 77 + 88 = 170", parse_exp("170"), r1);
   check(dhexp_typ, "Run 2: 5 + 77 + 99 = 181", parse_exp("181"), r2);
   check(
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -287,21 +570,21 @@ let test_rhs_edit_after_body_edit_invalidates_body = () => {
     bool,
     "Run 2 reused some entries (not a from-scratch eval)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
   check(
     bool,
     "Run 3 reused some entries (not a from-scratch eval)",
     true,
-    incr3.reused != [],
+    has_reuse(reuse_plan(~prev=incr2, exp3)),
   );
 };
 
 /* ========================================================================
  * Coverage for more Exp.t forms beyond let + binops. Each test uses
  * replace_int_lit for in-place edits (to preserve ids), checks the final
- * value, and (where reuse should fire) asserts incr.reused != [] so we
- * can't silently degrade into a from-scratch re-evaluation. */
+ * value, and (where reuse should fire) asserts the reuse plan is non-empty
+ * so we can't silently degrade into a from-scratch re-evaluation. */
 
 /* Function application: editing inside a function BODY should invalidate
  * every call site that depends on that function. Function bodies are
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -316,7 +599,7 @@ let test_function_body_edit_invalidates_apps = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=9, ~to_=3, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: 45 + 90 = 135", parse_exp("135"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -329,7 +612,7 @@ let test_function_body_edit_invalidates_apps = () => {
     bool,
     "Second run still reuses some entries (call-site args)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -342,15 +625,74 @@ let test_function_arg_edit_reuses_other_calls = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=7, ~to_=100, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: 14 + 22 = 36", parse_exp("36"), r1);
   check(
     dhexp_typ,
     "Run 2 after first-arg edit: 200 + 22 = 222",
     parse_exp("222"),
     r2,
   );
-  check(bool, "Second run reuses some entries", true, incr2.reused != []);
+  check(
+    bool,
+    "Second run reuses some entries",
+    true,
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
+  );
+};
+
+/* Editing an expression sequenced before a function call should not force
+ * the call to rerun. User repro:
+ *
+ *   let f = fun x ->
+ *     5;
+ *   in
+ *   6; f(20)
+ *
+ * Edit 6 -> 4. The application `f(20)` has the same elab and depends only
+ * on `f`, whose binding is unchanged, so the Ap should be reused. */
+let test_seq_edit_before_function_call_reuses_call = () => {
+  let src = {|let f = fun x ->
+  5;
+in
+6; f(20)|};
+  let exp1 = parse_exp(src);
+  let exp2 = replace_int_lit(~from=6, ~to_=4, exp1);
+  check(
+    bool,
+    "replace_int_lit actually changed the expression",
+    true,
+    !Exp.fast_equal(exp1, exp2),
+  );
+  let f20_id = {
+    let found = ref(None);
+    let f_exp = (continue, e: Exp.t): Exp.t => {
+      switch (e.term) {
+      | Ap(_, _, arg) =>
+        switch (arg.term) {
+        | Atom(Int(n)) when Bigint.to_string(n) == "20" =>
+          found := Some(Exp.rep_id(e))
+        | _ => ()
+        }
+      | _ => ()
+      };
+      continue(e);
+    };
+    let _ = TermBase.Exp.map_term(~f_exp, exp1);
+    switch (found^) {
+    | Some(id) => id
+    | None => failwith("could not locate `f(20)` Ap node")
+    };
+  };
+  let (r1, _, incr1) = eval_incr(exp1);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
+  check(dhexp_typ, "Run 2 result is unchanged after 6 -> 4", r1, r2);
+  check(
+    bool,
+    "f(20) is reused after editing only the preceding sequence expression",
+    true,
+    directly_reused(f20_id, reuse_plan(~prev=incr1, exp2)),
+  );
 };
 
 /* If: editing the UNTAKEN branch leaves the result unchanged; reuse should
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -362,7 +704,7 @@ let test_if_untaken_branch_edit_reuses = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=77, ~to_=999, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: taken branch = 42", parse_exp("42"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -374,7 +716,7 @@ let test_if_untaken_branch_edit_reuses = () => {
     bool,
     "Untaken-branch edit leaves reusable entries",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -385,7 +727,7 @@ let test_if_taken_branch_edit_updates = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=42, ~to_=13, exp1);
   let (_, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(
     dhexp_typ,
     "Run 2 after taken-branch edit: 13 (not stale 42)",
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -396,7 +738,7 @@ let test_if_taken_branch_edit_updates = () => {
     bool,
     "Second run still reuses some entries",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -408,7 +750,7 @@ let test_match_untaken_arm_edit_reuses = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=22, ~to_=333, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: matched 0 -> 11", parse_exp("11"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -420,7 +762,7 @@ let test_match_untaken_arm_edit_reuses = () => {
     bool,
     "Untaken-arm edit leaves reusable entries",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -463,15 +805,20 @@ let test_tuple_destructuring_edit_updates = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=20, ~to_=200, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: 10 + 20 + 30 = 60", parse_exp("60"), r1);
   check(
     dhexp_typ,
     "Run 2 after editing tuple middle: 10 + 200 + 30 = 240",
     parse_exp("240"),
     r2,
   );
-  check(bool, "Second run reuses some entries", true, incr2.reused != []);
+  check(
+    bool,
+    "Second run reuses some entries",
+    true,
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
+  );
 };
 
 /* List literal: same idea — editing one element shouldn't break the result.
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -506,15 +853,20 @@ let test_shadowing_inner_let_edit = () => {
   let exp1 = parse_exp(src);
   let exp2 = replace_int_lit(~from=7, ~to_=77, exp1);
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: 10 + 7 = 17", parse_exp("17"), r1);
   check(
     dhexp_typ,
     "Run 2 after inner-x edit: 10 + 77 = 87",
     parse_exp("87"),
     r2,
   );
-  check(bool, "Shadowing still allows reuse", true, incr2.reused != []);
+  check(
+    bool,
+    "Shadowing still allows reuse",
+    true,
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
+  );
 };
 
 /* Function bodies are a DEFERRED boundary: no incremental entries are
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_al…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -546,7 +898,11 @@ let test_probe_replay_on_reuse = () => {
   let exp = parse_exp(src);
   let (info_map, elab) = statics_and_elab(exp);
   let info_map =
-    EvalInfoMap.of_info_map(~probe_all=CoreSettings.on.probe_all, info_map);
+    EvalInfo.of_info_map(
+      ~probe_all=CoreSettings.on.probe_all,
+      ~targets=Id.Map.empty,
+      info_map,
+    );
   /* First run: no probes targeted. */
   let (_, state1) =
     Evaluator.evaluate(
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · *   `let z = x + y` are never visited and so end up in NEIT…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -622,12 +978,11 @@ let test_pbt_regression_unit_pat_dup_label_dh_let = () => {
  *   its cached entry, it short-circuits via `Evaluator.re:158-164` and
  *   marks only that one id as reused (`IncrEval.mark_reused`). The
  *   surface-sibling inner ModLets `let x = fib(b)`, `let y = fib(b)`,
- *   `let z = x + y` are never visited and so end up in NEITHER
- *   `incr.reused` NOR `incr.recalculated` — leaving them un-tinted in
- *   the editor even though they're effectively frozen.
+ *   `let z = x + y` are never visited during evaluation, so the UI must
+ *   derive frozen ids by walking the reuse plan rather than visited output.
  *
- *   The fix is to derive a "frozen set" from `incr.reused` by walking
- *   each reused id's `prev_elab` and unioning all rep_ids encountered.
+ *   The fix is to derive a "frozen set" from the ACK reuse plan by walking
+ *   each entry's `prev_elab` and unioning all rep_ids encountered.
  *   That set is what the UI should paint as frozen. This test pins down
  *   the desired contents of that set. */
 let test_module_c_inner_ids_in_frozen_set_after_edit_in_module_a = () => {
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (_, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -694,13 +1049,12 @@ let c = {
     List.length(c_inner_modlet_ids),
   );
   let (_, _, incr1) = eval_incr(exp1);
-  let (_, _, incr2) = eval_incr(~prev=incr1, exp2);
-  /* The "frozen set" is what the UI should paint as frozen. Currently
-   * `incr.reused` only contains ids that the evaluator actually visited
-   * and short-circuited. The intended fix expands that to the elab-
-   * descendant closure: for every reused id, walk its cached prev_elab
+  let (_, _, _incr2) = eval_incr(~prev=incr1, exp2);
+  /* The "frozen set" is what the UI should paint as frozen. The reuse
+   * plan can contain an ancestor that short-circuits evaluation; frozen ids
+   * expand that to the elab-descendant closure by walking cached prev_elab
    * (in `incr.entries`) and union all rep_ids. */
-  let frozen = IncrEval.frozen_ids(incr2);
+  let frozen = frozen_ids_for(~prev=incr1, exp2);
   let missing = List.filter(id => !List.mem(id, frozen), c_inner_modlet_ids);
   check(
     int,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · check(bool, "incr2 reused something", true, incr2.reused !=…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -773,7 +1127,12 @@ let test_diag_nested_module_rhs_edit_marks_binder_dirty = () => {
     true,
     !Exp.fast_equal(r1, r2),
   );
-  check(bool, "incr2 reused something", true, incr2.reused != []);
+  check(
+    bool,
+    "incr2 populated incremental entries",
+    true,
+    !IncrEval.is_empty(incr2),
+  );
 };
 
 /* Repro: `let x = ({}, 0) in (x, 3)`. Edit `3` → `4`. The let-x rhs
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (_, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -829,8 +1188,8 @@ let test_diag_module_in_unchanged_rhs_tuple_lands_in_frozen = () => {
     | _ => failwith("rhs inner is not a 2-tuple")
     };
   let (_, _, incr1) = eval_incr(exp1);
-  let (_, _, incr2) = eval_incr(~prev=incr1, exp2);
-  let frozen = IncrEval.frozen_ids(incr2);
+  let (_, _, _incr2) = eval_incr(~prev=incr1, exp2);
+  let frozen = frozen_ids_for(~prev=incr1, exp2);
   check(bool, "Atom 0 is in frozen set", true, List.mem(zero_id, frozen));
   check(
     bool,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · * - Each test also asserts incr2.reused != [] so we can't s…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -859,8 +1218,8 @@ let test_diag_module_in_unchanged_rhs_tuple_lands_in_frozen = () => {
  *   between the two variants, so the inner body's cache entry from one
  *   run is keyed by an id that's still present in the other run — exactly
  *   the situation that triggers the bug.
- * - Each test also asserts incr2.reused != [] so we can't silently degrade
- *   into a from-scratch eval and accidentally produce the right answer. */
+ * - Each test also checks the reuse plan so we can't silently degrade into
+ *   a from-scratch eval and accidentally produce the right answer. */
 
 /* (1) Deleting an inner Let that was shadowing an outer same-named binding.
  *
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp_without);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -886,7 +1245,7 @@ let test_delete_inner_let_uncovers_outer_binding = () => {
     !Exp.fast_equal(exp_with, exp_without),
   );
   let (r1, _, incr1) = eval_incr(exp_with);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_without);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp_without);
   check(dhexp_typ, "Run 1: inner x=1 wins, x+x = 2", parse_exp("2"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -898,7 +1257,7 @@ let test_delete_inner_let_uncovers_outer_binding = () => {
     bool,
     "Second run reuses at least some entries (not a from-scratch eval)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp_without)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -919,7 +1278,7 @@ let test_add_inner_let_shadows_outer_binding = () => {
   let exp_with = parse_exp(src);
   let exp_without = strip_let_with_int_rhs(~rhs_val=1, exp_with);
   let (r1, _, incr1) = eval_incr(exp_without);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp_with);
   check(dhexp_typ, "Run 1: only outer x=10, x+x = 20", parse_exp("20"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · incr2.reused != [],</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -931,7 +1290,7 @@ let test_add_inner_let_shadows_outer_binding = () => {
     bool,
     "Second run reuses at least some entries (not a from-scratch eval)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp_with)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -997,14 +1356,14 @@ f(8)|};
     };
   };
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: f(8) = 1", parse_exp("1"), r1);
   check(dhexp_typ, "Run 2: f(8) = 1 (unchanged)", parse_exp("1"), r2);
   check(
     bool,
     "Run 2 reuses something (sanity, not a from-scratch run)",
     true,
-    incr2.reused != [],
+    has_reuse(reuse_plan(~prev=incr1, exp2)),
   );
   /* The actual bug we're pinning: `f(8)` should be reused — it doesn't
    * reference the `_ = 55` binding and `f`'s rhs is unchanged. With the
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · List.mem(f8_id, incr2.reused),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1013,7 +1372,7 @@ f(8)|};
     bool,
     "f(8) is reused on run 2 (it doesn't depend on the edited _-binding)",
     true,
-    List.mem(f8_id, incr2.reused),
+    directly_reused(f8_id, reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1063,7 +1422,7 @@ let test_three_run_leftmost_binop_reuses_on_run3 = () => {
   };
   let (r1, _, incr1) = eval_incr(exp1);
   let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
-  let (r3, _, incr3) = eval_incr(~prev=incr2, exp3);
+  let (r3, _, _) = eval_incr(~prev=incr2, exp3);
   check(dhexp_typ, "Run 1: 1+2+3+4 = 10", parse_exp("10"), r1);
   check(dhexp_typ, "Run 2: 1+2+3+5 = 11", parse_exp("11"), r2);
   check(dhexp_typ, "Run 3: 1+2+4+5 = 12", parse_exp("12"), r3);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · List.mem(plus_1_2_id, IncrEval.frozen_ids(incr2)),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1075,12 +1434,12 @@ let test_three_run_leftmost_binop_reuses_on_run3 = () => {
     bool,
     "Run 2: `1 + 2` is in frozen_ids (subsumed by a reused ancestor)",
     true,
-    List.mem(plus_1_2_id, IncrEval.frozen_ids(incr2)),
+    List.mem(plus_1_2_id, frozen_ids_for(~prev=incr1, exp2)),
   );
   /* The actual bug: on run 3, `(1+2)+3` becomes `(1+2)+4` so its parent
    * (and the parent's parent) must be recalculated. The evaluator descends
    * past them into `1 + 2`, whose subtree hasn't changed at all — so this
-   * id should land in incr3.reused.
+   * id should land directly in run 3's reuse plan.
    *
    * Currently fails because run 2's reuse at the `(1+2)+3` level drops
    * `1+2`'s cache entry from the outgoing incr.entries map (only the
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · List.mem(plus_1_2_id, incr3.reused),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1090,7 +1449,7 @@ let test_three_run_leftmost_binop_reuses_on_run3 = () => {
     bool,
     "Run 3: `1 + 2` is reused (its subtree is unchanged since run 1)",
     true,
-    List.mem(plus_1_2_id, incr3.reused),
+    directly_reused(plus_1_2_id, reuse_plan(~prev=incr2, exp3)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1133,7 +1492,7 @@ let test_outer_edit_does_not_dirty_inner_shadowed_use = () => {
     };
   };
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(dhexp_typ, "Run 1: inner x=4 wins, x = 4", parse_exp("4"), r1);
   check(
     dhexp_typ,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · "Body `x` is NOT recalculated on run 2 (resolves to inner l…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1148,9 +1507,9 @@ let test_outer_edit_does_not_dirty_inner_shadowed_use = () => {
    * let's name-based dirty `x` falsely invalidates the inner `x` use. */
   check(
     bool,
-    "Body `x` is NOT recalculated on run 2 (resolves to inner let, not edited outer)",
-    false,
-    List.mem(body_x_id, incr2.recalculated),
+    "Body `x` is frozen on run 2 (resolves to inner let, not edited outer)",
+    true,
+    List.mem(body_x_id, frozen_ids_for(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · * be in incr2.reused, and the final result should differ fr…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1162,7 +1521,7 @@ let test_outer_edit_does_not_dirty_inner_shadowed_use = () => {
  *
  * On run 2 the body `x + 1` shouldn't reuse the cached value 5 — x's
  * binding is now indeterminate. Concretely the body `x + 1` id should not
- * be in incr2.reused, and the final result should differ from run 1's. */
+ * be in run 2's reuse plan, and the final result should differ from run 1's. */
 let test_let_rhs_becomes_hole_invalidates_body = () => {
   let src = "let x = 4 + ? in x + 1";
   let exp_with_hole = parse_exp(src);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with_hole);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1211,7 +1570,7 @@ let test_let_rhs_becomes_hole_invalidates_body = () => {
     };
   };
   let (r1, _, incr1) = eval_incr(exp_without_hole);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp_with_hole);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp_with_hole);
   print_endline("DIAG r1 = " ++ Exp.show(r1));
   print_endline("DIAG r2 = " ++ Exp.show(r2));
   check(dhexp_typ, "Run 1: x = 4, x + 1 = 5", parse_exp("5"), r1);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · List.mem(body_id, incr2.reused),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1225,7 +1584,7 @@ let test_let_rhs_becomes_hole_invalidates_body = () => {
     bool,
     "Body `x + 1` is NOT reused on run 2 (its binding became indet)",
     false,
-    List.mem(body_id, incr2.reused),
+    directly_reused(body_id, reuse_plan(~prev=incr1, exp_with_hole)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1326,7 +1685,7 @@ n|};
     };
   };
   let (r1, _, incr1) = eval_incr(exp1);
-  let (r2, _, incr2) = eval_incr(~prev=incr1, exp2);
+  let (r2, _, _) = eval_incr(~prev=incr1, exp2);
   check(
     dhexp_typ,
     "Run 1: string_length(\"hello\") = 5",
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Incremental.re</code> · List.mem(builtin_ap_id, incr2.reused),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Incremental.re level=2 baseBlob=5654addd806009e11b3654a557df1b77bfc929af -->

```diff
@@ -1340,7 +1699,7 @@ n|};
     bool,
     "string_length(\"hello\") Ap is reused on run 2",
     true,
-    List.mem(builtin_ap_id, incr2.reused),
+    directly_reused(builtin_ap_id, reuse_plan(~prev=incr1, exp2)),
   );
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Prelude.re</code> · (~step_limit=1000, exp: TermBase.exp_t)</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Prelude.re level=2 baseBlob=391e3a4d14e8f0ad7e1d508debdb19001c445780 -->

```diff
@@ -160,8 +169,7 @@ let single_step = (exp: Exp.t) => {
 };
 
 let full_small_step_reduction =
-    (~step_limit=1000, exp: TermBase.exp_t)
-    : Evaluator.step_constrained(Exp.t) => {
+    (~step_limit=1000, exp: TermBase.exp_t): Evaluator.limited_result => {
   let rec go = (~steps_counter=0, exp: TermBase.exp_t): option(Exp.t) =>
     if (steps_counter > step_limit) {
       None;
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · Evaluator.evaluate(~targets, ~env=Builtins.env_init, elabor…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -21,7 +21,15 @@ open Test_Evaluator_Prelude;
 let get_probes_map = (code: string): Id.Map.t(list(Sample.t)) => {
   let (_term, elaborated, _info_map, targets) = parse_with_probes(code);
   let (_, state) =
-    Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+    Evaluator.evaluate(
+      ~info_map=
+        EvalInfo.{
+          statics: Id.Map.empty,
+          targets,
+        },
+      ~env=Builtins.env_init,
+      elaborated,
+    );
   EvaluatorState.get_probes(state);
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · (~pinned=None, ~indicated_call=None, stack: Sample.call_sta…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -36,8 +44,7 @@ let partition_by_depth =
 
 /* Make a cursor at a given stack, with optional pin */
 let mk_cursor =
-    (~pinned=None, ~indicated_call=None, stack: Sample.call_stack)
-    : Sample.Focus.t => {
+    (~pinned=None, ~indicated_call=None, stack: CallStack.t): Sample.Focus.t => {
   call_stack: stack,
   index: List.length(stack) - 1,
   pinned_stack: pinned,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · (f: Sample.stack_frame): Sample.stack_frame =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -137,7 +144,7 @@ in f(5)|};
         /* Simulate step-into: cursor has same stack but with None name */
         let cursor_stack =
           List.map(
-            (f: Sample.stack_frame): Sample.stack_frame =>
+            (f: CallStack.frame): CallStack.frame =>
               {
                 id: f.id,
                 name: None,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · (f: Sample.stack_frame): Sample.stack_frame =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -179,7 +186,7 @@ in f(1); f(2)|};
       let first = List.hd(samples);
       let pin_stack =
         List.map(
-          (f: Sample.stack_frame): Sample.stack_frame =>
+          (f: CallStack.frame): CallStack.frame =>
             {
               id: f.id,
               name: None,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · (f: Sample.stack_frame): Sample.stack_frame =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -255,7 +262,7 @@ in f(1); f(2)|};
       /* Pin to s1's context, with None names (as step-into would) */
       let pin_stack =
         List.map(
-          (f: Sample.stack_frame): Sample.stack_frame =>
+          (f: CallStack.frame): CallStack.frame =>
             {
               id: f.id,
               name: None,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · Sample.equal_call_stack(kept.call_stack, s1.call_stack),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -278,13 +285,13 @@ in f(1); f(2)|};
         bool,
         "kept sample should match s1's call stack",
         true,
-        Sample.equal_call_stack(kept.call_stack, s1.call_stack),
+        CallStack.equal(kept.call_stack, s1.call_stack),
       );
       check(
         bool,
         "kept sample should NOT match s2's call stack",
         false,
-        Sample.equal_call_stack(kept.call_stack, s2.call_stack),
+        CallStack.equal(kept.call_stack, s2.call_stack),
       );
       /* Full select should also return 1 */
       let (selected, _) = run_select(~cursor, samples);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · (</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -441,12 +448,7 @@ in ^^probe(f(1)); ^^probe(f(2)); ^^probe(f(3))|};
 
 /* Helper: mk_cursor with explicit index for intent preservation testing */
 let mk_cursor_at_index =
-    (
-      ~pinned=None,
-      ~indicated_call=None,
-      ~index: int,
-      stack: Sample.call_stack,
-    )
+    (~pinned=None, ~indicated_call=None, ~index: int, stack: CallStack.t)
     : Sample.Focus.t => {
   call_stack: stack,
   index,
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSelection.re</code> · Evaluator.evaluate(~targets, ~env=Builtins.env_init, elabor…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSelection.re level=2 baseBlob=9289d6999447a24060cbbb42f38a0d4acf04f98a -->

```diff
@@ -750,7 +752,15 @@ in ^^probe(f(42))|};
       /* Evaluate to get samples */
       let elaborated = elaborate(term);
       let (_, state) =
-        Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+        Evaluator.evaluate(
+          ~info_map=
+            EvalInfo.{
+              statics: Id.Map.empty,
+              targets,
+            },
+          ~env=Builtins.env_init,
+          elaborated,
+        );
       let probes_map = EvaluatorState.get_probes(state);
       /* Find the call probe (wrapping f(42)) and inner probe (on x) */
       let call_probe_id =
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_ProbeSteps.re</code> · Evaluator.evaluate(~targets, ~env=Builtins.env_init, elabor…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_ProbeSteps.re level=2 baseBlob=72bf073b1affc17dea5b1f8965bcc5e521fae2ac -->

```diff
@@ -64,7 +64,15 @@ let relationship_testable =
 let get_all_samples = (code: string): list(Sample.t) => {
   let (_term, elaborated, _info_map, targets) = parse_with_probes(code);
   let (_, state) =
-    Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+    Evaluator.evaluate(
+      ~info_map=
+        EvalInfo.{
+          statics: Id.Map.empty,
+          targets,
+        },
+      ~env=Builtins.env_init,
+      elaborated,
+    );
   let probes = EvaluatorState.get_probes(state);
   Id.Map.bindings(probes) |> List.concat_map(snd);
 };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Probes.re</code> · Evaluator.evaluate(~targets, ~env=Builtins.env_init, elabor…</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Probes.re level=2 baseBlob=73ec1d16fc1a7c2baced5e5c1aff2189151e8a06 -->

```diff
@@ -89,7 +89,14 @@ let get_samples_by_line = (code: string): IntMap.t(list(string)) => {
       );
 
     let (_, state) =
-      Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
+      Evaluator.evaluate(
+        ~info_map={
+          statics: Id.Map.empty,
+          targets,
+        },
+        ~env=Builtins.env_init,
+        elaborated,
+      );
     let probes = EvaluatorState.get_probes(state);
 
     /* Get segment and measured for position lookup */
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · | Completed(_)</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -24,7 +24,7 @@ let qcheck_evaluator_does_not_crash_test =
           exp,
         )
       ) {
-      | Completed(_)
+      | LimitedCompleted(_)
       | StepLimitExceeded => true
       | exception e =>
         switch (e) {
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · | (Completed((bigstep_exp, _)), Completed(smallstep_exp)) =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -69,7 +69,10 @@ let qcheck_stepper_confluence =
         ),
         full_small_step_reduction(~step_limit=100, elaborated_exp),
       ) {
-      | (Completed((bigstep_exp, _)), Completed(smallstep_exp)) =>
+      | (
+          LimitedCompleted((bigstep_exp, _)),
+          LimitedCompleted(smallstep_exp),
+        ) =>
         let show_core_exp = exp =>
           exp
           |> ExpToSegment.exp_to_segment(
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · smallstep_exp,</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -88,7 +91,7 @@ let qcheck_stepper_confluence =
             Equality.semantic.exp,
           ), // Output is easier to view through ExpToSegment. This may result in a loss of information
           "Small step reduction and big step reduction are equal",
-          smallstep_exp,
+          smallstep_exp |> fst,
           bigstep_exp,
         );
         true;
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · | (Completed((first_exp, _)), Completed((second_exp, _))) =&gt;</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -148,7 +151,10 @@ let qcheck_pattern_equivalence_test =
             elaborated_second,
           );
         switch (evaluated_first, evaluated_second) {
-        | (Completed((first_exp, _)), Completed((second_exp, _))) =>
+        | (
+            LimitedCompleted((first_exp, _)),
+            LimitedCompleted((second_exp, _)),
+          ) =>
           print_endline("First expression: " ++ show_core_exp(first));
           print_endline("Second expression: " ++ show_core_exp(second));
           Alcotest.check(
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · | (Completed(_), StepLimitExceeded)</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -159,8 +165,8 @@ let qcheck_pattern_equivalence_test =
           );
           true;
         | (StepLimitExceeded, StepLimitExceeded) => true
-        | (Completed(_), StepLimitExceeded)
-        | (StepLimitExceeded, Completed(_)) =>
+        | (LimitedCompleted(_), StepLimitExceeded)
+        | (StepLimitExceeded, LimitedCompleted(_)) =>
           print_endline("One of the evaluations exceeded the step limit");
           false;
         };
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · EvalInfoMap.of_info_map(</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -338,21 +344,23 @@ let qcheck_incremental_matches_fresh_after_edit =
             Some((info_map_edit, elab_edit)),
           ) =>
           let info_slice_orig =
-            EvalInfoMap.of_info_map(
+            EvalInfo.of_info_map(
               ~probe_all=CoreSettings.on.probe_all,
+              ~targets=Id.Map.empty,
               info_map_orig,
             );
           let info_slice_edit =
-            EvalInfoMap.of_info_map(
+            EvalInfo.of_info_map(
               ~probe_all=CoreSettings.on.probe_all,
+              ~targets=Id.Map.empty,
               info_map_edit,
             );
           /* Baseline run (no prev) of the original — its incr_eval becomes
            * the cache handed to the incremental run of the edited exp. */
           switch (try_eval(info_slice_orig, elab_orig)) {
           | None
           | Some(StepLimitExceeded) => true
-          | Some(Completed((_, state_before))) =>
+          | Some(LimitedCompleted((_, state_before))) =>
             /* Edited evaluated two ways: incrementally (reusing the baseline's
              * cache) and from scratch (empty prev). These must agree. */
             let fresh = try_eval(info_slice_edit, elab_edit);
```

</details>

<details>
<summary><code>test/evaluator/Test_Evaluator_Properties.re</code> · Some(Completed((e_fresh, _))),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Evaluator_Properties.re level=2 baseBlob=04e5f632404208ac103a43487258a48a65905e94 -->

```diff
@@ -364,8 +372,8 @@ let qcheck_incremental_matches_fresh_after_edit =
               );
             switch (fresh, incr_eval_result) {
             | (
-                Some(Completed((e_fresh, _))),
-                Some(Completed((e_incr, _))),
+                Some(LimitedCompleted((e_fresh, _))),
+                Some(LimitedCompleted((e_incr, _))),
               ) =>
               Equality.semantic.exp(e_fresh, e_incr)
             | _ => true
```

</details>

<details>
<summary><code>test/evaluator/Test_Stepper.re</code> · Completed(float(3.)),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Stepper.re level=2 baseBlob=ed43c08e3a3b7e64f27e6bd37a63f5af9d6b40f1 -->

```diff
@@ -18,7 +18,7 @@ let tests = (
         Alcotest.check(
           step_limited(dhexp_typ),
           "1. +. 2. = 3.",
-          Completed(float(3.)),
+          LimitedCompleted((float(3.), EvaluatorState.empty)),
           result,
         );
       },
```

</details>

<details>
<summary><code>test/evaluator/Test_Stepper.re</code> · Completed(int(6)),</summary>

<!-- changetour:hunk file=test/evaluator/Test_Stepper.re level=2 baseBlob=ed43c08e3a3b7e64f27e6bd37a63f5af9d6b40f1 -->

```diff
@@ -46,7 +46,7 @@ let tests = (
         Alcotest.check(
           step_limited(dhexp_typ),
           "(fun x -> x + 1)(5)",
-          Completed(int(6)),
+          LimitedCompleted((int(6), EvaluatorState.empty)),
           result,
         );
       },
```

</details>

<!-- changetour:excluded-section -->

<!-- changetour:exclude file=src/language/dynamics/EvalInfoMap.re -->
<!-- changetour:exclude file=src/language/dynamics/state/StateSlice.re -->
