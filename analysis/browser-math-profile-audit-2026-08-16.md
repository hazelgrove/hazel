# Math Profile Browser Test Checklist

Date: 2026-08-16

This checklist covers the recent One Step profile-inheritance work and the related exercise, Math Mode Builder, and Rocq checks. Each section contains a short manual test, the expected behavior, and the result observed during the automated browser audit.

Headless browser interaction with Hazel's SVG editor is less reliable than a manual check. If your result differs from the recorded result, capture the expression, selected subexpression, active profile, automation mode, final status text, and browser console.

## Before testing

1. Build and serve the current branch normally.
2. Open Hazel in Chrome and open Developer Tools.
3. Select **Default levels** in the Console so warnings and errors remain
   visible.
4. When testing a rewrite, wait for the displayed `Checking…` state to finish
   before pressing **Replace**.
5. For a useful performance check, type normally instead of pasting the whole
   target. Typing should remain responsive while the checker is debounced.

## 1. FOIL with Algebraic Cleanup latency and granularity

- [ ] Open **Exercises**, exercise 3, **FOIL with Algebraic Cleanup**.
- [ ] Select only `(2 * x - 3) * (x + 4)` on the left side.
- [ ] Choose **One Step**.
- [ ] Type this target:

  ```text
  2 * x * x - 3 * x + 8 * x - 12
  ```

- [ ] Confirm typing remains responsive.
- [ ] Confirm `Checking…` appears during the one-second debounce.
- [ ] Confirm the result becomes `Valid` and offers **Replace**.
- [ ] Confirm the route mentions polynomial expansion/distribution rather than
  ```
  an unrelated broader tactic.
  ```

Expected: typing is immediate, checking begins after roughly one second of
inactivity, and the written-out expansion is accepted as one visible Algebra
step.

Observed: **Pass.** Typing 25 characters with a 25 ms scripted delay took 588
ms, so there was no per-keystroke checker stall. The result became `Valid`
about 1.0 seconds later. The reported route was `Expand a polynomial product → Distribute multiplication over addition`.

### Confirmed follow-up failure: repeated factor to power notation

- [ ] After replacing the whole-product expansion, select only `x * x` in the
  ```
  resulting `2 * x * x` term.
  ```
- [ ] Choose **One Step** and enter:
  ```text
  x ** 2
  ```

Expected: this is automatic Algebra power-notation cleanup and should become
`Valid` after the one-second debounce.

Observed: **Fail.** This remained on `Checking…` beyond 12 seconds in a fresh
isolated browser, matching the manual Chrome screenshot. The expression was
well typed as `Real`. The exercise profile retains the Power Notation cleanup,
and the synchronous cleanup checker accepts `x * x → x ** 2`. The live One Step
path does not run direct power-notation cleanup, falls through to incremental
catalog search, and then fails to return either a result or its configured
eight-second timeout to the UI. This is not an invalid student step.

## 2. Changing math mode in instructor mode

- [ ] Enable instructor mode using the graduation-cap control.
- [ ] Open **Math Mode Builder** on a selected expression.
- [ ] Change **Algebra / One Step** to **Trigonometry / Check Result**.
- [ ] Close and reopen the panel and confirm the selection remains active.
- [ ] Return to student mode and confirm the instructor-only controls disappear
  ```
  without removing or corrupting the exercise.
  ```

Expected: both selectors update the active stepper configuration. Student mode
hides the configuration controls while preserving the exercise.

Observed: **Pass in the active session.** Both selector values updated, and
student mode hid the instructor controls. A full page-reload persistence check
was not completed, so please pay special attention to the close/reopen and
reload behavior.

## 3. Factor button visibility and profile override

### Expression-shape behavior

- [ ] In a Real-valued scratch expression, select:

  ```text
  x ** 2 + 3 * x + 2
  ```

- [ ] Open **Search** and confirm **Factor** is visible.
- [ ] Repeat with:

  ```text
  x ** 2 + x + 1
  ```

- [ ] Confirm **Factor** is not visible for the nonfactorable expression.
- [ ] Confirm **Simplify** remains available.

### Profile behavior

- [ ] In Math Mode Builder, start from Algebra.
- [ ] Disable `alg.factor_polynomial_normalize`.
- [ ] Activate the custom mode and select the factorable quadratic again.
- [ ] Confirm **Factor** is hidden.
- [ ] Re-enable the capability and confirm **Factor** returns.
- [ ] Select Trigonometry and confirm Algebra's Factor capability is inherited.

Expected: Factor depends on both expression shape and the effective profile.

Observed: **Expression-shape pass; custom override needs a manual check.** The
button appeared for `x ** 2 + 3 * x + 2` and was absent for
`x ** 2 + x + 1`. Unit tests pass for disabled profile overrides and inheritance
through Trigonometry and Calculus, but the explicit custom-profile toggle was
not completed in the browser audit.

## 4. Completing the Square parentheses and reflexivity

- [ ] Open **Exercises**, exercise 4, **Completing the Square**.
- [ ] Select the complete left side:

  ```text
  x ** 2 + 6 * x + 5
  ```

- [ ] Open **Search** and enter:

  ```text
  (x + 3) ** 2 - 4
  ```

- [ ] Wait until the panel says `Ready`, then press **Run Rocq Search**. Merely
  ```
  reaching `Ready` does not start this search.
  ```
- [ ] Confirm the result becomes `Valid`, then press **Replace**.
- [ ] Confirm the proof reaches:

  ```text
  (x + 3) ** 2 - 4 == (x + 3) ** 2 - 4
  ```

- [ ] Select the equality, apply reflexivity, and confirm the theorem reaches
  ```
  `true` and is marked proven.
  ```

Expected: parentheses may be removed and reintroduced in trace rows, but the
selected expression remains associated correctly and reflexivity closes the
final equality.

Observed: **Mostly pass.** Search returned `Valid` through polynomial expansion
and collection, and replacement reached the exact reflexive equality. The
headless audit did not perform the final reflexivity click. A previous manual
run reached `reflexivity`, `true`, and `proven true`.

## 5. First FOIL exercise granularity and parentheses

- [ ] Open **Exercises**, exercise 2, **FOIL, Written Out**.
- [ ] Expand only one product/distribution layer at a time.
- [ ] Confirm commuting factors is allowed where needed.
- [ ] Confirm collecting all like terms is not offered as an automatic shortcut
  ```
  in this deliberately verbose exercise.
  ```
- [ ] Select a middle or suffix portion of an additive chain and confirm Hazel
  ```
  can reassociate the expression without losing the selected chunk.
  ```
- [ ] Finish the proof and confirm the final reflexive equality closes normally.

Expected: primitive distribution and factor commutation are allowed, while
automatic collection and broad polynomial expansion are disabled by this
exercise's policy.

Observed: **Automated policy pass; full browser walkthrough outstanding.** The
policy and reparenthesization unit tests pass, but this entire proof was not
replayed headlessly because structural SVG selection is unreliable. This is a
particularly useful manual regression check.

## 6. Theorem variables resolve in rewrite targets

- [ ] In either FOIL theorem, select a subexpression containing `x`.
- [ ] Open One Step or Search and type a target that also contains `x`.
- [ ] Confirm the status does not say `x not found`.
- [ ] Repeat inside the Completing the Square theorem.
- [ ] As a control, type an actually unbound variable in Scratch and confirm the
  ```
  normal `variable not found` diagnostic still appears there.
  ```

Expected: theorem-bound variables are available to the target editor; genuinely
unbound scratch variables still receive diagnostics.

Observed: **Pass.** The theorem exercises resolved `x` correctly. No theorem
context `variable not found` error was reproduced. An intentionally unbound
scratch expression still produced the expected diagnostic.

## 7. Math mode library export and import

- [ ] Open **Math Mode Builder** and create a mode based on Trigonometry.
- [ ] Give it a unique name, activate it, and save it.
- [ ] Press **Export library JSON** and save or copy the generated JSON.
- [ ] Import that JSON into the same session.
- [ ] Confirm Hazel reports a name conflict.
- [ ] Choose **Replace conflicts**.
- [ ] Confirm the mode is imported, loadable, and retains its base level and
  ```
  reviewed configuration.
  ```

Expected: the JSON is versioned, conflicts require an explicit choice, and
replacement restores the saved definition.

Observed: **Pass.** Export produced versioned JSON. Reimport reported the
expected conflict, and `Replace conflicts` ended with `Imported Browser audit trig mode`. The imported mode was loadable and retained Trigonometry.

## 8. Custom trig rewrite and `Admitted` proof export

- [ ] In Math Mode Builder, start from Trigonometry.
- [ ] Add this custom rewrite:

  ```text
  Source: sin($a) * cos($b)
  Target: (sin($a + $b) + sin($a - $b)) / 2
  ```

- [ ] Confirm it is labelled **Untrusted session rewrite** and **One Step only**.
- [ ] Confirm the UI warns that proof export will place it in an UNSOUND section
  ```
  using `Admitted`.
  ```
- [ ] Activate the mode and apply the rewrite to a matching Real-valued trig
  ```
  expression.
  ```
- [ ] Export the Rocq proof.
- [ ] Search the export for the custom lemma and confirm its proof is exactly an

  ```
  admitted proof, for example:
  ```

  ```coq
  Lemma ... .
  Proof.
  Admitted.
  ```

- [ ] Confirm the rewrite is not silently persisted as a reviewed/trusted rule
  ```
  in ordinary Math Mode library JSON.
  ```

Expected: session rewrites are usable manually, omitted from trusted library
definitions, and isolated as `Admitted` lemmas in proof export.

Observed: **Browser workflow currently blocked.** Creation, activation, labels,
and warnings worked. Unit tests confirm the UNSOUND exported lemma contains
`Admitted.` The browser incorrectly classified a matching trig target as
`Needs Functions/lists`, so the rewrite could not be applied and exported end
to end. This should be treated as a real bug unless the manual run behaves
differently.

## 9. Trigonometry One Step inheritance

### Arithmetic inherited by Trigonometry

- [ ] In Scratch, enter:

  ```text
  explore 1 + 2 + 3 + sin(x) ** 2 end
  ```

- [ ] Select the complete expression.
- [ ] Choose **Trigonometry / One Step**.
- [ ] Enter:

  ```text
  6 + sin(x) ** 2
  ```

- [ ] Confirm the target remains responsive while typing.
- [ ] Confirm the result becomes `Valid` after the debounce.
- [ ] Confirm the route uses inherited constant evaluation and does not consume
  ```
  a trig-identity step.
  ```

If the raw Scratch expression produces ambiguous Int/Real diagnostics, repeat
the check with an explicitly Real-bound expression:

```text
explore use Real in
  (fun (x : Real) -> 1 + 2 + 3 + sin_real(x) ** 2)
end
```

Select only the function body and enter:

```text
6 + sin_real(x) ** 2
```

Expected: Trigonometry inherits arbitrary enabled Arithmetic and Algebra cleanup
as background work. One Step limits foreground Trigonometry identities, not
lower-level arithmetic.

Observed: **Backend pass, browser fail.** The exact raw example remained on
`Checking…`. The explicitly Real version was well typed at the source but the
target was incorrectly rejected as `Needs Functions/lists`. The backend
profile-planning test accepts this transformation.

### Algebra inherited by Trigonometry

- [ ] Under **Trigonometry / One Step**, select:

  ```text
  x * (y + z)
  ```

- [ ] Enter:

  ```text
  x * y + x * z
  ```

- [ ] Confirm it becomes `Valid` with a distribution route.

Observed: **Pass.** The browser returned `Valid` with `Distribute multiplication over addition`.

### Foreground limit remains enforced

- [ ] Try a target requiring two separate Pythagorean trig-identity uses in the
  ```
  same Trigonometry One Step action.
  ```
- [ ] Confirm it is rejected.
- [ ] Switch to Calculus and confirm multiple inherited trig identities may be
  ```
  background work while only one derivative rule is permitted.
  ```

Expected: inheritance does not remove the one-foreground-step budget for the
active level.

Observed: **Unit-test pass; manual browser check requested.** Tests cover the
positive and negative Trig cases, inherited Trig in Calculus, and rejection of
two derivative rules.

## Issues to file if manually reproduced

1. Trig function applications in a One Step target are incorrectly routed to
   `Functions/lists`.
2. An invalid or unsupported target can remain on `Checking…` indefinitely
   instead of terminating with a diagnostic.
3. Custom untrusted trig rewrites cannot complete the browser apply/export
   workflow because of the same target-classification issue.
4. Browser-parity tests should use elaborated Real trig expressions, not only
   internal integer expression constructors.
5. If Math Mode changes disappear after closing/reopening or reloading, profile
   selection persistence needs a separate fix.
6. Exercise 3 One Step must route cleanup-only `x * x → x ** 2` through the
   profile's direct Power Notation cleanup and must always surface incremental
   planner completion, rejection, or timeout.
