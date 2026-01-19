/* Adventure Mode: Gate Predicate Evaluation
 *
 * Evaluates predicates that determine when a user has completed
 * a tutorial task. Predicates can check for probes, term structure,
 * and combinations thereof.
 */

open Haz3lcore;
open Language;

/* Get the text representation of the editor content */
let get_editor_text = (zipper: Zipper.t): string =>
  Printer.of_zipper(~holes="", ~indent="", zipper);

/* Check if the zipper has any manual probes */
let has_any_probe = (zipper: Zipper.t): bool =>
  !Id.Map.is_empty(zipper.refractors.manuals);

/* Check if there's a probe on the currently indicated term */
let has_probe_on_indicated =
    (~zipper: Zipper.t, ~info_map: Statics.Map.t): bool =>
  switch (Indicated.index(zipper)) {
  | None => false
  | Some(id) =>
    /* Check if this ID or any of its target subterm IDs have a probe */
    let target_ids = ProbePerform.target_subterm_ids(id, info_map);
    List.exists(
      target_id => Id.Map.mem(target_id, zipper.refractors.manuals),
      target_ids,
    )
    || Id.Map.mem(id, zipper.refractors.manuals);
  };

/* Main predicate evaluation */
let rec check =
        (
          ~zipper: Zipper.t,
          ~info_map: Statics.Map.t,
          predicate: Adventure.gate_predicate,
        )
        : bool =>
  switch (predicate) {
  | HasAnyProbe => has_any_probe(zipper)

  | HasProbeOnIndicated => has_probe_on_indicated(~zipper, ~info_map)

  | TextContains(substring) =>
    let text = get_editor_text(zipper);
    /* Check if text contains substring */
    try({
      let _ = Str.search_forward(Str.regexp_string(substring), text, 0);
      true;
    }) {
    | Not_found => false
    };

  | TextEquals(expected) =>
    let text = get_editor_text(zipper);
    text == expected;

  | TermSatisfies(_description) =>
    /* For now, TermSatisfies is a placeholder that always passes.
     * Real term checking would require pattern matching on the AST.
     * This can be extended with specific checks as needed. */
    true

  | And(predicates) =>
    List.for_all(p => check(~zipper, ~info_map, p), predicates)

  | Or(predicates) =>
    List.exists(p => check(~zipper, ~info_map, p), predicates)
  };
