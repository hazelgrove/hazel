/* Tutorial-mode lesson sources: the committed .hzt files in
 * hazel-programs/tutorial ARE the slides — embedded at compile time
 * (ppx_blob), parsed into Tutorial.spec records at startup by
 * Web.TutorialText. Slide order is this list's order (filenames are
 * unnumbered; THIS LIST is the only ordering).
 * Adding a slide = add the file and a line here. */
let all: list((string, string)) = [
  ("intro.hzt", [%blob "intro.hzt"]),
  ("arithmetic-and-holes.hzt", [%blob "arithmetic-and-holes.hzt"]),
  ("the-backpack.hzt", [%blob "the-backpack.hzt"]),
  (
    "adding-and-removing-probes.hzt",
    [%blob "adding-and-removing-probes.hzt"],
  ),
  ("environment-explorer.hzt", [%blob "environment-explorer.hzt"]),
  ("tuples-and-records.hzt", [%blob "tuples-and-records.hzt"]),
  ("if-expressions.hzt", [%blob "if-expressions.hzt"]),
  ("case-expressions.hzt", [%blob "case-expressions.hzt"]),
  ("constructors-with-data.hzt", [%blob "constructors-with-data.hzt"]),
  ("samples-per-call.hzt", [%blob "samples-per-call.hzt"]),
  ("aligning-samples.hzt", [%blob "aligning-samples.hzt"]),
  ("auto-probe.hzt", [%blob "auto-probe.hzt"]),
  ("reading-bigger-values.hzt", [%blob "reading-bigger-values.hzt"]),
  ("map.hzt", [%blob "map.hzt"]),
  ("fold.hzt", [%blob "fold.hzt"]),
  ("pinning-calls.hzt", [%blob "pinning-calls.hzt"]),
  ("stepping-into-calls.hzt", [%blob "stepping-into-calls.hzt"]),
  ("print-statements.hzt", [%blob "print-statements.hzt"]),
  ("tasks-ahead.hzt", [%blob "tasks-ahead.hzt"]),
  ("task-dew-ledger.hzt", [%blob "task-dew-ledger.hzt"]),
  ("task-grove-name.hzt", [%blob "task-grove-name.hzt"]),
  ("task-watering-timer.hzt", [%blob "task-watering-timer.hzt"]),
  ("task-running-sum.hzt", [%blob "task-running-sum.hzt"]),
  ("task-planting-bug.hzt", [%blob "task-planting-bug.hzt"]),
  ("task-log-cleaner.hzt", [%blob "task-log-cleaner.hzt"]),
  ("task-harvest-streak.hzt", [%blob "task-harvest-streak.hzt"]),
  ("task-crop-plotter.hzt", [%blob "task-crop-plotter.hzt"]),
  ("task-growth-plotter.hzt", [%blob "task-growth-plotter.hzt"]),
  ("bonus-sample-colors.hzt", [%blob "bonus-sample-colors.hzt"]),
];
