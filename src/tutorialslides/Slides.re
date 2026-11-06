/* Tutorial-mode lesson sources: the committed .hzt files in
 * hazel-programs/tutorial ARE the slides — embedded at compile time
 * (ppx_blob), parsed into Tutorial.spec records at startup by
 * Web.TutorialText. Slide order is this list's order; a lesson's folder
 * comes from its @title, and lessons of one folder are kept adjacent here.
 * Adding a slide = add the file and a line here. */
let all: list((string, string)) = [
  ("01-holes.hzt", [%blob "01-holes.hzt"]),
  (
    "02-the-tylr-parser-and-backpack.hzt",
    [%blob "02-the-tylr-parser-and-backpack.hzt"],
  ),
  ("03-integer-arithmetic.hzt", [%blob "03-integer-arithmetic.hzt"]),
  (
    "04-floating-point-arithmetic.hzt",
    [%blob "04-floating-point-arithmetic.hzt"],
  ),
  ("05-let-bindings.hzt", [%blob "05-let-bindings.hzt"]),
  ("06-probes.hzt", [%blob "06-probes.hzt"]),
  ("07-type-annotations.hzt", [%blob "07-type-annotations.hzt"]),
  ("08-functions.hzt", [%blob "08-functions.hzt"]),
  (
    "09-multi-argument-functions.hzt",
    [%blob "09-multi-argument-functions.hzt"],
  ),
  ("10-partial-application.hzt", [%blob "10-partial-application.hzt"]),
  ("11-pipelining.hzt", [%blob "11-pipelining.hzt"]),
  ("12-if-expressions.hzt", [%blob "12-if-expressions.hzt"]),
  ("13-case-expressions.hzt", [%blob "13-case-expressions.hzt"]),
  ("14-list-literals.hzt", [%blob "14-list-literals.hzt"]),
  ("15-mapping-over-lists.hzt", [%blob "15-mapping-over-lists.hzt"]),
  ("16-folding-lists.hzt", [%blob "16-folding-lists.hzt"]),
  (
    "17-mean-of-string-integers.hzt",
    [%blob "17-mean-of-string-integers.hzt"],
  ),
  ("18-labeled-tuples.hzt", [%blob "18-labeled-tuples.hzt"]),
  (
    "19-labeled-tuple-projection.hzt",
    [%blob "19-labeled-tuple-projection.hzt"],
  ),
  (
    "20-labeled-tuple-extension.hzt",
    [%blob "20-labeled-tuple-extension.hzt"],
  ),
  ("21-labeled-tuple-omission.hzt", [%blob "21-labeled-tuple-omission.hzt"]),
  (
    "22-labeled-tuple-list-conversions.hzt",
    [%blob "22-labeled-tuple-list-conversions.hzt"],
  ),
  ("23-tables.hzt", [%blob "23-tables.hzt"]),
  (
    "24-table-column-projection.hzt",
    [%blob "24-table-column-projection.hzt"],
  ),
  ("25-rich-probes.hzt", [%blob "25-rich-probes.hzt"]),
  ("26-gradebook-midterm-mean.hzt", [%blob "26-gradebook-midterm-mean.hzt"]),
  (
    "27-gradebook-overall-grade.hzt",
    [%blob "27-gradebook-overall-grade.hzt"],
  ),
  ("28-gradebook-tidy-term.hzt", [%blob "28-gradebook-tidy-term.hzt"]),
  /* The probes tutorial (folder "Probes", set by each lesson's @title).
     Filenames here are unnumbered, so THIS LIST is their only ordering. */
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
  ("mapping-over-a-list.hzt", [%blob "mapping-over-a-list.hzt"]),
  ("folding-over-a-list.hzt", [%blob "folding-over-a-list.hzt"]),
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
