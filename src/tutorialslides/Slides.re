/* Tutorial-mode lesson sources: the committed .hzt files in
 * hazel-programs/tutorial ARE the slides — embedded at compile time
 * (ppx_blob), parsed into Tutorial.spec records at startup by
 * Web.TutorialText. Slide order is this list's order (filename sort).
 * Adding a slide = add the file and a line here. */
let all: list((string, string)) = [
  ("00-intro.hzt", [%blob "00-intro.hzt"]),
  ("01-arithmetic-and-holes.hzt", [%blob "01-arithmetic-and-holes.hzt"]),
  ("02-parser-and-backpack.hzt", [%blob "02-parser-and-backpack.hzt"]),
  (
    "03-adding-and-removing-probes.hzt",
    [%blob "03-adding-and-removing-probes.hzt"],
  ),
  ("04-environment-explorer.hzt", [%blob "04-environment-explorer.hzt"]),
  ("05-tuples-and-records.hzt", [%blob "05-tuples-and-records.hzt"]),
  ("07-if-expressions.hzt", [%blob "07-if-expressions.hzt"]),
  ("08-case-expressions.hzt", [%blob "08-case-expressions.hzt"]),
  ("09-constructors-with-data.hzt", [%blob "09-constructors-with-data.hzt"]),
  ("10-samples-per-call.hzt", [%blob "10-samples-per-call.hzt"]),
  ("11-aligning-samples.hzt", [%blob "11-aligning-samples.hzt"]),
  ("12-auto-probe.hzt", [%blob "12-auto-probe.hzt"]),
  ("13-reading-bigger-values.hzt", [%blob "13-reading-bigger-values.hzt"]),
  ("14-map.hzt", [%blob "14-map.hzt"]),
  ("15-fold.hzt", [%blob "15-fold.hzt"]),
  ("16-pin.hzt", [%blob "16-pin.hzt"]),
  ("17-step-into.hzt", [%blob "17-step-into.hzt"]),
  ("20-print.hzt", [%blob "20-print.hzt"]),
  ("25-tasks-ahead.hzt", [%blob "25-tasks-ahead.hzt"]),
  ("26-task-dew-ledger.hzt", [%blob "26-task-dew-ledger.hzt"]),
  ("27-task-grove-name.hzt", [%blob "27-task-grove-name.hzt"]),
  ("28-task-watering-timer.hzt", [%blob "28-task-watering-timer.hzt"]),
  ("29-task-running-sum.hzt", [%blob "29-task-running-sum.hzt"]),
  ("30-task-planting-bug.hzt", [%blob "30-task-planting-bug.hzt"]),
  ("31-task-log-cleaner.hzt", [%blob "31-task-log-cleaner.hzt"]),
  ("32-task-harvest-streak.hzt", [%blob "32-task-harvest-streak.hzt"]),
  ("33-task-crop-plotter.hzt", [%blob "33-task-crop-plotter.hzt"]),
  ("34-task-growth-plotter.hzt", [%blob "34-task-growth-plotter.hzt"]),
  ("39-extra-sample-colors.hzt", [%blob "39-extra-sample-colors.hzt"]),
];
