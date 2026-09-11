/* Tutorial-mode lesson sources: the committed .hzt files in
 * hazel-programs/tutorial ARE the slides — embedded at compile time
 * (ppx_blob), parsed into Tutorial.spec records at startup by
 * Web.TutorialText. Slide order is this list's order (filename sort).
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
];
