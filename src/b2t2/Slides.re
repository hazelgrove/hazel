/* B2T2 benchmark slides: the committed .hz files in
 * hazel-programs/docs/b2t2 ARE the slides — embedded at compile time,
 * parsed at load. The Datasheet slide is markdown (ppx_blob via
 * Datasheet.re), not a program, and keeps its own path. */
let text_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [
    ("B2T2 / Example Tables", [%blob "example-tables.hz"]),
    (
      "B2T2 / Table API / Constructors / emptyTable",
      [%blob "table-api-constructors-emptytable.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / addRows",
      [%blob "table-api-constructors-addrows.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / addColumn",
      [%blob "table-api-constructors-addcolumn.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / buildColumn",
      [%blob "table-api-constructors-buildcolumn.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / vcat",
      [%blob "table-api-constructors-vcat.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / hcat",
      [%blob "table-api-constructors-hcat.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / values",
      [%blob "table-api-constructors-values.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / crossJoin",
      [%blob "table-api-constructors-crossjoin.hz"],
    ),
    (
      "B2T2 / Table API / Constructors / leftJoin",
      [%blob "table-api-constructors-leftjoin.hz"],
    ),
    ("B2T2 / Table API / Properties", [%blob "table-api-properties.hz"]),
    (
      "B2T2 / Table API / Access Subcomponents",
      [%blob "table-api-access-subcomponents.hz"],
    ),
    ("B2T2 / Table API / Subtable", [%blob "table-api-subtable.hz"]),
    ("B2T2 / Table API / Ordering", [%blob "table-api-ordering.hz"]),
    ("B2T2 / Table API / Aggregate", [%blob "table-api-aggregate.hz"]),
    (
      "B2T2 / Table API / Missing Values",
      [%blob "table-api-missing-values.hz"],
    ),
    (
      "B2T2 / Table API / Data Cleaning",
      [%blob "table-api-data-cleaning.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / Flatten",
      [%blob "table-api-utilities-flatten.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / transformColumn",
      [%blob "table-api-utilities-transformcolumn.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / renameColumns",
      [%blob "table-api-utilities-renamecolumns.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / find",
      [%blob "table-api-utilities-find.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / groupByRetentive",
      [%blob "table-api-utilities-groupbyretentive.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / groupBySubtractive",
      [%blob "table-api-utilities-groupbysubtractive.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / update",
      [%blob "table-api-utilities-update.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / select",
      [%blob "table-api-utilities-select.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / selectMany",
      [%blob "table-api-utilities-selectmany.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / groupJoin",
      [%blob "table-api-utilities-groupjoin.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / join",
      [%blob "table-api-utilities-join.hz"],
    ),
    (
      "B2T2 / Example Programs / Dot Product",
      [%blob "example-programs-dot-product.hz"],
    ),
    (
      "B2T2 / Example Programs / pHackingHomogeneous",
      [%blob "example-programs-phackinghomogeneous.hz"],
    ),
    (
      "B2T2 / Example Programs / pHackingHeterogeneous",
      [%blob "example-programs-phackingheterogeneous.hz"],
    ),
    (
      "B2T2 / Example Programs / quizScoreFilter",
      [%blob "example-programs-quizscorefilter.hz"],
    ),
    (
      "B2T2 / Example Programs / quizScoreSelect",
      [%blob "example-programs-quizscoreselect.hz"],
    ),
    (
      "B2T2 / Example Programs / groupByRetentive",
      [%blob "example-programs-groupbyretentive.hz"],
    ),
    (
      "B2T2 / Example Programs / groupBySubtractive",
      [%blob "example-programs-groupbysubtractive.hz"],
    ),
    (
      "B2T2 / Errors / Malformed Tables",
      [%blob "errors-malformed-tables.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / Part 1",
      [%blob "errors-using-tables-part-1.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / Part 2",
      [%blob "errors-using-tables-part-2.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / Part 3",
      [%blob "errors-using-tables-part-3.hz"],
    ),
  ]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );

let all_slides = [
  (
    "B2T2 / Datasheet",
    Haz3lcore.PersistentZipper.of_slide_text(Datasheet.slide_text),
  ),
  ...text_slides,
];
