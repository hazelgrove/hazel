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
    (
      "B2T2 / Table API / Properties / nrows",
      [%blob "table-api-properties-nrows.hz"],
    ),
    (
      "B2T2 / Table API / Properties / ncols",
      [%blob "table-api-properties-ncols.hz"],
    ),
    (
      "B2T2 / Table API / Properties / header",
      [%blob "table-api-properties-header.hz"],
    ),
    (
      "B2T2 / Table API / Access Subcomponents / getRow",
      [%blob "table-api-access-subcomponents-getrow.hz"],
    ),
    (
      "B2T2 / Table API / Access Subcomponents / getValue",
      [%blob "table-api-access-subcomponents-getvalue.hz"],
    ),
    (
      "B2T2 / Table API / Access Subcomponents / getColumn",
      [%blob "table-api-access-subcomponents-getcolumn.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / selectRows",
      [%blob "table-api-subtable-selectrows.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / selectColumns",
      [%blob "table-api-subtable-selectcolumns.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / head",
      [%blob "table-api-subtable-head.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / distinct",
      [%blob "table-api-subtable-distinct.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / dropColumn",
      [%blob "table-api-subtable-dropcolumn.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / dropColumns",
      [%blob "table-api-subtable-dropcolumns.hz"],
    ),
    (
      "B2T2 / Table API / Subtable / tfilter",
      [%blob "table-api-subtable-tfilter.hz"],
    ),
    (
      "B2T2 / Table API / Ordering / tsort",
      [%blob "table-api-ordering-tsort.hz"],
    ),
    (
      "B2T2 / Table API / Ordering / sortByColumns",
      [%blob "table-api-ordering-sortbycolumns.hz"],
    ),
    (
      "B2T2 / Table API / Ordering / orderBy",
      [%blob "table-api-ordering-orderby.hz"],
    ),
    (
      "B2T2 / Table API / Aggregate / count",
      [%blob "table-api-aggregate-count.hz"],
    ),
    (
      "B2T2 / Table API / Aggregate / bin",
      [%blob "table-api-aggregate-bin.hz"],
    ),
    (
      "B2T2 / Table API / Aggregate / pivotTable",
      [%blob "table-api-aggregate-pivottable.hz"],
    ),
    (
      "B2T2 / Table API / Aggregate / groupBy",
      [%blob "table-api-aggregate-groupby.hz"],
    ),
    (
      "B2T2 / Table API / Missing Values / completeCases",
      [%blob "table-api-missing-values-completecases.hz"],
    ),
    (
      "B2T2 / Table API / Missing Values / dropna",
      [%blob "table-api-missing-values-dropna.hz"],
    ),
    (
      "B2T2 / Table API / Missing Values / fillna",
      [%blob "table-api-missing-values-fillna.hz"],
    ),
    (
      "B2T2 / Table API / Data Cleaning / pivotLonger",
      [%blob "table-api-data-cleaning-pivotlonger.hz"],
    ),
    (
      "B2T2 / Table API / Data Cleaning / pivotWider",
      [%blob "table-api-data-cleaning-pivotwider.hz"],
    ),
    (
      "B2T2 / Table API / Utilities / flatten",
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
      "B2T2 / Errors / Using Tables / midFinal",
      [%blob "errors-using-tables-midfinal.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / blackAndWhite",
      [%blob "errors-using-tables-blackandwhite.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / pieCount",
      [%blob "errors-using-tables-piecount.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / brownGetAcne",
      [%blob "errors-using-tables-browngetacne.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / favoriteColor",
      [%blob "errors-using-tables-favoritecolor.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / brownJellybeans",
      [%blob "errors-using-tables-brownjellybeans.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / employeeToDepartment",
      [%blob "errors-using-tables-employeetodepartment.hz"],
    ),
    (
      "B2T2 / Errors / Using Tables / getOnlyRow",
      [%blob "errors-using-tables-getonlyrow.hz"],
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
