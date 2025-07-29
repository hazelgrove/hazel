let out : string * Haz3lcore.PersistentZipper.t =
  ( "[B2T2] Datasheet",
    {
      zipper =
        "((selection((focus Left)(content())(mode \
         Normal)))(relatives((siblings(()((Projector((id \
         ef53e5e1-af08-417d-9a78-51d91d95c3cf)(kind TextArea)(syntax(Tile((id \
         2f1b25fd-2cf1-4e85-8f05-ccad01262e3e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         3ce0ea00-f178-4ccc-aec6-6597a4809970)(label(\"\\\"## \
         Reference\\\\n\\\\n> Q. Where can we learn about the programming \
         medium covered by this datasheet?\\\\n> (Feel free to link to \
         multiple kinds of artifacts: repositories, papers, videos, etc.\\\\n> \
         Please also include version information where applicable.)\\\\n\\\\n- \
         **Website**: http://hazel.org  \\\\n- **Source Code**: \
         https://github.com/hazelgrove/hazel *(TODO: Add version)*  \\\\n- \
         **App**: https://hazel.org/build/dev/  \\\\n\\\\n> Q. What is the URL \
         of the version of the benchmark being \
         used?\\\\nhttps://github.com/brownplt/B2T2/blob/fd227efadf532a20aefd25c7a8580978c2d684a2/Datasheet.md  \
         \\\\n\\\\n\\\\n> Q. On what date was this version of the datasheet \
         last updated?\\\\n2024-06-13\\\\n\\\\n> Q. If you are not using the \
         latest benchmark available on that date, please explain why \
         not.\\\\nYes\\\\n\\\\n## Example Tables\\\\n\\\\n> Q. Do tables \
         express heterogeneous data, or must data be homogenized?\\\\n  Hazel \
         tables are represented as *lists of labeled tuples*.  \\\\n  - \
         Columns may be heterogeneously typed.   \\\\n  - Rows must be \
         homogeneously typed.\\\\n    - The unknown type allows some degree of \
         heterogenous rows.\\\\n\\\\n> Q. Do tables capture missing data and, \
         if so, how? Do missing values affect the output constraints of any \
         operations, for example `groupBy`?\\\\n  - Represented via `Option` \
         types (`Some` / `None`)  \\\\n  - Incomplete programs can use \
         expression holes (not programmatically accessible)  \\\\n  - No \
         special handling in operations \\226\\128\\148 `Option` values are \
         ordinary\\\\n\\\\n> Q. Are mutable tables supported? Are there any \
         limitations?\\\\nMutable tables are not supported\\\\n\\\\n> You may \
         reference, instead of duplicating, the responses to the above \
         questions in answering those below:\\\\n\\\\n> Q. Which tables are \
         inexpressible? Why?\\\\nNone \\226\\128\\148 all tables can be \
         expressed using `Option` types for missing data\\\\n\\\\n> Q. Which \
         tables are only partially expressible? Why, and what\\226\\128\\153s \
         missing?\\\\nN/A\\\\n\\\\n> Q. Which tables\\226\\128\\153 \
         expressibility is unknown? Why?\\\\nN/A\\\\n\\\\n> Q. Which tables \
         can be expressed more precisely than in the benchmark? How?\\\\nNone \
         - hazel represents the tables as precisely as the benchmark. Once \
         again explicit option types make optional columns \
         explicit.\\\\n\\\\n> Q. How direct is the mapping from the tables in \
         the benchmark to representations in your system? How complex is the \
         encoding?\\\\n  - Very direct  \\\\n  - Benchmark tables map \
         naturally to Hazel's `List of Labeled Tuples`  \\\\n  - Missing \
         values use `Option`\\\\n  - Nested tables use nested labeled tuples \
         or lists\\\\n\\\\n## TableAPI\\\\n\\\\n> Q. Are there consistent \
         changes made to the way the operations are represented?\\\\nThe \
         operations are mostly presented as depicted, but here are a few \
         variations:\\\\n- Some operations utilize explicity polymorphism in \
         Hazel using the `typfun` keyword to require explicit type \
         application\\\\n  as implicit polymorphism has not been added to \
         Hazel as of 2025-07-08\\\\n- Hazel tables are represented using lists \
         of labeled tuples so there is no runtime schema available for \
         operations.\\\\n  For certain operations, such as `leftJoin`, this \
         requires looking at the head element to determine the schema \
         and\\\\n  give some behavior in the event no such element \
         exists.\\\\n- Certain operations have been made to return an optional \
         value rather than an error\\\\n- Hazel does not have first-class \
         labels, and therefore uses strings for columns for some of the \
         operations.\\\\n  If the operation was done inline primitive \
         operators could be used to recover typesafety.\\\\n\\\\n> Q. Which \
         operations are entirely inexpressible? Why?\\\\nAll the operations \
         are at least partially expressible.\\\\n\\\\n> Q. Which operations \
         are only partially expressible? Why, and what\\226\\128\\153s \
         missing?\\\\n- `leftJoin` can only build the resulting columns if \
         both tables have at least one row to determine the schema\\\\n- \
         Various operations only work if there's at least one row to determine \
         the schema\\\\n  - ncols, header\\\\n- `dropna` only works if every \
         column in a table is optional since there's no way to dynamically \
         dispatch based off of column sort.\\\\n\\\\n> Q. Which \
         operations\\226\\128\\153 expressibility is unknown? \
         Why?\\\\nN/A\\\\n\\\\n> Q. Which operations can be expressed more \
         precisely than in the benchmark? How?\\\\n- Several operations could \
         be expressed in a more typesafe manner if a projection function was \
         passed instead of a column name.\\\\n  - e.g. `selectColumn(table, \
         fun e -> e.name)` as opposed to `selectColumn(table, \
         'name')`\\\\n\\\\n## Example Programs\\\\n\\\\n> Q. Which examples \
         are inexpressible? Why?\\\\n- sampleRows is inexpressible as Hazel is \
         pure\\\\n\\\\n\\\\n> Q. Which examples\\226\\128\\153 expressibility \
         is unknown? Why?\\\\nN/A\\\\n\\\\n> Q. Which examples, or aspects \
         thereof, can be expressed especially precisely? How?\\\\n\\\\n\\\\n> \
         Q. How direct is the mapping from the pseudocode in the benchmark to \
         representations in your system? How complex is the encoding?\\\\n- \
         The mapping is quite direct as implemented. A less direct mapping \
         could accomplish a more type-safe translation of several of the \
         programs.\\\\n\\\\n## Errors (TODO)\\\\n\\\\n> There are (at least) \
         two parts to errors: representing the source program that causes the \
         error, and generating output that explains it. The term \
         \\226\\128\\156error situation\\226\\128\\157 refers to a \
         representation of the cause of the error in the program source.\\\\n> \
         \\\\n> For each error situation it may be that the language:\\\\n> \
         \\\\n> - isn\\226\\128\\153t expressive enough to capture it\\\\n> - \
         can at least partially express the situation\\\\n> - prevents the \
         program from being constructed\\\\n> \\\\n> Expressiveness, in turn, \
         can be for multiple artifacts:\\\\n> \\\\n> - the buggy versions of \
         the programs\\\\n> - the correct variants of the programs\\\\n> - the \
         type system\\226\\128\\153s representation of the constraints\\\\n> - \
         the type system\\226\\128\\153s reporting of the violation\\\\n\\\\n> \
         Q. Which error situations are known to be inexpressible? \
         Why?\\\\n\\\\n\\\\n> Q. Which error situations are only partially \
         expressible? Why, and what\\226\\128\\153s missing?\\\\n\\\\n\\\\n> \
         Q. Which error situations\\226\\128\\153 expressibility is unknown? \
         Why?\\\\n\\\\n\\\\n> Q. Which error situations can be expressed more \
         precisely than in the benchmark? How?\\\\n\\\\n\\\\n> Q. Which error \
         situations are prevented from being constructed? How?\\\\n\\\\n\\\\n> \
         Q. For each error situation that is at least partially expressible, \
         what is the quality of feedback to the programmer?\\\\n\\\\n\\\\n> Q. \
         For each error situation that is prevented from being constructed, \
         what is the quality of feedback to the programmer?\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))(model\"()\"))))))(ancestors())))(caret \
         Outer))";
      backup_text =
        "^^text(\"## Reference\\n\\n> Q. Where can we learn about the \
         programming medium covered by this datasheet?\\n> (Feel free to link \
         to multiple kinds of artifacts: repositories, papers, videos, \
         etc.\\n> Please also include version information where \
         applicable.)\\n\\n- **Website**: http://hazel.org  \\n- **Source \
         Code**: https://github.com/hazelgrove/hazel *(TODO: Add version)*  \
         \\n- **App**: https://hazel.org/build/dev/  \\n\\n> Q. What is the \
         URL of the version of the benchmark being \
         used?\\nhttps://github.com/brownplt/B2T2/blob/fd227efadf532a20aefd25c7a8580978c2d684a2/Datasheet.md  \
         \\n\\n\\n> Q. On what date was this version of the datasheet last \
         updated?\\n2024-06-13\\n\\n> Q. If you are not using the latest \
         benchmark available on that date, please explain why \
         not.\\nYes\\n\\n## Example Tables\\n\\n> Q. Do tables express \
         heterogeneous data, or must data be homogenized?\\n  Hazel tables are \
         represented as *lists of labeled tuples*.  \\n  - Columns may be \
         heterogeneously typed.   \\n  - Rows must be homogeneously \
         typed.\\n    - The unknown type allows some degree of heterogenous \
         rows.\\n\\n> Q. Do tables capture missing data and, if so, how? Do \
         missing values affect the output constraints of any operations, for \
         example `groupBy`?\\n  - Represented via `Option` types (`Some` / \
         `None`)  \\n  - Incomplete programs can use expression holes (not \
         programmatically accessible)  \\n  - No special handling in \
         operations \226\128\148 `Option` values are ordinary\\n\\n> Q. Are \
         mutable tables supported? Are there any limitations?\\nMutable tables \
         are not supported\\n\\n> You may reference, instead of duplicating, \
         the responses to the above questions in answering those below:\\n\\n> \
         Q. Which tables are inexpressible? Why?\\nNone \226\128\148 all \
         tables can be expressed using `Option` types for missing data\\n\\n> \
         Q. Which tables are only partially expressible? Why, and \
         what\226\128\153s missing?\\nN/A\\n\\n> Q. Which tables\226\128\153 \
         expressibility is unknown? Why?\\nN/A\\n\\n> Q. Which tables can be \
         expressed more precisely than in the benchmark? How?\\nNone - hazel \
         represents the tables as precisely as the benchmark. Once again \
         explicit option types make optional columns explicit.\\n\\n> Q. How \
         direct is the mapping from the tables in the benchmark to \
         representations in your system? How complex is the encoding?\\n  - \
         Very direct  \\n  - Benchmark tables map naturally to Hazel's `List \
         of Labeled Tuples`  \\n  - Missing values use `Option`\\n  - Nested \
         tables use nested labeled tuples or lists\\n\\n## TableAPI\\n\\n> Q. \
         Are there consistent changes made to the way the operations are \
         represented?\\nThe operations are mostly presented as depicted, but \
         here are a few variations:\\n- Some operations utilize explicity \
         polymorphism in Hazel using the `typfun` keyword to require explicit \
         type application\\n  as implicit polymorphism has not been added to \
         Hazel as of 2025-07-08\\n- Hazel tables are represented using lists \
         of labeled tuples so there is no runtime schema available for \
         operations.\\n  For certain operations, such as `leftJoin`, this \
         requires looking at the head element to determine the schema and\\n  \
         give some behavior in the event no such element exists.\\n- Certain \
         operations have been made to return an optional value rather than an \
         error\\n- Hazel does not have first-class labels, and therefore uses \
         strings for columns for some of the operations.\\n  If the operation \
         was done inline primitive operators could be used to recover \
         typesafety.\\n\\n> Q. Which operations are entirely inexpressible? \
         Why?\\nAll the operations are at least partially expressible.\\n\\n> \
         Q. Which operations are only partially expressible? Why, and \
         what\226\128\153s missing?\\n- `leftJoin` can only build the \
         resulting columns if both tables have at least one row to determine \
         the schema\\n- Various operations only work if there's at least one \
         row to determine the schema\\n  - ncols, header\\n- `dropna` only \
         works if every column in a table is optional since there's no way to \
         dynamically dispatch based off of column sort.\\n\\n> Q. Which \
         operations\226\128\153 expressibility is unknown? Why?\\nN/A\\n\\n> \
         Q. Which operations can be expressed more precisely than in the \
         benchmark? How?\\n- Several operations could be expressed in a more \
         typesafe manner if a projection function was passed instead of a \
         column name.\\n  - e.g. `selectColumn(table, fun e -> e.name)` as \
         opposed to `selectColumn(table, 'name')`\\n\\n## Example \
         Programs\\n\\n> Q. Which examples are inexpressible? Why?\\n- \
         sampleRows is inexpressible as Hazel is pure\\n\\n\\n> Q. Which \
         examples\226\128\153 expressibility is unknown? Why?\\nN/A\\n\\n> Q. \
         Which examples, or aspects thereof, can be expressed especially \
         precisely? How?\\n\\n\\n> Q. How direct is the mapping from the \
         pseudocode in the benchmark to representations in your system? How \
         complex is the encoding?\\n- The mapping is quite direct as \
         implemented. A less direct mapping could accomplish a more type-safe \
         translation of several of the programs.\\n\\n## Errors (TODO)\\n\\n> \
         There are (at least) two parts to errors: representing the source \
         program that causes the error, and generating output that explains \
         it. The term \226\128\156error situation\226\128\157 refers to a \
         representation of the cause of the error in the program source.\\n> \
         \\n> For each error situation it may be that the language:\\n> \\n> - \
         isn\226\128\153t expressive enough to capture it\\n> - can at least \
         partially express the situation\\n> - prevents the program from being \
         constructed\\n> \\n> Expressiveness, in turn, can be for multiple \
         artifacts:\\n> \\n> - the buggy versions of the programs\\n> - the \
         correct variants of the programs\\n> - the type system\226\128\153s \
         representation of the constraints\\n> - the type system\226\128\153s \
         reporting of the violation\\n\\n> Q. Which error situations are known \
         to be inexpressible? Why?\\n\\n\\n> Q. Which error situations are \
         only partially expressible? Why, and what\226\128\153s \
         missing?\\n\\n\\n> Q. Which error situations\226\128\153 \
         expressibility is unknown? Why?\\n\\n\\n> Q. Which error situations \
         can be expressed more precisely than in the benchmark? How?\\n\\n\\n> \
         Q. Which error situations are prevented from being constructed? \
         How?\\n\\n\\n> Q. For each error situation that is at least partially \
         expressible, what is the quality of feedback to the \
         programmer?\\n\\n\\n> Q. For each error situation that is prevented \
         from being constructed, what is the quality of feedback to the \
         programmer?\")";
    } )
