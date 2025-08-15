## Reference

> Q. Where can we learn about the programming medium covered by this datasheet?
> (Feel free to link to multiple kinds of artifacts: repositories, papers, videos, etc.
> Please also include version information where applicable.)

- **Website**: http://hazel.org  
- **Source Code**: https://github.com/hazelgrove/hazel *(TODO: Add version)*  
- **App**: https://hazel.org/build/dev/  

> Q. What is the URL of the version of the benchmark being used?
https://github.com/brownplt/B2T2/blob/fd227efadf532a20aefd25c7a8580978c2d684a2/Datasheet.md  


> Q. On what date was this version of the datasheet last updated?
2024-06-13

> Q. If you are not using the latest benchmark available on that date, please explain why not.
Yes

## Example Tables

> Q. Do tables express heterogeneous data, or must data be homogenized?
  Hazel tables are represented as *lists of labeled tuples*.  
  - Columns may be heterogeneously typed.   
  - Rows must be homogeneously typed.
    - The unknown type allows some degree of heterogenous rows.

> Q. Do tables capture missing data and, if so, how? Do missing values affect the output constraints of any operations, for example `groupBy`?
  - Represented via `Option` types (`Some` / `None`)  
  - Incomplete programs can use expression holes (not programmatically accessible)  
  - No special handling in operations — `Option` values are ordinary

> Q. Are mutable tables supported? Are there any limitations?
Mutable tables are not supported

> You may reference, instead of duplicating, the responses to the above questions in answering those below:

> Q. Which tables are inexpressible? Why?
None — all tables can be expressed using `Option` types for missing data

> Q. Which tables are only partially expressible? Why, and what’s missing?
N/A

> Q. Which tables’ expressibility is unknown? Why?
N/A

> Q. Which tables can be expressed more precisely than in the benchmark? How?
None - hazel represents the tables as precisely as the benchmark. Once again explicit option types make optional columns explicit.

> Q. How direct is the mapping from the tables in the benchmark to representations in your system? How complex is the encoding?
  - Very direct  
  - Benchmark tables map naturally to Hazel's `List of Labeled Tuples`  
  - Missing values use `Option`
  - Nested tables use nested labeled tuples or lists

## TableAPI

> Q. Are there consistent changes made to the way the operations are represented?
The operations are mostly presented as depicted, but here are a few variations:
- Some operations utilize explicity polymorphism in Hazel using the `typfun` keyword to require explicit type application
  as implicit polymorphism has not been added to Hazel as of 2025-07-08
- Hazel tables are represented using lists of labeled tuples so there is no runtime schema available for operations.
  For certain operations, such as `leftJoin`, this requires looking at the head element to determine the schema and
  give some behavior in the event no such element exists.
- Certain operations have been made to return an optional value rather than an error
- Hazel does not have first-class labels, and therefore uses strings for columns for some of the operations.
  If the operation was done inline primitive operators could be used to recover typesafety.

> Q. Which operations are entirely inexpressible? Why?
All the operations are at least partially expressible.

> Q. Which operations are only partially expressible? Why, and what’s missing?
- `leftJoin` can only build the resulting columns if both tables have at least one row to determine the schema
- Various operations only work if there's at least one row to determine the schema
  - ncols, header
- `dropna` only works if every column in a table is optional since there's no way to dynamically dispatch based off of column sort.

> Q. Which operations’ expressibility is unknown? Why?
N/A

> Q. Which operations can be expressed more precisely than in the benchmark? How?
- Several operations could be expressed in a more typesafe manner if a projection function was passed instead of a column name.
  - e.g. `selectColumn(table, fun e -> e.name)` as opposed to `selectColumn(table, `name`)`

## Example Programs

> Q. Which examples are inexpressible? Why?
- sampleRows is inexpressible as Hazel is pure


> Q. Which examples’ expressibility is unknown? Why?
N/A

> Q. Which examples, or aspects thereof, can be expressed especially precisely? How?


> Q. How direct is the mapping from the pseudocode in the benchmark to representations in your system? How complex is the encoding?
- The mapping is quite direct as implemented. A less direct mapping could accomplish a more type-safe translation of several of the programs.

## Errors

> There are (at least) two parts to errors: representing the source program that causes the error, and generating output that explains it. The term “error situation” refers to a representation of the cause of the error in the program source.
> 
> For each error situation it may be that the language:
> 
> - isn’t expressive enough to capture it
> - can at least partially express the situation
> - prevents the program from being constructed
> 
> Expressiveness, in turn, can be for multiple artifacts:
> 
> - the buggy versions of the programs
> - the correct variants of the programs
> - the type system’s representation of the constraints
> - the type system’s reporting of the violation

> Q. Which error situations are known to be inexpressible? Why?
Many of the programs require explicit parametric polymorphism and the higher-order function versions of the TableAPI operations to get the best feedback. 

* `getOnlyRow` provides no feedback on the error as we do not currently track table size information statically


> Q. Which error situations are only partially expressible? Why, and what’s missing?
* Two versions of `brownJellybeans` are implemented with tradeoffs on expressibility:
  * The first version takes a string column name and uses our more dynamic operations to select the column. This provides no feedback on the error but more closely matches the implementation in the benchmark.
  * The second version takes a function that selects the column and uses our more type-safe operations to select the column. This correctly localizes the error to the column selection.

> Q. Which error situations’ expressibility is unknown? Why?
None

> Q. Which error situations can be expressed more precisely than in the benchmark? How?
None

> Q. Which error situations are prevented from being constructed? How?
None

> Q. For each error situation that is at least partially expressible, what is the quality of feedback to the programmer?
* Malformed Tables
  * For missing schemas, rows, and cells they are represented by syntactic holes in the program. These are easily visible in the editor and can be filled in by the programmer.
  * For tables where the schema is the incorrect length static errors are added onto each row showing the type inconsistency between the schema type and the row type.
    * If extraneous columns are present, the error is localized to the column label and an error is placed e.g. `favorite color is not part of expected labels: name, age`.
    * If there is a cell of the wrong type, the error is localized to the cell and an inconsistent type error is placed e.g. `String inconsistent with expected type Int for label age`

Note that in the following programs the errors are partially localized based off of the chosen explicit type application. Using different type-hole inference or choices for parametric type application would change the error localization and message.

* `midFinal`
  * Localizes the error to the column selection `mid` in the editor.
  * Message: `Label mid not found in tuple's labels: name age quiz1 quiz2 midterm quiz3 quiz4 final`
* `blackAndWhite`
  * Localizes the error to the column selection `black and white` in the editor.
  * Message: ```Label `black and white` not found in tuple's labels: get_acne red black white green yellow brown orange pink purple```
* `pieCount`
  * Localizes the error to the column selection `true` and `get_count` in the editor.`
  * The error messages are similar to above
* `brownAndGetAcne`
  * Localizes the error to the column selection `brown and get acne` in the editor.
  * The error messages are similar to above
* `favoriteColor`
  * Localizes the error to the column selection `favorite color` in the editor.
  * The error message: `String is inconsistent with expected type Bool`
* `brownJellybeans`
  * The first version provides no feedback on the error as it uses the string column name.
  * The second version localizes the error to the column selection, `color` with an error message similar to above.
* `employee_to_department`
  * Localizes an error to the column selection `last_name` in the editor
  * Localizes another error to the tuple extension saying the resulting row's type is inconsistent since `last_name` is a `Int` but the expected type is `String`
  * The error message: `Label department not found in tuple's labels: name age department salary`


> Q. For each error situation that is prevented from being constructed, what is the quality of feedback to the programmer?
N/A