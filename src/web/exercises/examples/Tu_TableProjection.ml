open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "b1000001-0001-0001-0001-000000000001");
    title = "Table Column Projection";
    module_name = "Tu_TableProjection";
    version = 1;
    prompt =
      {md|**Column projection** broadcasts `.label` over a list, extracting that column from every row:

```hazel
let fruits = [
(fruit="Apple", color="Red", qty=5),
(fruit="Banana", color="Yellow", qty=3)
] in
fruits.color
```

## Task

A book collection is stored as a table:

```hazelnostatics
Book : (title=String, author=String, year=Int)
```

Implement the function

```hazelnostatics
get_authors : [Book] -> [String]
```

that extracts the list of authors from a book collection.

Example:
```hazelnostatics
get_authors(^^table([(title="Dune", author="Herbert", year=1965),
             (title="Neuromancer", author="Gibson", year=1984)]))
  == ["Herbert", "Gibson"]
```|md};
    display_hint =
      "Use dot-projection on the list parameter to extract the author column";
    task_reference =
      TaskRefDocs.compose
        [
          TaskRefDocs.function_definition;
          TaskRefDocs.column_projection;
          TaskRefDocs.table_construction;
        ];
    wrapper = false;
    show_report = true;
    your_impl =
      Option.get
        (Parser.to_zipper ~root:Exp
           "type Book = (title=String, author=String, year=Int) in\n\
            let get_authors : [Book] -> [String] =  in\n\
            get_authors(^^table([(title=\"Dune\", author=\"Herbert\", \
            year=1965), (title=\"Neuromancer\", author=\"Gibson\", \
            year=1984)]))");
    hidden_tests =
      {
        tests =
          Option.get
            (Parser.to_zipper ~root:Exp
               "test get_authors([(title=\"Dune\", author=\"Herbert\", \
                year=1965), (title=\"Neuromancer\", author=\"Gibson\", \
                year=1984)]) == [\"Herbert\", \"Gibson\"] end;\n\
                test get_authors([(title=\"Foundation\", author=\"Asimov\", \
                year=1951)]) == [\"Asimov\"] end\n");
        hints =
          [
            "Use column projection (.author) on the books list";
            "Remember to use dot-projection on the function parameter";
          ];
      };
  }
