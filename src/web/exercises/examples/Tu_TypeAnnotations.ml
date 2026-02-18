open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000009-0009-0009-0009-000000000009");
    title = "Type Annotations";
    module_name = "Tu_TypeAnnotations";
    prompt =
      {md|Hazel's type system ensures that expressions are used in ways that make sense. You can explicitly annotate an expression with a type using the `:` operator. For example, `(1 : Int)` asserts that `1` has type `Int`.

If the annotation does not match the expression, Hazel will report a *type error*.

The editor below contains `("1" : Int)`, which has a type error — `"1"` is a `String`, not an `Int`. You will see the type mismatch highlighted in the editor.

Fix the type error by replacing the string `"1"` with the integer `1`. The result should evaluate to `1`.|md};
    wrapper = true;
    show_report = false;
    version = 9;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "(\"1\" : Int)");
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = Id.mk ();
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "answer" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
                                    label = [ "1" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary { id = Id.mk (); content = Whitespace "\n" };
                    ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
            refractors = Haz3lcore.ZipperBase.Refractor.init;
          };
        hints = [ "Replace the string `\"1\"` with the integer `1`." ];
      };
    display_hint = "Fix the type error by using an integer instead of a string";
    task_reference =
      {md|## Quick Reference

### Type Annotation
```hazel
(42 : Int)
("hello" : String)
(true : Bool)
```

### Type annotations on patterns
```hazel
let x : Int = 42 in
let y : String = "hello" in
let z : Bool = true in
(x, y, z)
```

### Basic Types
`Int`, `Float`, `Bool`, `String`|md};
  }
