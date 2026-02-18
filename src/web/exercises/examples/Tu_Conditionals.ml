open Haz3lcore

let if_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000010-0010-0010-0010-000000000010");
    title = "If Expressions";
    module_name = "Tu_IfExpressions";
    prompt =
      {md|Given a boolean expression, you can use it to choose between two expressions using a conditional expression.

The syntax is `if condition then expr1 else expr2`. If the condition evaluates to `true`, the result is `expr1`; otherwise, it is `expr2`. The condition must have type `Bool`.

The editor below contains `if 3 < 5 then` with the condition and `then` keyword already provided. The `else` delimiter is in the backpack.

Complete the expression:
1. Type `10` as the then-branch
2. Press `Tab` to drop the `else` from the backpack
3. Type `20` as the else-branch

The result should evaluate to `10`.|md};
    wrapper = true;
    show_report = false;
    version = 10;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "if 3 < 5 then ");
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
                                    label = [ "10" ];
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
        hints = [ "Type `10`, then Tab, then `20`." ];
      };
    display_hint = "";
    task_reference =
      {md|## Quick Reference

### If Expression
```hazel
if 3 < 5 then 10 else 20
```

### Comparison Operators
- `<`, `>`, `<=`, `>=`, `==`|md};
  }

let case_exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000011-0011-0011-0011-000000000011");
    title = "Case Expressions";
    module_name = "Tu_CaseExpressions";
    prompt =
      {md|A `case` expression lets you pattern match on a value. The syntax is:

`case expr`
`| pattern1 => result1`
`| pattern2 => result2`
`end`

Hazel checks each pattern from top to bottom and evaluates the result of the first matching branch. You can use `_` as a wildcard pattern that matches anything.

The editor below contains `case 2` with the scrutinee already provided. The `end` delimiter is in the backpack.

Add pattern branches:
1. Type `| 2 => 20` for the matching case
2. Type `| _ => 0` for the default case
3. Press `Tab` to drop `end` from the backpack

The result should be `20`.|md};
    wrapper = true;
    show_report = false;
    version = 11;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "case 2 ");
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
                                    label = [ "20" ];
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
        hints = [ "Type `| 2 => 20 | _ => 0` then Tab to drop `end`." ];
      };
    display_hint = "Add pattern branches and drop `end` from backpack";
    task_reference =
      {md|## Quick Reference

### Case Expression
```hazel
case x
| 0 => "zero"
| 1 => "one"
| _ => "other"
end
```

`_` is a wildcard that matches anything.|md};
  }
