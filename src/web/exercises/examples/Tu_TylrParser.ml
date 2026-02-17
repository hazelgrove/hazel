open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000002-0002-0002-0002-000000000002");
    title = "The Tylr Parser and Backpack";
    module_name = "Tu_TylrParser";
    prompt =
      "Hazel uses a tile-based parser called Tylr. When you type a multi-part \
       keyword like `let`, Tylr automatically creates `obligations` — the \
       remaining delimiters that are needed to complete the syntactic form. \
       These obligations are held in the `backpack`, shown at the top of the \
       editor.\n\n\
       The editor below already contains `let x =` with a hole after the `=`. \
       Notice that the `in` delimiter is in the backpack — it still needs to \
       be dropped into the program.\n\n\
       Complete the expression step by step:\n\
       1. Type `1` to fill in the value for `x`\n\
       2. Press `Tab` to drop the `in` delimiter from the backpack\n\
       3. Type `x + 1` as the body of the let expression\n\n\
       The result should evaluate to `2`.";
    wrapper = true;
    show_report = false;
    version = 2;
    your_impl = Option.get (Haz3lcore.Parser.to_zipper "let x =");
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
                                    label = [ "2" ];
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
        hints =
          [
            "Type `let x = 1 in x + 1` using Tab to drop backpack obligations.";
          ];
      };
    display_hint = "Type a let expression and use Tab to complete obligations";
  }
