open Haz3lcore

let exercise : Tutorial.spec =
  {
    id = Option.get (Id.of_string "a0000002-0002-0002-0002-000000000002");
    title = "The Tylr Parser and Backpack";
    module_name = "Tu_TylrParser";
    prompt =
      "Hazel uses a tile-based parser called Tylr. When you type a multi-part \
       keyword like `let`, Tylr automatically creates `obligations` — the \
       remaining delimiters (`=` and `in`) that are needed to complete the \
       syntactic form. These obligations are held in the `backpack`, shown at \
       the top of the editor.\n\n\
       You can `drop` an obligation from the backpack by pressing `Tab` when \
       your cursor is at the right position. For example, after typing `let \
       x`, press `Tab` to drop the `=` delimiter, then type the value, and \
       press `Tab` again to drop the `in` delimiter.\n\n\
       Try it: type `let x = 1 in x + 1` in the cell below, using `Tab` to \
       drop the `=` and `in` delimiters from the backpack. The result should \
       evaluate to `2`.";
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
