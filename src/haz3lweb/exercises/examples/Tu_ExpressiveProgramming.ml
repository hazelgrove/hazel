open Haz3lcore

let prompt = Tu_ExpressiveProgramming_prompt.prompt

let exercise : Tutorial.spec =
  {
    title = "Expressive Programming";
    (* description = ""; *)
    module_name = "Ex_OddlyRecursive_tutorial";
    prompt;
    wrapper = false;
    version = 1;
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings = ([ Grout { id = Id.mk (); shape = Convex } ], []);
            ancestors = [];
          };
        caret = Outer;
      };
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            backpack = [];
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
                                    label = [ "answer"; "=="; "4" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp; Exp ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1; 2 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = Id.mk ();
                                              label = [ "answer" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                        [
                                          Tile
                                            {
                                              id = Id.mk ();
                                              label = [ "4" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                    ],
                    [] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [ "Reread the question!" ];
      };
    (* hidden_tests =
       {
         tests =
           {
             selection = { focus = Left; content = []; mode = Normal };
             backpack = [];
             relatives =
               {
                 siblings = ([ Grout { id = Id.mk (); shape = Convex } ], []);
                 ancestors = [];
               };
             caret = Outer;
           };
         hints = [];
       }; *)
  }
