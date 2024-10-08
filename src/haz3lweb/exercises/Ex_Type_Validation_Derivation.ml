let prompt = Ex_Type_Validation_Derivation_prompt.prompt

let exercise : Exercise.spec =
  {
    header =
      {
        title = "Type Validation Derivation";
        version = 1;
        module_name = "Ex_Type_Validation_Derivation";
        prompt;
      };
    pos = Proof Prelude;
    model =
      Proof
        {
          prelude =
            {
              selection = { focus = Left; content = []; mode = Normal };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "b533dba4-a7fa-46d1-91a4-4e2b9d611641");
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 17; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "bb1c3b18-0d2d-45b9-b67b-77d3709d5397");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "24d4008d-2a18-424d-887f-eae2ab5de29b");
                                      label = [ "delta" ];
                                      mold =
                                        {
                                          out = Pat;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Pat },
                                              { shape = Convex; sort = Pat } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "b3c26f13-96ad-44bf-85f6-1dec0c472d7c");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "22fd0d0e-b81e-49d2-876f-2111469150ac");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "17b70310-333d-4e61-8209-353f36514b67");
                                      label = [ "of_ctx"; "end" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Drv Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "a1ab2949-9945-4dd1-92cf-9fadef1653b7");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "cb7a7fdb-8155-4c3b-9d31-decebbb4d5f6");
                                                label = [ "["; "]" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [ Drv Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1 ];
                                                children =
                                                  [
                                                    [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "813229a8-2d88-4d5e-9509-58b7fc3e9ac7");
                                                          label =
                                                            [ "valid"; "end" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Drv Typ ];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort =
                                                                      Drv Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0; 1 ];
                                                          children =
                                                            [
                                                              [
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "2d391623-2177-43a3-ae2c-7b3790b5afeb");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "a0d9e8e0-1238-4b23-a9d7-92fc744b505c");
                                                                    label =
                                                                      [ "A" ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Drv
                                                                            Typ;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Typ;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Drv
                                                                                Typ;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                                Secondary
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "fa13eb3d-de40-45ff-aadd-43bc508c0941");
                                                                    content =
                                                                      Whitespace
                                                                        " ";
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "a828b61f-2cb5-4598-9acc-b43855c26e4f");
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "e1efd65b-b2dd-4022-9b23-18d55f1eb3ad");
                                      content = Whitespace " ";
                                    };
                                ];
                              ];
                          };
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "150ad291-71cc-43b6-a8aa-5b45dea42978");
                            shape = Convex;
                          };
                      ],
                      [] );
                  ancestors = [];
                };
              caret = Outer;
            };
          setup =
            {
              selection = { focus = Left; content = []; mode = Normal };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [],
                      [
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "2a1817bc-6d54-4a07-9c3b-3c29689ccfc7");
                            shape = Convex;
                          };
                      ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          trees =
            [
              Node
                ( Just
                    {
                      jdmt =
                        {
                          selection =
                            { focus = Left; content = []; mode = Normal };
                          backpack = [];
                          relatives =
                            {
                              siblings =
                                ( [
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "cd29a546-4616-488f-afb4-d959a990f709");
                                        label = [ "{"; "}" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Pat ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "c62109a2-2c49-44a6-8e4a-d8856dd418f2");
                                                  label = [ "delta" ];
                                                  mold =
                                                    {
                                                      out = Pat;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Pat;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Pat;
                                                          } );
                                                    };
                                                  shards = [ 0 ];
                                                  children = [];
                                                };
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "32c6cc78-d990-4a12-a0c1-2f7ed9b83aea");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1620c2cb-09c4-4d61-80f0-a28963f14694");
                                        label = [ "|-" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 26;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fc8ba8e3-9b35-4aae-899c-dd09dae9e548");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5901997d-d506-4493-be38-7490561a7616");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Exp;
                                                } );
                                          };
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "561e8a44-5228-4eb8-b288-1f624b5e6101");
                                                  label = [ "fun"; "->" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Pat ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 14;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1 ];
                                                  children =
                                                    [
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "43462108-e539-429e-9f1a-1969db14bd52");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "d9e2699f-bcc5-426c-aa5f-a4b64f21ce82");
                                                            label = [ "x" ];
                                                            mold =
                                                              {
                                                                out = Drv Pat;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Pat;
                                                                    } );
                                                              };
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "ff2ac15e-76ad-4ed5-91b5-370655a5be37");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "76992270-2973-49bb-9cec-02ff67d43f4c");
                                                            label = [ ":" ];
                                                            mold =
                                                              {
                                                                out = Drv Pat;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          12;
                                                                      sort =
                                                                        Drv Pat;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          12;
                                                                      sort =
                                                                        Drv Typ;
                                                                    } );
                                                              };
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "218c7a2a-a00e-44eb-93f1-4d8a6c0d404c");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "68585ba6-a584-4212-8464-bc1081550497");
                                                            label = [ "A" ];
                                                            mold =
                                                              {
                                                                out = Drv Typ;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Typ;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
                                                                      sort =
                                                                        Drv Typ;
                                                                    } );
                                                              };
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "b7c47e34-54b7-4519-a489-ce74976ca98f");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                    ];
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "c3e2d208-2134-405e-9dfc-ebaf7d9a8ab7");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a16c9d9a-1167-4749-8074-e0c93dc605a0");
                                                  label = [ "x" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0 ];
                                                  children = [];
                                                };
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4583f952-1a29-4271-84db-d626700bdae3");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ebe1f8b9-896c-4515-a52a-f15e7554ace5");
                                        label = [ ":" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 12;
                                                  sort = Drv Exp;
                                                },
                                                {
                                                  shape = Concave 12;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4d48d124-84f8-4342-893d-1069c0118405");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7325b8d1-941b-4628-877c-9fdae132597a");
                                        label = [ "A" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "74123937-04f3-4d8c-bdd6-d2e87ee6828c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "511a9893-5ed7-4c3c-a168-c7192cfbafa7");
                                        label = [ "->" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 5;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Concave 5;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0ed6deb5-0f14-4bdd-9761-41aae97b980a");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f8b4dd23-f0f6-4cbc-a178-709507db394b");
                                        label = [ "A" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                },
                                                {
                                                  shape = Convex;
                                                  sort = Drv Typ;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                  ],
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = None;
                    },
                  [] );
            ];
        };
  }
