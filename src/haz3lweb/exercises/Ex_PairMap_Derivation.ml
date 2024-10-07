let prompt = Ex_PairMap_Derivation_prompt.prompt

let exercise : Exercise.spec =
  {
    header =
      {
        title = "PairMap Derivation";
        version = 1;
        module_name = "Ex_PairMap_Derivation";
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
                                   "14d3aed5-6d49-489f-9ee6-3e4f837d4262");
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
                                             "5e6b6fe0-b92a-461f-94c4-cd0f1165bf10");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "98967004-89be-4064-a466-5c911339a117");
                                      label = [ "tau_pm" ];
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
                                             "94660ece-207e-481a-b6a3-977ed0906ba4");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "8bdff043-13a6-4e94-8d7c-3f0b50a59777");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "9fed99f7-5c6b-4e48-99ff-cc73131f65fc");
                                      label = [ "of_alfa_typ"; "end" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Drv Typ ];
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
                                                       "5ceb90d0-c6fe-4dcc-8f9d-7279f540cae6");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "5173b39c-a9db-43ba-8343-31ac4a1dad96");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Drv Typ;
                                                    in_ = [ Drv Typ ];
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
                                                                 "3e7b83f0-b7e5-4f81-b854-afa2d75ddbc6");
                                                          label = [ "Bool" ];
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
                                                                 "3fce968d-e6a4-4477-8684-f6b5e1048d7f");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6b790255-defc-40d2-9a3a-e8c9814ff4df");
                                                          label = [ "->" ];
                                                          mold =
                                                            {
                                                              out = Drv Typ;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 5;
                                                                    sort =
                                                                      Drv Typ;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 5;
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
                                                                 "35d6a225-b55c-49f6-9b0e-d62c3f1be4b5");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2fa1bd89-e6a3-47b5-aef2-fa4b8e70ece7");
                                                          label = [ "Bool" ];
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
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "1dc40a6e-c660-4b86-b487-efea84caf034");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "87e47c24-4c11-4493-a20f-b9537d0e895d");
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
                                                       "8f4bbfa2-6773-45ae-93ad-51beddc16d51");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "2194c0ed-afd8-4e86-ac8a-2c04732b5d7e");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Drv Typ;
                                                    in_ = [ Drv Typ ];
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
                                                                 "adf3d73b-4615-482c-99f4-15b6ab2cb770");
                                                          label = [ "Bool" ];
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
                                                                 "c9555a01-2a9f-4898-9af1-270cc5425334");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2fb82f40-f65e-49f1-80d7-16527f4f758b");
                                                          label = [ "*" ];
                                                          mold =
                                                            {
                                                              out = Drv Typ;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 3;
                                                                    sort =
                                                                      Drv Typ;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 3;
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
                                                                 "e51081cf-dce6-42ee-b074-18e70f0656c0");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "23ad38e3-de2b-41c7-a560-81a400808583");
                                                          label = [ "Bool" ];
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
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "c6cc5703-608f-4754-b779-78c56b962447");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "77d0e345-6526-427b-ade5-06df1991a363");
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
                                                       "a994fb88-360f-49fc-8c0c-501398d12e9f");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "ebb1acb0-78d0-43d2-b69f-f94c07f5bb5b");
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Drv Typ;
                                                    in_ = [ Drv Typ ];
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
                                                                 "3bda48a5-3d93-47db-b425-2f84205cf667");
                                                          label = [ "Bool" ];
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
                                                                 "cadbfe1f-7f2b-4ea1-b060-26c7b1c23ed7");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "750d0530-097f-488b-972c-3b1b09676d5c");
                                                          label = [ "*" ];
                                                          mold =
                                                            {
                                                              out = Drv Typ;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 3;
                                                                    sort =
                                                                      Drv Typ;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 3;
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
                                                                 "fc91a6c5-943b-496b-8788-4771979730ad");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f3d88dca-0bba-4b28-aec3-4cf63abd0ca4");
                                                          label = [ "Bool" ];
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
                                                    ];
                                                  ];
                                              };
                                            Secondary
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "f770b962-c6fb-4593-b5ea-c29b5ff1ca5f");
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
                                             "466427f9-0e0a-4ff9-8533-769f2bd2d05e");
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
                                   "2645eecd-ca12-4283-b931-7bae2d11d21c");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "f8e82f28-0ab9-463b-9108-bbe7dbd0ea8e");
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
                                             "b445930f-97a8-47a0-8403-18d60965acf2");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "0bd35460-efb7-42af-b6c2-79c1081d316e");
                                      label = [ "gamma_pm" ];
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
                                             "f9968de0-0bb5-4b9f-ab3b-4889c0eb4a86");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "8d211daf-fd02-4900-a7a3-0315671b222a");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "44f23186-e136-47d5-9d34-62140377463a");
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
                                                       "9b0cbc16-bcde-41a3-b635-ac058cdc884c");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "15f1883b-f47f-402d-8ed0-da87fbe6311b");
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
                                                                 "1c446d50-bec1-43de-83bd-75c3ed7b6a37");
                                                          label = [ "pairmap" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
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
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "544ebe42-7c9d-42c7-a588-f360745e3f59");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "83909131-b370-4067-9038-fa6189cbc762");
                                                          label = [ ":" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 12;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 12;
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
                                                                 "ebecd385-6453-44f5-96f6-d115936b04db");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "528b5e33-b55d-4d9d-acd5-b701ed88a096");
                                                          label = [ "{"; "}" ];
                                                          mold =
                                                            {
                                                              out = Drv Typ;
                                                              in_ = [ Pat ];
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
                                                          shards = [ 0; 1 ];
                                                          children =
                                                            [
                                                              [
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "c092e0b5-da84-4343-83f4-58f2d8148c6b");
                                                                    label =
                                                                      [
                                                                        "tau_pm";
                                                                      ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Pat;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
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
                                                       "5ce5f73a-51a0-42aa-b863-559dff98b278");
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
                                             "3e5ee357-f9b2-434f-87e1-510525fe8a73");
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
                                   "6f35fcbe-7eee-4e19-8190-609d989185a1");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "1e88cb4d-484a-4954-9f6b-dbe65dca12aa");
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
                                             "5be663ea-21c5-4ba3-871a-ce1272737f5d");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "2557a2e6-0b62-423b-8606-a1bf1babbdae");
                                      label = [ "gamma_pmz" ];
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
                                             "d648c8a6-8cd0-4d11-8bae-9bb8ed8ab671");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "cfeccd65-8254-4be2-901f-aa79f2288cc4");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "3298376c-9f11-4fb6-9769-9690e53b96be");
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
                                                       "8b14ef94-ecb2-49ce-bc60-cbcbd4d52cc3");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "38f297a8-d888-42b9-b607-d9c221d1da4b");
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
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "aa86b4f1-458d-453e-8cda-50b53358b1f9");
                                                          label = [ "z" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
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
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d4410063-859e-4108-98be-32b261243370");
                                                          label = [ ":" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 12;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 12;
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
                                                                 "a15e3fac-570c-4129-87f6-8f2dc1605eee");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "03f0f948-95da-4cfe-81e2-376afab05f31");
                                                          label = [ "Bool" ];
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
                                                    ];
                                                  ];
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "ec8d5644-0512-43a0-a2b2-41b2cab27754");
                                                label = [ "::" ];
                                                mold =
                                                  {
                                                    out = Drv Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 7;
                                                          sort = Drv Exp;
                                                        },
                                                        {
                                                          shape = Concave 7;
                                                          sort = Drv Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "d4c98a1c-482f-40a1-bbc9-3796b0a1b7a8");
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
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "4d5498b6-f0b7-4241-8419-c73ea1c28ee9");
                                                          label = [ "gamma_pm" ];
                                                          mold =
                                                            {
                                                              out = Pat;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Pat;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
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
                                                       "7d3ef5a2-dc48-4c50-acc1-71bff9fec400");
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
                                             "79328246-4f03-4ddb-82b2-87d414ece359");
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
                                   "ead22f7e-b98a-4956-bcd7-0df7505e9412");
                            shape = Convex;
                          };
                        Secondary
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "0baa7f97-24e7-4f93-a7d6-11549105ac9c");
                            content = Whitespace " ";
                          };
                      ],
                      [
                        Secondary
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "ef652314-80fa-4784-9bc5-17b228249e29");
                            content = Whitespace "\n";
                          };
                      ] );
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
                    ( [
                        Secondary
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "8a783470-ca8e-447f-838b-779e6c0152df");
                            content = Whitespace " ";
                          };
                      ],
                      [
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "88a9e00d-8540-450b-bcf5-bed32e2f7b83");
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
                                               "eae3babf-06d6-4d28-a06b-b11e8b0782cc");
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
                                                         "dcbb3dd1-1cf1-4d50-b102-86d599f04d29");
                                                  label = [ "gamma_pm" ];
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
                                               "871e62b0-60d3-4fa8-b34c-864ab6d11184");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "24c9d702-02af-46f2-a335-ecb472f85695");
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
                                               "1a33aede-950a-4a7b-aac9-f076bf0eb6e3");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "00fb842c-b23e-4094-acf6-56ac7244fe0c");
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
                                                         "d284b554-f152-4c5f-a047-3c9b88faade4");
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
                                                                   "fb5b941d-b9ca-45d7-8689-db4dadab07d0");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "e3be8d4b-7083-46d2-8cd7-d4297530f00e");
                                                            label = [ "z" ];
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
                                                                   "263fd3ed-683d-4aa5-9761-9fadee6a6b6d");
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
                                                         "8180e090-a951-4b9a-af19-fd5689b0e5e5");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "42f9e26e-5378-4549-b879-4e1620bc1f9d");
                                                  label =
                                                    [ "if"; "then"; "else" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Exp; Drv Exp ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 13;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1; 2 ];
                                                  children =
                                                    [
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "48fcf6a5-8039-4f60-a99e-31f7f8feed2c");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "647b3738-8ee5-4fe2-80d8-7b20478ddf30");
                                                            label = [ "z" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
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
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "9472d3bb-c20b-49ca-a01a-3df89b954d34");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "6050855e-e6d2-4a1b-a0b9-e01f8c1016ed");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "858bbcfc-d4d1-452a-ba77-2c9d512e2544");
                                                            label = [ "False" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
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
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "91a182bc-4b8a-4e67-b0ca-f64e649d922d");
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
                                                         "8a01d60d-44ba-4b1e-9a36-c2b1850c83dc");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "1767ca56-f744-49a6-ac2b-07ed30199c58");
                                                  label = [ "True" ];
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
                                               "ce894a84-f07e-4208-9e69-3079b1506b56");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1ce92e67-f304-4229-8fbe-06ca877c8c84");
                                        label = [ "<=" ];
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
                                               "77bfeb7d-e83f-4b36-9344-739924c4b137");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "111bd3c0-c755-4276-be93-dfd84c39dcc1");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [ Drv Typ ];
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
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "6c401649-ff3c-41d6-a0be-0f3e31798160");
                                                  label = [ "Bool" ];
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
                                                         "dc07d1a1-3e39-4700-a71c-4124f1df55b1");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "691d7296-88a5-4677-9974-21b57353bfe2");
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
                                                         "57838101-99e3-4aac-87c2-a8273e0b6039");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "b07a6ea6-08b8-4aa4-91b0-a9c6b601b6c3");
                                                  label = [ "Bool" ];
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
                                            ];
                                          ];
                                      };
                                  ],
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some A_Fun;
                    },
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
                                                     "f7af40cc-5ade-457f-bf03-64fd62fac902");
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "e2c554ae-904e-40d7-956f-b906e1766f2e");
                                                        label = [ "gamma_pmz" ];
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
                                                     "bd0ed7e4-dccd-4bfd-9f5f-19425a0d806c");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4264438f-ddf3-40f9-8b3e-0720b48b6f39");
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
                                                     "51904bcc-8700-4171-aa12-65e1131bc3c1");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "0b194a17-ad49-4218-83e2-3eff89e93c42");
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "b489c147-953c-4aff-8630-f2387d177564");
                                                        label =
                                                          [
                                                            "if"; "then"; "else";
                                                          ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ =
                                                              [
                                                                Drv Exp; Drv Exp;
                                                              ];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 13;
                                                                  sort = Drv Exp;
                                                                } );
                                                          };
                                                        shards = [ 0; 1; 2 ];
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
                                                                         "b7a7833b-2d23-4f9b-8b7e-01f6c3504e5f");
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
                                                                         "f79b2b91-b318-4a3b-b7b9-d7d90dfdab43");
                                                                  label =
                                                                    [ "z" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "c3cdcd55-324f-4a5f-96cb-aac9b1d1b9a0");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                            ];
                                                            [
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "add63e18-35c2-47a4-95b6-a321e4260535");
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
                                                                         "6eada7cc-bc97-4af2-9fc9-cc3ea4fdb4cd");
                                                                  label =
                                                                    [ "False" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          } );
                                                                    };
                                                                  shards = [ 0 ];
                                                                  children = [];
                                                                };
                                                              Secondary
                                                                {
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "ed1a5d54-72ba-4157-81f1-ec62f14d2c88");
                                                                  content =
                                                                    Whitespace
                                                                      " ";
                                                                };
                                                            ];
                                                          ];
                                                      };
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "0bf03e57-5257-40d1-9da3-8d5bd1f2220f");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "18b9159d-6c32-4968-a529-3b5e53e5f4a0");
                                                        label = [ "True" ];
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
                                                     "df1640fc-852f-4030-a274-a0b3b9d83c66");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "23a8e326-c2b2-4203-a69f-26b868353697");
                                              label = [ "<=" ];
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
                                                     "ebd3652f-9d38-4a01-b109-46316b00f6ba");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "5cc78650-04f6-49d2-ae02-cf13d54c7a55");
                                              label = [ "Bool" ];
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
                            rule = Some A_If;
                          },
                        [
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
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
                                                           "6be41543-e475-4de1-a24f-91b557ed3a7b");
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
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "2dfb3ada-e22f-4e85-925f-ae6f4c92122e");
                                                              label =
                                                                [ "gamma_pmz" ];
                                                              mold =
                                                                {
                                                                  out = Pat;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
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
                                                           "390b81a0-fa0d-4430-9c59-ee9c593025c5");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "acbfb8ee-b1a8-4a6e-be46-e8390941dabf");
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
                                                           "d1b396be-1214-4c11-a232-e94585cec310");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "0d1384f6-05cc-480d-87f0-50c022a70ad4");
                                                    label = [ "z" ];
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
                                                Secondary
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "3498916e-33b1-4f4e-9ab9-a4dd46b411e1");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "d4f034a3-bfe6-4c42-9ebf-ea1af22c136b");
                                                    label = [ "<=" ];
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
                                                           "361f3258-dbf3-45ec-8b3e-01f6e0dd7613");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "c210ae24-4085-4106-abb7-c021ac5a1cd0");
                                                    label = [ "Bool" ];
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
                                  rule = Some A_Subsumption;
                                },
                              [
                                Node
                                  ( Just
                                      {
                                        jdmt =
                                          {
                                            selection =
                                              {
                                                focus = Left;
                                                content = [];
                                                mode = Normal;
                                              };
                                            backpack = [];
                                            relatives =
                                              {
                                                siblings =
                                                  ( [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "bc9e4528-b35e-48c4-8ec1-bdc844cad2f7");
                                                          label = [ "{"; "}" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Pat ];
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
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "fd121ded-1f22-40a3-89da-ed3dfbc9e70e");
                                                                    label =
                                                                      [
                                                                        "gamma_pmz";
                                                                      ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Pat;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "56523913-59bc-4079-9d0f-e1953ecf15d8");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "8a758d42-a670-412b-a3dc-0ea0240f09a4");
                                                          label = [ "|-" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
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
                                                                 "ab7b673f-7f86-4e6a-ad70-574ab5c2f701");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f754b16f-a284-4206-bab8-41c799881141");
                                                          label = [ "z" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
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
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "20bef3c5-ab71-4f53-9074-516078bd1f43");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "57ba402f-b55f-4b64-9850-abfc2df5a759");
                                                          label = [ "=>" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 12;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 12;
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
                                                                 "4dc10c0b-0dda-4b17-93b6-24e42ce19741");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f49f5cf4-2571-4c4f-899d-83f0f27675fa");
                                                          label = [ "Bool" ];
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
                                                    ],
                                                    [] );
                                                ancestors = [];
                                              };
                                            caret = Outer;
                                          };
                                        rule = Some S_Var;
                                      },
                                    [] );
                              ] );
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
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
                                                           "15d9a24a-54d1-479e-94cc-17f13e6df7d3");
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
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "c454ce8b-1b83-4a94-8564-af188f812441");
                                                              label =
                                                                [ "gamma_pmz" ];
                                                              mold =
                                                                {
                                                                  out = Pat;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
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
                                                           "da4e224e-b5c3-44be-a6a7-6dd93dd43100");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "ee85ba25-1df5-4d6b-ba70-6b796a331dc9");
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
                                                           "12ae20de-bebd-449a-b71d-ba0f64fdca6f");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "be54bb10-c799-4c81-9130-583ee1fb25f1");
                                                    label = [ "False" ];
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
                                                Secondary
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "fd9fe2ba-4563-4543-8f51-d618d419eb93");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "5c109b06-f584-4199-9d02-519430c1bf2b");
                                                    label = [ "<=" ];
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
                                                           "352dc2a1-bce7-41a4-bffa-d747c2e1efbc");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "9b523f05-594b-4024-994c-21797d30a24d");
                                                    label = [ "Bool" ];
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
                                  rule = Some A_Subsumption;
                                },
                              [
                                Node
                                  ( Just
                                      {
                                        jdmt =
                                          {
                                            selection =
                                              {
                                                focus = Left;
                                                content = [];
                                                mode = Normal;
                                              };
                                            backpack = [];
                                            relatives =
                                              {
                                                siblings =
                                                  ( [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "37b19fe0-bbe4-4627-8b12-5b9d6d63ee49");
                                                          label = [ "{"; "}" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Pat ];
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
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "12e39bce-f1af-4bc8-b0cc-d2ad3462ee41");
                                                                    label =
                                                                      [
                                                                        "gamma_pmz";
                                                                      ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Pat;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "9bcb7143-be1e-4a35-abc4-d71dad22bbed");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "bd0af794-b541-4650-80f5-98b0f07c7d9d");
                                                          label = [ "|-" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
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
                                                                 "ff7a5ec6-9ecf-4c60-81a3-04252cbd3f5d");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a980346b-1b85-4e74-9236-abbd4566509e");
                                                          label = [ "False" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
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
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "b6806642-927d-4d4f-8476-c3b192b5e08f");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1c426e86-6e3e-4afa-b43e-80f7d287436d");
                                                          label = [ "=>" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 12;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 12;
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
                                                                 "a4e0a1ff-e512-4ab2-bf38-664de418327b");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "23fb594f-a244-4dcc-ac66-1f5d9db68204");
                                                          label = [ "Bool" ];
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
                                                    ],
                                                    [] );
                                                ancestors = [];
                                              };
                                            caret = Outer;
                                          };
                                        rule = Some S_False;
                                      },
                                    [] );
                              ] );
                          Node
                            ( Just
                                {
                                  jdmt =
                                    {
                                      selection =
                                        {
                                          focus = Left;
                                          content = [];
                                          mode = Normal;
                                        };
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
                                                           "5b4386cd-b8a4-476b-96c6-4fbcc89a09c3");
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
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "60fa4659-1a06-453a-b6b8-6df443fd35ae");
                                                              label =
                                                                [ "gamma_pmz" ];
                                                              mold =
                                                                {
                                                                  out = Pat;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Pat;
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
                                                           "2b314e47-35cb-400d-b69b-430df4b5f547");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "c180e93a-8767-4fee-93ab-139220744d97");
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
                                                           "4d187cad-6531-4cce-8d3a-7ddce77794b5");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "179eaf89-2625-4097-8a41-561bf9ef1738");
                                                    label = [ "True" ];
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
                                                Secondary
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "bdb6313b-1668-40a3-9068-6dd4aecdcfb3");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "6e188068-0b7f-46dc-bbb9-1248d46fb9e8");
                                                    label = [ "<=" ];
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
                                                           "762d485a-b1a9-49e6-9b79-772b8e411102");
                                                    content = Whitespace " ";
                                                  };
                                                Tile
                                                  {
                                                    id =
                                                      Option.get
                                                        (Haz3lcore.Id.of_string
                                                           "4ad0c4be-7dc3-435c-bfc3-dbd448f6ba69");
                                                    label = [ "Bool" ];
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
                                  rule = Some A_Subsumption;
                                },
                              [
                                Node
                                  ( Just
                                      {
                                        jdmt =
                                          {
                                            selection =
                                              {
                                                focus = Left;
                                                content = [];
                                                mode = Normal;
                                              };
                                            backpack = [];
                                            relatives =
                                              {
                                                siblings =
                                                  ( [
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "854a2de2-6f09-4c7a-b7af-72eb2089c3ce");
                                                          label = [ "{"; "}" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [ Pat ];
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
                                                                Tile
                                                                  {
                                                                    id =
                                                                      Option.get
                                                                        (Haz3lcore
                                                                         .Id
                                                                         .of_string
                                                                           "644c9bd0-45ce-4d49-beea-3b6de0b5c12d");
                                                                    label =
                                                                      [
                                                                        "gamma_pmz";
                                                                      ];
                                                                    mold =
                                                                      {
                                                                        out =
                                                                          Pat;
                                                                        in_ = [];
                                                                        nibs =
                                                                          ( {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            },
                                                                            {
                                                                              shape =
                                                                                Convex;
                                                                              sort =
                                                                                Pat;
                                                                            } );
                                                                      };
                                                                    shards =
                                                                      [ 0 ];
                                                                    children =
                                                                      [];
                                                                  };
                                                              ];
                                                            ];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "ef810e0d-d43b-477e-adb6-a57cb1926af4");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d78cb787-fe4a-47fe-b1d6-05f3c3a8086f");
                                                          label = [ "|-" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 26;
                                                                    sort =
                                                                      Drv Exp;
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
                                                                 "198c75ae-7866-4f81-8985-6237775e7e9f");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "a8960366-2d63-4406-bb20-fcb7299ff3e3");
                                                          label = [ "True" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
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
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Secondary
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2e5ca888-324c-4100-880a-8d3f20b8b585");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "d00051a8-9979-4557-abca-debc1f1d4e39");
                                                          label = [ "=>" ];
                                                          mold =
                                                            {
                                                              out = Drv Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Concave 12;
                                                                    sort =
                                                                      Drv Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 12;
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
                                                                 "68af1cd0-1f65-4f26-bf3b-242dcb5f8184");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "6d49569b-0da4-491c-a8d1-8aa09cc9c119");
                                                          label = [ "Bool" ];
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
                                                    ],
                                                    [] );
                                                ancestors = [];
                                              };
                                            caret = Outer;
                                          };
                                        rule = Some S_True;
                                      },
                                    [] );
                              ] );
                        ] );
                  ] );
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
                                               "0cfeda2a-70d8-45e5-b505-a6fc115f8f19");
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
                                                         "f12e894c-09ba-4acd-aaff-5205e805042d");
                                                  label = [ "gamma_pm" ];
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
                                               "e996f301-d3cc-43c8-a0a0-10c2bd3a8578");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "217a62db-e6c1-452a-807a-05271c6d6bb3");
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
                                               "894d6ed4-f62e-464d-9397-1695f1024ddd");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e10dd477-ef75-4096-8c1c-c96869010d24");
                                        label = [ "pairmap" ];
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c38f5456-b68c-49ba-b16b-2bd31d8cb912");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3789f4de-d0a2-4c18-9f57-ae91e9e4fd94");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Exp;
                                            in_ = [ Drv Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 2;
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
                                                         "17187d57-79a9-499f-b280-ae37965cdf0f");
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
                                                                   "e6ddd2fb-c553-4af5-a7f0-83cf5ea5afbb");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "32f1e3fc-8e78-4ecf-9278-ad9f90ea2fa4");
                                                            label = [ "z" ];
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
                                                                   "28d48c20-ead3-49f6-965b-e375a4bae75b");
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
                                                         "3ca14712-8f71-4ba8-b9b7-57287c7a3803");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "00cc34c2-0df2-43f6-ab93-11123c877a54");
                                                  label =
                                                    [ "if"; "then"; "else" ];
                                                  mold =
                                                    {
                                                      out = Drv Exp;
                                                      in_ = [ Drv Exp; Drv Exp ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Drv Exp;
                                                          },
                                                          {
                                                            shape = Concave 13;
                                                            sort = Drv Exp;
                                                          } );
                                                    };
                                                  shards = [ 0; 1; 2 ];
                                                  children =
                                                    [
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "ab277ce6-b69e-4167-ad30-cd93d0ee61d8");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "dd838f1a-e8d7-44f2-9cd8-ce3fb9a724ee");
                                                            label = [ "z" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
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
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "37c395f9-d623-45e0-b9c1-41ef2f633835");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                      [
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "66274830-890d-4dde-95eb-2b997888c6a0");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "699169b3-93a8-4d64-b421-933feab2713e");
                                                            label = [ "False" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
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
                                                            shards = [ 0 ];
                                                            children = [];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "0153676e-fd69-47ba-b7b5-be8ce78739d2");
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
                                                         "a70ed443-fc53-4780-b3ac-d018b4dc1282");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "44d3ea76-259a-4259-bf5e-4ab1d19a264a");
                                                  label = [ "True" ];
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
                                               "809703aa-2c90-4527-9887-f197bf2ba7e4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fe24afec-e242-444e-b25b-5c6c8d19cb67");
                                        label = [ "=>" ];
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
                                               "67d37158-4da9-46f9-ace4-538bd8c4a348");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "835d5ba4-562f-441c-a73a-8aebadcc5641");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [ Drv Typ ];
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
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "62458122-7381-4769-a0ee-8f6eebc3bea0");
                                                  label = [ "Bool" ];
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
                                                         "df299164-c73f-4d27-a9d5-43f4891b8e93");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "33b4fa17-7d8f-4cb1-9e5e-f01c3d53cf1f");
                                                  label = [ "*" ];
                                                  mold =
                                                    {
                                                      out = Drv Typ;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 3;
                                                            sort = Drv Typ;
                                                          },
                                                          {
                                                            shape = Concave 3;
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
                                                         "c4d17621-12f2-4dad-aa1f-eaaeea277319");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "7a7f98ef-cdac-408f-97cf-3fd14f0788a1");
                                                  label = [ "Bool" ];
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
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "18cb8196-e210-44c0-bbec-0352cfc023ce");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7e3b44db-714d-49a9-84e6-8bb9c9ce4d72");
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
                                               "9c43a69e-5304-44ab-a8d1-b0e41a4bfee4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "49240377-77db-47b8-aacd-15bce9a32fd8");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Drv Typ;
                                            in_ = [ Drv Typ ];
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
                                        shards = [ 0; 1 ];
                                        children =
                                          [
                                            [
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "c19681d4-b0e2-4bc3-bb56-a33201bc154d");
                                                  label = [ "Bool" ];
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
                                                         "bf6833f6-e388-4f3e-b787-eacba812ea7c");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "b17d50f3-53e6-4195-aaa5-b881356e89fb");
                                                  label = [ "*" ];
                                                  mold =
                                                    {
                                                      out = Drv Typ;
                                                      in_ = [];
                                                      nibs =
                                                        ( {
                                                            shape = Concave 3;
                                                            sort = Drv Typ;
                                                          },
                                                          {
                                                            shape = Concave 3;
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
                                                         "1b0ae2a9-2334-4adb-9dc2-05366210f02c");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "549b24fa-3b64-4408-9541-a4ec7d98b963");
                                                  label = [ "Bool" ];
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
                                            ];
                                          ];
                                      };
                                  ],
                                  [] );
                              ancestors = [];
                            };
                          caret = Outer;
                        };
                      rule = Some S_Ap;
                    },
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
                                                     "bf5ac69c-2080-46d0-b1a5-04ba6f67c3bc");
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "dee1e853-7757-4f70-a316-7a2538a0d174");
                                                        label = [ "gamma_pm" ];
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
                                                     "c2ba8a89-3907-496a-8ca1-f123994e43cf");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "bca2bb96-21bf-4ffc-82bd-6a4cf0d7f2ee");
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
                                                     "eb183bac-10a6-4665-954e-8074bc6d4baf");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "ce54c3be-9dd1-4599-a5e3-82ccaf61fe7f");
                                              label = [ "pairmap" ];
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
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4a66594c-b2e7-48bf-9da6-14ec3cf9d8a8");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "3811f8b8-f3cb-4214-9c7c-5f5ca11ae4e3");
                                              label = [ "=>" ];
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
                                                     "0eca2157-036c-4316-9987-1fc8e00eb6c9");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "21f540ad-f3cf-4c18-948a-5af4761d62c8");
                                              label = [ "{"; "}" ];
                                              mold =
                                                {
                                                  out = Drv Typ;
                                                  in_ = [ Pat ];
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
                                                               "a27423e2-0205-4dca-8ac8-6a14cf6764e6");
                                                        label = [ "tau_pm" ];
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
                                        ],
                                        [] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                              };
                            rule = Some S_Var;
                          },
                        [] );
                    Node (Abbr (Some 0), []);
                  ] );
            ];
        };
  }
