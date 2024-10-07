let prompt = Ex_Curried_Function_Derivation_prompt.prompt

let exercise : Exercise.spec =
  {
    header =
      {
        title = "Curried Function Derivation";
        version = 1;
        module_name = "Ex_Curried_Function_Derivation";
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
                                   "9a557ef0-7d77-4c8c-8b10-ffb268135c9c");
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
                                             "c8d04bff-85bc-4cf6-84b3-baf2517bdeb0");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "2b60fa81-1f51-4b43-a023-6f40e2032bb1");
                                      label = [ "ctx_a" ];
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
                                             "cdfc37fa-c913-4c51-8fae-673b3f64e8ef");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "cd318be1-fa2d-42b7-8aa1-b0eb9151ad7c");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "656bc72f-0719-4d0a-9cd6-c41e823bbc71");
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
                                                       "d639061d-58b6-4253-8fed-0343ecf32010");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "d742a41b-b642-42d9-850a-05981a84a00a");
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
                                                                 "568831b2-59e1-48f0-9873-6d9a0710bd6a");
                                                          label = [ "a" ];
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
                                                                 "d2a122dd-5493-43c4-868c-ebf144cabe05");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "2e83b2c0-1a03-4473-8664-fce55b245505");
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
                                                                 "83ff71b4-6ee8-442b-9931-65d83b131973");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "c9829c7d-5e53-4b3f-a60d-e47444e9c517");
                                                          label = [ "Num" ];
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
                                                       "070850ce-bee6-4a84-a6f2-bbb300501a6c");
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
                                                       "6986b0e4-f833-42ee-849c-cfe5ddd847e8");
                                                label = [ "[]" ];
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
                                                       "25bf1471-b7c2-4f7f-a5bb-258ecaa1e8fe");
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
                                             "efcbabe0-dd12-4027-9c3b-1d75fdd38ff5");
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
                                   "f2696306-5eb4-4c8e-8949-887c0b2a23c1");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "25b68ecb-caf9-4bb1-b2c7-2aae751663b2");
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
                                             "d9a87fc4-969d-4d53-81bc-d24da32621dc");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "ca5db582-421e-404a-937c-9ea6234b4ebd");
                                      label = [ "ctx_ab" ];
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
                                             "f61c020a-14fc-476a-86d3-2a0fedbfa65e");
                                      content = Whitespace " ";
                                    };
                                ];
                                [
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "f2aec451-304e-4399-a03a-9cd00d4a9d99");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "8f1e0b03-1d36-4858-a466-73c26b309c14");
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
                                                       "2192643a-c493-4cf2-9989-f634c55b8c47");
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "a164e47b-b77d-4783-b477-ce382eda978e");
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
                                                                 "a12d17f8-31bc-4943-8762-b2654eaab912");
                                                          label = [ "b" ];
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
                                                                 "593ac0e3-cfad-4224-9023-960a856f9ab6");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "722a98e0-55f2-4bd0-a4e4-bdf5184827de");
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
                                                                 "9933d7b0-20d3-4743-adbc-fa7e4c8c604a");
                                                          content =
                                                            Whitespace " ";
                                                        };
                                                      Tile
                                                        {
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "bf70f862-9f4c-4b1e-8fc5-d7b82c870fb2");
                                                          label = [ "Num" ];
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
                                                       "c322cb39-a217-42ca-ac15-3e2800072d4b");
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
                                                       "1d07bee7-6514-4261-8483-06bd34df6805");
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
                                                                 "64cdd758-6f6e-468d-aea5-cf5192cc8764");
                                                          label = [ "ctx_a" ];
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
                                                       "df136289-239d-47ed-ade1-6f616d49b824");
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
                                             "2c3a853e-a2a7-49b8-a064-22c0d586481f");
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
                                   "d6b83579-1bb7-40cb-be27-5616d3331009");
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
                                   "7a205d3c-14f7-4b39-a13e-f0b4bba49577");
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
                                               "50630d10-db00-4a26-8d00-708ad0d71d94");
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
                                                         "306980a7-d797-40a3-b01d-34ac6de3d413");
                                                  label = [ "ctx_ab" ];
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
                                               "d0b014f7-9d13-40ed-9946-921b33aa94ec");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "46fab45e-9fc3-433e-8199-1a50635835f6");
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
                                               "ac931f0e-3b99-4b8c-8a42-3e7e834d34f8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "54ca991b-4f01-40f4-9dd1-97b48a24af44");
                                        label = [ "a" ];
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
                                               "ebe0e95d-98fc-40ec-9955-f19d295b44f8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5e5cafa7-bcd3-402d-855e-d68191e4e50b");
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
                                               "09353f4f-2387-4e0f-af33-c313d1fe7a38");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ab184dd3-38ce-47c3-a042-6627d576a40e");
                                        label = [ "Num" ];
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
                      rule = Some T_Var;
                    },
                  [] );
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
                                               "54d9f785-e729-41b3-9b57-c941f9ae0827");
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
                                                         "73ca6943-e926-4821-805d-bdcd847aea6c");
                                                  label = [ "ctx_ab" ];
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
                                               "ba06ba41-ebda-46e5-9828-a659880d26b7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "dd2f9b4e-136c-4985-8687-4dcbbc68b0a7");
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
                                               "37b0437c-f73b-4446-85cb-da18da735bdd");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "38557079-d7f5-407d-9db0-df137d61d5cd");
                                        label = [ "b" ];
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
                                               "e2e88d2b-f708-4eaa-97f3-6f1339afd8f2");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "2172964a-a296-4227-abda-1acf41b981f1");
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
                                               "79f06eed-a168-4517-8954-03b1232327f0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e87ef2de-9e52-497c-8d27-6525df1b39f1");
                                        label = [ "Num" ];
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
                      rule = Some T_Var;
                    },
                  [] );
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
                                               "59d3c28e-b27d-4476-ad8b-ac69d80a0e5e");
                                        label = [ "|-" ];
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
                                               "8fac760c-4d2f-4361-b09b-cedea9c7262d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8dfab0e3-e734-442b-ab3c-fc75dd399a95");
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
                                                         "33a40f4d-087b-4e9e-825c-8b16d9f0c1ea");
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
                                                                   "f4d11a3a-4cb6-4a07-9dbc-8f2a7545f9a4");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "449fae2d-a732-476f-9dcc-84e3a69ce4c8");
                                                            label = [ "a" ];
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
                                                                   "1ec32a16-35e5-45f8-9c8f-e12487c1107e");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "44c14fea-6ee0-4c22-8e82-8edab2d3216c");
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
                                                                   "85ccd03d-0cad-404b-99a6-96e1de803642");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "f92bffdf-f057-40fb-8075-ef13ec7e946b");
                                                            label = [ "Num" ];
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
                                                                   "0f032e67-0674-4314-af87-e533bb485712");
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
                                                         "1d967800-3083-4289-a3d8-f35ea2dc5597");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "db6ca8c2-5533-40c0-b9d5-39645600b55d");
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
                                                                   "7f0fa801-03c5-4288-a6c0-6615060f813a");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "9bad651e-192c-4087-b9e1-382f855ee7c8");
                                                            label = [ "b" ];
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
                                                                   "30d8b3a2-ab63-430b-9052-80d0b95ed4b2");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "ce51c665-a481-47dc-957c-e17716bfbfc0");
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
                                                                   "4c32c65d-5157-4dcc-bbdc-de2474ef55e2");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "b1765627-c1f1-42d2-95ca-3b9e474b5fd5");
                                                            label = [ "Num" ];
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
                                                                   "1af7650f-b286-4888-b06a-d941081b87dd");
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
                                                         "ccb1a462-509d-4a07-89f0-fbf7549bcde3");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "43ec61b6-64e5-49bb-9ac1-b99f54d02bf7");
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
                                                                   "ae8555dc-d238-4cf6-8af3-cb369171dd3a");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "2faa3523-5c1d-4664-808d-5ee31a0f8ab9");
                                                            label = [ "a" ];
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
                                                                   "1d2dfe4a-c703-49ed-a322-126cfbe65ac6");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "68c11f2c-07c5-475d-b6bf-755440091b3a");
                                                            label = [ "<" ];
                                                            mold =
                                                              {
                                                                out = Drv Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          9;
                                                                      sort =
                                                                        Drv Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          9;
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
                                                                   "206d72ed-2367-4d21-9c76-3428e675fe28");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "3f46fd48-59d5-4f8a-968c-7ae59d94540b");
                                                            label = [ "b" ];
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
                                                                   "84401726-ddb6-4e74-b665-0f2d2bdd3cdc");
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
                                                                   "4d2bc1ab-8b3d-40ae-9d43-f9ce04ec040c");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "3da71d96-9af4-4b86-9249-73a391379bb6");
                                                            label = [ "a" ];
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
                                                                   "a4884900-aefe-43a0-8636-342842050657");
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
                                                         "b5fd92d9-4e52-45e1-8a1e-36e50973451b");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "0865451f-f2a2-42ae-875b-3c668042c4ea");
                                                  label = [ "b" ];
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
                                               "0d639075-9113-4bd0-bfb4-4e8c0582f733");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a928d7b2-8a89-4943-98b2-13406bada42d");
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
                                               "4f937986-b75e-45a2-b886-1881698c4bc0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7a97cb34-a66f-417f-8277-69f54c907b8c");
                                        label = [ "Num" ];
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
                                               "f1a54919-6387-477b-9d38-f39859e04998");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "88eae50b-eedc-455f-ad3d-c109fd420eb7");
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
                                               "8162904c-bcc3-4848-8103-437ddc297465");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "57644540-2045-4612-83d2-90e4880ab1ee");
                                        label = [ "Num" ];
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
                                               "a2d870ab-9bf2-4f26-9879-47080e59c615");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3c8e75a5-013f-4259-833b-928d05127b26");
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
                                               "2e183b32-def8-4245-a4c0-764253db6229");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a064f3c2-7f1b-47d9-ab31-a67869df4ad8");
                                        label = [ "Num" ];
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
                      rule = Some T_FunAnn;
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
                                    content =
                                      [
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "ea988a5f-7b89-41d9-8e24-14a182d024cf");
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
                                                             "e7397832-7902-415d-8caa-94504ca05052");
                                                      label = [ "ctx_a" ];
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
                                      ];
                                    mode = Normal;
                                  };
                                backpack = [];
                                relatives =
                                  {
                                    siblings =
                                      ( [],
                                        [
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8b185a53-260a-4629-9734-af82d1c41363");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "078ad95c-e15b-48c0-90c2-97a01d1fcd90");
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
                                                     "f36c0c19-d5e2-4741-b2d8-eb020f0fb2fb");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "c9416285-5e95-4138-aeac-fcdb6671de39");
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
                                                               "74b2dbb4-5c74-4234-b77c-6d65cb06954f");
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
                                                                  shape =
                                                                    Concave 14;
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "1f395c64-e201-44c9-b5f8-20167106b888");
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
                                                                         "6597c764-b0c1-4add-bcbf-e17fca636d68");
                                                                  label =
                                                                    [ "b" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Pat;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Pat;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Drv
                                                                                Pat;
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
                                                                         "7e4e4cc0-e656-4012-8e6e-23ace500c307");
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
                                                                         "d0275ee8-030b-47dd-84b3-e7c17a9cab34");
                                                                  label =
                                                                    [ ":" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Pat;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                12;
                                                                            sort =
                                                                              Drv
                                                                                Pat;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                12;
                                                                            sort =
                                                                              Drv
                                                                                Typ;
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
                                                                         "d6bdf864-b648-47d2-9f4d-c86394c64bc8");
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
                                                                         "5c8285dc-77fa-41b8-9da3-77fc575ebdb8");
                                                                  label =
                                                                    [ "Num" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Typ;
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
                                                                         "8caa122d-c46f-42dc-a958-710b929a77af");
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
                                                               "155588b2-cf82-4caf-bdae-91608a03d2ff");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f8523e7d-4844-4f7c-a5e7-49b92fe80de5");
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
                                                                         "08a69a6b-6e21-4dd8-8bcf-68bdbc80603e");
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
                                                                         "85e5a794-2476-4145-a698-613bf5d7122c");
                                                                  label =
                                                                    [ "a" ];
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
                                                                         "d88022e9-578b-4fc2-b58c-30046e10da9c");
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
                                                                         "4464b823-1b28-45a8-aed3-2f7dd7733431");
                                                                  label =
                                                                    [ "<" ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Concave
                                                                                9;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Concave
                                                                                9;
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
                                                                         "5560fa4f-73e3-49d7-92b7-671f4d8575a3");
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
                                                                         "e71455a1-189f-4a5c-9706-6e1a08ff19a0");
                                                                  label =
                                                                    [ "b" ];
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
                                                                         "901accb7-355e-406e-9e2c-4e090717b10a");
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
                                                                         "4ddb9c3b-f513-4d83-bd6f-223e7f795b0d");
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
                                                                         "edbecf84-2b19-42fa-a719-bd0c7af882e3");
                                                                  label =
                                                                    [ "a" ];
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
                                                                         "9e780b39-11b4-4a6e-a4da-5cd1c42a8cbb");
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
                                                               "1f9d3bdb-4137-4da4-adf9-296701022859");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ffb06917-ac7d-4fb2-a550-216e0ff0d395");
                                                        label = [ "b" ];
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
                                                     "0f5d890a-41d9-411b-938f-4018a8d4e001");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "4d19c3b2-9446-48a9-a26f-8ef5b8cdca4f");
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
                                                     "4ec7f213-2df1-4702-914f-0e943d3d280d");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "96698d23-6ba1-48bd-9629-47c67ba50b9a");
                                              label = [ "Num" ];
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
                                                     "a07c4fd1-43d1-4173-962c-66f0dd74d9b4");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "863c6471-6b80-4cbc-a2d6-9a7b652f889f");
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
                                                     "32b96e15-2e31-4127-bbbb-aeba80a2e35f");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "028e528e-2178-4e9c-b2de-b905deec4b0b");
                                              label = [ "Num" ];
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
                                        ] );
                                    ancestors = [];
                                  };
                                caret = Outer;
                              };
                            rule = Some T_FunAnn;
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
                                                           "d7884e8d-b567-4a91-bf57-2704114a3c1a");
                                                    label = [ "ctx_ab" ];
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
                                              ],
                                              [] );
                                          ancestors =
                                            [
                                              ( {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a1881c96-91e8-4ddf-8adb-f06563716046");
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
                                                  shards = ([ 0 ], [ 1 ]);
                                                  children = ([], []);
                                                },
                                                ( [],
                                                  [
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "a474b963-9d46-40e2-a36a-45595391a87f");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "0ab7fdb3-494a-4f83-8495-7e36bda5618d");
                                                        label = [ "|-" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 26;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 26;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "7cfd37a7-8c40-45e2-b492-eb072136e43d");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "6b4a6fb6-d04a-465b-886a-e332c9e88ae7");
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "330a39a6-4412-4054-81c6-23d089988f43");
                                                                  label =
                                                                    [
                                                                      "if";
                                                                      "then";
                                                                      "else";
                                                                    ];
                                                                  mold =
                                                                    {
                                                                      out =
                                                                        Drv Exp;
                                                                      in_ =
                                                                        [
                                                                          Drv
                                                                            Exp;
                                                                          Drv
                                                                            Exp;
                                                                        ];
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
                                                                              Concave
                                                                                13;
                                                                            sort =
                                                                              Drv
                                                                                Exp;
                                                                          } );
                                                                    };
                                                                  shards =
                                                                    [ 0; 1; 2 ];
                                                                  children =
                                                                    [
                                                                      [
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "ccd15702-f5e9-4e07-9e94-b7707f2654b4");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "4f8bc615-6092-47bf-be9e-44ea978a87da");
                                                                            label =
                                                                              [
                                                                                "a";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Drv
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "b47f5c8b-4175-457b-b1e5-298eb502e63a");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "f438f57d-fe7f-4983-83de-b66015465ea1");
                                                                            label =
                                                                              [
                                                                                "<";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Drv
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                9;
                                                                                sort =
                                                                                Drv
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                9;
                                                                                sort =
                                                                                Drv
                                                                                Exp;
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "018d0417-3bf9-49e8-9984-7b14846fca38");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "e8452d3a-149b-4b73-bbdf-2f5a0bf049fa");
                                                                            label =
                                                                              [
                                                                                "b";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Drv
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "2035eb95-cacb-4a52-96b9-aa390fad5ce2");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                      ];
                                                                      [
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "7494f939-47d5-49dc-a1b4-2584ebebacc5");
                                                                            content =
                                                                              Whitespace
                                                                                " ";
                                                                          };
                                                                        Tile
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "f8c38ca9-382d-44d3-afb3-397166e3c836");
                                                                            label =
                                                                              [
                                                                                "a";
                                                                              ];
                                                                            mold =
                                                                              {
                                                                                out =
                                                                                Drv
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                                }
                                                                                );
                                                                              };
                                                                            shards =
                                                                              [
                                                                                0;
                                                                              ];
                                                                            children =
                                                                              [];
                                                                          };
                                                                        Secondary
                                                                          {
                                                                            id =
                                                                              Option
                                                                              .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "25371ce1-a326-4f6c-a643-2065211d87a5");
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "0add2745-a1c1-457a-9431-c433b6569116");
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
                                                                         "5031c722-3195-416d-ae64-d2708ca9882f");
                                                                  label =
                                                                    [ "b" ];
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
                                                            ];
                                                          ];
                                                      };
                                                    Secondary
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "6d4ee6d7-97db-41c8-a5a7-582b9669a9a1");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "378f7878-684a-43eb-9b02-89541301298b");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 12;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 12;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ad5a73ac-38b1-476a-be6b-60f9fb0bed24");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "89ef470c-7e40-4a6b-bb64-3be7a9ee0f97");
                                                        label = [ "Num" ];
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
                                                  ] ) );
                                            ];
                                        };
                                      caret = Outer;
                                    };
                                  rule = Some T_If;
                                },
                              [
                                Node
                                  ( Just
                                      {
                                        jdmt =
                                          {
                                            selection =
                                              {
                                                focus = Right;
                                                content =
                                                  [
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "bc095666-3260-47e8-8de2-5a9c5207a559");
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "561464cf-0ef3-4b84-8eee-4c0555ccd63b");
                                                                  label =
                                                                    [ "ctx_ab" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "dee40e86-a559-4ce1-abbd-c1d9cc06b49b");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "b8955a78-2264-4ce3-9b6c-55b42eec93a9");
                                                        label = [ "|-" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 26;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 26;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "d04d64eb-65ee-4ca7-92cb-a7af69bd7df4");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "e706f00c-b463-4879-be0c-6c917fd9a140");
                                                        label = [ "a" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "b36fedd5-10d0-4325-818c-27daadf4ae11");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2f20049c-c2d6-433e-8a22-fdebd25785e8");
                                                        label = [ "<" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 9;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 9;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2bb3a32b-e925-4cc3-94ec-80ec1113f080");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "701ddbd9-bc42-4ddb-a4e6-cb2d2e490943");
                                                        label = [ "b" ];
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "11c8764d-39a4-4a42-94be-3788a29c8e9d");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "1bb536d6-c6e1-4baa-91ce-957b34daef89");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Drv Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 12;
                                                                  sort = Drv Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 12;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "3d31dc30-b3b7-4739-9400-bb2fd57b8f2d");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "ac0e982a-2994-40ed-a27a-ef19ae5b5fa9");
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
                                                mode = Normal;
                                              };
                                            backpack = [];
                                            relatives =
                                              {
                                                siblings = ([], []);
                                                ancestors = [];
                                              };
                                            caret = Outer;
                                          };
                                        rule = Some T_Lt;
                                      },
                                    [
                                      Node (Abbr (Some 0), []);
                                      Node (Abbr (Some 1), []);
                                    ] );
                                Node (Abbr (Some 0), []);
                                Node (Abbr (Some 1), []);
                              ] );
                        ] );
                  ] );
            ];
        };
  }
