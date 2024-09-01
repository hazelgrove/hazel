[(ConstructorEntry
  { name = "E";
    id =
    Option.get(Haz3lcore.Id.of_string("e08fe5ea-548b-44a1-b4e7-b78e9154c057"));
    typ = { ids = <opaque>; copied = <opaque>; term = (Var "D") } });
(ConstructorEntry
   { name = "F";
     id =
     Option.get(Haz3lcore.Id.of_string("e08fe5ea-548b-44a1-b4e7-b78e9154c057"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = (Var "D") }))
       }
     });
(TVarEntry
   { name = "D";
     id =
     Option.get(Haz3lcore.Id.of_string("e6c74f7c-3fd7-44e2-a2df-7e7b78751525"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("E",
                 [Option.get(Haz3lcore.Id.of_string("f29156b4-f5fb-4acc-bf97-fb7260758c11"))
                   ],
                 None));
               (Variant ("F",
                  [Option.get(Haz3lcore.Id.of_string("504827d5-810c-4b17-b44d-81932ddefed7"));
                    Option.get(Haz3lcore.Id.of_string("b6725de1-d2d7-480a-9884-31a0276ac16f"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>; term = Int })
                  ))
               ])
          })
     });
(ConstructorEntry
   { name = "C";
     id =
     Option.get(Haz3lcore.Id.of_string("9e1e96e1-4976-4365-9799-f35a758b65ed"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = (Var "B") }))
       }
     });
(TVarEntry
   { name = "B";
     id =
     Option.get(Haz3lcore.Id.of_string("a21719e9-6fb0-4675-88f9-e62ed542c364"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("C",
                 [Option.get(Haz3lcore.Id.of_string("ce6cbac4-5a8d-492d-880d-8b37d3509115"));
                   Option.get(Haz3lcore.Id.of_string("5665716a-e189-415c-b6a8-5ba1bf088ef3"))
                   ],
                 (Some { ids = <opaque>; copied = <opaque>; term = Int })))
               ])
          })
     });
(ConstructorEntry
   { name = "A";
     id =
     Option.get(Haz3lcore.Id.of_string("63a2341d-caf5-4d9e-a4a6-46a0268c8071"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "A") } });
(TVarEntry
   { name = "A";
     id =
     Option.get(Haz3lcore.Id.of_string("87cd4a6b-90e5-46dc-ad5b-362502f7331d"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("A",
                 [Option.get(Haz3lcore.Id.of_string("5bd171cd-7a39-4c03-b0e3-91454d21ad44"))
                   ],
                 None))
               ])
          })
     });
(ConstructorEntry
   { name = "$e";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "$Meta") } });
(ConstructorEntry
   { name = "$v";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "$Meta") } });
(ConstructorEntry
   { name = "Falsity";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "Prop") } });
(ConstructorEntry
   { name = "Truth";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "Prop") } });
(ConstructorEntry
   { name = "Implies";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "Prop") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }))
       }
     });
(ConstructorEntry
   { name = "Or";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "Prop") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }))
       }
     });
(ConstructorEntry
   { name = "And";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "Prop") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }))
       }
     });
(ConstructorEntry
   { name = "Atom";
     id =
     Option.get(Haz3lcore.Id.of_string("08a8a4d6-7d2c-4bca-8a3a-6a169db6be4e"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }))
       }
     });
(TVarEntry
   { name = "Prop";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("Falsity",
                 [Option.get(Haz3lcore.Id.of_string("b1bc9711-cb41-4cf2-9710-fe4d2051cfb2"))
                   ],
                 None));
               (Variant ("Truth",
                  [Option.get(Haz3lcore.Id.of_string("724be289-344d-4db8-8333-31dee7ff34c9"))
                    ],
                  None));
               (Variant ("Implies",
                  [Option.get(Haz3lcore.Id.of_string("ed66d4be-e444-4708-8dfb-fae210fc5e1b"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term =
                          (Prod
                             [{ ids = <opaque>; copied = <opaque>;
                                term = (Var "Prop") };
                               { ids = <opaque>; copied = <opaque>;
                                 term = (Var "Prop") }
                               ])
                          })
                  ));
               (Variant ("Or",
                  [Option.get(Haz3lcore.Id.of_string("8480e572-0374-40e6-8539-9cc7790cb377"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term =
                          (Prod
                             [{ ids = <opaque>; copied = <opaque>;
                                term = (Var "Prop") };
                               { ids = <opaque>; copied = <opaque>;
                                 term = (Var "Prop") }
                               ])
                          })
                  ));
               (Variant ("And",
                  [Option.get(Haz3lcore.Id.of_string("1dead23d-d2e7-4257-9197-5f3ede711818"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term =
                          (Prod
                             [{ ids = <opaque>; copied = <opaque>;
                                term = (Var "Prop") };
                               { ids = <opaque>; copied = <opaque>;
                                 term = (Var "Prop") }
                               ])
                          })
                  ));
               (Variant ("Atom",
                  [Option.get(Haz3lcore.Id.of_string("b8d6ef9e-7cd6-48f6-a84b-4523c5dfbd33"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>; term = String
                          })
                  ))
               ])
          })
     });
(ConstructorEntry
   { name = "OpTimes";
     id =
     Option.get(Haz3lcore.Id.of_string("c2c86276-333e-489f-9439-55e86ad917cb"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "BinOp") } });
(ConstructorEntry
   { name = "OpMinus";
     id =
     Option.get(Haz3lcore.Id.of_string("c2c86276-333e-489f-9439-55e86ad917cb"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "BinOp") } });
(ConstructorEntry
   { name = "OpPlus";
     id =
     Option.get(Haz3lcore.Id.of_string("c2c86276-333e-489f-9439-55e86ad917cb"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "BinOp") } });
(TVarEntry
   { name = "BinOp";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("OpTimes",
                 [Option.get(Haz3lcore.Id.of_string("29a830cf-36ab-4271-87b9-fe9c58a55d74"))
                   ],
                 None));
               (Variant ("OpMinus",
                  [Option.get(Haz3lcore.Id.of_string("13de30b6-a56e-4497-9a16-45ebf8ce913c"))
                    ],
                  None));
               (Variant ("OpPlus",
                  [Option.get(Haz3lcore.Id.of_string("f11bdad4-7c57-4147-b22a-efa0e10474f6"))
                    ],
                  None))
               ])
          })
     });
(ConstructorEntry
   { name = "OpNeg";
     id =
     Option.get(Haz3lcore.Id.of_string("6a22ad78-5743-48e0-9cd7-c72662af4d46"));
     typ = { ids = <opaque>; copied = <opaque>; term = (Var "UnOp") } });
(TVarEntry
   { name = "UnOp";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("OpNeg",
                 [Option.get(Haz3lcore.Id.of_string("fafe790d-71dc-4fda-b1de-5046aea1f998"))
                   ],
                 None))
               ])
          })
     });
(ConstructorEntry
   { name = "Entail";
     id =
     Option.get(Haz3lcore.Id.of_string("6649ced5-2bd8-4181-b80d-f4d091a2f2ee"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>;
                  term =
                  (List
                     { ids = <opaque>; copied = <opaque>;
                       term = (Var "Prop") })
                  };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Prop") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Judgement") }))
       }
     });
(ConstructorEntry
   { name = "Eval";
     id =
     Option.get(Haz3lcore.Id.of_string("6649ced5-2bd8-4181-b80d-f4d091a2f2ee"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "Expr") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Judgement") }))
       }
     });
(ConstructorEntry
   { name = "Val";
     id =
     Option.get(Haz3lcore.Id.of_string("6649ced5-2bd8-4181-b80d-f4d091a2f2ee"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = (Var "Expr") },
          { ids = <opaque>; copied = <opaque>; term = (Var "Judgement") }))
       }
     });
(TVarEntry
   { name = "Judgement";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("Entail",
                 [Option.get(Haz3lcore.Id.of_string("b97c9112-0403-417f-ba29-641342c35213"))
                   ],
                 (Some { ids = <opaque>; copied = <opaque>;
                         term =
                         (Prod
                            [{ ids = <opaque>; copied = <opaque>;
                               term =
                               (List
                                  { ids = <opaque>; copied = <opaque>;
                                    term = (Var "Prop") })
                               };
                              { ids = <opaque>; copied = <opaque>;
                                term = (Var "Prop") }
                              ])
                         })
                 ));
               (Variant ("Eval",
                  [Option.get(Haz3lcore.Id.of_string("3b5e6105-eeba-41f8-8ff5-2646877ba6e8"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term =
                          (Prod
                             [{ ids = <opaque>; copied = <opaque>;
                                term = (Var "Expr") };
                               { ids = <opaque>; copied = <opaque>;
                                 term = (Var "Expr") }
                               ])
                          })
                  ));
               (Variant ("Val",
                  [Option.get(Haz3lcore.Id.of_string("2092d206-414b-4ce8-847f-2bae954ecb42"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term = (Var "Expr") })
                  ))
               ])
          })
     });
(ConstructorEntry
   { name = "BinOp";
     id =
     Option.get(Haz3lcore.Id.of_string("0ccfa954-8dd4-4a08-9308-31acf47f94df"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "BinOp") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Expr") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }))
       }
     });
(ConstructorEntry
   { name = "UnOp";
     id =
     Option.get(Haz3lcore.Id.of_string("0ccfa954-8dd4-4a08-9308-31acf47f94df"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = (Var "UnOp") };
                 { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }))
       }
     });
(ConstructorEntry
   { name = "NumLit";
     id =
     Option.get(Haz3lcore.Id.of_string("0ccfa954-8dd4-4a08-9308-31acf47f94df"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = (Var "Expr") }))
       }
     });
(TVarEntry
   { name = "Expr";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("BinOp",
                 [Option.get(Haz3lcore.Id.of_string("1310f0ef-123f-4173-8200-96c17679c0ad"))
                   ],
                 (Some { ids = <opaque>; copied = <opaque>;
                         term =
                         (Prod
                            [{ ids = <opaque>; copied = <opaque>;
                               term = (Var "BinOp") };
                              { ids = <opaque>; copied = <opaque>;
                                term = (Var "Expr") };
                              { ids = <opaque>; copied = <opaque>;
                                term = (Var "Expr") }
                              ])
                         })
                 ));
               (Variant ("UnOp",
                  [Option.get(Haz3lcore.Id.of_string("27e8ae7d-3edc-438f-ad89-f9c75bf50fef"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>;
                          term =
                          (Prod
                             [{ ids = <opaque>; copied = <opaque>;
                                term = (Var "UnOp") };
                               { ids = <opaque>; copied = <opaque>;
                                 term = (Var "Expr") }
                               ])
                          })
                  ));
               (Variant ("NumLit",
                  [Option.get(Haz3lcore.Id.of_string("b327c7c9-dbc1-46eb-80ad-07d631383334"))
                    ],
                  (Some { ids = <opaque>; copied = <opaque>; term = Int })
                  ))
               ])
          })
     });
(TVarEntry
   { name = "$Meta";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     kind =
     (Singleton
        { ids = <opaque>; copied = <opaque>;
          term =
          (Sum
             [(Variant ("$e",
                 [Option.get(Haz3lcore.Id.of_string("1945bd61-e806-42cd-aab3-f96c991c3674"))
                   ],
                 None));
               (Variant ("$v",
                  [Option.get(Haz3lcore.Id.of_string("4076c7fe-c1ed-4f0a-ab1e-036089a2fa92"))
                    ],
                  None))
               ])
          })
     });
(VarEntry
   { name = "string_sub";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = String };
                 { ids = <opaque>; copied = <opaque>; term = Int };
                 { ids = <opaque>; copied = <opaque>; term = Int }])
            },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "string_concat";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = String };
                 { ids = <opaque>; copied = <opaque>;
                   term =
                   (List
                      { ids = <opaque>; copied = <opaque>; term = String })
                   }
                 ])
            },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "string_trim";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "string_compare";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = String };
                 { ids = <opaque>; copied = <opaque>; term = String }])
            },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "string_length";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "mod";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow (
          { ids = <opaque>; copied = <opaque>;
            term =
            (Prod
               [{ ids = <opaque>; copied = <opaque>; term = Int };
                 { ids = <opaque>; copied = <opaque>; term = Int }])
            },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "atan";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "acos";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "asin";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "tan";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "cos";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "sin";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "sqrt";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "log10";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "log";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "exp";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "floor";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "ceil";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "abs_float";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "abs";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "bool_of_string";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = Bool }))
       }
     });
(VarEntry
   { name = "float_of_string";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "int_of_string";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = String },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "string_of_bool";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Bool },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "string_of_float";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "string_of_int";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = String }))
       }
     });
(VarEntry
   { name = "float_of_int";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Int },
          { ids = <opaque>; copied = <opaque>; term = Float }))
       }
     });
(VarEntry
   { name = "int_of_float";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Int }))
       }
     });
(VarEntry
   { name = "is_nan";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Bool }))
       }
     });
(VarEntry
   { name = "is_infinite";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Bool }))
       }
     });
(VarEntry
   { name = "is_finite";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ =
     { ids = <opaque>; copied = <opaque>;
       term =
       (Arrow ({ ids = <opaque>; copied = <opaque>; term = Float },
          { ids = <opaque>; copied = <opaque>; term = Bool }))
       }
     });
(VarEntry
   { name = "min_int";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Int } });
(VarEntry
   { name = "max_int";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Int } });
(VarEntry
   { name = "pi";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Float } });
(VarEntry
   { name = "epsilon_float";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Float } });
(VarEntry
   { name = "nan";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Float } });
(VarEntry
   { name = "neg_infinity";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Float } });
(VarEntry
   { name = "infinity";
     id =
     Option.get(Haz3lcore.Id.of_string("00000000-0000-0000-0000-000000000000"));
     typ = { ids = <opaque>; copied = <opaque>; term = Float } })