let startup : PersistentData.t =
  {
    settings =
      {
        captions = true;
        secondary_icons = false;
        statics = true;
        dynamics = true;
        async_evaluation = false;
        context_inspector = false;
        instructor_mode = true;
        benchmark = false;
        mode = Examples;
      };
    scratch =
      ( 0,
        [
          {
            zipper =
              "((selection((focus \
               Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
               84074f5f-951c-4dd8-931e-436317c54aa1)(content(Whitespace\" \
               \")))))((Grout((id a6fcd6f6-fe59-4178-93dd-6f0fbec53269)(shape \
               Convex))))))(ancestors())))(caret Outer))";
            backup_text = "  ";
          };
        ] );
    examples =
      ( "Introduction",
        [
          ( "Introduction",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 caf14c09-7dab-41b0-8c24-b3c18b396f13)(content(Comment\"# \
                 Welcome to Hazel! #\"))))(Secondary((id \
                 a485bc1b-59b3-4b7b-ad40-ef510ecb3a06)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1fcafdb3-62e6-4208-bea4-a1df9dc88fce)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d1f85a08-a508-464e-a951-16e13d141176)(content(Comment\"# This \
                 is a program cell, which consists of a structured editor  \
                 #\"))))(Secondary((id \
                 c3d5b456-ba54-4c18-8d04-41c5b2f3492c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 fc64838c-0f6c-4bd4-bda4-3e9183db95b1)(content(Comment\"# at \
                 the top and its evaluated result at the bottom. Right now,  \
                 #\"))))(Secondary((id \
                 17c41438-956a-4958-91da-bf6b93c59d42)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 65d8354e-1dfe-4e01-a6e5-b10113f0b644)(content(Comment\"# that \
                 result has a question mark, as the program is incomplete! \
                 #\"))))(Secondary((id \
                 37e5e748-d04c-459d-ae60-bec1289547b6)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 39944d68-b5d2-42b2-ac99-d9ab89e4dbae)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 686b2878-2d78-4c15-97cb-3433ff63773e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 091afcc2-dd85-4487-a93b-565d6d32641c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adf98a73-0f13-4b8c-b213-28cfdb0a4b9c)(label(your_function))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 832626bc-6e86-44a7-8568-302da5143530)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a65594fb-5336-40fc-ac1b-ba4c497eae1a)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b9da2827-0995-4561-8cd6-edc91805d774)(content(Comment\"# Fill \
                 the hole below to see how the result changes \
                 #\"))))(Secondary((id \
                 8003b3d2-7465-47aa-aefd-271a3750a8d2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9fcfa127-a880-4f86-8e65-7a516251414e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d408fc42-9f62-4365-b8d5-4bd87431af32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5c13677-68dd-407f-8d83-786383b50e16)(label(parameter))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 6dcba105-f871-4a58-8245-486686f6161f)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 f5814ac2-6bc6-4a38-a9f4-d9861d5cff95)(shape \
                 Convex)))(Secondary((id \
                 5dcf7280-c5f1-40ef-a6d9-b2a04fc26691)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 20ef1fc6-0075-4240-b572-b8a1782675b2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7e9fcabf-3c3a-42a5-b9c4-09c31ebefd6c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 dfcfbe99-2936-4b87-b73a-d50be4feeb22)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c1685dd3-30cb-4000-b5ef-fa60ff818b88)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ce42d211-7831-4c6b-aa4b-c9dd0efc7056)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 b451f53e-6607-49ea-8f87-591e692d5f64)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0dfc285c-90dd-4af0-9c22-2c7a5e308e89)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 16609792-7d04-4ed7-9319-ed7f2008de4d)(content(Comment\"# Here \
                 in Examples Mode, you can use the upper left dropdown to  \
                 #\"))))(Secondary((id \
                 0fc96e04-fbfe-49a3-9d21-29a66adb72a8)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7acb0335-7a56-4fc1-ade3-eb138413bfde)(content(Comment\"# \
                 browse Hazel language and editor features \
                 references.          #\"))))(Secondary((id \
                 6fb35260-65c1-4260-a68e-07f9266a9b90)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d238ee9b-a959-400b-8098-3b5fe8531bf5)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 ae2787e6-5e7f-4b91-8a31-1c2e629e61eb)(content(Comment\"# \
                 Select Scratch Mode from the upper left dialog to access \
                 blank #\"))))(Secondary((id \
                 abcf6a37-730a-42a6-a884-5a7e882558de)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a2c6be2e-e6eb-479d-b1a0-ded93a7445a1)(content(Comment\"# \
                 cells where you can write code; use the arrows to \
                 navigate.    #\"))))(Secondary((id \
                 a9690ada-c6cb-4716-8af6-159fc0e7f784)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f79f4d54-db8a-48f8-9fe9-feabd956e3f2)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1a3d42ce-dbc9-47a7-ba6b-1ae6fa339307)(content(Comment\"# \
                 Select Exercise for a small functional programming \
                 tutorial.   #\"))))(Secondary((id \
                 b029ec91-ee00-4422-9b81-3cfd1c883e32)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 d4facda4-3ec4-4e3a-a26b-34d985d64bf9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fe072c3e-3463-4827-9c37-b037011c465c)(label(your_function))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4ad392fd-c897-4f83-9245-fd22c1adb66f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 50ec3143-8dde-4262-8258-6b3f86477c70)(label(\"\\\"argument\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d2ee964b-b036-4a14-ad68-de1ae4697644)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2a70019b-5b71-4c54-bbd4-2cd3d20f0d65)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7e9fb119-9baa-4cbd-9eea-e9f14bc8f3bc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d072cadb-d309-4b70-ad37-5953f7ce9bdb)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b7b9f1c2-9124-4895-85d7-a16acfaf3233)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# Welcome to Hazel! #\n\n\
                 # This is a program cell, which consists of a structured \
                 editor  #\n\
                 # at the top and its evaluated result at the bottom. Right \
                 now,  #\n\
                 # that result has a question mark, as the program is \
                 incomplete! #\n\n\
                 let your_function =\n\
                 # Fill the hole below to see how the result changes #\n\
                 fun parameter ->      \n\
                 in\n\n\
                 # Here in Examples Mode, you can use the upper left dropdown \
                 to  #\n\
                 # browse Hazel language and editor features \
                 references.          #\n\n\
                 # Select Scratch Mode from the upper left dialog to access \
                 blank #\n\
                 # cells where you can write code; use the arrows to \
                 navigate.    #\n\n\
                 # Select Exercise for a small functional programming \
                 tutorial.   #\n\n\
                 your_function(\"argument\") + 1\n";
            } );
          ( "Basic Reference",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 d6095d2a-c3a1-4e51-a6b9-ce46efa762ed)(content(Comment\"# \
                 Hazel Language Quick Reference #\"))))(Secondary((id \
                 0e7c6baa-4af8-42a1-b2a3-48ed11473c9c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0cb0ab73-ee1c-4d0b-9229-5cdaf8d935d4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 369c7b76-47a4-449f-ab5b-e9fefd612f7e)(content(Comment\"# \
                 Empty holes stand for missing expressions, patterns, or types \
                 #\"))))(Secondary((id \
                 7f6d6dc3-e50a-4e7c-b743-746a45f0018c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1fd6e990-dc13-4180-b9de-687df2161be4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 54e11594-b1ad-47b2-8c99-c233e9c464a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83abc7c4-6710-4614-9b30-df1652698223)(label(empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ac67e417-ac17-4f47-9d75-107b4cc018f9)(content(Whitespace\" \
                 \")))))((Grout((id \
                 7a84292f-f3aa-4402-b666-2296d15ab7d5)(shape \
                 Convex)))(Secondary((id \
                 c520bad9-796b-4905-9435-6c73b030488d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d44b0561-35d8-47cf-a02f-bb3cb8e70d46)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4f103c0d-3978-4c87-b810-b473e398d00a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b637fe5e-5c5c-4081-abbf-d9b1c9afe070)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c0040318-92bb-4af0-8c27-7d497b409c77)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 55316bb0-42f6-4797-a145-421bfae72f64)(content(Comment\"# \
                 Integers #\"))))(Secondary((id \
                 9a7a5b36-2af9-4aa2-b745-753eeacd117c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 00527677-063b-4ae0-8fbf-d2e8cddbc00c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 549595b5-48b8-4521-8898-41dd2c12bf1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 881a1d81-e20a-4a04-b9d4-d51bcc58b251)(label(int_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d14fc0cc-09ed-45d7-bc60-709e2a444073)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ecf08c05-a676-4190-ab05-6b1532afbce3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 27930e91-56cc-4866-8112-7d30034b94d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6d7b32fc-1fa8-4d03-bf4f-8560bf11e909)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 57d3bf68-3ca9-4508-b02e-f9e09383c35b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3215e0fb-e7a9-4d8a-9678-58c0005a108e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5fe4b26a-fff4-44c0-a667-4aafa6bca2e0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b3d9cd6d-4fee-4427-b7c8-0881106c2e4d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2cc5b8fa-8029-47ea-8abe-9f88399d38c2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7d6d04a2-f23e-499d-9e7d-c35145221788)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3a254177-4c30-476b-ad0e-3e74e7f4a097)(content(Whitespace\" \
                 \"))))(Tile((id \
                 070aee60-5a1a-4133-8bbb-591e1f1df5a8)(label(negation))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3f435134-d754-4467-a183-b5a775fe89f0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ca8ff05b-d17c-460a-9b40-26a9a3048e6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40f9eda9-4c76-4193-8bb8-7bb91be6bea4)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave \
                 2))(sort Exp))))))(shards(0))(children())))(Tile((id \
                 61fd8eb9-d09f-421d-9e09-ac63d148b5aa)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6a254b7a-c812-49ef-a272-b73344f9bb83)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fa006494-8e34-422b-88f9-650d36f41766)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 409b0749-39f8-4888-a510-d58bbd4d30f7)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ecb5d6b1-13a2-45bc-95bd-48a36715eee9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d412ec99-33ff-49a1-892d-8f655be5be19)(label(arithmetic))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 63572398-a8bf-4605-8d69-b6d04d045502)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2fe66e06-04a2-4ab8-9cf0-4ce652e816f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 35a33a86-a957-4d04-be6e-5a683b0333ff)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a9792420-4568-4b30-96ef-2181ea5ace41)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 3f8e5ca8-8230-405e-83bd-becc685a78fa)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8ab239e3-3896-4318-9848-677ad32b4987)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8cfe5475-4434-4310-bf12-b4b750ef8b3a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0c6ff971-7cc6-46d5-9dcc-8828476c8468)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2db1df34-9614-4960-883b-8cd02cf3a19e)(label(8))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5889e480-85ee-4305-af1f-a4a9520258ed)(label(/))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 b842d6d3-81f4-4b36-ae2f-98f77ef29647)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9daae471-dece-468a-afbf-aea5c032806b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e13820bf-3c8f-4364-9be4-fd9d390cec31)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8691e7b3-3b4e-4059-b403-88ff6fe4cea5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fe7aecce-b71f-42fd-9f7c-6f77ebfbe260)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0de3c71b-0579-4841-8d67-98ac28ad6b57)(label(int_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2966c441-69c8-479c-abe2-7300d50b94d5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 898e7e55-8e56-4d5d-80a0-e6a3bb1b0ad0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f82639b-6e2d-4fba-af0b-906e86313e47)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 27149311-6fc1-4e9e-b91d-3e095225bcb0)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b889fd12-426c-46ef-a63a-d786aea37081)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57eb3244-cd7d-4108-a041-b1e139c90577)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0e163481-85ad-47d9-8bac-437012bb9198)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ad3f3a6f-9767-4fa3-bd7e-6dc5f1fe2736)(label(10))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4094776f-062b-411e-b340-d03a67b93357)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 65ab87b4-57d2-4f37-a435-ed29aa80b070)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5e7acc8c-fc84-4c4a-bcd0-c0c7d7a1923b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f779cc08-9e71-4b0d-afab-9539589c6a24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3ed1975c-6888-405f-a43c-7eb0392b209a)(label(<))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2c317591-a036-4730-8e32-f92becdf3cf5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bb08a809-9cfa-4553-adff-f156790ab7bb)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a5b9b4eb-4f30-430d-a6b4-9a2c4d3b3915)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 71cc07d1-5882-443d-bf80-a1696da5e429)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b3483f8-4255-436c-9689-3701254f2cca)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e2053d3b-8a46-48f8-9fd4-4477e4668b1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 91d7cfb3-8903-4cf3-902c-00a5767677f5)(label(<=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5c5f193e-166b-43f3-9164-a529fdafb763)(content(Whitespace\" \
                 \"))))(Tile((id \
                 347af5ad-5f2e-43b3-bc61-2407bb769498)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 82b918c8-c440-4bd2-b917-6657e224f0cf)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a35996f8-08e1-4b1a-b2d4-29a59ecbe3be)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ea194881-3517-4249-ae8c-a7956b55dbbd)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e217b81f-9062-4527-9989-ac503677239a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ab12dad-38d7-466f-bc83-a43471daa0c2)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3d72def0-d5af-41cc-bae4-6e4645b41d74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fc603add-229a-4ecd-8a99-bd66261ff60c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6391d557-c700-4dd0-b143-b3f99a598e85)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 b881d671-587c-4e3f-bdd8-3ba5ae5dc967)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c710336a-bffd-4aee-a66b-2eb2fbec563d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6ee826a7-4b42-4f9d-aa3a-d46e5251fe2c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0d58afec-59fc-43a9-b1c9-b4a295e275cf)(label(>=))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 98ec1faf-1727-4cc8-9527-a85bff5264ef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 baedc803-75f2-4d97-9c67-82ad65a3c55e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 41ea6606-ad49-4503-b6ea-f17f851017bc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f4f73754-5d21-40de-84d1-7abee875c93c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 61ddccc9-371b-4b72-a804-f171ba420039)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 269cac9f-780e-4a8a-a427-b0e54a4b316a)(content(Comment\"# \
                 Floating Point Numbers #\"))))(Secondary((id \
                 98eb3edc-e596-4ef3-8778-4e58c0a8b95e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e6a915d3-875f-44ca-b6a3-c8a10846122f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0629fe1f-d465-4e49-b3a5-36275870cabc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07020ad8-03dd-4e9c-80d5-c1d7b705c287)(label(float_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 eb54082a-3226-4f08-9106-b57b9681c17c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e19a4ba8-22f2-446d-9d6b-4559bb89b2cc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9fbe71e7-7c9d-452e-a116-94226e63f8f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 287480de-7647-4586-89ea-5309af66b2b1)(label(Float))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1675a412-8b12-4e5a-8b5f-3850bd7f2610)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 273fe322-ebd4-47b9-9916-697c8895419b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f6a612f-f5e9-4c21-b6bb-29dcbdb74c71)(label(1.5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 24d9edb2-4c36-4aa1-83f4-6261b5fb467a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3524b654-8726-4d1f-bada-fcdc83280c54)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b597821a-2d12-4c63-aee9-29ca2120049e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 678d352d-5e46-43e1-9154-4ee72d1fb36d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42c1d5bf-6881-40a5-82f5-9832c531a9f2)(label(float_artih))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 51fee63d-c336-4b0c-8d27-4a4b9f8355d0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 700b3734-f190-4e03-8002-a2ea39e66c91)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13f9d682-344b-4ecb-bbd9-b882266ca614)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 cd82c627-b4b0-40c6-9407-5ac8923f7a1d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 678a672d-6955-4185-ab84-bc8a2143539b)(label(*.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2759e5e5-58d1-4afb-8f2a-90e04bfd8baa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e3e9b3a2-e589-4de0-8664-ceefb8412233)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 557675e1-7b7c-4a53-bfc0-2ee69dd3b321)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b0647af-2d5d-4ed6-97c3-cc8e2ca26ac8)(label(+.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 42766197-e123-40a0-9c08-21142bb74a93)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cb02db44-27ee-47b0-b5f8-4e7aec6a5972)(label(8.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1cad29d9-6d69-4724-8fe8-1a515208f4a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe599188-4bcf-4121-a033-4c0b4adb315e)(label(/.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7b1b07ee-f3cb-4a36-b18f-29a1716ee232)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6119e82b-afd7-4a0e-aa8a-c7e428bfa628)(label(4.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0a53ffa6-6883-4074-afdd-8edf92eeda8a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 025bcfcb-27b8-4db9-b69a-65f5c3ec49ee)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7268fc75-df1f-4b0c-b23d-ce439443ff65)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8598d943-979d-4d6c-be4f-d5b1d54c6836)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f26990a8-8f3c-4b24-aaa7-2dc39ce34a2a)(label(float_comparison))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 09bc54ef-1bbe-47ea-b2f9-37bb11844fa9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 41d9469b-47f2-4452-9672-2881b8412374)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b843bb46-aaef-4609-bfd4-f6495bf32589)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 75d64281-b029-4a61-8c67-c2e466f60f5e)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 17bf6dcc-1bd4-4b96-9dcd-4165117780a8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f970f27-03cc-4785-959f-bd677c3d074d)(label(==.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 24e759db-b664-4485-8ddc-bba92a61f04f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f61f81cd-1a48-49ba-8fe4-37f1aaa17a0a)(label(10.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b7edf03c-abab-483e-abf1-d70c3541013d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e9f19f7f-7cad-4aa7-9b49-d0674991a1e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 334e2554-fbfe-4d70-b626-6cb5a475791c)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f88aa2a8-1414-4c8f-ba73-fd642dc92e53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc61a395-cb23-4c88-93df-841183ed8049)(label(<.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7607c1a4-7104-446b-873a-66f8e92795ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 81916e33-13e9-40ab-9219-3a5a3c14ff7d)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 67a9fee7-3200-45c6-8b5e-de424b9b12e9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1e540f63-8bad-41a1-b4ae-16c993fac281)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93382261-3057-4cf2-926b-e5808023d242)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a55f9cf6-ec9c-40bd-ae14-bf3d84f98be8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e696af81-440c-4331-a6a5-16fe32d83fdf)(label(<=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c07082f4-7609-4f85-82a1-fcf7266da577)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1195305b-e41f-435f-91d9-efc19374ffbd)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4d99172d-51c4-405c-a8c4-db7eb71c6744)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 faa1ee82-d931-4a71-9d8e-489cb03935a7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50636239-caf0-4afd-8a71-3e7097224ba8)(label(3.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5861a960-ba6d-4121-9b65-64b7c890f873)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f55a8508-165b-4e92-b265-2938d490929b)(label(>.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5c286526-f395-4954-b7e6-4cd6518b2511)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ce4efb0-ad42-4046-94b5-48d91db6d279)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b1b1f51b-075c-4790-ac54-be1510809f14)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 477d5407-664d-49a6-b15b-4990e6db56e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8e8b9a9a-f3d3-4b4d-ac36-3193c14d76e1)(label(2.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4db5772b-ffe1-4cba-9686-df9f9d3490e3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1ffe947-89af-40bd-bb9b-665ba3ef42ce)(label(>=.))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2c23c282-814d-4b9b-a5f6-63a803d8c526)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d1a3a2c3-8f40-4176-948d-28761da9059b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 720be9d8-4020-497d-b01a-3bc9ad2ec737)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5509b34a-4108-442c-8c7d-cdbcc1f75393)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 93b4e2b3-0143-405f-a6e8-2de7ae05be48)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0fe32d5d-2101-4bb7-b0c0-aba3d67b36ca)(content(Comment\"# \
                 Booleans #\"))))(Secondary((id \
                 17a7336e-e108-4e20-ba42-5a56918e82e7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a1172adf-3f6e-4b59-97d1-31e3ab608d6e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ab736488-8cfd-4295-b3e1-a424c842174f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3fcae931-1610-43d7-a4ab-e6b032a02e71)(label(booleans))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f79ec242-8a33-4265-920d-2a466aaa7dbf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5cd02c4-8184-4be1-a15d-46a092fed9df)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ef0c22fa-3a28-4def-9491-a86f2e9d48a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40b9037a-1a75-4256-b20d-76bf4721aea0)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e34e1406-c2e7-4516-bdf5-a2a792d08dab)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 52773967-0a8d-4938-9c8c-fa87577d71da)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 09f91262-d479-4494-ac36-51d3db7afd73)(content(Whitespace\" \
                 \"))))(Tile((id \
                 712c55f6-3c2b-4c26-bb0d-6576ea415008)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 fc739bd0-97ab-445e-89f3-97d11f056d7a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 87056b20-d078-448c-8749-74f0262c61d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b152ef4d-668e-4c99-b566-a25eb76b1311)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d17a4c07-80ab-43a3-a235-8832e587b101)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 3130b267-1f5e-4f72-a9f6-58f19fd67e46)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 39bb6eeb-d8c3-4372-b7dd-bac830bcd46a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 439ae3af-c0f3-450d-b41d-30250692f297)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 17159189-d7cd-47d3-8fbc-a17504e13026)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a9e7a801-9e1f-4688-a269-86ee6353324b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 efb05e98-cb30-475d-9c24-5c74fb36891e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 20ee63b8-e490-48d5-b3b7-fd7e4a795eb1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24edac6a-9797-44e4-bb18-c48d74311916)(label(conditionals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7029f815-c89f-493d-aef5-a9e2e22fde76)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8ae441ea-8606-4daf-a139-dd4300ff73d1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d2fcc16c-e80f-49d0-afcd-607345451bc0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 07485112-22e1-44c3-aba9-256cf2f65efc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 398f6962-fcce-4b38-8012-1c4b81f8bb0d)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ef51b6ea-85bc-4a30-8d1d-4df812899884)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b6b45555-f1ac-4d94-be0f-60848de2533d)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 4122f947-79ac-4a1c-89ab-59757fe824a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d77206e-c557-42c9-9b12-8754f6c56c27)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 754493b3-4fb1-4364-973f-a9e26b925004)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9527f7a7-fd69-4f8c-a49a-d4c096dcb324)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3793d403-6c1a-473f-9f09-8162721ee893)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b429f81f-480d-4a46-ae3a-fca2bbc3b83d)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6087feea-d171-4e96-946b-7b0007d50855)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d308fe65-d96b-4bfe-906b-e798a51a2669)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1024151c-9cf2-417b-aee8-55e9d836c71a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae857834-12fe-44ae-9d50-aa02ddfb742a)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1a8e0168-b3d8-44b0-8f6e-c48fab7f6f14)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 cc30918e-7ca4-4720-bd06-2d77d545cc03)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1164a834-99e2-4d87-89c1-97539cb4652f)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fe868289-4544-4abc-bae4-9fb85a63f756)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31044ab0-6ad8-47ac-8c48-96e2da504825)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3f3bebf2-8ed6-4408-93a2-2f42bbc2cceb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c89e4b34-b91b-45ca-bb2c-c107910c1be6)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 7d99f13c-5158-4a22-ad1d-f720e5bcd34c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d14edcac-a901-4981-8cee-0944fb84c540)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 fea0838b-afcc-46ce-ba85-f9a11ab66690)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 844757b6-4d51-45a0-8ca5-0a0909b1408e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 954a2239-e02e-48ab-94ec-15f01b785c00)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0c9358dc-0cb7-4d92-81d1-74d36cdd425f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b49d31e7-cfe2-45b8-a5bd-77daab59c39c)(label(>))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 695560cd-a879-46d9-929d-bde905203a4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff808c45-a715-41d7-80a9-238a8554c08a)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 42a1ae3f-0c26-464f-8feb-4f9319cc2ead)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 23abf016-63da-418d-8bbf-90a2fa94ad04)(content(Whitespace\" \
                 \"))))(Tile((id \
                 940fe599-36a4-47ac-b696-7ee4f8e62e75)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4fb1bef3-b77e-4345-897e-2bed6cdd15eb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 33abfdef-23f6-42ac-9e01-898de19a5046)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d3ce4e3c-8aa5-40d2-b0c4-a8cf42e44b33)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8a81f77e-9031-4b01-9dc7-2bde98795e3e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8d667807-df41-486e-92f8-0421082b19a7)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 2a87c213-209a-4f1f-b61c-fd273b9845bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6c4fd245-c6f5-4fac-a52c-197d207ff75a)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7bdcced7-1a5a-4559-863b-d97a01af5d9c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 83ed5ac2-b20e-4a3e-a15d-c4318d694088)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 32319825-7fe5-4790-8f91-70e058c3baf1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d85b5eef-6c67-45c0-b3ad-52ad146b04a4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 41df7417-3b5b-496b-bdc5-6ddccbfbc16e)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 dc817642-f289-4325-9be3-4ba488e7eb66)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 14c0c7d5-84ce-4192-b980-0d1a9ea97126)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 23cc172e-6a6b-4bc5-8299-0d0de3a7dc75)(content(Comment\"# \
                 Tuples #\"))))(Secondary((id \
                 9b474c38-885c-4acb-b7e2-c5d73af5429f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0dff7aa8-5531-4580-a91d-18ed0477919a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7432180c-a5f1-4ffc-88cf-56d761165dcc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2f38510a-7abc-4856-8766-6d6e2088f81e)(label(tuples))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7e408af8-8c06-4b93-9316-946a8487d2dc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a210d0f4-2c48-4417-9b8b-5eeac7ac4f79)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 827c9cb0-d7cf-4ada-a29c-b9f259fa22ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fab70612-11af-4d8f-be33-3121195d04e8)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 886d9305-a937-4faa-9000-e6c78a371b03)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 cdef5a1b-500b-4845-9150-857b04558dcd)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 38a55725-d68a-4048-a5a2-2d5874f92594)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5e54cc8-29b1-40bc-ba60-5f6c06336a2b)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ca88853b-f364-40d9-b7f6-3aa88483aa1f)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b3b72f6e-fd38-49ee-8d8a-94bedc29df53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6d8a808-18fc-48e2-8a4b-09e9d05e75fa)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f3c50eab-32af-444d-b78a-251f0d346cac)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ace4b716-74d5-415d-8c6e-32256d4b4540)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 72c64f9f-5d38-4ee3-af75-37e2be624979)(content(Whitespace\" \
                 \"))))(Tile((id \
                 447e7579-04b8-4bb6-9ed5-2bd48668565c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children())))))))))))))(Secondary((id \
                 8ae93543-a043-45d8-abaa-e995d34be017)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 193294d6-62fc-4604-8418-067d672e8b36)(content(Whitespace\" \
                 \"))))(Tile((id \
                 653102b0-4205-4487-99eb-359275435d1f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5d5e5b30-9707-4619-b1df-48916ddf19bc)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 265b69ee-69d9-4960-89a3-3a4c37912b31)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7f3152c1-72f8-42fd-bb79-058861e6ede5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f446c87-77d4-43ee-a283-73e3cedb3849)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 39c33b1a-f6f8-4f34-b776-34ae8daf3867)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 2fa559ab-70cc-42c5-8bb0-405276132f6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b8825f5-d21e-421c-8156-586cb229c5f8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9dbeba6c-c232-4642-b084-5e3a4fb0dc60)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 67210865-2a73-4e7f-81f4-e36700d27338)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 07141660-b902-4549-b472-bb064977bc31)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8864316e-05be-4e37-b175-5e80e81565fc)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 e4950b1f-4a6d-4f63-adf5-2e225b55ff74)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c74fcd75-2591-4d49-9043-ad5dde32b837)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0a568440-61d4-4451-8900-c5c776ae4435)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9995bfdb-e086-4bb4-9bfe-e8318497fa4e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfc437fd-9518-4b61-9e13-164f7a194e50)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 fa9430b7-ae3b-4fb6-978e-8df6e7dea214)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7721ee7b-420a-4dfc-90ca-34b4a43e98e3)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 25b111bc-3a84-44eb-83db-6cdb4ea88828)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a833011d-65d2-49a8-891a-700fa86f9058)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fff08681-af61-4231-ad14-68064a5b3960)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 7437ca20-b93a-44dd-9f35-3c472b1fee85)(content(Whitespace\" \
                 \"))))(Tile((id \
                 120e5eec-469a-42fa-b6bd-80db8c4ec00f)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 0914d5c5-6b9b-4faf-ae5e-10a285fc4dc8)(label(c))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a89e824e-297e-4871-a519-8592d4c574eb)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 5e1545e5-030d-4f9f-af5e-06b1f70276ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7bbb3dac-80e7-473d-95a6-8ce007b625ae)(label(d))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children())))))))))))))(Secondary((id \
                 baef9c31-357c-4ef5-8654-3b944f78e407)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 533ddbfa-9917-4a9e-96ce-78861ef1f4c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2be19f8f-adf1-481a-b4e9-e3e50828f679)(label(tuples))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 80159bf5-b3ca-442d-8531-27bba0c2e58f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2338c7d9-60a3-444e-b59e-3202669eefef)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2d87805c-cfd2-4f52-9059-08816c92d6d0)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 37f2ac43-09c4-4745-9af2-bfcc87d04a57)(content(Comment\"# \
                 Functions #\"))))(Secondary((id \
                 303dbefa-836f-4a84-8ccb-e934ad324f08)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5a85d259-e773-469c-a409-71b3f39ac5a4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b5310df0-36c6-4209-9658-7a29cae38e7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfe882d6-109a-4baf-b7e7-1fd6d59fdc67)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9bb302cc-329a-4258-9d04-f55457271e54)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3470bd70-054c-4d5a-befd-b231749be948)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 20ca3839-9690-4102-abea-242f20f1b21d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 930da3c3-e45e-43a8-8b5e-3ee1f977bc1a)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d9f0b431-a847-4fe4-8e1e-bc45aab38086)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 80cf4952-eb70-4da6-bbe3-10fc763dc763)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 30b770ec-7684-4c75-9158-6952bd90acd3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92f33204-4ff8-4b2b-8048-df467b415fae)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 23501aec-37f7-4f2e-a701-6ca54cb45c0a)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b6bf9367-ce21-4c02-b87c-72ad2ffc5e4e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f16bc6f4-ca8b-4eaa-b351-208e35eebf83)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 b7493d1d-1178-4a08-a474-884e58d6c039)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de4f48cf-9273-41ac-8b87-fa67705f34d2)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 41480efc-aa69-437d-9d9e-784e583cd76a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c1f9564a-9153-4ccd-b5e4-95dec5d54293)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7af5e354-cbb5-4ce4-bd40-ac1e2651cce3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 7f7042b9-6c29-489b-aa22-09577eeb1abb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1f8d7e77-1793-42c9-a6c3-aa29ecc85a62)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ac3e12fc-49ea-4998-a18e-357cec272d90)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eed6590e-2a21-4229-83fa-868b7425ef41)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 d0c6b5dc-ba56-4482-b3a8-d21d5c4dda12)(label(m))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 447fe9d7-4221-44e6-9b29-73de3dc9e028)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 a0b064a5-492c-4c84-b362-acd2c366fbcd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1c18420-0cd4-409b-9a3c-abe38a4c045f)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f6724032-b6eb-442e-81c5-ed962b058ed2)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 0267e085-4992-48a2-b9ce-7df018e57608)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b3b0de5f-1f79-495b-9fce-476a2198b8a4)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 1c8ce839-f3ba-49ec-a47e-0a4baa60f92b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 257d708b-8081-4807-9904-0d82e896e48d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c7b88894-67b1-47e5-befd-59d4a22e5f3c)(label(m))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f249a55d-72cd-406e-80b7-c7d27357def9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 303b9482-1d77-46bd-98c1-55a410c9d938)(label(*))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 4))(sort \
                 Exp))((shape(Concave 4))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3047d938-2f21-4d0b-9d51-a90342373547)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f47d7fe-324b-488f-97b5-7960748a75f1)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e8772f31-49ad-4f7d-98a8-31397e1dc13a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56179ce5-e7f2-4192-be76-b9480c5da325)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 81adc641-8a46-48c8-acd1-0c10d46f01a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 298c3029-70db-4cc5-8bf8-e336dbfc8ac2)(label(b))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 fb9f244e-f9ab-41d4-85de-e6887388208c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8821260c-19b2-41c2-9918-38bfcf8f9e17)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b3f1ad47-a958-4292-91f5-24f7b01496b8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ffb83254-31eb-40a6-a3c5-7a44d028ca40)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f5def489-84b4-42e1-841f-903900fc392a)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 2e0aa981-6d1d-478d-ba07-9a5f7b70ed94)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 de5de922-309a-47b4-9fe4-426b860dbb4c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 30771201-1847-465f-9295-a3de7534f886)(content(Comment\"# \
                 Recursive Functions (arrow type annotation required) \
                 #\"))))(Secondary((id \
                 ac2a1147-ab19-4cc7-86f4-635b5e2447f2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9c2d306b-78a3-4456-a303-6778850c150a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 36057a3e-0d54-47ad-9b7d-fc50ac1005f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c6bfc6b-bb3d-4d31-ab1b-7e1d3b78f91c)(label(double_recursively))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 5788775d-0c8b-4612-a3f3-1b400c12e731)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc413a2c-0efe-4789-b7b6-d3353c53b66e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5cc7fa9a-7701-4f21-80d7-b98cf846e92d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f1f800a9-f48c-4504-be34-9df618ba6fce)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 39f73786-71ff-4be4-a8c4-53919e1ea486)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a650c728-c7d1-4eec-ac26-50531d6da2c4)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ed844329-c101-4256-ad81-9dcec93a1877)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c076b11-aa2c-438f-9c66-95fdbe12cb16)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 03a74084-a678-4499-8f77-9ae4b2128305)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b4e8cf20-005c-4e12-bad6-02f14746a089)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 175015c8-7ecb-41ad-9302-800122168f60)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4270c3fd-1881-4c5e-b6ac-96b6688b0b97)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ea367b9-adc5-4f55-9022-db7fef8c3e43)(label(n))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ced31f59-9e1d-48ac-9dc9-a28f2dbc038f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 fb8d6dec-2319-4d15-a1cd-33bd16f3c5af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b6773701-4fcf-41a5-ae35-040953496ab0)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 956888ea-01dd-44c7-8eab-8e10f2abf9fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7e11c9fd-513e-4eb9-b504-026ff00ded2e)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 98b10318-f1fd-47a4-9cf9-95ea1ebac801)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a86a0e6-69ba-429f-8958-e9e80ca809e6)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 5aeff2a0-f356-4044-9d99-513af2345a89)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97bc3f6b-7732-482b-b739-35841d7f22db)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 30ea313f-6e82-4915-8629-4b3ad2f56d15)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0a0f0c2a-df29-4db2-9537-ac2feb58c178)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b4b1251-8a03-45b6-b247-bf06a3962aec)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5eb737cd-3e59-47ed-888e-d11e40575e89)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 374f7b97-7460-4c9d-83ac-a21bac6a885b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fe83d843-b665-454b-a3f1-1610c40c1447)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c7d17c16-0285-4562-bc95-d33e02da1f53)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 66e4fb1d-780a-4f34-9429-1aee274cd3e7)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 336374a3-9e27-4e76-af87-91d4b39a6944)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b424cd0-3ef7-498b-8fc1-0cf8e4a0d673)(label(double_recursively))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 52a7e18e-c53e-494f-aad6-7210fbf4c22d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ca0e127d-9750-4fe4-864f-3ff6f1902dd8)(label(n))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ce654ee7-65bf-4c3d-a7cb-2c6b76d96cf4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 65d01e0d-f411-4b1b-828f-4e8dbd85830c)(label(-))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c0c1027f-c062-4a8e-90ca-549568697941)(content(Whitespace\" \
                 \"))))(Tile((id \
                 421af97d-503c-460c-a09c-d88932bae60d)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 823424b1-4110-4e87-a034-5c2d69aa115a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b42f21dc-2630-4310-83c9-9cce5c4c5324)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 57aab120-2bfa-417d-ac9d-3e3e970a64a1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3fbe3ce2-76db-407e-8aac-e022a1b45635)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c88c11df-d4bc-4dda-8d53-6424e348d073)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8f182e2b-a3c1-4ae3-8a12-9d8c8cf48f12)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3a5b3ad4-6aaf-43c1-bda3-f58ea5bb4b0a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0aac0bf4-3397-4a57-b293-db02d06149d3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f6f96372-bc06-4d51-a8bb-c00d957c3efd)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 fe3a862e-9b21-4381-9c04-c4d2ccf8833f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 14fc5c7e-a719-438b-9a80-47b8ab630d54)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 34b94d7f-3595-4874-aedf-3c91d8dbc5f7)(content(Comment\"# \
                 Lists #\"))))(Secondary((id \
                 7303fa18-8537-4ed7-a13f-7e2427f37849)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e19181de-6138-4bf2-9d49-84cad3e9d4d0)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 764e788e-8333-4485-be83-a45aa31eda96)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1d7f101-7749-4634-be47-968557d27580)(label(empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 37751011-072f-4bd8-ba92-f48071f991a5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88360d96-f8ee-4a95-8a4d-7a3ffb523c55)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 65d98f70-71e4-4465-abd4-1ef818a0a0d7)(content(Whitespace\" \
                 \"))))(Tile((id 89c02c0c-9382-4109-8178-ef1bd1107832)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 10bc6def-e153-465e-9b05-e70831326243)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5a19b55c-c12e-45d1-a362-11f961ae460e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 bb29b04c-3993-486c-aa55-251001eb1982)(content(Whitespace\" \
                 \"))))(Tile((id \
                 effc2b34-3703-47c1-8d8e-ec880f987312)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 11bffa0e-1a3e-42ab-a002-c53cd8da55a3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 30ebe70c-d35d-4978-a064-2b12a5130436)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 eb1e17a7-ca2e-4d78-b1ea-d39dd24cd958)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 909eeab1-ca8e-45e2-81ea-cc6e6604a114)(content(Whitespace\" \
                 \"))))(Tile((id \
                 acebdca9-3ecc-49c6-9d36-d49498dbbcae)(label(non_empty_list))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 837fa3ad-88ec-451c-9942-e96fbc9f5a19)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c49332ec-4ba4-4a62-a316-0a15c5f44b95)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 33e80b69-7bf4-4759-9fca-f9c8bcdcaeaa)(content(Whitespace\" \
                 \"))))(Tile((id 760838cc-7f70-4a8e-99dc-95d3f090005b)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 814f4931-fe52-4581-a99a-98248c1002f1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 a0810d6c-3437-4c0f-b274-cbbc9270b57f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f28212e1-4250-4443-9265-4dc4c53250f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d060bf97-e46b-43df-9239-fb6ab5ddecb7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b93b039e-48fd-487a-92f0-e4ba7d3a8850)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 1842eefa-7cad-4e3f-867d-0fe437a8bb33)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 f5fdda6e-5976-4d63-a688-f2eec518d97c)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 4e021521-5eb4-4c77-b74a-f965ae72656c)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 68e8ae51-aa4e-4c7c-9134-3cb8ac2a7801)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 30e2cdf6-4f91-4f1b-a72e-36d1f33d0b6d)(label([]))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c4cc4358-51a9-42ed-b409-4fcc1d68a23d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5fa7bae2-cd08-499d-9045-64059f10c873)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 22ee89fb-61d7-4d0b-bced-2908816d28a2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 792b1ec8-1e40-4af7-b45a-44817f9e694a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98b32c6d-9b0e-470b-993d-0439c762a571)(label(list_literals))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 f7e61ce2-918e-44d1-9dc6-d9b12e46fc99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5219b5e4-f3d2-418d-b224-823bcf8dc852)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f1b82c77-31c4-4769-8d35-76512e0f8bbc)(content(Whitespace\" \
                 \"))))(Tile((id 2db7e604-9772-4fcc-baab-2cda219ed3fc)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 38a25631-e54b-4d8d-83c4-4c59b4fdbb21)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 3e2b08c2-0b30-4044-a3cb-6dcc202d16f9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b5215472-fbc7-47fa-81f1-1294a86060ef)(content(Whitespace\" \
                 \"))))(Tile((id 360593f7-0a6f-4664-a06a-8f19586ea462)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 007da74e-5a96-4dfd-8252-96b4d2006e0e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d3e83068-a2be-41d0-a2f9-a4d900ea781a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 9348b638-d589-4a90-b7cf-0d9af10b21d2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d1782a92-141d-4e54-8650-4986ad70daba)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ccda0709-76f5-4784-816b-2574809f37e0)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fb064b94-6999-450d-83d3-e307f6eee1c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 67e73c44-d62d-4cf8-9256-4cdc08e8afc3)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 17ca39e2-5534-4020-8b07-2f83612a6b88)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 53010620-a111-468c-93de-0a040f7361af)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0f7b7e43-b194-4ee7-96ee-3ba03ef65abb)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a7800a90-42de-4424-b4e6-b3145d2e99ae)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dc93bdba-aab3-424c-966f-771bb09b4f50)(label(length))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b6c0ea15-fd1c-4839-9857-4362fbbaed6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 887d277e-c7e3-4617-a6d5-d36feb96d706)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 97a234b8-9b1c-4305-b914-b9d4161bb914)(content(Whitespace\" \
                 \"))))(Tile((id b491db01-bf56-4a9c-98a1-baf0ca71892a)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 fe5cfbba-ddc4-4e2c-9bfa-6f2372494879)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 f7942f74-b98a-4dab-9136-70bb5a992422)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09621832-c956-46d7-878d-a70ea8987908)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 584e9bd2-a2a9-44ee-8c00-a552e8a13fc4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6c1d0024-d863-4cde-acc6-eda793f1f0a8)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e3a0aad3-6779-4440-b002-7fb9a8e35e92)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 be225dcd-5070-446c-89c4-a377baf6c82e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 febd8e9e-fa47-4439-9c6b-babebc9c14a6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 672ba047-7a9c-4ade-8f6f-677ad3fe942a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3054c72d-66c5-4a00-b136-551afc24396a)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 9f231ffb-8f72-45b7-ba7e-1b31136148d8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 09c2ca09-8fc1-47b6-9061-8e0c9dea0e68)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 de1ec564-4f9c-46ed-b313-6a6fdc6bcc83)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 00bfd425-af8e-4d4d-bbd8-bef881d5a39d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 382ab72e-27ac-4e2e-a8be-36579960997f)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d74dee63-56e9-4078-bb59-6cdec0359337)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9b11e9f1-fa85-480e-baf4-f5082d666fce)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 29674e12-ce7c-427f-8260-813a7b0a9a08)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ac49e1bc-4cd3-460a-a319-777d7e0c8d09)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 fef081e3-901b-4ff9-bf3f-ad9db8063647)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7dca884c-4657-4e46-984f-21e88b79c553)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e1085245-9a30-402c-82fb-c948bc7dbde9)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ed2603e6-fdbd-43fd-b3f1-d3e749b49851)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c9181f2d-1188-44a6-afca-50765d15800b)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f9ed5f67-9815-4488-90a4-35a58fe6cc53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8a7f7a7-e8b4-4e96-ad5d-11d9573e6f16)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7ed19f00-fd7a-490d-8692-b9b4e5e9537c)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 3e03b104-2536-4553-8b0a-8d02d45c4568)(label(tl))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2d508b16-c706-4074-a185-ffdee28e6abd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7c42346d-0876-4650-a1d7-df5c7ed3b60a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c1f987e-07d3-46a5-bfdc-4bea49dd9773)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3e6d65b9-7f10-4d67-94f3-d2c941f3a001)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b73c6df-1029-4ed2-b266-1491ebee3044)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f84cc57e-7a04-41cb-a87e-d2247b24469a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f7edc571-c3cd-481a-a111-11625a1a9c80)(label(length))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 65c8dd79-7c8e-4d9e-b7a0-6b9992985840)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4b8e4be4-1e5d-4409-8457-88fd7f4bee9f)(label(tl))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3b74800f-78f9-403e-b53a-e9e14f38201c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9ca05a85-0c4d-455f-9384-48759fca1b8f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9b72db79-51f3-47d3-bd73-c33f23f3dce5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 12e40e4a-4822-4b01-b61f-ca1330eb80cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 11b52631-7128-44dd-82e6-804eda661e66)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 18edaab7-0564-448c-8f20-30a7e9821fd4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bb55cc6d-1a0f-4a72-af85-819d8782276f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6061bdbb-bf40-4334-9ad1-88438304c780)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a6ec3479-fb5c-47d8-992a-86117a41862a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 55b5a5f3-6c36-4caa-bf89-8448610f1064)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 c317cecf-f2fa-4d4f-a277-5e6e7d93283f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 91699516-50ad-4050-9ffc-e5246ed68282)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3f38ad56-2cb8-49cd-90f4-beaf0a107fc6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1beb0b9e-030c-4296-b7d3-8aefe9326de9)(label(has_at_least_two_elements))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 2d3c79bc-5979-484f-8105-5596f2f160fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 236dfb67-06ce-4ce5-b39c-f6d397365ee3)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6021cbc0-2e19-4f0c-8094-477f7ac235fa)(content(Whitespace\" \
                 \"))))(Tile((id 3c2e8518-6e04-4205-a183-7441675fd157)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 b9129361-abda-4314-b283-bf31cb695fa0)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 337afb06-5ec3-49d0-9e8b-9d7778500901)(content(Whitespace\" \
                 \"))))(Tile((id \
                 03b7018f-4718-44dc-845e-8a65c404ea86)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 935848bd-aec4-4c72-ac75-9803432d85ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff094c7f-25b8-4462-baf5-9ffa65c9040e)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 133c0f86-6fa2-4970-ac8b-43b3dcf13dd9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2d99c805-0819-4aff-9581-bcbb61a95976)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2f418759-a036-4d35-83e0-ef0d426fb18b)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 258c47e2-db5e-49b9-bfa9-9659c4bdeda4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 89a42f17-70f7-44fd-ab46-da34b9c92d22)(label(xs))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e60276d1-17b0-4c1d-90e6-f25ccd3c42dc)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b3cb5d12-fac9-4f03-bb3f-85f555034895)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 717bc679-a083-4471-873a-b09a08d669bc)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 05d3d635-c1c7-44f1-8066-8512aa70510f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5cddf75c-2b8a-47db-9da4-fe45b802440d)(label(xs))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3ff11485-c6e7-4b60-b696-cfae32a9e129)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b2a8bb8b-4617-4c2e-81b0-3912810c81e4)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 814cf2c1-a081-45eb-8c34-3ffa04048468)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd316234-fcee-4aa8-97a1-418a27c5fa2d)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 8485aecc-9c91-4277-afaf-6a0338187777)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 633de626-3120-4fc0-be03-c203b364ad6d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30b49bf7-f7f9-4837-a588-15aaaf22f086)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a656a6a3-70b9-48bd-9ae2-42197920ba31)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e900cc8-f796-4d36-ba7c-03742a8932a8)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 e45f7352-4ddd-4952-8b80-d55601dd6624)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4cc8dc7-3884-451f-80cd-e8ce63d49fc2)(label(hd))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2b368bdd-541e-4f16-bf4f-e8e1b85264f5)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 df13c77e-2877-437d-b100-6282ec7e79b9)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 70af199f-1f9b-45d1-bce7-de4fb88e57e3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1a021837-8cdd-4847-b528-d549a766d30e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 92d35b53-4a75-4a93-8368-bc38e64088bb)(label(false))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 83efde1a-fab2-4baf-be7c-dd6179abce09)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9ff4c42b-3568-46c1-83a6-3012c48b3ded)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ef373fa1-e016-404c-a430-559069f5fb3b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 44d3e298-44c9-475e-9cca-e48ff2b23558)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a8edf86d-7743-4981-9a1c-fd2b7607ec48)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 c5f1bf86-aaa0-4366-ac2a-b75dc64c0672)(label(b))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 118fd192-975b-4d45-9cd4-e999eeee130f)(label(::))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 6))(sort \
                 Pat))((shape(Concave 6))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 8355660e-f455-4b3f-9d9a-bc64d61f7cbc)(label([]))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 d6cb1336-7af3-48ce-a94e-1ff8e19a213b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3625f1dc-10fc-40a4-a817-7f54024c94e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45b2007a-fc59-4163-a47a-417f12301f7f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 59a7d313-46f5-4b78-a0be-ac47902bc36e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c0ce4872-86c2-4b02-adba-a74f5bcce87b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 66b9617e-8e64-4b59-9fe1-906ca4c0d7f7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 23d8b15c-fc4a-469d-af4e-842366bc0eaa)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e880e321-513e-4661-9fed-60305a310332)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 5062e301-7d0b-47b7-8ea7-81d472c23526)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4d818975-eaff-47aa-9d37-65ef76c57cea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0d11255a-00ed-4b33-8908-06db0960b05d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d2cfa0aa-d753-44a7-b4b0-f3862a4c61f1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9df6474d-390e-4054-8a41-95b6bede535a)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 32d37929-4d15-4397-9727-35ea4cc734f8)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 f7d1248d-746a-4e7f-aa6f-a7f6e869023b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0b905512-0be3-4f74-8c6f-718b0176a4e2)(content(Comment\"# \
                 Strings #\"))))(Secondary((id \
                 25921b72-56c1-4397-afb9-d3b8f635cd54)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3027c0d7-aab3-4a79-8684-549176bf1b78)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a9a2506b-d073-47c1-b43d-05ead178764e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56cf4647-874f-4d49-bff4-9a313aee77cb)(label(string_lits))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e608f4a0-c896-4091-b2a3-6dddc3af9501)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c47b4cec-f134-46f8-8973-74d8153c0f8b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24c0fb7c-6e49-47ff-a1a8-572ecac20888)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 4a974657-cc7a-4c4e-bfee-a1f77bd147e0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3806bb22-ee59-4ef5-be6c-ac343d49fb18)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9e1250c7-2aa0-4e60-9c48-7a0137fe2329)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1530f89c-097e-4046-8aa7-450a278bfdef)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 28398dd9-5adf-443a-b025-de2dbe295340)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd8b9329-3039-473e-9a83-3e540807cb3e)(label(string_equality))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1e714b71-05a2-4eb7-86c6-3dc67c0dc901)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f7b8444f-5647-4f86-ad13-405884a0eca9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b0910ad-fc38-415a-9a5e-440623247d14)(label(string_lits))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dfa0da8a-8ea8-4493-a0bc-ea2818fcbd1b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3780e3f9-3aeb-4e4f-a104-1020f2cf4b7a)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e0d0703d-47c5-4799-a62c-6c33d5e38e3c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 97240b10-1202-415a-8247-022885cf424d)(label(\"\\\"Hello, \
                 world!\\\"\"))(mold((out Exp)(in_())(nibs(((shape \
                 Convex)(sort Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e992bf5c-5298-4473-8e9d-8f509877df5f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9f8352fd-73e2-4cc4-9db7-a93fe1d21668)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 70eebc52-af3c-4dfe-a6e7-28ac843a1913)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 37feb95c-532c-40da-b549-6ef8d61f8bdb)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a0d33d65-6fb9-4f4e-828b-6ae48202e479)(content(Comment\"# \
                 Non-empty holes are the red dotted boxes around errors \
                 #\"))))(Secondary((id \
                 f0009691-ea0d-4657-9c33-caeb211f0b7b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7507a5ab-64c4-4953-b9f7-d01539c26eed)(content(Comment\"# (you \
                 can still run programs with non-empty holes) \
                 #\"))))(Secondary((id \
                 30b508b4-189f-43c5-90b8-c81a609be133)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9a1c6e9f-b689-4b4c-a59c-ceff653f5769)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 310ce64e-e4ae-4c6a-ac44-787dad5b1d02)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6069c29e-5ee7-47b5-9391-98cd4b63375c)(label(non_empty_hole))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3b141e21-65d3-4d6a-80c3-46c254e703c4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 050a1d41-c106-4a68-aec6-f587470928af)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cffc32fb-a495-4f21-aca6-8c8c1e116277)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3d09ab02-a577-4fbb-b212-b0996606bca2)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 12521db4-2ddd-4e44-9e5e-03d41df95808)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 472ae250-27dc-4c34-966c-75a9ee9d2a8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 19fc1377-e13e-4f19-9fdf-9b5446b217c2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4d3a39f1-29e5-492a-b133-05cd401637cd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bb00ce5f-8dfd-48a5-aad6-b4b9709fe2f9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9da81a5b-d084-4c69-9cf6-963b76e36460)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b4ce37fd-2c4a-4656-8afa-dcd3c6d66978)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0ce9bae7-1be6-4604-8c12-6cf6e78c4545)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 be1a2f76-9f56-4a79-8f8c-0a66d47cc35e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0391784-1fab-4c4b-bf30-192bb8183b1f)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 20747a76-02fc-4663-a225-cfc4d65c651a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4e93a869-23eb-4a8b-945f-2640d1b0ecce)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 db6b1fe4-2541-4eb9-95ba-5f19b5c7c89c)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "# Hazel Language Quick Reference #\n\n\
                 # Empty holes stand for missing expressions, patterns, or \
                 types #\n\
                 let empty_hole =    in\n\n\
                 # Integers #\n\
                 let int_lits : Int = 1 in\n\
                 let negation = -1 in\n\
                 let arithmetic = 1*2 + 8/4 in\n\
                 let int_comparison = (10 == 10, 1 < 2, 2 <= 3, 3 > 2, 2 >= 1) \
                 in\n\n\
                 # Floating Point Numbers #\n\
                 let float_lits : Float = 1.5 in\n\
                 let float_artih = 1. *. 2. +. 8. /. 4. in\n\
                 let float_comparison = (10. ==. 10., 1. <. 2., 2. <=. 3., 3. \
                 >. 2., 2. >=. 1.) in\n\n\
                 # Booleans #\n\
                 let booleans : (Bool, Bool) = (true, false) in\n\
                 let conditionals =\n\
                 let (x, y) = (2 + 2, 3 + 3) in\n\
                 if y > x then 1    \n\
                 else 2    \n\
                 in\n\n\
                 # Tuples #\n\
                 let tuples : (Int, Bool, (Bool, Int)) = (1, true, (false, 3)) \
                 in\n\
                 let (a, b, (c, d)) = tuples in\n\n\
                 # Functions #\n\
                 let y : (Int, Int, Int) -> Int =\n\
                 fun (m, x, b) -> m * x + b    \n\
                 in\n\n\
                 # Recursive Functions (arrow type annotation required) #\n\
                 let double_recursively : Int -> Int =\n\
                 fun n ->\n\
                 if n == 0 then 0    \n\
                 else double_recursively(n - 1) + 2    \n\
                 in\n\n\
                 # Lists #\n\
                 let empty_list : [Int] = [] in\n\
                 let non_empty_list : [Int] = 1::2::3::[] in\n\
                 let list_literals : [Int] = [1, 2, 3] in\n\
                 let length : [Int] -> Int =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => 0\n\
                 | hd::tl => 1 + length(tl)    \n\
                 end    \n\
                 in\n\
                 let has_at_least_two_elements : [Int] -> Bool =\n\
                 fun xs ->\n\
                 case xs\n\
                 | [] => false\n\
                 | hd::[] => false\n\
                 | a::b::[] => true    \n\
                 end    \n\
                 in\n\n\
                 # Strings #\n\
                 let string_lits = \"Hello, world!\" in \n\
                 let string_equality = string_lits $== \"Hello, world!\" in \n\n\
                 # Non-empty holes are the red dotted boxes around errors #\n\
                 # (you can still run programs with non-empty holes) #\n\
                 let non_empty_hole : Int = true in \n\n\
                 2 + 2\n";
            } );
          ( "Types & errors",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 7bbf4bbd-5592-44d0-8c1a-2776327b105d)(content(Comment\"#Types \
                 and type error examples#\"))))(Secondary((id \
                 5394aeef-39dd-49a5-858b-316e4af5decf)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 4b4f402e-96ad-478a-a0f5-626f32aa0464)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 840c882c-b940-4974-8907-bf7d78383bce)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9be573a8-82f3-4b75-840a-256c89e87c2d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 293ff584-a374-43c9-9f11-616504aceca2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0652234c-dd35-4fb5-89a2-aa91a19b9b48)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 00b4e988-64a6-464a-b23e-10771a01f428)(content(Whitespace\" \
                 \"))))(Tile((id \
                 12719b1f-5790-48e9-81dc-a61ff6c2f76b)(label(unbound))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c049f710-3edc-4e6f-90cf-289b50a5563b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cf340790-0a95-49fe-b9f1-cd9b33bb5f9c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2affb401-8ee8-4d1c-8c18-4c3076fdac91)(content(Comment \
                 #err#))))(Secondary((id \
                 8ec740b3-921c-49dd-a781-a9b696cba816)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 573c558b-3b54-4318-9860-b946e896028a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0f9f4e0f-092b-4472-af06-b5895e9aca4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09968899-7da2-4a2c-b13a-e06dd3941c10)(label(Undefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 17a8913c-c0ad-4ddb-9303-d9407d10c4aa)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6ebd2bc6-8bee-4737-9b1e-79342eba7979)(content(Whitespace\" \
                 \"))))(Tile((id \
                 797cf5b6-e280-4c43-9eb3-91d428d05bc5)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7ac1b8a4-66ff-4801-8fbb-408b7b923b5e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b23d1e4e-50f4-4b09-a1ac-f918299c0df1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f73b5d5f-8c68-431a-87d2-61e0e6e9d699)(content(Comment\"# 2x \
                 err#\"))))(Secondary((id \
                 106b8b4d-6025-477e-9018-62b7b71aac6d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9e53474f-386c-4754-a7d8-2e6a97dfce5a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ac6ce019-c4f9-4c2d-9497-d66fbd9420fa)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9508dc44-1fb4-4256-ba10-03b1f4d1018a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68e510f4-60f3-4424-8bc2-4b9d4d732455)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 e5ba792a-faa2-4bff-835e-e44932a07622)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3c6b9dab-5b0a-4b90-9bab-229256d389d4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79558711-6e7c-4f8d-a2fe-b555a329e8ba)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 51365adf-2b52-4b74-a69f-a6e55d9a7f50)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 edaa2b1b-c36e-41b1-91fe-466cca107be6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c10f524c-e197-43a2-b088-216bd64a7fc5)(content(Comment \
                 #err#))))(Secondary((id \
                 29f2ebbf-9c4c-4cd7-a05e-ed2733dd8311)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9a921b97-d4d3-4002-93e6-0c656ba18d29)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 107b6f0c-ab6c-4a8f-9de1-02682506c0cc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4f577624-a7fd-42d4-bf33-335d775cd35b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 a40c6ad6-c401-478c-840d-837112ece76a)(shape \
                 Convex)))(Secondary((id \
                 6090d5e8-e8d2-4260-b158-0c8cda111bb2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d1e5d32b-a51d-41bd-8741-b7d9c2e39bce)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d89892c4-73a4-459b-a861-f7daa6b6ea83)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 220e686b-589f-4582-b9e4-38844e077496)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93baa2cf-4075-41c0-acdd-64de12ba62dd)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 038bc831-d4d3-45df-bdd8-78e9dcd3cb52)(content(Whitespace\" \
                 \"))))(Tile((id \
                 185d4343-ea1f-4b2d-baa8-0e6318f2346f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 adb11bee-ecf0-441d-a121-3448a30c47aa)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 115b056f-81c7-4c88-92ba-d0a2de4083a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 468d5687-d766-4d20-895a-54a0c04411ce)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 39bd78ff-4902-4225-9dbd-2eb790588d09)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ff3aaf9a-5118-4afa-8d74-7c91bab9467a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cfc080b9-6453-4dce-a94f-bebceb529a5a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0461209e-deb5-4288-8022-0ac3ec620cf5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a4779ea6-127a-4e99-b28e-c55d280eaae8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 efc5f00e-c4ba-4f0d-a537-213fe642a456)(content(Comment \
                 #err#))))(Secondary((id \
                 cfba859e-cd01-4974-92fc-41139f645853)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 afe36ebc-8f86-442a-a006-e785c3c49e80)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9cc3e98c-92d8-42eb-8ab0-1820d0f987e5)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 61a435af-fa71-4c0f-9275-3c665a92a1fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c00154d-7cb5-46fe-b552-9d8159124560)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1160aebe-c53a-4696-a2e4-25ccb1117985)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ac2eabd3-9d29-431a-9e57-a68935dbc3cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b51647bd-bacf-4f62-babb-29fb030fddac)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f8e1fc66-9166-4f89-9027-1fed9d0852eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f23c35a3-d93a-40e9-b485-4c5c8ee37d35)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 51a9874a-06fb-4523-aec8-25863f6acd50)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 423e948a-6512-490f-8bb7-ad1ecb0081f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 32754997-1fa5-46ed-803c-713d426ecf26)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dc3a1053-0cbe-458f-b1df-f000f122a2a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ab11f5dd-299e-4b6e-89e0-aaa672b88dce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b937fb8-599a-496d-b897-747eb7cbe19a)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4d7d2cda-d049-4c26-9dcb-15b5765fd396)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 24f8da6a-c4fb-44d1-882b-7a49d9051fd6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 13009ff7-459e-4bea-943e-0b3746ef9b43)(content(Comment \
                 #err#))))(Secondary((id \
                 d7822624-e1ea-4a64-ac76-f7e3d6482cf8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 599cb867-4c11-4636-b1ef-a18f3a810e02)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9f975986-b212-47a3-b5d7-f0acd8e99aa3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ef59d5d-e4ed-40f0-a4de-2aaf3fca3907)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f8d36666-1a91-408d-842a-9a29aac3560a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 7bc8e8bc-2b7b-49c3-a9d9-03ac16ce4783)(shape \
                 Convex)))(Secondary((id \
                 aac4de71-5b04-4d8b-81ff-1938fe1d7f18)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fa6893a2-7fdd-453a-a50a-fae628e331da)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c22f511b-1ff9-4fb8-9822-224c83480d7e)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ac9c4ad1-9f71-415d-beb8-e3bc3abe67df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1cade910-a94f-4e89-879c-aa4f74f23c78)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3e4e215f-ae0a-4e65-9c03-e77734f6105d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 866c3457-bc19-4dc0-8ccc-11ef020646b3)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 852a6aab-b556-421a-a226-52fe85a9c589)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b6b2c0d8-b7eb-4449-b300-88cdaabdf991)(content(Whitespace\" \
                 \"))))(Tile((id \
                 09b58125-e78d-47b2-8e9f-79ab86a0895e)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d457d648-9a9c-4443-a38f-060e0ce49a24)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 980b3f50-f3d3-4d2f-91df-8f439f487524)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40184810-d0a3-4f36-a33a-ad2f0d4c663f)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c6192707-e7ef-4075-ad86-9d36e1c58509)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3537a208-58a9-4aed-8532-b039a340a6cd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e9e953fb-f6f7-48c2-abd6-befb46145a36)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b726b51a-7b35-42bc-9625-a08ce49df2eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26028545-149a-473d-ae3b-1f3ab6a5227e)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6b89bcc4-ea26-4058-875c-2014765002a7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ae30d1a1-f42e-46f2-8270-f419213a3956)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79832f9a-db6e-4ee7-a1da-891ac35465f6)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 d2fb8762-2617-469e-b02e-eb1a147bb0b9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8bcc0973-4360-4c86-9bd0-07fa202c7c7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8386931d-2ce4-46ed-9b01-69281b91fa3a)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5f25d918-b1c8-40f7-88fc-ccd5f7eb5068)(content(Whitespace\" \
                 \"))))(Tile((id \
                 05f5571f-8620-484b-a455-d1670ec58a37)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d37f5a2f-7069-43a0-84b9-9682f761bf5d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c328c40e-d179-4edd-9cbe-e9c8f46fff4e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ed62c11d-ef7b-4105-8245-dfc710436891)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dc5d41b3-d8b7-40a4-bfd5-58f3a930d0ac)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 93a52308-6c20-4305-95d1-ce50998ae183)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d917adc-74a7-4bf0-b669-ce3252357332)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7762352e-fdb3-40cb-b0a2-c2f483ecdcdf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 84a0111a-3dc4-47d2-869f-6871556058a0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 faf7d715-a5f4-42b7-8427-4e6e3ffa1ccd)(content(Comment \
                 #err#))))(Secondary((id \
                 6c7076c8-3a1f-47ac-9f52-741614d2118c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2dc6be3a-ebd1-4f2d-bd5f-cf03490c2114)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 32222bb6-1caf-498f-a63c-48aa001746b9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 02a27a86-9afd-40fb-872e-00242b8cd496)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0fd711a0-c50a-46b2-bc55-afa353deeb8b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 de77405c-9993-4925-8d90-487d8686d52e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd981c58-b2be-496f-b441-2c2fe4253ae3)(label(Fake))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f55bdd64-daea-4279-af1c-623f3f1f4c0c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4cbf028a-76ed-47ea-b45b-06f8db0d6790)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11ce1a79-d926-4583-a1f1-977690b04263)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cbff56e4-7190-4fa8-a6da-1c08d77b0e9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6edfad04-ae92-4cbb-b167-ecf1d36ea303)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1b3cfd45-5df1-4851-8a47-a352a2cbf314)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 18a5f5e3-0136-4ff4-bb14-51eae4dd8289)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b185783d-f116-4871-be50-fddacba3fc9f)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 84bc1769-036e-4547-b96c-a7653259c9cd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ca99425f-dc4b-4a72-9fe8-b9efd412369e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b781899-544d-4e69-a8a1-11e4aa9897fa)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 41831a12-c636-4a48-820a-73238b9f8ed7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c0457577-f5a1-46f0-af04-1ae919478010)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 61ff86a6-a262-4b10-9df3-692ba0750f5e)(content(Comment \
                 #err#))))(Secondary((id \
                 12c45c83-12f2-40d3-8cda-8460b20e7486)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ab7acf86-9b50-493e-84e6-0649584d9e5a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d4a05834-b475-4fd9-b3d1-baec2288c702)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a9b7957c-c309-454e-bacb-703e71e94140)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 afeda7d9-749f-42d7-9766-93cf083261ea)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 234b84e6-a0f4-4261-b5a8-c0a59122f258)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6a9567b7-9a58-41f6-93af-2a04e7fd1979)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ef75a280-7f15-4573-9804-6888ce2de492)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f49d0db9-ef96-4bd8-9e1a-a0f67705a7e5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8474e13-1093-4281-84e3-afee29efdf65)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ec8d53a8-572f-40a1-8649-04a5ed9ebb8b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f34c550d-eaee-4d9c-9df6-73fe42bd01f6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3d8aabd0-f0f6-42df-b29d-628c1c1f3e19)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8435bb8b-49dd-483d-a279-b459804efd43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3b7d837a-8c1b-46ac-90db-aa96887f43d9)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 51decc95-92b4-44f5-b653-0da3e5b154f2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 78f85647-a692-41b5-afeb-5341880a8d30)(content(Whitespace\" \
                 \"))))(Tile((id \
                 76089fa7-4950-4b60-a50f-5e876480a387)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8ccae157-db71-40dc-9c1b-f44a347003d9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 feefab44-544b-4bc0-ab02-fa3cb899909c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3eccebb7-0601-40dc-85b8-27a29ab29aca)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 675e595a-2a9d-4978-9885-5dd338e9cb84)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 066d8c36-3c43-498a-867c-11c729476508)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5b1d7e9c-dee1-4082-94fd-e58343b6a1f9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95b0f2d8-cbee-481b-a9bd-01a6cf7ea74b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 825bd5fb-cef9-4a38-bbae-f3f5b0230848)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 020e1933-cecd-4092-9137-1d45b62b34e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98636a1c-15a8-4175-a910-7770fea50a43)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4e48b960-110d-422b-933e-b3965c2077e9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1e84222c-3173-4fd1-8236-1c6574e72ab5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4faada93-a3c7-45bf-aac8-bc669e9d5a29)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bd3d5005-d036-477d-bbdb-1c50dc926ae9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6bb0b604-9b2f-4292-84c3-b9ca16aa3c24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07e87c37-cfed-4759-9e2a-265bf4f40baa)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7043747e-8a2c-49a0-8e0c-ece966076428)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 806043df-f8b1-462e-9c97-0e1a96deec16)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d00ca42-33f5-43dc-9835-9603d064ad86)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4abd2b7e-8173-42c4-8faa-f0b1019f739a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b064f07a-27f3-4e7f-a905-18c74b98fcca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a6a01105-6504-4476-8801-b94ae79789cc)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 6d31269f-7a07-45e6-a676-40d08b69a169)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 0ada021e-3b33-4f67-8931-26c0c52931b6)(shape \
                 Convex)))(Secondary((id \
                 26bdd2d6-5ace-4c66-8ac5-4a02498b0120)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 67dfb167-ad9c-4612-b78b-29a4b88ef8e8)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1133b9d9-b0b0-4a53-8c73-57d15959638c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6da1b134-412b-4387-b0ef-d05e7b71b584)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4190a66e-be24-4b37-b133-f6c801215631)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f4a15582-8644-4be0-bd37-bd5c328ab79d)(content(Comment \
                 #err#))))(Secondary((id \
                 617500bc-25df-49d1-b810-cccb646a5627)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 83b25021-1e1c-4ad7-8e3f-ea3ba4155ffd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c1e224ed-bd8a-4fdd-8114-ae3e82ac8335)(content(Whitespace\" \
                 \"))))(Tile((id \
                 964ae469-1a59-448c-a14a-8db1afb83065)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 358cb27a-085b-4f05-afd4-1a3ae938bf83)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1c6ea6db-bf5a-47b7-bfd6-cc018fce09ef)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8e0a35c2-c2f2-45cb-b00e-b941ef99c8b4)(content(Whitespace\" \
                 \"))))(Grout((id d1e27aab-e1b8-4dbe-b52a-80ca81ffd8e3)(shape \
                 Convex)))(Tile((id \
                 8d596400-019f-4398-8a89-885e4d3ac32a)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 efa8e723-6af9-4684-9abb-51cf2ef17646)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60a07125-fc26-416a-a479-78593e4c9289)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 175be3c1-f1b4-4425-bc04-206747f4dc39)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3039388a-33b3-4319-9fe1-2881d71dfc50)(content(Whitespace\" \
                 \"))))(Tile((id \
                 215e9911-ada7-4f56-ac5c-e6129e090435)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4587bc38-241f-43b5-9903-ece267848c90)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1a323e42-65fe-4fdf-939f-7cae7e042c4c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c703265-7815-4fef-b71c-9814b7cedc04)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dba7c647-be53-4707-a026-95dfd65c270f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6359dfae-d8fd-45be-9075-eea47c25cfc9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6b34811-bfc9-4561-83e6-2e29dfb973df)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4d7b96a0-adad-4d6a-bda5-d9c4a17f4901)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 54f36a2f-e6d3-428a-b7a7-d7528270cb3d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 95edae67-bb30-4440-a8d7-387bbe4c52bb)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9b064641-a364-4295-a5d2-18bf24302f52)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 f36e8fba-5d07-48ce-aca3-25fe3663dbb0)(shape \
                 Convex)))(Secondary((id \
                 2943c96b-d94d-4377-89e0-251754247f89)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 5cef59fb-f73b-4467-b732-e8c01ca2d8c4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 28bdccc3-58f2-4e3f-b5b9-b91f655e64d2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ba3a0ce8-2d91-4dbd-b08d-4039d44f0403)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d0b59a6a-ce61-45f0-a6c8-3a101f01db46)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6e284973-5af5-4842-9952-e82b44081ead)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a35b6a98-8b66-4e2a-bd92-4e0e3142498c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e1cb643b-ca9f-42ba-a0fb-9a29cda5c71c)(content(Whitespace\" \
                 \"))))(Tile((id f4d2f2c9-aa6c-469f-a958-1a6eb7d449f6)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 68be359a-73a8-4627-bd90-83f47d37f3aa)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 2c456f2d-48a3-49c8-98a3-9fd124e00ad1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 91ef6988-549a-4d4d-b5b4-dd9583ef95d4)(content(Whitespace\" \
                 \"))))(Tile((id c2394036-c2f7-4e6a-a634-fcfc03481933)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 72361731-aef3-4d98-8d17-f247aa835d90)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9c0b21c6-71c6-4fa0-acd0-b6d0b165e5e8)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1e3d3d39-1d7d-4e4f-8586-c7ca5c999da9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 36a03a65-14aa-4322-9372-0c5f0c9986a5)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 86e44ca5-b94a-464e-9d0e-3aa2e587e2c7)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d1860a76-3d50-4457-a75a-11a5a0b0bff9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b08e907f-a1c3-4b5b-8349-7b10afd058f7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 596c9408-ee19-49fe-91c9-32acad65a9a6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d1bab370-2829-4223-8d8a-2293bff5e390)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c56d58c7-11d1-432d-b394-2823d82d7c21)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 b00107b5-efbc-4c28-a506-6f58b5c5dba1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 86cda280-ff5b-4788-9628-4a2e1489df40)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 af724dfe-f674-4e5e-a569-76ceb539236d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a8351da1-6720-439c-9a1c-c56b013a8c70)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 36c67452-ca07-4145-a16c-9c50edd66476)(content(Whitespace\" \
                 \"))))(Tile((id 9e638a89-edfc-4932-9d7f-5fb6ceaa6866)(label([ \
                 ]))(mold((out Pat)(in_(Pat))(nibs(((shape Convex)(sort \
                 Pat))((shape Convex)(sort Pat))))))(shards(0 \
                 1))(children(((Tile((id \
                 3cfff64c-507c-4530-8555-6b681dda8212)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 b18f4006-8183-4b56-93b4-de1b9c6493a1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 255be73e-6069-4b39-ac1a-7aaa9dc5de5f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2603de3f-131f-4bfa-817c-8713e4532ae5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 67dcce00-cc7c-45eb-8212-d1c1ba31cb49)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e3bbd8d5-d495-4a2f-991c-7a94301b7a61)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3240a9f-0ff7-468d-ad84-557b9ccc42fa)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d0e67998-f4f5-4738-9bcd-1f6ddeac2b39)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 83bc4fb3-73e0-4847-b21c-f203d0e9b37e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b3b7f55-346c-469b-9367-1ec291caa072)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c6d82268-08d5-45ae-a5ed-52e3a0b74096)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 36bc8e5a-ae33-447d-8453-9560784302b0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0bd452c-f938-499c-a9cc-ab882aca40a2)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 cfd5380c-433c-46b8-824c-b5792b842235)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 89babc50-7a22-4a24-a1c8-886e184e67c7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 30c2e099-2d08-4c7d-b2ee-39a2f5b853bd)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 54a7c7ae-01da-4c38-914b-d823ac34e10a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cf875f70-4047-407c-a97e-0732f506e628)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 751f0ddf-8a7b-4725-9740-3208025f087d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 854eaca4-b1df-4b83-8225-d9c557bbbf57)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 7cfc21ab-cfa6-400e-a9f3-a9ebf71890d4)(shape \
                 Convex)))(Secondary((id \
                 e8526ad9-c7a0-4b75-855b-8fb7e0421ce2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e7ff8c1d-3c82-420f-9f22-3ada8cdac015)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 b39a6466-f029-463c-8c75-5d041a4a5458)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 85ad8ed7-c101-4cf4-8ca4-a4897bc1fa5a)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 64fee043-4bfe-41ea-adc8-fe9d5a78730c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd1217d4-ebd8-4b20-a737-24a18e313d8a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 38c06071-cb12-4bcf-83c8-9da866d0eac2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ffe3ef95-f7ad-4f61-8038-fc500443dd01)(content(Whitespace\" \
                 \"))))(Tile((id \
                 21f7a0e9-ae51-4b21-af55-3984e4716c73)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d7850bcf-82b4-4613-8e19-6cdd17609e75)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f14b4a7f-55ee-44ce-b60a-f48d26daab03)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13f402a4-0c46-4c49-90cc-740185911a71)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d16ff589-ff21-42f0-a76a-8fcce1cfafe1)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a0f5c08e-9b5d-4fc8-b9bc-a153f77d0568)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5384ec00-96fa-43b2-9000-79e3c4bd31a7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8be2ef60-ee98-45e5-8ebb-d9992139a425)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4bf48d56-43c4-4fe4-8a29-40dd128465d9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c5dad731-bd4e-4beb-a039-64e1e98ee609)(content(Whitespace\" \
                 \"))))(Tile((id \
                 abcbad7a-cfe3-4211-86ac-983e49a654e6)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ae6415d9-e6c1-4541-a63a-06739bca7871)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d92d910e-2dc5-4417-8796-a07438ef9db8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26313923-9ace-451b-8a6a-8c30087e3aa5)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 45695b29-91e1-416a-8123-63c579cf9f49)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0f07dbc6-dcd6-466a-b89c-1ecaafd48af3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 158d7cf4-733d-4bc4-8fd8-5a22e66cb22d)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a9ed9e98-56e8-4869-a0f4-30b10e7ac9e5)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 bfff01de-a1be-4988-bb30-bcf4d3c7db9c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 123f5d9c-ad39-4c29-9a64-faaa13519763)(content(Comment \
                 #err#))))(Secondary((id \
                 faccd160-af9b-4d15-a227-1b9cc10411a3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b9d724e-77ed-493d-8b8d-746ab000c2e5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 78a27786-5452-4a35-8aab-0d0da9e16147)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 40ee2f45-a518-4cc6-aa02-cc56212698dc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a2e795c3-f27e-460f-a51b-957ce9d7c979)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b778931b-13c0-410d-bb1f-d1482ed9f257)(content(Whitespace\" \
                 \"))))(Tile((id \
                 faf7dda8-d4b8-42ea-ae13-5aa11feb8e2b)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8a0cbbbe-6fd9-4b6a-ab53-9fb91dbdab6c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 50948c4f-b7c3-4e06-9f9b-258aa1f727bc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff3feda8-f328-49d4-bd41-629e79b618a0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d0178505-95d0-4f2f-8032-892f51a6fc28)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8881bc71-9b9d-41a8-818d-7903156eaf95)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fd0b3f52-fa0a-4688-b836-c9b36ecc940e)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 52e3a3f7-a035-4b18-8b8f-9f979dda4990)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 317fdc99-f36b-4ff0-879b-850e5edc9a11)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b2b31f5a-9c7d-46d4-b3eb-760782eacddc)(content(Comment \
                 #err#))))(Secondary((id \
                 be0b842d-b1a2-4c2f-b048-2548143749dd)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 88c595aa-29be-42ee-a924-6b70bffff2da)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 74746f33-9ec2-4435-9349-bb3802f21404)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Grout((id \
                 1ed90d3b-0259-44fe-9eca-6ca3983ef3fc)(shape \
                 Convex)))(Secondary((id \
                 2df1b0ff-eb47-409d-a793-5ef5f060a1a7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cd06c18e-8d8c-4d11-a929-012cdf879eb2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 78fd89b9-6a3f-4f5d-840f-b05cbd0c6291)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 118ee06e-04a3-4984-bf7f-fc049655bec8)(shape \
                 Convex)))(Secondary((id \
                 4b84a846-618e-4a20-91e6-630a5cfc6216)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9efc685e-eb0b-4274-af5c-3b727a11468e)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 651bd3a5-8e60-46f7-af7a-fbf0fed279c0)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84df017b-efff-424a-9d84-9db1ee8af3ab)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 45ac71a3-b3cc-439a-9b19-1f1ecd291b5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9e3a7f9-2208-42d6-848f-5bbd215b8578)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7fbd601f-fc6f-4474-8587-f24e202d63d9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5444be75-8a15-4011-9be5-4ff1a100d478)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24a747b7-f52c-4f48-953a-a4a9601b05ce)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4b16c7eb-225b-41da-ab7d-29bb09bc15d7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 472666f9-c63a-4cea-afb3-e1b831d26628)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8c611150-d724-4343-8bca-8bba7e11aff5)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 19ecb84f-9b2c-4cb2-b92b-81930ddc0205)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8a1d7d9f-73f5-4be0-9797-ef14a199cf70)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 99b064a4-e53d-4ed7-aaa9-f88b679b2f8a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 55d3ed06-7536-41d8-9616-57c74c6da35a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 035b5015-bb1c-480b-81c3-a6df95b5b47f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 921678f1-441c-4598-bb33-f5f8183a0a45)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 fa08b0a8-91d1-4232-a801-b5478ebb1c22)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 0253f8e9-42b3-4313-9301-9ae2045be51f)(shape \
                 Convex)))(Secondary((id \
                 17a28bd9-724a-4b71-9ed0-95eb785b2c57)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 41f83e36-fa3c-44e4-922f-cfc708efcd22)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 07892366-7b3a-4ebf-a5d5-7a44e445e9dc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 09aee674-f8bd-4ab9-b988-3dba6a3097d7)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 01f5b11d-6991-42b5-9660-d8496e530045)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0afab371-56d8-4bb5-a392-598fcc42ce09)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2bcb1899-4192-42f1-afff-afba8fd29fef)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 960152f8-c040-4dbd-89f6-6f1cf799fca0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c844db93-0d03-4676-846d-20225157cc29)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a292e713-1843-48d4-8174-01fb849750d1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 23ac5305-27e7-4db7-9ac0-42d20a039c87)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9b6be93b-02ac-4494-8734-3b62532cff8e)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 2f362bd8-3470-4953-bc61-3fe80a8142cc)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 17aeefcc-625e-4104-a7a3-2751a81d2b51)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 079f041d-79c1-4944-b3b5-74cac6969f80)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5413c750-fcb8-4fa1-bc45-1ddb7301ed75)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a3ef706c-2607-48ba-82d9-251092fe28ad)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b475d1c8-cae5-40d8-b27e-65b67cba7bb6)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8fc4f3fb-478a-4d17-b19a-8811e26983a5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 b106bea2-635d-43aa-ae3d-1c86de51dfc4)(shape \
                 Convex)))(Secondary((id \
                 e0390bed-06d6-44c5-b1af-bbbc0c0bc9a2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a4f19df6-6ffe-46aa-927c-ea7a39e3b4c9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 be4539da-e6c4-40e1-9d99-c9d7d50bd2e7)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 7f49a150-347d-4d57-9621-ff0f8ad88c0f)(shape \
                 Convex)))(Secondary((id \
                 6c13bf42-d47d-40f8-97bb-f04d74cc984f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cd33d663-f459-41a4-873e-e34e3fa384f3)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 96483cfc-b9ef-4684-9a75-62f36e00b88c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 149da6e3-ee58-4051-a5d8-6a3930e70937)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9fd28161-ffe1-45ac-975a-d8300b6a71a9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de43cf0c-f735-4e49-b3fb-866aed74aa95)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 748f10bf-7a10-4ce8-afb2-d44d8787f65f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fe1acda5-c560-4514-99bf-e957bb24faa2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b83ba715-c9fd-4c46-b81c-e358a159be99)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9f95d96c-0a21-48a2-a94c-cc9792cb26f1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 053e6448-6d29-47d7-8116-533da90f027d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 51569807-0587-439a-a826-e623e568a763)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 a40fac7c-257b-4a92-a765-57507bc4bbac)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a5ca69f4-aa5d-4fc8-94ec-6c1548fe4d7d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 9b657f48-3565-4cc4-a7d4-7c2fc3af16ad)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 089ad6b8-c777-438e-9c55-61367c0a0e69)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c45e3cce-0a37-4776-9295-23d9488b54bc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 71ecc101-b41e-4d2d-ac3a-503099d51913)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 5bb68b81-9ae2-42bf-aa3b-0281eefa2da2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 06cd6dd3-a04a-4a9e-ab34-0afa26931213)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e7f1ed9-c472-430c-8e77-482e1ce91e13)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 916e32d5-996f-4f42-b6db-c06f7fb7f6ec)(content(Whitespace\" \
                 \")))))))))(Grout((id \
                 55575446-5ef8-4311-b88b-fed02c44c49e)(shape \
                 Convex)))(Secondary((id \
                 5aee4b12-f425-40db-b406-92287f6ad87d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 66511602-1a80-4862-b0b8-967c66010071)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 974e3ccc-f894-45ee-a8d5-6ac71b050ce9)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 940988dc-bb25-4a7a-a21b-0591a643db2b)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a648e149-85df-4f32-9afc-318fd1c1f451)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf9fcce1-2ae8-4129-8dc8-9c9da53003ab)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 216f9fdc-8fc0-4fba-bd51-c08f86668dc1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f4ec8e14-af2a-47ab-9483-70158b77a5ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be778c9e-58e0-4667-b1a1-3ac107751cd4)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 8d98eee4-307b-48fd-a2e2-34049d1b7ea3)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 71b87e03-3b13-4a93-b837-fd68bf67b34c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f42c9b3-9cfc-4fdd-92b7-ae8c27066277)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 1367b6e9-fa0d-4fbc-a428-20654831c9d6)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 43087a2f-28f8-4cbf-9746-64ec2037134b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b1810be7-ed7b-4dda-8621-59c431e2585b)(content(Comment \
                 #err#))))(Secondary((id \
                 2f6c69f5-cb14-4862-9f9c-3fa852365d80)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0e32c0d6-dc5d-4e09-8ea3-84e10cdf5e90)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bcfcc940-7ee2-4aea-a7a5-545b391a4a50)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8c7e68ea-99f7-414b-8abc-47e89603fcb3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 003aad98-006f-45c7-96a5-4fab187f9fcf)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 958a09e8-b55b-4e6e-88b2-d4b20ac12017)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c2415dbc-e616-493e-bd35-9ce6087dc497)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0bca339-cf08-49e2-9e4d-d4f82fe9369e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 bfb9ec1d-59ca-4d5d-9214-cbbcd161e8fb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8b3c8a66-79c7-4022-a474-76f2619ff47c)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b8d455b4-0c6b-4534-a518-4f944689b2e5)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b126ed24-c7a6-4024-aca6-465a0d9a4ff1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f708d0f4-ca1b-4a4d-ad0a-041fef669553)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0ccb3358-a752-41d7-888c-254be8df343c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00ffb2cc-1f00-4f72-bb89-6d0e95a98d1c)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6b68b3fb-72b1-4c9e-8a90-941b10cc0f3f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f14ead86-a167-401c-8943-5482db473985)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0c77d95-df79-4350-8670-df665ff7920b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1c8e06f1-5385-4bce-bb6a-12b92d631229)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8f10a713-dc32-4976-9d8f-4a42962e4c30)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a6218f3-064e-4d6a-9b48-358c90a25015)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9ad83faa-9dd2-474c-a4b1-4929e9978019)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8813b34a-4380-4ae2-b43d-1442fa66d8ea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 cba15917-e911-4cb5-aa74-8ac35141b283)(content(Comment \
                 #err#))))(Secondary((id \
                 6917381e-b893-436e-8bcb-30fed960a252)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 525b1ace-d1cb-43ad-a1ab-d596a31af40b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2b15bff5-bc10-414d-9bac-4893b8ed5b53)(content(Whitespace\" \
                 \"))))(Tile((id \
                 41c73ce5-a37d-491c-aa06-9dcfcd0699d3)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f0a57d54-0cc9-44be-b0e4-3b1778919895)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 fce8dccf-e0a1-4143-89ab-14a0122fd179)(shape \
                 Convex)))(Secondary((id \
                 6b1b2611-b437-46cd-a0cf-68b5890cf00f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 82c1bbfd-b917-4787-9182-7af4ce3ffdab)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bc788ed5-0f82-4d4f-8655-80a71592e593)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 97f0bc04-d12f-4dce-8254-4a203d3eea4f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4ea579c-2bd0-4891-89a1-55d6215b94f5)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 b5723cb5-147d-43d5-8a9d-64158eeaaf6c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd4359d3-6f47-4094-9698-3b8a672dc419)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1ed10c4e-f687-4bec-9131-4d9ee6a15fbe)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 da88700d-7ff2-48b5-b2a8-3ffca840b4e6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0e9b72f-80bb-4fca-95ef-1088dfbc1229)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 915cc24d-90ba-4a32-b927-d57f01206937)(content(Whitespace\" \
                 \"))))(Tile((id \
                 278537b8-cc80-44c7-80a8-bbb9b5e246f0)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3a558d4b-ca19-40d0-af39-569d260a2c5c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4fef1108-08fc-45d5-86a5-200783a621ed)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4180f9d4-b6d8-430c-8b66-f27b42f133ea)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7345a42e-3873-4a25-9529-31d5bf8517e4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 83c24cda-37af-482d-9c33-3a7e4480a07d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5fa67820-df6a-4560-9719-79bf9cc8a921)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 923eba45-d9a5-4f50-b859-5599d638b7f1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4cac8ccc-0b3b-473a-a4b1-cb31e7f14c53)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0eec5b45-df91-4e00-b467-d27a58ccbb7b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f4f4f216-3bad-4ba5-bd86-856dbbcf9192)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2db3e3ee-53a0-4c33-8d7a-6a63962e8751)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 30427694-966e-4d0f-bf86-7fc30445f735)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 92954bd0-1483-4741-814e-7d9c2cab1c6f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e68bc511-8997-4877-a165-36f0821b8a77)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 648509c0-4423-4f14-a0e9-88b4e7a821a1)(content(Whitespace\" \
                 \"))))(Grout((id e77ee6f6-6a8a-4644-9baf-15878905da89)(shape \
                 Convex)))(Tile((id \
                 07b9c6d3-db44-4555-90e8-699bfd2c8776)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 3978c5e6-310d-4172-9b1a-f3fdb9c9a7e5)(shape \
                 Convex)))(Secondary((id \
                 53bf9131-1101-40e0-9b38-c83cdf33732f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f01ec109-3ccb-4204-8c26-c72c4f9f755c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d17a8d29-53de-43af-b6b9-c150f57f1479)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 58f0f2b3-69ad-4bc0-ac4f-094923b88688)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2b6653d9-f2c3-46f3-803c-b64b1d52e5be)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 f42abde4-9d13-47ec-a416-9b99a3ccb03c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 07915269-1223-42d3-a407-aaee0e908c57)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 b49d36c7-f1b5-48e2-8c52-280f1885a7b9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7f3f254d-c215-4ee8-b4bd-3152a23559a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cac822c9-7a44-4100-9dae-d057216ba5d9)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5b26a641-e697-4d9f-aae0-d838541eb2af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 151430ec-aac2-4aca-bb53-8448aea2a12f)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ae58d245-4aee-4dec-a394-2ecadbe5130f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 42549891-490d-4e27-ac04-153748d2f717)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84d9938b-993e-439a-9626-583fa5a63770)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 90c576ba-ae2e-4e03-9f51-ed8783271c35)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 297587c1-e848-42b9-b2c7-e02556ec8dce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50345125-324b-47ff-8e7e-13a78a0b5f84)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1a139ae9-20da-4c2e-b25b-509d9ef78d51)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5f530883-0f20-4c44-9dc5-03f59af8b13d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 5d43e39a-e825-4f1a-855e-e0a8aa853805)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5e785c68-2acf-4b8a-a5cc-d1f9e4bb694e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f79f3780-dd43-4481-b369-4318fdbd1163)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b525f9ad-fdd0-41bd-b4f7-547b13801cec)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 50c95fad-b2ed-43bb-8659-5ba0d4299238)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6302e541-d006-4160-ad1d-4256109af72e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 979f1933-00f5-4904-9433-d19509a95220)(content(Whitespace\" \
                 \"))))(Grout((id aa9fe745-d50f-4426-b064-7d5cdda4dd1b)(shape \
                 Convex)))(Tile((id \
                 a193ea69-f128-49f9-a24b-b0dbe7759428)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 500e1353-6ca2-4224-a98a-b464bdeb20bd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1fe50cf2-33aa-4cf1-8c84-6d23c9fecde4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 3e481782-832b-411e-8883-3b4aaabf731a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 29c49e7c-3b9e-4a33-b28d-4745f5851b75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 acd2c434-961d-43cb-8ad2-f575de2e38ed)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 9f5c6260-eec8-425a-92c7-502748113e7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 11821e84-4b1c-4764-8b5c-91f9080d98f2)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 51d6e56d-81f0-46f1-953f-7ed0c9fc83a0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 116e46b5-5f4f-46f9-9f86-cc1899a095a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 550549fe-8c8d-4b4c-a2db-4c79cbe9f9f0)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 84e853f7-b9b7-4c5a-8340-6367988d9653)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2770f1f7-7810-45b4-8cd0-5215c5a1db63)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e757fa32-d8b2-4fc3-b8cd-e7ad63d8a9ce)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5c03b5be-173a-4817-b3fd-c713c9695961)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1380d79-e479-4916-a69d-2d8dc3f16155)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 61adf246-1cd2-46b2-bc6d-5f24a46fda03)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 cc5dd598-5882-45c2-bcb0-be728ac4c477)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f45c476e-7e9d-474e-b896-7e48821ae048)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 72c544f1-d934-4769-8e24-17b7ec9a6729)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4d48baf5-0a5e-49d1-9dc0-0d3ba0ca6b06)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 95808360-29bd-4e52-9fc8-584bee02b271)(content(Comment \
                 #err#))))(Secondary((id \
                 d3aeade4-ed02-48eb-abe8-137cd59311e0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 808fcb59-7f17-4f7d-b14d-544b3f707644)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b0886ad8-9ef5-4383-bc48-56c1d19aca5e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5051c1b-3f84-4824-ad2d-abe3d897ff1b)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7fcc4087-1693-41d6-9ea9-05ef2d248563)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 ef48fbea-33d1-4487-958c-9f343de3965a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 817d5f8e-029e-4817-b89a-e1972f6fbb63)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2340a0f9-9c04-4a18-8c67-127330f4fcc5)(content(Whitespace\" \
                 \"))))(Grout((id 2b349e26-8df7-49f0-84d1-9ff245de66a5)(shape \
                 Convex)))(Tile((id \
                 58873e2f-8d36-4bc3-bbb2-9c5737655181)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 eefaf6a3-9ff5-4773-b9d3-7ce68adf9677)(content(Whitespace\" \
                 \"))))(Tile((id 91edd887-5da8-4bda-87f5-cc4a461d441b)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 327a353b-1a55-4b3a-b45e-6e90fd3bcc42)(shape \
                 Convex)))(Secondary((id \
                 8801792e-5bd4-4268-90d4-27c9243aad80)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d1eaf48a-161a-411a-a75f-99b40cfee490)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cc2f93f1-9fde-4902-85e6-aa9fecf80042)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9607a2f1-6118-464c-8be6-a8bc4bd41018)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ca964dca-0758-46c2-8113-537e160ef4c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7f218f9c-a0d3-4f27-b49e-d1fe0ef90892)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 4edb3178-8c46-4e4a-aa29-37b19eec2960)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 42092f1e-6384-4c00-a71b-71a5452caaa3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2dbf11f-9902-48c6-9f9a-1dcfa8526383)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 8de65036-c62e-48ff-ae95-36a74c889b2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7b89304b-1fa9-462a-8c4f-324a1cc7c781)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d20cfc4a-e944-4344-a87d-461c2725dd8a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1b97ac7c-8929-4732-a327-7c92e8dfae67)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b8b8deb2-c2e8-4da9-9677-03262af91166)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 d8a5468d-c6e7-4e57-8224-8f158dcc851e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3adbcbde-a47e-43d2-8107-1c697ecb2f9d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1153a25a-2402-4baf-8b74-978e4074a613)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 da81349f-87ef-499f-a72e-44c91fafae5c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 072e1d9f-e0f2-4ae1-b6c6-b3aa516209c4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d6198240-8b31-411f-ad47-16c8110b9b2c)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 27cb4566-8da1-4518-9a60-44707f1592b6)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 de9d1f1b-666d-4b01-bc9b-1a622f153a5e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3ae0d130-77cf-498d-8fed-0e21b2ee6b56)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 e10fb2e6-9d0d-44c4-b4b9-e8e4438c0373)(shape \
                 Convex)))(Secondary((id \
                 70d9166c-ad37-42bb-8591-9355f60a42cd)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 6836089e-15db-46e0-a6ff-69581590361e)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 937d97ef-a795-4364-83b5-b408fbcb2192)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 425082ee-cd27-4523-b515-566577bfc5df)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1c16eb46-9083-4f92-87e6-6a9eb31bd27f)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0b14f155-fd1d-4566-b583-deacf7233830)(content(Whitespace\" \
                 \"))))(Tile((id \
                 25861fa8-e6e2-4869-a0e4-08da94c7da08)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 65419746-58ae-4219-911d-89504c84616d)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 84f84c32-975f-476e-b1b7-05c1dea4904b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c990a8e-d585-4dcb-8a32-c52e125ef1d8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e1d389a4-0040-4ac9-bf49-600e36bd1a66)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d1a1e36d-13ca-4447-ae30-84fbe454c39b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5dcad24b-9cb7-4400-936f-ae0eca233122)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 6b3edbd6-4f58-4954-9629-f8a63596558c)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 0e6db454-24d5-4186-9b36-5bba89f7a950)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a81cdf92-7cef-40b3-bd3c-8622c607074b)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 be677733-66ab-4bfa-ae18-2515727751af)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 43e00334-03ed-4fc9-9fb4-64f5a86d32e0)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ee24ccc2-6905-4f82-bfed-28ab1d1cad0f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 196eadb8-d6b0-4931-8658-f13d52addd41)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5eea9ad8-e21d-41e5-9798-b349d0f2397a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57df228f-8ee0-4a31-a58e-653dc6d030be)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e7ea053c-659c-4b8b-9cf8-e834e7baa130)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c7ca9a74-35fa-4114-b70c-41f1a2384c30)(content(Whitespace\" \
                 \"))))(Tile((id \
                 720a926c-64ae-44b3-b403-8989d8bacd12)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5db6b532-3d37-4726-a062-554824d129c7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 dcc6e7ee-621d-44c2-8a68-095d68402ce0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6081bc10-e963-46b9-bfba-5695608fafde)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 92372eb0-1c91-46f8-a7e9-39c3a066468d)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 93657341-d5e9-4292-9f73-ea0c9431acc4)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f5f3796a-bcea-44c9-bd19-596e74423e5b)(content(Comment \
                 #err#))))(Secondary((id \
                 5dee7e40-e3b3-4cf4-955f-7098169b493e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d9a6c137-1c26-43d3-be62-37523f212ab5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 570b2149-05bf-4865-86b8-c4e01713556c)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5816bbac-0a70-4aaf-93c0-229ab489ec7d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 34f06cf8-0d17-4ca9-b1ac-41f51404c2d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a829ea64-6e21-488b-80ba-3089f26163c6)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 d320e931-4401-415b-a9a4-3aee6a88e514)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 0f9b062f-dfda-4418-911b-2b0ed2b67110)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9b11ac73-e710-4eb7-9ab7-e081a7af0881)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e232cf37-c9e4-49a8-9daa-d89f111fe247)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 aef81daf-0823-4162-8156-98884e1c92fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc52bc46-beaf-4e43-b2c9-7ebe15ccb6d2)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2644788c-7c22-43a1-bc88-ab4162dcf03a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d2f006ff-a185-4816-a9cc-05ca5da03ef7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4ff87cb-2f83-4330-93ab-57f487872953)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b5daff1e-356f-4ce0-be69-88cab421aaa1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4d5f850e-68a6-4aec-b0d3-0e5a035efc96)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e3ba422-3de5-4d07-83ff-71bed79de077)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 33ea8987-2be2-435e-9284-26390ecbb0f2)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 95473f15-606a-49b5-927d-bf37726d7dbe)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a70b8fc7-fb61-4955-b83e-e2ce3f72fbcb)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 ace09da0-6b6a-4361-aeda-f8d496ebe930)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0c62e3d5-569a-456e-883a-7cb63568e6eb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0501bd34-8788-403e-b817-1ae4f4139cba)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 87271f91-a9e2-4f65-ab86-ff1e39b22c90)(shape \
                 Convex)))(Secondary((id \
                 55b78ce7-0d89-4cd6-8c6d-cb3c5f2936ce)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 49a7d88b-6964-4cf8-93d5-8f1897b29e8f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1783aee2-cd93-4c3b-8990-2bdc905265f5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 adaa074e-3e3e-4163-93e8-c91357a7c069)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 53c844b4-b15e-4864-9c71-af68c54c3745)(content(Whitespace\" \
                 \"))))(Tile((id aab7194f-7d9a-4ee4-b504-9a66ebe3f954)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 4f230b3e-64b1-4b32-b876-b88ee6b26c92)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a834942f-f299-4104-a91b-14e88f783fa1)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 a50b5a5b-2712-4e2e-9422-8315d413fa4d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7eaae820-c46a-4439-b638-440eb2635475)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cdb85333-3df0-4c61-b74a-6c2579442dd3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 99a18462-9059-450e-ba3c-ee3b584dcc84)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d8fc35f-ce87-4df4-b38d-f07da07760a8)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 74e1da18-76d0-4a01-89f4-444e9507d9cf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d75373ea-a9ae-4f1b-b84d-a2b3464f4538)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8c69c1ba-8e81-43c8-9197-3a74ace2bed5)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 776e40e7-e80b-4c65-a794-9553ffd60a44)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 56dad9ee-9a23-49bb-9aa7-1cabf9cf6e57)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 85103f04-7cc8-4b15-9f92-c3d15416e45c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e2a1fc3c-8493-4f37-83a1-633234f96497)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 785ef0c7-ea5f-4336-be1c-b9db7c0c39f1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a96f9a2f-e72b-468f-baff-1f716ea22a6b)(content(Whitespace\" \
                 \"))))(Tile((id 07997f0c-1a46-443c-af54-aecd4df3624f)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 aa080005-7b81-4ab4-b551-5a20baf6acd7)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6efe36ec-75d1-4e46-ace9-6a3be7dec19b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7da234db-3b38-41a0-86d3-205abf8ed475)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b370920-ba33-44ec-a9eb-0097c9160f39)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ff7c5014-bdcc-4c94-97f4-59b02db1068a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7280800f-c9e9-44e0-9232-490c614168b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a5957257-d1aa-4fbb-97e6-bb3671bc68b1)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8faf8cea-42c7-482e-87ff-2b7cb7bfe852)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 22b48561-8b0e-4fbd-ad16-620a0dd71ef2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7a104f2e-b42f-4fcc-8f9f-3e85c38ab1e6)(content(Comment\"#err: \
                 inconsistent#\"))))(Secondary((id \
                 2df9ddb6-1136-4cb6-9e6e-a5b7ffae61a2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1ff7c125-5505-46a2-bf5c-d53ece411d68)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 e95eb53b-9fb0-4913-a1c8-fceebbb5b96b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20f7114a-dc58-456b-8caf-6b379f892478)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b4c216f5-7522-48bd-9848-0858284e3277)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 d33d2c86-7c65-456e-99cb-b4bd1f4c1a7c)(shape \
                 Convex)))(Secondary((id \
                 2e87e184-f9ae-46d4-a340-4e2a9f323ca6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b834c4c1-da05-4242-bffc-a090c7b2d8bb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 64f2f6bb-e8c0-4a52-af39-f305ba06f364)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4d4fb8fd-97ba-4b50-8f1f-3fcf728b5db4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a0ea0b4a-21a8-46fc-94fb-2db83fca9ea4)(content(Whitespace\" \
                 \"))))(Tile((id 2cc39d55-c60d-473c-998b-6160a75638e4)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 117c83b8-2206-429d-a56c-d40bfd7776ac)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6259bd06-00ac-4b40-be99-0cd0e341c512)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1de6639f-bba9-491e-8ebd-e787f2038d32)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d5dbe4c8-5e5f-49c5-a523-f39dfd8f87d4)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 424d617e-fc9f-4f4e-979e-1849c65bf947)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22ba54c0-2024-42cc-b65c-1dd1f313e1e9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9674cb1d-530c-42b7-9d59-77162f16a9f4)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 efa9f2c4-eb02-4ac2-9877-0739e9cf4a9d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 41ae0873-4c54-4585-b881-abfb6b192c44)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1d812f1d-0c64-4847-a948-7b16d463ccd8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bfe30437-7302-40d6-b296-752d83a1575a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1b82c0aa-0ae2-4a46-939e-23dbfb12322c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9c71529b-2815-4b95-8197-498f454ae3ab)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 42ec30c0-01bd-434d-ae1d-d14be295e21b)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 69889b7f-c217-40d9-9487-859092a5b846)(content(Whitespace\" \
                 \"))))(Tile((id 8bc084bc-07b4-40d9-811b-3b6eda86dad6)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Grout((id \
                 49df99bd-88e0-44d1-810a-effdf59c081f)(shape \
                 Convex)))(Secondary((id \
                 46f09d71-b172-477b-a3d2-92bdcdec860e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7d1ee486-97af-4ae6-b0de-735330b8d08f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9b4f59b0-3467-4834-8ee7-c63ba9983644)(content(Whitespace\" \
                 \"))))(Tile((id 3a7f90fd-3bf6-48fc-abb6-3591ce1c47e7)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 152ade16-b873-4894-8820-355d43f28657)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9f048dff-10be-4279-843d-1426fa351c90)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c8a74948-a2f1-4ffd-967a-c15a41df83cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 267e4ce4-37f5-4e5b-8edb-05e9d55f3af3)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1f2b882a-77fd-431c-bb4b-8b17a1d59fcc)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 1132e137-75f4-4b4d-bd68-80cfd288591d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ad0586f-a689-4bf8-bb24-a66a0235b8c9)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8d9e8dcb-fa87-4201-afec-136ae484628e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5bff88ed-a4db-4d24-855e-dda4a08cdd58)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 95aa55ff-e342-49ac-897f-cc93134c801d)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b5bb8567-2b0d-4cad-8ee9-7bdad8e7c482)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d904fb88-21fe-4e0c-a9b3-a30aab73eae2)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d1a12ddb-adb4-482e-ad9d-4dc4972e70d1)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d2710fa5-04af-41bb-b0ad-69d35e679207)(content(Whitespace\" \
                 \"))))(Tile((id fa33d60d-a0f0-470e-9088-d32074b7f19c)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 a3bb7870-89a6-4185-9653-9cf85f3a986f)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 29b182e0-8fff-4c40-907f-9405bf7743dd)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1cf9c440-5614-4961-987f-26d645bdcf77)(content(Whitespace\" \
                 \"))))(Tile((id f575521d-ec82-4902-ad2c-19b7e12a708a)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 2f9c3a2e-6233-4aae-9d4c-f2a5337df1f8)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 93a850f6-648e-492c-a7a5-745953806111)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 17d767f6-998e-4ba6-879a-d7b37ab31093)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4b5a3f85-bf43-41a0-bbe8-b187c38f544b)(label(1.))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 dc8bec51-db57-427a-a777-053b74da4c4a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 85cd09aa-7a51-47bd-86de-e4cf72111de3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8488d530-224d-4a7c-ae29-0053ed79b88a)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 06475275-2d03-4e8a-9c7c-2871b9c70e38)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 caf75728-54f6-4721-8c79-b838ad71e5bb)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6afc455b-10c5-44a2-99ff-a2d4cb0930a5)(content(Comment\"#2x \
                 err#\"))))(Secondary((id \
                 495c074d-333a-4a60-817a-a100a6dd685b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 40ca2476-4981-43cd-a3af-4ef582e72e68)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1a5bf5b8-7a54-41cc-af8e-868299b50e35)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 51ae45cf-7353-478f-aafc-900191de3a75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 16c834c1-7948-4d83-8d87-7895bb96c8a0)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 904444ed-8256-4436-a567-6bbf3211e45f)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 fa379734-6d8a-4091-980c-6848674adaee)(content(Whitespace\" \
                 \"))))(Tile((id 776d7837-f87c-45a4-b2d4-a7bc92fc518e)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 5636d52d-06ee-463b-9c60-b65b71c84cb5)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 cd39e386-c4aa-4f09-83de-74c8d954546c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 81f0362f-5867-4343-b5af-3effcc9f2bbb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a3e7127b-e803-42f7-b2de-c062f9edcb31)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 412634de-d42b-47ca-9994-37756ce3370d)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 bbe78c16-ee7d-42a4-b885-82bc4335cc97)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bda66683-b578-4a85-be97-c2d3457d8e8f)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 f444f61c-85e4-4528-975a-5450329d7c1e)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 90f24c55-e87f-4652-9945-a67dae89cbdb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 74bfc0ec-4e2a-48cc-a849-59d3d24f6368)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b9036931-40e2-4b6a-b9d5-3278f0db58a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ddeeb70a-6fcb-456c-81f4-ffa183d5e0f3)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b3244d84-83df-4132-a597-f4cd4699b670)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 47122dff-f5f9-402a-b05b-b567e5c48714)(content(Whitespace\" \
                 \"))))(Tile((id a31b19e4-5a23-4325-ab2b-bdede6494449)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 9913e144-00a4-4a5a-81db-f286882ce187)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 222b863a-8253-485e-8df1-5a2fc1fe55f0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f3b80efe-3f82-494c-8d19-634dd8c36c77)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2c6aa0ad-c7de-4b85-a9de-8067645d91ed)(label(1.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c0ede111-0834-44cf-a2e5-83f05abcd76e)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 e32400d7-dc82-49dc-b1ed-e7f83653c456)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 28bb6727-f87a-4880-8e61-d4ea6a040acf)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 b8711222-cfd3-4d58-b33b-ae4f84493052)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3b4fea0e-4d22-48b2-82f4-dcf5ac950785)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bad01cc0-cc0e-413c-9776-56144666333c)(content(Comment \
                 #err#))))(Secondary((id \
                 a2b1169c-a58b-4080-9016-082308d36481)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 106fe1d4-4c4c-4244-9aa0-0524b192e583)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 02d47129-fa52-4200-ab5a-f3a8457d7329)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2458dc4-4ae5-4825-9ae8-b16ef4a05037)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 76b99e8b-b542-47b1-94d7-66e8137186d5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dc97ac8b-d950-4cf2-a7a8-c2913fcdea16)(content(Whitespace\" \
                 \"))))(Tile((id 6c3e1a9f-3e6b-476e-a5b9-289b4820eae0)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 b0b66a3e-17ae-48d3-8f6f-c886d5af1dcf)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 94c87a73-c2af-4990-b211-b003d54a1921)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9686510e-dd05-478e-a2af-0f8648e46bdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9e0ec85f-6a4b-436c-a961-fecfb6970ef2)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 6aa76622-14e9-4818-bb2a-42c46cce7203)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 6731a3a7-6c75-450d-ae92-f6cee728d8e7)(label([ ]))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d1f031ff-2075-4b72-a01d-1a12b3158de0)(label(2.0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3880c391-4a75-48fc-9f6a-ef8c6bc190d7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 55689b84-3008-4afc-9008-fbfa2313dc8e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a9d0b15f-0fe6-4759-919e-93efc4872afa)(content(Comment \
                 #err#))))(Secondary((id \
                 5155a99b-f4d9-4a27-89ef-21051635147c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 75964442-dfe8-4c9c-b72a-c04eaea85f73)(label(\"\\\"BYE\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))()))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "#Types and type error examples#\n\n\
                 let _ = unbound in #err#\n\
                 let Undefined = Undefined in # 2x err# \n\
                 let true = 2 in #err# \n\n\
                 let    = if true then 1 else 1. in #err# \n\
                 let _ = if true then 1 else 1. in #err#\n\
                 let _:    = if true then 1 else 1. in\n\
                 let _: Int = if true then 1 else 1. in #err#\n\
                 let _: Fake = if true then 1 else true in #err#\n\
                 let _, _ = if true then 1 else 1. in #2x err#\n\
                 let _, _ = (if true then 1 else 1.),     in #err#\n\
                 let _:   , _ = (if true then 1 else 1.),     in \n\
                 let [_] = [(if true then 1 else 1.)] in \n\
                 let [_] = (if true then 1 else 1.) in #2x err# \n\n\
                 (   )(if true then 1 else 1.);\n\
                 1(if true then 1 else 1.); #err#\n\
                 (1)(if true then 1 else 1.); #err#\n\
                 (fun    ->   )(if true then 1 else 1.);\n\
                 (fun _ ->   )(if true then 1 else 1.);\n\
                 (fun _:    ->   )(if true then 1 else 1.);\n\
                 (fun _: Int ->   )(if true then 1 else 1.); #err#\n\n\
                 let _ = fun x -> if true then 1 else 1. in #err#\n\
                 let _:    = fun x -> if true then 1 else 1. in\n\
                 let _:    ->    = fun x -> if true then 1 else 1. in\n\
                 let _:    -> Int = fun x -> if true then 1 else 1. in #err#\n\
                 let _:    -> [  ] = fun x -> if true then 1 else 1. in #2x \
                 err#\n\n\
                 (  )::[(if true then 1 else 1.)];\n\
                 1::[(if true then 1 else 1.)]; #err#\n\
                 (1, 1)::[(if true then 1 else 1.)]; #2x err#\n\n\
                 let     = [1, 1., true] in #err: inconsistent#\n\
                 let _ = [1, 1., true] in #err: inconsistent#\n\
                 let _:     = [1, 1., true] in \n\
                 let _: [  ] = [1, 1., true] in\n\
                 let _: [Int] = [1, 1., true] in #2x err#\n\n\
                 let _: [Int] = 1::[2] in\n\
                 let _: [Int] = 1.0::[2] in #err#\n\
                 let _: [Int] = 1::[2.0] in #err#\n\
                 \"BYE\"";
            } );
          ( "ADT Statics",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 e02ab4b0-ad7f-4a5f-adbd-7f3f6cadb6e4)(content(Comment\"#Non-recursive \
                 sum/alias tests#\"))))(Secondary((id \
                 b46efd66-c0d2-4fa8-b91b-4e1b72cb542b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c60f815c-eef4-49f3-bbfc-069c66a03c88)(content(Comment\"#all \
                 lines with trailing err comment should have 1 \
                 error#\"))))(Secondary((id \
                 374b4054-6c04-4230-b7a5-500739a62808)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 b802cb83-2a52-492f-a979-da5e5b00a441)(content(Comment\"#no \
                 other lines should have errors#\"))))(Secondary((id \
                 a2c1f9e3-670c-4509-b7ce-73cf3d50f0cf)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 17a87e85-5319-4231-b76a-c38fcd3b4822)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 ff3c5fc5-e7b5-448e-8317-84b0dc287a3f)(content(Comment\"#type \
                 definitions: no errors#\"))))(Secondary((id \
                 d132bffa-c6e2-4fe9-9daa-6cc38ee03d06)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8291f65c-2d5b-4a0f-9caf-c9e2c17aaba2)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 23c0fc12-e3a6-40c7-86bd-97bc52d198b0)(shape \
                 Convex)))(Secondary((id \
                 f767359a-89d0-4103-8f93-1add0e7ed747)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4a88e09d-cc6b-4ab8-a75b-0595e77e6156)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9c881c5e-68d4-447e-b481-7c09997b644e)(content(Whitespace\" \
                 \")))))((Grout((id \
                 21173251-8182-4ec4-a2ab-5cdc87412d39)(shape \
                 Convex)))(Secondary((id \
                 04022844-8aaf-4d11-b275-0355f7abf01a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 56ef1483-c3a8-4d47-ae25-9b71ef3cb878)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ce66f30f-e003-4ec5-8f1d-19dd9a1efc71)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 27010414-463d-4d4d-a267-be7c8ba285de)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 30faa404-a0ec-42d9-bc43-e0db67559e13)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d140aec5-5ef8-4a59-9f02-ddd1f92357cd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 005a49d6-55f6-479c-bbbf-f97c95e87d37)(label(SingleNull))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 60effa1d-49ed-43da-80da-c1389b2bb1b9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 69a145ae-58fd-4595-bdde-1cfe8bdc919b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1c2e414b-2529-4e2d-8cc2-4074e99ed2ee)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7ca1c234-543e-4349-9e56-378f940a33e0)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ceae9315-3d26-4860-8bc5-6b004ba91b8f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 418cda1a-40ae-4fa7-85fa-972846efd4bc)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 507b0776-9352-41ca-848f-bf9a0d8b55d5)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d2209ba3-068c-4cb5-8958-958ee5b16de0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 defece0a-0a34-4fd0-81b7-321e87f6f06d)(label(Single))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 03a1674c-7511-4f4d-ad0e-5279ad40cdd4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 77075ac4-a958-4494-9f3b-84e4f644a608)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2e68c83b-5262-4fa9-8691-3d6f9f3af97a)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6f4c8927-aeab-44eb-a511-c25a40f4f319)(label(F))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e442009f-cbea-4885-a208-f51035c4f555)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 37f6bd0f-5e99-4680-9d13-3d331ca20d3d)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 dedd7be0-b3ca-4838-83a6-bb0557ac78bb)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c95862c2-d39a-43df-9ee8-c2ff130a761c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52f06b1d-84de-43a5-94c1-c40e2a820353)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ee823c6a-ddcf-4a4d-a951-2f46adefa674)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6ed501c7-173f-49fb-aa01-0985df743cb1)(label(GoodSum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 601105db-10d7-4fb0-9079-868ac9ae9258)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6a240a5a-b76e-4769-924a-4b37463fbe7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf9c0ad6-9ec3-4900-babf-396c12dc7b20)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 9c303381-a8a1-453d-8289-e675c3d09d3f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f30e1879-f764-417f-836d-9bc86213b4da)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c70b7715-a3d2-4996-8255-b2a26d109be8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0253a77a-beed-4cb0-9ad7-82adf826f289)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 11a8a4f5-2228-4df6-bf43-03e15b30a2f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f2ae5688-a75f-4d4f-95a4-0b3a992db947)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 b729282b-804b-4f3a-a473-6bd34d8a48bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a0e78f0a-7ec9-403d-b59b-e734d4249bae)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 220ee55c-1689-45ad-9a62-c8e0cf4c9093)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 cb3341fa-5271-4a85-8676-9c51256c389d)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 656e946d-cff5-4238-a2e8-7615fcc69a85)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 bdab1916-422e-46bb-b83f-eb91dd03f341)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e00df336-fdf1-487f-a835-79371a5cf636)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 ed15425b-1db8-43a8-b460-05c711e2a139)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6c97ade9-4e36-4c6e-9838-0e4f663b4271)(label(Partial))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 ab353827-ce00-4f57-9c97-0dbb332e72a3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3228bff2-e7b4-4478-885c-7cd454193659)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8038806c-aff1-496b-a545-f07b9346b28b)(label(Ok))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 0b00e961-0cc2-441c-9d39-4925f37b7ac9)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Grout((id \
                 95abaa2b-a737-46ae-998e-1d879a9b09aa)(shape \
                 Convex)))(Secondary((id \
                 120abcfa-f358-4373-ba21-baa95f454f60)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3a6718b3-c60d-45e4-9f98-2a232b79b3fa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3565599-9dd6-43cc-b075-da15070a2a72)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f403501c-f4d4-4f52-9aa2-e4f908ec85e5)(shape \
                 Convex)))(Secondary((id \
                 e82891c5-1e43-453d-8428-69f04b8f9f60)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e8920f16-c8c8-4596-8c16-96ef063e6263)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e6d3725d-6685-4b8a-a875-30f02b34fec4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 31b6daef-942d-4f9d-a666-ea8800b85694)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 83d3b871-e45f-4713-b15a-c5eda96bc639)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 56ba1a8c-3a80-4f5c-9f6e-16afff59ab44)(content(Whitespace\" \
                 \"))))(Tile((id \
                 194c5e55-d74e-4d0c-8095-be7ae0f570e0)(label(DoubleAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 5e360b02-6181-4d82-a088-bd245208c872)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 aa77f60c-b166-48bc-90f3-a28bc772ee80)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4346daa5-8f36-4f9a-86e6-9021aae25752)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ae2f8a06-13ff-410e-a07a-9a1cc7f47281)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0c5ea509-b36f-431c-9291-eeca9dca7aca)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b4cf592b-8109-41ee-958e-de372373f649)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1d6f3c1b-9bad-4449-8e72-46f877284162)(content(Whitespace\" \
                 \"))))(Tile((id \
                 26e534d9-11c1-4ee5-98e7-6d8a524aeaab)(label(VerticalLeading))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 73c5925e-4e47-4e78-9079-7d0b316cde95)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 45a265ae-4184-4ac3-a3e1-a46012a97cb2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 82ccbf8e-3a2c-4bff-badc-70de9c1f7004)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4a5e35bf-55d0-42af-a8aa-c675bcd5fd82)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab1ffaee-50cf-4cc3-b6fa-0979225448e9)(label(A))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 09829a3e-108d-4f97-b5e5-7962433d4428)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7d23bdb2-087f-4d39-8e67-c67752280558)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 999bfe5e-a16b-4935-8f26-290888d6e63e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f110505e-4a62-4db6-aa7d-6b783d01ca37)(label(B))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 03268b63-7baf-4304-b7da-559aef2b6635)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 dde499b7-8bea-46d8-8b1c-f25e14cec5df)(label(GoodSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 7d553134-d91f-4865-8d98-b8a98c08448b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d7f5db0f-78f0-4ca4-8db7-3309b20720f8)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a087ca3c-d0cb-4671-b620-d026bfe8e5b4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4d14b2b-2756-4e9f-beba-f39e17ffd321)(label(C))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 13ae127f-33c8-4fd0-9b3f-5dec6c902677)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 d816a858-4309-419d-9052-0be33b39dc21)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 f4703885-ca37-48e9-9fd8-4fe9a028a4cc)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 ea8213f2-c2c6-4736-95e7-2a4bd872bd28)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 b6e64091-e1d8-4adb-bced-88fb11a2abd6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d5b710e8-db46-4bad-9498-9cfc228cdcad)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2567998e-4ef0-405b-a8ff-5c5dd63fe8a4)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 4b6ea144-929a-450e-9a15-5ea2990c79cb)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8069d756-0ccc-45b8-85d0-1aa2537a11f4)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 9969ea4d-80e9-4daf-88ea-8d5ecae4edec)(content(Comment\"#incorrect \
                 or incomplete type definitions#\"))))(Secondary((id \
                 1736ca9e-53df-4d50-8b84-77b7cbf0362d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 de23843f-5b32-49df-879a-babc4443da9d)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d3d83a8a-3cdc-4158-b968-25e09d38a1ce)(content(Whitespace\" \
                 \"))))(Tile((id \
                 83290241-547e-4cde-9561-5f06800293c9)(label(badTypeName))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b2463b92-cf1e-46a1-a97f-3d9fff97af5b)(content(Whitespace\" \
                 \")))))((Grout((id \
                 f67634db-6045-4641-a56c-890d8508f381)(shape \
                 Convex)))(Secondary((id \
                 d78ea29a-a97c-4cfd-829a-7e3493478135)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d71cdbd2-4280-48af-9230-7f7f1e99fffa)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 4af55c89-7d80-4bd4-ad9e-ba8dc3e05fde)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 741192b7-0b11-4a6c-a6fe-2775a172d992)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ef985702-292c-4a4e-bb49-f385d1d8a81b)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 b8dbb302-1688-4295-8ade-f25bd1d78b82)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 74571058-ab2f-4ad3-8ac7-889515164841)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7d6c3f0e-3be5-41df-b7bb-14b9c19aa665)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9ce14231-c16e-4729-a11f-409051723919)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
                 744240d7-3593-4c35-8120-837a395c5d69)(content(Whitespace\" \
                 \"))))(Grout((id cec6159e-e53d-48f1-8efc-088b9f01a587)(shape \
                 Convex)))(Tile((id \
                 0b97e8fd-7c11-4a10-858f-493161af4c32)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Grout((id \
                 990ac44b-df6e-43d1-b1fa-36bc9415f49d)(shape \
                 Convex)))(Secondary((id \
                 47003f8a-92c3-4a32-a452-81aecbc1c198)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2573d024-24b2-4511-b224-a39bffbf191c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d933f911-16b9-4ecd-97ed-04dd6fda848d)(content(Whitespace\" \
                 \")))))((Grout((id \
                 0277ce51-c48e-4a61-9ea5-4a36e5ba665e)(shape \
                 Convex)))(Secondary((id \
                 a9a8dce0-9810-4e77-819b-5efd538bfa24)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a055e932-523e-4872-a88d-cbbaca432e1b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a0019b19-49cd-4c47-8725-645fbeab30a7)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 26ea4913-d2c0-4862-9c07-301580969b5a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2ef0f26f-9505-4375-b333-24c8fcc820da)(content(Comment\"#err: \
                 invalid type name#\"))))(Secondary((id \
                 6152005b-0d3d-46a4-9245-ea20ad71087b)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 76e1ed85-aa91-4709-8ed4-0e0b29c416ef)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Grout((id \
                 84842b95-af11-4784-a4cd-9a3462d5ee1f)(shape \
                 Convex)))(Secondary((id \
                 07aad66b-79a0-420f-8c79-dd06a2f0d57e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 81cb9e7c-4533-4bb1-baa5-2645215bfa4c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ae7bab47-7c00-40de-8b83-11ec610b3ea6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 42350f99-cfb4-4fef-81b8-fe998ef67694)(content(Whitespace\" \
                 \"))))(Tile((id \
                 934b20ac-a16c-49c7-995c-5c86ff450b9a)(label(badTypeToken))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 2987f60b-72a7-48c4-b216-48a3bd3e2038)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48c367fa-b5db-4701-b50e-0f04ba6cb505)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 47df24ba-d2dd-466a-9bdb-59bf4eace217)(content(Comment\"#err: \
                 invalid type token#\"))))(Secondary((id \
                 561597e2-669d-4e63-868f-5751bc6deb34)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8e4a58e7-4b02-4c77-8534-af22968b68fc)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 633f77d0-9712-4097-a276-bd3ca672c308)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1be60ade-36d9-40eb-b072-87640b432253)(label(NotASum))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 a5d2097f-be39-4092-84f0-7e1b11bd6445)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 dd16a57f-27e2-4e4a-967e-ae55dee7a4ac)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cff41b03-4d87-4b61-8cdc-d19c1d03dbc5)(label(NotInSum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8295ac17-c3e8-40a6-87c9-7ebc7bfd65ef)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 4d7c0f06-754c-467b-a775-49c8e44b1b16)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 10df3ed1-35f8-450c-b384-aa952bc0ed50)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b0ca6230-c1ad-4b19-93ba-ba8906a6a154)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3fa5eb99-09f4-4888-a4e9-134dc8d37667)(content(Comment\"#err: \
                 cons not in sum#\"))))(Secondary((id \
                 61caa9d1-0ab2-4656-b8a4-ebff1a489f3a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3893d81b-9988-4e79-8d6f-5bb3d85028e5)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 481b05cb-6b98-4d01-a272-afb098f09d2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 878f6994-d711-49be-bc4f-afdebea8d45d)(label(Bool))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 2ac87eee-da97-43d2-96b6-c2e3867eb041)(content(Whitespace\" \
                 \")))))((Grout((id \
                 077be232-f613-4320-bb48-5103cbe0dcb7)(shape \
                 Convex)))(Secondary((id \
                 03db7084-8180-417f-b73c-14263cf5971b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 168c10f7-66ba-4d60-abf9-0cf5ca29b6ea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 642f8e68-debe-4ee5-aec4-7187f4c869f5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 87815d7d-7161-4fb2-b964-0a9c858be0b4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ffb60f65-c457-494a-be95-dd3360ff9061)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 35199152-be8f-40b9-afdd-02a268351ad3)(content(Comment\"#err: \
                 shadows base type#\"))))(Secondary((id \
                 be878039-43b9-4760-b444-6b10ee4da7db)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 70daf0a8-49b2-4a88-816a-4285fd10d107)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f40acae0-b4df-4b5f-849e-628700c41fb1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29bf64e8-38d1-4dc8-a5f1-934c6f30c9b5)(label(Dupes))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 4c0476d8-93ca-48bc-bb28-f42c9773cb42)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9a7bb76f-0d28-4011-b64a-5517003ea310)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bf2f3f86-5da9-476c-b3b6-286deda7ad90)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a52bdf83-fb3b-4a11-bdb7-7f45ddd340e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 870720c1-52c8-4727-b437-2f718a149220)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7b1e077b-4896-4377-aadd-14b1175326c1)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 10053a1b-0845-401c-87d7-5a2fe4e76220)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5ef238dd-463f-4380-b7f4-aa89cf981d4b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8e7a7535-1b33-4207-9705-0183dc9e5609)(content(Comment\"#no \
                 err#\"))))(Secondary((id \
                 6e2aa0c4-1ad7-4554-a75f-0bb980e442e6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ec36d992-63f7-4e5c-9d7d-3ef9a57a8396)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e7e12cef-9ca3-40d7-a6d3-d22c98093853)(content(Whitespace\" \
                 \"))))(Tile((id \
                 56fcd642-48d5-4123-a614-fb36d710dce0)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2f58bfe2-d888-4ddc-ab3b-e1695e84e15b)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 9ba3a4d3-6bf1-4bda-aa67-1f05f11999dc)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9153da45-df76-42f5-ac68-bd90c3e7350c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8ffb042b-81e7-4196-b961-eb0442f02232)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 663e26aa-acdf-4de7-b7a1-f8d64171e202)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6c3b6545-4a42-4401-9423-c32059732f10)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c88b6534-201e-4f6c-b3aa-846eb40722cb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 032d9629-76fe-43a6-8f69-51767ee524cb)(label(Guy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 103c3bdc-1116-49c5-9853-f16a28fc9040)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ebd5399f-6ae3-4d6b-b78b-28bbc67e78d0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 fe6a114b-b3c3-4594-a0a1-f928285bd0db)(content(Comment\"#err: \
                 already used#\"))))(Secondary((id \
                 e0de24ef-c34b-4695-a45d-9ed066a319ff)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d61b4cbd-2f89-47c9-878a-e591823c92fd)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dd444ca8-546d-405a-b523-c4d9e8c4356f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 683e0651-17f9-4543-bcb2-5e0330b288bd)(label(BadCons))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 21d22b03-ed47-4d19-b641-74ecc6a30edb)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e3fd647c-4446-4b3c-9f2b-c4012dd45f3d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 51280c23-f305-43de-9f79-715070965a3b)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f66a4139-f454-4619-9796-85cefc2ce3c0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2333764-33d8-4a2a-ad7d-7a2bd17c16a1)(label(Um))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 22cd251d-4429-4c76-bff5-a84d30719e2a)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 020ff86e-32c0-4fa4-b517-f3fb04692224)(label(Unbound))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 10710ea0-c590-41a7-8995-89865e57eb33)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 bc14205e-2db6-439d-9467-c5f7d27f8e16)(content(Comment\"#err: \
                 unbound type var#\"))))(Secondary((id \
                 97210d32-6308-4c36-b1eb-3b19faa10bb9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 43f9f2f1-4e2a-4b6f-be2f-36eff112c892)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 a693e168-ea2a-4046-b331-44909fbd9dd5)(shape \
                 Convex)))(Secondary((id \
                 3bebc0c1-087d-49ab-9ecc-40b6cc67a48d)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 7a609e7f-8e59-448f-872f-34f6fba7ab57)(label(valid))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c89300e1-510b-4664-a061-9ea665af1c5c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c7c97659-3874-4a46-9c17-bd30dd1259c7)(content(Comment\"#err: \
                 invalid#\"))))(Secondary((id \
                 655d1416-e7f5-436b-8b71-1505249e4b9f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 331bab51-e7d6-4fc8-b196-89f4145ed056)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 52a9ae53-66b8-4812-89c5-2c55025dd265)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d630187-027a-4194-80d7-c93e9a1f42fe)(label(Bool))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 aafb4ded-3eff-41dd-a9c8-36c6d56a1328)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 2a408a1c-dab0-4d04-a87a-0dfe20a396b2)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 607c98db-9e2e-4cb4-9175-33e9b8fe2009)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 68506acc-c3ae-4c12-ae5f-ed8a65c67fb4)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 260e45ba-b3c8-4c2b-93e2-dac976be2106)(content(Whitespace\" \
                 \"))))(Tile((id \
                 50fcbe55-0837-4275-bdc4-25ca3b0eebaa)(label(Int))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8a4016d2-245e-42ab-bca5-bd0b052d500a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d72ee02b-a929-4cc0-bfaf-5c761f2be9e2)(label(Int))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 9ba7c66e-3196-4a5c-8e41-33b1c317cc7e)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b26e63bd-e82c-4bb9-a53f-71aa6c5ac2a5)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 cdede3f0-bef5-4348-bb29-dfbbd3b9e676)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e307b09-f6bc-4807-91f0-3f381719102a)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 11821834-9714-4234-8146-1bb366f7b8f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1b097b9f-bc4a-4aa7-a4fd-65b211a69c62)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Grout((id \
                 ad6ea394-5eb6-481f-8920-d320441a572b)(shape \
                 Convex)))(Secondary((id \
                 4611c108-9c5a-48e5-92b4-4846fccad7b9)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 baafd461-1a5c-4c67-a7a5-7b49520e6a12)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 579756e6-8361-4225-a7e3-8d926c234def)(label(Int))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 0c92606d-b3d1-46e6-8ac9-7156c2935666)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 3df6c1c1-1af6-4d8a-aea4-ac756640f4fe)(content(Comment\"#err: \
                 expected cons found type#\"))))(Secondary((id \
                 463813fa-f4bc-40bd-b528-4f040461f503)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8714da00-16a8-4dfa-a0c8-db81af581093)(label(+))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 5))(sort \
                 Exp))((shape(Concave 5))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 fd13c725-b57c-4046-a854-0fa5d1ecda8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a117afad-4c7c-493d-8cd4-472aa5fe4c6e)(label(A))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 145eef27-d48b-4975-9b2d-a8fc82e2b5d5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ca264f4b-c885-4a7c-8bb4-b49f21c53675)(label(Bool))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 25897273-9d3b-4082-9575-9d3637870ae5)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 fc3ecd39-4f4e-438c-b695-110e140d9723)(label(Int))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Grout((id \
                 c9db37ea-d7cf-4262-996a-1c363a5a6126)(shape \
                 Concave)))(Tile((id \
                 51491d9d-98ae-461c-8eb3-91a85444661a)(label(in))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 67fdd0c8-8a31-46f6-abb7-6bd5945b8017)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 28d8adaf-6b5e-49bb-aa28-5ef72e66f7fa)(content(Comment\"#err: \
                 expected cons found app#\"))))(Secondary((id \
                 256a8a2e-74e6-461b-b06d-ee9dff2c48b1)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2550e4bc-195a-4720-8906-4eb81c2df6ed)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 224eba49-bc50-482e-aae7-ed32589ffd8f)(content(Comment\"#sums \
                 in compound aliases dont add ctrs to \
                 scope#\"))))(Secondary((id \
                 d98602d3-6f0d-4d81-9304-3bfe7b16ad45)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 1a874592-c9e2-4f4c-9128-07e91b72253c)(content(Comment\"#but \
                 compound alias types should propagate \
                 analytically#\"))))(Secondary((id \
                 6fd618ce-d083-4d0f-a993-21bf38a223c3)(content(Whitespace\"\\226\\143\\142\"))))(Grout((id \
                 427d38e5-6992-48ca-a028-8d5ae9364915)(shape \
                 Concave)))(Tile((id \
                 ecbcb53e-4cc4-4b10-b224-c89053aa065b)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 18a1f912-0393-4442-b62f-7d631f976437)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79075a28-7967-4cf9-8991-40a65a4e7c79)(label(CompoundAlias))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 0b584ddb-cce0-46db-8f4d-cf54547237a6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 372e5eca-5b5a-492d-a2ce-b1d784c1ea0a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8500da0b-b7fe-4a8e-8194-a5a2a341fb0e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 f3be51af-66d1-415e-8755-6537dfa74659)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 291caf0d-f8cb-437e-bae0-14a476c89737)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 f6d52954-f9d8-4313-ac17-388837df6d7d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1ff8a153-a8f2-4124-81c7-dae8dbe149af)(label(Anonymous))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b7dde300-5388-43e4-b686-da82f88901e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 394f77d3-f266-4c15-8170-4269e7e53e0d)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8e530bb1-0794-41b9-8c96-08bd54d5c169)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ed21cf3-2065-44c3-a0a4-d56e4cb1c42e)(label(Sum))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 87342b2f-010f-420f-b435-f3c0824cb89f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 114b89cd-8e6a-41c1-83d9-f0952b63bda3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 d28590df-fa00-4517-a257-639b57f9061f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc9ab6e4-665b-44d7-a1b6-a3880eccbc28)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2d8bb07c-3782-4079-bc5a-bec3465bc55e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4532a81d-199d-4d3c-987b-8ccf78f0f2be)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 a43fa792-36b8-40d8-988d-33296d687863)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 fdf74110-7660-4d77-b2a2-d09ed8c80f9c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 62c302dc-c7ad-428d-9645-db56d952f22d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 017a5ad4-62f3-4c6f-b142-c8810b34f934)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1c3f5aaf-7c7f-4dbc-b6b7-db8f3999815b)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 662fa66e-d213-4435-8340-31abaff714bf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1e3281a7-2023-46f3-b135-59827e813c2b)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 a35cb243-b67e-4493-b45e-94df54f6fed9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 315f1a80-4ec5-4081-9444-cdd2bced4604)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 faa6fba4-675f-4129-8198-d9c34240c25c)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 2fafc2c1-2842-4fcc-b85c-0f74fb16054a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ef3a371e-4145-4f65-9516-2e8a27e04e90)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b8d477a6-4363-428b-a519-bc53aed8d32a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 546149ef-8661-4527-8814-4ac9cc50285e)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 efb4e8ec-123d-4621-8b96-a5905de88ce2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8a8a9c59-f973-4533-ab5c-8594eb9c89af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6f52f52-66c6-4669-a185-1c034d3da2fc)(label(CompoundAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 ebf17fca-9594-4842-8870-fadf94ee94a4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e73fc0c0-d6bd-4dba-9c5f-091e8c62ac7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b37fad58-2d22-431a-b04a-bb8dd85b42ce)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bcf1f084-5628-4ccf-85fc-d692806f14c1)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cd265c68-00ee-4b4e-8960-3e16ca73d59c)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 f9066f2e-8373-4522-ad42-cab367fff895)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c043599b-f43c-4412-828c-b84db9b7d0eb)(label(Sum))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 841676ae-a910-40d1-8d70-a0f48081ae45)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a1ef7912-59af-4501-8566-e00a7f081a0a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b0576134-bd2b-46cf-812c-bfa57169f0b1)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 a534c8b7-2e44-4c24-bd8c-49b1351f0147)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 b8bccf90-082a-4b52-8927-c2b90d1a533b)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 99c371aa-f7c8-47fb-a0b6-90d12aa8e55e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bf1eef54-1593-44f7-a470-8aca2dd07bdb)(label(Yorp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 f1916edd-f8b8-4d12-8874-f8bb2c95bda5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 da84132a-5515-4193-8b63-4af4b16847a6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 40bc8d14-d90a-452d-8d8e-74d22b03ea99)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 7438dd70-9736-47c3-b844-142197b66085)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4e8de8c-03b1-4d92-9117-1e0cf893b6d4)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 3d49f7e0-e654-435f-a3ae-86e6ed7cb4ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 162e419b-1262-4f5f-bac3-40122a346b3e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 abb96d64-c403-42cf-95e0-95d07164f88f)(label(Inside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 e56e7527-3fdd-45f2-bb07-6621810ed578)(content(Whitespace\" \
                 \"))))(Tile((id \
                 42a817b6-c1f7-438a-a6f7-1106c16dd276)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 53ab60f2-0582-433e-8a16-7ce2d3866793)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d7891ec-87fa-4f54-b6e3-b7be9be6bb40)(label(Ouside))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 d65816bb-910b-4512-8365-314d6cdc043c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9e14305b-50d1-44b4-b8b1-531aaef029cb)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 1e1c131a-eed0-400a-926c-48cddccdc786)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 fd034ddc-20ba-41fb-a32b-2c4fe9374ca2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 197b7f7d-19ad-437a-8f84-7fdce3cd26cb)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 882a12b0-09e5-4a01-ac48-dfac2cc4bb14)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5b2e6e67-3d36-48dc-a656-762bbaca46f5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4f071910-cdd4-4155-b4d8-cff307c48df6)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 4749a36d-73cb-4315-90b7-d4a689c87e23)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8a6a906-d504-4c66-a864-9822cc83acd8)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 251eb378-d52f-4f01-9275-ef54f752e6d1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ffb93107-b6cb-4405-a8a7-c34988f48382)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8577c7ed-0b74-419d-b2cc-8ca856b8728e)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 dcecc212-0d1c-4fd5-a7c0-cc6ce9c9a646)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 547d4bbf-8033-4904-a8ae-bc3b1aca0f0a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e9abb272-8c5c-4f08-9e5d-d0a6b2f3d78a)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 97a4bb75-6ba4-4a9b-8965-18d3f976ccd5)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 447e457b-6467-4a80-96ba-84f15cd78f57)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 164dfa3a-fad6-4e25-a11e-d89f4731cac4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 89ee0638-ea85-405a-a898-da26d64ea429)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 97e9c8e6-ef56-4e2e-805e-6f8d60a2357c)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a8d5f997-6ae3-4010-9b1f-0881aff1364f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 696b0910-8156-4c99-a0b7-01887319d4d6)(label(Yorp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 95f29af4-7f5a-4df4-8f2d-aaf6243a9311)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 5dbeef21-58dc-46e5-a81f-7fcf678a7c1e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe706460-c22e-4f33-b079-3f229268784e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c3d89284-09b1-4dc7-b628-a44ff8212716)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3efdf362-4cb0-45b3-a272-83effe4e9388)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c14d5db8-7538-4e34-88d2-96018ef7c525)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f8c88e13-4f97-465f-90ab-21971afcb71a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 368a9600-c404-4acf-b737-3d8b6b2cdd59)(label(Inside))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1c9af7a9-3cdf-46da-9e28-afff96cc1d30)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 0e963b08-f411-4b67-ab47-b6c2bdfa33fa)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e0920ae2-3116-4946-934e-559e3dd5fb95)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 df6ee1d6-0e3a-4331-a688-494e02d4903f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0481efe3-7d73-4b91-9aba-9159402b1203)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3c87062e-8863-4055-a8a9-ad079b5f14a2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 57a65a57-d69c-4709-bbe6-f66200e00552)(label(Gargs))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 b3fc7fb8-26e7-432b-b75b-46929caae083)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 277e9a10-a1b7-4489-b63b-8eb191ab5d69)(content(Whitespace\" \
                 \"))))(Tile((id 98dc24d0-6998-47a3-bbef-c9b1f95a4aee)(label([ \
                 ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort \
                 Typ))((shape Convex)(sort Typ))))))(shards(0 \
                 1))(children(((Tile((id \
                 22560e5f-14bd-42a9-bf36-71ff6fe3ea0b)(label(BigGuy))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 58cd73ea-024a-4e06-937d-63636d8e33c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bd15c9b5-20e7-4e03-b110-0a12dc812785)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a0d87150-4562-464d-9c73-50355d954ce4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfbf4516-984a-44ee-a5e5-e610067d8f78)(label(Small))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 0f31c2fa-e274-4c62-afad-0b2496e78e3b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5fd8a8a1-b4cf-44e7-92de-cd4540f97fe1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ecc053f2-5941-4299-a009-5991434712b8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9fd608e6-adaa-436d-b0b4-849a598c2050)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f0c1e6a-1559-4a84-81e2-a42857a5b4e4)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 698b12ff-c67f-41fe-b5dc-a9b8385f0de5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 34c23f04-7270-404a-b85a-79b5bb103632)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7bbc3a29-db19-4af1-b6ee-b9da83775e33)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7b31beb2-e827-4d8d-a1be-528b14a8bfde)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 00664bab-feef-4aec-9647-b860e76b35f7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a70026d5-200a-4725-952f-82c8d8c9caa3)(content(Comment\"#err: \
                 not defined#\"))))(Secondary((id \
                 47f7cbbf-1ad2-48a6-9550-5bc46d02d00a)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4bfc0b02-191d-4a95-ae7d-75910be65552)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 71af69c2-188c-444d-991d-f3461673fcc4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1234c7dd-2137-4a13-b262-15831c7e20bd)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 2586f461-04fc-47e8-86e7-16284a66e707)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 fe47ea5e-9579-4d0d-b54c-dad9316f68e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 59a2d286-77f6-46e3-80d9-24570103e134)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 37f6af6f-aa92-4f33-8428-a292d05452b3)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cb7d0dd4-a1bb-468a-a4f7-ce04603f9ccb)(content(Whitespace\" \
                 \"))))(Tile((id 0894cf2b-fa68-40ec-8605-5eca9a59a2fe)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 b7d81abc-ac9f-400c-ab09-6e1a96af2065)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 5d0255af-cc1a-4ad5-862e-9f27d9d5df1d)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2be9425c-dba6-4912-ac85-c263e6d07e51)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 209eb627-7718-4c3d-bb62-a1ebe38228e1)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 4697e975-882d-4491-9e6d-cfeafa3e32d3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 346dda03-e621-4d6a-a61e-8cb01f4db7a6)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 13102995-9075-4071-84bb-ee1a0d5f5590)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3c82c3d1-5299-4361-ada9-24525afb831c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f692bb7a-9643-4a25-bc2d-86b44206aabd)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cccfe634-ac48-4b23-ba6e-c1e19119cae5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f3ac5db5-84b5-4254-a23e-512eb7343c1c)(label(Gargs))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 063660ac-2dab-4942-bdf0-6d04992d4785)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 b13378d0-793a-4ff4-9123-9d2a31fb1d74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b99992ff-f6aa-4907-8251-ea55508fced9)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6d2624ff-c0aa-4b1b-81c6-5225da73cce8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d1c03e49-73c6-4072-b18c-b5bb4206f094)(label(::))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 6))(sort \
                 Exp))((shape(Concave 6))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 547ace79-2b48-4b82-be2b-99f1c0523298)(content(Whitespace\" \
                 \"))))(Tile((id ebbcaa9a-8c54-47ba-be5c-39edf41bb845)(label([ \
                 ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Tile((id \
                 af744ceb-f925-4647-b2c2-b01bc82358d8)(label(BigGuy))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3162ee57-e171-4c9f-a9cd-3b06afb9435f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f67e5ecb-a429-40a7-8bdb-8a42a6acf166)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 463eaf5e-2149-46ae-b92b-7de5a3496fd0)(content(Comment\"#no \
                 error#\"))))(Secondary((id \
                 4aeb2e56-6b51-4757-a1ee-0eda3e02760b)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 fa518201-e737-4d5a-a1ee-5023f4788f35)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 2123876a-a6f6-46b4-b85b-30b2b54cecff)(content(Comment\"#unbound \
                 tyvars treated as unknown-typehole#\"))))(Secondary((id \
                 d419fca2-f3ca-45ae-b6d8-6afeb88e629d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6b8a82e0-8bfa-4a37-9bfe-4f8c675ab666)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 dda54d0f-8fa9-44f1-9794-3850d6905171)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e9ce2514-9181-4ad2-867f-116eee042585)(label(a))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0a9df4b8-ba38-4c87-af40-0671900c9eff)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 2a9ba23a-2b80-4e7c-9dfc-ad597707ef1e)(label(Bad))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 10b4dafb-fd3f-4f2b-94bd-fe0298f70d78)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 1f522a3e-7b5a-40ac-8144-789e12fe7ed6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f4d8b1ce-9a50-44e7-a6b5-5ea79ac0d761)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e9212438-e6bc-41f4-bf7d-e5a1cf2a2821)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 76b90eeb-88c5-43a5-99c1-095b66d8f022)(content(Whitespace\" \
                 \"))))(Tile((id \
                 927a3152-662e-4d5a-8664-aef60f6e7090)(label(a))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a552255b-3fbd-4ee5-ac87-4f73489ea868)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe54925e-0ad0-488a-ae2f-32166646f701)(label(==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 6fc79236-19b9-45cc-97d5-1885410b7f95)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4d5b16a6-31a4-4f17-b9c8-b2d20d2cdd59)(label(0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 8f3afda2-2c5a-40ae-9afc-f27672d767f3)(label(\";\"))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 10))(sort \
                 Exp))((shape(Concave 10))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3e10f474-4748-4fe2-a66c-c17d62dd4e04)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 31a1357a-2059-4ee8-b5f5-6575cb160978)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 4e8afcff-02ce-416d-b9aa-125009a799db)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 a2dfb0ca-a40f-4c41-9741-82d628b60ab3)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 7ed8d885-bf12-41d1-86d3-212ba4d9804f)(content(Comment\"#non-sum-types \
                 cant be recursive#\"))))(Secondary((id \
                 29825c17-4a9f-4fd4-9610-1e534da7f5d3)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c6d49eaf-08d6-44c0-8c9a-fb137cafb79d)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d77d3d6c-10b3-4559-91ac-abc2e2128473)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e85f74e4-6e44-4e9d-ab67-e8be950177a2)(label(Lol))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 423ef3ef-8a72-46b1-a60e-4e73bc1431b2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 f3aedafb-c727-41ef-8d92-9b2702c73297)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6f3f969b-c322-48c1-8af4-968f63eac22f)(label(Lol))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 fee00064-d683-486e-b55d-e1ea4147737a)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8f2c5f9e-c62b-4512-8910-6cb3a85855ff)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 db19e590-ec79-4c71-a79f-07d9d74c2cef)(content(Comment\"#err: \
                 not bound#\"))))(Secondary((id \
                 1695b8ec-5532-4ef0-92e0-48d572ba6acc)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 e73ad04a-e1cb-4dc3-ba50-c0f2f3d71723)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 6cf3b33e-cff0-4f35-aea1-994d00144c6c)(content(Comment\"#no \
                 errors: analytic shadowing#\"))))(Secondary((id \
                 85f4ad77-0aa3-4795-b726-98b39fd73068)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3af95115-fb87-4bff-8d48-4a27cfe5e89f)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 902d2c1a-f53b-4b45-acec-5e1e5410640c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 24db3928-0592-41d4-990e-4ca143f4c8dc)(label(Tork1))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 cf52fe27-50ef-44d6-a912-5d7352528e67)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d629aee7-d331-4615-a6e9-e23da20cb667)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7d54ca4d-bda1-492e-bb39-e876ef63ab61)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 a47bd328-8cad-4cca-838e-63019c10d745)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 bcac7624-baa3-4134-b208-e1ec8a710969)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1f8cdf42-92f3-401a-a530-22d66e6e6bea)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 4f41ba02-b738-465a-a067-eeb39965ba51)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3cbfadf0-7b4f-44a1-b6c7-65314e955c05)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd43ec22-1a57-4b44-be3e-8227b9e47e1f)(label(Tork2))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 4dc90c7c-fc0f-4d9f-83d0-2e5ad1acaf7a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3f6413e6-95d6-4796-914d-5b4b8e91df46)(content(Whitespace\" \
                 \"))))(Tile((id \
                 00bd721a-0254-4c74-81a7-8c674e028233)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 339a96a3-febe-44a1-9bfd-46a3184d5b39)(label(Blob))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 07ab48f2-2f9d-4284-8919-a7472833f3c9)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d5bb06c1-eb6c-498f-961d-563e595a2bf1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ca670333-6868-413d-8095-71975e0e4b4f)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 08e0b09a-5279-4d53-8e4c-0fb38a300e66)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 85f6e1d1-a5d5-412e-829e-16a4a23d17a3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4dadd949-3382-41d0-97be-86ef706528e2)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 39590b94-15fd-4dca-ae0a-c835d5f5801e)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Tile((id \
                 f1199285-afef-41bc-ae0a-420bef02fbdc)(label(Tork1))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 02aaa03b-1f42-4454-96d7-1fd11c51822b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cd14e7a9-91db-435f-a477-e1632f8f10f2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fe3e34a1-e37e-4e25-8b9f-accd969efc1b)(label(Blob))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 57f93e8b-103f-40df-b06d-77fd6140b4fd)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 af7296d6-8632-4f14-98f2-e57554a9cc68)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 89164e58-585e-4335-a47c-5fdec254d1ed)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 994ac747-702b-496b-a16f-b38a47f8b956)(content(Comment\"#exp \
                 tests: happy#\"))))(Secondary((id \
                 19180832-a634-483b-a9f3-e8c07781ecd7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2dce8006-e684-4f7c-911b-971e23c9059f)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 39e4eca4-8b49-4fd2-a94e-5494dab2314f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 90ddde07-a15a-4b0c-87a2-07f77bfd53eb)(label(YoDawg))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 ffa4516e-a6d9-43c7-8d0e-7ec6448992d2)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cd97ed06-4bb7-456d-bfe4-af25a374b2d1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 210213b9-39ce-4a78-92d9-cef287a4ca2b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf983397-70dd-4c9b-bddf-02fca3bff1a4)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 317b5e9d-b490-4d97-8e6d-c88582947d7d)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 538a1bcc-fea1-4f58-bab6-332f63662c4c)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 8c65faa8-a4af-49e5-9a2e-5466806cdf27)(content(Whitespace\" \
                 \"))))(Tile((id \
                 31dfaf77-67bd-4f2f-b15a-6c7c8c45a88e)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 640e7ac5-7da6-4ad9-9608-1cd8d81ac9ea)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5f1eccb1-48cb-4d1c-aefa-d2b657e0416f)(label(Bo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 43741549-c0ee-4696-bab8-c1def1b8f85c)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 72215c96-fd39-42be-b3be-7ea4d65aa6f4)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children()))))))))(Tile((id \
                 fbb82682-02c3-443f-91fe-3e4ff3ea1d68)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 eb224c8f-f0c8-4ad9-8fcc-d88b480bcefe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5646e6f0-ef14-49a4-aea4-8e17479a35d5)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 b36d68f6-e01d-4957-b00e-179030b321fb)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 a74af42f-26aa-4df5-81fd-b3a94b16e68a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 af43bb01-c59d-47e3-a0fc-f861a007a0b1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ccd2e292-7084-41f9-aed3-dc756d7bfbc8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8bb79bd3-4cb7-4c5d-9f16-7f09470b8076)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 2903900b-e202-47c6-9bcf-38a13d2a54e0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0375efa8-8e9c-4015-88d7-cced95022d5c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c3e7fbb9-bd77-48de-b451-61562b582d6f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4a9a44dc-5921-4ab1-a206-6978c920336f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afb4891f-127f-4061-8d47-c7fa9e6d49c0)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 c12c57e0-9db7-4080-b8c1-52456166527f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 305fdc69-5cef-4f8c-b906-ab3914e5717a)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 c6814cb4-8297-42de-916b-83c2ea7345c2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5768f353-f017-4e4f-83d4-932e40471a4d)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 447a1057-8943-4ce2-b2bc-497c41a83901)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c6ad34e7-59c0-45fb-9ce9-bd8a149c5c62)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a18e3f92-f108-4f0e-8b55-c100a51503f3)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 7acc6455-b658-4f50-88b5-ecdb848fcb4b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 367a835d-438c-4023-8a54-5a5d51616307)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 08df7f97-0a86-468e-a186-1ffb558e62ca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5c7cd3a3-37e9-49b6-bc5f-822e9e3c4cc6)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 dee2f158-d02c-428a-ad84-18f59be021ea)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 467f895c-9944-4acf-80df-b311af87f856)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ef52f593-ce3a-4020-a4b5-13a593d5df0a)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 48814ec8-5160-4605-b3cf-0530cef63fd3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 64cc4922-0007-44cf-8592-e6df254bb10c)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 f1e910a9-9c02-453a-9b5e-2affff4c9dd0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 917ef49f-aceb-49b4-9d17-6180e5c239e6)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6e8b1c9f-5441-49f9-afce-43abe0478d07)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 411574a7-b800-4791-a3f8-8ff1597f2980)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9851eba1-441e-4106-a8e5-12baa7ccb1af)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 929d9832-bc1d-4a86-91fd-f28357569e24)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d0fae5ea-2bd9-4d94-ac9d-194e257a707d)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 cc8bfc06-207e-4d1e-9ded-a29a99e0ef22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 63f5fad3-1fe3-49f7-8aa1-03288998e20e)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ab386589-0ef0-4f03-b21d-ebbe872113bf)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 93758c8c-8d74-4be1-bf48-f332cb7d036e)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 ca2522cd-0cf4-4ed6-9e98-80eba70d7d8a)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 17723f70-cfd0-4daf-9e4f-03a15d7639c5)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3a8ba98d-1219-46d7-9fc2-f4583ee98bf7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adea7e2a-93c1-40d8-b8bc-a1f1a4b2195b)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cbd25946-53ec-4bc3-a05f-103046b69a94)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 81855c09-d7f4-4ab4-b7c9-fc91198087fe)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 e40156ed-929e-4507-a1b2-56b1d2a077e2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9d783e8f-ab9b-41b5-b1cb-c90cfa45adb1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 7caaac70-41f0-46c5-9cc8-fe11fb67bb90)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 a22efaad-b218-42ee-a8d8-6ba098ef4b97)(content(Whitespace\" \
                 \"))))(Tile((id \
                 de12991d-d09e-4781-a966-602372bfb04c)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 58ec2ae4-687c-4098-bc78-0e850a9cd9f8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e9482714-4b80-47b4-81fb-8848602b2ff7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 a0d5f9df-2846-4fc2-800f-431e6528f9ec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 87589944-8e50-457d-ac9d-e1166a43d2c3)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 95dbabc2-59b5-4bf5-a165-09b99e5ca39c)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 1a537e82-cf52-4775-b7df-a455838b9def)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79ae2e27-fc36-4ece-8c86-8ff4dec73fda)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 dff41df5-5192-4d22-8505-a413c4a198e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9d63539-656e-478d-8e9f-883f62c74aec)(label(Dawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8973e0af-0df4-4299-84b5-c9a957ab8c6d)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c12c2697-83b4-45e6-90bc-28ce4b0f1cf3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a1983329-2324-4702-86a6-877c22be0902)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 5fc98bae-a0da-45d6-b668-888774e89e48)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 0a2afb0a-b883-4956-90de-e0d20c50cab2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f911cb12-d8b3-408f-918d-0706c11e77e4)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bf0f106d-7995-4873-b0f7-40735f8adfc2)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 63849b4e-8d30-4b06-ba3e-f740c9081108)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 f566acea-c9b8-4caf-8e2d-fa1132265fd3)(label(5))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 7e1d1e0c-5546-4e54-84bb-87641ebdbb9b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 86b47d7a-c6b7-4f23-916d-cd76b88405cf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d5f2170b-5783-4377-a8fc-f15f0086c34c)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5af19164-839c-4f47-bd90-35e9c48b5e66)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd6e05eb-4d7c-4bdc-8546-76f09cb54925)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 132151e1-0235-4722-a8b9-70e19159e4e4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 45bd1087-66bf-4c1e-ade6-fbe7eccec3e7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 77572944-9f31-4808-bed0-1c51659b584c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cdc3c6d8-ba3c-4b50-8265-63791777f036)(label(DoubleAlias))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 6aed203f-3f64-4326-9231-73ee5ec3dcb9)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a1834eb7-dca2-4ef1-bd51-3d2ba3565241)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dcdccfaf-23f9-4ee0-af13-db9d81a0af51)(label(C))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7a85ce70-cb12-49b9-9ca6-fd1a9bc16d1f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 005fce44-15ad-4a7b-85d3-4e75016fb1f9)(label(4))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 bcb7d96e-9371-42b6-a9ab-13327c6ef5c4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 77a737c6-49aa-4ad4-a14b-98fb223d309f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5d5e29dd-9826-4b57-afd3-b850d941b697)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 77f64c07-d9fc-4e23-ad54-4e379ce13321)(content(Comment\"#exp \
                 tests: errors#\"))))(Secondary((id \
                 21d18ecb-e3a0-4e04-ac38-c8b1195bf185)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b696714-a754-4c30-8a74-1d4b82fb4166)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 7fd65bbc-c37c-4636-bead-54da6fa97f7b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7466ea1f-00c5-43e9-bf0a-a8fa7bb0cfa4)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 00e6ae03-2922-4f1b-8051-1fae18885d60)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e2ea3202-bee9-4134-9bbe-dbd2a9cd5156)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c58f442c-c777-4b8c-8ff9-4e86a47990e2)(label(2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7ff1c8ff-8274-4168-97d4-fabb54f7a545)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e2aeb2ac-be00-41e8-9a38-fda9e5053934)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4248e879-0fdf-4421-902d-139a49d47df8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b1fb64b9-8c5a-44fb-a141-9cbeb9315b8c)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 9f413f71-65b8-4821-8a41-b150477c1552)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 f23536c2-666f-4afe-8a79-a97d6b078008)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 e31ca670-df2c-4897-b06a-b050153416fa)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b2faec9f-6a39-47c3-9a4f-2609e31ea774)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9bb21324-0343-499b-ab25-f2526c1fe082)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 bef45e68-7839-4e67-8250-439bb4862eab)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 16da7dd5-2851-4078-9620-dc88fac339a0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 653925a2-3973-483c-9b7b-78d9179f7a67)(label(Undefined))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ecaaece5-f486-44e5-9b29-d6d94eba460c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 35358d17-64bc-4336-bb88-5482b6817abf)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 656f791a-ae16-4725-91d8-af3cb86ee329)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d4f0f6ff-82b3-4e0e-854b-2ea54cfb3d46)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 aa7830be-c939-4efa-97f5-055de4753a03)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 0c6a7d18-2a75-4737-a453-bffdc3011a97)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 52603fe1-8574-4953-9e16-511e29780c07)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 aa035928-db8e-454b-96aa-3e4ca1ce2952)(content(Whitespace\" \
                 \"))))(Tile((id \
                 18849081-ce72-4c0f-86c4-1cd657e4d71f)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 01abf6a0-537f-4bd0-8f89-4bafd1fc8f8c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2a1f2282-f6e7-4ced-b02c-c10606bdab47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8e328fd3-bb57-49d5-9062-3e65b0715d90)(label(B))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b41de1fc-678b-49cc-9d96-e124b0878f73)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e3327b4a-9a10-4148-aa70-83a9bdf8a6a6)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 30849d92-fceb-4ce3-bd26-88258bbf0bcf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3d27f9bd-45f4-4061-b1e2-e5080c5f4a93)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1b3ccdcf-0e9d-470f-b305-8ebb38b117a0)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 8fee5658-d91b-4985-9891-50090125476c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 91c7e7ef-be92-48c2-9194-60c0ec2e4ce8)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6f231831-f241-4fdd-982b-df14a6701e3c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88133492-9833-4b29-a7df-65978fbc2d32)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 0b54d2a3-253a-48ec-ae30-d2dd69eaa127)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5129ba63-c213-4863-83ba-2b1c6332e5cc)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 d391d590-acf3-4482-9502-6f21067a9893)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cfa42531-916c-4418-80e0-a36e674acfde)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 05304934-832c-4a25-91a4-c2ea7c99bebc)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 94717ab7-04de-4f03-9b42-5529a04c7066)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 0eec0f68-a89b-4662-a5c2-b028b3e12421)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 9d329cac-0667-4157-a7b5-c0ebabf18759)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 394a2afd-a6e1-4fcb-868e-28e839e14b8a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bda7ad58-37b9-4126-9805-6d5c68fac15a)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 bf51ace2-37c7-4e1b-8fd6-d9a0404ebd1f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7cc05791-952a-4bbb-bf9b-7891da23c696)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c9a6c63e-5850-4d8c-b942-0c4ca4752e60)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 41f09832-e54f-407d-bc8a-6bd76398bdf0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 000ba7eb-a5f6-4416-ad95-359cfad07655)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 cb321105-1a2b-4804-8d0d-ab4b02933d70)(content(Whitespace\" \
                 \"))))(Tile((id \
                 69dbedce-1d1a-4cbc-9658-c8e1463e7cc9)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 56fd6fe1-d487-4877-93e7-ee4109f27e79)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0c91ac3b-64d3-46a9-ac74-754c02bb9022)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 25679fa7-32c9-4923-9ef6-535d9ac6cba0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0cc4b205-3b12-4588-995a-8041e55820dc)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 03450a96-deee-4177-b42b-14156a9fe57e)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 a0eb717a-7bee-44ff-9b37-948eb3e4e12a)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 860ecbe3-f909-47ba-86c2-1249a43380c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b716af4-de22-4070-bdff-3cddb50df70e)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 2baa2e15-7a34-494a-bbfd-c9e5e7b4afbf)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8aced0d3-2103-4255-944b-7e6b29b056b2)(label(\"\\\"lol\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 3713fc56-3978-4600-94f7-fd6ffdcdeab6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 3f0eaeba-d75a-4538-8ae8-2c79507bf7f5)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 30ec3f0e-bc5d-4d7d-9d9e-343ab2a221c8)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 693baeb3-dec8-4a9a-8fea-0165c0175fae)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 a7735d36-76fc-4843-ab34-f2ca204db05e)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 07ac4077-b335-4301-b2d4-f5ed09a9637c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 afd02093-df63-42f2-bc72-a49d942fc689)(label(_))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 c896bef8-5d52-4e1f-bdf4-ec22d1265785)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2ef4d153-440b-495d-9b61-1e5a3e0f74c2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8560b636-04d1-4d57-ae8d-7f33096d0c6f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2a0b4be-f9b1-4a4e-ae04-d828b328fe31)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 668d1e0f-ec3c-4ad8-9208-ba6ca691ba0e)(label(One))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 87f5e546-9b76-4b02-b0b2-09fc04383825)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 cd456630-e72e-45a4-bbc6-447ff63e9e0c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d303a414-0d13-45c3-b04a-2ad1803bd923)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 633932ab-232e-4d99-94af-1c526d3837eb)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 84eb73b6-7bfa-4808-a33f-9bffe6fd47e0)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 ef5e57a1-50a0-4a3a-9d0f-c741380ac038)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f513c630-13f3-4d3a-965f-1a22569453f9)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a9d60b3a-593a-4deb-bbce-8ae2d0af2aba)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 a0ad709f-132f-49e0-9477-e4586d4d520c)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 60edb6d3-20a6-4d8f-b498-9372f53188ba)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 5a7de217-87f6-42f6-9bed-90bd51f8c842)(content(Comment\"#pat \
                 tests: happy (but refutable patterns so \
                 weird)#\"))))(Secondary((id \
                 e9c1353b-100c-43a0-a28a-8e579b93e53c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 62a3758b-4028-4f4a-9511-4504531a8233)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 619809d7-e1dc-4319-94f6-31eed0db3f07)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9cb02852-47ce-478b-8a76-e9836445b04b)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 99bcadba-3054-43f5-a77b-2805517e7333)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 de3b3052-2486-4076-ae2a-44ef8592ed99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 eab0fc18-844d-4758-b61b-db0ad14533ff)(label(Bo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 64125a37-9753-4058-99ba-b8aed1750bd1)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 df4148b9-ef7f-4612-a451-b8d32b1eb9df)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 e7bfcbb4-5241-4159-a98f-f65f7c23b393)(content(Comment\"#kind \
                 of a weird edge#\"))))(Secondary((id \
                 ef031a15-eaff-4116-a185-253047c433cf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 ea26db40-715c-4810-a16a-0276e02c96e4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 217c7d56-24bd-4ecd-923c-d5977299d2ef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a4a86722-8e60-4782-9366-e7ac3082d1ce)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 f3cf5d99-0b0d-4e41-9068-2e183847dc17)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 399d0c59-b0c9-4f7b-a756-aeb1c52e00d8)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 fb09667b-09d7-40cc-8193-147c4e983491)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 83152726-eea7-402b-bc1c-131100692462)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ae80c923-4a63-4398-9735-7875d89c4b11)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5b14478a-6ca4-474e-ad6a-065eeb74acfd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 3ab7d885-544a-439e-a8eb-99f5a8301f65)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 da5ab3a9-ba09-4e97-a23a-13c09a073e8b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 79095cc7-e6c6-4b9f-b1d1-8f2779f773cf)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 0b58ca60-addb-4f87-b999-e566b00ab655)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0131d3bc-8dc5-4d8f-b496-c02d480f7b5b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d70fa42d-ac4a-454c-b157-eda0ae6d990c)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8c1d9c05-81e5-4904-953a-5972f30f40c0)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 1ac0fbb0-1d19-47d0-a9c3-16c9d0b03a3b)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 06c079a5-7914-40ce-a058-b72014f1adfe)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 31364e19-e7c8-4d5e-9852-86e0340a84a4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 74415452-f29f-4fa7-98d1-98f3acb534a5)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 c27aa909-decb-4d3e-a144-0a3398755ab0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c513b896-be9e-4083-bba3-bacf82170ae0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9239fe0c-0eb2-4503-8fc9-cf9d04770ef3)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 26373d03-d84b-4e14-8b49-b05e82386c3c)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 2d746dae-b735-4b21-9feb-118a1c360b53)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 8ea98bc3-9f00-45dc-846e-845753149003)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f40afd19-8b50-4dae-b3cb-d7b8bfd35e65)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 71138984-e567-4bb9-9f18-108f85c35f82)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 142f46fb-a252-46de-9f36-e5899f194cfb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f317032b-a348-4311-9023-95f6a738441c)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d9893b9e-d506-403a-a691-34d17a69bd1d)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 4efc05be-5a4f-45fd-b2f3-fbd934dd407f)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 387b72b8-8646-493b-94e9-a23a741e913a)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6ea3b27a-caf4-4c07-b757-72906ed3bde4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 86fd2c75-3c0f-47c2-bac6-325c5908e72d)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e0f934a3-c230-45b2-89d8-16412b648cbf)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 37bf6c74-997c-4b2f-ad7a-29f97564c6a3)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 782825aa-c187-4095-9556-9f6fd6969ea1)(label(Int))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 4589217a-e933-4087-a2ea-90a3813541fc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 2092a7f8-14de-4fa1-b13e-7b7b637bf457)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5ad3364b-eb3b-4265-9514-094acd8d4354)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0f5c7b07-1628-43c7-aab1-988e0eac974d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 27498cf8-4c12-459a-95a8-4f34f85f7c02)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 298ad3b7-2ad4-46e9-a40a-0762def52086)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 1d80a33e-7a52-4c03-a0b0-670b030b5ecf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 a9dde5ca-36fb-4176-8d22-2eba84597723)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 d67d126c-516c-4dfa-b55d-61fe25d05d68)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1059417a-eab8-4339-a72c-bfd7f137104a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c76dc73a-bc91-4108-b7d3-469d2fdcbc20)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3cf5160f-1c53-47e7-9e16-38e168be09a9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 541b90c4-1258-4566-b138-872d8ac3cbda)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fd5e0de-6dbe-4e5f-bdef-2ca809500c42)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 8016e0c0-43a9-4192-a320-b7efbc8da15a)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 f8a9a4af-e668-4d71-9aea-84861334e6bf)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a02c255b-066f-4127-87fc-48446cd6ff1a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46b01776-5b7a-4649-8409-43d4291aa373)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f8d5a16a-2f00-4be6-942c-0759a2fdf2b2)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f54eb928-47b3-467d-83ef-b2da76491214)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 30dfdd32-feab-46b9-b2b8-2fd51da685b7)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 c585661f-7da2-47b0-9a37-ab9cca3d27a3)(content(Comment\"#pat \
                 tests: errors#\"))))(Secondary((id \
                 fe5da16a-1e02-40e4-9d63-17a72bdb4234)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 41128f6e-dcbf-4629-a05c-8de953804073)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 af701261-e474-4c2c-a0c2-af1f5809eaaf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 29d26934-0490-45e3-84dc-0ffdf38854fe)(label(2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 e96d8b90-1e05-4d2f-9018-e28692c44ec8)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 cacf2e3b-a49e-4ded-9b5d-f02fd189a247)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 a35eec21-b33b-4028-8161-0cae293b1b50)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4a4d2bac-9664-4ac1-a202-a4a0e6d4691e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fda9fbcb-f3bc-4256-900f-5d04bda1e4b6)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b7d83d8d-11ba-4dbb-941a-6dddfb2f7f96)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 866099af-d99d-4ebe-8ded-ba92818f4aea)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 52d7401f-46c1-46d6-8515-f796ccb5e4ff)(content(Comment\"#err: \
                 incons with arrow#\"))))(Secondary((id \
                 dcf01a15-f734-466c-be9f-6d9687fb5c60)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 07d41ee2-033b-46bc-8d50-84a4776d3030)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 10ba830e-e3fb-42ed-add5-1468f2b4a4c3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4bf188e-f4d4-4d24-801b-c492992c97fb)(label(NotDefined))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 76704c04-9e76-4bb5-8cc3-01ea911f250c)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 6b57b06e-719b-4275-ae2f-4a776e875a0c)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 859f458d-0f19-4c8c-ae42-8ab4c435d463)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6e7a63da-c6e3-4061-a8dd-ac37f5ba19a5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0f57d40-2ae8-4352-994a-99a67c2ecac3)(label(3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 016b563e-a393-4d44-9181-d21d767508ee)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f90f0395-879c-46f1-94ce-4eb182d296e6)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 0a1760eb-5f79-4ae7-be35-a22d82a48c46)(content(Comment\"#err: \
                 cons undefined#\"))))(Secondary((id \
                 f172fa50-8207-4502-a0e9-f0b3cd69d3d0)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 641c9b4c-087f-4b2e-b2bb-9c39c43d15a2)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b2f23c9b-2faa-4a3c-a6ca-11febbe416ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9fdc2ee-a2fb-4f13-81e0-a855e9c9a931)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 22263475-9c08-468e-99f7-cde3bb84541b)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 ec7a4fab-4c32-44be-b4fc-09b188338cb0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b33dd23a-60f1-43a4-bd9f-595292222ed2)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 3e395734-e6fa-4104-a2c2-8b60ace62b56)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b13a7183-820f-4398-bea9-c0e6caffd585)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c59ffa25-7e4a-450e-a51f-e8f1d69900dd)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 0ea152f0-d71d-4113-b514-b88b30d92801)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2ae8f848-0c03-45b0-87d7-93767f95e179)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 d25241ef-7423-4772-a336-88f916fbe430)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5723df79-d19e-42f6-9507-b8b1c1d8e5b7)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 95b5d57d-20d4-4172-b4e2-edebc1b2fe96)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 97049e56-fe8a-429c-a913-d90a534a6067)(label(true))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 b0ee6e0f-3790-4618-98a2-feda11b1cc92)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 a959dd80-1fa9-4cc6-bc8e-a0c72e1bbe54)(content(Whitespace\" \
                 \"))))(Tile((id \
                 645e4982-5833-4875-a7ea-50c2d9ab4d03)(label(Dawg))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 38dc9207-aac1-47f9-ba64-7875373dd87a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 c1e8ac4f-4537-412f-8d7e-b865f25eb097)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 5e62537e-be41-4179-8b5b-0b29b606d128)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d853dc1b-8b74-474f-afce-ac51d13af196)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7cd0e187-67f2-4049-9ea5-66cd2a6de0cb)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 f92a1ee1-0d4a-48df-b43f-47cfba949b90)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 251c7058-5829-4a0b-95f3-0e8b1fd5f93f)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 48b1178c-5127-41fb-b40b-39d1416528f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adabea27-eac5-4545-991e-b1c9b5cc45f1)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 fba68b87-33e0-4825-9197-e271258f7ce4)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 70aee4a3-ae7b-4894-b640-5a8ecbde546f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d9afb1e4-9e7c-4e72-8874-d1f4127618d0)(label(YoDawg))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 4fd7c99e-b6d3-46ae-8f53-3d25a42c79dc)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4cfac449-a744-44dd-b175-85e1e759242c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5678d6b9-1b44-42a9-ad13-2d6aacc7341c)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 0a9fc60b-c1ea-4208-83dc-004825bce198)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 0e902fd6-fa69-4288-a792-f27efcc2a5c3)(label(1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 eefd10d1-322d-4b58-b777-54ebce49f088)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e8d042d9-988a-4199-8a9f-69f08d52fc96)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b7e9a629-6f43-4532-b485-e2cdb33791b1)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 579f0aea-1271-4f7f-9fd9-d0daa3d69909)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 dc787afb-e253-463b-a1bd-7cdcb29b116a)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 77c94bd7-a72e-4f93-92e2-2bd4d6c79ef7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 46bd36b1-b843-4496-bc7c-04b83c7a2ce3)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 83536cb8-2ff7-47eb-80e0-e659c2b25c00)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 56de49ff-e54d-47d5-8b27-fbaeedc79147)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 85ad05fe-5202-4b36-b262-1716515788c9)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 c21b1a99-bcb9-4608-a20a-d7077e37bdd1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2445c10d-bdfe-4fa6-98a8-a1de34d7436b)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 6a0a182d-9f93-40a0-bc15-92d4c1e2afd4)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 8b332b08-8087-4b4b-bfa0-8ccb55c4fe33)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 41f53fcc-32a0-47f2-b292-435240ea63eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a865ae8f-6339-4bcf-8c36-22a3866b9f3f)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 c89f923a-bce9-460f-ad3f-d4c50fec1f96)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 64e631bd-a0ed-4f51-aac6-e31df15eabe1)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 6f1313b0-bedf-46a9-ab22-e8a68092ef8e)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 71ab5073-30d5-4253-8b74-11a38887a33e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 950896cc-ee6c-403e-a744-b145c9670022)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b877ab76-4fb7-47ac-830f-83fa91e14429)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9549a8d8-1639-4d70-991f-543868b68eb9)(label(Yo))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 3aa612d3-0e16-4645-95a0-e37fbc672fdf)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 433b91f2-f591-44f7-94bb-d9f9d96e5215)(label(1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children()))))))))(Tile((id \
                 61c6a7c6-5f7c-4377-b01c-52d8935112d5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6e96e3d1-addc-4c95-8807-09d81e3fc6b8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc156c90-162b-426f-b02f-748ea644d272)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave \
                 10))(sort Typ))))))(shards(0))(children())))(Tile((id \
                 e964d3a4-0b64-4962-a32e-ec8afd193619)(label(Yo))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 97dd6cc7-8a66-4ae6-b112-952bf1abf924)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 36a6b941-ec8e-4539-892b-4b54aefd673e)(label(Bool))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 cd117244-0138-4c85-b77c-55061fb4af54)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8a84368b-c4bb-4af7-90f2-2fc6212d6c2e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 adc6198a-2834-4247-9807-c2890e2d3610)(label(Yo))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9e03edbe-e23c-4453-84ec-f8b88f8ac396)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 b4adf11b-0dec-499b-a607-402011ca5242)(label(true))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 02115aab-5e41-46b3-b463-081292198c3b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 def5cf90-cc74-4fe1-8984-b3a7a4ed8ee3)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 787a0333-ab2f-46ae-9010-c4537e7fae90)(content(Comment\"#err: \
                 type incons#\"))))(Secondary((id \
                 760c92b5-714a-45c7-8071-49c290c2acf9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 3e70b905-1b69-4563-ad5e-e49c85fd87d2)(label(\"\\\"Thats all, \
                 folks\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 712da874-fd1b-4a7a-a7a8-5115ce1ddecf)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "#Non-recursive sum/alias tests#\n\
                 #all lines with trailing err comment should have 1 error#\n\
                 #no other lines should have errors#\n\n\
                 #type definitions: no errors#\n\
                 type    =    in\n\
                 type SingleNull = +One in\n\
                 type Single = +F(Int) in\n\
                 type GoodSum = A + B + C(Int) in\n\
                 type Partial = Ok(  ) +    in\n\
                 type DoubleAlias = GoodSum in\n\
                 type VerticalLeading =\n\
                 + A\n\
                 + B(GoodSum)\n\
                 + C(Bool->Bool)  \n\
                 in\n\n\
                 #incorrect or incomplete type definitions#\n\
                 type badTypeName =    in #err: invalid type name#\n\
                 type (  ,   ) =    in #err: invalid type name#\n\
                 type    = badTypeToken in #err: invalid type token#\n\
                 type NotASum = NotInSum(Bool) in #err: cons not in sum#\n\
                 type Bool =     in #err: shadows base type#\n\
                 type Dupes =\n\
                 + Guy(Bool) #no err#\n\
                 + Guy(Int) #err: already used#\n\
                 + Guy in #err: already used#\n\
                 type BadCons =\n\
                 + Um(Unbound) #err: unbound type var#\n\
                 +  invalid #err: invalid#\n\
                 + Bool #err: expected cons found type#\n\
                 + Int(Int) #err: expected cons found type#\n\
                 + (  )(Int) #err: expected cons found type#\n\
                 + A(Bool)(Int) in #err: expected cons found app#\n\n\
                 #sums in compound aliases dont add ctrs to scope#\n\
                 #but compound alias types should propagate analytically#\n\
                \ type CompoundAlias = (Int, Anonymous + Sum) in \n\
                 let _ = (1, Sum) in #err: not defined#\n\
                 let _: CompoundAlias = (1, Sum) in #no error#\n\
                 type Yorp = Int -> (Inside + Ouside) in\n\
                 let _ = fun _ -> Inside in #err: not defined#\n\
                 let _: Yorp = fun _ -> Inside in #no error#\n\
                 type Gargs = [BigGuy + Small] in\n\
                 let _ = BigGuy in #err: not defined#\n\
                 let _: Gargs = [BigGuy] in #no error#\n\
                 let _: Gargs = BigGuy :: [BigGuy] in #no error#\n\n\
                 #unbound tyvars treated as unknown-typehole#\n\
                 let a:Bad = 0 in a == 0; #err: not bound#\n\n\
                 #non-sum-types cant be recursive#\n\
                 type Lol = Lol in #err: not bound#\n\n\
                 #no errors: analytic shadowing#\n\
                 type Tork1 = +Blob in\n\
                 type Tork2 = +Blob in \n\
                 let x:Tork1 = Blob in\n\n\
                 #exp tests: happy#\n\
                 type YoDawg =  Yo(Int) + Bo(Int)+ Dawg(Bool) in\n\
                 let _ = Yo(1) in\n\
                 let _ : YoDawg = Yo(2) in\n\
                 let _ : +Yo(Bool) = Yo(true) in\n\
                 let _ : (Yo + Dawg, Int) = (Dawg,5) in\n\
                 let _ : DoubleAlias = C(4) in\n\n\
                 #exp tests: errors#\n\
                 let _ = 2(1) in #err: incons with arrow#\n\
                 let _ = Undefined(1) in #err: cons undefined#\n\
                 let _ = B(\"lol\") in #err: type incons#\n\
                 let _ : +Yo(Bool) = Yo in #err: type incons#\n\
                 let _ : +Yo = Yo(\"lol\") in #err: type incons#\n\
                 let _ : +One = Yo(1) in #err: type incons#\n\n\
                 #pat tests: happy (but refutable patterns so weird)#\n\
                 let Yo = Bo in #kind of a weird edge#\n\
                 let Yo(1) = Dawg(true) in\n\
                 let Yo(1): YoDawg = Yo(1) in\n\
                 let Yo(1): +Yo(Int) = Yo(1) in \n\
                 let Yo: +Yo = Yo in\n\n\
                 #pat tests: errors#\n\
                 let 2(1) = 3 in #err: incons with arrow#\n\
                 let NotDefined(1) = 3 in #err: cons undefined#\n\
                 let Yo = Dawg in #err: type incons#\n\
                 let Yo(true) = Dawg(true) in #err: type incons#\n\
                 let Yo: YoDawg = Yo(1) in #err: type incons#\n\
                 let Yo(1): +Yo = Yo in #err: type incons#\n\
                 let Yo(1): +Yo(Bool) = Yo(true) in #err: type incons#\n\
                 \"Thats all, folks\"\n";
            } );
          ( "ADT Dynamics",
            {
              zipper =
                "((selection((focus \
                 Left)(content())))(backpack())(relatives((siblings(((Secondary((id \
                 dfce056b-bfde-4b70-a8e0-4dd6ccc94a8d)(content(Comment\"#recursive \
                 sum type dynamics tests#\"))))(Secondary((id \
                 8afd322e-af0c-453f-8ec6-ed4140abd47f)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 8b64f65f-9a8e-48cd-81d7-31950bcdcc8d)(content(Comment\"#all \
                 calls should evaluate fully with no exns or cast \
                 fails#\"))))(Secondary((id \
                 4558faca-9365-4c35-be29-73295d07f985)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 59481d6d-6ee9-4e2d-8077-1fa849141531)(label(type = \
                 in))(mold((out Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f3a2df91-39ed-4120-8554-ed90af83e920)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2ecf738-8703-406a-b520-bc369a535124)(label(Exp))(mold((out \
                 TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape \
                 Convex)(sort \
                 TPat))))))(shards(0))(children())))(Secondary((id \
                 a8c60cfb-3a82-4866-9033-e67f09fa86a6)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 3b6b50a7-b128-48ee-b691-cc34a92a31e7)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68c41941-b58a-4325-98db-3d6f7ae7285f)(label(Var))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 ce42b158-00db-4a19-99ed-f50de8cdf9c9)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e95b77e1-0331-4347-9756-b2de25f9dc43)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 eb6d6a31-ddb0-4ac4-ab9f-2e09a842adab)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b852d6b9-7c28-42b9-a954-9c1f30becf90)(label(+))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 10))(sort \
                 Typ))((shape(Concave 10))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e37eaa39-1d35-4615-925a-c7a4fe051e5d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68555513-8044-4c05-8369-d6fc4e9479ea)(label(Lam))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 77f3b50c-659d-4b25-83e6-450c60e50b15)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape(Concave 1))(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 e1dedef2-7710-4643-8ff7-da3f7963be58)(label(String))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 2f732dc8-a2a3-4480-8eed-62c6d872695a)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 733387e9-09ad-4491-b0b9-9a34089d846c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5b680114-2e69-40ee-b8f4-62c2c765f1d0)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 cf98b330-aff4-415e-bed2-ed03b7150673)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 d0e80bfa-6afa-4a83-bba6-5c99975e6b92)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 db614320-d1c2-4f59-adbb-8563790674f8)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 91fb36a8-e9e4-423c-8e20-93881ca9dd96)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 0a09273c-ce68-4e77-a86c-78fa0c97d77d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6599d528-c92d-4998-8119-1f33b44eaa22)(label(s0))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 ad6f37e5-9725-4a52-b5de-fcbc96756a72)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2bd49cf7-2c78-47a9-b5ee-e278744a4cf2)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 e8380c33-f3a5-43f3-80f7-392167a31cc1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b4b68294-d91f-452e-9435-a0118dcb2292)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Secondary((id \
                 f6b4ac72-8819-421d-9633-002d770f4b65)(content(Whitespace\" \
                 \"))))(Grout((id cd0bc854-2299-49ce-bee9-de9fd82346eb)(shape \
                 Convex)))(Tile((id \
                 1da8095f-70e9-478a-b95a-348276ec4d5d)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 7f9c047c-8446-426f-9edf-cbeb76ca9b2a)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 befb32f1-599a-4557-92a6-a4d64a693eda)(content(Whitespace\" \
                 \"))))(Grout((id 76d6d520-7195-47fc-a579-61a33684e86e)(shape \
                 Convex)))(Tile((id \
                 611f7dd3-ba41-4ac3-87cc-32b5254d7228)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 5139d477-27a3-4826-b8a0-2c97a1f57e79)(shape \
                 Convex)))(Secondary((id \
                 b600e722-c04b-47e4-aa58-e9b9b9b1c80f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 ca1739d4-1b3c-40ef-b37a-57c13fd971a8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5d9655be-5048-4687-bb6b-6b4f5b56583f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 775aef81-972d-4a34-8667-dbd1d2f65b6b)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 1c7eb0c4-0a5a-4477-b90e-c6ea3f493df6)(shape \
                 Convex)))(Secondary((id \
                 c7bfafe9-2550-4311-9a2a-88a938e4f9cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 185fbf06-5a65-4d57-b291-1a4da58923cf)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 dd411c86-759c-4563-b3a2-a63260dc9727)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 8f1e5db4-de58-442f-bd9f-cf2b482a20fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a20febf0-9ad2-4ef0-9b19-63b36c0aef8e)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 a4f341cf-f91f-4415-9aae-c02fed14f0d9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 557b65dc-720c-4a18-8dd5-2c5c1913da37)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 99a6baf5-bb4e-4209-9939-b34888282503)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 20cfd714-86b8-4663-b91e-bc26da4bfaf4)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 4daf5ed4-88f0-423d-b652-2357d95fe277)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 60adfc37-bd6f-4026-b483-2661ac052384)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 fb8ca99c-8c68-486b-bcaf-e2a893308867)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 13851621-0f89-4592-8e69-3cbed6a61d33)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ff5fd32a-7b39-47af-be9e-77c42a007386)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 c1de2ae8-bc4e-4a40-899b-beffb7b90f24)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 c40fb4fa-77a1-4b7b-a108-d94ba43a81f3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9d22c218-90c9-4814-80e3-f71427df6dd3)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e523b517-a7a4-47fb-b15b-44c24ef429ef)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 2d50f0a3-e2e1-40b9-bb5d-c18c4395129f)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fa2cfa9c-2602-44c8-8ae6-e66c0643a9fe)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a070290a-9d49-406d-a571-969d3c59bc04)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 a74ee29f-0612-4873-b904-42d84f357ad7)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 f504aea7-e59b-4c0a-b271-0c7f25c26897)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 563c5ceb-1115-4546-a6c9-cf8966d30d74)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 f0a21c78-4ddd-430a-ae7c-bf5b868a42cf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ff3eec97-f813-4ea3-824d-9bd39f42863f)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 446ad766-4b3a-49cd-a59e-c97a99e77f87)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 59854337-72ea-457b-9928-b47040616972)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1778cc4e-c3c5-4fca-9337-8149d9049695)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ada7476b-b869-45aa-8835-10766aa5b2bd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10b2690f-3300-4e15-8191-ae15a13fd0fe)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 22c3f663-8321-45e7-8dae-2eb1a9580a14)(content(Whitespace\" \
                 \"))))(Tile((id \
                 93b94ff3-c38b-4d5b-a57a-ef67a66bb7ff)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 00ec041e-446f-4d9f-a251-f271badb92d1)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 116d9caf-de07-4fac-aa6e-dff7801d3010)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2ec60d5e-2a5f-4b9e-9585-1115645a64e8)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5b8b9aea-65d0-41c0-b80b-db188ac01147)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 832e784b-788d-40d6-a483-1bca42949f29)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8f9e44b-4ade-43fa-a712-adcd666e1ec8)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 f36dc0f9-0138-4ee6-9b8a-ace0eac6b30c)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 f7cc9d32-b4ef-4b92-a74d-c146ac696c5c)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 ec532306-88f1-4d5b-962d-af460f2dc638)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37e4ede5-9819-440c-88ce-7b509ec5aef9)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 befef6e4-0ebc-4fc9-a56e-bfb0759ab6a1)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 be8c53ec-f1c1-424b-8389-b65df87d3e46)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 756abca1-adb0-4fd1-9760-8d8ce610b570)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 7457ecbd-777e-4d6f-9211-c6a57c6dbdd4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 942eae46-5ba5-4ff3-bbd3-7371781cd01f)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 68708be8-5afc-4522-bca1-e3ec22500a20)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6f72377e-216d-496a-8701-719c9061ad21)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bca6df7e-9dbe-4afe-b10c-8e43dc255aee)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 5937d802-b671-4a85-a83a-917be83e1b65)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 3f9c9712-f50a-4454-8185-d020adae8753)(content(Whitespace\" \
                 \"))))(Tile((id \
                 13d06d16-4d6d-4cba-bbf3-b36cce15048b)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 a8fa2921-c115-4ca5-a4bb-4f26d22d8b14)(content(Whitespace\" \
                 \"))))(Tile((id \
                 89653b2e-7c09-48a5-bf21-442261ce74d7)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 46584037-73f7-4d73-8ff3-729b030efe7f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c15ff89a-b4f6-40dc-a793-eb4a33da0ee2)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e3de6e5e-1afe-45cf-87a1-10ce114b259c)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6c8249ae-e7c4-425e-916d-35a1c0e622f0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 942aa65c-b4af-4f29-9571-cb289d1a6165)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b2f0604c-cf8e-422a-8cdd-bacda95303d8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 96779aa3-30c6-4b11-b168-e8bf433697ba)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e85c27d2-a928-4ca0-9f26-b6b48c559a83)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 44e7eb79-97f1-4f73-be15-e57d7d5ab7ec)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 198a2ffd-ad59-4518-a9a2-d984fd723eeb)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1d92aba9-e84e-46e9-8949-a2e5fdde7288)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8fc7a23d-69ab-4bc7-80fb-506c72f99617)(content(Whitespace\" \
                 \"))))(Tile((id \
                 84d27fe1-acee-4b8e-9e50-f18205e7ca7b)(label(s0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 05417c5f-7abc-47b4-b117-01bbad2b817a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 742b38bf-253f-4a0d-b93c-e787355d02ba)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 e25f74f4-bc1e-4411-9642-31bb1450c7f8)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 64c50f0a-033a-426d-9b3a-d212a0c42261)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5126a3c1-064a-4416-b7a5-258f24cc8a4d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 e833bdb8-b6b0-48d7-9b9e-a4a3b805ebc5)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 bad8840e-0ebc-4ec0-9d83-1410ac7928d7)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b3cf1ada-2c15-495f-a7bd-f32179d1756f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 7164e04e-1d8f-4437-a1f2-e198ea877f36)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 9e17ed83-c96f-4d74-ad00-a00ac01c1c01)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 ab114014-0c5c-4dfc-9581-75415f79d969)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 49e65ad2-77c3-4e88-a2ab-253a8bc47d5e)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 24fbb80d-09b7-44c4-a660-7144517963f4)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 1782d34a-69f5-43f4-98d5-4e9965aa09e8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 73b65955-223f-4149-ab88-3981fa5a41e6)(label(s1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 1108f478-9be4-4b8c-b00b-9587fe7901d3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d81b18c3-16e7-4a8a-ae26-fc0cea47cee7)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 9b152dd0-2282-49e1-ac18-c8933fbfcaeb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 54c18b7d-3d58-4f1e-b19c-7874f1eff256)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Secondary((id \
                 2c8ce26c-17c5-4e0f-ab3a-2597df42228f)(content(Whitespace\" \
                 \"))))(Grout((id 3badbdc8-23cd-454e-96ad-ff811f33bc4d)(shape \
                 Convex)))(Tile((id \
                 08770fea-fed8-4dc5-ae95-5a61bb0dfbc4)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 5d49f3d5-9923-4990-892c-289df59a324d)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 f1e00411-2f62-4a4c-8725-6c31210c5ce1)(content(Whitespace\" \
                 \"))))(Grout((id d9df15ae-16c0-4d89-aa12-a0173b03bcd5)(shape \
                 Convex)))(Tile((id \
                 9eb40612-97d7-417c-9183-4b8b82310135)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 f66aea33-9dfe-4669-97ed-3ad3e2183ece)(shape \
                 Convex)))(Secondary((id \
                 e5281e84-a459-4ac3-9df5-a675a58f8dd2)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 10a3ea36-5717-4a9d-a904-8e909ce21452)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 6c94cf87-44ef-449f-93b6-1a91026554c1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e7dce9a2-c60a-4cbc-9533-e559916247eb)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 737c10e7-f830-4d65-83c7-ca86518ca8b5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5398d156-db31-4856-b6f3-bf799296688b)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 32478ca5-a90d-4080-96ac-8e8f82d52bce)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 45feb05b-e378-4171-a272-15a7e7c050cc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be54db6e-a5ea-4268-acdf-b767636259ca)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d4fab028-3a59-4483-b159-a6154b245e18)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be4a711b-8975-4021-991d-ccf13e4d2a13)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 9d01ad49-e3f5-41b3-bab8-12b18e46d4fe)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cd39f53a-c54e-4bf5-86a4-fd431a552b48)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 28475c14-44d6-422e-b2f7-5ce689b9f943)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 c017d5b7-dda4-437d-904e-134859616101)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 a67e7abd-f21a-4656-a67a-d6d44b276095)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 f01375e6-7b93-4552-bab8-734c4138f0ec)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 4ff93411-4656-49bb-8371-f59110782753)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 880accff-3ffe-46b5-b84e-1906566397e2)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 72bf3f63-3fbe-4e92-a853-5814c87cafd9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8d6ef231-d184-4e32-83a8-d4857517de93)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4550cd46-c3fa-40fe-830d-497271c862d2)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 bc1c4b0d-d79e-4538-89d6-1ff88830f82d)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 2e40128d-329b-4565-8fe0-a9a3e1f51e6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 85480a24-0bc6-493b-a3bd-5b309a9b1813)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 b2e7fb26-d4b6-4ce9-b022-7a134f72cc40)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 dac270ab-9a36-4f3c-b104-6da8bb42bb4a)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 f0697919-452d-4547-a355-03a295d10699)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 78fb69aa-e085-4e88-a5bb-b3f6833158df)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dced379f-c870-4971-983f-d1d21eb6340a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4c24a1c5-a6fc-46c7-b1e0-c3c60c11eb85)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 6b001209-0115-4779-b6e6-1c2c408bce6b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a96b23c-2969-41bd-9f50-519bdb97fca7)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 9a39a50b-08b8-4521-a578-d2582b48850c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37416597-aadf-4dd5-93b0-711ec8a27589)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 8c51d6a5-aac3-4693-81f0-8b004992ba3a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5d6ec0fe-96e7-41f7-8494-5ca2d369ee6c)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 db24e8c5-5c5d-4102-a5cf-be2a524cd227)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 e1bb3b03-196d-4b4f-9ed8-65142dfd5845)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1dbe904e-ec64-44db-877a-1a5b31f2a436)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 5460b9bd-f20f-470e-881d-5bdd9c69eb75)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 2f3daa4f-12e2-4e84-8192-a9337671d441)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ab987731-c02e-4fb3-a4b0-93eb7792a87d)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 d47f8790-7ce5-47db-a3b3-1ab578d3edde)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 6518bedd-3b89-45db-ac71-a931a150e62f)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 36caac6a-cd96-4ad4-aee1-74ba94acdc4a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9286a72d-b807-4538-9b94-83d6bcd804f8)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 643d6b3b-4696-4947-ad6e-6c2bc3b07027)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 ff339187-882f-4e99-bd75-538f2a86fa6b)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8d307a67-4be0-48ef-8ba5-fc2f154c54fb)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 ed34b66f-351c-4ca3-b636-cbbdaaeda542)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55dd439a-ca8a-40da-92d8-8a797422a310)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 938e8b36-64f1-4e68-8ba3-c2fec8f3f45b)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 beedca58-2326-4480-91e7-e5f4d99fa547)(content(Whitespace\" \
                 \"))))(Tile((id \
                 848337bd-2480-4e5f-bb80-38d5187508e3)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 ef6b4b33-20b1-43eb-92b2-e2636422f239)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 78dcaf30-db0d-4f5d-8cc4-e001629e68fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0b597452-2317-4da2-8471-47526adbb0e6)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 4b9e5caa-0da9-47a8-9c5f-cb27ed6e4288)(content(Whitespace\" \
                 \"))))(Tile((id \
                 79b544f9-0532-4229-b0ed-463a20ec2b54)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 465f5fe4-dcd9-4445-83e1-a4e942cc874d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f144d612-18fb-4b94-a01a-c808c4b33ed8)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 f132d170-28ff-44da-935e-58cfa886dee0)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 16ead05e-7851-41c6-a1e1-ae59c3945975)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6dd5701d-58f1-42fc-b81a-f20000a0762a)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b2f043b0-6a3e-46f5-a5ab-39042a4cabf6)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c4ec282b-7b13-44aa-8d73-63d352b5ffef)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b20fa6b1-a28b-4f6c-b32a-0bae7273148e)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ac69155b-d937-4b0e-8aaf-9c8688b57555)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d2df7948-f0a9-469f-b740-ee9e49052ac2)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4e677a8f-98a5-41d8-9ffd-8364268786c5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3c82e313-1e93-4600-917b-31f7b6a8f304)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3f73bc90-6955-4173-94d6-f6b0c5c57a22)(label(s1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 faab18a4-5617-4435-bbbb-384a05ea3990)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 165de0c4-61cd-47cb-8c96-91c70f740a9b)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7ec571f5-2457-4bae-82a5-1c579919465d)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 200413d8-cb0c-4dd7-9854-fb3aa78a82f2)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 1012ed31-b75c-4845-b4bb-53ea5e113240)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 dd6fb831-40b3-4819-96c1-58df9743982a)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 52ce5092-82bf-4efc-9642-03a78680c9dc)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 dc4af1b0-9f2b-4084-8733-5ebd1869490c)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 c0e0acaf-7e68-4bbd-a210-17ed32bb3161)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 734364d1-d1c3-44bb-b1a5-91d58c4baf6b)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 b691f0ec-90d4-4c8e-aae3-afb169b323aa)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 cc1922c2-814d-4224-8122-fae8cb55249b)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b2b89dc2-8577-47d5-a3e6-725ccce77352)(content(Whitespace\" \
                 \"))))(Tile((id \
                 5a5c3653-fcf0-4f3b-84ce-6046defec88b)(label(s2))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 fa43c6b3-1947-4243-a0b0-1332b6a0e4d8)(content(Whitespace\" \
                 \"))))(Tile((id \
                 938ef5b8-37a0-4a7a-826b-4bcd7f01dece)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 bb449d4f-9d15-4f75-a0a0-b48b7363fc43)(content(Whitespace\" \
                 \"))))(Tile((id \
                 747de3b9-fd0a-49c0-b1fe-83920596b86f)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 610756b2-438a-4e74-b9d3-183c0431eda2)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 7589e69a-7f6c-470f-90e6-673992d12edf)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 6d294332-5e34-467a-8829-d3de4687d69f)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 939c86e6-b9d2-4ebf-8df9-bf18cdc24bce)(content(Whitespace\" \
                 \"))))(Grout((id 99fe0329-0226-4898-876a-ef6f2d2738ac)(shape \
                 Convex)))(Tile((id \
                 dbd016f3-4159-46e1-ab1f-b08f74e55556)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Grout((id \
                 1f49d859-12a8-4ba0-bbbd-f6382be4c1d0)(shape \
                 Convex)))(Secondary((id \
                 bda2ebed-db38-4d8b-ba19-640bc231fc31)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 98c0ee65-911f-4dc7-a141-f8028436ceac)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5851e756-e845-43e7-87d2-e9a7714420f1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 88de798e-e3b8-4b22-bfa9-9139e5920913)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 1613e686-2967-4262-86b5-304bd3d45bf2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bfe690a2-ab0b-4cb5-a56a-17d7ad111ea9)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 5c0adcd3-a218-442e-a069-e5da52f9be77)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 4c2d33c1-a7bd-4c27-819a-24edc29ee829)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0769fd4-fbfe-43d5-9c10-49ebf258e03a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 52b5805c-cb97-4f75-a153-ea6d80c6064b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20f65515-cdfa-4e1a-97e0-0f72c23c1215)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 5aa6dd4f-91e7-40da-b703-79e8f9d5f828)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 d1b5e5eb-a60c-41dc-8c77-bbcfa0b4e335)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 23396bcb-11c3-4b5a-8c03-5c8469f353d7)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 7762fb15-bcb3-4250-865f-984342407b80)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 7fa9b386-604c-4d17-bf41-ee6b59ab4ca7)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 ec3c19a0-e68f-44b7-a321-e13a9a3ef7f8)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 7702c7b8-1fcf-4665-9b3a-229ce13f22d7)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 451e4472-7dcb-4d4a-a755-f1ea15a4c0dc)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 1124ac0f-6857-4ffe-9011-d2f483a2bcdb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d2c541ab-5f7e-4b3a-8abc-7cd83c560865)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0689dc3b-1f08-4e83-91d3-327ad6eb0578)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 41868c44-9c4b-43ac-be15-cc6c8cb07386)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 d6edc2c6-b705-4472-8d36-7ff3be59833b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9cd6d33a-5dcc-4c61-9fbc-7ca1435508db)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 0208c75b-aec3-4658-bcf0-68a84976ce80)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 d0e4fc56-3b03-43ce-b3cc-6d67ca07b2ac)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 33e3b9c4-2795-4b04-b602-0761426a6488)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 48acd3e7-f185-42d5-9d2c-cf87a12027b6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f0998eaf-466e-4fa6-a976-aeabfc90a4f8)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 148d17b7-810b-4824-a983-300bb273ca22)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 f6960b08-78f6-4d53-b386-e6fcfa917b5e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4912f8ea-9a01-4b01-9cc5-c443fb0b6a31)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 0ce1ec5d-badf-4a70-8993-b23bfb058e86)(content(Whitespace\" \
                 \"))))(Tile((id \
                 1f04a106-7ef2-4b20-b133-a65742ee6bf1)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 73177127-45dc-437b-88df-78782bd16cec)(content(Whitespace\" \
                 \"))))(Tile((id \
                 20bd86ce-cb0a-41ae-897f-61d67430a209)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 ceafd688-d6bc-4378-a277-ffa7057d281f)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c377475c-5614-4ef4-b73f-e840e0c729fc)(content(Whitespace\" \
                 \"))))(Tile((id \
                 d06b9be3-8765-46a8-8748-40c3825e07d7)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 6db9d342-dfe7-4f42-89f5-e92e28a248bf)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 9ad0e739-e445-4c99-a607-fd288bd17c68)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e6a632f3-977e-4a74-b196-9b3367acceb3)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 4212c12e-83ab-4695-a9df-e514e0863ae1)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 03310cca-6d4d-474a-bf0d-621a73070f34)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 613b8f84-65b7-46a4-b888-0a66bf462c75)(content(Whitespace\" \
                 \"))))(Tile((id \
                 68178d83-062a-4ea4-8227-9abfdfee6d4c)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 cd90a459-c9dc-475f-903c-cda1073524db)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 35e11b3d-4761-4cf1-a2b1-4f943142ea2f)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 52777fb6-b90c-4bf3-91b8-c48e76bbae2c)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 b3355287-b21c-492a-88b1-02642438d0e2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e3f0071d-a028-4c88-97ef-de174df2276f)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 621c2d5d-561c-46f3-9a0a-e2e20186f647)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 03a1caba-3c6e-4ab2-9c91-a6ff19871d22)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cbae577a-825f-4306-a0d3-ba5fdb2b53fc)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 05558162-3890-48eb-a8e3-b434674d94db)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 4ac8347b-5d89-4a96-9b71-376498adba2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0230fa65-371e-4789-8a66-2fe773709f47)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 7bf1f73c-351a-4941-bc19-3cbd8e37126e)(content(Whitespace\" \
                 \"))))(Tile((id \
                 531ca411-4d2f-4568-95fb-114775d32d1b)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c4bada90-da04-4339-a065-040fb49290eb)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9fb7c672-396d-4af5-abcf-176e575810fe)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 1105fa5c-496d-47ad-9bd6-f9cf1d7ec280)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 d20a5732-a80c-463b-bcf1-ed54ff64913a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 75857d41-49d0-4843-91f0-4c9a2f05b9e8)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 219d7cb7-2d91-4e9e-9db6-de8cc942589c)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c3bc91cc-ea93-4459-927f-670d2feee23b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 3e3ac24f-819f-4a77-a078-8fba40db4b14)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 ce52ed38-3a85-4969-bfbe-6d6e015ebe77)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9a707bbd-4f14-4fa8-ae15-00066e37431d)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 7363dd7f-037f-4449-97c0-d64f6570e580)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 e15152ba-0933-483e-81dd-fc5f76b838d5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 7097dacd-a195-4685-a8c0-671b6e2942fe)(label(s2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 669a52c7-c789-45a2-abc5-b286ccbba945)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 8b2e154a-e012-47b8-9179-e4bfc19d7b70)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 13d406b2-152d-4e7c-ba9b-b905624dd2c9)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 d6109b21-3b96-4f27-bea8-dc11496c27bc)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 9c33d11d-8461-4414-b0d5-4a96f6b74204)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 2f6c952d-ce4c-4a27-8a58-02d7ee1a1c23)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 5f857afa-f951-42f1-938a-3de3389fb431)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 c0af9e91-803b-4aaa-953e-099160cef805)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 0ec4a780-d2f1-4db7-8b1e-c85c4461984f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 c970d281-994d-4eba-baa0-09578482a505)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 8c0bdc0f-e7ab-47eb-8a9d-1de5b502a153)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 8c2139d2-0dea-4596-bfab-34341c1104cd)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 c974ec10-6d1f-4cdf-a67b-ccb87c6a1703)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e0771a4f-a690-4efa-adb8-b716ac8e827f)(label(s3))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 14e7a5b3-63fb-4842-8b47-3420114c8273)(content(Whitespace\" \
                 \"))))(Tile((id \
                 db4094e9-df9a-4df9-907e-d309a95ea8c5)(label(:))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 11))(sort \
                 Pat))((shape(Concave 11))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 98eb5b6c-eb78-45f2-8d7d-1313450f0494)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4a2bfc50-75e6-4b70-9562-3d4f8bb6bac4)(label(\"(\"\")\"))(mold((out \
                 Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
                 58c8d0f6-5de5-4e48-96b3-ddc352d417e8)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Tile((id \
                 baac9843-0703-45b3-b1fe-e72076a52ae7)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 8161ec7c-551a-40e1-92be-22534aa72cf0)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 af36594f-adcd-4392-b7b6-831ca24a7e51)(content(Whitespace\" \
                 \"))))(Grout((id 87ab3bd6-953e-4795-bc19-495818da05a0)(shape \
                 Convex)))(Tile((id \
                 d45b9d20-9c1c-4405-a4dd-91482d95e1a6)(label(,))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 14))(sort \
                 Typ))((shape(Concave 14))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 34acaff4-8978-4814-9033-da2dd2e883aa)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cd5d6193-42bf-4705-a73d-48d23ffb1ddb)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort \
                 Typ))))))(shards(0))(children()))))))))(Secondary((id \
                 bf7a302d-d31a-4f1a-8e41-e32b2a25d53c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dfdba68a-19d9-4b64-87a5-2ec624d25196)(label(->))(mold((out \
                 Typ)(in_())(nibs(((shape(Concave 6))(sort \
                 Typ))((shape(Concave 6))(sort \
                 Typ))))))(shards(0))(children())))(Secondary((id \
                 0677c053-e1ce-49b9-816a-e49ad30fcb2f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 123f4e0a-e8be-4ea6-ad8c-92277ed74c56)(label(Exp))(mold((out \
                 Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape \
                 Convex)(sort Typ))))))(shards(0))(children())))(Secondary((id \
                 b39041b5-ee18-40d8-897b-e8cfb0c6fd13)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 c0eb8777-ef3c-4048-988b-284ea9c90989)(content(Whitespace\" \
                 \"))))(Tile((id \
                 33264e6f-0614-420a-ba4e-d1b77addeb5a)(label(fun \
                 ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 13))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 355d3c9d-3cfb-41b2-bd8a-bf2be8ebe16f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b177ac7b-52a4-4d0d-a790-6aebdb5db7ed)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 78271b7a-0844-4e38-b7b6-cadca05fa973)(label(e))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 8543f7ce-2bd0-4eac-bccb-699af7d503aa)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 b3d5fac5-0afc-4938-a1a8-b12db52ca4d4)(label(x))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 307f30c8-b829-4736-b2ee-b3d16e180e15)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Tile((id \
                 2318c6a0-ffb0-4fe0-8762-09bb2a5e1586)(label(v))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 fe4cad7c-1a93-4667-a9a8-bfa06b50bb23)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 15ff503d-76d5-4079-a563-1e066095eb27)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 123fc198-1bb3-4731-9ecc-95483461f0e1)(label(case \
                 end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort \
                 Exp))((shape Convex)(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 941fdd6c-5504-4b74-899b-07d4bac062a5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8dc4d2dc-37fb-433c-a658-e7085d4685b3)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 97c5b2c7-28e2-4ad2-aca4-09f95494ebb9)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 620c9eb4-5693-4398-b78c-f3a09ae6f79c)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 899d4e98-af91-414c-b2d6-71c021e88196)(content(Whitespace\" \
                 \"))))(Tile((id \
                 722fa588-d967-4016-b72f-9279ab8627e1)(label(Var))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 4733709a-16cc-42b7-beba-100898c2c858)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 5e28a863-ceb0-4534-9241-9bd611521e1f)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 3e5d5ef6-9f93-4ad4-a20d-3055e67b0cc0)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 5836dde5-b464-43bf-b78e-ad1a875bde94)(content(Whitespace\" \
                 \"))))(Tile((id \
                 cf629cf9-509c-43c8-b23c-5a9a622798ac)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e3be0ed3-d617-4995-ad17-09b10993296a)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 b159b16b-2041-45a0-948f-9c424cbcf226)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e36f353f-1078-477f-b5dc-f2cb453d1b91)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 40c7dd06-913c-460c-8314-68b090bb6cd5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b62381b2-5908-4786-9d01-b9f977115d19)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 df3de83f-b013-476b-9370-471eb5003085)(content(Whitespace\" \
                 \"))))(Tile((id \
                 963faf89-4990-4fc1-ada6-7459c82d3717)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 e4043c67-5605-42e4-8b5f-9a2a9c209d96)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 da6e61c1-30a5-4d9c-a4dc-831054826abf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ba68d36b-434a-48f1-a629-2f743030e2cd)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 afdbd0fa-542f-41fe-9069-76045e498b53)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e4c0260d-a4cc-492f-b4c4-771786993a99)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c980681c-f918-4808-89ae-c6d346595ef6)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 cf724525-27e0-45f4-aaef-237eafc97d66)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 04605de0-0313-4068-b31b-bb316495e6d9)(label(| =>))(mold((out \
                 Rul)(in_(Pat))(nibs(((shape(Concave 19))(sort \
                 Exp))((shape(Concave 19))(sort Exp))))))(shards(0 \
                 1))(children(((Secondary((id \
                 fc575796-1873-4f2f-994a-6518ccddf861)(content(Whitespace\" \
                 \"))))(Tile((id \
                 55c4c1ad-7678-418c-83bf-196accce31de)(label(Lam))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 6aaecc03-4785-4aeb-b707-0c92d7c7a266)(label(\"(\"\")\"))(mold((out \
                 Pat)(in_(Pat))(nibs(((shape(Concave 1))(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0 1))(children(((Tile((id \
                 e338050e-96f5-4b48-94e6-c96abfe896a1)(label(y))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Tile((id \
                 97ac1e08-5893-48e8-b346-3de1f9c7f3c9)(label(,))(mold((out \
                 Pat)(in_())(nibs(((shape(Concave 14))(sort \
                 Pat))((shape(Concave 14))(sort \
                 Pat))))))(shards(0))(children())))(Secondary((id \
                 183bb5e8-f97a-43ee-9f95-c68938fd9321)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6eb46efa-7e41-4cce-98bd-0772fa6c9826)(label(e1))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort \
                 Pat))))))(shards(0))(children()))))))))(Secondary((id \
                 c183e36c-56a4-4624-9822-51862cd880d4)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 8d5a1533-b4e4-4ca3-ac15-9f34d76e409d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4ca9bc01-1866-4ac1-871b-bd492836c3ca)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 cae9c90a-f491-479a-8b07-99fe83f43e3e)(label(if then \
                 else))(mold((out Exp)(in_(Exp Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 12))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 5ab212c5-150c-4aed-af1e-1ee3aa267ca2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 30ec99c0-2596-4d35-8ef4-96e3a78982f3)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 05de7eb7-19b8-4022-ab59-48a2ee9371f6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 60970233-09bf-44a8-a3f5-b9b55b697c66)(label($==))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 8))(sort \
                 Exp))((shape(Concave 8))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 3ee4060a-bc12-4127-b802-28c97fa227fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f904eadd-22d2-4c70-a2f6-a9a8e95f7e9f)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 b4a5ba6d-a9d9-4681-aba1-279b7fcce864)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 9235c309-54fd-47a4-86e4-6bf2b145766f)(content(Whitespace\" \
                 \"))))(Tile((id \
                 bc917217-9f4d-4618-abc9-a4c7badf8fcf)(label(e))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
                 523dfd65-7cfb-4e10-8140-58df45189954)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 a331ebca-4174-497a-88df-260a2839c82b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 10b38dbe-ab08-4a6c-bd0f-15ffbec9c599)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 cdfa30aa-80c7-4076-8c1b-d000ae174933)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 a70e5f58-ff10-40ab-b34c-cc420ee29260)(label(y))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 89324dbb-d5e1-49e0-9931-cb28e90588a3)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 7874fd31-d316-4407-a76f-3f2c12981222)(content(Whitespace\" \
                 \"))))(Tile((id \
                 fef0eb24-089c-41ad-9170-ea96e0d912f9)(label(s3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 70918b0a-1b52-4e91-acfe-4215c714cd21)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 1e1de978-71b8-45b1-98da-24c609e7a38f)(label(e1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5d4890f5-cde1-4fb2-9d70-d9cdc3e244f6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 879b0459-d8c0-4886-8f01-3afafde541a4)(label(x))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 bd487df9-93b1-4cad-b618-593640268d1e)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 9f7edd71-46bf-48ff-8a8b-4902ae76833a)(label(v))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))))))))))))(Secondary((id \
                 ed26ce90-f540-4b1e-8911-82e676d03663)(content(Whitespace\" \
                 \"))))(Secondary((id \
                 1bfd7d79-c9cf-46e0-bf89-70b889618913)(content(Whitespace\"\\226\\143\\142\")))))))))(Secondary((id \
                 3c8c82c7-64b6-431f-923c-3730db26f30f)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 b7fed359-df21-4225-b200-2e22dc6c14aa)(content(Whitespace\"\\226\\143\\142\"))))(Secondary((id \
                 0661ebad-406f-4078-bb67-cf85d94ff553)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 16273e1f-8dfb-4fae-a92d-8f5f91ec6b86)(label(let = \
                 in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
                 Exp))((shape(Concave 16))(sort Exp))))))(shards(0 1 \
                 2))(children(((Secondary((id \
                 9e58d3de-17b6-42bd-9377-87bf4c00b70c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 ea2af831-cae3-4705-b845-7de25cc75207)(label(in))(mold((out \
                 Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape \
                 Convex)(sort Pat))))))(shards(0))(children())))(Secondary((id \
                 3b1486cd-8c60-4d65-a2b9-55ee3ecec7f4)(content(Whitespace\" \
                 \")))))((Secondary((id \
                 6789fd99-3b2c-488e-b3f1-bbbba4849d18)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6e44b37e-b9a0-4a61-b2bf-261e052cac96)(label(Lam))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 b62d419f-99a8-4663-b37d-a79a86f10d37)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 9bc17b58-fb9a-4d20-bc28-845f69112809)(label(\"\\\"b\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 13cac507-9ec2-4220-af73-d3eccfe9aa37)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 16f90e53-90b4-4177-a45e-94f96e7e3742)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4425699e-4c8f-4f54-857b-6bec78073a29)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 d59ed508-25e8-4a7d-977f-14914aef681e)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 57830974-8559-45e2-aeeb-77af11fba153)(label(\"\\\"a\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Tile((id \
                 cc3d8487-b3c9-4db6-a0d4-2f75cb251df6)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 6a74fb1b-b755-4f33-aecc-649c7c6ed5ea)(label(\"\\\"a\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 5e10d17e-1ccf-4344-bbac-3bbb61d8cde5)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Tile((id \
                 5edcd2bf-3bcd-47a5-901f-71641e7e228e)(label(Var))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 87066fa2-471e-4fb4-9738-2faadfbeb7cd)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 e2eec479-e331-49c9-a9f1-09895a850541)(label(\"\\\"x\\\"\"))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children()))))))))(Secondary((id \
                 816b5495-6cd9-4326-b7b8-9fb3e3794faa)(content(Whitespace\" \
                 \")))))))))(Secondary((id \
                 e3addd57-d3ac-4aaa-97cf-bd7d7daded11)(content(Whitespace\"\\226\\143\\142\"))))(Tile((id \
                 aa386e02-c97a-4762-a2d6-f0e04a84eee1)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 4057738f-9053-41b7-80f7-fb9528567458)(label(s0))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 932de609-46c5-4911-a857-1e4db230ff4d)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 bda89467-fe1e-4713-8a48-f80855dad888)(label(in))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 76791e66-8295-4b10-8851-a6b3193d19bc)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 67aaf480-2efb-466c-ba70-fe00fb81748d)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e331eb90-3c47-4b60-83dd-df135813f3bd)(label(s1))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 4979394c-7ce6-447a-aeab-d10f7cdbbcfa)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 7f79c7fb-a64c-4027-a993-59de01b39c3b)(label(in))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 9aae2031-4dad-45cd-80c6-d86cec1bb66a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 713f0055-f9df-4582-9f17-1fabf831af5a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 37422a8c-42bd-4778-b989-ecd472930c32)(label(s2))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 a4fdf2e0-55d2-4885-8863-a8f609233d3a)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 d5064142-4ca8-40a4-a58e-5ffe611e2800)(label(in))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children()))))))))(Tile((id \
                 f48e6a9b-349d-4602-b643-488d47977c8a)(label(,))(mold((out \
                 Exp)(in_())(nibs(((shape(Concave 14))(sort \
                 Exp))((shape(Concave 14))(sort \
                 Exp))))))(shards(0))(children())))(Secondary((id \
                 c53a4276-9776-4dc3-97c3-6da69bd405d1)(content(Whitespace\" \
                 \"))))(Tile((id \
                 632434b0-6530-4c2c-9eff-811f041f90db)(label(s3))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
                 67f703ea-bfcb-4da9-89e1-3c4b2eaa2c85)(label(\"(\"\")\"))(mold((out \
                 Exp)(in_(Exp))(nibs(((shape(Concave 1))(sort Exp))((shape \
                 Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
                 999bd632-f0b7-420f-81e8-36293d44a1b5)(label(in))(mold((out \
                 Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
                 Convex)(sort \
                 Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
                 5b1dfea8-f1f8-4603-8878-9da0ac1d00c4)(content(Whitespace\"\\226\\143\\142\")))))()))(ancestors())))(caret \
                 Outer))";
              backup_text =
                "#recursive sum type dynamics tests#\n\
                 #all calls should evaluate fully with no exns or cast fails#\n\
                 type Exp = Var(String) + Lam(String, Exp) in\n\n\
                 let s0 : (  ,   ,   ) ->    = fun (e,x,v) ->\n\
                 case e\n\
                 | Var(y) => (if y $== x then v else e)\n\
                 | Lam(y, e1) => (if y $== x then e else Lam(y, s0(e1,x,v)))  \n\
                 end in \n\
                 let s1 : (  ,   ,   ) -> Exp = fun (e,x,v) ->\n\
                 case e\n\
                 | Var(y) => (if y $== x then v else e)\n\
                 | Lam(y, e1) => (if y $== x then e else Lam(y, s1(e1,x,v))) \n\
                 end in \n\
                 let s2 : (Exp,   ,   ) -> Exp = fun (e,x,v) ->\n\
                 case e\n\
                 | Var(y) => (if y $== x then v else e)\n\
                 | Lam(y, e1) => (if y $== x then e else Lam(y, s2(e1,x,v))) \n\
                 end in \n\
                 let s3 : (Exp,   , Exp) -> Exp = fun (e,x,v) ->\n\
                 case e\n\
                 | Var(y) => (if y $== x then v else e)\n\
                 | Lam(y, e1) => (if y $== x then e else Lam(y, s3(e1,x,v))) \n\
                 end in\n\n\
                 let in = Lam(\"b\", Var(\"a\")),\"a\",Var(\"x\") in\n\
                 (s0(in), s1(in), s2(in), s3(in))\n";
            } );
        ] );
  }
