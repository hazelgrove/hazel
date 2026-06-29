let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         cb07dfc0-5797-499e-85ac-3dfbfebb7489)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         7b1158f3-54d3-4a3b-abc2-4f5e4419b1d5)(content(Whitespace\"\\n\"))))(Secondary((id \
         35759ed2-31b9-4b88-b0df-f0c21b06ff33)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         5e4ac6d7-51fa-47e1-9063-ac318be8316f)(content(Whitespace\"\\n\"))))(Secondary((id \
         c61a62e6-9392-4ef6-b931-3d99a8a73b03)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         b7224d8f-70e4-4f5c-a372-0c0d44937dc4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8035bd41-7218-455c-804e-6458c7b55470)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         3583323f-b10d-43b5-95c6-6f7b0de5088e)(content(Whitespace\"\\n\"))))(Secondary((id \
         732f8a1c-10c6-4629-83be-bf2c52e0992f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         1034dd9e-9cfe-4200-b490-836ad4b2b98d)(content(Whitespace\"\\n\"))))(Secondary((id \
         bee12bbd-b862-4719-bbae-031d84a5b90f)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         dece21f8-ebb5-4f62-ab7e-1697bb7fb737)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e42bbbc-3f35-4995-b023-2a205b811443)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         f0c0b4a6-0d12-4cc5-8183-4b175859371f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1cd9e1ce-8c08-465d-b781-20d5d4abf114)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         0fc57e6b-7ab4-4790-b0a1-9e34eea033e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfef79d5-85f8-4f5d-82e0-db2fbe7c9d34)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         2a8c38d4-6543-413d-99dd-e2ed4bbdde46)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac944a90-d22c-4b4e-b415-043158674bad)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         f6fe6958-3682-40d5-b66e-11e7245fa6a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb791bf6-e0fb-4f54-8f72-ec11953c8ab3)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         2bb77aea-1f64-4156-9b4c-be28dc742178)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6f94fc1-dbee-4c61-8246-bc96141f7636)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         56f1f528-e207-417f-8eaf-881ea1030a12)(content(Whitespace\"\\n\"))))(Secondary((id \
         a16fbc91-fada-48f3-8bcd-85c83e9478b9)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         c6e03c51-47e3-4c3a-a53a-ee9c7d11a59b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a0617ea-7a51-4ca9-a628-2e8cba711b2e)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7823eda6-63b5-437e-8146-fd4cb2254e3f)(content(Whitespace\"\\n\"))))(Secondary((id \
         22954623-a662-417d-bd4c-691bbc1a9730)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         1996b9c8-5ca6-4f30-a9ba-558709801723)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6c33e0a-efb5-48af-8baf-5843219d5f2b)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         a180b748-4c19-49b6-a121-c3436723ad19)(content(Whitespace\"\\n\"))))(Secondary((id \
         80e4b54b-a6aa-46ce-a316-1aed4e11e432)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         940cd3c0-9a28-42cd-a02f-7b184d83cb67)(content(Whitespace\"\\n\"))))(Secondary((id \
         7fc2fce3-32d9-464b-b54a-20d9ac6505c1)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         be8f1f9d-1222-4c8b-95ce-5cf0c7509588)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb1e6f11-861b-4be6-b3c6-737779b5b75f)(content(Whitespace\"\\n\"))))(Tile((id \
         629fb84f-3835-499d-b5cc-e3a333f3622b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0d8cb5a2-f1cf-4252-a32e-789e8f0cbd96)(content(Whitespace\" \
         \"))))(Tile((id \
         a3aa2587-33b6-4c5a-997e-9f3338eb7f42)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b36db800-3037-460c-aec9-0ef7fb8cb00a)(content(Whitespace\" \
         \")))))((Secondary((id \
         205a1cfe-0d39-4685-ae74-471b756d2fc0)(content(Whitespace\" \
         \"))))(Tile((id 5e86ce74-83cc-466d-a9b6-c6c896d02b20)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         70ab69a6-9e06-49ec-9c5f-f945674ea34d)(content(Whitespace\" \
         \"))))(Tile((id \
         882df781-dc65-44a0-8a33-63be2275faf5)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         f00febb0-e1c8-469f-9612-dd79253bc990)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f0aa0375-2e00-41bd-bf95-01acb218b24c)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         b430f0a6-5062-4c4f-a3fe-2632951e9dd5)(content(Whitespace\" \
         \"))))(Tile((id \
         8bad2704-ac29-4232-9484-a01e971745bc)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         932c6204-9cf6-4e5d-8aa6-418b4476f160)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         751514d1-f148-4406-94ce-7e41a9d025ec)(content(Whitespace\" \
         \"))))(Tile((id \
         4935e512-91f5-40eb-ac54-4f4e79df7e5a)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         114b8cd7-95fb-4e56-9ead-58cdfa814a80)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         162d180d-9efd-48b8-8a3c-2e32ae42b1da)(content(Whitespace\"\\n\"))))(Tile((id \
         e024ae70-272c-46be-bb1c-f8e3bf7f9a51)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0cee91b3-fe1e-4300-aea7-2db1456c477c)(content(Whitespace\" \
         \"))))(Secondary((id \
         1176e9e0-4eaa-4196-bdfd-e253be8e1cc7)(content(Whitespace\" \
         \"))))(Secondary((id \
         4186caa5-73ab-4368-bcbc-a0ffec715249)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c8d447d-8e16-40fc-9e99-fcfcc0973339)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3dc074d-27d1-47d7-aa0d-0a88ad096023)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1e1c0e09-5558-4306-992a-dc703b36c522)(content(Whitespace\"\\n\"))))(Secondary((id \
         44438d96-574e-4451-9ddd-6f758876c051)(content(Whitespace\"\\n\"))))(Tile((id \
         a3e176df-3d4f-4944-884f-0bb5f90e8a3d)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         3ab16dde-5c46-4d91-b0c2-64c135ef3b1c)(content(Whitespace\"\\n\"))))(Tile((id \
         f86bdf1c-39d2-414a-97d7-c9dd714c496c)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a9aa20ee-eb3e-4a8d-9213-4426ed21471d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b1598820-c496-4c7f-a3ce-dee3e8c36b67)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9da805de-737e-4cf0-bf13-a6333981fbfe)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dd89e22d-3528-4615-b663-64def83f0a8b)(content(Whitespace\" \
         \"))))(Tile((id \
         9c854004-8b0d-43e1-842a-b2bd1ae51cce)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d1e1d58-f673-41db-adf5-385aeebdf2e0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         81f46c8d-b153-4c9f-b65f-25a10cfe2b51)(content(Whitespace\" \
         \"))))(Tile((id \
         385f3022-76cd-4b4e-8f76-08c42a10aaa6)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6979300b-82d5-46b2-bfd8-66194bafa50e)(content(Whitespace\"\\n\"))))(Tile((id \
         6d85f0f8-e317-4e03-a62a-82e697a6f25c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5fca042a-155b-4ec6-b594-393824c5dabf)(content(Whitespace\" \
         \"))))(Tile((id \
         448df9ee-20a1-4ddb-bf88-d342c62752b8)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88e46915-e36d-4935-b616-0fd41ac76473)(content(Whitespace\"\\n\")))))))))(Tile((id \
         38cd6a51-9654-4721-b7c5-d8e748d63151)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6ba10b55-c2e7-4551-9cf3-675b3a1008af)(content(Whitespace\"\\n\"))))(Secondary((id \
         248a1a6d-f95d-4d32-9e61-34ba02b895b8)(content(Whitespace\"\\n\"))))(Tile((id \
         8ba14d76-19df-4e47-8360-5057266e1825)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         deea30f5-2378-4b27-abd0-bb303d0b5391)(content(Whitespace\"\\n\"))))(Tile((id \
         fab7af85-eeb1-4077-a262-556cf337bf36)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         018d670c-eed8-416f-8d05-18e721e764d4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8bd666d3-2923-4812-811a-10ba5a326b2d)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         437ebe85-8bf0-483b-8d3a-558774dd96ac)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d0360e37-d0b0-4782-a498-c62f6ad8488e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         775e154c-0069-43e0-b300-2eeb15f55f3d)(content(Whitespace\" \
         \"))))(Tile((id \
         f2507b84-030a-4072-9c2a-98058002594b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e5fe672a-2cac-4c4e-a289-2073e6734af6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2dd4c98e-625a-4caa-9db6-79ab02509faf)(content(Whitespace\" \
         \"))))(Tile((id \
         c2127dc0-0d3e-429a-9532-efb04090c402)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         80ebcf0e-0767-4555-bc34-af002c5c57be)(content(Whitespace\"\\n\"))))(Tile((id \
         a5ad383b-e47d-4799-a621-74d9d07396aa)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         167c2d18-7d83-4854-a1b1-a854c90cf43f)(content(Whitespace\" \
         \"))))(Tile((id \
         6779864d-0d48-4012-855b-5e3be56cdb7a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ffb34e74-8bc4-4a3c-a943-c1be60b5225d)(content(Whitespace\"\\n\")))))))))(Tile((id \
         b4b769a7-8030-4454-a3bc-4933a55d4e25)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         93ccabb5-c026-406c-ad27-1c373a9c00c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         0b912d69-e7d3-4314-acb2-fa09ead68251)(content(Whitespace\"\\n\"))))(Tile((id \
         ac4cd2a2-b16b-4715-b854-7829b1f1d7f1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         894ce55d-72c5-4b5f-ab60-bd89099e69e2)(content(Whitespace\"\\n\"))))(Tile((id \
         d9c50f2e-6081-4b4b-8816-882c6b4a0a8c)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a59a73cb-a46a-49e2-a3e1-245fa3993422)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b916f9e8-f679-4fe9-8a20-ad4b5c3b2ea9)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d620f19-c1bf-4cd9-b854-89a3b56b2935)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe2c9cbe-4aba-4566-85a9-f57ff0407c11)(content(Whitespace\" \
         \"))))(Tile((id \
         45320f15-db56-45e1-ab20-35d06445020f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d4ba9e07-cd48-4090-8ee6-871f5420c9a4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c01cf870-f790-490a-866c-e84e5f7b9d7e)(content(Whitespace\" \
         \"))))(Tile((id \
         7d0fbb63-5633-4f6a-8ff7-4e26132495cd)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2c2055d1-45c7-4efa-a1fe-68b7f965556c)(content(Whitespace\"\\n\"))))(Tile((id \
         2df3888d-c306-4a6e-9c8f-fd187b0740bf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         576dab31-afd1-47c6-ac8a-2b3a2fb25fb1)(content(Whitespace\" \
         \"))))(Tile((id \
         00def8ef-ecd0-47e3-b56c-58d6c7833c26)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a54d0305-40b2-495a-9dc6-03712836a6d8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         9bdf5f7b-f223-49ee-82cd-3a2cc24511c1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e79900f-8d8b-4110-a9c7-5389fef51f54)(content(Whitespace\"\\n\"))))(Secondary((id \
         f163337a-1965-4790-8001-f6f9088cb9d2)(content(Whitespace\"\\n\"))))(Tile((id \
         059951ce-acee-43f8-9956-832c9a7b4619)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         21217707-8d9b-4349-ba32-3227bbd9bdd1)(content(Whitespace\"\\n\"))))(Tile((id \
         840c20c0-c320-42a7-8a2e-aeb19575df72)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5c553ae1-a819-4e97-b4eb-f94e64265877)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         92a44514-ce48-48d1-a705-d02a5c4d727b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         22fa139e-ea42-404a-a7cc-af5a0a7deac8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         86fc841c-6aba-4acf-b514-e600337f4cce)(content(Whitespace\" \
         \"))))(Tile((id \
         b5539ed1-25a7-4ce9-aa90-2c35ecec351b)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1194b78a-ea81-432e-b713-de6f6e2d7cb6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a6dd2364-0c32-4bbf-be4e-b8323018defb)(content(Whitespace\" \
         \"))))(Tile((id \
         b05048f3-17c1-405e-9348-59801acc26a2)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         514fee20-6fae-4698-908c-a0ae5ce9be7a)(content(Whitespace\"\\n\"))))(Tile((id \
         d35fe38d-311f-4998-af52-81ffc676f39a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         301bf7b9-a3cf-4533-80c1-b6a39f73f4fa)(content(Whitespace\" \
         \"))))(Tile((id \
         2bbf0bda-f329-4dc8-addf-620fcd501d6d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         914aa135-475b-49ca-b225-d998de0c8c6a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e064c4ed-c067-4fe6-9ea8-227a2e9bee92)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e51e9341-70d2-4167-9bc6-efb63130a992)(content(Whitespace\"\\n\"))))(Secondary((id \
         1e59e5d6-c885-40dc-843b-1f53d9fd6cb0)(content(Whitespace\"\\n\"))))(Tile((id \
         b8c73a9c-932d-4bbf-9fc7-a15e83618cc4)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         d425dfdb-a5ba-4ef1-b434-94c3611b872f)(content(Whitespace\"\\n\"))))(Tile((id \
         5937ed90-1116-4b71-9d28-c08cbd6f34ea)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         839a22fd-8462-4e20-b11c-91e4d542b206)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1ccab16c-05f0-4c6f-8225-a9937d30bf69)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c03f6181-8c61-406f-a4c5-377fd6122a74)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9bf10fe1-51e4-4fb1-aa31-5d595b6ad76e)(content(Whitespace\" \
         \"))))(Tile((id \
         151fd499-9030-49e3-b4d2-d6444358d68f)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2717115d-5124-421d-ae22-3c0b9e598b4a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3f382fb4-ca35-442c-a67d-4292ffb59c92)(content(Whitespace\" \
         \"))))(Tile((id \
         5789a026-5c25-4487-a74d-808bc33fac5a)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1ec38487-e8e8-486f-994c-4affe1f2e71d)(content(Whitespace\"\\n\"))))(Tile((id \
         c22a9cb5-617f-4b8c-97c2-70b9680aa366)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         aba441f2-16f3-44d5-8cdc-0c666fe86138)(content(Whitespace\" \
         \"))))(Tile((id \
         6b6015e9-4e8b-45fe-a23b-c51ea4e19f59)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0b126c3b-b625-4b67-bc21-b9282c089880)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f482a681-4e93-4384-824e-58df27a90e19)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# CLAMP TASK                                   #\n\
         #                                              #\n\
         # Implement clamp: constrain a number to be    #\n\
         # within a given range [lo, hi].               #\n\
         #                                              #\n\
         # If x < lo, return lo                         #\n\
         # If x > hi, return hi                         #\n\
         # Otherwise, return x                          #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   clamp(5, 0, 10) == 5    (in range)         #\n\
         #   clamp(-3, 0, 10) == 0   (below min)        #\n\
         #   clamp(15, 0, 10) == 10  (above max)        #\n\
         #                                              #\n\
         # Syntax reminder:                             #\n\
         #   if cond then expr1 else expr2              #\n\
         #   Comparisons: <, >, <=, >=, ==              #\n\
         #                                              #\n\n\
         let clamp = fun (x, lo, hi) ->\n\
         ?  \n\n\n\
         in\n\n\
         test\n\
         clamp(5, 0, 10)\n\
         == 5\n\
         end;\n\n\
         test\n\
         clamp(-3, 0, 10)\n\
         == 0\n\
         end;\n\n\
         test\n\
         clamp(15, 0, 10)\n\
         == 10\n\
         end;\n\n\
         test\n\
         clamp(0, 0, 10)\n\
         == 0\n\
         end;\n\n\
         test\n\
         clamp(10, 0, 10)\n\
         == 10\n\
         end\n";
      refractors = "()";
    } )
