let out : string * Haz3lcore.PersistentSegment.t =
  ( "Examples / study-write / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         5fdff61d-fce5-4f19-b56d-152a17ded50c)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         0d594431-af86-4e27-ba2f-d8c9da32e2b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         7ac3f6cc-d209-43a2-91bb-69abfa6dfbd0)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         60e53d21-9e2a-40db-946b-a08d0e4e41ff)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b4e9454-34e1-4142-8d07-7c3e88650bed)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         f4f94078-6335-4dfd-a829-2b740c1cd22b)(content(Whitespace\"\\n\"))))(Secondary((id \
         75ee88b7-af56-4f0b-9579-2948b20d85cb)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         7691709a-63dc-4262-bd63-4e1d45c1bb88)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ead6ab3-bb7a-4caf-aaa8-3c1cb4628822)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         c44837fc-907e-4f4c-be14-b84a873047e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5e39f79-4b37-4b0c-8419-ac1f84607493)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         110ad333-d379-4423-9532-8694aefd80e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a22efaa-f839-4923-a190-7f9721aeeedc)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         8815054c-aa46-418f-8456-f1d2e0a5e74d)(content(Whitespace\"\\n\"))))(Secondary((id \
         1a2641f8-dcfc-4880-8abe-5f22799d8e26)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         36958afb-60bd-4d13-b17b-403f0f6a6b83)(content(Whitespace\"\\n\"))))(Secondary((id \
         52138edf-5798-4a38-9351-132dcf3ea5a2)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         02dc3d63-434b-441e-839a-bf004112d550)(content(Whitespace\"\\n\"))))(Secondary((id \
         ad0a4c08-42be-4a64-8a8b-ea886fc7598e)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         670ffe83-fa82-4093-8ed6-7b90d65f1168)(content(Whitespace\"\\n\"))))(Secondary((id \
         35240a49-cbe7-4227-8762-8bf0a9491269)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         4b84caa1-1bd7-400d-aac9-03ec0b0b65bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         c8db5a6f-deac-47a9-b283-0f34342faeb5)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         379c1a19-98cd-4943-aa87-79ab6b133f79)(content(Whitespace\"\\n\"))))(Secondary((id \
         3681b0ca-0a3e-4cc3-8334-c1b3cfafe783)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         0e7e96bc-02e1-49d0-9b24-89f93a50aabd)(content(Whitespace\"\\n\"))))(Secondary((id \
         046657ae-aa9b-44cb-aaf1-5e9fdcbcfd35)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         8891e783-66d5-4dd6-aef3-87ae93489d17)(content(Whitespace\"\\n\"))))(Secondary((id \
         d2a74b5d-7d87-4d88-8bbb-62becb07ca6f)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         deb2a17c-a63c-44dd-a6e4-0b66420b58f8)(content(Whitespace\"\\n\"))))(Secondary((id \
         d0dbdc6d-5b7a-4378-bd6c-31460ec8913f)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         d7b0ccf7-2c72-4d22-843c-cfc3dc8b89b8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c3227faf-1589-486a-940a-b6436daea6e4)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         b8188e1d-ae90-4458-ae76-17014e51a1e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         09d17954-b86b-403b-8888-96e95092c90b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         52db0426-7bad-40ab-a30d-0890b5cca26e)(content(Whitespace\"\\n\"))))(Secondary((id \
         efe4ffe6-0ae6-431d-affc-9fbf314e440d)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         8d99748a-a00e-44c9-a3be-fe283266e9c8)(content(Whitespace\"\\n\"))))(Secondary((id \
         5164cea6-ca7a-4328-9cbd-3e5ba29c1580)(content(Comment\"# to see which \
         branch is taken for each test.  #\"))))(Secondary((id \
         cea99c77-74da-42d4-9789-991740f13972)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba7cdcb0-d090-4dd0-9132-a0be490a04df)(content(Whitespace\"\\n\"))))(Tile((id \
         ec7d69eb-28d8-4a38-8234-72c6deb6573d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         40))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d41594c0-12bb-4376-a276-a922c0bb4b3f)(content(Whitespace\" \
         \"))))(Tile((id \
         49308fc0-94d9-44d8-ab12-2d80b76634a1)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         15b8181a-9262-4bbb-bfec-7c2e00331b60)(content(Whitespace\" \
         \")))))((Secondary((id \
         e964b043-f76d-4b78-8c32-5a2392e6d902)(content(Whitespace\" \
         \"))))(Tile((id 6458f720-8e3b-47ae-be2d-40937f2604e6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 36))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         a8e10712-b8ff-4098-b8d1-30c5874696df)(content(Whitespace\" \
         \"))))(Tile((id \
         007b216b-09d2-41e8-b989-1b3d908c8265)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         50d65626-4512-4c8b-b2a6-7a5e6686b0f4)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         f6cee689-24fa-47c2-86cd-1f16d97ad558)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         8541773b-02b1-451b-a18d-d346b18f38dd)(content(Whitespace\" \
         \"))))(Tile((id \
         654ae02a-7e88-44af-a7ab-b7b2ac33ad7c)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7096ca1c-e606-409d-b1e1-43812a6d0171)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 47))(sort Pat))((shape(Concave \
         47))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         0cfaad77-931c-42d8-a27f-8d47b1adf660)(content(Whitespace\" \
         \"))))(Tile((id \
         e7eb1961-5453-4593-bf44-96d38d269767)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         756dc6be-b495-4f06-ad5b-b94e64ffbc7f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c3f9efda-e2e3-4c42-bfe3-22dde3f1713a)(content(Whitespace\"\\n\"))))(Tile((id \
         2a463577-a7c4-439c-93c1-85cceb7db744)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0d67ff48-ea69-4865-8e1c-cd1a8c9bb172)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         cdcc1ad4-bc0f-4492-bedb-873691ba77f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb006e57-8149-4424-b3ea-0ac07e606491)(content(Whitespace\"\\n\"))))(Tile((id \
         80317afa-f21d-48fe-88b1-6ea8fbe8d180)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f3f0f649-b611-45fa-84ce-092f8f5b29f4)(content(Whitespace\"\\n\"))))(Tile((id \
         d9eb05fc-3183-401f-b26c-9d0ff9936494)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         69642329-0456-41a5-8d89-fe8e9f41fe8f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29037383-094b-41ff-a978-eea2618fdc37)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e2701426-67b7-4c8b-afc8-4dd6da8e517e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         096e9f83-5953-4a00-8f6a-3bcdedb90442)(content(Whitespace\" \
         \"))))(Tile((id \
         5253c7f4-8d2d-439d-be9e-e01340e4cb90)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9ce0638e-7122-4226-bb54-99c504d5fb94)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b403a76-630b-4458-a9b1-b2d3fefd2af9)(content(Whitespace\" \
         \"))))(Tile((id \
         23ed37ea-66ac-495f-8a2e-46a73a1e475d)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         eb6d1ea6-cfb6-4b99-9aae-250d9ec1600d)(content(Whitespace\"\\n\"))))(Tile((id \
         3f4ea67c-7bc8-4627-911d-aabb602ab68c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8a7121cb-aa9f-49c9-bfa0-c3633f0d4645)(content(Whitespace\" \
         \"))))(Tile((id \
         6ea8f6ac-09ae-4586-93b0-4669d3a03b4d)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a81de139-f776-4a57-b45d-1617ba0631eb)(content(Whitespace\"\\n\")))))))))(Tile((id \
         4a65a33f-e27f-4042-bf7c-785213307ebb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c53a3f8-22bf-47af-87da-2a8b07c05fdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         fbadd971-eae2-420b-a4ef-15ec055dfaf4)(content(Whitespace\"\\n\"))))(Tile((id \
         d6f25e14-66ea-43da-8437-f166c8cf0b2b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e230448e-7725-41b1-ba88-7c84444475f4)(content(Whitespace\"\\n\"))))(Tile((id \
         1fbf8f79-7be3-472b-8a40-6cee6d7b7324)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b2b35e4d-c5ef-4bdb-aa24-3a99fce3758e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         add84016-24ab-4d87-a1f5-c20e7bc47e4d)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         54314063-723f-4dac-a7bf-c3c0bc5681e6)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c7fe9b61-cfd7-44f7-9b44-868f8fb9aaf4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e99e6117-7f4e-4acd-b881-bf5754012227)(content(Whitespace\" \
         \"))))(Tile((id \
         0d0ddb03-e7bc-4905-b656-d5ee7837de90)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e85b78c7-55c5-4b15-b59a-babf599eb063)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bb31bce6-f338-441d-a4f5-85c58acef71a)(content(Whitespace\" \
         \"))))(Tile((id \
         f55a1e6f-0042-40c8-acd7-87a0170220ff)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         389c6d72-894b-41b8-b3c0-5dfba7f20a21)(content(Whitespace\"\\n\"))))(Tile((id \
         422f8f9d-4f7f-45b7-b119-80212025b09a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1bdbfda6-ba38-48fc-b466-b4f994ab9378)(content(Whitespace\" \
         \"))))(Tile((id \
         274237ce-2d3f-456b-b9d1-6691e4576b26)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         488a9e14-5a27-45e8-97df-564747623d4f)(content(Whitespace\"\\n\")))))))))(Tile((id \
         f93f5691-f119-4ba6-b8d6-684a5010cf2d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         06846266-00b4-401b-99f1-0908d1815bc2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3452a9f-9f02-4b98-8e97-3c26269cffa5)(content(Whitespace\"\\n\"))))(Tile((id \
         5da69eac-48f0-46e1-96d1-bcae20a03318)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         459f7f7e-8ad8-40c6-9d66-475d995a47aa)(content(Whitespace\"\\n\"))))(Tile((id \
         ab44847e-a136-4652-9877-c33a0327ded7)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ff68ec5f-4eed-4da0-bad8-f1417a70ae23)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3224279f-8b2a-4b47-90c8-a98a0d90c64a)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         19d1493e-2df5-4994-b511-6a7ec744a179)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         baa55a43-9b30-4813-9624-1ce8ad63c99e)(content(Whitespace\" \
         \"))))(Tile((id \
         458fd135-dcd2-482f-bd9a-68fc27c2a816)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a12af38d-d56a-4fff-90e8-e87ea07697c8)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de61e71e-f940-4411-b98d-49bb767f2fdd)(content(Whitespace\" \
         \"))))(Tile((id \
         fc79d9e0-5e62-4717-a959-81bb7d4ebdc5)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c825144d-3c3e-47f4-86bb-f86b1459d877)(content(Whitespace\"\\n\"))))(Tile((id \
         9eabfa0e-799a-430e-9a1e-0021d682dc18)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         430ff750-1d8d-4182-b874-00ba364fd346)(content(Whitespace\" \
         \"))))(Tile((id \
         5b7cbede-8a4b-4ef0-973d-f27179ffdd46)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5db73ce1-0796-4ddf-98f4-801e0459b6b8)(content(Whitespace\"\\n\")))))))))(Tile((id \
         14e0260b-ccfd-4140-894a-615aa0411d5f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf5843f8-1301-4771-8e08-5a00c6448f0c)(content(Whitespace\"\\n\"))))(Secondary((id \
         40a6a0ab-64ab-4e61-8331-4a7d23ddc59f)(content(Whitespace\"\\n\"))))(Tile((id \
         a600d6ee-e23b-4971-a44c-1ad8217ecbca)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c90070ec-7c4b-41a1-bb2c-7f3247a8e3ac)(content(Whitespace\"\\n\"))))(Tile((id \
         1698cf8a-994a-4a38-8bd7-8f3d8e7a30c0)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         85c95924-a421-4120-b73b-7dea827c0253)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4b29fa33-640f-49ed-a7f1-b1ca9041f76d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ee2c4dae-758a-4efa-b1ce-668d250c149e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         144acbc5-48c1-46be-a99c-b8ed30c09133)(content(Whitespace\" \
         \"))))(Tile((id \
         30a1ffdc-814c-4ccc-bdc5-5ad377870fac)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d47fca45-50d8-4ab1-a937-45936b579871)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe4f5675-cf7e-4563-adb0-b3dc39a144c0)(content(Whitespace\" \
         \"))))(Tile((id \
         abb9c8e7-6bd8-40cb-85af-786a0d4bd025)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2b51f305-0251-4728-bd6b-ad601cad4c8f)(content(Whitespace\"\\n\"))))(Tile((id \
         bee12551-a1a9-47e9-8f66-ede7f20cb9ce)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2e9601f3-5659-49d3-96a6-fb6db42b2a4e)(content(Whitespace\" \
         \"))))(Tile((id \
         bf2120d7-67e2-4e60-a96c-c72463f447d9)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b2b1010d-05e2-4e2b-8521-ecf4d0e8577a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         46586082-929b-41b5-9602-41635eb593a5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 38))(sort Exp))((shape(Concave \
         38))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d92ffcc1-4ae4-4fd4-8ffa-1677c0224611)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b940e3a-52b8-416d-b797-7ed6df041eea)(content(Whitespace\"\\n\"))))(Tile((id \
         68c12c43-cc9a-435b-a131-f3a7e932dd6b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         158a0ad4-4a8a-4d4c-9fc8-f23fb6f0b6c5)(content(Whitespace\"\\n\"))))(Tile((id \
         b65ad4da-d410-4ccc-abc5-abdfad5d7012)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e98d04f8-415c-4617-8d05-27340d624843)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         79f334e5-732d-4c71-96bd-928be29f5c04)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         890ead1e-fcfb-46ce-b28c-a4a3680d2d20)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         87083750-ae4e-45b8-ab1c-f6e77062afc7)(content(Whitespace\" \
         \"))))(Tile((id \
         60064f2d-af0b-4f36-8720-8fc383936997)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         dcd9a10f-7614-4ac6-a3ac-742ff1633590)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 47))(sort Exp))((shape(Concave \
         47))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         71790743-20d4-4f78-976c-5525940e0250)(content(Whitespace\" \
         \"))))(Tile((id \
         39cad287-bef9-433c-a7fc-e952c60d0a16)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         e531835a-4a44-4181-b7fa-3ee00e63faf8)(content(Whitespace\"\\n\"))))(Tile((id \
         1a311a6a-fdf0-4a3b-aa3d-1000a4de1fe9)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25822648-22c7-45e4-bb4a-43e1e4c77877)(content(Whitespace\" \
         \"))))(Tile((id \
         cd34cefb-c6ed-4e7b-a9ed-1eaba3a464e0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         62975433-162f-44aa-8f5f-8de9a4a2bb65)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9406dc1c-72f6-481d-80be-a09593772a1d)(content(Whitespace\"\\n\")))))";
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
         #                                              #\n\
         # Tip: Turn on auto-probe (microscope toggle)  #\n\
         # to see which branch is taken for each test.  #\n\n\
         let clamp = fun (x, lo, hi) ->\n\
         ?\n\
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
