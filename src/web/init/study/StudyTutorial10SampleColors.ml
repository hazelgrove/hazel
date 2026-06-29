let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 10-sample-colors",
    {
      segment =
        "((Secondary((id \
         3916a8c1-5440-4d16-8351-83b5b8b6be62)(content(Comment\"# PROBES \
         TUTORIAL - PART 10: SAMPLE COLORS                        \
         #\"))))(Secondary((id \
         2187605c-54d1-4986-85cb-b24b30033132)(content(Whitespace\"\\n\"))))(Secondary((id \
         c5f1c305-8a41-45cf-b48b-ce9066ab2a71)(content(Whitespace\"\\n\"))))(Secondary((id \
         5b525ac2-4ac9-436b-aa9f-22867e4b76d5)(content(Comment\"# When you \
         click a sample, other samples change color. These      \
         #\"))))(Secondary((id \
         229a8151-9a8b-46fc-bbe3-cc664c3f5554)(content(Whitespace\"\\n\"))))(Secondary((id \
         573bc6dc-5813-4d2b-877d-a5f692c69285)(content(Comment\"# colors show \
         how each sample's evaluation relates to the one     \
         #\"))))(Secondary((id \
         414bb7ba-a468-4bf0-a752-f49f66079232)(content(Whitespace\"\\n\"))))(Secondary((id \
         d939deb4-61d6-4593-a853-220c36382084)(content(Comment\"# you \
         selected, whether it ran before, after, or nested          \
         #\"))))(Secondary((id \
         b92e9ab5-2986-49d4-a81b-fed51138a49a)(content(Whitespace\"\\n\"))))(Secondary((id \
         e571f796-6c5f-4249-88ef-41180aef1f54)(content(Comment\"# inside or \
         around it.                                             \
         #\"))))(Secondary((id \
         344daea5-041b-4b2f-8ed6-22db786d2e5b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bec275aa-9d5e-42f6-90ba-6834a08c5af2)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         7fac4e5b-51ad-448e-bd28-ff177e2cc53e)(content(Whitespace\"\\n\"))))(Secondary((id \
         46834d4e-9a84-4a31-aa76-fef66a9fb1f0)(content(Comment\"# Look at the \
         LEGEND at the bottom of the probe sidebar.          \
         #\"))))(Secondary((id \
         71473fa7-afab-4cc5-99d6-3c11938a3249)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d87d362-5dc4-4dd6-8fa8-f62d9fd871b5)(content(Comment\"# Hover over \
         each entry to see what it represents.                \
         #\"))))(Secondary((id \
         fedcf60e-c3e9-444b-8361-d15a055401bc)(content(Whitespace\"\\n\"))))(Secondary((id \
         e47b0728-52e0-440a-b59a-05d6313bffd6)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         db537b9a-9aa6-4d36-9af9-640ca2941baa)(content(Whitespace\"\\n\"))))(Secondary((id \
         c86635a9-8265-4add-b71b-65298060b16e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d4ace7b-7773-4d73-960a-838ff40b7630)(content(Comment\"# BEFORE AND \
         AFTER                                                 \
         #\"))))(Secondary((id \
         db2d0cc2-122c-40e4-8929-84d39d7c3cd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f72d0e29-2d74-4270-bc7e-1f50cb14ae58)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         8e7cdfce-bd7a-4a64-94ac-1a8e9e223c33)(content(Whitespace\"\\n\"))))(Secondary((id \
         efba37d1-13f8-4d00-9eba-ba18a3086116)(content(Comment\"# The simplest \
         case: expressions that evaluate in sequence.        \
         #\"))))(Secondary((id \
         3ed858a5-2209-4f30-b2ed-0158bb0149b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         d688d406-f83d-49ce-9c92-14914fa89a60)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         16e8a823-95f4-4155-bf8d-dec7147e8479)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab1e35d5-2957-4587-846c-f9066dd1a122)(content(Comment\"# TRY THIS: \
         Click the sample for `second` below.                  \
         #\"))))(Secondary((id \
         9c05b909-6a05-40cb-8072-729684ee8b58)(content(Whitespace\"\\n\"))))(Secondary((id \
         bbc95619-eeb5-4f8d-a229-a388ff07a395)(content(Comment\"# - `first` \
         shows a Before color (it finished evaluating earlier)  \
         #\"))))(Secondary((id \
         b3fb3653-0297-4004-892f-74d13fd151a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         11d47fff-8d82-487c-88bd-f3381087763f)(content(Comment\"# - `third` \
         shows an After color (it hasn't started yet)           \
         #\"))))(Secondary((id \
         127f2940-e02c-4068-ad44-5f13ae1e6293)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e9d166f-699c-4179-8751-1795d613d0d6)(content(Comment\"# Click \
         `first` or `third` to see the pattern shift.               \
         #\"))))(Secondary((id \
         674ed90b-b1a5-41d7-90fb-6555dea46587)(content(Whitespace\"\\n\"))))(Secondary((id \
         7661d806-95b1-4c47-a49d-e19f2de97dca)(content(Whitespace\"\\n\"))))(Tile((id \
         828bc196-f42c-404b-bedf-6aa9e81d85d9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b54774a1-b2ac-42bb-ac1a-71a487411a5d)(content(Whitespace\" \
         \"))))(Tile((id \
         b4da253d-f608-4749-99a3-cead66b81a8a)(label(first))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         89320cc9-aae1-4f0c-8fca-4137b7b3bb7a)(content(Whitespace\" \
         \")))))((Secondary((id \
         d6ec97af-5286-428a-a9df-3485c9af7916)(content(Whitespace\" \
         \"))))(Tile((id \
         786aa611-bad6-418b-9f47-eead67402263)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         2b6754e8-6e58-4ef4-9833-7ad939d2e7dd)(content(Whitespace\" \
         \"))))(Tile((id \
         9d955c56-1b5d-4d12-b5dd-852401ae122a)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         df7a5f0c-3d57-4544-9153-181b80037ec5)(content(Whitespace\" \
         \"))))(Tile((id \
         c9e950e1-cf6c-43af-aee1-49e34e7ed7d4)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         262886a1-44d2-468d-8a7e-4604674b3ae9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         95f5219b-3b29-46b8-be9c-d0ac95d581fc)(content(Whitespace\"\\n\"))))(Tile((id \
         82410e75-674b-45c3-a205-fbb8f138b069)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         404c08a7-df84-4bbf-b624-470ef8497d67)(content(Whitespace\" \
         \"))))(Tile((id \
         3532899c-8b07-4f41-acdc-e3f6118636a3)(label(second))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         165bb4e8-c422-4859-8e67-942c55f8f006)(content(Whitespace\" \
         \")))))((Secondary((id \
         c85e517f-fbe1-438c-a1e5-dd2ed8e743d2)(content(Whitespace\" \
         \"))))(Tile((id \
         9b788b3b-f4e9-48ac-a62a-608c7f9a7158)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         12199cab-4ef3-4c1f-a0f4-b95f54c8e893)(content(Whitespace\" \
         \"))))(Tile((id \
         a98fdbbe-8016-473d-b4de-48c4998fd280)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         483d1a23-d3b3-4e0f-967c-fd05fa242df1)(content(Whitespace\" \
         \"))))(Tile((id \
         ab5466d4-f3df-4095-b451-25be972a831a)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d449672e-4a98-4bb1-9d14-792f77f1f0a9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         42a72188-c396-400e-8a31-e9c58403e5b0)(content(Whitespace\"\\n\"))))(Tile((id \
         b0a4c670-b920-4f22-b2cc-e881efad633b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         eaf2f9a5-86f7-4038-beb9-0eabfd80ff7f)(content(Whitespace\" \
         \"))))(Tile((id \
         157b1553-fe09-4a73-b224-850313b676b3)(label(third))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         87077389-a382-4cf2-90f7-4c6e11114eee)(content(Whitespace\" \
         \")))))((Secondary((id \
         1905c496-62bb-47e9-ab10-1d16293c7c29)(content(Whitespace\" \
         \"))))(Tile((id \
         c1491dae-bf74-48d0-9765-808396a6d2e7)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5d42b0a8-7550-47fb-b044-2bba1c1fa7b6)(content(Whitespace\" \
         \"))))(Tile((id \
         f77a319b-6041-4762-82b2-68e96f5f3cb0)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d3a7e0bb-f7d6-4f07-bc0b-8da4f2d8d0fe)(content(Whitespace\" \
         \"))))(Tile((id \
         c6d8ba56-a86f-4db1-8b83-30cc09cf563f)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4634826c-a889-4dce-a865-913d512ae0d6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         120b9480-0a38-4bdb-8632-2ec6378dd9fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         752a50ae-8af4-497a-90fb-96ee35e9dc2b)(content(Whitespace\"\\n\"))))(Secondary((id \
         01e0f9b1-fb6e-4410-a483-cfb82e631372)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         feb6be52-0fbc-48f5-b27d-3bd49af9da2e)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbfd538f-6d6b-469f-a2a6-50cda834b2e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         74a5bf8c-4997-42cb-9523-85d2a61768e0)(content(Comment\"# CONTAINS AND \
         INSIDE                                              \
         #\"))))(Secondary((id \
         c8204dc4-8974-4af1-ab86-5ad446304819)(content(Whitespace\"\\n\"))))(Secondary((id \
         af43e539-725a-4c2c-8890-2733484a7569)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         c719f6d5-9f08-41d2-b48f-1a763af311e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         97db2d27-8ee1-4dd4-b805-c4528cdb1ff4)(content(Comment\"# Evaluation \
         isn't just a flat sequence... it has depth.            \
         #\"))))(Secondary((id \
         02801808-8298-47cd-92bc-4bd600a17a7d)(content(Whitespace\"\\n\"))))(Secondary((id \
         73b455e4-118a-4850-af6a-7ab544e43d07)(content(Comment\"# When you \
         call `daily_water(250, Full)`, the call starts,        \
         #\"))))(Secondary((id \
         a51c642b-fd63-47b5-a379-63da5e1edcd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         afa634e8-4e1f-436c-b869-1f772383523a)(content(Comment\"# then the \
         function body runs, then the call finishes.            \
         #\"))))(Secondary((id \
         e23ed2e4-8a01-4e72-9951-201698c91ce2)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c47a88d-4e5d-4130-983d-f52ffc9e139b)(content(Comment\"# The call \
         *contains* the body; the body is *inside* the call.    \
         #\"))))(Secondary((id \
         6911538f-750b-449f-85e6-ec761cc0518e)(content(Whitespace\"\\n\"))))(Secondary((id \
         943e9867-9798-4637-b7ab-ee8827d27f51)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         93dd0eb8-0d99-40f3-ad00-bff40b5d4cc0)(content(Whitespace\"\\n\"))))(Secondary((id \
         3adf3391-e8b3-4ee4-bac1-65c6d0cbee8a)(content(Comment\"# Contains and \
         Before share a color because both represent         \
         #\"))))(Secondary((id \
         8202be6e-df76-44df-99ed-fe1164ca9aca)(content(Whitespace\"\\n\"))))(Secondary((id \
         599986d8-2ca0-46bb-8059-5eac7750eaaa)(content(Comment\"# things whose \
         evaluation *started* before the focus sample.       \
         #\"))))(Secondary((id \
         7128928d-7ae2-4590-ba9f-88ec1710f79f)(content(Whitespace\"\\n\"))))(Secondary((id \
         196d2ea4-e35b-4732-9f4b-575ce7f153fa)(content(Comment\"# Inside and \
         After share a color because both represent            \
         #\"))))(Secondary((id \
         4de254d4-2252-4179-b271-5c694705c77d)(content(Whitespace\"\\n\"))))(Secondary((id \
         011b4e34-889b-4b0d-a5cf-b782656d77be)(content(Comment\"# things whose \
         evaluation *finished* after the focus sample.       \
         #\"))))(Secondary((id \
         6ad304df-0d7d-4cfc-9e7c-38f41dddc141)(content(Whitespace\"\\n\"))))(Secondary((id \
         0246b53c-8704-4073-b7be-c2a2afb3e47b)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         5ccd0a7b-8c80-4442-858c-285b4f623d7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         686bc019-ffa6-4548-9ea9-9f840133ffbc)(content(Comment\"# TRY THIS: \
         Click a call sample at the bottom of this section     \
         #\"))))(Secondary((id \
         2eed2082-5de3-4c79-a06c-09dab15c2f59)(content(Whitespace\"\\n\"))))(Secondary((id \
         deec8069-807f-4366-bf89-0a665911dc05)(content(Comment\"# (e.g. the \
         first one). The body samples above change color       \
         #\"))))(Secondary((id \
         7105b07d-c7ff-4637-a8ea-8628a11dcbef)(content(Whitespace\"\\n\"))))(Secondary((id \
         357466f2-16bf-42f1-82ac-19e2be1bd1c5)(content(Comment\"# to show they \
         are INSIDE that call.                               \
         #\"))))(Secondary((id \
         47ab8be4-1f44-4d99-a884-2f33ea43cdaa)(content(Whitespace\"\\n\"))))(Secondary((id \
         7707a645-866d-4b1b-807a-cfa91131eaf5)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         6c7fefd1-cc21-4036-a8df-932bd7829af4)(content(Whitespace\"\\n\"))))(Secondary((id \
         ed610848-cbba-430a-a010-6b49571899db)(content(Comment\"# Now click a \
         body sample instead (e.g. `adj`). The calls         \
         #\"))))(Secondary((id \
         f11fdac7-f39b-4231-826d-bfef9598aa5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         d837daed-ae35-45d4-b339-94c76fe7fe66)(content(Comment\"# below that \
         launched it are colored as CONTAINS.                  \
         #\"))))(Secondary((id \
         e155ecf0-b28d-4e3b-b441-a1124ba4563b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a7155ad-8428-435a-802f-888a2983ef32)(content(Whitespace\"\\n\"))))(Tile((id \
         633f2d08-cf61-430b-a46d-e7323b171616)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         54597db9-f265-4e3f-9a87-2a9df905aae0)(content(Whitespace\" \
         \"))))(Tile((id \
         b794ad94-73bb-41f3-9e36-ef666a3bff0b)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         00149963-1c4d-4382-a6db-b64f22a04911)(content(Whitespace\" \
         \")))))((Secondary((id \
         ae710819-8ecd-47bc-b23c-7d861d09ed24)(content(Whitespace\" \
         \"))))(Tile((id \
         c718bfd5-73a0-478a-87e5-c6fb75329422)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         256a9031-12c5-42a7-94dd-b7e8cc47b40c)(content(Whitespace\" \
         \"))))(Tile((id \
         629385a9-4971-4a6b-8be6-6c0bb6f807a7)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         8fb507ef-7f29-434c-ae27-ae73582a4d37)(content(Whitespace\" \
         \"))))(Tile((id \
         69e471c9-93bb-4c82-9a46-3d39c722ea1d)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4afa70a4-b525-4aee-b34a-a29c50f07774)(content(Whitespace\" \
         \"))))(Tile((id \
         04b4b9e0-07e6-4677-9e43-9d288121f07a)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         c1f5ee66-9e62-426f-85e0-d6f6595a8eee)(content(Whitespace\" \
         \"))))(Tile((id \
         a685044c-745b-4e0f-8517-c0f822ed2140)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         baf58dac-3f66-4c69-9104-6598a51805b6)(content(Whitespace\" \
         \"))))(Tile((id \
         6c3a6a11-77e0-47d2-b344-cfe0e74e86f2)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a5c1ca37-26ca-4875-9971-652bc7a6bbe1)(content(Whitespace\" \
         \"))))(Tile((id \
         2a7b6b01-8ba7-4705-a43f-64507c20197e)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         98426897-4ef1-4db2-9241-6694513b9ccb)(content(Whitespace\" \
         \"))))(Tile((id \
         19fa9ecc-77ef-499b-9f11-f7ae36ebf2c7)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9207c303-7932-44e5-9d11-dbb941de65aa)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2a348c79-308a-4daa-8854-56c37fb08880)(content(Whitespace\"\\n\"))))(Secondary((id \
         881c1d76-2c45-42af-8898-33a3c79bf3d2)(content(Whitespace\"\\n\"))))(Tile((id \
         2a9f30f6-7717-4c5e-b678-30e0a401d9fa)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         0e66c432-5dae-42b0-897c-889db6dcbee7)(content(Whitespace\" \
         \"))))(Tile((id \
         464421e3-103d-4de9-bd67-647bd88f52a0)(label(phase_mult))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         edd17265-193e-4107-8b8a-779bf4b71fc2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b5e5dbcd-f7c6-4d22-bbd2-a527036a153b)(content(Whitespace\" \
         \"))))(Tile((id \
         962673e3-51dc-4f3b-b3c7-1999786714d2)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         a77ee115-07c6-4de7-b72c-11848342bca2)(content(Whitespace\" \
         \"))))(Tile((id \
         741ad30f-31fd-4c87-a87a-fc9e389eae10)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cf5498fd-945d-4bd5-89ee-48ac6bc7fbc4)(content(Whitespace\" \
         \"))))(Tile((id \
         23272cd5-7d36-4c5c-8de4-aee7e19e120b)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         aed95caa-6f73-41bd-ba1a-f64a54d5f0cc)(content(Whitespace\" \
         \")))))((Secondary((id \
         75faa363-6e20-435a-9b0d-ebae1ae4c620)(content(Whitespace\"\\n\"))))(Tile((id \
         c582c232-66a1-4bf3-ab21-33a4813fa675)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         afb3a4eb-94f5-480e-b41e-d8bc53d625c4)(content(Whitespace\" \
         \"))))(Tile((id \
         447e34c9-7564-47f2-80f4-9aef528e8234)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         36b80830-6f43-4770-a87d-d31f1d92562c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         10f1bb44-1e83-46aa-b2a3-7280821e9b6b)(content(Whitespace\" \
         \"))))(Tile((id 31cb6dcb-a1bf-4b90-9e1a-0f45741c16e6)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d5431d2e-e4f8-4983-bc74-5163584d2a08)(content(Whitespace\" \
         \"))))(Tile((id \
         8df77ff7-c6c9-4516-b9d9-47c2bbf98d19)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a71e7fba-28e1-45da-8422-ed10eec484af)(content(Whitespace\"\\n\"))))(Tile((id \
         09641304-adab-4b78-b816-3fc251f8c2c5)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6a96d3ae-69f7-4d59-95fc-da2879ebea9d)(content(Whitespace\" \
         \"))))(Tile((id \
         ab9a1f53-8594-45ea-9de7-3eb2d75a80a2)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5607df98-4919-41c0-9772-768ba0acef46)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         05fcc7be-e992-4804-b4ac-19b75d98fd0e)(content(Whitespace\" \
         \"))))(Tile((id \
         5cc1a654-fdb6-4a60-a232-826dc9212656)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9538667e-a67a-4022-a731-144ed9af2c71)(content(Whitespace\"\\n\"))))(Tile((id \
         49262c77-2888-468b-9552-121cc1c8efb0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9f518cae-d64f-4ac4-bfa7-fb24fc528d7b)(content(Whitespace\" \
         \"))))(Tile((id \
         33393d4a-8690-49b3-89b1-03527dabc132)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0925499c-7c3e-4b9b-a47a-59eff05ebd1f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4e17f30d-3361-4329-853a-d8984a297ef3)(content(Whitespace\" \
         \"))))(Tile((id \
         9575d652-ec79-4bea-88d1-111551e6c8c1)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c86bb358-42a7-4aa2-8ab3-574d7219daea)(content(Whitespace\"\\n\"))))(Tile((id \
         bacf1198-d48c-4a7b-811b-13392074b9f8)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         bea79b9c-c729-4af2-b32d-9a5619356259)(content(Whitespace\" \
         \"))))(Tile((id \
         ec15af00-ee1b-4bc6-80ea-30e0ce26ce7e)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0e5968d6-afa0-43b7-b4a1-2af5edc6719e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         65edd1a9-2411-41e8-bbc7-aeb5b2603081)(content(Whitespace\" \
         \"))))(Tile((id \
         3dfea58c-4812-406b-8535-772e03ef0082)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c98b150a-c2d4-4c68-8d43-8eda2af30023)(content(Whitespace\"\\n\"))))(Tile((id \
         44f171d9-c49c-4ff7-8444-12dd38b2a529)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3fbe704e-0ecc-45f9-85d6-d8e2df642de8)(content(Whitespace\" \
         \"))))(Tile((id \
         c49966f4-6d3d-4f58-a6e7-96f6c7695c8e)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         905cc334-4ef1-49a5-9662-cedb53a4dc1b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4d028a5b-e0c1-4b76-a7a8-bb2dcc4fe020)(content(Whitespace\" \
         \"))))(Tile((id \
         3b9d8622-8961-4c50-bff4-c2f2be46618f)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8cd0c4c0-f0ff-4e9b-9b31-accbc31e3cbf)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         bd2e41e0-3bd3-407f-aacd-4c9e4ddc57b4)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         505faee2-39e7-4cdd-9169-b1901582036d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9a2b50f4-9265-4cc1-8d5f-a8a749c3a94b)(content(Whitespace\"\\n\"))))(Tile((id \
         4e20f44e-257c-4dad-b472-0b906d40dc3b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b5a27b93-36cd-4a4c-ad61-78c9f004ac96)(content(Whitespace\" \
         \"))))(Tile((id \
         5ef4bdce-6a53-4601-90cd-7c0db8a5c2f8)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         a03a6d83-50f8-4a2f-8108-4106ec59ab28)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         7789415c-86e7-438c-904c-c9b6329b5c0c)(content(Whitespace\" \
         \"))))(Tile((id \
         3741887f-ace2-41df-8eff-db4e9785cc6f)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         f73194ba-bc95-4eeb-9b17-b294000d67c2)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         05c6f13c-738d-4cec-8531-2db80fb450d1)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2c89fce4-87c4-4153-b227-56f59c52789a)(content(Whitespace\" \
         \"))))(Tile((id \
         7cc1413e-ef80-4ffe-95ba-e5ccabdfefec)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         b9660825-5cab-4c3a-82de-d0b19cc5f0cb)(content(Whitespace\" \
         \"))))(Tile((id \
         44236578-7929-4dee-90cb-d5fa20e25adf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         64969e49-ce96-44e3-95d8-15611e9eeab5)(content(Whitespace\" \
         \"))))(Tile((id \
         0f07306c-1701-448a-8ce2-42b553722183)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9561a482-f56b-47fd-9c16-4ad9d4245f8e)(content(Whitespace\" \
         \")))))((Secondary((id \
         376c318f-aa19-4430-a14f-a3fc90b69ab2)(content(Whitespace\"\\n\"))))(Tile((id \
         5a9d7405-7ed3-4654-b4c8-4436aa03010f)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c4a97cf7-b1e8-4904-9d76-21f228abf6b4)(content(Whitespace\" \
         \"))))(Tile((id \
         dd815938-074d-4913-a556-dc21f77a6c0b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         010d5868-8274-4a6c-bff5-ec93e881d06b)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         ee461019-eec3-41b1-8aef-6d70c767e3bb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         556950fb-9503-45e1-b16b-b86fb30e52f6)(content(Whitespace\" \
         \"))))(Tile((id \
         1e011862-eedf-4b12-8329-bcaca18599ce)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         398787b7-26c8-464f-b036-38e06cf6add1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3a3072c8-960f-495d-8240-f5a31f4aaa7d)(content(Whitespace\"\\n\"))))(Tile((id \
         b70b9974-e0fb-466b-b6ad-bdaa675f41b6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         8bce9c8f-359f-4331-8671-1aebc42d4c7d)(content(Whitespace\" \
         \"))))(Tile((id \
         0eeea983-2e55-4e40-b3c6-0a1e1a112342)(label(adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d6358ae5-485a-4f6b-89f6-eafbc78d39cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         d4b9568d-52ba-43e9-8a69-6c5026a90795)(content(Whitespace\"\\n\"))))(Tile((id \
         298197d3-6333-4dcc-a893-852207ee29bc)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e88d790b-07c3-4f6a-941e-ca4daa8ef884)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         96832613-4fa9-43d2-a718-4697ad0a7171)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c9742082-3f80-4328-8dac-fa7d996d63ba)(content(Whitespace\" \
         \"))))(Tile((id \
         d4a6a9d5-e9d5-480e-893f-bd9912478108)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         42acb3fd-7022-4015-a822-fee2d78b0a64)(content(Whitespace\" \
         \"))))(Tile((id \
         ee28cd2c-5705-434c-b5d4-ed09becbd29a)(label(phase_mult))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e451f9cd-7e95-47c9-ac39-ea236417a188)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2e69e45-ff30-4fe3-adb2-6b7d72f057d4)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         19c180c8-3f93-4c71-8e74-cd9d86a0c875)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9355575e-7bf3-4b9a-9d4d-06258988033f)(content(Whitespace\"\\n\"))))(Tile((id \
         829842c6-c81b-455c-99a9-0ecbb1a193cc)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c7f76a1-56c2-4d3b-bfd5-34a416ddd43f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b2524b42-4816-4d68-811e-5cfef8d4a0fe)(label(adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         895ba29f-7123-4aa9-bfea-ccb0e4fd6617)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         8fc819b4-d11e-4a0b-8c30-a6b14133cc41)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd1d88a6-683a-4672-9271-9120dd511e39)(content(Whitespace\"\\n\"))))(Tile((id \
         dacc2579-d7c6-42f0-904e-657b41ee8a2b)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         75740b3d-85f8-4540-b8e7-3380b05ad8b5)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a2217157-4bd0-42a0-abde-80fc5dd04430)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91c12749-cc13-4c49-830f-6c0326a429a5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49688668-9bf8-412d-9f6a-dd0014879313)(content(Whitespace\" \
         \"))))(Tile((id \
         29c6a844-3686-4825-aa7c-907d5b1b70dd)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         7a0d21a6-8649-4b18-8555-ef19f55a7ab8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f2508f69-4621-4a3a-9b99-bf2c8b3b0c26)(content(Whitespace\"\\n\"))))(Tile((id \
         2bde1e58-62c2-4d4f-ae76-40f6a1393c41)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f1f1b2f9-80c2-45f8-a3c0-48ab0e9ad585)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2a4e9c4d-8da7-49f4-9bf2-3dae78512edc)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b03dc22b-b7e9-4b6f-8bf1-88be98e3c87a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b786e689-3b5e-461c-aa7b-fc22b1facb5d)(content(Whitespace\" \
         \"))))(Tile((id \
         8dab6fd4-a982-4f61-9beb-2bb7d72427dc)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b3e01bb4-0f65-4c13-90db-e767cd2c5844)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6cc2f645-9528-4652-9deb-8336edc2e2c7)(content(Whitespace\"\\n\"))))(Tile((id \
         a81cd8cb-94b5-400f-9c77-7bddaf468dab)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45401164-d01e-42dd-9052-b33b9de9577a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9866f479-92b1-4fa4-a2e1-b30835ea4b68)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         780263f1-6489-4ee8-90a1-922c8b5f07d5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f711724c-e57f-42b8-bd33-fc5e0a826993)(content(Whitespace\" \
         \"))))(Tile((id \
         4a5092e4-9e79-478a-b68d-6d876a650a33)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4ce3a425-235c-48f9-91e5-c51f9a17dd3b)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         598b8554-719f-4bf5-948c-7e8fb17a0ed2)(content(Whitespace\"\\n\"))))(Secondary((id \
         3fdc1087-f24e-43c5-a406-6b3d3bca2650)(content(Whitespace\"\\n\"))))(Secondary((id \
         4b829584-bc7d-49e0-8bc5-5734f395a2e4)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         f0547304-b73b-4701-8b60-5df339dcd9ea)(content(Whitespace\"\\n\"))))(Secondary((id \
         2c893752-9825-4b1d-a522-f692ed0b315b)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac455190-ee77-4bde-89c8-c04af540a47f)(content(Comment\"# DEEPER \
         NESTING                                                   \
         #\"))))(Secondary((id \
         55f95586-1c97-4f08-b9eb-c44e53533276)(content(Whitespace\"\\n\"))))(Secondary((id \
         65d466bc-d74d-4c92-aa5a-c9b6a940e456)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         1d9496a5-7600-4e20-8c24-0c5ec5be2696)(content(Whitespace\"\\n\"))))(Secondary((id \
         07955b05-b709-4918-b5b4-5dd92d7d87b5)(content(Comment\"# With map, \
         each iteration of the callback is inside the           \
         #\"))))(Secondary((id \
         1f806501-80d0-4c62-8099-cecedaf5db21)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e1f7466-5649-440a-b94e-f5b3ec0f86df)(content(Comment\"# enclosing \
         call. Different calls to the same function show        \
         #\"))))(Secondary((id \
         67b9ef6a-d6aa-429a-9d1f-794c68278e9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb9e08fb-0216-4816-9617-6121a4a06e70)(content(Comment\"# as unrelated \
         (gray) they're siblings, not ancestors.           \
         #\"))))(Secondary((id \
         10ff80b6-2eb5-4c50-b894-34ee5e8b209e)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf40cbc2-1d24-42da-899e-f649e645acff)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         ef3d02ce-8902-4c17-96d3-8f45281b9b2a)(content(Whitespace\"\\n\"))))(Secondary((id \
         0cbe3795-07f0-4c73-8d8c-e6f97c9ae31a)(content(Comment\"# TRY THIS: \
         Switch to Many mode (double-click a sample).          \
         #\"))))(Secondary((id \
         e47894d0-0314-4c0f-a6e1-dee93941c860)(content(Whitespace\"\\n\"))))(Secondary((id \
         cbecaaf7-d373-4bfc-94a0-cbfa238ec512)(content(Comment\"# Click a \
         sample at the `plant` level inside the map callback.    \
         #\"))))(Secondary((id \
         6bc212eb-b9d0-4a16-ae8c-57c7bc99a0cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         da27539b-628f-43e4-9c9f-aca4daaac660)(content(Comment\"# - Samples \
         from the same outer call are colored (related)         \
         #\"))))(Secondary((id \
         82c0e292-f057-4c73-9cd7-d470fa6ac487)(content(Whitespace\"\\n\"))))(Secondary((id \
         6466d0fd-971e-4558-9bf4-ec0dc0d57c9e)(content(Comment\"# - Samples \
         from a different call to bed_labels are gray           \
         #\"))))(Secondary((id \
         0568b896-0d34-4ea8-b879-99736535ca92)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8311dc0-1db7-4e27-8576-0f3e16524150)(content(Whitespace\"\\n\"))))(Tile((id \
         00fc37e0-66d8-490d-944a-32c5325db111)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         66db97c1-3a68-44af-a44f-0e6379afce6d)(content(Whitespace\" \
         \"))))(Tile((id \
         c52f8143-f968-4731-a45d-3413e127b51d)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         24595914-a6ff-4aab-a2cb-19f9aa516fda)(content(Whitespace\" \
         \")))))((Secondary((id \
         f3348800-35d1-483b-9a22-5678bc7794bc)(content(Whitespace\" \
         \"))))(Tile((id \
         d00d066e-b142-4785-84bc-abff4ced44be)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         836f2097-c962-4322-8130-3fa2d29bb7bb)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cf4be7f9-7407-4a57-9771-38ff31c5006d)(content(Whitespace\" \
         \"))))(Tile((id \
         46f72f4d-b282-404b-867e-7419f03b9db5)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2172b568-7599-4a53-92c6-44bd53b96983)(content(Whitespace\" \
         \"))))(Tile((id \
         7f7f980b-b9e2-4518-ac3b-e5de76e715e4)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         26125628-d62d-4e2b-a7da-15846eaac8fc)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         224f0467-0c0e-4c12-b653-d6a817f989ed)(content(Whitespace\" \
         \"))))(Tile((id \
         dd06de72-124c-4d15-a59a-ea5e7964d12f)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         ddaae658-6b5d-4aab-a77a-e617b9f533b5)(content(Whitespace\" \
         \"))))(Tile((id \
         dac968da-e62c-44b4-8d24-25196ae01bf4)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6bb59e3a-cd87-4879-842b-060cbfd14c3e)(content(Whitespace\" \
         \"))))(Tile((id \
         d8abe9ef-2065-42ac-862d-bccdd3534f2b)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         2b653073-a3dc-4b6f-ae33-1c3e311fb49e)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e2e8e370-7972-4877-8b6b-2ffb5a9eb4c7)(content(Whitespace\" \
         \"))))(Tile((id \
         4f7586ca-882c-4901-bf44-9609ee1e839c)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         81bce69e-f4cb-44f9-be4a-d4c809df9f29)(content(Whitespace\" \
         \"))))(Tile((id \
         9ef86fae-38be-45c7-ad98-3052e96b86b1)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4c2bbabb-c409-430b-b641-8554995be415)(content(Whitespace\" \
         \"))))(Tile((id \
         4775cc9f-2147-4a61-ba54-c5c257b42960)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         72e76cfe-b3bd-42ed-b262-c425ae8e8e44)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f7dd62fd-2098-474d-a4b3-74d814df3077)(content(Whitespace\"\\n\"))))(Tile((id \
         0d28501f-d23a-4824-9676-b39195219d79)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         81ab2128-7238-41d2-8944-04b668e3c3a9)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc7819d-3a94-4d02-930e-59685c98d3fd)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fb7f58d1-d82b-425a-9a27-e48cfb386d62)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f42ea986-d7bf-4f37-8af1-f694bd9b15c1)(content(Whitespace\" \
         \"))))(Tile((id \
         f47b3355-b5dd-42d1-8466-7bfc53110607)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2a55ffb5-3856-4ea9-98b3-1ccfc56dadcb)(content(Whitespace\" \
         \")))))((Secondary((id \
         7572d18c-75a0-4708-86c2-be0d04b05b93)(content(Whitespace\" \
         \"))))(Tile((id \
         48111425-fd44-4c61-993f-4c658381d6c2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         71a9f273-3d33-4f87-be43-fe4d14ac95cc)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a29b3726-c06a-4f20-b766-cc168b5f32e6)(content(Whitespace\" \
         \"))))(Tile((id \
         00c99a38-2c16-44fb-8bbb-1a937a4a45dc)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be3156f8-7283-4100-85c9-28df0b251f02)(content(Whitespace\" \
         \"))))(Tile((id \
         43393e8f-8f8a-4211-8704-505a40719b63)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         745ed44a-231e-4090-8681-6dd5f3fccb7d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eadb0a89-c78f-42c2-8090-fa004d181835)(content(Whitespace\" \
         \"))))(Tile((id \
         757109a4-5cf2-4d9c-a696-0d2fa727db7c)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5634eed4-3769-4590-88cd-a6834019c19f)(content(Whitespace\" \
         \"))))(Tile((id \
         bd4d4fb0-fb00-443f-9b6d-269e4fe4c536)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb5ece16-867c-4ad9-a188-905942d3e2f0)(content(Whitespace\" \
         \"))))(Tile((id \
         ea682f96-589f-4ec3-9222-1488e72922be)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ebd4467f-40bb-47a3-a39c-67649e81b0c5)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d72146d-ef4d-4b4a-aaf3-61bc5cca4a2a)(content(Whitespace\" \
         \"))))(Tile((id \
         2a948e96-7971-42f9-980a-569a7ea2ad8c)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f1ac7d0-13f2-48a9-91fa-eca220e95811)(content(Whitespace\" \
         \"))))(Tile((id \
         c734fa9c-7c4d-48df-ad2a-fd4121077d59)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         466c4458-8f13-471f-bf36-c490defffc34)(content(Whitespace\" \
         \"))))(Tile((id \
         f800155f-37e6-4243-b0d8-4510c7b91136)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3126fade-2129-4199-b950-b19f874db0e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4c823279-2641-4759-a641-bc66bac5a50e)(content(Whitespace\"\\n\"))))(Tile((id \
         3ba65453-640c-460b-abea-49583868e062)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5f22176b-cb9a-4129-a9c4-e9eaf49339f4)(content(Whitespace\" \
         \"))))(Tile((id \
         6d9f246b-92f0-4977-ac60-683302694602)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bf6ff9d5-8c41-4d4f-a9ab-0adfd670477b)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         473c4bca-50a6-4015-ad06-a7b629dc0fa0)(content(Whitespace\" \
         \"))))(Tile((id \
         6980b165-cb25-4812-8f64-d41ced1f057e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         3dfa5ff2-ef19-4340-b154-f1b4d5d5fbcb)(content(Whitespace\" \
         \")))))((Secondary((id \
         1cab91f6-9b78-4fa8-ba4e-8bc140c4a397)(content(Whitespace\" \
         \"))))(Tile((id \
         b7316c85-8413-40e2-85c1-a22222c98490)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         d5967232-f786-4d54-8283-12d322f20a97)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         88462d71-f9e3-444f-977b-56a65006a070)(content(Whitespace\" \
         \"))))(Tile((id \
         484388ca-053e-45d1-9b3d-52aca222ccdd)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         074b72c8-5a84-4df9-8998-6da9101c0dff)(content(Whitespace\" \
         \"))))(Tile((id \
         75c9be19-59d0-40dc-9f5a-01f720cee1a4)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1f63f129-367f-484f-bfc3-9b530d6becda)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e14f929-4645-4ea5-b50d-efc3d86fb93d)(content(Whitespace\" \
         \"))))(Tile((id \
         4a76e014-088a-4a31-8c03-343ffd08bf38)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9639a25a-b083-4b4f-b4a2-1b448f2124b6)(content(Whitespace\" \
         \"))))(Tile((id \
         1fad386f-7510-4c52-8609-0cf4b9018e83)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         74e9496b-d150-4ce9-a8e4-e37b99f47d0f)(content(Whitespace\" \
         \"))))(Tile((id \
         ff16120d-4540-416b-98bf-c649bf80f43f)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83a3c37f-5141-49db-8e04-723dfe576566)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         34b882a8-9171-4971-b550-6d95036afbfe)(content(Whitespace\" \
         \"))))(Tile((id \
         b0b89b3c-3fba-43a3-8ffb-9c82e205fc03)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf65d4ad-3ccc-42ef-b774-7a1d5dcb9d32)(content(Whitespace\" \
         \"))))(Tile((id \
         1720bc44-d995-4d7f-abb2-b61f1cdb216f)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         561bfdd7-ff29-4b91-a536-955005bbfd8c)(content(Whitespace\" \
         \"))))(Tile((id \
         8b4ac69c-51dd-4bcd-9691-17a2f316ce4e)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         ad135c64-e568-43c5-bae9-8ff8c9911104)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ea892623-c48a-4af0-a984-4331a91ecf8c)(content(Whitespace\"\\n\"))))(Tile((id \
         303c44c3-c219-480f-a0a1-426084370d20)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         37cbf61c-abf0-4519-a283-812399d44738)(content(Whitespace\" \
         \"))))(Tile((id \
         05715f57-d159-44cd-90f3-b7f9e0301bdc)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1c931885-5a82-492d-a1e0-409ec94f3f64)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         52867d65-0e4a-41bc-9419-050a0802ea5b)(content(Whitespace\" \
         \"))))(Tile((id \
         897632f0-bcfa-47e2-b5c8-4009a242159a)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         eeaa2ac0-e94a-4af1-8ebf-2c25cfac73c4)(content(Whitespace\" \
         \")))))((Secondary((id \
         e698eaa7-9c0d-4fa3-ab66-cb63179bb228)(content(Whitespace\" \
         \"))))(Tile((id \
         d3f49dfa-c260-4f6e-9fb3-d70fae251762)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0f98508d-448e-460e-9d6f-70557a848299)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f44b90c8-9f14-4719-95a7-9615201b9aef)(content(Whitespace\" \
         \"))))(Tile((id \
         c5b18296-da40-445d-bcba-9ce836cd19a4)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c473855-6e38-4547-92f7-df70eb5b2a27)(content(Whitespace\" \
         \"))))(Tile((id \
         98b3aa70-7dfb-4f6a-a59b-07e120b1c27e)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f120fb3f-b456-4024-af34-018dfb83cf4e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b16b3b20-a337-4faa-b376-b787e11a2be0)(content(Whitespace\" \
         \"))))(Tile((id \
         5b104f42-2961-42d1-ab87-387024b4e59e)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7177d7fa-7fdb-404d-b419-369cc3b00b30)(content(Whitespace\" \
         \"))))(Tile((id \
         ac141151-7ec0-4ce2-89ca-3eafe0f3920b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0b1d4669-2ac7-40d2-b9a9-d100f5ac39b5)(content(Whitespace\" \
         \"))))(Tile((id \
         0d4931b3-a90a-40d5-90df-d66c43bdd638)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8e47419a-dd1f-4441-a514-1a1e9021ba05)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9858378d-f1fe-4cd3-a5a1-dbe9d63fcee2)(content(Whitespace\" \
         \"))))(Tile((id \
         12cd6701-601f-48c1-ac3f-97c838db68e7)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b9a2c7f-1fee-49fd-bf83-265f8061e72b)(content(Whitespace\" \
         \"))))(Tile((id \
         9c608211-0780-4cf9-90e3-de0086d089b9)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85400d39-0bfd-4144-8184-b839f51ed051)(content(Whitespace\" \
         \"))))(Tile((id \
         69aea38e-211d-43ef-9bbe-a6e0731542de)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d400e392-53c6-41be-986a-001e710ad95f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c66f176d-a5c5-49f0-936d-0028cc788126)(content(Whitespace\"\\n\"))))(Secondary((id \
         22f8c218-f616-470e-945f-a61bd2857bf2)(content(Whitespace\"\\n\"))))(Tile((id \
         9dfe965b-d3c2-4e9f-910e-7c206ccf0fd2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e90e0a1b-fb86-4cda-82bc-19e14b6b4166)(content(Whitespace\" \
         \"))))(Tile((id \
         6cff4742-b95b-4195-8caa-6649cef1761e)(label(bed_labels))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         7f53fa0d-8940-4694-95ff-81ee5cdbe046)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dc1695e7-6215-4d77-9cb9-3e770f5de19f)(content(Whitespace\" \
         \"))))(Tile((id d270a52d-3a5d-42fb-9ae5-9efeefba63b4)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         d637885c-1ddc-4c4b-bc06-368ac395aef7)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         15b2607f-9a84-4c77-92b0-773bee941444)(content(Whitespace\" \
         \"))))(Tile((id \
         eca9e702-36ae-4a6f-a8b6-18cec57075fd)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         4cee1379-6605-4903-808c-0ff5a334c8d6)(content(Whitespace\" \
         \"))))(Tile((id 718e8ec3-b232-45fc-818d-e6d60a93d20b)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         dcc71d73-fb50-412f-a868-ac0116e7892e)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         df2e43b6-8019-45bf-964e-76958e027240)(content(Whitespace\" \
         \")))))((Secondary((id \
         4863ccf9-1e28-49e9-9988-91238f163868)(content(Whitespace\"\\n\"))))(Tile((id \
         aad575d1-164f-4407-b32a-e78e41126c27)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8277a41b-ce44-4967-a8a2-4bbaaa1255a7)(content(Whitespace\" \
         \"))))(Tile((id \
         3ff8f035-b8a2-48ba-be24-04046c0646ed)(label(bed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d7264627-5244-4581-9f13-5d75ea6840b7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         05b349d2-6f47-4e6f-a58c-9056adc6553c)(content(Whitespace\"\\n\"))))(Tile((id \
         6940a671-dfab-4258-9c60-7a1777b4ea0b)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a4751e30-452f-45af-9973-0ec1ce158e2f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         ce9bec04-98f9-4223-9999-2f4fad79d7cd)(label(bed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4763a57b-0567-4c00-8f8e-02c3ffda4b8c)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6443e1ba-fe1a-4bdd-a1ac-78b817ea9dfe)(content(Whitespace\" \
         \"))))(Tile((id da2bf582-c020-4f6d-a1be-12ce68841ec3)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         00c19043-6c75-4257-861b-a8ab6f39ee8f)(content(Whitespace\" \
         \"))))(Tile((id \
         6e8cfce5-b617-483d-8224-9fc5c230d1c8)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5aab1477-46e5-4f39-89f3-80f3a013100d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         986855c7-88bf-48f6-9d8e-26618eee0883)(content(Whitespace\"\\n\"))))(Tile((id \
         1d490f0d-173c-4068-bd08-4a61a5ca1c6b)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bced07bd-0378-4094-ab93-cda5b667bdfb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         f2026009-87c2-4c6c-a61b-906ea0ea1e2d)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ea2d4a3b-1b97-4ee2-ba5a-6dc3006ae8d9)(content(Whitespace\" \
         \"))))(Tile((id \
         a9f05c55-45d6-4b52-ad7e-49e0446ea18e)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         16165587-c5b4-4e76-9965-2c6282a03cc9)(content(Whitespace\" \
         \"))))(Tile((id d52b9ccf-5ba0-4bfe-bdbc-499d5db0afa8)(label(\"\\\" \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfe36fc6-308d-4641-823e-4126893d78d2)(content(Whitespace\" \
         \"))))(Tile((id \
         76073399-e186-4b6c-be8e-7b0f4efd63d4)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ad814995-d0e6-4866-85cf-c3a292466443)(content(Whitespace\" \
         \"))))(Tile((id \
         e25cdd7d-b05b-49d4-9e0c-6f23692f1880)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3fe19399-8aca-4667-b578-e2b7afd7fefb)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         532f4455-e66a-4fe0-b6b6-f09df1e617d2)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         11cb7313-f02d-46cf-a4ad-4b91526f0da0)(content(Whitespace\"\\n\"))))(Tile((id \
         5904337c-12ff-460b-8b2d-ed9f56e1f431)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66be23f7-c3b1-475f-88c0-ed5edf24353b)(content(Whitespace\" \
         \"))))(Tile((id a8aebbd3-de3e-4aad-9995-381d070540b8)(label(\"\\\": \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4b7da651-c8b4-4193-99ae-bf71773a50cc)(content(Whitespace\" \
         \"))))(Tile((id \
         194e6c87-ae0f-420f-b75a-d16cf0d82096)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e01d2db6-0831-4556-bc80-4b9c04c08c97)(content(Whitespace\" \
         \"))))(Tile((id \
         0946de77-8e86-47bc-8056-35fe5285aaef)(label(string_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc090023-fe37-4979-91b7-2e17c503e421)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7b75cab4-86ea-467b-907a-82ab1224d60c)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         42dc1fe0-d328-4433-acef-ecc2b726bb06)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8a1fdacc-6a86-454a-8eb3-259c7f159f04)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8392e28d-058b-447f-9f8e-3bbd0aa15709)(content(Whitespace\" \
         \"))))(Tile((id \
         5843fa35-ef0f-401c-a023-f3a10a12ed4a)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         29059289-16d1-4f9a-a520-9b984d3f5d2d)(content(Whitespace\" \
         \"))))(Tile((id \
         0d59b5da-9648-4039-a39c-34d240a78453)(label(\"\\\"ml\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5b0f4f25-8e4f-48bc-942c-dec65e0b3123)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         64287d0f-e21f-4b7d-888b-972fc9eda9a1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d086989f-740d-460a-887f-e014db6d01e0)(content(Whitespace\"\\n\"))))(Secondary((id \
         848317e3-61a9-4d61-8066-3f60d3e1142d)(content(Whitespace\"\\n\"))))(Tile((id \
         21bbaa7d-9b71-4496-84bf-54ab0d9e8248)(label(bed_labels))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3bb941a-1e53-42aa-b915-7b8673110803)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3fc58b85-d610-4359-990a-99435ef5d01a)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0fd13181-ed69-45a7-ba18-7d9ff57e138c)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         20104700-c7f8-4054-b08e-43aadbd08987)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bac9041f-3335-4b05-bfce-8fc8a27d3a55)(content(Whitespace\" \
         \"))))(Tile((id \
         d3beb1e5-85e9-4ad1-b0a0-ec5e2db799e6)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         2071fcc8-26b6-4b52-8d68-eee73bfd8885)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fe144aee-2d9c-4467-9de5-1fae9cc1bec9)(content(Whitespace\"\\n\"))))(Tile((id \
         165b90a6-1594-4e4b-a164-e42e0ada864e)(label(bed_labels))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9f3d70a2-40ba-4938-b807-7ec39ead4ca0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8aaae5fe-5145-45b6-a352-59ddfffcc657)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         a4f01728-ac6b-4c9a-9ad8-8da88ab9f567)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         4341ca1a-6280-4f63-9e47-eb67d10c5169)(content(Whitespace\"\\n\"))))(Secondary((id \
         14297b4f-a785-4284-b822-e7c2fb99b9b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7b1b484-6a1e-41e4-b0eb-c73c6737b9d1)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         520b7b3d-4ddc-475a-acbf-94ddc8fc4ef4)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d7a1a13-085c-46b9-8fbd-bc3378b410f6)(content(Comment\"# You don't \
         need to memorize the color categories. The sidebar   \
         #\"))))(Secondary((id \
         a3a939c5-846e-45f6-ad9c-d116576180df)(content(Whitespace\"\\n\"))))(Secondary((id \
         eaa3c3f4-8018-41de-8c5f-e564aac5950a)(content(Comment\"# legend is \
         always available. As you work through the study        \
         #\"))))(Secondary((id \
         95d7f59f-e8b1-4b36-aaf1-6e30c6b67167)(content(Whitespace\"\\n\"))))(Secondary((id \
         23b5384e-a1f9-499e-9e6c-efbf62de14fe)(content(Comment\"# tasks, the \
         colors help you see at a glance which samples        \
         #\"))))(Secondary((id \
         fa552f3a-9a85-459a-9c33-ea861a5daad7)(content(Whitespace\"\\n\"))))(Secondary((id \
         750aead8-d93e-41df-ae43-823152c02940)(content(Comment\"# are \
         connected to whatever you're currently focused on.           \
         #\"))))(Secondary((id \
         ec73f0c0-1e05-44f4-880d-0437a02214fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e75b4bb-eb0a-4d45-890f-bc4d8bde898e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c05904a4-5108-4a9c-b692-cd220b517ed9)(content(Comment\"# END OF PART \
         10 - Select the next slide from the top menu        \
         #\"))))(Secondary((id \
         b4393ba2-4ea9-4f0c-aaed-c5f1eeb1cf34)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# PROBES TUTORIAL - PART 10: SAMPLE COLORS                        #\n\n\
         # When you click a sample, other samples change color. These      #\n\
         # colors show how each sample's evaluation relates to the one     #\n\
         # you selected, whether it ran before, after, or nested          #\n\
         # inside or around it.                                             #\n\
         #                                                                  #\n\
         # Look at the LEGEND at the bottom of the probe sidebar.          #\n\
         # Hover over each entry to see what it represents.                #\n\
         # =============================================================== #\n\n\
         # BEFORE AND AFTER                                                 #\n\
         #                                                                  #\n\
         # The simplest case: expressions that evaluate in sequence.        #\n\
         #                                                                  #\n\
         # TRY THIS: Click the sample for `second` below.                  #\n\
         # - `first` shows a Before color (it finished evaluating earlier)  #\n\
         # - `third` shows an After color (it hasn't started yet)           #\n\
         # Click `first` or `third` to see the pattern shift.               #\n\n\
         let first = ^^probe(1 + 2) in\n\
         let second = ^^probe(3 * 4) in\n\
         let third = ^^probe(5 + 6) in\n\n\
         # =============================================================== #\n\n\
         # CONTAINS AND INSIDE                                              #\n\
         #                                                                  #\n\
         # Evaluation isn't just a flat sequence... it has depth.            #\n\
         # When you call `daily_water(250, Full)`, the call starts,        #\n\
         # then the function body runs, then the call finishes.            #\n\
         # The call *contains* the body; the body is *inside* the call.    #\n\
         #                                                                  #\n\
         # Contains and Before share a color because both represent         #\n\
         # things whose evaluation *started* before the focus sample.       #\n\
         # Inside and After share a color because both represent            #\n\
         # things whose evaluation *finished* after the focus sample.       #\n\
         #                                                                  #\n\
         # TRY THIS: Click a call sample at the bottom of this section     #\n\
         # (e.g. the first one). The body samples above change color       #\n\
         # to show they are INSIDE that call.                               #\n\
         #                                                                  #\n\
         # Now click a body sample instead (e.g. `adj`). The calls         #\n\
         # below that launched it are colored as CONTAINS.                  #\n\n\
         type MoonPhase = + New + Waxing + Full + Waning in\n\n\
         let phase_mult: MoonPhase -> Float =\n\
         fun phase -> case phase\n\
         | New => 1.2\n\
         | Full => 0.88\n\
         | Waxing => 1.1\n\
         | Waning => 0.95\n\
         end\n\
         in\n\n\
         let daily_water: (Int, MoonPhase) -> Int =\n\
         fun (base, phase) ->\n\
         let adj =\n\
         ^^probe(float_of_int(base) *. phase_mult(phase)) in\n\
         ^^probe(int_of_float(adj))\n\
         in\n\n\
         ^^probe(daily_water(250, Full));\n\
         ^^probe(daily_water(50, New));\n\
         ^^probe(daily_water(180, Waning));\n\n\
         # =============================================================== #\n\n\
         # DEEPER NESTING                                                   #\n\
         #                                                                  #\n\
         # With map, each iteration of the callback is inside the           #\n\
         # enclosing call. Different calls to the same function show        #\n\
         # as unrelated (gray) they're siblings, not ancestors.           #\n\
         #                                                                  #\n\
         # TRY THIS: Switch to Many mode (double-click a sample).          #\n\
         # Click a sample at the `plant` level inside the map callback.    #\n\
         # - Samples from the same outer call are colored (related)         #\n\
         # - Samples from a different call to bed_labels are gray           #\n\n\
         type Plant = (name = String, icon = String, water = Int) in\n\
         let fern: Plant = (name = \"Fern\", icon = \"\240\159\140\191\", \
         water = 250) in\n\
         let orchid: Plant = (name = \"Orchid\", icon = \"\240\159\140\184\", \
         water = 180) in\n\
         let cactus: Plant = (name = \"Cactus\", icon = \"\240\159\141\132\", \
         water = 50) in\n\n\
         let bed_labels: [Plant] -> [String] =\n\
         fun ^^probe(bed) ->\n\
         ^^probe(map(bed, fun ^^probe(plant) ->\n\
         plant.icon ++ \" \" ++ plant.name\n\
         ++ \": \" ++ string_of_int(plant.water) ++ \"ml\"\n\
         ))\n\
         in\n\n\
         ^^probe(bed_labels([fern, orchid]));\n\
         ^^probe(bed_labels([cactus]))\n\n\
         # =============================================================== #\n\
         # You don't need to memorize the color categories. The sidebar   #\n\
         # legend is always available. As you work through the study        #\n\
         # tasks, the colors help you see at a glance which samples        #\n\
         # are connected to whatever you're currently focused on.           #\n\n\
         # END OF PART 10 - Select the next slide from the top menu        #\n";
      refractors =
        "((9f3d70a2-40ba-4938-b807-7ec39ead4ca0((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(f3bb941a-1e53-42aa-b915-7b8673110803((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(a4751e30-452f-45af-9973-0ec1ce158e2f((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(6e8cfce5-b617-483d-8224-9fc5c230d1c8((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(3ff8f035-b8a2-48ba-be24-04046c0646ed((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(45401164-d01e-42dd-9052-b33b9de9577a((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(f1f1b2f9-80c2-45f8-a3c0-48ab0e9ad585((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(75740b3d-85f8-4540-b8e7-3380b05ad8b5((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(7c7f76a1-56c2-4d3b-bfd5-34a416ddd43f((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(d4a6a9d5-e9d5-480e-893f-bd9912478108((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(f77a319b-6041-4762-82b2-68e96f5f3cb0((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(a98fdbbe-8016-473d-b4de-48c4998fd280((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\")))(9d955c56-1b5d-4d12-b5dd-852401ae122a((kind \
         Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw \
         0))\"))))";
    } )
