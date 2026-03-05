let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / night-bloom / night-bloom-sketch",
    {
      segment =
        "((Secondary((id \
         2fcfc92d-4736-41e5-9668-3c24b6a0d15e)(content(Comment\"# NIGHT BLOOM \
         FILTER TASK                        #\"))))(Secondary((id \
         cf0562dd-a3b9-40d4-baa4-6849ad0d075a)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab1e8840-e6ce-4268-9ee4-91140d2c8589)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         036bf253-afa0-4cce-bad0-7da808591bc9)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0ad14aa-27b3-4553-9a12-1036cd704429)(content(Comment\"# A plant \
         catalog has entries like:              #\"))))(Secondary((id \
         eb83aae4-ad5b-4ccf-868a-c6818f7af553)(content(Whitespace\"\\n\"))))(Secondary((id \
         459e7df9-acbc-439b-ae01-6ebbd8f25706)(content(Comment\"#   \
         \\\"Moonbloom [night] 200ml\\\"                    \
         #\"))))(Secondary((id \
         587052a0-9bd8-404c-a5b4-79396ba3c3ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         8eab7749-b32e-48b1-8718-2d992548ada4)(content(Comment\"#   \
         \\\"Duskrose [day] 150ml\\\"                        \
         #\"))))(Secondary((id \
         d67b261f-5971-4f38-82a6-5f2bb17f006c)(content(Whitespace\"\\n\"))))(Secondary((id \
         11cbe019-fdfb-498d-9859-7172d0775e25)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         f3c7f6a6-2d52-41eb-822e-09a49356d3ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         6395a0c7-b32e-4dd6-8bc1-6e7a92949d1a)(content(Comment\"# Filter to \
         night-blooming plants and extract    #\"))))(Secondary((id \
         10655d64-805a-469e-9157-c50b4e307f9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9596358e-b4c1-43f9-a071-579094725294)(content(Comment\"# just their \
         names: [\\\"Moonbloom\\\", \\\"Starfern\\\"]    #\"))))(Secondary((id \
         0660c6df-5a85-45bf-966b-28ba035061a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         e95daae5-de63-4303-be28-038e36969606)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         82839fea-140e-40da-ac24-2892b6592c6b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5607e64-6ffc-42d8-a01d-64820f9ea2c2)(content(Comment\"# \
         Steps:                                         #\"))))(Secondary((id \
         22124599-adfc-4865-a70a-fa9563c56662)(content(Whitespace\"\\n\"))))(Secondary((id \
         d832c763-6fe0-4136-b24e-a682679ae800)(content(Comment\"#   1. \
         is_night: check if entry contains \\\"night\\\" #\"))))(Secondary((id \
         0831a2dd-330a-4fab-92a7-52c2e2c51c8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         43ab215f-aee0-46d0-855c-182d81f4965f)(content(Comment\"#   2. \
         extract_name: get the first word          #\"))))(Secondary((id \
         9b6ddad2-fec8-4bd8-9f50-19382c8a4002)(content(Whitespace\"\\n\"))))(Secondary((id \
         5619412a-82f5-4c97-9d5e-667896396093)(content(Comment\"#   3. Combine \
         with filter and map               #\"))))(Secondary((id \
         ef81b634-3cf5-487f-a8ad-e08efd6a9a15)(content(Whitespace\"\\n\"))))(Secondary((id \
         171e7781-3556-4877-a218-5a59ca966640)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         e3ef0121-103f-44c8-a925-844ad1a431e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6dfdcc1-8cf6-43a6-8668-ac7adc392e3f)(content(Comment\"# Available \
         functions:                           #\"))))(Secondary((id \
         67be8470-536a-4ae9-9175-1e109956737b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c51d81be-1e38-4a02-8cc3-2b852a14b7f0)(content(Comment\"#   \
         string_match(pattern, str) -> Bool           #\"))))(Secondary((id \
         63bacfd5-ff8a-4691-81e7-d44037e1cb41)(content(Whitespace\"\\n\"))))(Secondary((id \
         f7c92577-5acd-4fb8-8544-c26a5fec851f)(content(Comment\"#   \
         string_split(separator, str) -> [String]     #\"))))(Secondary((id \
         57e09cf2-6438-4259-af9a-0201e1ad4e64)(content(Whitespace\"\\n\"))))(Secondary((id \
         1d08916e-6079-4746-884a-2c03949767de)(content(Comment\"#   nth(list, \
         index) -> element                  #\"))))(Secondary((id \
         e82d14f3-221c-470a-8955-dc506a1f0748)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b5cc662-6e24-41b7-841b-e0858e28934d)(content(Comment\"#   \
         filter(list, predicate) -> list              #\"))))(Secondary((id \
         152fee23-0d95-4d10-8ef0-51462b817c3d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f1488c8d-d88c-4f05-a80f-899428e266c8)(content(Comment\"#   map(list, \
         fn) -> list                        #\"))))(Secondary((id \
         8a99aca9-379f-42b1-a11d-86226eefe38c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c4a714ef-dbc1-4ef3-872b-aca21f20fccc)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         bf8c3d2d-29b5-4cf1-802a-719ec5779f35)(content(Whitespace\"\\n\"))))(Secondary((id \
         52dde52a-70c7-476a-81ca-14d6030e757b)(content(Comment\"# Note: \
         string_match uses regex patterns.        #\"))))(Secondary((id \
         ae638d6b-7736-4251-9263-9e3838b530c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c628c90-24ce-4271-9371-5baf85db64d8)(content(Comment\"# The pattern \
         \\\"[abc]\\\" matches any of a, b, c.   #\"))))(Secondary((id \
         0a273d0b-6a57-4af9-991a-6ca8a6b2dd76)(content(Whitespace\"\\n\"))))(Secondary((id \
         a00a0bef-378b-4bcf-9f78-37fb690a6347)(content(Comment\"#                                                \
         #\"))))(Secondary((id \
         07f32707-bf25-4ff2-95d8-923361792199)(content(Whitespace\"\\n\"))))(Secondary((id \
         747ed046-ce65-46df-8afe-83c40df15505)(content(Comment\"# Tip: Use \
         probes to see what your pattern       #\"))))(Secondary((id \
         7652cb3a-aa8a-4a69-93da-cf9936aef515)(content(Whitespace\"\\n\"))))(Secondary((id \
         be052ff4-8b35-4394-b857-91428bc104c0)(content(Comment\"# actually \
         matches -- regex can be surprising!   #\"))))(Secondary((id \
         94a00558-c5e2-4228-bd56-55a29a2f0080)(content(Whitespace\"\\n\"))))(Secondary((id \
         fae5200d-ef2e-4639-b712-55b85e4657a8)(content(Whitespace\"\\n\"))))(Tile((id \
         987eb3e2-8513-42f3-81da-06c4158706d6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         84a1bde2-93d9-4821-bcbc-a9c895c491ed)(content(Whitespace\" \
         \"))))(Tile((id \
         034f16e0-dd80-4d4a-a9d9-5843c99cd2e1)(label(entries))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cf23185f-e40c-40b9-b8c1-40ce94e3b4a2)(content(Whitespace\" \
         \")))))((Secondary((id \
         912c431f-9dd6-431a-addc-75d8df93539f)(content(Whitespace\" \
         \"))))(Tile((id 8c0dfbd6-0af2-4f9e-b27d-bb9fa94ab94a)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         1b7f4b20-a4b5-4cda-b98a-c71864d35a02)(content(Whitespace\"\\n\"))))(Tile((id \
         82898232-9153-4fa7-8480-8140d00d5dee)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         e6d8457c-39b5-4ec1-a6b2-a9d4e082f18f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c4cf2e8f-6a80-41ff-b024-e1c879077934)(content(Whitespace\"\\n\"))))(Tile((id \
         b7b73e0a-ac79-498f-85fd-a5ab7672cdc2)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         e4fdd74a-5d94-43d0-a677-eb6b52f65c2a)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fcba4ef0-8379-42a9-85c8-bfdaf34227e9)(content(Whitespace\"\\n\"))))(Tile((id \
         e2da6f00-11c9-4e41-96fd-15cadf11ea05)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort Exp))))))(shards(0))(children())))(Tile((id \
         0fb86cce-6309-448a-b6d0-805bcb72e60f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1cdcadaf-c07e-4ed3-a31b-b056008c06ec)(content(Whitespace\"\\n\"))))(Tile((id \
         d634dd34-2bd3-4799-a583-910b8ac425be)(label(\"\\\"Ghostvine [day] \
         100ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         adf003ec-717a-4665-bc48-8b2654033644)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         c921617f-295a-4e3b-8e83-aa40eaa80c2c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         26a8bd98-31fa-4cc3-a40e-274dc8f44b77)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c7ad06f-1bd1-4988-9452-be98077d3282)(content(Whitespace\"\\n\"))))(Secondary((id \
         26c15440-cf8e-417e-a634-c37d01632204)(content(Comment\"# Check if \
         entry is a night-blooming plant #\"))))(Secondary((id \
         a596970b-e7ea-4211-89fd-9ba12903d70f)(content(Whitespace\"\\n\"))))(Tile((id \
         3c653bcd-fe54-419f-8010-edf3ecd34696)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         07157373-98a6-4391-a78d-06a783a186ea)(content(Whitespace\" \
         \"))))(Tile((id \
         f7fa88cc-272d-4542-8653-2f459eb5477c)(label(is_night))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         597af58c-9c3c-41a2-9b98-22e48d75417d)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b811ef5a-8444-4811-a39f-0e2909e03b3f)(content(Whitespace\" \
         \"))))(Tile((id \
         2e1b87d1-bfa9-4583-b940-5ea50accbaef)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9d6530f2-02d3-4f17-809e-eef3b4ae095b)(content(Whitespace\" \
         \"))))(Tile((id \
         932901cc-815f-4846-83ab-18397cbff659)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         db46af5e-146c-4e3f-8348-08d05249f7b5)(content(Whitespace\" \
         \"))))(Tile((id \
         fbd3e2f9-0e23-4c3d-a8b8-a9fd2c32b08d)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e9f588b9-0234-4a1b-937f-0ca67e8e428c)(content(Whitespace\" \
         \")))))((Secondary((id \
         c4a093aa-abb1-4b9e-854f-e38b3b80fba0)(content(Whitespace\" \
         \"))))(Tile((id 445027c9-7d44-4f9d-ab8e-946e5eba60ab)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         56035d08-ebae-44a7-84dc-2734129621e5)(content(Whitespace\" \
         \"))))(Tile((id \
         ab4a0a6a-fa51-4bbf-90be-711201ec140b)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         53236e9b-2c25-421f-a232-c7d5239aaa74)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e293b6f9-f17d-4bf7-bd95-365b88b94c13)(content(Whitespace\"\\n\"))))(Tile((id \
         df52b634-9c80-4802-a274-b8ee5cc15d02)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dc679d75-c173-45dc-8a91-789336673807)(content(Whitespace\"\\n\"))))(Secondary((id \
         7fc3244c-f7c9-456f-bb2d-8df8274b1034)(content(Whitespace\"\\n\"))))(Secondary((id \
         4477e29b-586f-4b57-a9b5-c32d18cfc518)(content(Whitespace\"\\n\"))))(Secondary((id \
         27630dcc-8d29-41e1-8f6e-445e16c2c348)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         880c3c34-af4c-4d66-8a4a-c5253756a62e)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a75642a-240f-4435-9e75-e814e40939b9)(content(Whitespace\"\\n\"))))(Secondary((id \
         3630337b-bf3f-4cbd-a8e3-a04ce3acdc6a)(content(Comment\"# Extract just \
         the plant name from an entry #\"))))(Secondary((id \
         58259aaa-5e9f-4105-a56f-a2ec8d86889e)(content(Whitespace\"\\n\"))))(Tile((id \
         11ecd700-dfbb-4b31-b6bb-89501023f0d2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e17c16b-5922-4391-92a7-4e876f5de9b5)(content(Whitespace\" \
         \"))))(Tile((id \
         68f6079e-0851-4dd4-bdc0-dbc239fe8d55)(label(extract_name))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         0845c0e1-bc58-45f5-8dd1-df9d98f33589)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         cebf64be-a4b3-4490-a3f1-549c1ca8bf59)(content(Whitespace\" \
         \"))))(Tile((id \
         be7bc085-318c-4bbf-a512-f36d97998f4f)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d2152ee1-f36b-42b6-9539-67164ca244a7)(content(Whitespace\" \
         \"))))(Tile((id \
         8c30f5ae-62d0-485d-99a1-a45b40abe5bf)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5dcedb7a-d32c-4088-a5c1-6e6c9d558465)(content(Whitespace\" \
         \"))))(Tile((id \
         89ced3b9-80a7-48f9-bea5-5c073d8445bc)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         37db12c8-c793-4e0f-b8c5-28b084da721a)(content(Whitespace\" \
         \")))))((Secondary((id \
         5f35979b-8bde-4389-9d73-6bb3bb1d9c83)(content(Whitespace\" \
         \"))))(Tile((id a27bbadc-6cb3-4574-9e87-c77ab14c6233)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         61387bcb-4f99-423b-b196-0cc2a2ad098f)(content(Whitespace\" \
         \"))))(Tile((id \
         ba2dd599-d2a8-41df-8fdb-71793bba3fbc)(label(entry))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3ad32875-183e-460b-9a7c-1d8cfb0f475d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d95152dd-d4ed-4a93-a2aa-f1bc96367573)(content(Whitespace\"\\n\"))))(Tile((id \
         be67dd5b-98d6-4e2e-8514-326ea570e525)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d90c7ffa-3bca-4fb1-af52-087759c1d4c2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d9fb123-8feb-4363-8285-6d7efdc6d856)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f68670c-02b3-4153-993f-96a322093318)(content(Whitespace\"\\n\"))))(Secondary((id \
         3cb5f953-009d-4f00-90c7-cbabe6ea88d8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         4dd9aa6c-0891-4270-b475-2e830ce2d9ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         be0f0e6b-4552-41b3-83a3-ecf611cb7448)(content(Whitespace\"\\n\"))))(Secondary((id \
         baafc068-d975-44d2-87ad-0c3ee7029061)(content(Comment\"# Combine: \
         filter night entries, then extract names #\"))))(Secondary((id \
         9b1a4d96-e257-45e7-8005-ba9485ecd743)(content(Whitespace\"\\n\"))))(Tile((id \
         b7fd355c-7f4b-47d0-8ff6-31eb3b5010d5)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         94e8e145-cce6-48b1-b3b3-0a5842ff4142)(content(Whitespace\" \
         \"))))(Tile((id \
         8f7a2e83-b158-4159-b96c-07800b3c17ce)(label(night_names))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c892a3a2-baac-4193-b32d-122eb5d6ea0f)(content(Whitespace\" \
         \")))))((Secondary((id \
         619671e3-4d7d-42d7-bbff-be78063bad8c)(content(Whitespace\"\\n\"))))(Tile((id \
         9e3731b1-8604-478a-ae25-95733157e589)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f8f4bedd-8545-4f88-a243-1dfbdd4036a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         d49b8c8d-f096-4839-af4e-420177027030)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d8155e0-0be1-43a5-9b6e-138dba32cfd8)(content(Whitespace\"\\n\"))))(Secondary((id \
         df2430f5-4c2f-4c2e-8e3e-10137e74a713)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6216bc42-e7cc-4c4e-a238-5beaf24da0eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a5858e0-8f81-491a-bbaa-ae90b60dcc5d)(content(Whitespace\"\\n\"))))(Tile((id \
         07345cdb-db33-4f76-96ac-df90649d7a69)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         573f242b-4d67-4d81-a53e-83b47e2c201c)(content(Whitespace\" \
         \"))))(Tile((id \
         754d7e0f-757c-4fcd-a144-7abfa63bd30d)(label(night_names))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         92fbba16-c706-43b0-a1c0-00cc06384d3f)(content(Whitespace\" \
         \"))))(Tile((id \
         627d76d7-4fe9-4bb1-a354-9ea4976a621b)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1365af9d-4bef-4d1a-abdd-286fda0ca42c)(content(Whitespace\" \
         \"))))(Tile((id f788a056-b63d-44f7-868b-d35811cd3893)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f0905a11-884a-460e-95c7-d039dc14944e)(label(\"\\\"Moonbloom\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9c00ece7-0a9a-46a5-9162-1e7c85dc5197)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c81f2ca3-ad6d-4b23-9ae0-88c151246710)(content(Whitespace\" \
         \"))))(Tile((id \
         156cbb9e-41f8-4076-a676-fab03305b4b8)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         339645fe-d9a7-4497-b12d-f5f3d69e8fc4)(content(Whitespace\" \
         \")))))))))(Tile((id \
         c799ed2b-a287-4044-997d-950634406a04)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d8334c6-8fae-4ee0-b35a-9cf8582b290b)(content(Whitespace\"\\n\"))))(Secondary((id \
         57ca6351-ea87-473f-ab9f-4e91cbbdac4d)(content(Whitespace\"\\n\"))))(Tile((id \
         55ff399c-e043-4ab0-bc70-f5a52928e450)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f136a7e6-3fcd-4b4a-9925-bd7d95491f46)(content(Whitespace\" \
         \"))))(Tile((id \
         ce988c39-e75d-4772-b24b-e1f2a699e359)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         862ffc08-47d3-408a-a666-4c517a5965b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0ba11428-6af9-4cf9-ac9a-23873d03d2a4)(label(\"\\\"Moonbloom [night] \
         200ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7d6950f5-35b5-44ed-901d-1499e75d0fa6)(content(Whitespace\" \
         \"))))(Tile((id \
         37bf384a-413e-468e-ac38-23e0af1ea863)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         35583d01-0d7c-4620-9679-218438d36dd3)(content(Whitespace\" \
         \"))))(Tile((id \
         6c5a79e1-a3d5-4848-9340-1c1197bb4c77)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ba4f731-8c48-4ea8-a0fa-e0748ee47ad7)(content(Whitespace\" \
         \")))))))))(Tile((id \
         a9cbab74-19b0-4999-bc39-481498085eee)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d531ef93-4b27-42af-8f69-a8d6a5cf52ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         6c635055-242b-4980-8a48-7e14dc14bf5d)(content(Whitespace\"\\n\"))))(Tile((id \
         ac804b81-8dd5-4092-a19a-0a0dcbb67464)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         42f1c02d-bf31-43c4-a704-eb9c6cea85b7)(content(Whitespace\" \
         \"))))(Tile((id \
         5b2ba976-fe3e-4848-aabe-d6094a2aa518)(label(is_night))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         066bd747-5491-4c57-894c-ac00f4699ffb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         08f2c46f-4dec-41be-9fc6-3dff2ae81634)(label(\"\\\"Duskrose [day] \
         150ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         69f1b88b-5818-495e-93ec-bbf01a985c4f)(content(Whitespace\" \
         \"))))(Tile((id \
         2c3d1a7f-3a25-4c93-9844-e58bad513ff4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         08ff8a54-5015-4f3b-a8ea-259b2c80c80e)(content(Whitespace\" \
         \"))))(Tile((id \
         160414f3-b223-4f76-804d-be7005880a29)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6d35186e-8908-4344-923a-b68325128108)(content(Whitespace\" \
         \")))))))))(Tile((id \
         7b27037b-0b30-454f-a4ba-98681abcf304)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1a48d4c-8aae-44b2-8c4a-b0b054a4cd3b)(content(Whitespace\"\\n\"))))(Secondary((id \
         46e8e146-7428-499f-9fb8-ef52b1a4fca8)(content(Whitespace\"\\n\"))))(Tile((id \
         1be59fea-2ca8-4154-83fd-9e0b17697a75)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a9535c8e-2837-4115-b8c6-39afb83b4f5a)(content(Whitespace\" \
         \"))))(Tile((id \
         af2a04c3-4295-4de7-8ba5-a52a36dfadc2)(label(extract_name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         29debef3-de7e-4b40-ac94-42d494f81206)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         79e13620-f789-4348-83c7-f3f7ab5ed1d3)(label(\"\\\"Starfern [night] \
         175ml\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6395a1ca-ec1a-4336-8b26-ad8e38161a45)(content(Whitespace\" \
         \"))))(Tile((id \
         6a6b01eb-b46e-4624-994c-8b114dec8d0c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cf32d062-7021-4163-8cf8-dbd0f86bb0c9)(content(Whitespace\" \
         \"))))(Tile((id \
         2cff3083-1aa7-45eb-b1a4-1e1dce20d389)(label(\"\\\"Starfern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         307d7791-f1e0-45cf-9947-9186e434a7a7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7681b46b-19c1-45b1-bb9e-d1e2bbafd284)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# NIGHT BLOOM FILTER TASK                        #\n\
         #                                                #\n\
         # A plant catalog has entries like:              #\n\
         #   \"Moonbloom [night] 200ml\"                    #\n\
         #   \"Duskrose [day] 150ml\"                        #\n\
         #                                                #\n\
         # Filter to night-blooming plants and extract    #\n\
         # just their names: [\"Moonbloom\", \"Starfern\"]    #\n\
         #                                                #\n\
         # Steps:                                         #\n\
         #   1. is_night: check if entry contains \"night\" #\n\
         #   2. extract_name: get the first word          #\n\
         #   3. Combine with filter and map               #\n\
         #                                                #\n\
         # Available functions:                           #\n\
         #   string_match(pattern, str) -> Bool           #\n\
         #   string_split(separator, str) -> [String]     #\n\
         #   nth(list, index) -> element                  #\n\
         #   filter(list, predicate) -> list              #\n\
         #   map(list, fn) -> list                        #\n\
         #                                                #\n\
         # Note: string_match uses regex patterns.        #\n\
         # The pattern \"[abc]\" matches any of a, b, c.   #\n\
         #                                                #\n\
         # Tip: Use probes to see what your pattern       #\n\
         # actually matches -- regex can be surprising!   #\n\n\
         let entries = [\n\
         \"Moonbloom [night] 200ml\",\n\
         \"Duskrose [day] 150ml\",\n\
         \"Starfern [night] 175ml\",\n\
         \"Ghostvine [day] 100ml\"\n\
         ] in\n\n\
         # Check if entry is a night-blooming plant #\n\
         let is_night: String -> Bool = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Extract just the plant name from an entry #\n\
         let extract_name: String -> String = fun entry ->\n\
         ?\n\n\n\n\
         in\n\n\
         # Combine: filter night entries, then extract names #\n\
         let night_names =\n\
         ?\n\n\n\n\
         in\n\n\
         test night_names == [\"Moonbloom\", \"Starfern\"] end;\n\n\
         test is_night(\"Moonbloom [night] 200ml\") == true end;\n\n\
         test is_night(\"Duskrose [day] 150ml\") == false end;\n\n\
         test extract_name(\"Starfern [night] 175ml\") == \"Starfern\" end\n";
      refractors = "()";
    } )
