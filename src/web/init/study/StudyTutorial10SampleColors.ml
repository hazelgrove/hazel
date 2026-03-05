let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 10-sample-colors",
    {
      segment =
        "((Secondary((id \
         95ae76fd-c254-4ff8-bd40-7a787be99de7)(content(Comment\"# PROBES \
         TUTORIAL - PART 10: SAMPLE COLORS                        \
         #\"))))(Secondary((id \
         34739138-443d-4016-b988-e5c1d5371cea)(content(Whitespace\"\\n\"))))(Secondary((id \
         86b0f87e-e724-4aa8-8da2-e0ff56fde2ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         1cb2e041-9a96-4b14-8dca-665fb2d9bd65)(content(Comment\"# When you \
         click a sample, other samples change color. These      \
         #\"))))(Secondary((id \
         c6f11abd-f5d5-4529-a2d4-6ecb9d581ff5)(content(Whitespace\"\\n\"))))(Secondary((id \
         deb7fee6-7374-45e1-a217-10109ed6a7e4)(content(Comment\"# colors show \
         how each sample's evaluation relates to the one     \
         #\"))))(Secondary((id \
         1f4a720b-632d-4708-a0d5-46cfc0bdb6f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f29e5548-5d00-48ca-9636-2bd583b2813b)(content(Comment\"# you \
         selected, whether it ran before, after, or nested          \
         #\"))))(Secondary((id \
         4481ec70-96ff-40ad-b701-42a70df4f406)(content(Whitespace\"\\n\"))))(Secondary((id \
         e30a99a2-acd8-4596-b353-0daf75d3af01)(content(Comment\"# inside or \
         around it.                                             \
         #\"))))(Secondary((id \
         222c5ee7-153d-4b0c-8783-9bc94ad18a12)(content(Whitespace\"\\n\"))))(Secondary((id \
         a5261d18-6b2b-48e4-bbc9-a5ef8ce5ae7a)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         0f085107-01de-4db0-9ace-f929ba63c9e8)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7c9a382-0509-4190-a7a2-861d2e946697)(content(Comment\"# Look at the \
         LEGEND at the bottom of the probe sidebar.          \
         #\"))))(Secondary((id \
         57051e2e-c9b1-434d-ab96-7e2015675e73)(content(Whitespace\"\\n\"))))(Secondary((id \
         8df0d270-b0d1-486f-abfd-9b3197371ccf)(content(Comment\"# Hover over \
         each entry to see what it represents.                \
         #\"))))(Secondary((id \
         7ed5878d-b35e-4841-af02-84005f78bf78)(content(Whitespace\"\\n\"))))(Secondary((id \
         3298c798-a017-4be3-b5aa-4a8809371385)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         eb017d58-ebac-4bc1-9e97-cee41aa22c53)(content(Whitespace\"\\n\"))))(Secondary((id \
         51edaa18-3eff-48bf-b640-11375f7ec009)(content(Whitespace\"\\n\"))))(Secondary((id \
         a25e5481-4eea-4d01-a792-81e1c5bf464d)(content(Comment\"# BEFORE AND \
         AFTER                                                 \
         #\"))))(Secondary((id \
         26f46047-bcb7-44a3-851a-77843591cfd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e8a31255-0352-40ca-9b55-195c3f33dba2)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         d04b0db9-34c0-4991-a977-2a443feb003b)(content(Whitespace\"\\n\"))))(Secondary((id \
         1200faef-b102-4aaa-8c2b-7893d9630beb)(content(Comment\"# The simplest \
         case: expressions that evaluate in sequence.        \
         #\"))))(Secondary((id \
         b6529cd9-8b3e-48bf-836f-78265c3bdc6c)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c9b456a-14b7-404b-82ae-f25fe2edac40)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         e9967c08-a037-49bd-8900-9a4e8dcc5d55)(content(Whitespace\"\\n\"))))(Secondary((id \
         0c791898-7449-4158-aa8f-6d1751befb81)(content(Comment\"# TRY THIS: \
         Click the sample for `second` below.                  \
         #\"))))(Secondary((id \
         797ab891-13bb-4557-8923-299705f609e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         b4cf2f59-89d0-4f59-94b5-ab6814f88397)(content(Comment\"# - `first` \
         shows a Before color (it finished evaluating earlier)  \
         #\"))))(Secondary((id \
         5ea5167e-b200-4393-abf2-140aa6cb6491)(content(Whitespace\"\\n\"))))(Secondary((id \
         46b3f331-9714-43d0-885d-f9db46f40a0b)(content(Comment\"# - `third` \
         shows an After color (it hasn't started yet)           \
         #\"))))(Secondary((id \
         65637418-df8d-4bb1-b446-44e92f072917)(content(Whitespace\"\\n\"))))(Secondary((id \
         d7fa0475-da53-4246-a3a7-30f50907dce4)(content(Comment\"# Click \
         `first` or `third` to see the pattern shift.               \
         #\"))))(Secondary((id \
         476e8b78-79a5-496b-982d-81c849e4928a)(content(Whitespace\"\\n\"))))(Secondary((id \
         41339770-b56a-47e2-bdc0-c7c2cb046a7d)(content(Whitespace\"\\n\"))))(Tile((id \
         6a8e3f46-c4b5-44b1-a49e-fb1a721223e4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c8bc893b-f4fe-4e01-abcd-b19396bf4bf9)(content(Whitespace\" \
         \"))))(Tile((id \
         8135d125-87a0-4d0f-b71f-e2be166caf1f)(label(first))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         896bda18-853e-49ff-95e4-afee700a73c9)(content(Whitespace\" \
         \")))))((Secondary((id \
         050949c5-dc9b-4fe1-86a5-1f1f5dddfd14)(content(Whitespace\" \
         \"))))(Tile((id \
         d8dd124a-1244-4572-aea8-4b72e75249ec)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4ec977a5-cb32-4165-9f13-350af8445ed9)(content(Whitespace\" \
         \"))))(Tile((id \
         b0e611d2-30b4-45ce-ad5f-119a9a7a5162)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d8afc43f-44d7-4ba3-b3f8-ff06fcc66b5a)(content(Whitespace\" \
         \"))))(Tile((id \
         a61f39ae-66ed-44c1-9da7-705582ea9825)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8f9510fe-66ab-41d6-9a7a-952bbfbc69d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2c420bd5-98ca-4a56-aa0b-a9c7236538d8)(content(Whitespace\"\\n\"))))(Tile((id \
         45f1b139-2cda-4e23-8224-5d2fc82ccf37)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         34fcc50c-a7c9-4965-8051-42a5e44fbf66)(content(Whitespace\" \
         \"))))(Tile((id \
         b0d5c61e-2122-4c58-8606-b73f19ee36e5)(label(second))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f9f92d07-c873-47b3-ad7f-7b23101a1c3f)(content(Whitespace\" \
         \")))))((Secondary((id \
         a83fc377-856f-4663-9131-75ad8d53cb8b)(content(Whitespace\" \
         \"))))(Tile((id \
         5eab5265-feab-406a-be12-59ca13c72976)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c28c23f8-326e-4abd-a0be-d2fcb0ef3d40)(content(Whitespace\" \
         \"))))(Tile((id \
         34c2a9c4-5ac7-4c75-8bf4-4a3047435cad)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         89260950-f673-4b7c-be33-f39e03b50f4b)(content(Whitespace\" \
         \"))))(Tile((id \
         18355b79-d41c-472b-b603-3946daec4816)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e77dca30-0c14-4a9a-a150-c3970937f423)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9dabdfd9-1302-4d4a-b109-3d12de82c688)(content(Whitespace\"\\n\"))))(Tile((id \
         fa1d901c-3d21-47ed-81b0-e80a7eae0510)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e7fc81e4-ddd9-4909-9060-f56bb07c4d7f)(content(Whitespace\" \
         \"))))(Tile((id \
         4122f0a2-8848-4d5d-bb2f-84ef45220016)(label(third))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6eae2ad-0e03-4de9-b621-23b4af37d1c9)(content(Whitespace\" \
         \")))))((Secondary((id \
         7b372fb5-fc41-40d9-9948-595ccbe515fc)(content(Whitespace\" \
         \"))))(Tile((id \
         c92c4ffa-69a9-4abc-87b9-ce22ed765845)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7f2d50ee-b8e0-4a93-955f-1cccbc70bca8)(content(Whitespace\" \
         \"))))(Tile((id \
         189d49ee-4766-4291-9b34-6d001da763c7)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5ef6f009-1ec5-421c-b326-29a9f7f2c2e2)(content(Whitespace\" \
         \"))))(Tile((id \
         498d10dd-1819-4c17-9b9b-e19b867fb63e)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         38398b6f-5a7c-4e91-acb7-a4d5bfa5d481)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         666ae0d7-1e0e-4b9f-bd29-6ee96d5dedf4)(content(Whitespace\"\\n\"))))(Secondary((id \
         b6b7115b-c573-4a8f-97e4-696f970363ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         da910b58-e302-49be-8679-5aaf2f990536)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         007a6bdb-ae50-4c00-9595-956d283f1cf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         f2f9b429-e14d-4c6a-8c5d-cbe07833f9cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         c1bb0ef5-e704-4e82-9c8e-126cde89b1f6)(content(Comment\"# CONTAINS AND \
         INSIDE                                              \
         #\"))))(Secondary((id \
         a81b9e6a-5111-42bc-94b8-df541f95048c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d58fdb0-7cd1-44fa-b305-b609ebb44dc7)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         7f762a5d-1267-4749-af50-be06adff5adf)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd082727-78c4-402e-b12b-c2fce62f2f28)(content(Comment\"# Evaluation \
         isn't just a flat sequence... it has depth.            \
         #\"))))(Secondary((id \
         07716d30-4124-43c9-a95f-15491f21d27b)(content(Whitespace\"\\n\"))))(Secondary((id \
         26820e48-f216-4559-9206-ba405e52d1e6)(content(Comment\"# When you \
         call `daily_water(250, Full)`, the call starts,        \
         #\"))))(Secondary((id \
         004b18f9-e9ce-46f6-9668-f3da1b1ffd53)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b733681-72ea-4b8c-ada1-9beece47e05d)(content(Comment\"# then the \
         function body runs, then the call finishes.            \
         #\"))))(Secondary((id \
         4c8eff0d-f7dd-4dc7-af01-932709b199e6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ecf666c2-ce94-47e9-a2cb-8a8216d44e90)(content(Comment\"# The call \
         *contains* the body; the body is *inside* the call.    \
         #\"))))(Secondary((id \
         01f0e33b-3e8a-450a-825b-5e8fa8ee3c14)(content(Whitespace\"\\n\"))))(Secondary((id \
         944bbdd1-4a14-42f3-80c9-8e6ee617cae7)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         39974c9e-3b99-492b-b2ea-b1b611e5b2f2)(content(Whitespace\"\\n\"))))(Secondary((id \
         e85c1bfa-bf92-4dbb-a81b-54ccceccb3a4)(content(Comment\"# Contains and \
         Before share a color because both represent         \
         #\"))))(Secondary((id \
         367a2e63-c278-49c3-b174-11eab879683f)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd818945-6c47-4fd6-aa97-195b38951cad)(content(Comment\"# things whose \
         evaluation *started* before the focus sample.       \
         #\"))))(Secondary((id \
         63b6d4a5-8f0e-4381-8921-aa98c9174788)(content(Whitespace\"\\n\"))))(Secondary((id \
         1931ba2b-b971-422a-a309-09472fc93d1e)(content(Comment\"# Inside and \
         After share a color because both represent            \
         #\"))))(Secondary((id \
         77bc7e91-4ed5-4b38-bbd9-a897404b12c4)(content(Whitespace\"\\n\"))))(Secondary((id \
         f047666e-2f1d-46ca-aadd-3c79415d5d0d)(content(Comment\"# things whose \
         evaluation *finished* after the focus sample.       \
         #\"))))(Secondary((id \
         52c73263-0b66-472c-8dfe-707115aec9ca)(content(Whitespace\"\\n\"))))(Secondary((id \
         0546c713-d08e-4a49-b230-4c76aab81816)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         808e0f09-4f62-4d8a-83b2-b19ca781a21a)(content(Whitespace\"\\n\"))))(Secondary((id \
         44ce2dd3-babc-4539-bcb5-8231791334b6)(content(Comment\"# TRY THIS: \
         Click a call sample at the bottom of this section     \
         #\"))))(Secondary((id \
         e8015fcd-a6cf-4393-a8be-b4e6d18e8a92)(content(Whitespace\"\\n\"))))(Secondary((id \
         9e46dfc4-7236-4a7b-a1d2-c365c3fb8b8f)(content(Comment\"# (e.g. the \
         first one). The body samples above change color       \
         #\"))))(Secondary((id \
         aa409f76-d4f4-40ef-8675-af705f5533f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d42f7638-3a76-473b-9559-c0de35075436)(content(Comment\"# to show they \
         are INSIDE that call.                               \
         #\"))))(Secondary((id \
         522fdfc2-9f98-48c0-88c0-27504f453dc9)(content(Whitespace\"\\n\"))))(Secondary((id \
         7cd32558-8d09-4393-844c-dd437410a818)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         e4a0fb21-9a99-48e3-8295-e83dd6db197d)(content(Whitespace\"\\n\"))))(Secondary((id \
         075fa68f-a8f0-4bc3-a728-b4bdb81711a2)(content(Comment\"# Now click a \
         body sample instead (e.g. `adj`). The calls         \
         #\"))))(Secondary((id \
         d9fb76ef-5d39-449d-ab92-0975ed0f53d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a2b26b9-3b09-4d40-9f3a-f95615c60377)(content(Comment\"# below that \
         launched it are colored as CONTAINS.                  \
         #\"))))(Secondary((id \
         71a5ef32-c012-409f-9600-79a3a5950510)(content(Whitespace\"\\n\"))))(Secondary((id \
         37203044-9e25-4684-94a8-67acf63252d5)(content(Whitespace\"\\n\"))))(Tile((id \
         44a84b3b-855f-4d99-9e74-5c054011152b)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         75566046-2f9a-4fea-ae09-8ba9cc559a6c)(content(Whitespace\" \
         \"))))(Tile((id \
         29161f49-3ff5-45b8-a6b5-decaa33820bb)(label(MoonPhase))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         4fb589c4-436a-436d-b8e3-ad330a405aec)(content(Whitespace\" \
         \")))))((Secondary((id \
         1541c853-3099-4815-a8d6-da38c63d2c55)(content(Whitespace\" \
         \"))))(Tile((id \
         b23adea3-b21e-48a9-8b79-ab75ab6a1753)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape(Concave 33))(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         2305d11b-9fbf-4c8a-b6e1-7c786bcb9b55)(content(Whitespace\" \
         \"))))(Tile((id \
         40793eb6-c20d-4555-bc45-9cb955de3589)(label(New))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4f13beba-5a76-4bac-80a3-7c57ad046761)(content(Whitespace\" \
         \"))))(Tile((id \
         3721f313-59ed-4214-bb77-f89877d1a262)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6373c0ac-156a-45ea-a877-aceced6e6231)(content(Whitespace\" \
         \"))))(Tile((id \
         503454a1-de30-4c6b-bf8c-c8f2ae220d2d)(label(Waxing))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         848302b2-a59c-4769-8a93-f1834eb174fd)(content(Whitespace\" \
         \"))))(Tile((id \
         b2313b19-3dd2-4ee3-bce1-a429d9d5ad0f)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         5b3cc9cc-a817-414a-943c-50d67f5103d4)(content(Whitespace\" \
         \"))))(Tile((id \
         50ddd1ba-beec-4cec-8e71-011dfd1ca47e)(label(Full))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         e117aac0-fd73-43c1-9750-86bf03739ee3)(content(Whitespace\" \
         \"))))(Tile((id \
         aa5ae507-9154-48ac-95db-3564cf8edebd)(label(+))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 12))(sort Typ))((shape(Concave \
         12))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         2bec958c-cfe3-4279-8601-6aea401471ea)(content(Whitespace\" \
         \"))))(Tile((id \
         325fd293-364b-438f-9fb2-053562656563)(label(Waning))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         024f5101-8d0a-429e-a344-34f8d143bae8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         08144cf3-c810-4871-be2b-ec48a3eaf330)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe725500-7ec7-4538-819d-9f8e55032eb9)(content(Whitespace\"\\n\"))))(Tile((id \
         a427c886-7447-452e-8917-5733b3f5938f)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         3e0ad45f-cef1-48cf-895f-cc0dadd84aa3)(content(Whitespace\" \
         \"))))(Tile((id \
         436c69ca-0acb-4696-a67a-56a5205184a7)(label(phase_mult))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         998b5719-b132-4ddd-9320-b6af896ebc2f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         6069c373-2b0f-4803-b365-8fb1b3f5af05)(content(Whitespace\" \
         \"))))(Tile((id \
         e9c3c6aa-0e37-41a8-b99a-1fc720fe10d8)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         bb489d94-09e9-40a7-8a4d-b7ef9dac772b)(content(Whitespace\" \
         \"))))(Tile((id \
         0ea95b7c-30e9-4d52-9d99-7c3f05a6645f)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         0b4b2569-87fd-402e-b311-f84684953e63)(content(Whitespace\" \
         \"))))(Tile((id \
         89c9d007-4fb7-41b7-9843-e3619f7f8e5b)(label(Float))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         42c05339-2d5d-432f-af65-7de923c2b7af)(content(Whitespace\" \
         \")))))((Secondary((id \
         2d6a5854-e523-4b71-8ae1-10f197dc9376)(content(Whitespace\"\\n\"))))(Tile((id \
         577b0508-ed6f-408e-a984-ec76308ac22c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9de61414-d18d-478a-a98e-3b59aa5b466e)(content(Whitespace\" \
         \"))))(Tile((id \
         a82826cb-1b39-40f1-a62d-7cd21a22abef)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5e1606db-3422-4706-9d04-e05afcbcaeb8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         9e2876f5-2fc7-4410-9b15-7caebbb4831e)(content(Whitespace\" \
         \"))))(Tile((id 16e11271-7ae8-41b5-868e-29e90020e4fb)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         82383d68-dc10-4eb1-9208-d63578b87430)(content(Whitespace\" \
         \"))))(Tile((id \
         4f9d780b-e302-488f-84d9-c81561cac13b)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c86e4ae4-0daa-4368-a244-3edfd67910d1)(content(Whitespace\"\\n\"))))(Tile((id \
         6be31aa9-c511-449f-8550-edbed46ad094)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         4b6c1f4e-26da-4819-a2a8-9d4af704d5ee)(content(Whitespace\" \
         \"))))(Tile((id \
         f1c01a5a-2819-4299-967b-5e75cf0cc430)(label(New))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         742ffad4-b056-448f-b9fb-0f288135ee7a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f8075af8-4fee-4738-9e08-6d8beb3cd2d2)(content(Whitespace\" \
         \"))))(Tile((id \
         c89743f7-e00a-4004-ab6f-c8696203d823)(label(1.2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         da43cca1-a14c-45a0-a025-238b5e52d116)(content(Whitespace\"\\n\"))))(Tile((id \
         c842bcdd-80de-4c24-a4be-6963ca17091c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         ede73dd9-15ee-48cd-9fd1-5beeb245ed30)(content(Whitespace\" \
         \"))))(Tile((id \
         308100d8-1a85-425a-b9b0-92c2fbf36ac9)(label(Full))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9eaf1603-ca81-4735-95e5-6de2b64accda)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dbfb0da8-121b-4e57-85e9-907c24bc9949)(content(Whitespace\" \
         \"))))(Tile((id \
         c7df37c3-adbe-414f-bdf1-b71f824c7293)(label(0.88))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         49b59c32-830d-4bdc-a3d8-381f1568f02e)(content(Whitespace\"\\n\"))))(Tile((id \
         589c124f-bdc6-4fa8-92db-023199e75f74)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         99b09e63-e8fc-4bc8-b042-08c44707d5fd)(content(Whitespace\" \
         \"))))(Tile((id \
         a9488298-2708-4ec3-88f1-98afb7000a1d)(label(Waxing))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f2558859-9d8e-4c83-9f4d-acd51a2d8bbd)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         a7eb727a-93aa-4231-a970-ab296e84dae5)(content(Whitespace\" \
         \"))))(Tile((id \
         4d212fea-b39e-475b-9200-8d546bf68b80)(label(1.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         af850d50-ccbe-4a90-bc1f-70e9ae4a803d)(content(Whitespace\"\\n\"))))(Tile((id \
         ee19311e-25f1-416a-913a-a883f47eace3)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a30fb88b-4c1a-4e5f-a4f5-6b778d38d5eb)(content(Whitespace\" \
         \"))))(Tile((id \
         2f194da5-9580-4118-8b44-521f1b29d876)(label(Waning))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9d0f3405-7e21-4a56-a46d-36d297874598)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8908014e-2b24-4ff8-bb9d-e42cc6ee127e)(content(Whitespace\" \
         \"))))(Tile((id \
         46dea220-74e7-43b1-af14-271217f93a83)(label(0.95))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         64564dba-8579-48fe-9d02-9a8e73c2049b)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         ad282fba-62ad-4cf3-9390-f67f48c6506d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d95fc16b-6f3b-4646-a5ce-cc067a116538)(content(Whitespace\"\\n\"))))(Secondary((id \
         b5aeaa12-8d67-4f6a-9fec-c9af57d82558)(content(Whitespace\"\\n\"))))(Tile((id \
         a1c4eb12-505c-4648-96a8-9f1784492e87)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ae81eab7-02d5-4a3c-bcd2-0734e264d893)(content(Whitespace\" \
         \"))))(Tile((id \
         22158efa-a9ad-450f-aafc-18ee621f0ddf)(label(daily_water))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1b3bcc2a-e37c-490f-beba-e0b4d21915b2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         9e4b79ca-f881-4cf3-94af-7f96f686c214)(content(Whitespace\" \
         \"))))(Tile((id \
         4b4d5174-d615-44fd-8e9e-62912aa6faa0)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         5cceeb24-c50c-4f76-9a63-36b83b29d2c4)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         e9ffa278-86bc-454c-973e-934e6d1318b9)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         83027cc4-cbf3-4f41-81aa-96b562cd047d)(content(Whitespace\" \
         \"))))(Tile((id \
         c4cf7189-a402-4c99-a84c-fca2ba0ee1ae)(label(MoonPhase))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         64c93b73-5c2d-4bcb-9c2d-e9997cca28f1)(content(Whitespace\" \
         \"))))(Tile((id \
         e4db83e4-ce75-4d35-8414-708717ca433b)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         685a2209-f070-440b-a5f2-019dd8b6db40)(content(Whitespace\" \
         \"))))(Tile((id \
         717f7d4d-0661-4bab-b7bf-04b1251cd45f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d9ed6865-2452-4bee-895e-4256151295f5)(content(Whitespace\" \
         \")))))((Secondary((id \
         aceb8b90-56cb-444e-9078-e51e54f61cad)(content(Whitespace\"\\n\"))))(Tile((id \
         03588565-2bfd-4e55-bcdf-7d1b7bcec2b2)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         6fb3bc98-20e4-419b-ae0c-6ab4414ec976)(content(Whitespace\" \
         \"))))(Tile((id \
         de560276-6e10-4124-8d24-022899f7bf0b)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         a6159bd9-6525-4adb-9409-b70186d7a766)(label(base))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         8569e638-6353-4932-8f36-d096a67903ca)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e3ee2b48-b4b7-4740-9f6f-44ac282ad354)(content(Whitespace\" \
         \"))))(Tile((id \
         ba99b9d1-c542-4f08-8ad6-c05d3894df7c)(label(phase))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         4ba34e88-812a-4621-84c5-f460193a22ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8544254e-8ba3-4177-97f9-ece3d40fa7f5)(content(Whitespace\"\\n\"))))(Tile((id \
         05901568-a1af-45ad-8bb4-aaebcd7f62da)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4fd1247a-60df-4766-8a7c-b7bdd5a9316a)(content(Whitespace\" \
         \"))))(Tile((id \
         1060099c-3200-4c0c-90ed-e66335e0fef7)(label(adj))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         27872716-e9c2-4ea9-96d1-c01da4d51b8e)(content(Whitespace\" \
         \")))))((Secondary((id \
         081b6332-e712-496a-b9df-b1588830d21a)(content(Whitespace\"\\n\"))))(Tile((id \
         bf47b52d-d79c-4c56-bea6-dfc4f4d97895)(label(float_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4f4a47e9-1d9b-4fb2-8a81-ed20fa1715a8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3d5b76d9-fd20-4720-94d4-0d6058438ebd)(label(base))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         03dff1f7-bf17-4ba9-becb-30add8b8baaa)(content(Whitespace\" \
         \"))))(Tile((id \
         4964a68e-56ca-481a-abfd-37ea00c111ac)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2626e424-887d-4d59-be14-53f9e7b60759)(content(Whitespace\" \
         \"))))(Tile((id \
         25dda4dd-fa6b-42e1-85d7-96f26cc06f51)(label(phase_mult))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         121c72ca-510a-4030-914b-bac05b2a7c12)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         831396ee-df50-44f0-b101-0081850f1080)(label(phase))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f90aae36-f4bf-40bd-9e7e-fe7f0688cd98)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         8aa42713-00cf-4ec9-a45b-281cab2ddf52)(content(Whitespace\"\\n\"))))(Tile((id \
         ad922f17-f84b-4aa8-ab77-c4f46b7d7a61)(label(int_of_float))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c709e3c5-4c12-44cf-a305-1e7e972763d0)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         692c0b1d-81af-40c0-9da7-527db7077fc9)(label(adj))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3119dc82-54bd-4cc4-87ef-6739d28c9f9d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d9605fc9-dd4c-42ee-9f02-ba2b5cd87be9)(content(Whitespace\"\\n\"))))(Secondary((id \
         cb37b1db-b0a2-4b21-90f0-7cb337276f9a)(content(Whitespace\"\\n\"))))(Tile((id \
         c97167a9-ec74-45bf-90e7-e2c5fcac82e3)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         407697d1-b637-4553-b4be-6db2ff04f0aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3ab2f028-8305-4ca6-8d54-4d77df8fb634)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         49ba59a2-37f2-4e28-9350-e873102b4c2d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         85955315-0eb5-4337-bac9-ffe5ff38c0b4)(content(Whitespace\" \
         \"))))(Tile((id \
         c2ec9410-1a66-474c-95b9-bfb2f03962bf)(label(Full))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         fddf36f4-7dfa-40f1-b074-1ce7ac5adb3e)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3aa01175-0a27-40c4-be00-9c10117da25d)(content(Whitespace\"\\n\"))))(Tile((id \
         70e1a2de-de2a-4e49-94a1-cadf26fadf37)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e3c50f70-2faa-4d37-8471-9ab667f9553c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         61894027-795e-4db1-8075-b92e2f95cb48)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6b4196e1-8f23-4686-a646-379868365594)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f3056e26-c9f5-4543-bbf6-85566158fea9)(content(Whitespace\" \
         \"))))(Tile((id \
         09725ea6-76aa-4cc8-b61f-313b64a76076)(label(New))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         e93c7ca3-a16b-439c-ab94-a58e22767948)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         57c03d46-a43d-44d4-bdec-e843567bdec0)(content(Whitespace\"\\n\"))))(Tile((id \
         855dc37f-d335-4c56-ad6b-d644ca14b46f)(label(daily_water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d5e1f2da-e1f9-4fb9-a99c-b1d1aa9e02b4)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f5382b3e-353c-4330-87b6-859935c4fd48)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         64335b26-1bdd-4cea-9003-3462730bfe4f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         46635cb3-1e64-4f8c-8036-dd9754cee166)(content(Whitespace\" \
         \"))))(Tile((id \
         a1138430-8c79-4397-aa29-0b5962d7b8e8)(label(Waning))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         c29c0422-ea05-4204-ad4a-c21ed29bcad6)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6bbe10d-1b8e-4d4c-be27-9df2058fa23a)(content(Whitespace\"\\n\"))))(Secondary((id \
         dbf56c1d-2b1f-4aad-a914-df55e1ad8964)(content(Whitespace\"\\n\"))))(Secondary((id \
         095c6aef-c6a6-40be-aab5-4f2da7700fa0)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         e010d67a-03ee-46c5-83f3-e33f6a927993)(content(Whitespace\"\\n\"))))(Secondary((id \
         bc68ffe4-8100-4ae9-b778-f6275a84f21b)(content(Whitespace\"\\n\"))))(Secondary((id \
         595eb250-897c-49a0-b361-d2f2a6f5d594)(content(Comment\"# DEEPER \
         NESTING                                                   \
         #\"))))(Secondary((id \
         1d0326dc-d598-473c-893d-81d98c795671)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a2336a3-3469-4248-89a0-331467f207fc)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         60647f69-0015-4963-a369-e6044bebb579)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce7895a5-f976-453b-8a77-8de7b1bb013d)(content(Comment\"# With map, \
         each iteration of the callback is inside the           \
         #\"))))(Secondary((id \
         b1aa4914-3765-45f4-948f-920734efe72a)(content(Whitespace\"\\n\"))))(Secondary((id \
         23748f4c-3d34-405e-8493-33c5da237e12)(content(Comment\"# enclosing \
         call. Different calls to the same function show        \
         #\"))))(Secondary((id \
         07a0d56a-9f7e-46b7-9c11-98b584dda2a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fe4be5b-1220-4d6a-85cc-963ce1581c5f)(content(Comment\"# as unrelated \
         (gray) they're siblings, not ancestors.           \
         #\"))))(Secondary((id \
         9cbd1580-15ec-4567-9764-7e8c2ba5e9b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         e0dbb38f-bfdd-4596-80df-803205c5cd0c)(content(Comment\"#                                                                  \
         #\"))))(Secondary((id \
         45556bff-cd37-4d19-bd8e-58c706676a3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa28f2c1-4fad-48ea-9fdf-653a9f0c4a99)(content(Comment\"# TRY THIS: \
         Switch to Many mode (double-click a sample).          \
         #\"))))(Secondary((id \
         740fb81a-476e-44e3-8515-c24cdca10dd5)(content(Whitespace\"\\n\"))))(Secondary((id \
         d53b8c02-7e54-4dcb-8485-1c12ebfc881b)(content(Comment\"# Click a \
         sample at the `plant` level inside the map callback.    \
         #\"))))(Secondary((id \
         a7d96624-f157-453c-8644-e9ce97305209)(content(Whitespace\"\\n\"))))(Secondary((id \
         bf4e8b07-78c7-445c-8b4d-d22c4f7685ce)(content(Comment\"# - Samples \
         from the same outer call are colored (related)         \
         #\"))))(Secondary((id \
         24a8225e-a560-4582-abe7-19bd8cda23af)(content(Whitespace\"\\n\"))))(Secondary((id \
         37946701-2672-479c-84bb-c8dbdc269537)(content(Comment\"# - Samples \
         from a different call to bed_labels are gray           \
         #\"))))(Secondary((id \
         57226073-7b91-47bc-b46f-793fd1cb0566)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d15d520-b8b8-4544-8a5c-5e3d103af8c6)(content(Whitespace\"\\n\"))))(Tile((id \
         8d307190-07bb-47c2-9100-27af9d45ba56)(label(type = in))(mold((out \
         Exp)(in_(TPat Typ))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9e1cbbb5-bd62-4fc3-8e33-7b7e2166160f)(content(Whitespace\" \
         \"))))(Tile((id \
         97b47655-e459-4121-bf16-4305c5d7f4fe)(label(Plant))(mold((out \
         TPat)(in_())(nibs(((shape Convex)(sort TPat))((shape Convex)(sort \
         TPat))))))(shards(0))(children())))(Secondary((id \
         61e47f24-bf09-4118-81b4-44c78cc87198)(content(Whitespace\" \
         \")))))((Secondary((id \
         fc80ff9e-91b4-4e92-8b3b-49901a147c34)(content(Whitespace\" \
         \"))))(Tile((id \
         b6c7cd85-11d9-4990-b9aa-dcd33ca91f96)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         2148607b-13be-419c-a467-7798ecec89f8)(label(name))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6928b865-d3ba-401e-8824-c8fa3fe13953)(content(Whitespace\" \
         \"))))(Tile((id \
         bcc31be2-fa02-49da-91b0-b6af3cdd88ba)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         06c176fe-efe6-474c-a3f1-757e76966530)(content(Whitespace\" \
         \"))))(Tile((id \
         f56250c2-77bd-4ce4-8783-30e7002c976c)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         513b791b-7f2f-465f-8f07-9d4cf8a3e848)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         f1302021-5e33-4c72-85a9-fdc002f9d8c9)(content(Whitespace\" \
         \"))))(Tile((id \
         3a7a7973-494f-47ac-bf05-8384555d6946)(label(icon))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9b8110f8-9c59-485e-ba32-27c10fd8b862)(content(Whitespace\" \
         \"))))(Tile((id \
         7ac05233-ab45-4a2d-8bf4-adebc01a33fc)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a013fd20-cd61-4d5d-b28c-a72c7e551d27)(content(Whitespace\" \
         \"))))(Tile((id \
         2cf2429a-e0db-477c-99c3-e5b2a302b8a9)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         507189bb-6504-43ff-a3d5-69c43ff71f41)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         28e40615-5482-4798-9e40-bdd0d2c95a82)(content(Whitespace\" \
         \"))))(Tile((id \
         d6558893-224c-4f9f-afc2-98a904dfd83c)(label(water))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         d23fb8fa-08a6-4655-9543-6dc5266d07ac)(content(Whitespace\" \
         \"))))(Tile((id \
         97749285-924c-428c-8258-380df887c3cb)(label(=))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 39))(sort Typ))((shape(Concave \
         39))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         daa3f539-7478-4fba-9dac-a87afb11d8e4)(content(Whitespace\" \
         \"))))(Tile((id \
         187ab72d-9784-490c-89aa-294ae972b06f)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         1b60787c-cda1-41cd-80fb-070db1b0dee0)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         200b10ef-bf37-4ee0-8239-b656e89952ce)(content(Whitespace\"\\n\"))))(Tile((id \
         dc8409df-1bb4-4cb6-b8cc-84b530b2571b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         06da0fbe-fb48-43f4-880c-f8218c36725f)(content(Whitespace\" \
         \"))))(Tile((id \
         d926d54b-2e9d-4100-a346-2855d1892c1e)(label(fern))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         fb551bc8-c1a7-4af3-b6d0-98bdf56f8208)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         e365b081-a0f5-4dd9-9288-1e963906c97c)(content(Whitespace\" \
         \"))))(Tile((id \
         e49ae5a7-5953-4bb5-b90d-071a8d5249d1)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         f950a55c-6239-4185-b9a4-7eb558a218d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         d704fd0d-67b6-4eb9-817f-636ddacf32b2)(content(Whitespace\" \
         \"))))(Tile((id \
         b6951492-5648-4535-b3a1-e9e53d900754)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bfe03b88-a85e-4786-9cae-5f637b38f85c)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         06496251-e6bc-408c-8fe6-f5d7a41bddde)(content(Whitespace\" \
         \"))))(Tile((id \
         1703f4aa-787f-4db6-ad76-cee5963383c6)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         319a3b29-e5c4-453f-a624-6ab50c30cd7d)(content(Whitespace\" \
         \"))))(Tile((id \
         761c63ee-4ff9-4528-8bdd-bf72e9817e1d)(label(\"\\\"Fern\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         35ca5a2d-f745-430c-9a97-3c81fe8f7dce)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6f2fa1db-43fc-4748-b55c-8173e46a4abb)(content(Whitespace\" \
         \"))))(Tile((id \
         c088246a-a474-45e7-90af-3131cd70bcd1)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         85990aff-cb9d-470a-ba00-d350873d4241)(content(Whitespace\" \
         \"))))(Tile((id \
         2b6397cb-8995-41a3-8fd2-a1acd2219be7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         107e7dc9-2247-429b-84ed-7a2c60adcfa5)(content(Whitespace\" \
         \"))))(Tile((id \
         c7969c59-f3e5-47d6-b09b-7c891d4216e4)(label(\"\\\"\\240\\159\\140\\191\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f34f9238-f792-4db5-b205-50d546fc953f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c9811b89-7135-4867-be7a-bef0f0f8e46b)(content(Whitespace\" \
         \"))))(Tile((id \
         58004a91-362b-4622-a9ab-22fba579d57b)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         485a0bb5-ac9c-48b7-865a-84f59ae1f770)(content(Whitespace\" \
         \"))))(Tile((id \
         15600cbc-a2e8-40de-8fff-6e10cea239bb)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5d74e269-4953-4ab8-92b8-1cc3ed4185bd)(content(Whitespace\" \
         \"))))(Tile((id \
         c8837f19-7740-407a-a5da-a6887606600b)(label(250))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         af280088-c585-4d6e-b011-f844d62b4b88)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         95930180-f767-4ee0-8367-3967d1f34714)(content(Whitespace\"\\n\"))))(Tile((id \
         b9676f52-0691-4643-85f3-48d485d5c606)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b19f414e-32d7-40d8-bc6c-1422ec4104eb)(content(Whitespace\" \
         \"))))(Tile((id \
         86248f10-15dc-49a4-9be4-cf2db7810d9d)(label(orchid))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9ac46490-5fde-4fc1-830d-6391735246e2)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         ce89f858-a3a2-46de-96b7-e8ed3e665d95)(content(Whitespace\" \
         \"))))(Tile((id \
         8129c1bd-4cf2-415d-9b03-a51618771936)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         cd05f3c5-7aa3-4213-8438-4684d3f72c19)(content(Whitespace\" \
         \")))))((Secondary((id \
         4c1d505d-29ec-466e-a27e-ec6d8626a93d)(content(Whitespace\" \
         \"))))(Tile((id \
         b95f193e-d5c5-4a78-a694-faac1c87dc04)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         95a331ce-be5c-4599-be52-fb333f6279d0)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         d4b2b0da-b8f4-4cfc-a55b-05b0215cd7d5)(content(Whitespace\" \
         \"))))(Tile((id \
         8e1dff39-e499-4a9b-9f46-2a4108f47240)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2bcbc843-4a95-42f1-8a7c-670f085baf39)(content(Whitespace\" \
         \"))))(Tile((id \
         182e2c0a-0742-4f1a-b701-089e5d6ea00a)(label(\"\\\"Orchid\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1d45c521-2197-4607-b605-8fea1c09cf00)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45b617f0-ecf3-4bfc-8789-1e689a033e26)(content(Whitespace\" \
         \"))))(Tile((id \
         7d63950e-b4be-4828-b512-18f5352f5ac5)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f429747f-c626-4166-b111-edaa05bb214e)(content(Whitespace\" \
         \"))))(Tile((id \
         404216ae-b25a-4c14-a7be-ec64013bdaa2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b894b0f5-71f9-4693-a203-40865c380f98)(content(Whitespace\" \
         \"))))(Tile((id \
         a22e1393-47c1-432f-90ce-efd89f3a47c5)(label(\"\\\"\\240\\159\\140\\184\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0639e282-1ed1-41b5-8419-9cbcd4e9feb4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e2cdc10b-c946-475a-90d7-15fcdbd5fac3)(content(Whitespace\" \
         \"))))(Tile((id \
         245b9517-0acf-4ee8-bbe5-55289097c769)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bd655fb7-cd38-4538-b5b3-d241626d7280)(content(Whitespace\" \
         \"))))(Tile((id \
         34e119d9-3dee-46d3-b8d8-cb94d36d810a)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9dd91053-9d92-4b59-9881-9e05e465eac4)(content(Whitespace\" \
         \"))))(Tile((id \
         823ffcf1-c70d-4a3f-ae89-3bb893abe0a8)(label(180))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         387336be-9889-484d-ae2f-b9b08950dc55)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         73aa7631-b87e-48df-9775-f2f5791c6764)(content(Whitespace\"\\n\"))))(Tile((id \
         09086ae5-e13b-4370-a620-3b996c4b3e34)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         47cb3d58-844a-48bb-a33c-ff3b52f5b265)(content(Whitespace\" \
         \"))))(Tile((id \
         ddbf8fd8-76d8-41a9-a60a-e64ae9503be2)(label(cactus))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         c8195293-0664-4494-8fcb-8c9d3f48f8ef)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         afc3a67d-30cd-4264-981b-5858a9a554eb)(content(Whitespace\" \
         \"))))(Tile((id \
         eb040478-90e5-445f-929f-7c89e069b76e)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         dcd5bbee-67bf-4253-b213-419e5f70299f)(content(Whitespace\" \
         \")))))((Secondary((id \
         222bc4f8-b23b-4f86-853b-deabbcb89aaf)(content(Whitespace\" \
         \"))))(Tile((id \
         3a738636-3c96-408b-8b0a-dbf9bc041d9c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         8e554a55-8ee5-4bd0-8b39-42237e0b38e4)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e3dd4d7b-36a0-473c-9bcb-9b7fde9f092e)(content(Whitespace\" \
         \"))))(Tile((id \
         d00b397e-ce11-446f-940c-455a90cdc044)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d85433da-1b2d-40d1-9abb-335ec0a3df34)(content(Whitespace\" \
         \"))))(Tile((id \
         fd8d506a-8d3e-445c-9266-c28029746bbe)(label(\"\\\"Cactus\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         393074ee-2f2d-42b6-9bd9-bbbf72f57083)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6410709b-651e-4a8b-abb2-7c427bd3d61e)(content(Whitespace\" \
         \"))))(Tile((id \
         52c79d74-c0f4-4eb7-bed8-14b9d3001493)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         efd1e4bf-2d80-42fa-92cc-12e78ee448dc)(content(Whitespace\" \
         \"))))(Tile((id \
         35ad0777-2c38-4e15-87a1-bf11f08c1603)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a96d7508-62c8-4f0f-80f3-54756fd2127f)(content(Whitespace\" \
         \"))))(Tile((id \
         ceb3098b-619f-4443-9363-d7ec242ee223)(label(\"\\\"\\240\\159\\141\\132\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d9de47e5-ad65-422a-98e4-1d8c39b5de41)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         153e1983-3243-4432-b01d-8d30712392a4)(content(Whitespace\" \
         \"))))(Tile((id \
         f7b0516e-5d31-4036-bc0f-d7327c2f4cb8)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a359467-2b73-4e62-91b4-21b2c4ba5bdf)(content(Whitespace\" \
         \"))))(Tile((id \
         b8bbd320-7bdc-4b30-9935-9a5239ef4116)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fb655757-6b3a-47d0-8d04-7c621bf99854)(content(Whitespace\" \
         \"))))(Tile((id \
         8d752d7c-30c4-44d2-8bf5-19ffd1bf66b5)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         bfbfa727-44cd-460c-9313-fb3136403910)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         dfab25ba-dbf6-4b4e-8185-490c324de51b)(content(Whitespace\"\\n\"))))(Secondary((id \
         668d3255-ed6d-43f9-85a7-213cb2ee12c6)(content(Whitespace\"\\n\"))))(Tile((id \
         86e5be14-6ecf-48c9-ac31-3d0ad73a6ccf)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a039cb8e-e6d1-4631-95e4-8e103d3b18d7)(content(Whitespace\" \
         \"))))(Tile((id \
         858375c9-9f9a-4a94-b4d1-4fdd9357086f)(label(bed_labels))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         13845439-8ae1-4311-be56-4772a573eca9)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         37bc7bb1-5b5f-4297-8d66-3791547eb48a)(content(Whitespace\" \
         \"))))(Tile((id 0c73a467-b47f-42e5-bbd8-58f4f070e59e)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         8f408cfe-4444-440c-bc19-97764bc1b1ae)(label(Plant))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         c2f87e1b-d261-4636-b01e-bee2572d39a9)(content(Whitespace\" \
         \"))))(Tile((id \
         4ce4f2ac-f250-44cc-894d-9774fcbaec55)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         3aeb619b-4c61-402b-ab7c-48a669cdd7d0)(content(Whitespace\" \
         \"))))(Tile((id bed666d3-6490-4e75-b615-7e005bd9ac73)(label([ \
         ]))(mold((out Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape \
         Convex)(sort Typ))))))(shards(0 1))(children(((Tile((id \
         5d966ee0-3b6b-4502-a615-644814af163f)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         e6c76398-5def-46b7-89c4-2e8aaf8700cb)(content(Whitespace\" \
         \")))))((Secondary((id \
         7e16856e-4937-4d02-ae64-eace8796d838)(content(Whitespace\"\\n\"))))(Tile((id \
         9d0c4c0c-3d93-4d43-985e-964e8a43278a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8e396c4e-358f-4563-95ae-5e7b71df5a2f)(content(Whitespace\" \
         \"))))(Tile((id \
         94bce6e8-2e9c-4884-802f-424e40aacba7)(label(bed))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8c9a8ec6-af6a-4093-834b-eec48fe2d4d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d3d59fcf-b266-4e62-be14-87192bdfcb24)(content(Whitespace\"\\n\"))))(Tile((id \
         3c440543-ccaa-422e-bfdf-f76319cb9255)(label(map))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3c855e34-df1a-48ef-88b8-3e0ead172c5b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         2797a699-d4cf-4f0f-b9a1-dbf0a2381dec)(label(bed))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         90461a78-30e9-4c9e-b12a-5162ace79665)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         eb649566-335b-4727-bbc5-304e7713903e)(content(Whitespace\" \
         \"))))(Tile((id 9f54344a-55a0-4883-ab47-cd163e850f5e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f8908e54-bc31-4d3f-8164-95ca3d47f8d9)(content(Whitespace\" \
         \"))))(Tile((id \
         948e9ed1-fbbe-42f2-ad6d-6a40ea161e4c)(label(plant))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         629cb1b1-db78-4959-ab53-e65a728d5a02)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         345b848a-7e4b-4f2e-b2df-4ee34113d054)(content(Whitespace\"\\n\"))))(Tile((id \
         a638dd46-cf80-4cb2-b1cc-433eb34854a2)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07b77f23-1a68-405f-8e66-a03eaeefcb69)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         143b0e52-8e38-4020-b0bd-25d8c27ad3da)(label(icon))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ad0bdbbb-6233-43f9-bad2-87e57778d7d8)(content(Whitespace\" \
         \"))))(Tile((id \
         0d302890-c0a4-44d1-b78b-df627b18df01)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5dff1832-64ec-44ef-98be-50c947c3828c)(content(Whitespace\" \
         \"))))(Tile((id 12fb5970-4dcf-4a2d-8bc2-d2a671038c48)(label(\"\\\" \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
         620b076a-0601-46b1-8922-a6c7eca6684d)(content(Whitespace\" \
         \"))))(Tile((id \
         45cd16aa-14df-452b-988e-7c03846245e2)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b23948bd-ce81-47d8-9b39-ccc79d410eae)(content(Whitespace\" \
         \"))))(Tile((id \
         d3205812-ce59-40d0-9b4e-c45a945c5e68)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7be18b20-4e12-45e0-a507-cab50cb2cfb5)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         1fedf8d4-6a33-4181-9d9c-c5fa217aeb31)(label(name))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         035e4dc9-4e27-43d2-9279-ad781b569d0d)(content(Whitespace\"\\n\"))))(Tile((id \
         087221a9-59f7-4caa-9b84-84178ce5ae08)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         00a5f3b6-86a4-4946-9beb-bcf18d6dc19c)(content(Whitespace\" \
         \"))))(Tile((id ff5ac65b-8837-4dff-bd5e-fc82496cb9ca)(label(\"\\\": \
         \\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0))(children())))(Secondary((id \
         25572a83-5124-45f6-a86a-c67f1ddd1b1e)(content(Whitespace\" \
         \"))))(Tile((id \
         c3341da9-0502-47cc-b80e-f03b48fc0ff8)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d580e1d3-710c-48d0-b7bf-8680a7eec05e)(content(Whitespace\" \
         \"))))(Tile((id \
         db42be5e-2819-4c24-9b15-ee9adc6270c7)(label(string_of_int))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cc26b10c-8cb9-4bbf-971d-3ab028f1f0dd)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         89b6947d-8b6a-4d0d-8694-ba29414a6ec2)(label(plant))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         39109f6f-f3cf-4a1e-9f4c-f7a37460f500)(label(.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 22))(sort Exp))((shape(Concave \
         22))(sort Exp))))))(shards(0))(children())))(Tile((id \
         93d0ea3b-6025-4593-a527-41061fd87ed4)(label(water))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         28f29035-06d3-485e-ab91-40da4f972ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         01f00ae0-a4a1-43a2-a5f8-0ed1baa72919)(label(++))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 30))(sort Exp))((shape(Concave \
         30))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1c805e5c-575e-4598-8697-69b558c7e1f0)(content(Whitespace\" \
         \"))))(Tile((id \
         bbf1c913-b25e-4d53-aced-0cf64218bad6)(label(\"\\\"ml\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5661ace0-188b-4b51-9480-8d8359250e36)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         90e0ac06-5552-4c8d-a131-ddcc37f46ba1)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         d37b56a9-6cac-4ded-978c-a5afd7f6ce0b)(content(Whitespace\"\\n\"))))(Secondary((id \
         15a9840e-fa5b-4841-9a69-d3ac751332b1)(content(Whitespace\"\\n\"))))(Tile((id \
         d5a799e0-f260-4ed1-b64c-731d3000c4c5)(label(bed_labels))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa38ce41-1764-405f-b2ab-8b6061f836eb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4c40d1dd-327b-4bd1-bd70-eccb2734777f)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         1d4e088a-b64a-4fbb-9178-69a0d412e7af)(label(fern))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e047a8fc-537f-4406-983d-a62eb8f0a96d)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         90cdff73-68bb-4568-ad72-c31dfe7bc540)(content(Whitespace\" \
         \"))))(Tile((id \
         dac73ce3-d047-4f87-bcc0-12a3ab3fb39f)(label(orchid))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Tile((id \
         5e3c4e0b-e0da-42f5-89ef-4644817c4a9f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ab5e21c8-f599-4d92-a2cc-a81551b0c7da)(content(Whitespace\"\\n\"))))(Tile((id \
         1e46cae6-59ce-4eea-a307-5017c68bcab9)(label(bed_labels))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         82ea7508-e916-4a02-8782-4ed90a251972)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         3966723f-bd48-41fc-9983-1382cd9acf4c)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         4320ccf5-dd70-4097-a12a-920843df229d)(label(cactus))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))))))))(Secondary((id \
         ae0aaa54-0a2d-4f9b-b066-ddc75ba285f6)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce4dbe21-c4c5-4964-92bc-b2d1bf3dfab6)(content(Whitespace\"\\n\"))))(Secondary((id \
         5bc00b1b-8b8f-46a9-8649-17df73b48e84)(content(Comment\"# \
         =============================================================== \
         #\"))))(Secondary((id \
         3743e22b-72ec-458a-b7ba-f108599d8e4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b96d3428-ea48-4680-a262-fb10487559b6)(content(Comment\"# You don't \
         need to memorize the color categories. The sidebar   \
         #\"))))(Secondary((id \
         f2b9dba3-1ce3-4b86-85b7-253e7efc1e30)(content(Whitespace\"\\n\"))))(Secondary((id \
         f045cd63-83b8-4e34-9551-77d34a4d26a9)(content(Comment\"# legend is \
         always available. As you work through the study        \
         #\"))))(Secondary((id \
         129b1b86-40db-43f7-a63d-c1aba184e01f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c2f18e7-01a7-4764-bd61-f904ef2df07d)(content(Comment\"# tasks, the \
         colors help you see at a glance which samples        \
         #\"))))(Secondary((id \
         55804bec-1702-41c6-9a76-ccdd501a0bf3)(content(Whitespace\"\\n\"))))(Secondary((id \
         e5029677-b166-4a10-a9a1-1d35dbc10318)(content(Comment\"# are \
         connected to whatever you're currently focused on.           \
         #\"))))(Secondary((id \
         b674ef90-fea3-4e39-86b4-86ff800108d2)(content(Whitespace\"\\n\"))))(Secondary((id \
         159006eb-932b-4023-861b-dd7bb27b4c74)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ad24029-809d-4c06-bfe0-c92f42e4f113)(content(Comment\"# END OF PART \
         10 - Select the next slide from the top menu        \
         #\"))))(Secondary((id \
         e0dead78-e934-4eb9-afe0-b49fb799af22)(content(Whitespace\"\\n\")))))";
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
        "((82ea7508-e916-4a02-8782-4ed90a251972((kind \
         Probe)(model\"()\")))(aa38ce41-1764-405f-b2ab-8b6061f836eb((kind \
         Probe)(model\"()\")))(3c855e34-df1a-48ef-88b8-3e0ead172c5b((kind \
         Probe)(model\"()\")))(948e9ed1-fbbe-42f2-ad6d-6a40ea161e4c((kind \
         Probe)(model\"()\")))(94bce6e8-2e9c-4884-802f-424e40aacba7((kind \
         Probe)(model\"()\")))(d5e1f2da-e1f9-4fb9-a99c-b1d1aa9e02b4((kind \
         Probe)(model\"()\")))(e3c50f70-2faa-4d37-8471-9ab667f9553c((kind \
         Probe)(model\"()\")))(407697d1-b637-4553-b4be-6db2ff04f0aa((kind \
         Probe)(model\"()\")))(c709e3c5-4c12-44cf-a305-1e7e972763d0((kind \
         Probe)(model\"()\")))(4964a68e-56ca-481a-abfd-37ea00c111ac((kind \
         Probe)(model\"()\")))(189d49ee-4766-4291-9b34-6d001da763c7((kind \
         Probe)(model\"()\")))(34c2a9c4-5ac7-4c75-8bf4-4a3047435cad((kind \
         Probe)(model\"()\")))(b0e611d2-30b4-45ce-ad5f-119a9a7a5162((kind \
         Probe)(model\"()\"))))";
    } )
