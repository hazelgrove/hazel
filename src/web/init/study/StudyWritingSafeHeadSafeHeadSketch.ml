let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / safe-head / safe-head-sketch",
    {
      segment =
        "((Secondary((id \
         2d00a44a-ca71-4056-b874-db1e1cd2c5f9)(content(Comment\"# SAFE HEAD \
         TASK                               #\"))))(Secondary((id \
         f6fa503e-05c8-4b63-9765-d0fcbe0c84d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         1b796488-aad3-4e05-9c0e-d40e4a194d4b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         bb0c9fde-3bb9-466b-90ca-481dd049c3bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         1775e90c-2270-4ac9-919f-44f30844b23b)(content(Comment\"# Implement \
         safe_head: get the first element   #\"))))(Secondary((id \
         ab682540-4420-4ada-bee6-940954a70dc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b1e420f-59b1-4d27-a16e-e54328f92e16)(content(Comment\"# of a list, \
         or return a default if empty.     #\"))))(Secondary((id \
         dc8c701b-2299-4ec5-841d-6742594d8f7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         4f4c138a-2cd1-42b9-82ec-6b1d7f08ec2f)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         e453b425-f3e3-417d-9d67-4d69d66b95de)(content(Whitespace\"\\n\"))))(Secondary((id \
         5fd1d9e1-b4d6-433d-9bcb-c3a441d2dccb)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         99ca4b67-5ff7-4e3f-93e3-ad44dc6dfa69)(content(Whitespace\"\\n\"))))(Secondary((id \
         10d68a02-332e-47be-afb2-0e595be153cd)(content(Comment\"#   \
         safe_head([1, 2, 3], 0) == 1               #\"))))(Secondary((id \
         4ab8b24e-8c78-4d31-9e20-4fc8c37f1386)(content(Whitespace\"\\n\"))))(Secondary((id \
         20caca89-32ad-4040-af16-f7bfff0849fe)(content(Comment\"#   \
         safe_head([], 99) == 99                    #\"))))(Secondary((id \
         1be6a319-739e-4380-9d90-8ab1f94cb6ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         9169778a-a691-48ac-8d79-c696d2536b97)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         74185f13-b214-4310-ac45-044f7b88c963)(content(Whitespace\"\\n\"))))(Secondary((id \
         3260437c-7fc7-438c-8dc5-ad2f51ac5a49)(content(Comment\"# Available \
         syntax:                            #\"))))(Secondary((id \
         8c13e904-028d-436b-ba2f-b4cc8f5d07f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         ff934631-724a-4c02-8b5c-6813a0b6ecde)(content(Comment\"#   case \
         expr                                  #\"))))(Secondary((id \
         bed2d814-7ea5-432f-b4f6-fc071402882d)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3cc71d8-d7e9-41fb-a1e2-ddc9ab80b601)(content(Comment\"#   | pattern1 \
         => result1                      #\"))))(Secondary((id \
         e143c986-590b-485e-a313-f2a691bf7d31)(content(Whitespace\"\\n\"))))(Secondary((id \
         152bd377-410b-440f-9a73-8c6ba28ec355)(content(Comment\"#   | pattern2 \
         => result2                      #\"))))(Secondary((id \
         512a1404-845b-49c6-9df0-0b55160aab27)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2adade0-3794-47a5-a8b1-469d7dc1355c)(content(Comment\"#   \
         end                                        #\"))))(Secondary((id \
         54e79135-73d5-47bd-97a4-cff58d0b9a74)(content(Whitespace\"\\n\"))))(Secondary((id \
         76dfb053-88d0-413d-9cff-b229a00dff87)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         9dac830b-705a-44ae-8cae-beca5f73d4c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         74e4e091-516d-47d2-bf59-068e61f1c161)(content(Comment\"#   List \
         patterns: [], x::xs, [a, b, c]        #\"))))(Secondary((id \
         7829d6ec-159b-40af-abe8-23e65767c232)(content(Whitespace\"\\n\"))))(Secondary((id \
         76f6786a-fab2-4895-8279-611fc42f0207)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         98cb2ff2-21a5-4132-a0c9-6376e4c16395)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd652b44-bd2b-4c42-8af9-e433718dc5f5)(content(Comment\"# Tip: Turn on \
         auto-probe to see which branch  #\"))))(Secondary((id \
         ef0912cb-93fe-4daa-b9a7-81869afba032)(content(Whitespace\"\\n\"))))(Secondary((id \
         e55dabb0-ca45-4e44-867e-ea046c260339)(content(Comment\"# is taken for \
         each test case.                 #\"))))(Secondary((id \
         1fbed408-d575-4113-aaf0-43e56cf953ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         d674b5cc-0de9-4815-a197-2b4b797dea7b)(content(Whitespace\"\\n\"))))(Tile((id \
         a8fb47b6-5b57-4138-84b1-5579c3176fa9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         31d55101-94af-43c5-9632-54327ba508fe)(content(Whitespace\" \
         \"))))(Tile((id \
         4b3f99d9-a826-4221-813c-cf44f2346bc7)(label(safe_head))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         7e723cf1-46bb-4403-b706-78d694e128d0)(content(Whitespace\" \
         \")))))((Secondary((id \
         7c14cecf-35fa-4e4b-9627-f05227efdefc)(content(Whitespace\" \
         \"))))(Tile((id 2278c98e-5acd-4229-a7e2-1d6f6abd6a99)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         d1f4e1d1-ca0e-4509-b184-399242441129)(content(Whitespace\" \
         \"))))(Tile((id \
         7ce24735-7931-4b63-9ac9-7d0de8de3473)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         3c4a3526-5f45-44c4-bc78-e808fbdb5ffc)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         b1420659-4d3e-4564-8ffb-f066d685a990)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e312c05f-d4af-4a50-8878-cfabee9d168a)(content(Whitespace\" \
         \"))))(Tile((id \
         1d1ba5ad-306e-424a-ade6-ce7ac9c560d3)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         c049d7f8-4c89-42d0-9826-6799e4dacc93)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f7302cd4-b6a2-451f-b867-7b1498364468)(content(Whitespace\"\\n\"))))(Tile((id \
         93a33c20-777a-47a6-b240-36ff1318cf95)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a884cee1-7459-47bc-b3e2-ebac62bca284)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         2c039adf-cb25-49ec-bdfb-ad9590819a08)(content(Whitespace\"\\n\"))))(Secondary((id \
         bffa27ce-c4bd-4d21-97d0-beaca1ec4d54)(content(Whitespace\"\\n\"))))(Tile((id \
         8afccb2f-6723-4567-a3ea-cccd663f338b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         063c6862-edf2-4595-b0dc-fcf44147a749)(content(Whitespace\"\\n\"))))(Tile((id \
         ba17aaa0-672a-47a2-9e46-2cfebf0167c2)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3652ec6d-f820-4539-8b35-9eda1ef05c91)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         992972b1-582f-4ba9-b0e2-8d03c7687c25)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         0b40b4fb-1be6-4807-95c5-e2c222511a5d)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e18ea71a-3388-4a88-99b0-45f25ba632e6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8aea916-2ebd-4039-9bbc-5d6d30a0b386)(content(Whitespace\" \
         \"))))(Tile((id \
         c60a33dc-ba4d-4b2e-9ff4-dd140fea5f04)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         241ab10e-604f-42b2-ad05-c7e3236d2e87)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0ecd6412-1d8b-4986-8df1-f5d1ec39e3b2)(content(Whitespace\" \
         \"))))(Tile((id \
         4fe90e99-a0d2-45d0-b8cb-4ce8f3b78c0e)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ba75d3ac-40c9-4916-b903-df7b7b852231)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c8239c05-d721-4db1-84b2-1211c75a20f3)(content(Whitespace\" \
         \"))))(Tile((id \
         83180354-24c8-4770-9f4a-8ae360e05d70)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         14a8f14d-72b6-46d9-8039-1532f855a404)(content(Whitespace\"\\n\"))))(Tile((id \
         0467be41-35bc-47e9-ad33-6331171ad1ba)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8dd9f184-8287-4f24-8e2f-c463587adfcd)(content(Whitespace\" \
         \"))))(Tile((id \
         ec8c5744-e1fe-4507-a922-48e7aec90144)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8d4b6b53-5c28-48eb-8329-cfc865ceef84)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e29df4d1-1ce4-49ca-b952-4890d2097ae7)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d473869-fa85-4ae2-8c01-230a513894da)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ece84ef-0780-4d80-a41b-0a8a826a92d3)(content(Whitespace\"\\n\"))))(Tile((id \
         dc4a1c51-5885-4912-9021-04bdb80f534c)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         a4e8e35b-519f-4251-99e5-f40389d9bfad)(content(Whitespace\"\\n\"))))(Tile((id \
         c10da912-c55c-4ddb-ba3c-22af39a3502e)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         aa8d6a09-03c6-4985-8e31-18e2f0e8dc8f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9c57d2b0-1b0f-4f31-b1ae-d345af0702c3)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         787884c1-2bc8-4241-a168-714845583cb8)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ed34ec11-0adf-48ef-8754-5cd29ead30cd)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e14bbafb-253f-4d11-ad0a-e3a9ae110c23)(content(Whitespace\" \
         \"))))(Tile((id \
         fab4a923-6dc2-4d9a-9a69-15bf1e5c2715)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         dfcc6b08-3e04-4259-aa12-9df4fb6a99ca)(content(Whitespace\"\\n\"))))(Tile((id \
         e1422148-6f78-4e1e-868c-13a972c28446)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1d8c8129-2400-45f6-b9fd-969ba9eb3ac8)(content(Whitespace\" \
         \"))))(Tile((id \
         7f5b5b8d-4b57-4db5-92f0-c90db9911106)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ff9774d3-288d-44ae-b9a8-53eb2ff502e7)(content(Whitespace\"\\n\")))))))))(Tile((id \
         c82f577d-fc0a-4e9f-8ef4-81c4caeabdab)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f8e9e877-7200-4c83-93b1-18742b630f3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         3a91ece9-eaa4-4911-8ca4-bf9d3e72fbec)(content(Whitespace\"\\n\"))))(Tile((id \
         8e7724b9-4e41-4cb9-97f8-881e5ea1dc69)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         f04edb8d-b08e-4134-ad05-8d1ba1055d98)(content(Whitespace\"\\n\"))))(Tile((id \
         17061632-ccee-4cb1-8535-cb372a1fedf4)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d905abb-d66c-4378-9eb7-d4ff71f7f08a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8c66d945-287b-4f58-9396-1257e4bf2cc6)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d393c393-6d09-476f-8933-f4a91baebef3)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8b6437b7-8c12-460a-bc09-d804e3192f50)(content(Whitespace\" \
         \"))))(Tile((id \
         0becbe77-3718-48a0-a6d2-05d2946082f3)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f528794e-320e-4cbf-a220-0d2356193d4a)(content(Whitespace\"\\n\"))))(Tile((id \
         de827ffb-7aa7-44c6-b9ca-582ad585c498)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f345515-3a02-4286-bdfe-15dcb129c789)(content(Whitespace\" \
         \"))))(Tile((id \
         57468e0b-8328-4851-9512-bb5c0ed7386c)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5945e673-344b-49ed-b7e6-d91e05cacdc2)(content(Whitespace\"\\n\")))))))))(Tile((id \
         3dc10fbb-8aea-41cf-9ef1-16071929cfdb)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b1bfaa00-e1d3-450c-b513-f60b5fffa8ee)(content(Whitespace\"\\n\"))))(Secondary((id \
         9359c826-fa2b-41c8-b4b9-5bd6e12c1181)(content(Whitespace\"\\n\"))))(Tile((id \
         ae4fdf39-95cc-4153-adad-403c2e31706b)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6c9eac4c-5c68-4873-a7d8-1adfb0157bc6)(content(Whitespace\"\\n\"))))(Tile((id \
         01bb7724-4202-499d-bea7-460b9d965207)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5d468fe2-82c6-494b-a2d1-ae0ee20f20d7)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f2115046-4190-41e7-bd98-e8668be494cc)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         01c157c7-5e59-4cab-9452-ad8f0c808180)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         50c4df94-1b6b-4602-b74b-ef1745b9bf55)(content(Whitespace\" \
         \"))))(Tile((id \
         9b42adb6-9017-4740-a7b3-7117441d62b5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3d4d7d7b-5978-427a-b350-7d7494eac236)(content(Whitespace\"\\n\"))))(Tile((id \
         56a1c389-e9a9-45a5-b252-bae4473eb7ec)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         14b1887c-9f42-4eda-887c-2b74ed0f851e)(content(Whitespace\" \
         \"))))(Tile((id \
         ae200d38-42fe-41b9-89b5-099d492f3f0a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7c57770b-6944-457d-951b-10d0ee1b833e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6413cb15-b49c-4c4f-bc6b-452d77aaee20)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# SAFE HEAD TASK                               #\n\
         #                                              #\n\
         # Implement safe_head: get the first element   #\n\
         # of a list, or return a default if empty.     #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   safe_head([1, 2, 3], 0) == 1               #\n\
         #   safe_head([], 99) == 99                    #\n\
         #                                              #\n\
         # Available syntax:                            #\n\
         #   case expr                                  #\n\
         #   | pattern1 => result1                      #\n\
         #   | pattern2 => result2                      #\n\
         #   end                                        #\n\
         #                                              #\n\
         #   List patterns: [], x::xs, [a, b, c]        #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe to see which branch  #\n\
         # is taken for each test case.                 #\n\n\
         let safe_head = fun (xs, default) ->\n\
         ?\n\
         in\n\n\
         test\n\
         safe_head([1, 2, 3], 0)\n\
         == 1\n\
         end;\n\n\
         test\n\
         safe_head([42], 0)\n\
         == 42\n\
         end;\n\n\
         test\n\
         safe_head([], 99)\n\
         == 99\n\
         end;\n\n\
         test\n\
         safe_head([], 0)\n\
         == 0\n\
         end\n";
      refractors = "()";
    } )
