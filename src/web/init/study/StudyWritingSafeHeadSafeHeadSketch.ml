let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / safe-head / safe-head-sketch",
    {
      segment =
        "((Secondary((id \
         0e43e842-e98a-4964-a235-300b5fc487a7)(content(Comment\"# SAFE HEAD \
         TASK                               #\"))))(Secondary((id \
         43be971f-34ec-48f4-9886-27a69f93ecd1)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ea5122e-fa16-4171-a815-d89263bb5a31)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         256302e3-b383-4ccb-86bf-f62ddffbfa18)(content(Whitespace\"\\n\"))))(Secondary((id \
         790abd97-2c99-4ae7-b3dc-a35192e7a407)(content(Comment\"# Implement \
         safe_head: get the first element   #\"))))(Secondary((id \
         7f673de0-a909-476c-9cb1-1f405c7c140f)(content(Whitespace\"\\n\"))))(Secondary((id \
         86fe65c0-2489-4ca6-8675-ce9bfff27155)(content(Comment\"# of a list, \
         or return a default if empty.     #\"))))(Secondary((id \
         69190c47-35fb-451e-952c-eae98dcca85c)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa32a6b1-de6d-4fda-8a06-6cfffa87132d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         a31ab7f3-f598-4e43-958a-4cbfddd48b8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         c7d8d115-a221-40ff-89c4-8c3d78b30f6f)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         85928a88-dee8-40bd-9577-745a71c4665a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e6ad2ce-7fd5-482b-9973-4297b3d8093c)(content(Comment\"#   \
         safe_head([1, 2, 3], 0) == 1               #\"))))(Secondary((id \
         a79edca9-d269-4ac1-ace8-d95e261c476c)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce698909-17e4-46fd-8752-b061d5ecd481)(content(Comment\"#   \
         safe_head([], 99) == 99                    #\"))))(Secondary((id \
         b40c3877-5d79-477a-82dd-ffdeccf61c5d)(content(Whitespace\"\\n\"))))(Secondary((id \
         81288c12-b92f-4bf3-975f-7bf34b51858d)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         38bdc6d1-252c-4da9-b9ca-cf5af1e878d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         25501fb3-4632-4f3e-a50b-319a5999ad40)(content(Comment\"# Available \
         syntax:                            #\"))))(Secondary((id \
         79ce99eb-ec3d-481c-a7da-b285dae094fa)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6a589fd-2ee6-4d45-9812-84e5f0f3aba4)(content(Comment\"#   case \
         expr                                  #\"))))(Secondary((id \
         4fb15f87-02eb-4040-8fe7-b27c25c5b279)(content(Whitespace\"\\n\"))))(Secondary((id \
         331d22d7-18b3-4b13-9e02-c91bb6f4a717)(content(Comment\"#   | pattern1 \
         => result1                      #\"))))(Secondary((id \
         a5efef3f-e3c7-4d50-9753-b9b160e817d0)(content(Whitespace\"\\n\"))))(Secondary((id \
         10ad908e-d4c9-4bf1-bc37-1136c75eb194)(content(Comment\"#   | pattern2 \
         => result2                      #\"))))(Secondary((id \
         267b547e-00a4-4158-83be-d2e09b43369c)(content(Whitespace\"\\n\"))))(Secondary((id \
         821ea69b-9a2b-47bb-8833-dde54ccb9ba5)(content(Comment\"#   \
         end                                        #\"))))(Secondary((id \
         c340f369-8a40-47b8-bf2b-62725fc90325)(content(Whitespace\"\\n\"))))(Secondary((id \
         128bf26e-c76c-4c63-92ae-4fc39b6c6761)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         c0da5fc3-c58b-4b8e-b155-5f0cb10742ab)(content(Whitespace\"\\n\"))))(Secondary((id \
         857076d5-4a63-4978-a1b4-1b0c13844069)(content(Comment\"#   List \
         patterns: [], x::xs, [a, b, c]        #\"))))(Secondary((id \
         952ca24d-9617-42c5-9699-f944ebe74fab)(content(Whitespace\"\\n\"))))(Secondary((id \
         34231ff9-efe9-4981-be6c-1f9a6eb727fb)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         c4653176-d116-461c-af97-a824b8b5c4e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         813631fd-e990-4a47-9a6d-74fd1d0d017f)(content(Comment\"# Tip: Turn on \
         auto-probe to see which branch  #\"))))(Secondary((id \
         03b87c0c-9972-4848-ade9-c0bd76cef09d)(content(Whitespace\"\\n\"))))(Secondary((id \
         f18f4c7a-b631-444f-8751-b765d60fc062)(content(Comment\"# is taken for \
         each test case.                 #\"))))(Secondary((id \
         fd02185d-b081-4a1b-b295-89feea094e4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         2dc0a83b-2e42-472d-ad38-683b46ae0d0c)(content(Whitespace\"\\n\"))))(Tile((id \
         e1894021-ff31-4656-863b-cca2619e39c9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         93ea76fb-04a7-4b8b-a072-5bafecb1f603)(content(Whitespace\" \
         \"))))(Tile((id \
         1fa7fb4f-029d-494d-8ebe-190c91a9531d)(label(safe_head))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0787a356-e060-4bec-b000-700f845af8fb)(content(Whitespace\" \
         \")))))((Secondary((id \
         4e80733a-3182-4134-bac2-1ddb15c07861)(content(Whitespace\" \
         \"))))(Tile((id 865a4cd7-0331-4e6d-a97e-bc788288c799)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         fa0d2e7d-7f0f-4dac-beb8-c237ca7de633)(content(Whitespace\" \
         \"))))(Tile((id \
         39cefd6e-6a87-433b-9028-5f5b03c48e9d)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         4dfb1d6e-2ccb-4a92-a7fc-0ae878eeabd9)(label(xs))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         33de705b-9b38-427f-946b-32ca5a7e6459)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         e1d8999d-d77f-4bca-9180-5fd83aacdfac)(content(Whitespace\" \
         \"))))(Tile((id \
         3b5952da-04a5-4a19-8d83-47ee4440b246)(label(default))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         58cd6caa-ddb5-4603-8481-145ebab36e9c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         47e834f6-1b76-456d-9acf-2c25e13f353e)(content(Whitespace\"\\n\"))))(Tile((id \
         15f38303-766f-40ff-9356-7705eb307aaa)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f27336e8-67b9-4f13-889b-e258bbfdec4e)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         5da98329-da4f-47fc-841b-b5c2991d187b)(content(Whitespace\"\\n\"))))(Secondary((id \
         52277983-82a2-4fc6-ad05-82b271031c6a)(content(Whitespace\"\\n\"))))(Tile((id \
         12cb61a2-9d0a-4dba-bd06-d2852338af27)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c2546895-6e5e-42b7-9e2f-851f3291f1cc)(content(Whitespace\"\\n\"))))(Tile((id \
         3cbd1258-6fb6-4306-b0b0-e5c768d9d15d)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f630cb05-6ac4-4a2b-9624-1b6245dff012)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         23b916b2-0497-4a9a-ae9f-0174246012f4)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         37542277-2f84-4c74-bb72-6207f0e3882a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         da0a5767-2898-46eb-b98c-5dc27895a7f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7b25ec07-1368-4176-9549-39ed1acc635d)(content(Whitespace\" \
         \"))))(Tile((id \
         5f73b4d8-22c1-4e4b-95ec-04e216247a36)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         46f1c879-8d15-4797-b6f0-4c4f1bc17ae2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0c2c108f-0908-49df-bf33-474fa48fba08)(content(Whitespace\" \
         \"))))(Tile((id \
         25670ac6-0cc2-4193-b564-1651ac4f935d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         03d9c00a-7177-47da-95ff-4e86c4439542)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f654f0ae-c1f9-422d-8e2e-3f47044ab4cd)(content(Whitespace\" \
         \"))))(Tile((id \
         628a95f1-8399-4d62-8bbc-d291c91ec4c0)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         671c226c-269d-49ec-9233-3666f15a03e7)(content(Whitespace\"\\n\"))))(Tile((id \
         3d66a47d-5551-41f4-8635-e983c9e68911)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a1eba30c-8647-4b7a-be65-b86cfcee6198)(content(Whitespace\" \
         \"))))(Tile((id \
         f66dadd5-1098-4f5a-8d0a-7f4efe2ac3a8)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6acbcc5f-ed7b-49ee-8396-c7f65b09bed4)(content(Whitespace\"\\n\")))))))))(Tile((id \
         539bc898-f307-4d28-b67f-7a5389436652)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bf812d70-cc31-4a79-92f8-de5320ec8422)(content(Whitespace\"\\n\"))))(Secondary((id \
         162e0911-6b68-41a9-b868-4623fd509f47)(content(Whitespace\"\\n\"))))(Tile((id \
         e6d961b7-a057-4083-a04a-a5b910708ae1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         c94362d8-ff2a-4b14-aa90-2c2ebebf7cea)(content(Whitespace\"\\n\"))))(Tile((id \
         28eb4fb4-6ccc-4da6-8c14-dc0075e89132)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         3a2be20f-a892-4a48-879a-192ae35f4c6f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         1648b894-4b8e-46fc-a8ec-a43b3a9a1528)(label([ ]))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         10c30712-cf49-4e11-ae8b-a2308990b612)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         4bd4b269-4b53-4601-9abb-7fb42adc3d66)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c5020bf7-f79e-4569-9c92-3d6cccb8886d)(content(Whitespace\" \
         \"))))(Tile((id \
         1a1246d2-fad3-4bf6-a028-73ece1c8cdf5)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         20f8ad80-ba79-4502-9847-71aa4ab304b6)(content(Whitespace\"\\n\"))))(Tile((id \
         c7303d4f-6dd2-4126-8b42-f1bdb965c1bd)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         091caa84-e1ff-4cb4-b708-c71facbaea01)(content(Whitespace\" \
         \"))))(Tile((id \
         2f7c77bd-63a6-40d4-8efc-bf82b538e246)(label(42))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9fb55b73-1b6f-4d08-a0ba-e77cb2aac8f5)(content(Whitespace\"\\n\")))))))))(Tile((id \
         021d8329-a088-48ff-8d50-a98c78f20d73)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d1b5eec-22f2-4747-b612-c7864acb5b10)(content(Whitespace\"\\n\"))))(Secondary((id \
         85bd64a5-aeee-4bd5-896b-80c3708f903c)(content(Whitespace\"\\n\"))))(Tile((id \
         0d9ca8a3-0125-482f-ab8e-1ae936b9c2d6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         5ee0aafc-ae56-419c-80ef-a19b1ce3046b)(content(Whitespace\"\\n\"))))(Tile((id \
         97407a78-246a-4043-bb0a-10b9cdb3c63f)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         58c46823-523f-4974-9ab3-5a6c41a91982)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9556d196-2cb1-4dc3-8f59-5704931a4b24)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7b87c1cb-7f96-499e-bff5-e30f4f1a4485)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84e3e5cb-0d95-49f0-9228-0fd17a7dc508)(content(Whitespace\" \
         \"))))(Tile((id \
         61d3bcd7-d58b-4888-bc20-3435a73a1ba4)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4d798d02-e670-4b43-b65b-59ce416bbb42)(content(Whitespace\"\\n\"))))(Tile((id \
         8ebb0c9a-c559-4e7c-ad6a-da7b7aab543e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fbb0bab6-a78e-4d40-b11b-4042621ef49e)(content(Whitespace\" \
         \"))))(Tile((id \
         6385485b-41b6-4e8e-ad52-cf9e1b98cc5d)(label(99))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         775174c7-f7db-4ee8-b54d-55f8f0252f5c)(content(Whitespace\"\\n\")))))))))(Tile((id \
         38db91e9-d063-4803-b1a3-c20d91eb4aea)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2020c6b2-99a7-4290-a1b0-d6f4d7898af2)(content(Whitespace\"\\n\"))))(Secondary((id \
         88b67244-95b0-43dd-8567-0835b173b0e6)(content(Whitespace\"\\n\"))))(Tile((id \
         da274943-c87e-4233-9257-f612b9cfa733)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8228de8e-de9d-468d-872d-8d79da2cdeb2)(content(Whitespace\"\\n\"))))(Tile((id \
         1f3cc741-4ab6-486a-8619-b36cd1a6c4be)(label(safe_head))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9b844912-35e5-42e4-80dc-3ae7399fd83c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         afe5cb58-369c-454c-aa37-1b6e2bc3e282)(label([]))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78ae167c-2700-4a11-b925-0d204a47959f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         99a0da1b-d6ef-4a96-9160-23556492b2ab)(content(Whitespace\" \
         \"))))(Tile((id \
         ed419151-2b5e-48e0-9891-8d353d4b3b27)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         59bbaf7b-86cb-4a9e-bbe7-e31025fdfa6a)(content(Whitespace\"\\n\"))))(Tile((id \
         23b31d6c-128f-4bb3-9b95-fcc0d3bbbc6f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         84de6a33-f547-41bf-b3b7-c23c77e00ef3)(content(Whitespace\" \
         \"))))(Tile((id \
         3333f59a-f0f1-4535-a5f9-3b5713d9a11d)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         043af9ff-8607-46fd-a3fb-b9a33a76c549)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         f2aeb946-e36d-4761-a0a1-2d5035cf4be7)(content(Whitespace\"\\n\")))))";
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
