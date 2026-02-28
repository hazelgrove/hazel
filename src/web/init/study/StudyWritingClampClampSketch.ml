let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / clamp / clamp-sketch",
    {
      segment =
        "((Secondary((id \
         5dbd36d3-40ba-4337-a96b-192ee1983e2e)(content(Comment\"# CLAMP \
         TASK                                   #\"))))(Secondary((id \
         e5149350-2e8b-4477-b0ad-0822f4034742)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f2e0e42-1587-4308-b2c3-816397ac3ebc)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         be909bfe-11d8-481c-bbb8-4ae63540afd4)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d33dba7-95a6-4de8-a625-bc73890e359d)(content(Comment\"# Implement \
         clamp: constrain a number to be    #\"))))(Secondary((id \
         c2d6dc30-6efd-40c1-9be2-00daf22789a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a4b8f699-5928-4221-a98d-08f906a2d4c2)(content(Comment\"# within a \
         given range [lo, hi].               #\"))))(Secondary((id \
         c8decf96-ff1b-4cac-886a-0e492e49bf97)(content(Whitespace\"\\n\"))))(Secondary((id \
         7892bb15-494c-43f3-8740-909cb415b290)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         18679bf6-d049-4573-aeec-8975563a4c30)(content(Whitespace\"\\n\"))))(Secondary((id \
         87f64476-d7a8-4410-a0d0-d5ed74d1dd54)(content(Comment\"# If x < lo, \
         return lo                         #\"))))(Secondary((id \
         776435ec-da42-4630-8c1f-13ba306091b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f7e812f-1f0a-4088-b28c-1824fde0288b)(content(Comment\"# If x > hi, \
         return hi                         #\"))))(Secondary((id \
         384d554e-af49-4328-8f43-ccb2ec775720)(content(Whitespace\"\\n\"))))(Secondary((id \
         39c2d188-19f7-4db8-85e3-3de765a09c42)(content(Comment\"# Otherwise, \
         return x                          #\"))))(Secondary((id \
         3e915c79-fdd4-4a3e-9673-2f8135b05913)(content(Whitespace\"\\n\"))))(Secondary((id \
         1849ff2d-cd0a-42a5-aee4-5e277dd14f29)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         f3c39297-d722-4acf-87d0-8d2454310b9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         d83b231c-adf0-44f6-acac-fe712e1febc3)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         9e5ddaa8-0453-4110-9d37-d052a4fd2334)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c7264cf-a006-48d6-8185-20a83544919b)(content(Comment\"#   clamp(5, \
         0, 10) == 5    (in range)         #\"))))(Secondary((id \
         2f5ca8b2-5c77-4a64-9fb1-f25ae494cad8)(content(Whitespace\"\\n\"))))(Secondary((id \
         f23fbcf3-0202-40dd-a6d7-7ec8a4b617d2)(content(Comment\"#   clamp(-3, \
         0, 10) == 0   (below min)        #\"))))(Secondary((id \
         1c8f8fec-6e19-41d6-b719-d7a181aafdd2)(content(Whitespace\"\\n\"))))(Secondary((id \
         5938d120-735a-40c4-9ab8-2f4efd678e44)(content(Comment\"#   clamp(15, \
         0, 10) == 10  (above max)        #\"))))(Secondary((id \
         b4761ca2-56d0-4b7c-b5a1-60ccc54e8417)(content(Whitespace\"\\n\"))))(Secondary((id \
         b86886bf-4cfd-4cea-8533-ca913d4e2b54)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         5aa7d414-bf5b-49e3-b88f-a549b86afdf9)(content(Whitespace\"\\n\"))))(Secondary((id \
         5f9c984e-30e8-4341-9ef7-4bba7aaeb41f)(content(Comment\"# Syntax \
         reminder:                             #\"))))(Secondary((id \
         881aabda-6f59-4469-a3e7-75f6aa50aea9)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b62d87b-db99-4db8-bc9a-97e23177df74)(content(Comment\"#   if cond \
         then expr1 else expr2              #\"))))(Secondary((id \
         63a28a79-79af-41fb-a712-3ab7bb9ad41b)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6364c97-0289-4be2-b83b-f3c894d0eda7)(content(Comment\"#   \
         Comparisons: <, >, <=, >=, ==              #\"))))(Secondary((id \
         e3e53d05-d15f-463e-abc2-721792708f08)(content(Whitespace\"\\n\"))))(Secondary((id \
         588e141c-9ebe-41ce-9297-b277d66c8728)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         7f519739-594f-409c-884a-7a5ebdd98bf2)(content(Whitespace\"\\n\"))))(Secondary((id \
         3bed59ee-6323-44b7-b7d0-20ff13bbaa8e)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         1e11291e-3275-42b7-a049-21c7f2217f80)(content(Whitespace\"\\n\"))))(Secondary((id \
         6e04bc52-d9c8-4404-affd-4b752c167380)(content(Comment\"# to see which \
         branch is taken for each test.  #\"))))(Secondary((id \
         917e60de-7cd9-4ee2-a5e5-bb19b37142b5)(content(Whitespace\"\\n\"))))(Secondary((id \
         0e797c0a-914f-4e46-9ccf-569eb69e6a5e)(content(Whitespace\"\\n\"))))(Tile((id \
         d1856149-4965-4a5d-b81f-e5f320f25599)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2514883-a81f-4e15-ba90-ccc181e7f333)(content(Whitespace\" \
         \"))))(Tile((id \
         b11bd586-c3cc-4599-a1c5-48c5b61d0ec4)(label(clamp))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f0a6df68-e08c-479f-bc58-bc90f0590f65)(content(Whitespace\" \
         \")))))((Secondary((id \
         543d0109-f565-41e1-af49-e3c5ec926025)(content(Whitespace\" \
         \"))))(Tile((id 9df66f3c-e10b-4e6c-9418-8d46f15e7b3e)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         0dffbb59-56a6-4bf2-93e6-8be44d1f5bab)(content(Whitespace\" \
         \"))))(Tile((id \
         48fb49a2-16f8-450d-9427-637ecc17d185)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         39694d72-cc6c-45b5-94bf-4fc3cdf45e57)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         bdacc9c9-8e48-43b7-9a9c-25e6686039e1)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         5ff4997f-9d1a-4810-9b01-148141e180eb)(content(Whitespace\" \
         \"))))(Tile((id \
         a23dabee-da4e-4d91-aba1-8b761e6659ab)(label(lo))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         aad97651-eaf3-46f8-aa50-45d1aa89bc8a)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         348d36c3-db3a-4acc-bc61-077231cc10f6)(content(Whitespace\" \
         \"))))(Tile((id \
         8db18ad9-254f-4ca2-9da0-0ff3f7eb13e6)(label(hi))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         00d87cbc-4a9e-4f64-ab1e-67f978d8c50b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         62fb9fba-44c5-4dfd-863e-e19dd91175c1)(content(Whitespace\"\\n\"))))(Tile((id \
         7a45f4f0-0df8-4470-985c-d63e17cec6e7)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a9c404fd-8bb9-49c5-bf16-b2fb7e70cae9)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         724410c0-81e3-4792-ae34-53b9774d0210)(content(Whitespace\"\\n\"))))(Secondary((id \
         a35bfc17-188e-4b8f-996b-18286ced7521)(content(Whitespace\"\\n\"))))(Tile((id \
         04ae6d98-95d5-4026-8827-badd028cf44e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ce2a8271-7437-409a-915f-00acacfb20fd)(content(Whitespace\"\\n\"))))(Tile((id \
         3f55f4e3-e865-4bce-aff3-fbfad2c29d4f)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         cdc08b9b-600f-4469-a0ac-512832152e9b)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a371e623-7f81-4119-9665-160c93cf8851)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         91d46873-caed-41a4-aeae-2717deea931f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         fc197df8-6511-4979-b2f4-7d17674c8095)(content(Whitespace\" \
         \"))))(Tile((id \
         7d498362-1af1-47be-8bba-670df0afd45a)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78d376b3-1853-4569-a4f8-00f9eb8ee866)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         da2fa502-3f7f-4366-b185-e329b177471b)(content(Whitespace\" \
         \"))))(Tile((id \
         b8fe9057-5f24-4a7d-b453-ca0e6db394c8)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c2a40eaf-d82c-4a75-b81c-9bef5f2124dd)(content(Whitespace\"\\n\"))))(Tile((id \
         178088e5-86e5-45bb-beb4-c151c312f1eb)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2062d09b-8192-4280-9eed-aefd0c73217b)(content(Whitespace\" \
         \"))))(Tile((id \
         1558cb3f-4ae6-43de-95ed-449915c46fd5)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         18bd3da7-db4e-4191-b392-85d1f5e5bcbd)(content(Whitespace\"\\n\")))))))))(Tile((id \
         69cd4cdb-7180-46a1-9010-b63448272418)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6e470480-1afa-4293-8400-a69e8b94a3d8)(content(Whitespace\"\\n\"))))(Secondary((id \
         3eea91af-c804-4650-b4a2-714e6caae326)(content(Whitespace\"\\n\"))))(Tile((id \
         74f5cfb2-ccc3-4756-8001-b041e80ba8e6)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e5818fb9-79bb-4541-9180-d90ced084ef2)(content(Whitespace\"\\n\"))))(Tile((id \
         d0a3fef6-9c9a-4ac1-b1a7-5f3f71083ccf)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         21373435-369d-413c-914d-56df39906521)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d666a018-032d-4924-a425-cf6dd34e4c52)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape(Concave 25))(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         07ba3e9d-3c94-41d7-8c29-869f342a3f1d)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b59795bf-00db-4d0d-a257-581f0c18e5f4)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c93e85af-a2d1-4804-9142-eb61d4594dd1)(content(Whitespace\" \
         \"))))(Tile((id \
         db96cfba-e09f-4a8c-9422-d2033b884592)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45b467eb-520d-4a3d-858d-c0993ad2ac38)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a4148a8-bdba-490e-86fe-2a08fe684c14)(content(Whitespace\" \
         \"))))(Tile((id \
         f99d7750-4dc3-4101-b865-b884687bf2e0)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c3e49cfc-79a9-4ec0-b14a-94cdc70477c1)(content(Whitespace\"\\n\"))))(Tile((id \
         a44dc71d-40d4-451f-8deb-fcbd6164f940)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2a7534ee-ef1d-49a5-b0d4-e3bc5dc463f5)(content(Whitespace\" \
         \"))))(Tile((id \
         a8954238-e3c6-4ded-aa7e-308ad0d182eb)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aa61349a-9a35-4c27-904e-9697230d5808)(content(Whitespace\"\\n\")))))))))(Tile((id \
         70a04b1a-b247-46bd-b0f0-a18fbb738fd0)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f386b374-8fe1-4084-aefb-a2db115ee922)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab8a1285-7a35-4dad-850f-1f6e687e12de)(content(Whitespace\"\\n\"))))(Tile((id \
         c5fc2ec0-b609-4f0a-8223-5351f7a961c0)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         59f56217-6f0a-46b9-905b-8f025ccf44fd)(content(Whitespace\"\\n\"))))(Tile((id \
         cd1633a8-209b-4ce4-b8b4-382f9c792cf5)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ad4ea51d-8b78-41e3-a6c1-eeeaf5680e13)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         5b36201f-38e1-496d-8e2a-4489f67ed439)(label(15))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6e48ada5-1627-4e16-816a-6b647ede8827)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         be17b0a4-70c2-4b1d-9b74-6ed2b609ba6b)(content(Whitespace\" \
         \"))))(Tile((id \
         7e369999-914d-42f7-947a-6aeeea064a98)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5462a8a9-025f-48c2-b081-98c864ce675b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f4174be-9278-49bb-97e4-65fd7d432a90)(content(Whitespace\" \
         \"))))(Tile((id \
         8742bc17-0657-422b-9262-1800a6f5aab1)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         f8ebaaed-20a6-4b91-8a45-8a4888e678ef)(content(Whitespace\"\\n\"))))(Tile((id \
         47e450b2-3450-4627-aea9-16c85605cf6c)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef058dcc-c554-47c6-af4d-b19b6638d843)(content(Whitespace\" \
         \"))))(Tile((id \
         ea5b6266-8707-4abf-af8e-60ddd880bb9e)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         aee47d20-2183-4a90-96a2-4057d7fcb40a)(content(Whitespace\"\\n\")))))))))(Tile((id \
         064777fd-6e3c-4e4f-a878-e9a5745f0114)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         341f9473-47d9-4553-8b91-7a6a3f90debb)(content(Whitespace\"\\n\"))))(Secondary((id \
         329a3f27-eedb-4dc7-a697-2b8257a1999b)(content(Whitespace\"\\n\"))))(Tile((id \
         95bd6bec-775f-4819-8380-4bd87f417609)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7056735b-2c8b-4396-8ad1-27402e941a80)(content(Whitespace\"\\n\"))))(Tile((id \
         24f8d6fe-6e97-4c24-9406-e1d4526bad76)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         6cba7f43-84f3-409c-8020-0ce281eca574)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f9e609bf-0640-4fc0-ace9-18371a438819)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         303f1c77-0f30-46fb-9465-d9b17cda538e)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2c1c81a0-3a9d-4788-822d-813824233704)(content(Whitespace\" \
         \"))))(Tile((id \
         47564244-6b7a-43f7-8787-819449dee2b7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         e21a0698-a672-4604-a3c2-66017131a2f2)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         2f24c79a-04ec-4193-95e1-c5b1239994a6)(content(Whitespace\" \
         \"))))(Tile((id \
         e3221f03-c8ff-4150-9b46-395526f616a4)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         522028c5-366b-4fd0-ae2a-9cb45f0529d6)(content(Whitespace\"\\n\"))))(Tile((id \
         ebecff67-d5cf-4fdd-9ec8-e255d85d532f)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4bf087cb-d761-4dc6-ae40-1decd4eac022)(content(Whitespace\" \
         \"))))(Tile((id \
         75244925-86fc-4965-9570-ea4127a9b9dc)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dcfeec2c-4c67-4a51-a916-e0d287e031c9)(content(Whitespace\"\\n\")))))))))(Tile((id \
         983f400c-1e01-4558-861b-38f76885a89d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         13ce8f31-9310-4caa-8d5e-0d4230756937)(content(Whitespace\"\\n\"))))(Secondary((id \
         297ecb56-fbed-4187-913f-239f032c5cca)(content(Whitespace\"\\n\"))))(Tile((id \
         bf48908e-4d30-4403-9a28-087d0f7dc40e)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8fad0598-09c0-4bbc-8b2f-622544b98ef5)(content(Whitespace\"\\n\"))))(Tile((id \
         65797f28-509b-45ae-85aa-c11e45dd5e28)(label(clamp))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         332d1f1b-9c17-47bb-be3f-9ce6e8806f0d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         af41c192-d9d4-4163-a480-c219f156e890)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a93701c4-c2c1-4715-b05b-7f25cae9e962)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f44f2d19-1d53-41fe-b64e-0863615c87c1)(content(Whitespace\" \
         \"))))(Tile((id \
         9b747fad-ff95-473d-a221-09dd4f3c7fd7)(label(0))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         044e949e-d7eb-4b36-8508-5bf93f202e43)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ea25a6a4-ac7a-45e8-9cdb-cc4351cbd4a5)(content(Whitespace\" \
         \"))))(Tile((id \
         cd561eec-7c55-4bfe-a5dd-4bd40d64f6e1)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         88b75057-82e7-4c41-b4c8-bad4f34faec3)(content(Whitespace\"\\n\"))))(Tile((id \
         31b10ebb-f453-4a6a-a30c-1b1f8f1acb4e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b62c7a5e-ceb4-4cce-8529-a6f5c598150c)(content(Whitespace\" \
         \"))))(Tile((id \
         4795640c-b982-42a6-98d8-cf4abbe1bf30)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f3b6bb6d-e4e8-4df3-b9b7-89c12fea1399)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         11cf4299-db1e-410c-8099-e4df6fb61a76)(content(Whitespace\"\\n\")))))";
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
