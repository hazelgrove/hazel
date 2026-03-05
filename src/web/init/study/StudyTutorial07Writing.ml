let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / tutorial / 07-writing",
    {
      segment =
        "((Secondary((id \
         35969b35-f1c8-4f7c-a417-0a6effadd380)(content(Comment\"# WRITING WITH \
         LIVE VALUES #\"))))(Secondary((id \
         7f1e9b52-f7fd-4edf-b7f2-1cb25f33c60a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8cfc14f7-356d-4214-a458-5c37e6607f31)(content(Whitespace\"\\n\"))))(Secondary((id \
         46595fea-0561-4485-a115-06779a45f92c)(content(Comment\"# In this \
         tutorial you'll write code step by step.           \
         #\"))))(Secondary((id \
         9f7f585b-3757-4770-96e7-78b54e7debd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         1babadd8-236c-4433-8c82-5aed667fa6f3)(content(Comment\"# Turn on Auto \
         Mode (microscope icon, top right).            #\"))))(Secondary((id \
         266fe828-1d71-4b9c-abfa-17e90a6021ed)(content(Whitespace\"\\n\"))))(Secondary((id \
         c073ccbc-4cab-4a9b-aff0-8f9c83fbbbdd)(content(Comment\"# Each stage \
         shows you what to type, then you type it        #\"))))(Secondary((id \
         10b7ee82-bfed-4ad9-9292-a185d1e531e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         31b9c11d-b2fb-4e5a-85b5-cec08495fa61)(content(Comment\"# in the \
         designated area and see probe values appear.        \
         #\"))))(Secondary((id \
         e610bcd1-453a-40ba-9a6f-bd8ec2d03fc7)(content(Whitespace\"\\n\"))))(Secondary((id \
         de64328b-f0cb-476d-9f99-83c2481b5738)(content(Whitespace\"\\n\"))))(Secondary((id \
         8083aea4-9fea-4411-a442-7d59e6ea46ef)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         c073a820-5374-4d2a-a9f4-a91e21e8e0a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         b0dc292a-a27a-4493-addf-9268df526be6)(content(Comment\"# STAGE 1: LET \
         BINDINGS AND STRINGS                          #\"))))(Secondary((id \
         e4106d42-2601-431e-8205-50323a5aabc4)(content(Whitespace\"\\n\"))))(Secondary((id \
         acdc6d64-59ee-4708-97cb-35c66231ca97)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         755a3aff-856e-4b02-a145-de09bce00e6e)(content(Whitespace\"\\n\"))))(Secondary((id \
         11104f72-db37-4718-9f5c-ccce2a3bcfce)(content(Whitespace\"\\n\"))))(Secondary((id \
         9316797e-4200-4664-a100-b68966a171cd)(content(Comment\"# `let x = \
         expr in body` binds a value. `++` concatenates    \
         #\"))))(Secondary((id \
         ea192b15-b79f-488c-b676-84144a45c3cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b1c3aa7-f058-4a50-b750-4c8b0acba936)(content(Comment\"# strings. \
         `string_of_int` converts a number to a string.    \
         #\"))))(Secondary((id \
         882e03e9-7f88-4f32-92fa-f356fcd17dee)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9a42f57-b714-4951-b356-e5ab9c9a1fd7)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         f21bfeed-0fc1-4f7f-ba12-534a708a3ed8)(content(Whitespace\"\\n\"))))(Secondary((id \
         793b94bb-af23-4ca8-b3b9-e56e5d61be1d)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         6a7e217d-9511-43d6-aa90-8d8cd53e06c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebab932c-3d6b-4d9d-99b3-492ab5b9f87b)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8ce2c7ee-2e94-40fd-91b5-9e062e916682)(content(Whitespace\"\\n\"))))(Secondary((id \
         db31a13e-8aee-4631-a712-caa5aa485243)(content(Comment\"#   let name = \
         \\\"Fern\\\" in                                      \
         #\"))))(Secondary((id \
         e8b038b0-98a1-43ba-8aec-d701c43f23c9)(content(Whitespace\"\\n\"))))(Secondary((id \
         f0a518f5-24b2-46b0-a888-263ebdbe82c5)(content(Comment\"#   let water \
         = 250 in                                        #\"))))(Secondary((id \
         ea50b433-0281-4ef3-b5a5-045712202bb9)(content(Whitespace\"\\n\"))))(Secondary((id \
         e94160ff-e396-4097-9419-9184dd24357e)(content(Comment\"#   let label \
         =                                               #\"))))(Secondary((id \
         d5b8caea-e237-408a-9bb9-d17d35f88852)(content(Whitespace\"\\n\"))))(Secondary((id \
         cf2f147a-2f5a-4499-a857-964405ad468b)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         138aabb6-a25c-41d0-ab20-692b5f4c68cc)(content(Whitespace\"\\n\"))))(Secondary((id \
         3587eceb-3c30-4100-8cb2-aa3af8babcbc)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         06e4dac6-af89-4a7f-ac48-22f43ce09cd6)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd4bc453-1632-4bdf-a8d0-8c1f27ce93eb)(content(Comment\"#   \
         label                                                     \
         #\"))))(Secondary((id \
         4866da24-94d2-4f0c-b038-cee2dc784c07)(content(Whitespace\"\\n\"))))(Secondary((id \
         5de39099-2f21-4acf-9a6c-a9019d5d5240)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         af9f0ebd-6c1d-4877-98dd-868b80d40543)(content(Whitespace\"\\n\"))))(Secondary((id \
         58ab023b-1d84-4b20-b92e-825ba835ff4a)(content(Comment\"# Then click \
         inside your let bindings. Auto-probe should      \
         #\"))))(Secondary((id \
         ef644253-a246-4a41-8c04-518b0e884c4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         82a5e9d4-c0e7-4c79-b3ef-a42d7a498431)(content(Comment\"# show \
         \\\"Fern\\\", 250, and \\\"Fern: 250ml\\\". Try changing the      \
         #\"))))(Secondary((id \
         3f2dccfa-251e-460d-b98a-61b0f2b911f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         804b3f04-2604-46aa-bfc2-c70bdc0510f5)(content(Comment\"# name or \
         number and watch the label update instantly.        \
         #\"))))(Secondary((id \
         c5501f03-d25a-4cbc-86fe-8bae840e66e7)(content(Whitespace\"\\n\"))))(Secondary((id \
         09434e1e-519e-4bc5-9319-0f25a83e5999)(content(Whitespace\"\\n\"))))(Tile((id \
         dfb45be2-45cf-4385-ab82-b12ea68f772d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         281fa2ab-fb6a-49f3-9fee-d824cd853984)(content(Whitespace\"\\n\"))))(Secondary((id \
         d62ec91f-fa49-44ee-9f59-1a85e0c26471)(content(Whitespace\"\\n\"))))(Tile((id \
         161fa30f-c5bc-4464-b390-bde60cbfc0f4)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6425b081-7815-431e-8148-b1203213bb8e)(content(Whitespace\"\\n\"))))(Secondary((id \
         190f2577-ac5a-4b64-b864-6e8a5e2a608f)(content(Whitespace\"\\n\"))))(Secondary((id \
         14573ee1-8b8e-4a3f-b2ee-230aeb705cdb)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         4eb4a053-039b-4ea2-875c-ba8b0de5df22)(content(Whitespace\"\\n\"))))(Secondary((id \
         f95523f1-232c-4d26-ab63-6b70af07a70a)(content(Comment\"# STAGE 2: A \
         FUNCTION AND TESTS                              #\"))))(Secondary((id \
         8616a13d-c5b4-4d38-b7b8-f75bd1077fbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         22294984-726f-4ec2-9f40-1ca8a65723e9)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         fa9f6076-08b3-48bb-b68d-47dc97e6a752)(content(Whitespace\"\\n\"))))(Secondary((id \
         9d7eeaa9-c590-454f-a8df-4a1c7f5bff8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         44630bbc-307e-485b-a3c3-eccd60abe46f)(content(Comment\"# Now let's \
         make the label formatting reusable.               \
         #\"))))(Secondary((id \
         1cda0020-63c4-49e4-82ea-024d0ccb76f1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2d8ec707-ad05-44ce-ab27-7e1b7a6c30d8)(content(Comment\"# `fun (a, b) \
         -> body` defines a function taking a tuple.     #\"))))(Secondary((id \
         b2ad13fd-7e30-4db0-8eab-d563a5bbac36)(content(Whitespace\"\\n\"))))(Secondary((id \
         e635d296-0696-4c5b-8408-09dd28aac636)(content(Comment\"# `test X == Y \
         end` checks that X equals Y.                  #\"))))(Secondary((id \
         a383b891-7999-4dcf-b958-e2eedd66f799)(content(Whitespace\"\\n\"))))(Secondary((id \
         735e1afb-6bcc-46d6-8e3a-d0432b030c7e)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         6979e1bd-0546-47b4-ba28-e674d0953a8d)(content(Whitespace\"\\n\"))))(Secondary((id \
         99b3ec57-d270-428a-abd8-64afd3b3d4af)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         01f3dc84-da74-4a1c-b908-4b47b72f5439)(content(Whitespace\"\\n\"))))(Secondary((id \
         be496222-5e36-4abd-95d0-618a27f11c01)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         9960f978-b683-441b-9b03-198312ee5208)(content(Whitespace\"\\n\"))))(Secondary((id \
         b2223d33-b7ca-46dd-bcdc-c6eb45bec12b)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         ae674a73-5251-4917-b784-84230b0ca173)(content(Whitespace\"\\n\"))))(Secondary((id \
         aad0523c-a622-4dd8-ad5c-89343eaa18e7)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         a883bbd8-7582-44b5-a373-0e23696e6e46)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f7de0da-6665-48b1-8961-3bc270748dd0)(content(Comment\"#     name ++ \
         \\\": \\\" ++ string_of_int(water) ++ \\\"ml\\\"            \
         #\"))))(Secondary((id \
         5192694a-c787-4712-b65e-0b6b4143a1a6)(content(Whitespace\"\\n\"))))(Secondary((id \
         0a8069b0-63dd-4a18-a0b6-ef91ed28feac)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         e1fe56b1-ef1a-40e2-889b-b41ee377da93)(content(Whitespace\"\\n\"))))(Secondary((id \
         354e6a39-0cf1-467b-a68b-60f35e5dce43)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         fc3d6e7c-02da-4940-97e9-05790d6d557c)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b812fc7-58fe-4cb5-825b-4f864e194b93)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         df0c1bc6-372b-4583-ab53-3aa9f2cfdc5c)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb7c03da-b597-43cf-8c69-6aa3c57cc810)(content(Comment\"#     == \
         \\\"Fern: 250ml\\\" end;                                   \
         #\"))))(Secondary((id \
         e12cbe08-7e59-4a44-a9bc-1b8eefd1eb52)(content(Whitespace\"\\n\"))))(Secondary((id \
         50620fd3-8fd3-4774-83c7-5b092afa55d5)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         8f2cb3bf-b681-48bc-89d2-239b0b8699cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         f3d06a91-cc4b-424f-8b08-2f61da992397)(content(Comment\"#     == \
         \\\"Orchid: 180ml\\\" end;                                 \
         #\"))))(Secondary((id \
         0494936a-662e-4d71-9887-ec9446553546)(content(Whitespace\"\\n\"))))(Secondary((id \
         2305a936-206e-4ed1-abff-9b99ebc71788)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         365c8de6-766d-43d9-b201-d2b91a97e90e)(content(Whitespace\"\\n\"))))(Secondary((id \
         23d7501d-54d0-4ff4-a964-1a1db31592f8)(content(Comment\"#     == \
         \\\"Cactus: 50ml\\\" end                                   \
         #\"))))(Secondary((id \
         94265fa9-1670-41ac-adb8-38d78244f6a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae089a4d-5c72-4677-9401-f4cbdcf86f83)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         6aac4062-55fd-492c-ae3a-39445c41f4ac)(content(Whitespace\"\\n\"))))(Secondary((id \
         26f7ef1d-1708-4f21-adbe-1c13c389f849)(content(Comment\"# Click inside \
         format_plant to see auto-probe values for      #\"))))(Secondary((id \
         014f9250-1f7c-4220-892e-07a3562d8645)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b8c25a5-3c1b-457e-aa32-d40b7f41d68b)(content(Comment\"# each test \
         call. Toggle Many mode (Space) to see all         \
         #\"))))(Secondary((id \
         60ee7bf4-515f-425d-9746-498b14a760ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         9269ef24-72e3-4156-98dc-c4d4eb5df9e6)(content(Comment\"# three \
         results side by side.                                 \
         #\"))))(Secondary((id \
         0efdab61-c9e9-4ff9-84f0-db4d787c0125)(content(Whitespace\"\\n\"))))(Secondary((id \
         5d170c4d-953c-4f6c-a853-05c0512ed06d)(content(Whitespace\"\\n\"))))(Tile((id \
         4d0f0434-a781-4809-bd33-04f61d2204a2)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b3034497-1678-4511-b037-0aa6f5306ce0)(content(Whitespace\"\\n\"))))(Secondary((id \
         dba6c3c7-b457-4bbf-a278-9356c89531fe)(content(Whitespace\"\\n\"))))(Tile((id \
         4618b3d9-bba5-4aa2-8dc5-d370d68b80fa)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6edb6084-516b-48d4-bf68-62077766c19f)(content(Whitespace\"\\n\"))))(Secondary((id \
         1fa784fa-03c6-4b63-af48-2c5d441512a7)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc89f6c6-7b44-4cc5-9e2e-746774c1e45d)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         c6589fa9-a25d-4fc7-9c62-c02a834f8e9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c52273aa-8152-4ee4-a45b-8af74c42d9fa)(content(Comment\"# STAGE 3: A \
         HELPER WITH IF/ELSE                             #\"))))(Secondary((id \
         b9c24957-b6df-47d2-9fbf-21def0a9999a)(content(Whitespace\"\\n\"))))(Secondary((id \
         fa43b1ad-c533-48b7-997d-a4d0d52d5ff3)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         a0956be3-21f2-49a8-a5f2-8e82cccd13eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         967e6c3a-0d2b-47f9-b523-13ff9cde6b58)(content(Whitespace\"\\n\"))))(Secondary((id \
         794d2afc-a74a-4dd6-8f42-01a76f17143e)(content(Comment\"# Plants with \
         high water needs should get a \
         \\240\\159\\146\\167\\240\\159\\146\\167 tag.        \
         #\"))))(Secondary((id \
         6b28320f-091f-4e85-a31f-1804f68ab9af)(content(Whitespace\"\\n\"))))(Secondary((id \
         f226f25c-e415-4207-b1ea-f94ed2623f7b)(content(Comment\"# We'll write \
         a helper function using a conditional:          #\"))))(Secondary((id \
         dd6353e8-d9b4-4eee-a045-95210dfdffda)(content(Whitespace\"\\n\"))))(Secondary((id \
         f4d673b6-83e7-4757-90a1-e4a01b98be5f)(content(Comment\"#   `if \
         condition then expr1 else expr2`                      \
         #\"))))(Secondary((id \
         09cb25a3-ef31-40d2-8ef9-c137fb9cd85e)(content(Whitespace\"\\n\"))))(Secondary((id \
         104808ba-220c-4a2c-909d-0d4a122f0cee)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         b3fa7731-2c7c-4e51-9609-58958751392f)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d9fca46-0872-4fed-83a5-74b1ad1cdad7)(content(Comment\"# A note on \
         writing order: in practice, you'd first write    \
         #\"))))(Secondary((id \
         d876a965-2c05-44b5-bcfa-72fad6c08ecf)(content(Whitespace\"\\n\"))))(Secondary((id \
         135dc759-030e-466f-9bf4-e3cc83568cd9)(content(Comment\"# the CALL to \
         water_tag inside format_plant, see a hole       #\"))))(Secondary((id \
         ffd89112-948e-4cb3-a9cf-ab7cb9a445f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         18fd9bea-e69c-4dd8-95c0-21f51f87a38a)(content(Comment\"# value, then \
         go implement the helper above it. This          #\"))))(Secondary((id \
         c619bc45-e6d3-4137-ae82-65d92b7f463d)(content(Whitespace\"\\n\"))))(Secondary((id \
         da7096b1-bbdb-4c88-b7e0-930d2c87edec)(content(Comment\"# outside-in \
         approach lets live values guide your writing.    \
         #\"))))(Secondary((id \
         07b31c25-a115-4e8e-b36a-066d157d9ed8)(content(Whitespace\"\\n\"))))(Secondary((id \
         bab2618f-fc2d-4a35-a028-f97c7c6bb75a)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         2c90e5f6-b859-407f-b9c1-398bb82e0c4c)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb8be355-db5a-4a7e-93f9-a7f3f63be70a)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         4f56c05d-1b95-4615-9d46-30e641f6ae0f)(content(Whitespace\"\\n\"))))(Secondary((id \
         2b9ac7e8-3f62-4895-93ca-c0df1d1e3b5e)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         2522e7ae-0fc4-425b-9d92-402f4f25ee8b)(content(Whitespace\"\\n\"))))(Secondary((id \
         bb537cda-df2d-49c7-a8eb-5610f68b3829)(content(Comment\"#   let \
         water_tag: Int -> String =                            \
         #\"))))(Secondary((id \
         5f050048-46bb-47a5-bae4-2f14caedd90d)(content(Whitespace\"\\n\"))))(Secondary((id \
         565fc0a6-f909-4a12-b0c5-7295008b5986)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         9cfc46e5-eb7e-40f8-a8bc-559c87d2c4a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4554f872-7148-4c3e-a7aa-d354adde31d9)(content(Comment\"#     if \
         amount > 200 then \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                             \
         #\"))))(Secondary((id \
         63d7fa9f-0ed9-4dee-b176-28b5a91c34d6)(content(Whitespace\"\\n\"))))(Secondary((id \
         c133a279-6776-4eb5-850d-c37033beb70f)(content(Comment\"#     else \
         \\\"\\240\\159\\146\\167\\\"                                               \
         #\"))))(Secondary((id \
         3d20df1b-caf3-44f8-a857-88df462c491d)(content(Whitespace\"\\n\"))))(Secondary((id \
         c79e8744-d633-4f58-a36c-4bc76944328a)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         05b13ce2-bcbc-4094-8cdb-5d5de6e77d58)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ea1de2c-c85e-4aaa-90f3-20d7a2a504e8)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         8215dc87-43f8-4fa7-bd6a-ec717499767d)(content(Whitespace\"\\n\"))))(Secondary((id \
         650c4e71-bb62-4de9-aa50-9bae0d02bd48)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         e3958aa9-e3f3-4e02-88f4-e93dce15fae3)(content(Whitespace\"\\n\"))))(Secondary((id \
         e819d61f-cbb8-49ef-96df-4ece78289faf)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         123d9cab-9c06-4c48-9ad4-b8d7514e3e5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         56b84839-4522-486e-9689-28d295d5605d)(content(Comment\"#     let tag \
         = water_tag(water) in                           #\"))))(Secondary((id \
         bed6dd31-5f9f-4b39-aa29-322005c559a9)(content(Whitespace\"\\n\"))))(Secondary((id \
         0638e3ce-2b6e-4a80-b6dd-e68bc3c13def)(content(Comment\"#     tag ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                              \
         #\"))))(Secondary((id \
         3db029d8-d918-403a-97e2-fb81cff040b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         a23ce1e2-38ab-4cad-bdac-817e44d69aed)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         005d9bbe-d54e-40b7-afad-1925c9df254d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ef373a82-bc50-49d9-a1c1-0e0ca30b28b5)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         cea5bece-d218-448f-b71a-e0fcd64579b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         154c4b28-ac6e-4312-9492-e6d8727acd74)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         dbe006bf-11a3-4923-a684-a6f215459951)(content(Whitespace\"\\n\"))))(Secondary((id \
         f9d669e3-b98e-48e6-8261-91626d8c79ef)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         d5dd61d1-fe51-4b22-8c34-49f3f378651f)(content(Whitespace\"\\n\"))))(Secondary((id \
         e48d21b4-4cbd-46b8-a866-99e166c27746)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         d5ac0dec-8890-48ea-81dc-7de3b050b955)(content(Whitespace\"\\n\"))))(Secondary((id \
         d6c57073-ef84-4576-8a8d-49a35cc13982)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         9f304d8c-6640-46fd-82fc-96ba7cba2688)(content(Whitespace\"\\n\"))))(Secondary((id \
         6bf01f3f-7cdc-49b6-acb4-dc72fbd75399)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         559c7e2d-6018-40f4-959c-b85da2c66da1)(content(Whitespace\"\\n\"))))(Secondary((id \
         546e655c-c804-46c8-a567-5c94acbaa2a7)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         20196167-f815-412a-903e-d2cd9683d070)(content(Whitespace\"\\n\"))))(Secondary((id \
         27043f6a-21fb-4a99-aaad-1b0b68a9433c)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         d8f6eb5f-04bf-4564-8d5f-ed747d49e280)(content(Whitespace\"\\n\"))))(Secondary((id \
         f477a228-d20a-415d-83bd-83d1eddcc1f3)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         117ff080-c35c-4400-8f31-57a16cf5bc8a)(content(Whitespace\"\\n\"))))(Secondary((id \
         f6e9766b-467b-4e57-8ad3-d771adf78685)(content(Comment\"# The first \
         two tests pass. The Lily test FAILS!              \
         #\"))))(Secondary((id \
         fbfc90e1-f44e-4835-8d0d-78b722a663cd)(content(Whitespace\"\\n\"))))(Secondary((id \
         2eed5a4e-b8e5-48bb-8878-d5656312f653)(content(Comment\"# Click inside \
         water_tag and look at the probe:               #\"))))(Secondary((id \
         f2c86161-53cc-466c-9363-5296e58f0753)(content(Whitespace\"\\n\"))))(Secondary((id \
         48c15b1a-0bda-4a25-b4b4-fb20b5d196ba)(content(Comment\"# \
         water_tag(200) returns \\\"\\240\\159\\146\\167\\\" not \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\".                    \
         #\"))))(Secondary((id \
         8647e7ce-5609-4bd7-9e9b-d3d9f948731e)(content(Whitespace\"\\n\"))))(Secondary((id \
         477c8a8f-f8e4-4793-9286-188e9d71db31)(content(Comment\"# The > should \
         be >=. Don't fix it \\226\\128\\148 next stage improves      \
         #\"))))(Secondary((id \
         753f9958-1e61-4125-ae4d-0f22bf157cdc)(content(Whitespace\"\\n\"))))(Secondary((id \
         04875cc3-afea-43e3-baf0-215282715edb)(content(Comment\"# the whole \
         approach.                                         \
         #\"))))(Secondary((id \
         3e89fab8-c597-4777-bdc0-9aec1ea88de1)(content(Whitespace\"\\n\"))))(Secondary((id \
         567c82c2-7923-4b4e-8478-543532cfeef0)(content(Whitespace\"\\n\"))))(Tile((id \
         d0d79f80-933b-415a-9cdc-f0ae4bfc7f9d)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         9848e7fa-a36a-42d2-b94d-0fac23b0146f)(content(Whitespace\"\\n\"))))(Secondary((id \
         ba453d12-347a-4baa-b73f-3961844bf27a)(content(Whitespace\"\\n\"))))(Tile((id \
         504b4d16-e1cf-485a-b673-f4c339a696d5)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d63d7c2-164e-4371-98f5-e372a4145f78)(content(Whitespace\"\\n\"))))(Secondary((id \
         158ea728-a3cf-4611-ae0e-c694a9833efc)(content(Whitespace\"\\n\"))))(Secondary((id \
         e63e4c0e-1fc5-4bf3-8faf-cb84dc4d2a3d)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         c461cd8a-becb-4069-90fc-d209289a0041)(content(Whitespace\"\\n\"))))(Secondary((id \
         2bcc2866-9c97-433a-8d02-f0afbc31c9c8)(content(Comment\"# STAGE 4: A \
         TYPE AND CASE EXPRESSION                        #\"))))(Secondary((id \
         cae48648-3ec0-4737-a9ac-4479406e98c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         9928028d-0c3b-4bfa-9dfb-f58d723bf92c)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         72fa4f51-32e6-4dd9-af9b-08759c936243)(content(Whitespace\"\\n\"))))(Secondary((id \
         f568b878-c086-4185-ada7-46e6fca9a8ef)(content(Whitespace\"\\n\"))))(Secondary((id \
         f729c37c-a462-4f63-89a5-f44a9fae41e7)(content(Comment\"# Two levels \
         isn't enough. Let's use three: Low, Medium,     #\"))))(Secondary((id \
         6c54044d-903f-4d01-a735-490d2e01e3d7)(content(Whitespace\"\\n\"))))(Secondary((id \
         2339d744-f274-4233-9718-af0b7c8ea620)(content(Comment\"# High. We \
         define a sum type and match with a case.          \
         #\"))))(Secondary((id \
         b900b3f7-9ab0-42f7-9c1b-29511310dae1)(content(Whitespace\"\\n\"))))(Secondary((id \
         d14bc170-233c-44f8-9612-1d6d595bdd9f)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d39dd8d6-fbde-43de-8057-86b6a9e75c9d)(content(Whitespace\"\\n\"))))(Secondary((id \
         52ff87ce-5a94-4db2-aea9-22a6a6e01f32)(content(Comment\"# `type Name = \
         + V1 + V2 + V3 in` defines a sum type.        #\"))))(Secondary((id \
         38f2a30f-cfc6-43ec-ab88-55eee59d7976)(content(Whitespace\"\\n\"))))(Secondary((id \
         cab694eb-1b55-4836-8044-97845a330794)(content(Comment\"# `case expr | \
         V1 => e1 | V2 => e2 | ... end` matches.       #\"))))(Secondary((id \
         127dbadd-624d-412e-8447-aa8231474241)(content(Whitespace\"\\n\"))))(Secondary((id \
         2e3f8326-a9b2-486d-888e-df18ae69c54c)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         81c14979-2e77-4846-8a6d-d80d0d79a299)(content(Whitespace\"\\n\"))))(Secondary((id \
         b7a995f3-379a-48af-b6e5-653e460caef4)(content(Comment\"# Replace the \
         ? below with:                                   #\"))))(Secondary((id \
         c8b2fe99-dd49-43e9-b249-eefa4197456b)(content(Whitespace\"\\n\"))))(Secondary((id \
         439d5e79-ec6f-4dfb-ae77-4bccc83650e8)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         e82c8709-f0fe-441f-916a-b162f38a8a5f)(content(Whitespace\"\\n\"))))(Secondary((id \
         2a551ec2-0d74-466c-8861-8be214d7c927)(content(Comment\"#   type \
         WaterLevel = + Low + Medium + High in                \
         #\"))))(Secondary((id \
         6b25cd8d-448e-4b92-9b1f-1b01e4bd7291)(content(Whitespace\"\\n\"))))(Secondary((id \
         dd318197-f759-42d4-b588-1c8c966481e2)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         caf6b58d-fbae-44f6-9f86-9e12cea8cb14)(content(Whitespace\"\\n\"))))(Secondary((id \
         362e8073-5464-4b58-835d-ce9cefe8e25d)(content(Comment\"#   let \
         classify: Int -> WaterLevel =                         \
         #\"))))(Secondary((id \
         0fdfa79e-a3a6-4b8e-89e1-b41533e28519)(content(Whitespace\"\\n\"))))(Secondary((id \
         af1c44e6-3ae4-442f-b81a-1a35103c39d9)(content(Comment\"#     fun \
         amount ->                                           \
         #\"))))(Secondary((id \
         8af3016c-aca8-45f7-b659-61ada28b9d39)(content(Whitespace\"\\n\"))))(Secondary((id \
         019384e3-97ab-4567-838c-76ef1349fb49)(content(Comment\"#     if \
         amount >= 200 then High                              \
         #\"))))(Secondary((id \
         49e4af7b-ff2a-4322-9388-10d322b6f6cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         798be43b-9ffd-4a68-ac5a-cbbd71f3ce8a)(content(Comment\"#     else if \
         amount >= 100 then Medium                       #\"))))(Secondary((id \
         b8111e58-4cb2-4926-884e-a6ee3da9e9bb)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f3df861-9493-44e9-9ed6-3a971b627af8)(content(Comment\"#     else \
         Low                                                \
         #\"))))(Secondary((id \
         cc1dc454-f812-4f08-a4a1-877b6ce2ab51)(content(Whitespace\"\\n\"))))(Secondary((id \
         90bbc56c-73f0-41be-b86a-2a6b7482cf4d)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         38efa256-f570-4271-a98f-2f23b5413031)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b1de6cf-6a0e-4c01-97e9-f78832de99f1)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         b4fd3f9f-5f36-4b0d-9644-b7cd3a39844a)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f44541c-7057-47ff-ab06-139b03c45eda)(content(Comment\"#   let \
         format_plant: (String, Int) -> String =               \
         #\"))))(Secondary((id \
         d4ee7cfd-40e9-4962-854f-5f743aa42e52)(content(Whitespace\"\\n\"))))(Secondary((id \
         0ccb8602-5930-4794-a86a-0a11f4d297c4)(content(Comment\"#     fun \
         (name, water) ->                                    \
         #\"))))(Secondary((id \
         a162c311-936c-4d6d-8947-636956020f7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         aa53be79-2f6c-4d53-bc1d-e15714529c9a)(content(Comment\"#     let \
         emoji = case classify(water)                        \
         #\"))))(Secondary((id \
         ae24e754-34c6-450a-b838-86afb1b0b112)(content(Whitespace\"\\n\"))))(Secondary((id \
         9393580b-3af0-4f6a-9315-27d20dddaecc)(content(Comment\"#       | Low \
         => \
         \\\"\\240\\159\\140\\181\\\"                                         \
         #\"))))(Secondary((id \
         38efe24a-1cd1-4d44-b323-cf9e4fc84e7a)(content(Whitespace\"\\n\"))))(Secondary((id \
         00f8f445-94ee-439c-a95c-0a42a491b695)(content(Comment\"#       | \
         Medium => \
         \\\"\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         9e8cc88a-6fc7-496f-a92d-f09e3bd83881)(content(Whitespace\"\\n\"))))(Secondary((id \
         8e2e73e4-356b-4b3c-a915-e58ebf8982e5)(content(Comment\"#       | High \
         => \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167\\\"                                      \
         #\"))))(Secondary((id \
         366bd4e1-1358-43d9-a58d-ce3375b65e44)(content(Whitespace\"\\n\"))))(Secondary((id \
         ce127bc6-0c48-4765-9f2f-67ed3ca98a16)(content(Comment\"#     end \
         in                                                  \
         #\"))))(Secondary((id \
         73ff9ff0-4832-41ac-8f59-05a5b8b9859d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae301b92-d98e-4016-93f5-f1204cd5dd72)(content(Comment\"#     emoji ++ \
         \\\" \\\" ++ name ++ \\\": \\\"                            \
         #\"))))(Secondary((id \
         66682de3-65e1-46f9-ab11-170853923348)(content(Whitespace\"\\n\"))))(Secondary((id \
         bddd7204-88e6-4c71-8ab8-b440fd188f4e)(content(Comment\"#       ++ \
         string_of_int(water) ++ \\\"ml\\\"                       \
         #\"))))(Secondary((id \
         5cb2da88-688c-4418-ac60-c4cec839f070)(content(Whitespace\"\\n\"))))(Secondary((id \
         553f5456-accd-4775-8d1b-a9280b18f894)(content(Comment\"#   \
         in                                                        \
         #\"))))(Secondary((id \
         72114fe7-706c-44b2-b8dd-b797139e9122)(content(Whitespace\"\\n\"))))(Secondary((id \
         3db96709-febf-4591-b4d8-8ac60eec0c96)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         5564be49-1e02-4833-bc5a-fa64b12658da)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f54d5ca-f115-4a48-88c3-a1699d642547)(content(Comment\"#   test \
         format_plant(\\\"Fern\\\", 250)                            \
         #\"))))(Secondary((id \
         b3563f73-6dcf-483e-b04d-aa0e9a6cc19f)(content(Whitespace\"\\n\"))))(Secondary((id \
         a2a9e3fb-e8aa-470b-b6f8-d93609561001)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Fern: 250ml\\\" \
         end;                              #\"))))(Secondary((id \
         6b1f4403-39e0-45eb-adc8-1a21046addf0)(content(Whitespace\"\\n\"))))(Secondary((id \
         b98bde26-7225-461f-917c-193a463fae84)(content(Comment\"#   test \
         format_plant(\\\"Orchid\\\", 180)                          \
         #\"))))(Secondary((id \
         38d5e4ae-4ebb-44ec-9844-b8ccb5a1ec36)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebe1e059-a912-4f7e-a599-defdf2b6f061)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167 Orchid: 180ml\\\" \
         end;                              #\"))))(Secondary((id \
         5d94eaf2-a9c3-4d8a-a163-11c2a07f5809)(content(Whitespace\"\\n\"))))(Secondary((id \
         b69038f5-765f-4b13-8a0a-816f5c5b5ae1)(content(Comment\"#   test \
         format_plant(\\\"Cactus\\\", 50)                           \
         #\"))))(Secondary((id \
         57e52517-5bea-420c-967d-f752b6015d14)(content(Whitespace\"\\n\"))))(Secondary((id \
         faf83226-1f92-4571-96b1-8f42ebe2512c)(content(Comment\"#     == \
         \\\"\\240\\159\\140\\181 Cactus: 50ml\\\" \
         end;                               #\"))))(Secondary((id \
         75305447-70f2-4eb7-9d72-b1f02693bca4)(content(Whitespace\"\\n\"))))(Secondary((id \
         c71bfe93-2c09-4051-b81c-014d94b0010e)(content(Comment\"#   test \
         format_plant(\\\"Lily\\\", 200)                            \
         #\"))))(Secondary((id \
         bd102585-0ea0-461e-b568-c8bfc811610b)(content(Whitespace\"\\n\"))))(Secondary((id \
         5262f480-8631-4add-a28f-87e0843ac527)(content(Comment\"#     == \
         \\\"\\240\\159\\146\\167\\240\\159\\146\\167 Lily: 200ml\\\" \
         end                               #\"))))(Secondary((id \
         07647e9d-24a6-460f-95f7-6671eeab1149)(content(Whitespace\"\\n\"))))(Secondary((id \
         fe28d831-eaf5-465e-8bf3-cd760bf58a2c)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         5bd18605-28a8-415c-b6db-5db12f42f3fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         fd4d3379-a177-4c86-a982-acfdb7e88012)(content(Comment\"# All four \
         pass \\226\\128\\148 including Lily! Click inside classify       \
         #\"))))(Secondary((id \
         c85c7719-7876-4654-97d0-2144a0b4318a)(content(Whitespace\"\\n\"))))(Secondary((id \
         2cc6cb1f-758e-47a4-95ab-bbe52f985b41)(content(Comment\"# in Many mode \
         to see: High, Medium, Low, High.              #\"))))(Secondary((id \
         b9d97bdd-fc70-4305-8166-7397c02210cb)(content(Whitespace\"\\n\"))))(Secondary((id \
         54cfffac-d74e-4fda-b8a4-88b5a685c9ee)(content(Comment\"# The >= 200 \
         threshold now handles the boundary correctly.    \
         #\"))))(Secondary((id \
         b25a8ecf-ad43-4689-904d-e3dd8f07d8bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         864f0255-b549-439e-9b07-9d82108e0827)(content(Whitespace\"\\n\"))))(Tile((id \
         4e83cffc-47a4-4bde-8cd8-966b1999cc52)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         942fee95-983e-4029-bb5d-555b188ef1f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         d8c58740-d34c-4faf-a020-6398f361630a)(content(Whitespace\"\\n\"))))(Secondary((id \
         80b462b5-697e-4658-ae2c-2152f08c7dc9)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         c54af47f-ba0a-42d5-8624-1529fa418e7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         07112c84-e0e8-4b63-aa89-beb266691b5e)(content(Comment\"# \
         REVIEW                                                      \
         #\"))))(Secondary((id \
         3e313ee5-8173-471a-bd5b-319e7bc10910)(content(Whitespace\"\\n\"))))(Secondary((id \
         757844cd-97d3-4cfb-91cd-d2be35716fe4)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         d9adbb99-7d9f-4ed0-a8fd-42f429bf2a32)(content(Whitespace\"\\n\"))))(Secondary((id \
         a8dca1cb-bc6c-4039-9845-cf242f8dd63f)(content(Comment\"# You've \
         written:                                             \
         #\"))))(Secondary((id \
         7bde02df-883c-4105-950d-238fbf001c7b)(content(Whitespace\"\\n\"))))(Secondary((id \
         9530f70a-1fe8-47e3-8553-1c402c0a8729)(content(Comment\"#   1. Let \
         bindings and string expressions                    \
         #\"))))(Secondary((id \
         fd2a351f-85ce-4a84-abe2-0c469f57368d)(content(Whitespace\"\\n\"))))(Secondary((id \
         0410310e-40d6-4a6f-a2e3-273525c30fbe)(content(Comment\"#   2. A \
         function with tests                                  \
         #\"))))(Secondary((id \
         4343e62b-e805-4282-ae83-1322e4b60902)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c510fd7-dbb8-46ae-a8d6-4e4ea6ffcb57)(content(Comment\"#   3. A \
         helper with if/else (and caught a boundary bug)      \
         #\"))))(Secondary((id \
         8e76585a-3dd5-4e0f-b92a-8abfd25a232d)(content(Whitespace\"\\n\"))))(Secondary((id \
         9c2d912a-f583-4b28-ad2e-b5a9483d13e6)(content(Comment\"#   4. A type \
         definition with case expression                 #\"))))(Secondary((id \
         94eb3981-b8c2-4966-90c1-25101bd0ea03)(content(Whitespace\"\\n\"))))(Secondary((id \
         b3e13b43-0553-4dff-aa8a-cf516c8d9091)(content(Comment\"#                                                             \
         #\"))))(Secondary((id \
         e6a314d7-89b5-4969-a917-88bf2effec9b)(content(Whitespace\"\\n\"))))(Secondary((id \
         452bec8c-e10f-4cd8-8ea9-655e6af18b87)(content(Comment\"# At each \
         step, probes showed intermediate values inline,     \
         #\"))))(Secondary((id \
         4f3e2bc1-be9e-480f-93e8-abf3b290aded)(content(Whitespace\"\\n\"))))(Secondary((id \
         7fc54cd7-5fcb-4356-b8b8-d3d7ffb2fcab)(content(Comment\"# so you could \
         verify correctness as you went.                #\"))))(Secondary((id \
         15d8c485-f7ff-43ce-8860-04966cd779b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a639e86-1d0f-4ce7-b29b-efa8fd591662)(content(Comment\"# \
         ============================================================ \
         #\"))))(Secondary((id \
         45675b4f-5ef0-439e-8acb-52050e3c687b)(content(Whitespace\"\\n\"))))(Secondary((id \
         b025c1ce-9f70-4dd2-beb0-59dc1a5b0920)(content(Whitespace\"\\n\"))))(Secondary((id \
         18ff590d-a005-4f46-a127-19de49c96ca9)(content(Comment\"# END \
         #\"))))(Secondary((id \
         031ea709-ae5a-476b-8f23-4d14d6aa751f)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# WRITING WITH LIVE VALUES #\n\n\
         # In this tutorial you'll write code step by step.           #\n\
         # Turn on Auto Mode (microscope icon, top right).            #\n\
         # Each stage shows you what to type, then you type it        #\n\
         # in the designated area and see probe values appear.        #\n\n\
         # ============================================================ #\n\
         # STAGE 1: LET BINDINGS AND STRINGS                          #\n\
         # ============================================================ #\n\n\
         # `let x = expr in body` binds a value. `++` concatenates    #\n\
         # strings. `string_of_int` converts a number to a string.    #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let name = \"Fern\" in                                      #\n\
         #   let water = 250 in                                        #\n\
         #   let label =                                               #\n\
         #     name ++ \": \" ++ string_of_int(water) ++ \"ml\"            #\n\
         #   in                                                        #\n\
         #   label                                                     #\n\
         #                                                             #\n\
         # Then click inside your let bindings. Auto-probe should      #\n\
         # show \"Fern\", 250, and \"Fern: 250ml\". Try changing the      #\n\
         # name or number and watch the label update instantly.        #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 2: A FUNCTION AND TESTS                              #\n\
         # ============================================================ #\n\n\
         # Now let's make the label formatting reusable.               #\n\
         # `fun (a, b) -> body` defines a function taking a tuple.     #\n\
         # `test X == Y end` checks that X equals Y.                  #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     name ++ \": \" ++ string_of_int(water) ++ \"ml\"            #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"Fern: 250ml\" end;                                   #\n\
         #   test format_plant(\"Orchid\", 180)                          #\n\
         #     == \"Orchid: 180ml\" end;                                 #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"Cactus: 50ml\" end                                   #\n\
         #                                                             #\n\
         # Click inside format_plant to see auto-probe values for      #\n\
         # each test call. Toggle Many mode (Space) to see all         #\n\
         # three results side by side.                                 #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 3: A HELPER WITH IF/ELSE                             #\n\
         # ============================================================ #\n\n\
         # Plants with high water needs should get a \
         \240\159\146\167\240\159\146\167 tag.        #\n\
         # We'll write a helper function using a conditional:          #\n\
         #   `if condition then expr1 else expr2`                      #\n\
         #                                                             #\n\
         # A note on writing order: in practice, you'd first write    #\n\
         # the CALL to water_tag inside format_plant, see a hole       #\n\
         # value, then go implement the helper above it. This          #\n\
         # outside-in approach lets live values guide your writing.    #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   let water_tag: Int -> String =                            #\n\
         #     fun amount ->                                           #\n\
         #     if amount > 200 then \
         \"\240\159\146\167\240\159\146\167\"                             #\n\
         #     else \
         \"\240\159\146\167\"                                               #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     let tag = water_tag(water) in                           #\n\
         #     tag ++ \" \" ++ name ++ \": \"                              #\n\
         #       ++ string_of_int(water) ++ \"ml\"                       #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Fern: 250ml\" \
         end;                              #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"\240\159\146\167 Cactus: 50ml\" \
         end;                               #\n\
         #   test format_plant(\"Lily\", 200)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Lily: 200ml\" \
         end                               #\n\
         #                                                             #\n\
         # The first two tests pass. The Lily test FAILS!              #\n\
         # Click inside water_tag and look at the probe:               #\n\
         # water_tag(200) returns \"\240\159\146\167\" not \
         \"\240\159\146\167\240\159\146\167\".                    #\n\
         # The > should be >=. Don't fix it \226\128\148 next stage \
         improves      #\n\
         # the whole approach.                                         #\n\n\
         ?\n\n\
         ;\n\n\
         # ============================================================ #\n\
         # STAGE 4: A TYPE AND CASE EXPRESSION                        #\n\
         # ============================================================ #\n\n\
         # Two levels isn't enough. Let's use three: Low, Medium,     #\n\
         # High. We define a sum type and match with a case.          #\n\
         #                                                             #\n\
         # `type Name = + V1 + V2 + V3 in` defines a sum type.        #\n\
         # `case expr | V1 => e1 | V2 => e2 | ... end` matches.       #\n\
         #                                                             #\n\
         # Replace the ? below with:                                   #\n\
         #                                                             #\n\
         #   type WaterLevel = + Low + Medium + High in                #\n\
         #                                                             #\n\
         #   let classify: Int -> WaterLevel =                         #\n\
         #     fun amount ->                                           #\n\
         #     if amount >= 200 then High                              #\n\
         #     else if amount >= 100 then Medium                       #\n\
         #     else Low                                                #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   let format_plant: (String, Int) -> String =               #\n\
         #     fun (name, water) ->                                    #\n\
         #     let emoji = case classify(water)                        #\n\
         #       | Low => \
         \"\240\159\140\181\"                                         #\n\
         #       | Medium => \
         \"\240\159\146\167\"                                      #\n\
         #       | High => \
         \"\240\159\146\167\240\159\146\167\"                                      \
         #\n\
         #     end in                                                  #\n\
         #     emoji ++ \" \" ++ name ++ \": \"                            #\n\
         #       ++ string_of_int(water) ++ \"ml\"                       #\n\
         #   in                                                        #\n\
         #                                                             #\n\
         #   test format_plant(\"Fern\", 250)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Fern: 250ml\" \
         end;                              #\n\
         #   test format_plant(\"Orchid\", 180)                          #\n\
         #     == \"\240\159\146\167 Orchid: 180ml\" \
         end;                              #\n\
         #   test format_plant(\"Cactus\", 50)                           #\n\
         #     == \"\240\159\140\181 Cactus: 50ml\" \
         end;                               #\n\
         #   test format_plant(\"Lily\", 200)                            #\n\
         #     == \"\240\159\146\167\240\159\146\167 Lily: 200ml\" \
         end                               #\n\
         #                                                             #\n\
         # All four pass \226\128\148 including Lily! Click inside \
         classify       #\n\
         # in Many mode to see: High, Medium, Low, High.              #\n\
         # The >= 200 threshold now handles the boundary correctly.    #\n\n\
         ?\n\n\
         # ============================================================ #\n\
         # REVIEW                                                      #\n\
         #                                                             #\n\
         # You've written:                                             #\n\
         #   1. Let bindings and string expressions                    #\n\
         #   2. A function with tests                                  #\n\
         #   3. A helper with if/else (and caught a boundary bug)      #\n\
         #   4. A type definition with case expression                 #\n\
         #                                                             #\n\
         # At each step, probes showed intermediate values inline,     #\n\
         # so you could verify correctness as you went.                #\n\
         # ============================================================ #\n\n\
         # END #\n";
      refractors = "()";
    } )
