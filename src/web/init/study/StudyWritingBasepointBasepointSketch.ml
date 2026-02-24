let out : string * Haz3lcore.PersistentSegment.t =
  ( "Study / writing / basepoint / basepoint-sketch",
    {
      segment =
        "((Secondary((id \
         6cf87b51-e69c-4da6-b112-4a29f7c83454)(content(Comment\"# BASE ROUTE \
         TASK                              #\"))))(Secondary((id \
         c2c66157-f916-44e9-9a45-33982087a88b)(content(Whitespace\"\\n\"))))(Secondary((id \
         033ab384-6c46-4c50-ba12-2a63bcb7027b)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d7a48e00-e19d-4bf9-9774-ef888f7f5ea5)(content(Whitespace\"\\n\"))))(Secondary((id \
         39247149-5512-46e8-aff0-d658d9805d95)(content(Comment\"# Implement \
         base_route: extract the first      #\"))))(Secondary((id \
         d81fa563-0e13-4540-88f9-998faf717fb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         c6cd548f-4f9e-4063-b6b3-32a36a86d908)(content(Comment\"# path segment \
         from a URL path.                #\"))))(Secondary((id \
         3155c86a-705b-4d94-87f7-5348f94a2111)(content(Whitespace\"\\n\"))))(Secondary((id \
         aab115a9-f9d3-4cd8-91d8-e17f2b104923)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         09aaaeeb-041b-4148-b492-ff92c04c4ab1)(content(Whitespace\"\\n\"))))(Secondary((id \
         2ada880d-47b6-410a-99a2-6893bd879c7b)(content(Comment\"# \
         Examples:                                    #\"))))(Secondary((id \
         c9a3afa6-57c5-461e-b13b-89bb99a770d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         cd5cd02f-e293-4ab4-941f-b73b38bbb5c0)(content(Comment\"#   \
         base_route(\\\"/api/v1\\\") == \\\"api\\\"             \
         #\"))))(Secondary((id \
         72a2411f-5493-49ab-9daa-00dc7a90aaeb)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a028f7b-dde0-4381-a113-e67ba753ad07)(content(Comment\"#   \
         base_route(\\\"/api/actions/rm\\\") == \\\"api\\\"     \
         #\"))))(Secondary((id \
         b9b6057a-2083-49dc-9f15-3500b3d0ee19)(content(Whitespace\"\\n\"))))(Secondary((id \
         4cc7812b-32e6-4370-94a3-4567579f7b98)(content(Comment\"#   \
         base_route(\\\"/\\\") == \\\"\\\"                      \
         #\"))))(Secondary((id \
         744ecf23-1f61-43fa-90cd-748f6a1f4485)(content(Whitespace\"\\n\"))))(Secondary((id \
         400f0579-df80-42e8-9fe7-84297bd5907c)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         874d3a14-348a-4357-880e-d4b170ce59df)(content(Whitespace\"\\n\"))))(Secondary((id \
         e06aa191-f048-4e86-96e3-25696c62a7b3)(content(Comment\"# Available \
         functions:                         #\"))))(Secondary((id \
         6bad4d63-34ac-44c4-b50f-e1ac8d704339)(content(Whitespace\"\\n\"))))(Secondary((id \
         67ef2de9-00e8-40a8-8da0-53644da7fe18)(content(Comment\"#   \
         string_split(sep, str) -> [String]         #\"))))(Secondary((id \
         ec906408-9ac6-4a22-9ffc-5f982f590b3c)(content(Whitespace\"\\n\"))))(Secondary((id \
         add02e91-885e-4ec7-bfde-d7ed657c776b)(content(Comment\"#   \
         string_concat(s1, s2) -> String            #\"))))(Secondary((id \
         f95fba68-3ffa-425e-9214-62724ce2371e)(content(Whitespace\"\\n\"))))(Secondary((id \
         c9b83dab-582d-40ac-b91b-a5ffe6bfa37b)(content(Comment\"#   \
         string_length(s) -> Int                    #\"))))(Secondary((id \
         e03546fb-8967-48df-8619-2d4d204c2445)(content(Whitespace\"\\n\"))))(Secondary((id \
         5df47d5a-80ea-4830-b36f-17eb142f5699)(content(Comment\"#   \
         string_sub(str, pos, len) -> String        #\"))))(Secondary((id \
         a503f597-3954-459c-8a03-62f7f0f02062)(content(Whitespace\"\\n\"))))(Secondary((id \
         7c43c7e5-e0eb-4637-a717-c96491a2d04d)(content(Comment\"#   nth(list, \
         index) -> element                #\"))))(Secondary((id \
         aa329611-ee91-4963-80ea-1fb691e43a70)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e730ca1-60b5-43bf-9c78-2394cc647dbd)(content(Comment\"#   \
         length(list) -> Int                        #\"))))(Secondary((id \
         260ad328-125d-474e-b1ab-aff6b1ca0633)(content(Whitespace\"\\n\"))))(Secondary((id \
         88b214a7-1f3b-47ac-a3e6-5dc3f0b51d8f)(content(Comment\"#   map(list, \
         fn) -> list                      #\"))))(Secondary((id \
         408ceafd-b75e-4c61-8f97-bfa8498917da)(content(Whitespace\"\\n\"))))(Secondary((id \
         f582b1d1-adcb-4f36-9076-613a53220de2)(content(Comment\"#   \
         filter(list, pred) -> list                 #\"))))(Secondary((id \
         8ac2d120-dc17-470d-88fa-9db07700f538)(content(Whitespace\"\\n\"))))(Secondary((id \
         cc96dd76-2233-47ec-b9f6-d31be0c94e02)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         d49601c1-c322-4610-9c2f-37df8235edba)(content(Whitespace\"\\n\"))))(Secondary((id \
         6ddb6844-e9ad-446e-be3b-e1aac11db6ca)(content(Comment\"# Function \
         syntax: fun param -> body           #\"))))(Secondary((id \
         54869a1b-2570-44a2-b848-3ad8a86f2fb3)(content(Whitespace\"\\n\"))))(Secondary((id \
         4c007490-1073-4034-a098-4452838a62f7)(content(Comment\"# Let binding: \
         let name = value in ...         #\"))))(Secondary((id \
         224f188f-d772-4064-93b4-17159aad07e5)(content(Whitespace\"\\n\"))))(Secondary((id \
         4eb99fd8-17ac-4ba2-a66e-30712a7e8706)(content(Comment\"#                                              \
         #\"))))(Secondary((id \
         27e27d25-0d40-485b-9a25-a63321f58bbf)(content(Whitespace\"\\n\"))))(Secondary((id \
         6420347b-859b-4987-bff0-69bbeb5b6f0d)(content(Comment\"# Tip: Turn on \
         auto-probe (microscope toggle)  #\"))))(Secondary((id \
         b9042174-3861-4d8c-8d2b-94ddf47034fb)(content(Whitespace\"\\n\"))))(Secondary((id \
         eeed1a30-bd9a-4802-a746-4e0593ea3be9)(content(Comment\"# to see \
         intermediate values as you type.      #\"))))(Secondary((id \
         8f0c78d4-d5d9-40dd-b9ea-9fb6129e7de8)(content(Whitespace\"\\n\"))))(Secondary((id \
         910dc030-f167-4447-9ac5-8acd49f5b023)(content(Whitespace\"\\n\"))))(Tile((id \
         70618e1c-090b-46a7-899b-61dd4d91c30d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f2739b2e-8435-4de6-9b24-51d9e2eeb104)(content(Whitespace\" \
         \"))))(Tile((id \
         c87c7ad2-377f-4063-bc5d-b04501527b0b)(label(base_route))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         cbc7ce7c-7434-41c2-8acb-e9fe954a6b0a)(content(Whitespace\" \
         \")))))((Secondary((id \
         33d4490d-cbaf-4ef0-bc4c-89f3024f261d)(content(Whitespace\" \
         \"))))(Tile((id c7792320-5310-475f-a93c-f843fad20569)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         86cb4aab-2a60-48aa-b80d-1ec565ce8aa2)(content(Whitespace\" \
         \"))))(Tile((id \
         e55a4aa7-bcf5-44ec-8a19-48b1193683b1)(label(path))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8f83d18a-58c1-405e-a9a9-45697c70729c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4215d039-0622-4f6c-bbbf-afe71c9b0b60)(content(Whitespace\"\\n\"))))(Tile((id \
         896bf03a-b2a2-4301-85fa-2c0755f704ce)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6840132c-d754-4fdb-918c-a84f73235916)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         21131d75-4269-4d80-8084-c3bbbadb0729)(content(Whitespace\"\\n\"))))(Secondary((id \
         3d22dad8-631e-4dcb-98ea-c309998a35e2)(content(Whitespace\"\\n\"))))(Tile((id \
         33a9d5fa-ee2b-4aae-87de-f92dd18661b7)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         8d0232d1-f641-41c0-8e86-5f519953f0c6)(content(Whitespace\"\\n\"))))(Tile((id \
         fd6d86dd-2058-4b55-824d-0ee00827d2bc)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1268b377-7a9b-415a-bb84-34c8723c9fba)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cf0c9d86-19ac-47d5-a279-0a6632cd48a4)(label(\"\\\"/api/v1\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         97f27c45-2992-46f3-adfd-5b33c7e9387e)(content(Whitespace\"\\n\"))))(Tile((id \
         abfc71e4-948e-4485-82b1-ca5f11027daf)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         732dc8b8-230b-4547-a954-e20c64f27589)(content(Whitespace\" \
         \"))))(Tile((id \
         665cc729-7a75-48eb-8a81-1668878dbb04)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f5c49369-78ad-425c-9e7f-3c8ae1b70f36)(content(Whitespace\"\\n\")))))))))(Tile((id \
         865591ea-5953-4263-aa01-fdfaeea69466)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         139e53b1-749b-4d54-8634-16e14b42e4d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         8a3f4f7c-ccbc-4043-b03e-2ad589365fe9)(content(Whitespace\"\\n\"))))(Tile((id \
         7aee7772-5cfc-4f6c-84c6-66d57392ea83)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         01489720-cdfe-475a-b66c-60c83704ef40)(content(Whitespace\"\\n\"))))(Tile((id \
         c77553d0-abb6-44a8-b5f1-3350ac159570)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f357c9e9-673f-4b21-9099-be71a380a8e9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e515c5a9-099d-4bca-8704-806dc0e64568)(label(\"\\\"/api/actions/rm\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         5f11ce60-8abe-42b4-90ec-a5d26be91829)(content(Whitespace\"\\n\"))))(Tile((id \
         c7f3e1a9-afeb-4ee6-ae7b-447be2539a8e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         39f3e070-aed2-4849-8d84-87adca561f5d)(content(Whitespace\" \
         \"))))(Tile((id \
         5527657b-cdfc-465d-991e-df964a21730f)(label(\"\\\"api\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         89a13e2b-5339-4750-bbfa-b0991d9730f3)(content(Whitespace\"\\n\")))))))))(Tile((id \
         e1675d0e-2a3d-4a56-ac02-2c4ee5fa8390)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         578fac41-63c0-40d8-9e01-0570f617e180)(content(Whitespace\"\\n\"))))(Secondary((id \
         e6008edf-48a2-487d-8e1e-c0172537b4e0)(content(Whitespace\"\\n\"))))(Tile((id \
         d803e431-86e8-49b0-b0f6-1ce1c1196992)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         0f81b108-e624-49b5-bf6d-a090b371ffce)(content(Whitespace\"\\n\"))))(Tile((id \
         850e921e-f1c2-4c3a-8278-e89aeb296a46)(label(base_route))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4035e961-13b8-4160-9728-c43c1280df42)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bfc7cea0-77df-4317-b55e-63bda23a80a7)(label(\"\\\"/\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         084c8572-d64f-4d77-8fb7-caab9352c9bd)(content(Whitespace\"\\n\"))))(Tile((id \
         3962c955-7af1-4574-a83a-eb14e55129f4)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         45a64375-9e07-4e2b-902d-5c1e0a5430a9)(content(Whitespace\" \
         \"))))(Tile((id \
         857e3e4b-6072-4680-9049-c4ee42ecf02a)(label(\"\\\"\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e2591597-49e6-49f3-99c2-5580ec5522f8)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         03b5a195-b5e1-4d52-ac00-2b42495cff8a)(content(Whitespace\"\\n\")))))";
      backup_text =
        "# BASE ROUTE TASK                              #\n\
         #                                              #\n\
         # Implement base_route: extract the first      #\n\
         # path segment from a URL path.                #\n\
         #                                              #\n\
         # Examples:                                    #\n\
         #   base_route(\"/api/v1\") == \"api\"             #\n\
         #   base_route(\"/api/actions/rm\") == \"api\"     #\n\
         #   base_route(\"/\") == \"\"                      #\n\
         #                                              #\n\
         # Available functions:                         #\n\
         #   string_split(sep, str) -> [String]         #\n\
         #   string_concat(s1, s2) -> String            #\n\
         #   string_length(s) -> Int                    #\n\
         #   string_sub(str, pos, len) -> String        #\n\
         #   nth(list, index) -> element                #\n\
         #   length(list) -> Int                        #\n\
         #   map(list, fn) -> list                      #\n\
         #   filter(list, pred) -> list                 #\n\
         #                                              #\n\
         # Function syntax: fun param -> body           #\n\
         # Let binding: let name = value in ...         #\n\
         #                                              #\n\
         # Tip: Turn on auto-probe (microscope toggle)  #\n\
         # to see intermediate values as you type.      #\n\n\
         let base_route = fun path ->\n\
         ?\n\
         in\n\n\
         test\n\
         base_route(\"/api/v1\")\n\
         == \"api\"\n\
         end;\n\n\
         test\n\
         base_route(\"/api/actions/rm\")\n\
         == \"api\"\n\
         end;\n\n\
         test\n\
         base_route(\"/\")\n\
         == \"\"\n\
         end\n";
      refractors = "()";
    } )
