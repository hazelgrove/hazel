let out : string * Haz3lcore.PersistentSegment.t =
  ( "Probes",
    {
      segment =
        "((Secondary((id \
         10710d48-e178-4283-b35e-93f621a7e0d8)(content(Comment\"#  \
         _____           _                #\"))))(Secondary((id \
         d3af44d6-d5ee-488f-ac9d-b654bcdf9b9a)(content(Whitespace\"\\n\"))))(Secondary((id \
         08e74e57-21b8-4fb5-9209-5ffef47130eb)(content(Comment\"# |  __ \
         \\\\         | |               #\"))))(Secondary((id \
         aae3c35e-f8ab-4d02-a292-685c06309b44)(content(Whitespace\"\\n\"))))(Secondary((id \
         46f3b831-2d14-4a0c-ab49-07aa5ad8bb64)(content(Comment\"# | |__) | __ \
         ___ | |__   ___  ___  #\"))))(Secondary((id \
         4938b149-2ec1-4235-9802-c211c5cc7d04)(content(Whitespace\"\\n\"))))(Secondary((id \
         572b0c9a-cc47-4839-89b0-f5a1b1b58697)(content(Comment\"# |  ___/ '__/ \
         _ \\\\| '_ \\\\ / _ \\\\/ __| #\"))))(Secondary((id \
         48aa5267-6f8b-4eec-a368-fc26a4cf205c)(content(Whitespace\"\\n\"))))(Secondary((id \
         46e3aa18-8f6e-44c5-b231-16eef33c44da)(content(Comment\"# | |   | | | \
         (_) | |_) |  __/\\\\__ \\\\ #\"))))(Secondary((id \
         a0879bdd-523e-452a-bc77-5b0ac4536d32)(content(Whitespace\"\\n\"))))(Secondary((id \
         524e1860-fbb6-4b1c-9c17-d7d1a24b7f65)(content(Comment\"# |_|   |_|  \
         \\\\___/|_.__/ \\\\___||___/ #\"))))(Secondary((id \
         5edb290e-2561-4352-9ea7-a02e53811f89)(content(Whitespace\"\\n\"))))(Secondary((id \
         4e15228b-1a98-4b83-acbb-6b0d5bc42ea6)(content(Comment\"#    INLINE \
         EVAL WITH LIVE PROBES   #\"))))(Secondary((id \
         78d15da6-8021-4110-a425-74befb18b377)(content(Whitespace\"\\n\"))))(Secondary((id \
         d02b69e5-51e0-4732-a422-41d519f15ab8)(content(Whitespace\"\\n\"))))(Secondary((id \
         3c12d4fa-f8f3-452a-9f84-950e382652ae)(content(Comment\"# INTRODUCTION \
         #\"))))(Secondary((id \
         47bb8a86-5a3c-4972-a02b-eee58ce9e229)(content(Whitespace\"\\n\"))))(Secondary((id \
         7067834b-6063-434e-a600-efba25af0b76)(content(Whitespace\"\\n\"))))(Secondary((id \
         1767f0e1-e5f7-4e00-92fe-2993bba6b67d)(content(Comment\"# Probe permit \
         a kind of inline evaluation, #\"))))(Secondary((id \
         5c44cdb0-9ca5-43aa-aeba-2373b65a8f63)(content(Whitespace\"\\n\"))))(Secondary((id \
         71e18b7e-08dd-43a4-ae5a-e868d5cc8052)(content(Comment\"# similar to \
         value hints in Emacs or IntelliJ. #\"))))(Secondary((id \
         cd15320e-f6d4-42a3-981e-d9bc20283a18)(content(Whitespace\"\\n\"))))(Secondary((id \
         c642eaf2-ab5a-44d5-94b2-155214cdcbba)(content(Whitespace\"\\n\"))))(Secondary((id \
         5c4c474b-0361-4602-96ca-3a335a9b6d88)(content(Comment\"# You can put \
         one on any expression or pattern to see #\"))))(Secondary((id \
         72bb6073-a496-43a5-a829-3abf2d8cc911)(content(Whitespace\"\\n\"))))(Secondary((id \
         dff0e609-0626-4dad-ac20-df994c81736a)(content(Comment\"# the values \
         it takes on during evaluation. Sampled #\"))))(Secondary((id \
         815cf2cb-327c-40d3-bdae-e7464908e13d)(content(Whitespace\"\\n\"))))(Secondary((id \
         265c9244-d57d-40c8-a39f-4a6c8a1ccba5)(content(Comment\"# values are \
         sorted by left-to-right by most-recent. #\"))))(Secondary((id \
         c608ebe4-8984-4bbd-aa97-fed1473d6eb0)(content(Whitespace\"\\n\"))))(Secondary((id \
         38894d82-8a3c-40b5-83f3-c5949aa4f171)(content(Whitespace\"\\n\"))))(Secondary((id \
         d5198a95-2f1e-42af-8eae-3c34e7b5239d)(content(Comment\"# When a \
         sample is selected, you can hover over it to see \
         #\"))))(Secondary((id \
         da03936d-55be-4b82-9ae8-f18ac1506cff)(content(Whitespace\"\\n\"))))(Secondary((id \
         b138238b-d3d6-4e99-bf67-ef6350f718be)(content(Comment\"# relevant \
         environment variables, and all /other/ samples #\"))))(Secondary((id \
         62c15a77-7bcb-4dd3-a744-cfea2548b195)(content(Whitespace\"\\n\"))))(Secondary((id \
         406ec84f-9859-424d-bf9c-19f661d605c6)(content(Comment\"# are \
         decorated according to their relative position in \
         #\"))))(Secondary((id \
         65ed2f2f-a040-4eec-b6d9-c234aad2d382)(content(Whitespace\"\\n\"))))(Secondary((id \
         c72a615e-216a-46d7-9699-1224b6b9bc63)(content(Comment\"# the call \
         stack relative to the selected sample. #\"))))(Secondary((id \
         949f2d6b-581b-442c-8c47-b37dc90e70f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9799f52e-2e09-4555-8039-b48b63a5c540)(content(Whitespace\"\\n\"))))(Secondary((id \
         1ba583c4-83dd-4241-8acb-b02452b08033)(content(Comment\"# Probes \
         replace print statements while also offering some \
         #\"))))(Secondary((id \
         8e710b55-be43-45bc-a7b1-014a18413e17)(content(Whitespace\"\\n\"))))(Secondary((id \
         6f26773b-5e9c-49a6-add3-38d0219dd35d)(content(Comment\"# stepping \
         debugger features to help maintain context when #\"))))(Secondary((id \
         6367d88b-7b62-49be-bf2c-50268b942f93)(content(Whitespace\"\\n\"))))(Secondary((id \
         a484cb1c-8022-4297-a959-e03345bb47df)(content(Comment\"# navigating \
         between different probed expressions, which may #\"))))(Secondary((id \
         8ca20958-4f2f-4acb-ac31-d6c6e3b31d5a)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b4d08b3-bdcd-4e13-8433-3fa10801a5a7)(content(Comment\"# take on many \
         values across nested or recursive functions. #\"))))(Secondary((id \
         19fba9a6-c6ac-41db-aac8-3852fb65e084)(content(Whitespace\"\\n\"))))(Secondary((id \
         9b0ab506-21c1-464f-8d6b-7ec85201bec5)(content(Whitespace\"\\n\"))))(Secondary((id \
         735f2e32-b0f3-4774-ada6-25a55ab8c2e3)(content(Whitespace\"\\n\"))))(Secondary((id \
         972bd3bb-2fb5-4d7c-bf74-9aa666a61243)(content(Comment\"# TUTORIAL \
         #\"))))(Secondary((id \
         82b2034d-e3eb-4aef-abe4-5a8d78c4faf6)(content(Whitespace\"\\n\"))))(Secondary((id \
         402ec69d-a931-46ec-ac35-a448694fb8a8)(content(Whitespace\"\\n\"))))(Secondary((id \
         7a2eb76e-519a-4f9f-96e8-599b59145b99)(content(Comment\"# The \
         expression 10 * 10 below has a probe.  #\"))))(Secondary((id \
         93986545-6f18-4943-bf33-23039592245e)(content(Whitespace\"\\n\"))))(Secondary((id \
         1751fbb0-ddc7-4bed-ade6-7a796d1fc3e9)(content(Comment\"# Its value, \
         20, is shown in a cell to the right. #\"))))(Secondary((id \
         62570750-2a31-4d0c-935e-40fc14498261)(content(Whitespace\"\\n\"))))(Tile((id \
         006569a3-3803-46a7-bdb5-a1d01417e807)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         4dfbc80d-d60e-41a9-8185-ef4392076ab8)(content(Whitespace\" \
         \"))))(Tile((id \
         e7a40391-d443-4116-aff2-ec08c5125169)(label(chips))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5c4587c0-069d-4db9-9a71-97a5598f7376)(content(Whitespace\" \
         \")))))((Secondary((id \
         811fdb0d-ce3c-46d6-b551-28e1d65ae063)(content(Whitespace\" \
         \"))))(Tile((id \
         172bdcaf-2943-4191-82d4-8683b95aa524)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c2c6db07-70f0-46cd-8fc4-8284240d17e0)(content(Whitespace\" \
         \"))))(Tile((id \
         d3b7b215-9ea8-4eda-8df8-f229839f9c0a)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         36d7fd27-d1bd-4212-9d2c-d7214882484e)(content(Whitespace\" \
         \"))))(Tile((id \
         aec47582-4988-46ee-b787-07a3b8ce1ba7)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd33c8c1-3c3c-44d2-96fd-e2be4d984ad2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eac31e2a-4a51-42e7-a4b7-5b34926a464b)(content(Whitespace\"\\n\"))))(Secondary((id \
         fb3694b0-1937-48af-b1cd-93544d09b5f4)(content(Whitespace\"\\n\"))))(Secondary((id \
         9ccb4cd5-6ced-4910-88a4-9d4c09b9e452)(content(Comment\"# To probe the \
         below expression, put your caret to #\"))))(Secondary((id \
         e232e5ed-6fd2-43bd-b7cf-a02849d3c075)(content(Whitespace\"\\n\"))))(Secondary((id \
         11a05fb9-982a-4e31-8ebe-1cc0b48b8eae)(content(Comment\"# left of the \
         `(` and either press ctrl/cmd-E or #\"))))(Secondary((id \
         d9d1c95c-e057-4c0a-8def-c6863aeb8adf)(content(Whitespace\"\\n\"))))(Secondary((id \
         22d43412-e7c8-41a1-9708-7e91829642d8)(content(Comment\"# \
         context-click and select `Add probe` from the menu. \
         #\"))))(Secondary((id \
         a531f8a3-4c4f-4730-8308-3ea41a66c7bc)(content(Whitespace\"\\n\"))))(Tile((id \
         5440da92-56c9-4e2f-987b-372136a47be6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         e3997191-d93c-45f9-a729-4f46db300621)(content(Whitespace\" \
         \"))))(Tile((id \
         bd286fc8-e90b-4032-a875-495f2b19ac3a)(label(mult))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         984a020a-2025-4dea-875e-415c7016dcb3)(content(Whitespace\" \
         \")))))((Secondary((id \
         d1f90f0e-fdde-4409-86b3-5b7c75ec2dc3)(content(Whitespace\" \
         \"))))(Tile((id \
         6f98d336-de10-4072-adc9-5d27cfa65ac9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         54b0a602-94a9-40f0-8773-a7a3ad098e62)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         7aed621b-6a81-4224-9d2b-8503100d6ea5)(content(Whitespace\" \
         \"))))(Tile((id \
         c1ee724f-f121-4ca3-a35d-6b640d337cea)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         9be93c93-e41b-4990-b8a5-99a63ba2577a)(content(Whitespace\" \
         \"))))(Tile((id \
         1ece00c6-0de9-4d0c-a599-df0cc3c3e3d1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a559109f-b5d7-4cfb-b1de-65cb77805cfe)(content(Whitespace\" \
         \"))))(Tile((id \
         d1e5e556-fd9a-4bb4-a863-e7769c1026ee)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8fbd6a1a-a69e-416c-85b1-6b8624abb1a5)(content(Whitespace\" \
         \"))))(Tile((id \
         b8e92942-dacc-4944-8baf-1c29fbe1ee72)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         1583925b-f937-493c-a4e8-a349159d6007)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         622003d5-19b1-45d4-9a09-42e5504e7547)(content(Whitespace\"\\n\"))))(Secondary((id \
         943f8e27-ee45-469d-bb34-f09c52e997bc)(content(Comment\"# The \
         expression should be underlined in green, #\"))))(Secondary((id \
         632da6bf-4d09-4b69-afc9-ff7d4d89c783)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1c50b8b-3695-4910-81e1-cb4b071f5686)(content(Comment\"# and a cell \
         reading `7` should appear to the right. #\"))))(Secondary((id \
         a45df68c-39bc-40db-a175-c150cb65e417)(content(Whitespace\"\\n\"))))(Secondary((id \
         717f8e5b-e226-4b55-add5-be51779d3a98)(content(Comment\"# The same \
         shortcut or context menu toggle removes it. #\"))))(Secondary((id \
         577d5c29-a070-4f48-97e6-a0e72e0ecf15)(content(Whitespace\"\\n\"))))(Secondary((id \
         df00bb73-1f28-42f9-8035-de40205e7d44)(content(Whitespace\"\\n\"))))(Secondary((id \
         ceee2c6c-5d8c-45da-b894-4715fef54664)(content(Comment\"# Click the \
         below cell (with value 140) to select it. #\"))))(Secondary((id \
         0dc25a05-f58d-4103-bf8a-545fe2dfd6f3)(content(Whitespace\"\\n\"))))(Tile((id \
         06920781-10c2-4270-b778-909ab0a68bfc)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         2427e549-0d87-42ac-beb4-7dd033e0ba86)(content(Whitespace\" \
         \"))))(Tile((id \
         ff20a732-d97f-4030-8ca9-7649b64877a6)(label(score))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d8893d71-dd63-4284-9240-a49628026e5f)(content(Whitespace\" \
         \")))))((Secondary((id \
         9824b0dd-c8c6-4292-a493-8ed80179b838)(content(Whitespace\" \
         \"))))(Tile((id \
         c8b7d92b-ad24-4aae-9c47-c034bf785e15)(label(chips))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0cf6d6a2-f12a-4bec-8ca1-8d08c4ef601c)(content(Whitespace\" \
         \"))))(Tile((id \
         0efa756f-4e37-4155-bf27-08b5ed56cb52)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f6efeadd-da77-45cb-8af6-3201f9d4231a)(content(Whitespace\" \
         \"))))(Tile((id \
         d081a70c-b2e1-4664-9977-05b044733da6)(label(mult))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6aefe524-21f8-49d0-8c48-ece377f4a0f1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2293a1fe-3ec5-45b5-8b12-65bd1da64051)(content(Whitespace\"\\n\"))))(Secondary((id \
         988c5a65-0da3-406b-84bc-2515583e4280)(content(Comment\"# Notice when \
         you hover over a selected cell, it #\"))))(Secondary((id \
         91aab7ef-f08d-4f5b-94b0-0b3b5eb549c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         099dff6d-300e-460f-a36f-1f7b99d665d5)(content(Comment\"# shows the \
         values of any contained variables. #\"))))(Secondary((id \
         7e89e089-1ccb-4828-8d51-8e0a0f95a325)(content(Whitespace\"\\n\"))))(Secondary((id \
         b58f44b0-cd73-46ad-a5b4-a3e810194208)(content(Whitespace\"\\n\"))))(Secondary((id \
         5bd50746-e730-408f-bbce-661aebc0d80e)(content(Comment\"# Probes only \
         have cells if the are evaluated. #\"))))(Secondary((id \
         477cdfce-2e6c-4af9-8537-7cb95a22b0f3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f8ba814f-ee6f-4d48-afc1-b4cfd9c4b8bf)(content(Comment\"# Below, only \
         the first case branch is evaluated. #\"))))(Secondary((id \
         32433c45-f472-4f9d-8f75-1b4bc62e9491)(content(Whitespace\"\\n\"))))(Secondary((id \
         ae5b382d-d135-463f-b1af-5fe45534de9b)(content(Comment\"# Hover over \
         the empty set symbol to see a tooltip. #\"))))(Secondary((id \
         7e171369-952e-488f-846a-6a1347740d02)(content(Whitespace\"\\n\"))))(Tile((id \
         4c2be59e-205e-4fd8-9c75-9d1cfdccac12)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5fc597f6-899e-442b-83fe-c80bbcdda01c)(content(Whitespace\" \
         \"))))(Tile((id \
         2c8e2a37-dfc4-48e5-81f2-22ab1e980c3a)(label(check))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bec0f440-a0ef-4fed-b76c-9909b4855659)(content(Whitespace\" \
         \")))))((Secondary((id \
         dc7162ea-e792-45e5-a752-56ffb25d6455)(content(Whitespace\" \
         \"))))(Tile((id 4f503406-2694-4c02-bcba-09b239413d1a)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b7f08986-448a-4fd0-baca-3e04828468f6)(content(Whitespace\" \
         \"))))(Projector((id 19f44172-480a-4b0b-a873-71c0586c7949)(kind \
         Checkbox)(syntax((Tile((id \
         9c3ef734-be4e-49cc-894c-821f93cc809d)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))(model\"()\")))(Secondary((id \
         1d23b0d2-abe6-4cb1-a2aa-10aa3aed1479)(content(Whitespace\"\\n\"))))(Tile((id \
         ae52ff7d-ae83-4a57-9923-a020cc58e93c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         409d0e62-09dc-4b5f-b9e3-389834d485e0)(content(Whitespace\" \
         \"))))(Tile((id \
         f37501f9-4875-4a4e-b57a-9e160a067e82)(label(false))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c04c0512-70ae-4df7-ab6d-11f856c9a9da)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e62fcb73-7f17-42fd-b4f0-a67d747b0216)(content(Whitespace\" \
         \"))))(Tile((id \
         e9d020c4-8a93-4569-bc8f-74d75d5a3b52)(label(\"\\\"checks \
         out\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5e255c12-316c-482c-ba82-f07d4b25b432)(content(Whitespace\"\\n\"))))(Tile((id \
         d9fef42b-5bd1-488d-9119-3707a7f191eb)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         d79b37b7-4ea4-4f36-8842-82040a5c4e0e)(content(Whitespace\" \
         \"))))(Tile((id \
         edda030e-a0a7-4576-851b-7ca877bec4b3)(label(true))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         e44c88ba-c100-4452-ab08-bedc68884c21)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         14a22550-9c7f-4aa5-ba4e-96b461329c34)(content(Whitespace\" \
         \"))))(Tile((id 5b81c870-29bc-4d7f-9d88-da87517a8a2e)(label(\"\\\"you \
         cheated\\\"\"))(mold((out Exp)(in_())(nibs(((shape Convex)(sort \
         Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df563e72-e7b2-4e47-8ba9-6bdebabf4e34)(content(Whitespace\" \
         \"))))(Secondary((id \
         0fc28f04-e92c-4a00-9fdd-9047f948cdbf)(content(Whitespace\" \
         \"))))(Secondary((id \
         3b6e13e8-905d-4577-beb1-37d8755c61e5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         e2652638-987e-4800-96d5-d6a756763c59)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         daf75aa1-1e79-400e-9c9b-49a670d0bf1a)(content(Whitespace\"\\n\"))))(Secondary((id \
         53deea2a-b386-4ed5-9939-05281f26fc55)(content(Whitespace\"\\n\"))))(Secondary((id \
         a1c8913d-8db9-4998-9139-0e26d047f403)(content(Comment\"# Probes can \
         be placed on expressions #\"))))(Secondary((id \
         a40356df-f43b-49b0-949e-344646e9a3ea)(content(Whitespace\"\\n\"))))(Tile((id \
         dff6e9a9-2dc3-40ef-a141-2b2223e36cb2)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         5c36c198-5c36-4e40-aa5a-63a87b75a8f2)(content(Whitespace\" \
         \"))))(Tile((id \
         921173d8-e432-42c8-98a3-0ff398fb365b)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         931cca80-f131-4c2c-9437-81f07077407d)(content(Whitespace\" \
         \")))))((Secondary((id \
         3a344fd2-6690-4cda-aee2-84b5e03c36eb)(content(Whitespace\" \
         \"))))(Tile((id \
         91a1033a-17c0-42d5-8392-422fd2a0aea5)(label(50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c420e13e-5031-4777-9497-50737f2a8c2a)(content(Whitespace\" \
         \"))))(Tile((id \
         b0b04629-16a4-4599-aed6-68f6512cb85e)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ffc791b-ec6f-42a6-96c0-a6e9ce38fc1f)(content(Whitespace\" \
         \"))))(Tile((id \
         0e460c44-e0fe-41f7-b101-ae06f88f22d1)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bcda270a-12a0-4656-9e81-1498b7d8d03d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5f8a6e55-3995-448a-a87b-c68971664ab6)(content(Whitespace\"\\n\"))))(Secondary((id \
         92855447-66e0-4a01-8385-d910d7923e75)(content(Comment\"# And also on \
         patterns (e.g. variables) #\"))))(Secondary((id \
         ddfbf36e-a7f2-4c0e-a242-035a40f26b46)(content(Whitespace\"\\n\"))))(Tile((id \
         475eab7c-4204-4bee-9d94-4bd5f203f192)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         adf5674d-a485-4dd6-b1df-220ad56adf19)(content(Whitespace\" \
         \"))))(Tile((id \
         89e1b3fe-c5e3-43d4-af0d-eb2dd0ae19d8)(label(pow))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         68dac833-3619-43bc-99d4-a789e9d3ad50)(content(Whitespace\" \
         \")))))((Secondary((id \
         12cf310a-95af-4c9a-bdac-2b79fdedcdd5)(content(Whitespace\" \
         \"))))(Projector((id 29ff2ba7-5a3a-4127-8792-5011e67c8540)(kind \
         Slider)(syntax((Tile((id \
         d210519e-bbef-4533-bead-1b02df92153f)(label(54))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))(model\"()\")))(Secondary((id \
         c68b62c4-fdd9-424e-b1fb-da8f0e4bb83d)(content(Whitespace\" \
         \"))))(Tile((id \
         5eca18e5-baf9-4722-9e55-19902492b7d5)(label(**))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 26))(sort Exp))((shape(Concave \
         26))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a2d04dc9-dcbb-4172-b108-ecce2293ba35)(content(Whitespace\" \
         \"))))(Tile((id \
         6b47fe8c-7d02-479e-a168-c5b4e616f907)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ab5d63cf-e3c9-4164-8d6f-b2de45abe885)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4a7afdad-6366-440f-9d4e-2f99431992c1)(content(Whitespace\"\\n\"))))(Secondary((id \
         66fbae4f-df72-4b8e-81af-10f7241c238a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7df94860-c5c7-41d1-ae8f-6836e1541bd9)(content(Comment\"# FUNCTIONS \
         #\"))))(Secondary((id \
         fbbd8731-4a6e-4654-8b5b-b3b09efa34fc)(content(Whitespace\"\\n\"))))(Secondary((id \
         2aaa45e2-0ccf-4e58-b584-5f070cdcf816)(content(Whitespace\"\\n\"))))(Secondary((id \
         21054f96-f766-47b1-b21c-67b400827281)(content(Comment\"# Because \
         functions can run multiple times, they can #\"))))(Secondary((id \
         fc3bee0b-e2eb-4e04-8b5c-1820d346412b)(content(Whitespace\"\\n\"))))(Secondary((id \
         8227dd21-51f2-45ff-917b-094052def70b)(content(Comment\"# have \
         multiple samples. Note the closure counts circles \
         #\"))))(Secondary((id \
         5d4436b2-30a7-4471-9e99-63736e7328b7)(content(Whitespace\"\\n\"))))(Secondary((id \
         ebe5aca2-8518-47fb-b91e-b1c2ae08df26)(content(Comment\"# are all 2, \
         indicating each probe was evaluated twice. #\"))))(Secondary((id \
         2db54711-dd5c-4f9d-9e43-7eb56c08950c)(content(Whitespace\"\\n\"))))(Secondary((id \
         94ab4eab-1d35-4d01-8592-af45e494f0bd)(content(Comment\"# Double click \
         on any sample to show all samples. #\"))))(Secondary((id \
         0be31dfb-e452-450a-a089-8fd1e2929f38)(content(Whitespace\"\\n\"))))(Tile((id \
         d66d804f-1ddf-4111-a639-1863b772012a)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a2389dee-aea4-45fb-b5cd-61eb5cf5f540)(content(Whitespace\" \
         \"))))(Tile((id \
         0e459c86-0a71-4aff-8b09-0464eca8113b)(label(celsius))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         5711e4e4-7ecf-485a-ade5-f54925f53d8c)(content(Whitespace\" \
         \")))))((Secondary((id \
         13c54250-0aa8-413d-9863-57295a2ec8ad)(content(Whitespace\" \
         \"))))(Tile((id 7e548ec1-3abb-43d0-bc60-9c99d881a978)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         5759a2e7-0974-48ee-89df-b31b39f04c09)(content(Whitespace\" \
         \"))))(Tile((id \
         651e330f-0516-4ee7-82e7-674452735f67)(label(farenheit))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         8948cc6d-8b90-4171-9149-f5bacc299c44)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3562eb78-d793-4871-b244-1f2bebc5cd3e)(content(Whitespace\"\\n\"))))(Secondary((id \
         db499861-08c3-4e0f-9932-b1aa59a48b06)(content(Comment\"# Click to \
         select the cell above reading 72.5 #\"))))(Secondary((id \
         ff1e48c0-9972-491e-9627-e951796c876e)(content(Whitespace\"\\n\"))))(Tile((id \
         f6ae7e09-e830-41cf-9070-c445038960c6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f34416a9-5b59-4938-bd24-5b08f3e85f72)(content(Whitespace\" \
         \"))))(Tile((id \
         7834275c-1eb3-419f-a35c-c00b9e3d042c)(label(diff))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a6f93338-371d-4654-b0b4-3e8214b29060)(content(Whitespace\" \
         \")))))((Secondary((id \
         151d8056-80f4-40cd-ae1d-2e1c2605af36)(content(Whitespace\" \
         \"))))(Tile((id \
         8743d90b-057b-454b-9cd2-efc528bbfad0)(label(farenheit))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3e9fcbee-73c2-4e57-b9e8-5a6ef532e5b1)(content(Whitespace\" \
         \"))))(Tile((id \
         9e144906-34c2-4548-bab0-66d2e0eb673e)(label(-.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3413adf5-c054-4d88-8349-d85c7c61a6f8)(content(Whitespace\" \
         \"))))(Tile((id \
         4f844e4e-c2ed-4899-b420-566eaff42391)(label(32.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df0e9443-ecb6-4a8f-bb14-59de6caedfeb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2dc99456-c175-452c-ba81-190c2cc197a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         31e4ebee-932f-4a02-8e0c-0a4874e118c5)(content(Comment\"# This \
         highlights cells below corresponding to the same \
         #\"))))(Secondary((id \
         c20b3644-182c-4e7e-b730-84648b09a19e)(content(Whitespace\"\\n\"))))(Secondary((id \
         9dd5f54e-8cb3-42b9-bda3-c817d2378d1e)(content(Comment\"# function \
         call: the cells reading 40.5 and 22.5) #\"))))(Secondary((id \
         d57ecfc6-91ca-4a04-8024-a9cf2090888d)(content(Whitespace\"\\n\"))))(Tile((id \
         710fd071-bb97-4bc9-b525-a3457232e097)(label(5.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8f439e9a-f81d-4e37-90f6-6aea586bc3aa)(label(/.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         2b177ac5-c482-47fd-be79-14de56b3869e)(label(9.))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         c5ab3e1c-f4ed-4363-9a6e-bd46a17ffc71)(content(Whitespace\" \
         \"))))(Tile((id \
         a1ce4de0-a4ee-4777-ac2a-058f31efd19c)(label(*.))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5339f44a-cecf-43af-b654-f20e491afe52)(content(Whitespace\" \
         \"))))(Tile((id \
         97c362a8-42ff-4260-b858-575541d78148)(label(diff))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1dbbfdb0-ac13-4e47-85ad-afc73c6b5bbb)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b270e294-6b50-402c-bded-cd76d83078c3)(content(Whitespace\"\\n\"))))(Secondary((id \
         f5819643-dc82-47d2-9a1a-3237ccacc235)(content(Whitespace\"\\n\"))))(Secondary((id \
         224c9359-19aa-41ab-ab94-b3a79e320d74)(content(Comment\"# It also \
         accents the text of the sample of the #\"))))(Secondary((id \
         4b89a68b-bec3-4b02-beca-cfcfe67bc8ec)(content(Whitespace\"\\n\"))))(Secondary((id \
         e4562a67-17c1-4a76-9aa0-ac8e8282f5f4)(content(Comment\"# relevant \
         function call site in pink#\"))))(Secondary((id \
         599b74e0-767f-40b7-839d-39fd319ce206)(content(Whitespace\"\\n\"))))(Tile((id \
         063e99eb-b4c2-4d87-b727-7b0d417615d3)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d000d592-7801-4cf5-9d27-d65feb2150aa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         227a393e-8c95-4779-9441-663958b2b5e7)(label(72.5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         bcab15ba-fab9-4f70-ada7-275cbd51bcae)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         640bae73-27cb-4491-bdba-8fe885623251)(content(Whitespace\"\\n\"))))(Secondary((id \
         bd2cca8f-df85-4cee-8b9f-e8f2c4b4f7d7)(content(Comment\"# Now select \
         the cell above reading 22.5 #\"))))(Secondary((id \
         da0edbfb-318d-4c3f-9d8d-4c3bbfffd5ad)(content(Whitespace\"\\n\"))))(Tile((id \
         ba999218-6132-4083-a6e1-92867d636ee8)(label(celsius))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f7f45c1c-d78c-45ee-b717-e30cbdf12f60)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f698dd3d-fb14-4163-ba75-934f29d41eda)(label(103.1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         9b64ecc7-a5d8-48c5-865c-8ce2a52ecf50)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         525cc13c-138c-4f67-b440-194997751853)(content(Whitespace\"\\n\"))))(Secondary((id \
         c78252af-a266-4d53-99e1-f1d5192921d9)(content(Comment\"# Note the \
         72.5, 40.5, and 22.5 are no longer green-highlit \
         #\"))))(Secondary((id \
         42746f7c-8b8a-4270-8a33-711b03a416d9)(content(Whitespace\"\\n\"))))(Secondary((id \
         2f34d583-556a-44a5-8998-4c95d896880a)(content(Comment\"# as they are \
         not part of the same call as /the expression/ #\"))))(Secondary((id \
         c32fecff-0d86-436e-a947-486dd2e1a153)(content(Whitespace\"\\n\"))))(Secondary((id \
         85b60c20-e2fe-452c-a6ba-8515afc3ad33)(content(Comment\"# \
         `celsius(t1)`. However, they now have blue text, indicating \
         #\"))))(Secondary((id \
         60be4133-9597-4853-8cf9-c44e2d8df6f5)(content(Whitespace\"\\n\"))))(Secondary((id \
         16c8a403-e229-41be-84a0-5260d6344b92)(content(Comment\"# they are \
         below that function call in the call stack #\"))))(Secondary((id \
         e0fcc7f3-c9f5-4bf6-882a-02f56e15f03c)(content(Whitespace\"\\n\"))))(Secondary((id \
         8f9ddbda-d509-4d83-82da-a7f8593a9863)(content(Whitespace\"\\n\"))))(Secondary((id \
         5432af87-84cc-4c02-a7e2-e9422255ab9c)(content(Comment\"# BRANCHING IN \
         FUNCTIONS #\"))))(Secondary((id \
         16b11da5-a319-465e-8ee8-52064a79601c)(content(Whitespace\"\\n\"))))(Secondary((id \
         017c933c-ef7e-4c0f-b1a1-173eddb3b1d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         ec8efc02-8663-4508-8e2b-eae274d9a1bc)(content(Comment\"# Select `6` \
         then `5` then '4' below: #\"))))(Secondary((id \
         09a2aa54-be95-4345-b7a1-c14c88d3b9b2)(content(Whitespace\"\\n\"))))(Secondary((id \
         c90e561d-aae8-4123-97ca-a996d86df1fd)(content(Comment\"# (If there is \
         a no-enter sign instead of a sample, #\"))))(Secondary((id \
         e8a86eb4-7b03-43be-9466-797752ddb53d)(content(Whitespace\"\\n\"))))(Secondary((id \
         ac8bd400-7431-474c-977f-34182b84f392)(content(Comment\"# this means \
         that the sample cursor is aligned to #\"))))(Secondary((id \
         62a103a9-af7a-44e4-bc79-50cdf6f8f30c)(content(Whitespace\"\\n\"))))(Secondary((id \
         9fc137ba-b7bd-4a01-b97d-d3d31871b4d8)(content(Comment\"# another \
         function. Just click on the sign to realign it) #\"))))(Secondary((id \
         389da843-1e2c-4d42-9aaf-7990d8bea437)(content(Whitespace\"\\n\"))))(Tile((id \
         a3d874ea-6900-41e9-b574-c37e85155bef)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         c8c0324f-cc50-4834-9681-0818ee96c900)(content(Whitespace\" \
         \"))))(Tile((id \
         62cda317-93d5-4514-bbb5-f66940105001)(label(cases))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))((Secondary((id \
         137e1cb2-3235-4062-ae38-95069d42703f)(content(Whitespace\" \
         \"))))(Tile((id 78625d8c-f4c3-40d5-9a92-571d5028eaa1)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         9744a978-af9b-4432-9841-577fc7fccc33)(content(Whitespace\" \
         \"))))(Tile((id \
         07ab5fcd-568f-42cd-bf18-2e9d07ab0537)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         309d8110-7ba2-4015-918e-1d9d04b49bf9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         832153ea-b533-4bce-8cd3-1bf6d6ee7c78)(content(Whitespace\"\\n\"))))(Tile((id \
         d8fbd702-848a-4baa-998e-7cbeeae6b9da)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ccdc6410-a2e6-47a2-a72b-ae1f42086a86)(content(Whitespace\" \
         \"))))(Tile((id \
         9343a15d-b67d-484c-8d7d-496fd30bf4e5)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         bf310cac-6e69-4e92-a267-07b887502712)(content(Whitespace\" \
         \"))))(Secondary((id \
         b209625b-b0a6-439e-a759-91ad5d54716f)(content(Whitespace\"\\n\"))))(Secondary((id \
         43a6fbe4-6013-4fe0-8f8c-9db113beff03)(content(Comment\"# Note how \
         each activate exactly one branch below: #\"))))(Secondary((id \
         a836188e-8381-4907-b00d-7e0900823ed4)(content(Whitespace\"\\n\"))))(Tile((id \
         4d31d7f0-3b73-4a93-ad5f-adf7d224fc47)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         755585ad-9fa6-48a1-8818-d9f022626fe5)(content(Whitespace\" \
         \"))))(Tile((id \
         d7d807ac-eb36-4d0d-b2b3-906072a19793)(label(4))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         00f0f78c-3ba0-4658-b084-1f5784c7bf57)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4cd03cdc-8deb-4a04-853e-110c3674b817)(content(Whitespace\" \
         \"))))(Tile((id \
         a3804efc-4b42-4754-ab62-c1bb98fbf05d)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1bc65721-4d6f-4123-be9e-9ebca17cab4e)(content(Whitespace\"\\n\"))))(Secondary((id \
         f773f445-fd09-4aea-b65f-dc137b55eff5)(content(Comment\"# Select the \
         `5` above and then the `false` below: #\"))))(Secondary((id \
         3326bf20-20cb-4988-b905-13d38ff21d3f)(content(Whitespace\"\\n\"))))(Tile((id \
         4f550021-4e40-45e0-96f2-a5ecb13a63d2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3114bc94-2841-441c-9593-1e853d42e693)(content(Whitespace\" \
         \"))))(Tile((id \
         75025956-d941-477c-93de-07cf06749440)(label(5))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         841d49ed-247e-49b0-9e1c-3bd6895ab5d9)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2316601c-99a4-4c2c-90cc-6226efa57b9c)(content(Whitespace\" \
         \"))))(Tile((id \
         7f3051cc-f801-45f1-88a6-70a2388d647a)(label(false))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0dad2525-110f-42a0-8361-e971b6066b11)(content(Whitespace\"\\n\"))))(Secondary((id \
         9488400c-1109-47c1-b9cb-efc4569aa6a4)(content(Comment\"# Note the \
         same things are highlit as both cells are #\"))))(Secondary((id \
         b7c3213c-8604-456d-94bd-9cf204a86c34)(content(Whitespace\"\\n\"))))(Secondary((id \
         196de158-8df6-4a44-ae4d-3a327d770815)(content(Comment\"# from the \
         same call to cases#\"))))(Secondary((id \
         8f6f5fa3-cb12-451a-926f-9e488736908c)(content(Whitespace\"\\n\"))))(Tile((id \
         81611ffb-755d-4348-b92b-7695271031d7)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         8cab307e-6fd7-4fb8-9b26-df2ff7756989)(content(Whitespace\" \
         \"))))(Tile((id \
         120d1804-8b46-4eeb-a8d9-41c270b8b32b)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f67a30c9-da6e-4527-8077-f9e27171b7a6)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ef469718-30b0-4c45-a590-e57603c39454)(content(Whitespace\" \
         \"))))(Tile((id \
         78cd744a-e1c2-4d7d-929d-5185369a3121)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1d14ac02-4c85-4aea-ac13-d55ae2898e6a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         68b010db-8cef-4959-9bcc-00f0458e21f8)(content(Whitespace\" \
         \"))))(Secondary((id \
         85e0d78b-6be7-4844-a9d4-19420848b53d)(content(Whitespace\" \
         \"))))(Secondary((id \
         b4c38bac-d9b7-45c7-9812-a56f1971a000)(content(Whitespace\" \
         \"))))(Secondary((id \
         8ccc587d-3d5d-4bcf-a8f6-87ca6373e2e4)(content(Whitespace\" \
         \"))))(Secondary((id \
         39e2b2a4-78bd-4bc3-aeda-d0e4ce2cddfb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         147ea12e-b1f6-4bc4-ad26-61c0516af5fe)(content(Whitespace\"\\n\"))))(Secondary((id \
         8b1f37fc-59ed-459e-a165-91f9f1fbb77e)(content(Comment\"# Select \
         `true` below and then the `4` cell #\"))))(Secondary((id \
         d27691a9-564a-480a-a276-35034a492349)(content(Whitespace\"\\n\"))))(Secondary((id \
         9986bf81-ddb2-4345-84c6-6941904705c6)(content(Comment\"# for the \
         argument x to `cases` above. #\"))))(Secondary((id \
         30b75009-4ad4-4a22-a3a3-24ba3244e23f)(content(Whitespace\"\\n\"))))(Tile((id \
         bf649300-4c11-48b7-a66a-fd1c5c52714c)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5fa7c308-8963-4635-99a1-a453415c4fdb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c6be0f5a-acbe-4971-9eef-e16aec67ac8b)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         75d4cbe8-cada-438c-a160-5ebd28f6d24c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         060f9dfc-0f05-4ead-afd9-7f19628e6e7c)(content(Whitespace\"\\n\"))))(Secondary((id \
         c0e810c2-47b4-427f-bb79-3bc0d9472eee)(content(Comment\"# Note how the \
         same cells stay indicated, but the kind #\"))))(Secondary((id \
         9a773578-eb77-4a93-b4dd-b2dc02273b95)(content(Whitespace\"\\n\"))))(Secondary((id \
         086c5666-f90b-4ee4-9f36-ed155dd5fe8a)(content(Comment\"# of \
         indication changes. The `true` below the `4` above \
         #\"))))(Secondary((id \
         7c514b1f-ab94-46a6-a32a-d9215406b819)(content(Whitespace\"\\n\"))))(Secondary((id \
         78fba8a0-ddc6-4085-8f6e-c41981c10cee)(content(Comment\"# goes from \
         blue text (created by the cases(4) call) #\"))))(Secondary((id \
         46216298-e76c-4643-b972-bebffe6f8021)(content(Whitespace\"\\n\"))))(Secondary((id \
         d221dc9b-d5f0-47c8-97ab-694b49b6d823)(content(Comment\"# to green \
         highlighting (part of the same call as `4`). #\"))))(Secondary((id \
         2e402cac-bacc-494f-860e-6e11e35c784c)(content(Whitespace\"\\n\"))))(Secondary((id \
         cfee8651-fa93-430e-8798-b6e35d9db223)(content(Comment\"# The formerly \
         selected lower `true` now has pink text #\"))))(Secondary((id \
         552a93ad-467e-458e-8a0f-9d446ce56265)(content(Whitespace\"\\n\"))))(Secondary((id \
         30cfb10a-fe26-462f-bf4f-61710af38216)(content(Comment\"# since it \
         indicates the call where indicated `4` lives. #\"))))(Secondary((id \
         a9673569-97cd-455f-8d1b-581bbc6f650b)(content(Whitespace\"\\n\"))))(Tile((id \
         c9d163a4-0de5-42a9-8a0b-f418aa4c3987)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9128323f-f471-4cd3-9319-8bc9c45b415c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         d6764b34-8733-473d-b0c7-af73cf1eb70b)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         b756ef70-7081-4703-8b27-f170eae10006)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a8de1777-0188-4c07-ab8f-e53a4c727b2d)(content(Whitespace\"\\n\"))))(Tile((id \
         5d9b73de-6acd-4b19-a275-cdc3131f4ad2)(label(cases))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a060f95d-3d11-4561-a1c0-3fcd51011847)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         b0e56229-d75e-4c15-ba84-8ddf7ab6ecd6)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         53f7ccdb-47f8-4dfe-b5bc-2c0946d7545d)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ff05ad2a-5483-4d4d-bb7b-6613997aa4da)(content(Whitespace\"\\n\"))))(Secondary((id \
         768c032c-07f0-4fe3-8ace-5206c49567d4)(content(Whitespace\"\\n\"))))(Secondary((id \
         d54725ff-96a1-435c-b671-82fdf85ff80b)(content(Whitespace\"\\n\"))))(Secondary((id \
         40439dad-ced0-4624-bf73-3f6a2ca6e870)(content(Comment\"# FUNCTIONS \
         CALLING FUNCTIONS #\"))))(Secondary((id \
         2a750f4a-4c89-40ed-99c6-0eb0fa5c3d82)(content(Whitespace\"\\n\"))))(Secondary((id \
         8d9516b4-1266-4529-960f-b23c71ad56a3)(content(Whitespace\"\\n\"))))(Secondary((id \
         c89a79c5-e8d2-454e-8250-50696c2774e2)(content(Comment\"# Select `9` \
         below. Note four cells below become pink #\"))))(Secondary((id \
         614c11cd-ddb7-404c-981b-22031568138c)(content(Whitespace\"\\n\"))))(Tile((id \
         5fcb3ee6-d093-4b23-9d64-6050dbd5b39b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7a6023ba-b66b-4cbf-9ea3-a500a16cefdd)(content(Whitespace\" \
         \"))))(Tile((id \
         c58faca0-1c17-4074-95ad-e1430998aab2)(label(fourth))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         35387ad5-404d-455a-b481-8fd752345f6d)(content(Whitespace\" \
         \")))))((Secondary((id \
         1344bb25-6faa-4dd7-a00f-c3407988ffc0)(content(Whitespace\" \
         \"))))(Tile((id 963f1a51-bd01-4bc0-8f4e-0e1b3a7c42b6)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         47747f18-efa3-40cf-8e70-1c5f391ee69a)(content(Whitespace\" \
         \"))))(Tile((id \
         c2f790bf-8cab-45c8-bda1-80da9875edff)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c9d5760d-c97f-43af-bf42-3af820b35d3b)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f005f7e4-b661-464b-b477-783a5f787ac4)(content(Whitespace\" \
         \"))))(Tile((id \
         596143b5-4774-4295-a036-0afd8b67b269)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5c31d559-6560-4694-94ef-a22d1fe19e0a)(content(Whitespace\" \
         \"))))(Tile((id \
         359b9fa3-3995-4a6d-990f-6f73d157a0b0)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c15f76e3-ec08-4d74-8590-a80e65380655)(content(Whitespace\" \
         \"))))(Tile((id \
         4184660a-a985-4fb8-8bc8-06eb1acd54fd)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f01a166b-07b1-40f2-a7ad-c5746657d83f)(content(Whitespace\" \
         \"))))(Tile((id \
         821400fa-ef2c-483e-a9eb-472ac06ba2b7)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         66b3acce-e67f-4ca8-83c1-1f5464255a04)(content(Whitespace\" \
         \"))))(Tile((id \
         2a093840-4954-4907-b325-8760b74da899)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         804a1da7-6329-477a-83a1-49512c82d084)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         4290a877-39c1-4757-9141-74d7d62e9c76)(content(Whitespace\"\\n\"))))(Secondary((id \
         4d3211ce-3ac2-47f7-98ee-a2cde7f7ddba)(content(Whitespace\" \
         \"))))(Secondary((id \
         e9a7ea4f-f4d8-4bdc-a1bc-a03a71ebc792)(content(Whitespace\" \
         \"))))(Secondary((id \
         038d3cd3-7213-4001-adaa-dcd5de3f7c1a)(content(Comment\"# This is \
         because they represent function calls #\"))))(Secondary((id \
         58c1b747-f13b-4aad-a142-05b2b2f9fc99)(content(Whitespace\"\\n\"))))(Secondary((id \
         a3226c8c-6659-4265-b2bc-59cd23d1d8ed)(content(Whitespace\" \
         \"))))(Secondary((id \
         d5ea4be0-bd8d-4da2-9679-1d73faecf051)(content(Whitespace\" \
         \"))))(Secondary((id \
         0d1966a8-eee6-4985-a83c-f08cc0a5bb24)(content(Comment\"# above the \
         `9` cell in the function call stack. #\"))))(Secondary((id \
         6bd6ba2c-4467-49b7-b54f-59bec9af4f99)(content(Whitespace\"\\n\"))))(Secondary((id \
         6349a117-5a34-4009-87e8-bfa258986c1c)(content(Whitespace\" \
         \"))))(Secondary((id \
         a32f9408-b0e0-4612-b53f-388b6de7711d)(content(Whitespace\" \
         \"))))(Secondary((id \
         553369b4-cd7f-445c-90a1-2c88d6f46a6a)(content(Comment\"# For example \
         32 below represents the call producing `9`.  #\"))))(Secondary((id \
         aef22e8d-5e3c-4500-b360-faa2e120523a)(content(Whitespace\"\\n\"))))(Tile((id \
         5a7713b0-f5f9-4a36-b0de-acdca6c4a955)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f8896a9f-81f1-40d3-a7d5-1eb88ee74c9a)(content(Whitespace\" \
         \"))))(Tile((id \
         45c9c241-b4b6-43fc-bbca-2ef8a3e7a781)(label(third))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3d16cafa-dc3d-4887-91f5-0d31a9e304eb)(content(Whitespace\" \
         \")))))((Secondary((id \
         3d1a2a07-1fed-4f2c-9fae-07cb97f0af01)(content(Whitespace\" \
         \"))))(Tile((id a0f9184d-0551-4316-bd83-1b1d283e573a)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         004cb4d8-9a73-4ebb-a0c0-ba3c3bb72f5b)(content(Whitespace\" \
         \"))))(Tile((id \
         34397eb1-003f-413b-a2dc-90947e119db3)(label(t))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         daaab4eb-ebea-433d-bd24-211a3632bb4d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6b6ec47e-c639-4eec-947d-ec3f17d6ba1b)(content(Whitespace\" \
         \"))))(Tile((id \
         fbe547fa-0821-4aa1-aa41-9a924b36ffa5)(label(fourth))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7c187c97-da6d-48e1-a539-fa6e11740d59)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         c2eed234-8d47-49d0-ac67-30a56dc26e2e)(label(t))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         0d41c8dc-9ca0-4b2d-9fcc-0a7b23e2779b)(content(Whitespace\" \
         \"))))(Tile((id \
         806bdb47-ae18-476d-a692-3e0498e8bd7a)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         64171841-2c35-4dd0-9212-b05b7491d075)(content(Whitespace\" \
         \"))))(Tile((id \
         f0b0a84b-5ad4-4571-bb39-af96d3cdc9f8)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c47c0924-a31b-4181-acb2-930895220347)(content(Whitespace\" \
         \"))))(Tile((id \
         c87da7d4-da78-4b35-9b1a-aeee0b9c8058)(label(/))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         65bb9399-6c89-47ab-a899-44556cd822af)(content(Whitespace\" \
         \"))))(Tile((id \
         03bcbfae-09c1-4dda-9a91-5bc70adf0d61)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         46c0ec3d-302b-4a1a-849a-6d5e1d219f0d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5726d58c-1c58-4487-9a86-4b34878bf101)(content(Whitespace\"\\n\"))))(Secondary((id \
         91f64447-8927-402b-98d8-b68cb7d3f51c)(content(Whitespace\" \
         \"))))(Secondary((id \
         c1a72b2d-a4ea-43c1-b4c3-9a2a544bda51)(content(Whitespace\" \
         \"))))(Secondary((id \
         1c7b9013-97b6-44ff-bd22-fd9e13b56aa4)(content(Comment\"# Now, select \
         `32` above. Note the 9 now has blue text. #\"))))(Secondary((id \
         39ec2962-5fa4-4b95-bbb2-f54fadad69d3)(content(Whitespace\"\\n\"))))(Secondary((id \
         a9720bef-9c87-4c5a-8935-caaf2ec29e1c)(content(Whitespace\" \
         \"))))(Secondary((id \
         3033c663-3efd-4c89-864a-f617427a2de0)(content(Whitespace\" \
         \"))))(Secondary((id \
         ce96fc80-8188-405c-87ff-7b7d3508c304)(content(Comment\"# This \
         represents that it is below the `32` call in the stack. \
         #\"))))(Secondary((id \
         855e21a9-2266-45fe-ac25-3b13599e8d69)(content(Whitespace\"\\n\"))))(Secondary((id \
         626a6b7a-2e48-4d0b-b7fc-ac90b0d0b988)(content(Whitespace\" \
         \"))))(Secondary((id \
         8edf82ca-26e2-4695-b30e-7252c0e44d71)(content(Whitespace\" \
         \"))))(Secondary((id \
         d2e142ff-8c03-4026-ae0d-4ccb4226d6b9)(content(Comment\"# Now select \
         `10` below, which is a call to `third`: #\"))))(Secondary((id \
         02850bde-af52-45e4-9ca5-4bd7ca65c16f)(content(Whitespace\"\\n\"))))(Tile((id \
         b3a15572-75d2-47c3-8723-a0f11d4ecf66)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         b972e899-4586-498f-bb25-07c0c5c710f2)(content(Whitespace\" \
         \"))))(Tile((id \
         b196a30a-611c-45c9-b579-3515b0e21500)(label(second))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         852fd599-0089-4c7c-bfb7-8d041263f5ea)(content(Whitespace\" \
         \")))))((Secondary((id \
         8606a687-5b42-412b-adc8-a378ab9a22ae)(content(Whitespace\" \
         \"))))(Tile((id 1d52d497-2b8b-4388-8e7b-4c34e05439d2)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         05e04410-664b-481d-a0e4-414339516399)(content(Whitespace\" \
         \"))))(Tile((id \
         e854066b-064d-4f1f-8d72-7c51840f4f8e)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         84e38b82-9f4e-4d89-8da1-4902570e95e1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f3348af4-bd62-4f1d-b05a-b097a316920f)(content(Whitespace\" \
         \"))))(Tile((id \
         450d1d93-1608-4394-b805-c54eea72fa08)(label(third))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5dee61aa-29d7-4f1f-a037-705dc2c54ffa)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         cbe9a1d1-d573-4866-b0f3-d08eba87ac4a)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         dd136148-df72-4195-80d1-67a46b279c27)(content(Whitespace\" \
         \"))))(Tile((id \
         19f55af4-019d-4169-bbf5-862360404bfc)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         238f7e44-8821-4327-9207-eb941c256e6b)(content(Whitespace\" \
         \"))))(Tile((id \
         b6a63d75-2cd0-4113-a0a6-f5222b825d1b)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c257d685-5c4e-4a12-88a5-af2ded58f4fc)(content(Whitespace\" \
         \"))))(Tile((id \
         b4b86da3-8306-4064-8a7d-83c6ba3a20e2)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         de93120f-f9b2-45be-a2b4-7a7f70b08f94)(content(Whitespace\" \
         \"))))(Tile((id \
         75bde0e7-b471-450d-93e5-27668dee3981)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         21a2e940-1f28-4318-a985-a0bce8e8265a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         6ea5ffea-2ceb-4067-a000-d5bc7ea8a870)(content(Whitespace\"\\n\"))))(Secondary((id \
         90c5a969-a66c-4ae0-96d3-221aa3f4dd8b)(content(Whitespace\" \
         \"))))(Secondary((id \
         381ce6c6-1b50-41fb-95f0-ce203a190bd8)(content(Whitespace\" \
         \"))))(Secondary((id \
         4e0fae6b-5ab3-4064-89f9-38eb8497441f)(content(Comment\"# Note that \
         `9` and `32` both have blue text as the are below in the stack. \
         #\"))))(Secondary((id \
         af872b34-45fe-459b-988f-51f9eca504e2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8c821a6e-12ce-48b2-9172-d1f8e4c570b3)(content(Whitespace\" \
         \"))))(Secondary((id \
         d229cc61-0f24-4a71-9b9d-2cc584fea432)(content(Whitespace\" \
         \"))))(Secondary((id \
         bcb7cfcb-148f-4e06-8c3d-917f323a5bf5)(content(Comment\"# Now select \
         12 below, representing a call to `second` #\"))))(Secondary((id \
         e786d993-6620-4fe4-9352-b3e885258048)(content(Whitespace\"\\n\"))))(Tile((id \
         31835cfe-5f05-4c4f-a59f-68a19a2ff1c9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         544d1cde-3e46-408d-b344-2e596c203fbb)(content(Whitespace\" \
         \"))))(Tile((id \
         41cd415e-723a-46c3-a83b-0cf56eda9801)(label(first))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f2389202-9273-458d-879a-0271d218512c)(content(Whitespace\" \
         \")))))((Secondary((id \
         b3f7b1f6-64a6-498f-bd69-2a4284ec366b)(content(Whitespace\" \
         \"))))(Tile((id 9fb71d58-0f90-4ce4-96af-2a39e19d770d)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         f551cddf-b18e-4f05-a0de-960232825e91)(content(Whitespace\" \
         \"))))(Tile((id \
         7411cf64-4e89-4263-8a4e-ee809135fcaf)(label(f))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         84f093cd-1047-4fb4-b0b5-b48db405f32e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         b7977a15-5a60-43f6-a8eb-8d3d9ee16e9e)(content(Whitespace\" \
         \"))))(Tile((id \
         65ac31f5-c74a-4480-9977-46225c0c3b01)(label(second))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5d45f74-f3a3-422b-9dd7-6bc3277c7d07)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         a1671a43-91c4-4d1a-af46-fceee755c55b)(label(f))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b6dd39ff-dcf7-41f0-aed4-72d7c2aceece)(content(Whitespace\" \
         \"))))(Tile((id \
         b3c747f4-0ea1-487e-9131-96f7158c12fe)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         4c707aad-4ade-487c-96ce-506042dd891c)(content(Whitespace\" \
         \"))))(Tile((id \
         dd3dc0b0-974d-4cd9-b55b-3fc390ea7f9e)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7717d2ae-1b63-4ca5-aff5-363d4f47a6d9)(content(Whitespace\" \
         \"))))(Tile((id \
         7f3d897c-751d-493f-8d3a-bbb20c72b2bd)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         48c58921-177b-425e-b0fb-e53e25492561)(content(Whitespace\" \
         \"))))(Tile((id \
         3e6d7db5-d9a7-44ad-a5d2-1c3bd3b75f23)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         3d2aeafc-8b7a-4cd9-a93f-812964453843)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f80132c7-57f4-4fa1-a878-861701f8cdf7)(content(Whitespace\"\\n\"))))(Secondary((id \
         c995093b-5ede-41ac-a16c-ab5ab4269600)(content(Whitespace\" \
         \"))))(Secondary((id \
         81b297d7-fb1c-4240-a484-d226d2f8779b)(content(Whitespace\" \
         \"))))(Secondary((id \
         a9eae439-7194-4cb4-b3f2-3a71b381145c)(content(Comment\"# Note how the \
         colors have changed. Finally, select `24` below, \
         #\"))))(Secondary((id \
         a15f9df2-538d-42e5-bac0-70b6fdd18ba6)(content(Whitespace\"\\n\"))))(Secondary((id \
         7d53e265-7af1-4048-a51f-1299c4425b50)(content(Whitespace\" \
         \"))))(Secondary((id \
         af7d7681-de90-48f9-ad3b-2312d417c855)(content(Whitespace\" \
         \"))))(Secondary((id \
         889fa168-1f7c-49b8-896e-9dc88e4525c6)(content(Comment\"# and then \
         again select 12, 10, 32, and 9 in turn. #\"))))(Secondary((id \
         10332d3f-55b7-46f1-81e8-cd62e25b8008)(content(Whitespace\"\\n\"))))(Tile((id \
         f465135a-2e05-4ad8-bfe3-864fa58ec097)(label(first))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         f3b1e905-4d75-4a04-862a-86378b3d67a1)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4472340f-7197-44aa-b60e-294471df253f)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Tile((id \
         ea0a8b4a-5322-41b2-a2d1-1ab3b64ec37a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b8c55d92-5061-44d4-a242-c98d496cc3ad)(content(Whitespace\"\\n\"))))(Secondary((id \
         25351c05-4b51-474f-b2d7-179220259dbe)(content(Whitespace\"\\n\"))))(Secondary((id \
         a54ddbd7-880b-4a14-8c20-68067e336d77)(content(Comment\"# RECURSION \
         #\"))))(Secondary((id \
         2b34d620-7e03-4de4-9715-0df2b1e2a34f)(content(Whitespace\"\\n\"))))(Secondary((id \
         6fd894b6-c222-4470-b0d7-569c35c63e2c)(content(Comment\"# Note how \
         cells are lowered/raised to indicate their #\"))))(Secondary((id \
         5e47b0f8-6a91-4c2f-84cb-89fe894c3f39)(content(Whitespace\"\\n\"))))(Secondary((id \
         76115717-1095-47d4-b3b6-c42af3473d2f)(content(Comment\"# relative \
         call stack depth to the selected cell #\"))))(Secondary((id \
         28258588-8e53-4159-8f54-9b61c2675d4a)(content(Whitespace\"\\n\"))))(Tile((id \
         a05916ad-3624-4774-ae49-9d7f734e4127)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9a0ddadd-f32e-4726-a33d-8c257a31db6c)(content(Whitespace\" \
         \"))))(Tile((id \
         b38acca9-a3e3-4142-be35-ad4be7030e09)(label(fact))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b7554d33-e76c-4c51-b3a2-580f7a521e87)(content(Whitespace\" \
         \")))))((Secondary((id \
         ae91f148-cb5c-4f91-a360-d4169896af97)(content(Whitespace\" \
         \"))))(Tile((id f812459f-3fc0-4978-88f5-f5ba3c1e5f77)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         4a98e756-4346-4387-b49d-d2f33a19fe28)(content(Whitespace\" \
         \"))))(Tile((id \
         3f99cf1c-0d62-4d90-823f-828bdadd936a)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         c592a767-4f00-4c9e-b82a-b7e89377c469)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         2d5b16df-0d62-4353-a208-4223ab4001f8)(content(Whitespace\"\\n\"))))(Tile((id \
         0d9c35d1-a1d5-405d-887d-6cb4575fd980)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         06e84521-7726-4ddd-b36f-13d43c85e5bd)(content(Whitespace\" \
         \"))))(Tile((id \
         696e4a96-af09-4afb-99a3-2ad4980d4d10)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ac57f3e9-200c-4092-8652-d2e4ce7615b5)(content(Whitespace\"\\n\"))))(Tile((id \
         9c0acf9e-eba0-4d4d-8d3a-cf87f2a1998c)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         9369f5ac-3fd5-46bf-ab3f-99610bb1184f)(content(Whitespace\" \
         \"))))(Tile((id \
         aaca9175-b0c9-4528-8434-9dc958aa1f81)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         4e24acef-bfb2-498d-bced-220626e61c05)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5c1116c2-e5d0-4997-a8bd-610dc8cfe48c)(content(Whitespace\" \
         \"))))(Tile((id \
         80345184-fcf0-4a56-b0e8-7536b8860a2b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df5fa240-b23c-45c9-be68-a3fbc3af4512)(content(Whitespace\"\\n\"))))(Tile((id \
         09b75965-093c-484a-8458-e019509815f0)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         2a6279d6-1801-4422-a098-3cd09cd5536b)(content(Whitespace\" \
         \"))))(Tile((id \
         b4ca95e6-6881-41e4-8389-3e7ee4147906)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f9c2467f-6670-4f32-9cd8-f37c7bc1973a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1d59adb0-2d9b-471d-b593-eceb045cf500)(content(Whitespace\"\\n\"))))(Tile((id \
         119a7538-2f36-4ce9-aba6-2f362e07642b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         a83b8d12-740b-4187-8689-cdfebb479b9e)(content(Whitespace\" \
         \"))))(Tile((id \
         edb5b6cf-5542-4673-a0b1-efb20bd87507)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         180195e2-a36a-4bee-8052-d02ad0b4c5c4)(content(Whitespace\" \
         \")))))((Secondary((id \
         94792d24-da15-48a0-9257-448216d8a174)(content(Whitespace\" \
         \"))))(Tile((id \
         ebe13798-7eb6-49e4-a46e-f2bfca9ce4b7)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bc4b825e-4e64-42b0-98e4-a7bda0856adb)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         29a33d2c-78e1-4c2d-a65a-557086da68c0)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ace04615-760a-4df6-a206-7ced153b99da)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         d8ebc601-c606-47db-8603-622aa9063617)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         8fd8e699-dcf9-46ca-9815-91656e2c0aab)(content(Whitespace\" \
         \"))))(Secondary((id \
         be63fbd3-e691-47bc-8af2-1e81a5c3e392)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1302cd52-2488-4659-996c-97ba84382482)(content(Whitespace\" \
         \"))))(Tile((id \
         ced89041-9fdc-4cc9-a478-7a44a86b6b5d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d6812ba-4198-4d30-8065-9b5fc31c1e2d)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         750b2d90-a387-4af4-b75c-735a9f164a60)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         b1b33347-8a38-4ce3-b221-017fc1c55bf5)(content(Whitespace\" \
         \"))))(Secondary((id \
         c2788d0c-4db3-45c4-a370-ee82685b8acb)(content(Whitespace\" \
         \"))))(Secondary((id \
         79507e6e-38ef-445e-8154-c9bb4c7be8ed)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         13925477-47d3-4a0c-89f2-554315b081ae)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         c917bf78-82fd-4016-b9f5-6d431b5ced8b)(content(Whitespace\"\\n\"))))(Tile((id \
         050b2ffa-b29e-4cf1-a76f-0f05fdb37c84)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         063671db-4d31-44d5-9df0-dbcfa84e4bd7)(content(Whitespace\" \
         \"))))(Tile((id \
         5e8ec844-4d2a-435f-8f7f-b4c57de9d01b)(label(fact))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78f0539e-defc-4306-903a-6b13fb17b098)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         7212068f-9a0b-41af-8f9e-aeca7d97e263)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         595ee5d9-a9a7-4a59-94a3-12f12bcbbf0c)(content(Whitespace\" \
         \"))))(Tile((id \
         ef2629ab-1144-423d-a81d-15530f81c07e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         5e919f10-6053-4d99-89c6-99dfdd1b80e9)(content(Whitespace\" \
         \"))))(Tile((id \
         01b31b79-5eea-4036-a0a0-ca7f9c034238)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1d3e9c78-7771-4f6b-ae37-49c2accb5121)(content(Whitespace\" \
         \")))))))))(Tile((id \
         31cf221c-f97c-4d00-b6fd-8f9ae4065b28)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         dfe2a32b-e736-47c8-8080-749b6582ea9a)(content(Whitespace\"\\n\"))))(Secondary((id \
         88ab1674-bd52-48c2-b072-2aef340c23b3)(content(Whitespace\"\\n\"))))(Secondary((id \
         9f0de4d1-4b16-445e-a481-d4ba36ae9de2)(content(Comment\"# TAIL \
         RECURSION #\"))))(Secondary((id \
         83e6b120-2643-4d5f-b5b7-26d29b014f72)(content(Whitespace\"\\n\"))))(Tile((id \
         bab91559-9a0e-46dc-be21-571f272ba3a6)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d566240c-294d-4355-a5cf-ceb49321bb70)(content(Whitespace\" \
         \"))))(Tile((id \
         a2094855-7bf3-4e9e-8816-784dd1c4298a)(label(fact1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         64230852-a4d0-45d8-a1a6-c97a011e887e)(content(Whitespace\" \
         \")))))((Secondary((id \
         23c0dd2f-aa58-43ff-85b4-b431fda14542)(content(Whitespace\" \
         \"))))(Tile((id 4772491d-c7ac-46d6-bd50-8c6fddfaa0b5)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 45))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         9ce2ae47-7c88-49b1-9b7c-cfd48ac8dcbe)(content(Whitespace\" \
         \"))))(Tile((id \
         117b325a-e84c-45ef-886e-aebdc614a25a)(label(go))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         b3bb4dd9-7000-4190-920d-c7b8fb8f7eb9)(content(Whitespace\" \
         \")))))((Secondary((id \
         347aeae9-b3c9-4337-8613-1d90f9e5983e)(content(Whitespace\"\\n\"))))(Tile((id \
         c59f1b22-b8a1-44ad-91c7-7c8e09b910c4)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         c4a75931-32ea-462f-87cd-f4edf425dcbc)(content(Whitespace\" \
         \"))))(Tile((id \
         28079767-39a5-446e-998d-8886bb2259c6)(label(\"(\"\")\"))(mold((out \
         Pat)(in_(Pat))(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0 1))(children(((Tile((id \
         519780ff-d0aa-40eb-947c-c867e98bae41)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         08dfd703-8fc9-43cf-9ac1-4f31edc59ecb)(label(,))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 44))(sort Pat))((shape(Concave \
         44))(sort Pat))))))(shards(0))(children())))(Secondary((id \
         f27ba906-8066-4024-8ec3-c3b36f4375a1)(content(Whitespace\"\\n\"))))(Secondary((id \
         736e927e-c9fa-4416-8ee3-c54128d52979)(content(Whitespace\" \
         \"))))(Tile((id \
         e5942290-cf87-4269-8a9f-5abc3930fb2a)(label(acc))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children()))))))))(Secondary((id \
         32bfe16e-ffc1-4bf8-9259-80257e067de1)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         617389c6-5143-4249-b962-3ed623d654d4)(content(Whitespace\"\\n\"))))(Tile((id \
         8631e465-a9ac-4943-a3fd-62caae03ea93)(label(case end))(mold((out \
         Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e2e15690-0e48-4291-ae5d-e22a1066011c)(content(Whitespace\" \
         \"))))(Tile((id \
         ea7d64f1-bae3-4d88-a3e0-51f3c9b23aa9)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a36bcd7a-2ac0-439d-ac1a-1dbb808bead3)(content(Whitespace\"\\n\"))))(Tile((id \
         9cd62eab-32ba-4c98-9027-e796ed69d26e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         67086e7b-235d-4bd9-8d5e-32ae74df5327)(content(Whitespace\" \
         \"))))(Tile((id \
         69c3f7f2-2ad1-45b5-8cca-a7531c98ab27)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         926d7a7c-d7b0-4092-a8d5-ab8421149b4c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0273ddd1-b0e5-473b-9558-d9ab5518c28b)(content(Whitespace\" \
         \"))))(Tile((id \
         2290b291-fe12-44dc-8c78-0ef425560816)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6ac122ba-a21f-43b7-a436-ba940637dab8)(content(Whitespace\"\\n\"))))(Tile((id \
         fc0846af-5908-491d-8515-bc7227f677cb)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         54ad7af2-d7c2-414e-9a12-042873cde5a7)(content(Whitespace\" \
         \"))))(Tile((id \
         ea9bd355-7918-42bb-b0ce-e73575cc61d6)(label(_))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f6546879-cbd9-4743-8201-0c1f8f9eab43)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         3b0eb85c-d145-4750-89f9-8f63ba1c19e8)(content(Whitespace\"\\n\"))))(Tile((id \
         b28aa835-a85b-4b46-ae8a-23f2b94e96a4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1f894966-13a2-434d-b240-9b8e14e2ea0c)(content(Whitespace\" \
         \"))))(Tile((id \
         9e44e5c3-b7b1-4aa7-92ec-5a9133a75a56)(label(r))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d56d8734-ccca-4b54-8f13-33ddec257cb8)(content(Whitespace\" \
         \")))))((Secondary((id \
         726c3369-7f90-4f7b-8b1f-e351940e3b47)(content(Whitespace\" \
         \"))))(Tile((id \
         94ef32a5-2df3-4d6c-bae8-f409d2b3b04d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         48b0d149-f255-4f99-b497-2d619a02b0df)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         08cbbfce-7bb1-4338-854c-ffb56d682b35)(label(acc))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a5b48660-5d66-4537-97bb-3562a6769c10)(content(Whitespace\" \
         \"))))(Secondary((id \
         d74e1779-e057-4c9d-b18d-38e6397a2a2d)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         813ae768-7cc1-4570-a78f-8d2039505726)(content(Whitespace\" \
         \"))))(Tile((id \
         c32047f5-7d3d-4f6b-86ea-95176e3c0665)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         265f598f-9d2c-4a8d-8738-ca2d40617388)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e36ff17-5dfb-4b88-9a69-477e0df5a98a)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         61435f26-2a82-4ef8-b955-0c21a96bf30b)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         56403f73-adc0-439f-823a-3ed76dd7b46a)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         73074616-a9ee-464b-a498-7731794012f0)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         444c72c5-223f-4b39-810c-e8a30ea14ddd)(content(Whitespace\" \
         \"))))(Tile((id \
         478676cd-cc77-453e-b009-38d72d7e9713)(label(r))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4a87ec1f-03ec-4f0b-a1ce-7b7a2066977d)(content(Whitespace\" \
         \"))))(Secondary((id \
         1c70e960-0aaf-46c8-a68b-868f9ce6bc61)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         9cf86601-a725-4874-82b4-af98bdc8be5f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         973e87cd-b9ca-40fc-bc81-a0d0ac829d17)(content(Whitespace\"\\n\"))))(Tile((id \
         25c72631-8806-4dab-a7ce-9281e0210cd6)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         29340a54-4cb6-4d6d-afd7-c45f98fb7153)(content(Whitespace\" \
         \"))))(Tile((id \
         15fae75e-a6f2-44ee-a787-ac870622a379)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         6117ab3b-2730-4068-902e-575c7530590c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         eb580b02-cc1e-429c-9280-f7c8f45698a2)(content(Whitespace\" \
         \"))))(Tile((id \
         10744e89-506e-4fcd-81b0-4e2569e9c8e3)(label(go))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c5152c46-113e-47ab-bbd2-fcfe15ac529a)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0f75b0f2-5aa0-4efe-a2fe-6abd524b1fac)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a5cc4534-e07c-4753-b176-9bb9cd743c6f)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Tile((id \
         464f88a9-3c81-4981-869d-eb9a36bb953c)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         18d22c0d-ed0f-47ac-98d4-5037e5e03553)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         ef533095-4c9c-4a3f-96d2-7f8b08053a52)(content(Whitespace\"\\n\"))))(Tile((id \
         63d0e219-8fe6-4e28-afe1-83dd5a871d07)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         e029db21-1869-42f0-8003-6cbf2c6c12c2)(content(Whitespace\" \
         \"))))(Tile((id \
         beb974cc-e100-42a6-9352-0834da0278ab)(label(fact1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b9cb9f8d-d250-4669-880e-6d8ec46b8cbf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9661f31d-6848-4154-ae4d-2b258df889f9)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         35ec19a7-b554-490f-9fc1-6488b4b3d6ef)(content(Whitespace\" \
         \"))))(Tile((id \
         ab7977c3-13d9-4cb0-8c58-cbc2ebbe136a)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         143054ef-f987-4ba9-a83b-0a69e9e1aa3a)(content(Whitespace\" \
         \"))))(Tile((id \
         4d8196e4-855b-462c-a467-06963d963870)(label(120))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f738fe2e-7426-4671-a6b7-b7140eddd254)(content(Whitespace\" \
         \")))))))))(Tile((id \
         626788a2-b107-45b8-bd28-fc0d1b5d542a)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         e891e6ec-a2d3-4c9e-8864-3de23d8aa93e)(content(Whitespace\"\\n\"))))(Secondary((id \
         52e3378c-5a91-4eb0-974f-4259de66c3de)(content(Whitespace\"\\n\"))))(Secondary((id \
         182451fc-14e9-42e7-a426-6ef9141652fc)(content(Whitespace\"\\n\"))))(Tile((id \
         2bf8e8b2-fc90-421b-a1bf-d3fb63045aff)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         68213ac7-a2d3-4247-a888-42d4b6a55767)(content(Whitespace\" \
         \"))))(Tile((id \
         ecf37020-872e-4296-a3a1-082ced672fc3)(label(fib))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         9d800537-38f3-4e4b-a9ec-6486dd4d5051)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         d6ec11b1-df3d-44e3-9292-051d26ad28f9)(content(Whitespace\" \
         \"))))(Tile((id \
         5e81f303-ad4b-4633-91c0-2e63d161dc7a)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         4c6e890c-02ee-42e6-9494-a66aee43f9d4)(content(Whitespace\" \
         \"))))(Tile((id \
         b70c3e8f-f96d-4fce-8dd9-254e0cfc6531)(label(->))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 13))(sort Typ))((shape(Concave \
         13))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         73198900-1a45-43c8-9612-23e55ad2c54e)(content(Whitespace\" \
         \"))))(Tile((id \
         8c5c6b76-53b2-4359-a94e-38db6103bd6c)(label(Int))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         6e5e1730-b56c-4558-a2b2-acaeec60fcae)(content(Whitespace\" \
         \")))))((Secondary((id \
         f3285dce-c7f4-4962-aba6-aa389bae67bd)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ab1eca6-c4eb-4054-8315-773100bd8182)(content(Comment\"# Multiple \
         recursive calls can get complicated! #\"))))(Secondary((id \
         404935f3-bfd4-4a63-9e74-78eafa1b1335)(content(Whitespace\"\\n\"))))(Tile((id \
         5c95ed23-b366-4fdf-8e3d-c6bc8e4dfe0c)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         e1a7b3d7-0c01-4a2b-a8d0-78bd25314e5c)(content(Whitespace\" \
         \"))))(Tile((id \
         6fafb044-c2cf-4270-81c6-23b862299754)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d4136078-38a1-491d-a3eb-f778da23f8e7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         f0cf7cf5-69e9-4c74-b9ed-1e43210c39b0)(content(Whitespace\" \
         \"))))(Tile((id a0910dc7-80bb-4302-876c-2ed84398a46b)(label(case \
         end))(mold((out Exp)(in_(Rul))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3874d1f0-af54-40f8-835c-8d2e98c78e4b)(content(Whitespace\" \
         \"))))(Tile((id \
         2ad0416f-1043-4b5f-a78b-5f079049d25c)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         272a2783-12ad-497b-b820-3779778683fd)(content(Whitespace\"\\n\"))))(Tile((id \
         e2f8a4f5-e471-41b7-87af-95cf64f46376)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         a95a8a04-53b4-4fd9-ac30-c47cef8795dc)(content(Whitespace\" \
         \"))))(Tile((id \
         e5a86f33-f24b-4ada-abae-f66de3639b11)(label(0))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9460d066-26f6-4084-b562-86af01a2fcbe)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         1a987bac-b3cd-4d05-8d21-2f25ef908d01)(content(Whitespace\" \
         \"))))(Tile((id \
         d3f50fdc-45b9-450a-84e5-b26a5c67ff8b)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         127798e6-8f67-4d54-850e-10627b584d77)(content(Whitespace\"\\n\"))))(Tile((id \
         8dfc5890-1aad-4db8-bb23-cace5ccb967e)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         3df5b00e-c547-4815-8984-2e2518f1473c)(content(Whitespace\" \
         \"))))(Tile((id \
         3c2e0f1e-9f02-418d-ab8b-256a46c2e594)(label(1))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         0909f8e7-542b-4a92-a343-38a88a0aa74d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d8f943f7-5a22-40f1-8f22-ef45ff5fab5f)(content(Whitespace\" \
         \"))))(Tile((id \
         99486169-3f8c-4046-ad67-da7125ae0068)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         5ab9ce84-9ac3-4cca-9aa0-680eaa36365c)(content(Whitespace\"\\n\"))))(Tile((id \
         5e4ff416-d1fe-4446-87df-cbbe1d73cce2)(label(| =>))(mold((out \
         Rul)(in_(Pat))(nibs(((shape(Concave 46))(sort Exp))((shape(Concave \
         46))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         b7318889-798b-40c2-85a0-b26e65fdf188)(content(Whitespace\" \
         \"))))(Tile((id \
         3bfbf683-47a0-44f9-a933-0b6cf8f07378)(label(n))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         410b3a06-92d7-4ea5-b925-75bf2025ae2c)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fc0f9ad4-d0d3-4b6f-bb99-6397e154a93d)(content(Whitespace\" \
         \"))))(Secondary((id \
         17f806a5-06a4-444f-aae1-8853c1c99ca0)(content(Whitespace\" \
         \"))))(Secondary((id \
         06f955e3-c973-4dd5-9100-5cef7486ba31)(content(Whitespace\" \
         \"))))(Secondary((id \
         04038654-d52a-4ef8-920d-a1f76a0956c5)(content(Whitespace\"\\n\"))))(Secondary((id \
         8951eeff-d803-4361-b7a6-88908012888c)(content(Comment\"# Select the \
         first `1` below, and use left/right arrow keys #\"))))(Secondary((id \
         09ad0704-bee6-418c-b1ec-7dc42b9981eb)(content(Whitespace\"\\n\"))))(Secondary((id \
         894dbc48-50c2-434d-9850-1c288c1dd61f)(content(Comment\"# to move \
         between samples, considering how the other samples change. \
         #\"))))(Secondary((id \
         6d4895e3-8074-4bf2-8cbd-c6662b4658a2)(content(Whitespace\"\\n\"))))(Tile((id \
         2c49a098-29bf-4423-9772-5fe39ae1adf0)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bfd2c470-686f-48be-b54b-f78a07ec704c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         296befe3-eca1-4982-bb33-18b25ca8060d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         501ab0ed-45ee-4469-8897-67a2c5293e78)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         8f17b3ec-5de9-4532-ae3c-d5df20dd4271)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         c6ba054e-3f85-4ba2-b724-eb360942238f)(content(Whitespace\"\\n\"))))(Tile((id \
         24ea6e45-e2df-4bed-b7d8-bd353efcb564)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         77ce2535-8757-4610-ae0c-cfc018ec1db4)(content(Whitespace\" \
         \"))))(Tile((id \
         86dfd3e8-a566-4e02-8aeb-9934e8095611)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         45c75e4b-f552-444b-9452-39d32f6eed8e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e11c571d-ebc4-42b4-9418-74df56b1f81d)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         0350adf6-3af7-44dc-91aa-be81d9d35c87)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Tile((id \
         9f67cc51-3331-4bc0-a3de-04b3c8321dad)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d6abfb08-6418-40bb-9e25-357b7f93e383)(content(Whitespace\" \
         \"))))(Secondary((id \
         11375e1c-5d84-4449-9fb8-739def33efe5)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         1dd31b54-81a1-4352-ae5e-b652a89bca17)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         6f6158c8-8def-4c91-b376-7cd71f60824b)(content(Whitespace\"\\n\"))))(Tile((id \
         50069bc7-32a3-4244-aa3c-0cfa06d98aa8)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         9912a07e-8cdc-473d-aa59-9f9cce468825)(content(Whitespace\" \
         \"))))(Tile((id \
         3602dd34-a1c3-4a13-af04-c71c089c4e9b)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ccd4743-5f55-47c5-9331-cd7a0de9561c)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         90780269-9bfb-4646-b3d8-9f67c531adf9)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         d2d5c876-5968-4acd-b532-0068192a976f)(content(Whitespace\" \
         \"))))(Tile((id \
         c0932fe0-1a52-403f-8c15-4fd0f0cd36df)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c1899a4f-b21d-40b4-96c5-8720bbdf52c6)(content(Whitespace\" \
         \"))))(Tile((id \
         8da4dcfa-2fc4-4ba7-866b-095895415db6)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         880fe3e9-c530-4249-b867-b7102baf66f7)(content(Whitespace\" \
         \")))))))))(Tile((id \
         d6558eb8-d25a-40c4-938e-86f39e9d5ace)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32ab4209-3a8f-4335-adc3-dd9bdcf636ef)(content(Whitespace\"\\n\"))))(Tile((id \
         f5e92ce9-c28e-4040-99c8-11f8717599a9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         ec6811b3-2540-4cd3-94c2-0fc6df5d5731)(content(Whitespace\" \
         \"))))(Tile((id \
         1647a62b-0dac-498d-a820-e7bc1ff529ab)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         994c942e-0546-4e1e-a586-607cd35ea7ff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         f6bfd073-6c55-4d7c-85f9-c024d0aa4049)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         3a7f86c5-1d98-41e2-9399-61550ba2d55f)(content(Whitespace\" \
         \"))))(Tile((id \
         280e1a70-48fc-4b98-b44e-2080051b799d)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b9cba8f6-b57f-44c7-bf7c-4000669aae85)(content(Whitespace\" \
         \"))))(Tile((id \
         9e10d61e-273a-4dbb-a123-3dbf525b2a6c)(label(2))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f09e4471-1f88-46c7-879e-9b7e408168af)(content(Whitespace\" \
         \")))))))))(Tile((id \
         831f3760-3fea-4b75-b1fa-7b403cdb0a18)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f5455850-bc05-493c-9633-2dd08b92f697)(content(Whitespace\"\\n\"))))(Tile((id \
         799b4bb9-1c84-40eb-bda9-c69d0a08aa61)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         7b00a98a-ded8-49cd-93bc-c38e10fd476a)(content(Whitespace\" \
         \"))))(Tile((id \
         9454dfb8-5ad9-426b-a2e1-5d906911d054)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         30fb2c71-72b6-4b9d-b463-36cd28100621)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         273a6b91-b724-435a-bb5f-d180432d849f)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         abcfc9aa-85df-49c7-bd1a-27c6e467c4a9)(content(Whitespace\" \
         \"))))(Tile((id \
         41996312-b4e0-43e6-9f61-ff0d1218aae8)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d1aad321-7c59-4ac5-9441-d4cb9b7850ee)(content(Whitespace\" \
         \"))))(Tile((id \
         23619571-d2b8-40b1-99fc-b0f350446448)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         6410e5c6-83dc-4b8b-9351-59d1749b1a78)(content(Whitespace\" \
         \")))))))))(Tile((id \
         535ccb16-a99b-4b2a-bc13-7845fab08b34)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ce83db7e-9577-4e9a-9cd8-81afd9bcc024)(content(Whitespace\"\\n\"))))(Tile((id \
         ec91e1dc-1eee-4493-b434-1d9a8ad3e4c3)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         627f8cdf-d808-4f3b-b358-2827f1edd64b)(content(Whitespace\" \
         \"))))(Tile((id \
         04c64545-83ad-4442-aa0c-bd56f78095cd)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7d9fcf27-014c-465e-96a6-a83076b2dc78)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         bde3214b-0899-4ed8-8472-85068e55f460)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b045727a-cefc-4a56-bf97-a31c99960930)(content(Whitespace\" \
         \"))))(Tile((id \
         00e30492-3301-49c8-9c4b-89d728c7f260)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         54c1da57-6f31-437f-9b61-4604dfa23c79)(content(Whitespace\" \
         \"))))(Tile((id \
         a6304008-2661-48cb-9a69-0c19fc9dd5fa)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         e1e29553-d5a1-4b38-9e7d-acee216c57b2)(content(Whitespace\" \
         \")))))))))(Tile((id \
         87efb519-601a-4833-84c8-fad0e189ca8f)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         ef5a63d0-ccf5-4588-82c3-37ed35aa96b7)(content(Whitespace\"\\n\"))))(Tile((id \
         8261898a-4067-434b-b0db-c214e631bc28)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         6444ffb0-ccf5-4eed-8c31-fe8189078e69)(content(Whitespace\" \
         \"))))(Tile((id \
         953cfbae-b15d-40d0-8814-87c989519c28)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bf9f0592-3813-4e07-ab07-d7faa1a9572d)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         8134872e-508c-4533-9c6b-364cbdd11cf4)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         9a5460c4-be90-4011-b0e3-b54f3f5922c0)(content(Whitespace\" \
         \"))))(Tile((id \
         0628ce45-e10d-40c3-9312-8c9ea0304b7e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7f40d288-b3a1-4885-8c58-cb954f1f8ed3)(content(Whitespace\" \
         \"))))(Tile((id \
         5dc4f0c4-c796-4758-9429-62de3570f7c9)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         420d7232-76e4-4542-b107-f5ec7be83647)(content(Whitespace\" \
         \")))))))))(Tile((id \
         63bbcd08-d8c7-4a98-a392-6cb5a799c1a8)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         500b989d-99c4-4276-8958-81768dd6e612)(content(Whitespace\"\\n\"))))(Tile((id \
         ab347eb7-06d3-457b-a0bd-3a5bf3d7bda1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         625fe43d-de2f-462c-8b62-f266a6e01c40)(content(Whitespace\" \
         \"))))(Tile((id \
         b1cbebaa-c082-4561-98af-bdf5c538fe39)(label(fib))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ed46ea0-b8b9-48c7-91a6-3827570996ee)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         4a5285b3-801f-44fc-a8dd-80ab7ac35adb)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         b7a14830-f94d-4c13-9f6e-0d7dd5390ed2)(content(Whitespace\" \
         \"))))(Tile((id \
         3d8f5b55-3697-4c1a-a390-ec58841813ea)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         cdb309bc-3636-472d-aa15-55a0cb95d82f)(content(Whitespace\" \
         \"))))(Tile((id \
         ed1d2ef5-92ba-4f2c-bb02-14ac8d0faa41)(label(13))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         f4829ee9-a4d1-4d1f-a5d6-ed5877d13ac1)(content(Whitespace\" \
         \")))))))))(Tile((id \
         a76ede3c-7095-4855-ab23-1259542e952c)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         374679a2-564b-4e96-81c6-02922f877041)(content(Whitespace\"\\n\"))))(Secondary((id \
         a7baa64d-8e6a-4809-b617-f706f8a82b4b)(content(Whitespace\"\\n\"))))(Secondary((id \
         7e96c184-f3d5-4aa7-b023-4c5defae37be)(content(Comment\"# FUNCTIONS IN \
         FUNCTIONS #\"))))(Secondary((id \
         a8987696-763d-4f28-9254-cbd4b2d11941)(content(Whitespace\"\\n\"))))(Secondary((id \
         2650cbf2-1b00-40eb-8771-d68b7ace55a4)(content(Comment\"# The frunk \
         factory prethunks your frunk for later clunking #\"))))(Secondary((id \
         7c6c4377-5efe-417b-82f9-45f199b5a287)(content(Whitespace\"\\n\"))))(Tile((id \
         f6f22946-c626-4896-9863-6ac4bd2ff667)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d9d04a07-4478-4ff7-82ed-626e2a8845eb)(content(Whitespace\" \
         \"))))(Tile((id \
         269a10b0-76f7-4187-9b5a-fc24544fca1b)(label(frunk_factory))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         3d3e7c3f-0909-460e-a864-f29e1dc78008)(content(Whitespace\" \
         \")))))((Secondary((id \
         f6703634-d33f-4127-af11-209d4075d4e4)(content(Whitespace\" \
         \"))))(Tile((id c9285a8d-64fd-4f2b-8a0c-a09ed6ae6ccb)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         70e498c6-d8d9-45a6-9d70-d4867ed74b63)(content(Whitespace\" \
         \"))))(Tile((id \
         b75ffd0e-17cb-4815-bc71-173caea803d6)(label(y))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         70287715-3bec-4ffa-87bb-da73b94855e2)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         0464476f-81e9-48bb-9b2d-552537b2019a)(content(Whitespace\"\\n\"))))(Secondary((id \
         7b151771-71c1-4fcf-b2c4-992f249ddc18)(content(Comment\"# This is a \
         play area to explore nested function definitions \
         #\"))))(Secondary((id \
         5de5cb85-8137-413c-a226-7845d13aa30d)(content(Whitespace\"\\n\"))))(Secondary((id \
         3ab5b2a6-f7be-46ea-aac0-c5b82de879f7)(content(Comment\"# and \
         functions returning functions #\"))))(Secondary((id \
         d45e19e2-387a-4f58-8706-9cb4249dd574)(content(Whitespace\"\\n\"))))(Tile((id \
         36d11c0b-36c3-4d7f-a49f-5d5e781dd719)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         1cbe487a-2ddb-421e-8326-0ba946047664)(content(Whitespace\" \
         \"))))(Tile((id \
         eea0e3c3-299f-4350-b5b9-4908e40f66e8)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         db94ef16-309e-460e-bafa-a18971233176)(content(Whitespace\" \
         \")))))((Secondary((id \
         b4fc3bb4-cfe3-48af-96c4-04c4224e2d41)(content(Whitespace\"\\n\"))))(Tile((id \
         dbda3754-a454-4e07-bba5-a40dae8a3d86)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         1f19b6fa-bdc2-4d5c-befa-de1014ffacca)(content(Whitespace\" \
         \"))))(Tile((id \
         4efecf43-6786-4f69-84ef-fba9cc5d3559)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7317a9bc-c262-4742-91c1-b60b328ea4a5)(content(Whitespace\" \
         \"))))(Tile((id \
         76afe041-8dea-4b48-9424-646aee2101f9)(label(10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         df95208c-5e59-43fd-a175-4756a6155162)(content(Whitespace\" \
         \"))))(Tile((id \
         fede4021-cd15-4f11-a9e3-11d2f3e9f78d)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         7442d1f1-ba16-4e4c-992f-1302ec37ae1f)(content(Whitespace\" \
         \"))))(Tile((id \
         875f705e-6d1d-4c1b-b4ba-c44a45f00786)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         a658300e-5d2a-4d5f-adc3-7be75fa20fd7)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         51600d80-8415-49c8-92c4-d340cb8d962b)(content(Whitespace\"\\n\"))))(Tile((id \
         707614f0-67da-422e-8e96-5512db72cc1d)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         d690ad80-a081-4d37-bc41-e58f021124f6)(content(Whitespace\" \
         \"))))(Tile((id \
         14cc3f34-01e0-456a-8014-1a7871ea22aa)(label(refactor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         d946aae2-4d0b-4c8b-9441-03a74adc15b6)(content(Whitespace\" \
         \")))))((Secondary((id \
         5fbadf27-5934-4b76-8b6c-ebed0046ddbb)(content(Whitespace\" \
         \"))))(Tile((id be0ede0f-ee41-47bc-8c19-fe1acbeea9e4)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         29252f44-9c83-44f7-a576-641872c81f3e)(content(Whitespace\" \
         \"))))(Tile((id \
         71e99eb5-4525-49da-aeee-42836ef83f6b)(label(x))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         539c835c-572d-44f9-891f-7aea5a32af9f)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         d50721ea-8964-436f-accf-022be001054d)(content(Whitespace\"\\n\"))))(Tile((id \
         d606c69e-2824-4598-8745-dda276cdbaee)(label(x))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8bebfe4d-63cc-4478-bb91-2e58763b70a8)(content(Whitespace\" \
         \"))))(Tile((id \
         da8dfd6c-f377-43b4-b2a4-d77b8e052e8c)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         32194ffc-9ec3-448d-8e23-2a8429cf3984)(content(Whitespace\" \
         \"))))(Tile((id \
         4cf67633-e5ac-4988-8651-e97f9fa397e7)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         da4ea4aa-b0e2-4c5b-b907-1dd5043c2d2e)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         e0f8ec4c-f872-4084-a264-3770b92d5b7c)(content(Whitespace\"\\n\"))))(Tile((id \
         071755e7-da1b-4587-b385-eb5f3f0904d0)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         7d33d782-7591-4060-89d3-657f1c547b11)(content(Whitespace\" \
         \"))))(Tile((id \
         738a53e8-56d5-45ac-8271-e899fcaca173)(label(factor))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         a92dbd28-b905-44ff-8687-61f161e7c17b)(content(Whitespace\" \
         \")))))((Secondary((id \
         978ca23f-8713-4e93-9af6-eb754abbc773)(content(Whitespace\"\\n\"))))(Tile((id \
         bd2d252c-26c9-42e4-97d7-1cbb1975d2f3)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         78bba7cf-da95-4a6b-a9be-72a3bc9662bc)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         dff24c8a-e691-4970-9271-7829dd229da0)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         6493f64a-24d4-4dab-8eb4-376749813fae)(content(Whitespace\"\\n\"))))(Tile((id \
         042e3691-969c-4e66-9dc7-d3522082eb06)(label(-))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0f9837e9-7084-4c55-95f0-69566fe6e9b3)(content(Whitespace\" \
         \"))))(Tile((id \
         24f0c266-fffd-4577-b808-092bb4dae0b0)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         be2a56b0-64fd-42f0-857c-cad8e458e5ff)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         fca76cd1-322b-440e-a237-39ff4bdfb283)(label(y))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4b1e0970-ba5e-4c63-845f-9fde6530f118)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         fb7b8e6d-79bd-47ce-9fe0-5576957afd8b)(content(Whitespace\" \
         \"))))(Secondary((id \
         ff0573c3-9d08-4e80-9edf-666003bc10b7)(content(Whitespace\"\\n\"))))(Tile((id \
         70e1a72e-5529-4fa8-ad30-abab0d130fd4)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         f7d6b3c3-4291-4459-aaa4-63617ef97b01)(content(Whitespace\" \
         \"))))(Tile((id \
         7de8595b-4c31-4bf4-be61-d2745f858ff9)(label(perturb))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         dfe9b00c-9d04-4070-ab8f-07c8935f0616)(content(Whitespace\" \
         \")))))((Secondary((id \
         81b1f96e-1812-43fa-80f7-9e105abf0dc8)(content(Whitespace\" \
         \"))))(Tile((id 1e922444-082d-4936-9474-e32b40205098)(label(fun \
         ->))(mold((out Exp)(in_(Pat))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 37))(sort Exp))))))(shards(0 \
         1))(children(((Secondary((id \
         780556a9-f2ad-4653-a44b-2e2bd3b190d0)(content(Whitespace\" \
         \"))))(Tile((id \
         4cecc176-e73a-4175-9332-02934c1ed8f5)(label(s))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         9480e22e-d766-4dea-91eb-c319b6373bf8)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         5b518cf6-8167-4c22-80ba-c4d5696b7137)(content(Whitespace\"\\n\"))))(Tile((id \
         b9397441-b19a-4a3f-8972-b832fb5adc0e)(label(factor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         4a22a025-821f-45b6-af6c-dabf711a3c8d)(content(Whitespace\" \
         \"))))(Tile((id \
         ce76be63-9a1b-49eb-a511-743335ea010b)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a877edf0-a80e-49c1-b8f9-e4cc312d4154)(content(Whitespace\" \
         \"))))(Tile((id \
         f403d7c4-6e29-4a4b-84ab-76018129d8db)(label(refactor))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d79a8535-576f-48ab-a757-c4318bc74bbf)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9066ed5a-041d-4783-8e09-ff20702498f0)(label(s))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         4526a12d-51fe-4b46-b01b-8680dacecd79)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         76d99dea-33b3-4d4b-898c-fe50c5818b88)(content(Whitespace\"\\n\"))))(Tile((id \
         ad6a08e7-1fc1-48f2-a222-91d89103412a)(label(fun ->))(mold((out \
         Exp)(in_(Pat))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         37))(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         cfcb7485-7530-42e4-9de4-9f6b5348ec28)(content(Whitespace\" \
         \"))))(Tile((id \
         37d1dff6-9bbf-4f13-a866-a024d07e7b14)(label(z))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         f8481d15-5490-4f70-904c-8e336f025052)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         539a1531-2795-4bfe-88c4-7a46bddd7e01)(content(Whitespace\"\\n\"))))(Tile((id \
         cb0473a4-0f45-4e6f-9506-e3d049f0aa1b)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9bd0ecae-2582-4513-97c2-693d58124026)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         486347e5-dd5e-4618-850f-c0b486669ca4)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         1cc89a12-e68c-459e-9ee3-c2762221c64d)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         cf3bd585-375f-4a53-9af4-5b66641275e0)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         7c02b3c7-db51-4c20-93cd-ffd538d8c600)(content(Whitespace\"\\n\"))))(Tile((id \
         28cebf67-36ea-4845-859d-53d466f073ed)(label(+))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 28))(sort Exp))((shape(Concave \
         28))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         a32151d9-34d5-482b-83b1-aedfebc16d61)(content(Whitespace\" \
         \"))))(Tile((id \
         e2cef967-4258-4023-8579-4d50f2ebe124)(label(perturb))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9068d5d6-de20-4a14-aec6-559f01bae34f)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         9e186eee-1036-413b-9f7a-0d5fc50f4447)(label(5))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a6c1de65-8b69-4d6a-9ada-a88137cef4d6)(label(*))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 27))(sort Exp))((shape(Concave \
         27))(sort Exp))))))(shards(0))(children())))(Tile((id \
         088a00f2-34d4-4b81-88a3-e754d53119cb)(label(z))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         11c8b762-450b-4155-9e7b-e353e131e636)(content(Whitespace\" \
         \"))))(Secondary((id \
         1c7176bf-01d8-4301-8275-b9015626066c)(content(Whitespace\" \
         \"))))(Secondary((id \
         fd95898c-9322-4ece-9f2b-8f143e73ebfb)(content(Whitespace\"\\n\")))))))))(Secondary((id \
         0efb0429-5607-418a-8b39-e721662ec883)(content(Whitespace\" \
         \"))))(Tile((id 6d0eabff-102d-4b0b-b79a-0ab9170e2efc)(label(let = \
         in))(mold((out Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort \
         Exp))((shape(Concave 45))(sort Exp))))))(shards(0 1 \
         2))(children(((Secondary((id \
         bec6a608-9d87-422d-92e2-f6cce119d249)(content(Whitespace\" \
         \"))))(Tile((id \
         fd67e14d-0f16-4ed5-b195-9b7e3f377a88)(label(new_frunk))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         bf378563-37d4-428b-a638-6f7b5c57a941)(content(Whitespace\" \
         \")))))((Secondary((id \
         85d081fa-b245-4168-8980-01bf7b443b66)(content(Whitespace\" \
         \"))))(Tile((id \
         2f167608-e186-42e6-a701-cb0420fa49eb)(label(frunk_factory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4d5126c0-300f-4a98-a5e2-d36ed9f79b67)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         906f1d86-26c0-44e7-9e8f-066ec97e3552)(label(7))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         883aec7d-a304-459c-a8f6-cbbd46e9054a)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         82009792-4ebb-41dc-9add-accb8f3087e4)(content(Whitespace\"\\n\"))))(Tile((id \
         e053aa05-069f-4482-bec2-9c349e3912d9)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         953339cb-5ed2-45d8-b370-f5f45a43de0b)(content(Whitespace\" \
         \"))))(Tile((id \
         5f14368b-4e63-4aaf-9698-f0d30af9ac88)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         d039ceda-2f63-47bf-9e3f-78f9828f7a31)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         0b0a1de6-264f-4c89-bfe8-ff961217e7b1)(label(4))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         2dcca6e1-45f6-47b8-a569-0c8a30eaaeaa)(content(Whitespace\" \
         \"))))(Tile((id \
         e231b39c-7417-4e03-8837-fa643606fec1)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         922b9d69-6176-4b80-a6f6-58319ba66dd9)(content(Whitespace\" \
         \"))))(Tile((id \
         4772c253-5428-4a2c-b33f-47f5c981698f)(label(314))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         713c4161-7bde-4dac-87d6-f65a85726418)(content(Whitespace\" \
         \")))))))))(Tile((id \
         b22c4d41-b36b-4c87-a42c-b1928e8375e2)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         d9f59b1d-658a-4d41-8cd4-0269ce1cd0cf)(content(Whitespace\"\\n\"))))(Tile((id \
         a714ced3-832f-4e6b-af8f-c1751e5cbbf1)(label(test end))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Secondary((id \
         245c19f2-d90b-46a0-bc62-f856cf7eedfb)(content(Whitespace\" \
         \"))))(Tile((id \
         dd14863c-80e4-47d1-ba87-5fd21209f382)(label(new_frunk))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         2ec5b0fe-59ad-44a8-9489-57b73994f5b8)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape(Concave 23))(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Tile((id \
         e56a85e9-0d0c-4884-ac18-eb413f1a85e2)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         913f68fa-06b5-48f3-b01c-cf662653261e)(content(Whitespace\" \
         \"))))(Tile((id \
         61a69322-11d3-43f3-ba49-ee648adc853e)(label(==))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 31))(sort Exp))((shape(Concave \
         31))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8bdcd59a-7b56-49aa-ad71-b5bae61392eb)(content(Whitespace\" \
         \"))))(Tile((id \
         44bb2672-b281-4490-8347-6e2a4f6e9924)(label(330))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         790be863-e52f-4aec-81f6-2ba0f2a3476a)(content(Whitespace\" \
         \")))))))))(Tile((id \
         70a944ba-ce6a-41af-8884-6c2dba7b6aa1)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         3276c76b-d95d-4c64-96e1-0657d6848dae)(content(Whitespace\"\\n\"))))(Secondary((id \
         1463751a-e0d3-4518-be76-e165bf848fcf)(content(Whitespace\"\\n\"))))(Secondary((id \
         58c170cf-6248-4848-b6b5-48d456030320)(content(Comment\"# STATICS \
         PROBES: These show inferred type information. #\"))))(Secondary((id \
         82f56581-4d17-4aec-8b22-e37e042ea067)(content(Whitespace\"\\n\"))))(Secondary((id \
         10f0ed27-1c7e-40b8-a496-f33ac76d60d8)(content(Comment\"# Double \
         clicking toggles from analytic to synthetic type. \
         #\"))))(Secondary((id \
         f6134332-90ec-47c5-bf2b-141244ff7f8f)(content(Whitespace\"\\n\"))))(Secondary((id \
         144b17ff-98a9-4199-a23f-d75e0b618357)(content(Whitespace\"\\n\"))))(Tile((id \
         6ba31613-6819-48f6-9da7-c8a54ef30c3b)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         61da5efb-49fd-457d-bd06-40e9fe0bdfe1)(content(Whitespace\" \
         \"))))(Tile((id \
         ff647850-46f0-4e98-bdae-e02b72cc6b73)(label(a))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         40a0bc6c-59c5-443a-8154-276e0ba82583)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         b0d5f1fe-4ed5-4e5f-859b-2073e81d5ae3)(content(Whitespace\" \
         \"))))(Tile((id \
         e9c72731-3648-44ce-a2cc-16e5bd11b279)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         646dc206-a008-4472-b57c-7f3dc083e073)(content(Whitespace\" \
         \")))))((Secondary((id \
         f40afbc0-ef8b-4d44-b9ec-673102e1f2d3)(content(Whitespace\" \
         \"))))(Tile((id \
         de8fd635-cd00-4adf-9aa8-8f005c5f9dc3)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8a8c6a64-6071-40c9-a52c-5b3ae7823a53)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         15695dac-d70d-43fa-925b-38862471140f)(content(Whitespace\"\\n\"))))(Tile((id \
         d3c79e3f-4afb-4001-ad4e-f3878896ff17)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         9eece76f-644d-4cc5-b488-25170e83d829)(content(Whitespace\" \
         \"))))(Tile((id \
         c64714c7-9a44-43f0-b369-cfd257ddc02a)(label(b))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1f7be048-0eaa-4ca2-8d09-25750772e15f)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         416dfdf2-dc60-47e3-9076-f3c1ef27996f)(content(Whitespace\" \
         \"))))(Tile((id \
         7e50c827-7a6e-4f29-a15e-936559312397)(label(Bool))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Secondary((id \
         9b848112-829c-47fb-8a0b-678bff88fbf8)(content(Whitespace\" \
         \")))))((Secondary((id \
         74690ded-7b93-4cd0-b237-3eaecb61bc63)(content(Whitespace\" \
         \"))))(Tile((id \
         4c94a20b-853d-4c8a-82eb-c428f662abaf)(label(true))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         ede1ce4d-5036-4d6b-9872-98c593df1370)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         71df4594-6c70-4ae8-866b-96a273b00e9c)(content(Whitespace\" \
         \"))))(Secondary((id \
         01f9db55-d44d-458c-a244-a013befa4eaf)(content(Whitespace\"\\n\"))))(Tile((id \
         2bfe05bd-0e4e-4c95-978b-00a7a2e0e2c9)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         ce070d5d-d52c-4c2d-932b-0ec78d2be680)(content(Whitespace\" \
         \"))))(Tile((id \
         23a3af04-524e-4247-aa39-3321a4b3e17c)(label(c))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Tile((id \
         1aa6103b-34d5-4860-81d3-e3a64112a4e6)(label(:))(mold((out \
         Pat)(in_())(nibs(((shape(Concave 24))(sort Pat))((shape(Concave \
         24))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         a432b664-b4ca-45a5-9089-b5f7630c60d5)(content(Whitespace\" \
         \"))))(Tile((id \
         332b7701-99c6-4886-9a4b-13c8762df54c)(label(\"(\"\")\"))(mold((out \
         Typ)(in_(Typ))(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0 1))(children(((Tile((id \
         5dd61294-e770-4653-b6e7-b07217dd03ad)(label(String))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children())))(Tile((id \
         ce5b7cf8-d75d-4b93-95fd-aa947071d637)(label(,))(mold((out \
         Typ)(in_())(nibs(((shape(Concave 44))(sort Typ))((shape(Concave \
         44))(sort Typ))))))(shards(0))(children())))(Secondary((id \
         dbb91aa4-09d3-4068-b0c2-7b5fb410437e)(content(Whitespace\" \
         \"))))(Tile((id \
         af117985-d691-4a2e-a454-a59fb6078c94)(label(?))(mold((out \
         Typ)(in_())(nibs(((shape Convex)(sort Typ))((shape Convex)(sort \
         Typ))))))(shards(0))(children()))))))))(Secondary((id \
         d8942280-5466-48f1-9961-ec64e9ca2f1e)(content(Whitespace\" \
         \")))))((Secondary((id \
         9a128210-d1a6-401f-a5b0-1b428c9a6449)(content(Whitespace\" \
         \"))))(Tile((id \
         2cfee106-605d-4763-bbd2-3b1b97032858)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         9d211f62-cd00-476a-9c5e-4652da4afeae)(label(?))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba0b8025-7e16-48b0-857f-e7dcbc0ea055)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         bcbbf17a-526e-4e8e-996b-9ddad2d127d8)(content(Whitespace\" \
         \"))))(Tile((id \
         e3a679d1-27bd-4e93-b0b8-fad80ac494f2)(label(1))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))(Secondary((id \
         02456b5b-a187-4789-9f84-24ffaaeea677)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         52109702-7804-49fd-9db6-0bd5e393fccc)(content(Whitespace\"\\n\"))))(Tile((id \
         a7bf4864-c1d3-4d50-a6e7-02bdee4f2892)(label(b))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         5ef62c8a-2595-4918-9c77-44d60c1a9724)(label(\";\"))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 35))(sort Exp))((shape(Concave \
         35))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         6b861288-953b-4c8c-81c0-5c590921256b)(content(Whitespace\"\\n\"))))(Secondary((id \
         a711377e-7b52-4021-a847-1a484b85dd75)(content(Whitespace\"\\n\"))))(Secondary((id \
         ab51f208-c763-4743-acc7-e5f9b89fdcf8)(content(Whitespace\"\\n\"))))(Secondary((id \
         8ec0be3d-6c5c-4105-b546-48517d95f59b)(content(Comment\"# RICH PROBES: \
         Domain-specific interfaces for probe samples             \
         #\"))))(Secondary((id \
         42213462-08c6-4a14-b841-306c5c4b46a2)(content(Whitespace\"\\n\"))))(Secondary((id \
         6b8e7944-929f-4ea9-aa98-5d45864df02a)(content(Comment\"# A probe \
         sample's context menu offers \\\"View as ...\\\" entries that open  \
         #\"))))(Secondary((id \
         72635eae-179e-4ca3-8e51-a3d918cd4074)(content(Whitespace\"\\n\"))))(Secondary((id \
         4406ebe2-4608-4dab-b1f5-ca35f4b18b8a)(content(Comment\"# \
         domain-specific interactive views of the sample's value. Today the    \
         #\"))))(Secondary((id \
         0206f4c6-f2c9-4d99-84dd-9ed7053ffb00)(content(Whitespace\"\\n\"))))(Secondary((id \
         41dd4854-76ed-44fe-a7e4-749fd0827606)(content(Comment\"# only one \
         available is \\\"View as table\\\", which accepts any list of      \
         #\"))))(Secondary((id \
         dec304cf-4b40-4d88-a844-36cac849c539)(content(Whitespace\"\\n\"))))(Secondary((id \
         46a194bc-6775-428b-85df-12be099fb9fb)(content(Comment\"# labeled \
         tuples. The sample below has it opened already \\226\\128\\148 click \
         the    #\"))))(Secondary((id \
         29d18f6c-057e-4d98-8ce4-64eeb8de52c7)(content(Whitespace\"\\n\"))))(Secondary((id \
         d22f1e7e-87d3-4d80-b783-b0af0b6f0f0c)(content(Comment\"# probe sample \
         to open its context menu and choose \\\"Hide table\\\" to      \
         #\"))))(Secondary((id \
         1308632b-1e31-436b-a63d-9bbd0c72d7b0)(content(Whitespace\"\\n\"))))(Secondary((id \
         409f8a08-b858-4606-b1f9-22f75ac15702)(content(Comment\"# close the \
         modal. Inside the table you can drop columns, rename,       \
         #\"))))(Secondary((id \
         9536e0fb-572c-4fce-a893-bc8adf661ad4)(content(Whitespace\"\\n\"))))(Secondary((id \
         55cbf56a-5b13-4f0a-b1f3-19fdd4b82bb0)(content(Comment\"# sort, \
         filter, group by, etc.                                          \
         #\"))))(Secondary((id \
         16e7f40f-0be8-48e6-b3ec-2733a85bb917)(content(Whitespace\"\\n\"))))(Secondary((id \
         d22a8bf4-5b6e-4894-934c-7597e43a77e4)(content(Whitespace\"\\n\"))))(Tile((id \
         3ddadc1e-d5a2-4d09-9a93-6a962ba22c01)(label(let = in))(mold((out \
         Exp)(in_(Pat Exp))(nibs(((shape Convex)(sort Exp))((shape(Concave \
         45))(sort Exp))))))(shards(0 1 2))(children(((Secondary((id \
         da9a00ee-0cf1-48b7-9984-ccf68b471faf)(content(Whitespace\" \
         \"))))(Tile((id \
         6621d0bc-935d-4282-9892-af74e24a10db)(label(inventory))(mold((out \
         Pat)(in_())(nibs(((shape Convex)(sort Pat))((shape Convex)(sort \
         Pat))))))(shards(0))(children())))(Secondary((id \
         741a6b81-086d-4bc0-861a-7d61c8a52f1c)(content(Whitespace\" \
         \")))))((Secondary((id \
         f51491ed-d4c1-4267-8fb2-96c65dd70874)(content(Whitespace\" \
         \"))))(Projector((id 782264a8-035b-465d-b63f-82beb4588500)(kind \
         Table)(syntax((Tile((id 6248653e-9271-4e79-851e-dfc1a4aaa588)(label([ \
         ]))(mold((out Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape \
         Convex)(sort Exp))))))(shards(0 1))(children(((Secondary((id \
         5c225cc3-d8fa-479b-87e8-38fdd1f434df)(content(Whitespace\"\\n\"))))(Tile((id \
         52cdd889-a7ad-4946-9696-ea6bc7f383b9)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         50d4750e-045e-4a09-8daa-d5e1321d2fcd)(label(item))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7724d16d-25bf-4564-80ce-ad4fcea2314c)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         2f020e7a-4725-4fbc-8e73-5f04ac38665d)(content((Tile((id \
         38520368-3699-4443-aac3-f7c6bbf0e332)(label(\"\\\"Apple\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         023eaae2-19e0-49b8-a027-18d406ac3911)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         8d90d3f2-a960-4586-bb51-d5f1f7a30c4b)(content(Whitespace\" \
         \"))))(Secondary((id \
         4be6a541-c760-44aa-91c5-b151b3ae9306)(content(Whitespace\" \
         \"))))(Tile((id \
         26fd02a3-2046-4861-8e6c-05475b693328)(label(qty))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         8c9bdbeb-3ead-4f23-9368-b1ecf35cb9f7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         bd9200f5-aec8-4cf5-b16e-8586e678bc55)(content((Tile((id \
         1551dc32-9fb3-483a-a24d-98ede67bc454)(label(24))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         68a83678-8e95-46a2-9d87-50c916a05bf6)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c74a3ac9-2020-4b6b-aea1-414247b620d1)(content(Whitespace\" \
         \"))))(Tile((id \
         22f1e6bf-f351-4f02-a0a0-9b1134580880)(label(price))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         4a56963f-2d76-49ee-987f-80419fc4ce0b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         b4d75665-d8d4-4e25-aeec-721bfa79c775)(content((Tile((id \
         216a83ea-a4d2-464d-b350-2faf7f126921)(label(0.50))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))(Tile((id \
         f4d70223-a5d3-48e7-91c0-7b8564640579)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         769917ae-5d89-4f0d-b55e-4da97382c5fd)(content(Whitespace\"\\n\"))))(Tile((id \
         bbb420a9-dbb1-45e5-ae9a-72de3f54b29e)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         cf3734ba-20a5-4b7c-a5c7-d84377e7f85c)(label(item))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         af49c9bc-78fb-43c2-b2cd-e023199599ba)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         440eda1e-d92c-4463-832e-a680e42a93e6)(content((Tile((id \
         baa91acb-25f0-4199-bba4-661b2a4efd3a)(label(\"\\\"Bread\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         60b36a55-eb33-4311-80fb-5653663787ec)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         266c2079-3a40-44da-9551-bc500dbb8d21)(content(Whitespace\" \
         \"))))(Secondary((id \
         54ab2e68-e1a6-4ab4-a5f9-108a4e134738)(content(Whitespace\" \
         \"))))(Tile((id \
         20db779e-3683-4a43-9199-bbf42e959cf0)(label(qty))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         469919fc-3c11-43d3-9481-ff7b8b99f2c8)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0923a314-e2c2-4649-860a-c4cacd69ab63)(content(Whitespace\" \
         \"))))(Splice((id \
         16221e0f-3f76-4cf3-802e-54640aeb4b38)(content((Tile((id \
         1ec445e5-c7ac-432a-a386-813926cf05fb)(label(8))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         b71fb55f-cc08-4b6c-b6cb-bd4d710bc342)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         f7b06d9b-f740-4628-a27b-1c28a8d35456)(content(Whitespace\" \
         \"))))(Tile((id \
         aeeb0b87-d7fd-420b-b67e-5e9896d4a4d5)(label(price))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         bd0a5f35-1221-457f-8c6f-7c04b6128554)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         60aa3cfc-3176-4aed-8a2e-18d520ecddc2)(content((Tile((id \
         4f5095d4-eac7-4de0-ac58-bd63cb55cf80)(label(2.75))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))(Tile((id \
         a1d84ae3-9262-48e6-87da-91a1a68cca53)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         0d412338-731e-43d5-9ec5-5c54877c28a0)(content(Whitespace\"\\n\"))))(Tile((id \
         873eb1af-1aa2-43ec-aa98-5e3f86e46fd2)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         bca490dc-4296-491c-bdb8-3e4d1b7f0d28)(label(item))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         55a0b2dd-f00d-438c-b8a8-565b9a4ce79b)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         fcf42561-50d1-4320-ae70-04675c859199)(content((Tile((id \
         e67dcf56-5a6d-4708-91df-1cb93fe47382)(label(\"\\\"Cheese\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         c2e8ec2d-c248-4701-ad98-202bc45089df)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         1ab8d397-7d26-4f84-b894-0545f3c9eaaa)(content(Whitespace\" \
         \"))))(Tile((id \
         c90ba867-771d-4883-91d5-34d75172baf5)(label(qty))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         a092c1bb-c930-414d-9b42-5061bba7968e)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         123520ba-9f07-4b51-a57b-990be97f6b41)(content(Whitespace\" \
         \"))))(Splice((id \
         0e1f311b-5a6f-4d27-80e3-459cb7a3eda3)(content((Tile((id \
         28280c3f-4914-455e-b2a8-c69a0dd6efbf)(label(3))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         9dfcbbe4-248f-4e73-b34c-ea18d868ce29)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         010baa88-67d5-4a8e-ae5c-357c85c0954c)(content(Whitespace\" \
         \"))))(Tile((id \
         f57ded47-1a0b-4ac7-a2e7-2486232efeab)(label(price))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         83334fb2-03af-4bdb-ae21-666e0ba6ce39)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         90f944e8-72da-4bce-8b9a-a8e5cdd980ec)(content((Tile((id \
         b5d5aed4-c457-4b35-8742-e88dfa6f19f2)(label(6.20))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))(Tile((id \
         33326bb3-049d-461b-a324-e81c0fee2643)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         80983f48-ead6-474c-b2b9-f72b3529b26f)(content(Whitespace\"\\n\"))))(Tile((id \
         dd8c067c-94f2-459b-9800-ee2c6c815e87)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         7fd6ee62-8ad9-4bc8-abb0-7186d59eb0a5)(label(item))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         ba5a87f6-fc2d-4907-954a-f4693c1e831d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         325e5aee-f995-46f2-b4e7-082a0ea96a1b)(content((Tile((id \
         e27785b9-c825-4a59-8241-cbd7500dcf86)(label(\"\\\"Donut\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         737aec50-cadf-4732-87e6-e7be852e29a7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         72470084-b5d1-4026-8a12-b3d96a2efe60)(content(Whitespace\" \
         \"))))(Secondary((id \
         b7f919df-185f-4a80-907d-bd688bd114c9)(content(Whitespace\" \
         \"))))(Tile((id \
         9b2c6b2b-d353-41f3-ba88-62eecaf4103b)(label(qty))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         7a4e31e4-d05f-4d99-bb7b-cd7d6464cb57)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         cbada37e-aa40-44c7-ab1d-df100f1f9df5)(content((Tile((id \
         977820c1-2f58-4558-a42c-682b27996a6c)(label(12))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         a3ccce07-e937-485b-9e0e-89e54e88e46b)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         021cd612-2463-4756-a578-a47097297bab)(content(Whitespace\" \
         \"))))(Tile((id \
         124e26a9-7bb7-4e98-aec6-dc815576d04b)(label(price))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         c28afb97-f695-40a2-89b1-f81acebfa2d2)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         8e419461-eef0-4f21-8092-f64249c97e86)(content((Tile((id \
         4f90f077-6e65-4349-9027-796ab01026c3)(label(1.10))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))(Tile((id \
         2462afaa-52f4-4cef-bd8f-cc70e856bfa7)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         49e426b4-c003-4c60-9a60-146c5a526ca9)(content(Whitespace\"\\n\"))))(Tile((id \
         370f23d6-caf1-491e-b720-be3a4ede2594)(label(\"(\"\")\"))(mold((out \
         Exp)(in_(Exp))(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0 1))(children(((Tile((id \
         6b48cf05-8e62-4328-b1ae-f938e01793ea)(label(item))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         b0cedaf4-5055-41a6-94b3-5214e85990a7)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         c7b7fa14-d30f-40e8-a438-e6bd8c7ce2a7)(content((Tile((id \
         7c8f70b5-89a6-4118-976e-f6f7f91810a3)(label(\"\\\"Eggs\\\"\"))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         640ace8a-b66a-4f36-b7a2-9b2b0489e915)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         b7c25b29-0739-48a2-a9fb-20d18c48d625)(content(Whitespace\" \
         \"))))(Secondary((id \
         daf8eadf-9a3b-4716-9bb3-ec7792455b96)(content(Whitespace\" \
         \"))))(Secondary((id \
         f65c2877-3351-4055-8228-06b2278251b2)(content(Whitespace\" \
         \"))))(Tile((id \
         2b3e3814-ab2f-45c1-8f73-5d2096a14fd2)(label(qty))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         9e06229d-66a3-4def-8442-6361c64f2784)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         737567f6-adae-4dbc-9628-4d40204d8f4c)(content(Whitespace\" \
         \"))))(Splice((id \
         67f71f2e-5ee8-4f80-a926-5ba8b1f6b005)(content((Tile((id \
         d9be7113-a83a-42ec-846b-38a4c8ff6080)(label(6))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))))))(Tile((id \
         8f212406-61b1-4939-97cd-3538ef0be691)(label(,))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 44))(sort Exp))((shape(Concave \
         44))(sort Exp))))))(shards(0))(children())))(Secondary((id \
         c6adf28f-1924-480c-b489-6df88fc71cb5)(content(Whitespace\" \
         \"))))(Tile((id \
         99971756-dc51-478d-8b27-08590d9b41b8)(label(price))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Tile((id \
         70b431ea-a307-4167-b546-05a39289942d)(label(=))(mold((out \
         Exp)(in_())(nibs(((shape(Concave 39))(sort Exp))((shape(Concave \
         39))(sort Exp))))))(shards(0))(children())))(Splice((id \
         8c22bc00-de9c-437d-8d7a-253e6d4d1743)(content((Tile((id \
         385728bb-d799-47ad-ad99-0e9c583fb10a)(label(3.40))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children()))))))))))))(Secondary((id \
         ea8b5afb-da5f-4a88-9154-b2698077acdd)(content(Whitespace\"\\n\")))))))))))(model\"()\")))(Secondary((id \
         75f0faa0-5fa7-4496-b439-ce8557d6519d)(content(Whitespace\" \
         \")))))))))(Secondary((id \
         7815c472-8aff-4b1b-be4f-460cac900680)(content(Whitespace\"\\n\"))))(Tile((id \
         621e38be-3add-461a-91a0-8303974ac3ce)(label(inventory))(mold((out \
         Exp)(in_())(nibs(((shape Convex)(sort Exp))((shape Convex)(sort \
         Exp))))))(shards(0))(children())))(Secondary((id \
         8663fbf7-0b0c-431f-abc0-985e073a2a0a)(content(Whitespace\"\\n\"))))(Secondary((id \
         14894291-2960-40fe-a525-ac5bdb051904)(content(Whitespace\"\\n\"))))(Secondary((id \
         8faa22c4-6a2a-4743-ab5d-cba768a013dd)(content(Whitespace\"\\n\"))))(Secondary((id \
         798d2bb2-23b1-4bca-bc46-71aeb6bd7ca2)(content(Whitespace\"\\n\"))))(Secondary((id \
         8492043b-6fe7-4422-b495-55f3a532f3ba)(content(Whitespace\"\\n\"))))(Secondary((id \
         e9d37e33-ba1d-451e-960b-1aa6671ba365)(content(Whitespace\"\\n\"))))(Secondary((id \
         356b08ae-de9f-4705-9b6d-40bf2f43d90c)(content(Whitespace\"\\n\"))))(Secondary((id \
         36f48306-369a-4d59-9b81-908a0098ecff)(content(Whitespace\"\\n\"))))(Secondary((id \
         e1e4233b-99ab-4a22-8c94-9c0c8c57ab11)(content(Comment\"# Actions you \
         take in the table modal are real syntax edits \\226\\128\\148 they    \
         #\"))))(Secondary((id \
         96832053-3a11-41f3-aa2a-10b8b3f2d93f)(content(Whitespace\"\\n\"))))(Secondary((id \
         4877050b-34f2-43ea-a0c9-4101f98cc8bf)(content(Comment\"# rewrite the \
         underlying probed expression. Try dropping a column,    \
         #\"))))(Secondary((id \
         b2e2c1ba-b9fc-4a98-b0ea-2f159a01a1f9)(content(Whitespace\"\\n\"))))(Secondary((id \
         ddd5bdce-20c7-4c30-a7b0-f8ec5da2151e)(content(Comment\"# then close \
         the modal: the source above now ends in `|> map(...)`.   \
         #\"))))(Secondary((id \
         395b2e02-4007-42e4-b578-fbb9637a69c7)(content(Whitespace\"\\n\")))))";
      backup_text =
        "#  _____           _                #\n\
         # |  __ \\         | |               #\n\
         # | |__) | __ ___ | |__   ___  ___  #\n\
         # |  ___/ '__/ _ \\| '_ \\ / _ \\/ __| #\n\
         # | |   | | | (_) | |_) |  __/\\__ \\ #\n\
         # |_|   |_|  \\___/|_.__/ \\___||___/ #\n\
         #    INLINE EVAL WITH LIVE PROBES   #\n\n\
         # INTRODUCTION #\n\n\
         # Probe permit a kind of inline evaluation, #\n\
         # similar to value hints in Emacs or IntelliJ. #\n\n\
         # You can put one on any expression or pattern to see #\n\
         # the values it takes on during evaluation. Sampled #\n\
         # values are sorted by left-to-right by most-recent. #\n\n\
         # When a sample is selected, you can hover over it to see #\n\
         # relevant environment variables, and all /other/ samples #\n\
         # are decorated according to their relative position in #\n\
         # the call stack relative to the selected sample. #\n\n\
         # Probes replace print statements while also offering some #\n\
         # stepping debugger features to help maintain context when #\n\
         # navigating between different probed expressions, which may #\n\
         # take on many values across nested or recursive functions. #\n\n\n\
         # TUTORIAL #\n\n\
         # The expression 10 * 10 below has a probe.  #\n\
         # Its value, 20, is shown in a cell to the right. #\n\
         let chips = ^^probe(10 + 10) in\n\n\
         # To probe the below expression, put your caret to #\n\
         # left of the `(` and either press ctrl/cmd-E or #\n\
         # context-click and select `Add probe` from the menu. #\n\
         let mult = (1 + 2 * 3) in\n\
         # The expression should be underlined in green, #\n\
         # and a cell reading `7` should appear to the right. #\n\
         # The same shortcut or context menu toggle removes it. #\n\n\
         # Click the below cell (with value 140) to select it. #\n\
         let score = ^^probe(chips * mult) in\n\
         # Notice when you hover over a selected cell, it #\n\
         # shows the values of any contained variables. #\n\n\
         # Probes only have cells if the are evaluated. #\n\
         # Below, only the first case branch is evaluated. #\n\
         # Hover over the empty set symbol to see a tooltip. #\n\
         let check = case ^^check(false)\n\
         | false => ^^probe(\"checks out\")\n\
         | true => ^^probe(\"you cheated\")  \n\
         end in\n\n\
         # Probes can be placed on expressions #\n\
         let pow = ^^probe(50 ** 2) in\n\
         # And also on patterns (e.g. variables) #\n\
         let ^^probe(pow) = ^^slider(54) ** 2 in\n\n\
         # FUNCTIONS #\n\n\
         # Because functions can run multiple times, they can #\n\
         # have multiple samples. Note the closure counts circles #\n\
         # are all 2, indicating each probe was evaluated twice. #\n\
         # Double click on any sample to show all samples. #\n\
         let celsius = fun ^^probe(farenheit) ->\n\
         # Click to select the cell above reading 72.5 #\n\
         let diff = ^^probe(farenheit -. 32.) in\n\
         # This highlights cells below corresponding to the same #\n\
         # function call: the cells reading 40.5 and 22.5) #\n\
         ^^probe(5./.9. *. diff) in\n\n\
         # It also accents the text of the sample of the #\n\
         # relevant function call site in pink#\n\
         ^^probe(celsius(72.5));\n\
         # Now select the cell above reading 22.5 #\n\
         ^^probe(celsius(103.1));\n\
         # Note the 72.5, 40.5, and 22.5 are no longer green-highlit #\n\
         # as they are not part of the same call as /the expression/ #\n\
         # `celsius(t1)`. However, they now have blue text, indicating #\n\
         # they are below that function call in the call stack #\n\n\
         # BRANCHING IN FUNCTIONS #\n\n\
         # Select `6` then `5` then '4' below: #\n\
         # (If there is a no-enter sign instead of a sample, #\n\
         # this means that the sample cursor is aligned to #\n\
         # another function. Just click on the sign to realign it) #\n\
         let cases= fun ^^probe(x) ->\n\
         case x \n\
         # Note how each activate exactly one branch below: #\n\
         | 4 => ^^probe(true)\n\
         # Select the `5` above and then the `false` below: #\n\
         | 5 => ^^probe(false)\n\
         # Note the same things are highlit as both cells are #\n\
         # from the same call to cases#\n\
         | _ => ^^probe(true) end    \n\
         in\n\
         # Select `true` below and then the `4` cell #\n\
         # for the argument x to `cases` above. #\n\
         ^^probe(cases(4));\n\
         # Note how the same cells stay indicated, but the kind #\n\
         # of indication changes. The `true` below the `4` above #\n\
         # goes from blue text (created by the cases(4) call) #\n\
         # to green highlighting (part of the same call as `4`). #\n\
         # The formerly selected lower `true` now has pink text #\n\
         # since it indicates the call where indicated `4` lives. #\n\
         ^^probe(cases(5));\n\
         ^^probe(cases(6));\n\n\n\
         # FUNCTIONS CALLING FUNCTIONS #\n\n\
         # Select `9` below. Note four cells below become pink #\n\
         let fourth = fun f -> 4 * ^^probe(f) - 4 in\n\
        \  # This is because they represent function calls #\n\
        \  # above the `9` cell in the function call stack. #\n\
        \  # For example 32 below represents the call producing `9`.  #\n\
         let third = fun t -> ^^probe(fourth(t - 3)) / 3 in\n\
        \  # Now, select `32` above. Note the 9 now has blue text. #\n\
        \  # This represents that it is below the `32` call in the stack. #\n\
        \  # Now select `10` below, which is a call to `third`: #\n\
         let second = fun s -> ^^probe(third(2 * s)) + 2 in\n\
        \  # Note that `9` and `32` both have blue text as the are below in \
         the stack. #\n\
        \  # Now select 12 below, representing a call to `second` #\n\
         let first = fun f -> ^^probe(second(f + 1)) * 2 in\n\
        \  # Note how the colors have changed. Finally, select `24` below, #\n\
        \  # and then again select 12, 10, 32, and 9 in turn. #\n\
         ^^probe(first(5));\n\n\
         # RECURSION #\n\
         # Note how cells are lowered/raised to indicate their #\n\
         # relative call stack depth to the selected cell #\n\
         let fact = fun ^^probe(x) ->\n\
         case ^^probe(x)\n\
         | 1 => ^^probe(1)\n\
         | _ =>\n\
         let r = ^^probe(fact(x-1)) \n\
         in x*^^probe(r)  \n\
         end in\n\
         test ^^probe(fact(5)) == 120 end;\n\n\
         # TAIL RECURSION #\n\
         let fact1 = let go =\n\
         fun (^^probe(x),\n\
        \ ^^probe(acc)) ->\n\
         case ^^probe(x)\n\
         | 1 => ^^probe(acc)\n\
         | _ =>\n\
         let r = ^^probe(x*acc) \n\
         in ^^probe(go(x-1, r)) \n\
         end in\n\
         fun x -> ^^probe(go(x,1)) in\n\
         test ^^probe(fact1(5)) == 120 end;\n\n\n\
         let fib: Int -> Int =\n\
         # Multiple recursive calls can get complicated! #\n\
         fun x -> ^^probe(case ^^probe(x)\n\
         | 0 => ^^probe(1)\n\
         | 1 => ^^probe(1)\n\
         | n =>   \n\
         # Select the first `1` below, and use left/right arrow keys #\n\
         # to move between samples, considering how the other samples change. #\n\
         ^^probe(fib(x-1))\n\
         + ^^probe(fib(x-2)) \n\
         end)\n\
         in\n\
         test ^^probe(fib(1)) == 1 end;\n\
         test ^^probe(fib(2)) == 2 end;\n\
         test ^^probe(fib(3)) == 3 end;\n\
         test ^^probe(fib(4)) == 5 end;\n\
         test ^^probe(fib(5)) == 8 end;\n\
         test ^^probe(fib(6)) == 13 end;\n\n\
         # FUNCTIONS IN FUNCTIONS #\n\
         # The frunk factory prethunks your frunk for later clunking #\n\
         let frunk_factory = fun ^^probe(y) ->\n\
         # This is a play area to explore nested function definitions #\n\
         # and functions returning functions #\n\
         let factor =\n\
         4 + ^^probe(10 * y) in\n\
         let refactor = fun ^^probe(x) ->\n\
         ^^probe(x + factor) in\n\
         let factor =\n\
         ^^probe(refactor(factor))\n\
         - ^^probe(refactor(y)) in \n\
         let perturb = fun ^^probe(s) ->\n\
         factor + ^^probe(refactor(s)) in\n\
         fun ^^probe(z) ->\n\
         ^^probe(perturb(3*z))\n\
         + ^^probe(perturb(5*z))  \n\
         in let new_frunk = ^^probe(frunk_factory(7)) in\n\
         test ^^probe(new_frunk(4)) == 314 end;\n\
         test ^^probe(new_frunk(6)) == 330 end;\n\n\
         # STATICS PROBES: These show inferred type information. #\n\
         # Double clicking toggles from analytic to synthetic type. #\n\n\
         let a: Bool = ^^statics(1) in\n\
         let b: Bool = ^^statics(true) in \n\
         let c: (String, ?) = ^^statics((?, 1)) in\n\
         b;\n\n\n\
         # RICH PROBES: Domain-specific interfaces for probe \
         samples             #\n\
         # A probe sample's context menu offers \"View as ...\" entries that \
         open  #\n\
         # domain-specific interactive views of the sample's value. Today \
         the    #\n\
         # only one available is \"View as table\", which accepts any list \
         of      #\n\
         # labeled tuples. The sample below has it opened already \226\128\148 \
         click the    #\n\
         # probe sample to open its context menu and choose \"Hide table\" \
         to      #\n\
         # close the modal. Inside the table you can drop columns, \
         rename,       #\n\
         # sort, filter, group by, \
         etc.                                          #\n\n\
         let inventory = ^^table([\n\
         (item=\"Apple\",  qty=24, price=0.50),\n\
         (item=\"Bread\",  qty= 8, price=2.75),\n\
         (item=\"Cheese\", qty= 3, price=6.20),\n\
         (item=\"Donut\",  qty=12, price=1.10),\n\
         (item=\"Eggs\",   qty= 6, price=3.40)\n\
         ]) in\n\
         ^^probe(inventory)\n\n\n\n\n\n\n\n\
         # Actions you take in the table modal are real syntax edits \
         \226\128\148 they    #\n\
         # rewrite the underlying probed expression. Try dropping a column,    #\n\
         # then close the modal: the source above now ends in `|> map(...)`.   #\n";
      refractors =
        "((621e38be-3add-461a-91a0-8303974ac3ce((kind \
         Probe)(model\"((active_renderer()))\")))(2cfee106-605d-4763-bbd2-3b1b97032858((kind \
         Statics)(model Expected)))(4c94a20b-853d-4c8a-82eb-c428f662abaf((kind \
         Statics)(model Expected)))(de8fd635-cd00-4adf-9aa8-8f005c5f9dc3((kind \
         Statics)(model Expected)))(2ec5b0fe-59ad-44a8-9489-57b73994f5b8((kind \
         Probe)(model\"((active_renderer()))\")))(d039ceda-2f63-47bf-9e3f-78f9828f7a31((kind \
         Probe)(model\"((active_renderer()))\")))(4d5126c0-300f-4a98-a5e2-d36ed9f79b67((kind \
         Probe)(model\"((active_renderer()))\")))(9068d5d6-de20-4a14-aec6-559f01bae34f((kind \
         Probe)(model\"((active_renderer()))\")))(9bd0ecae-2582-4513-97c2-693d58124026((kind \
         Probe)(model\"((active_renderer()))\")))(37d1dff6-9bbf-4f13-a866-a024d07e7b14((kind \
         Probe)(model\"((active_renderer()))\")))(d79a8535-576f-48ab-a757-c4318bc74bbf((kind \
         Probe)(model\"((active_renderer()))\")))(4cecc176-e73a-4175-9332-02934c1ed8f5((kind \
         Probe)(model\"((active_renderer()))\")))(be2a56b0-64fd-42f0-857c-cad8e458e5ff((kind \
         Probe)(model\"((active_renderer()))\")))(78bba7cf-da95-4a6b-a9be-72a3bc9662bc((kind \
         Probe)(model\"((active_renderer()))\")))(da8dfd6c-f377-43b4-b2a4-d77b8e052e8c((kind \
         Probe)(model\"((active_renderer()))\")))(71e99eb5-4525-49da-aeee-42836ef83f6b((kind \
         Probe)(model\"((active_renderer()))\")))(fede4021-cd15-4f11-a9e3-11d2f3e9f78d((kind \
         Probe)(model\"((active_renderer()))\")))(b75ffd0e-17cb-4815-bc71-173caea803d6((kind \
         Probe)(model\"((active_renderer()))\")))(5ed46ea0-b8b9-48c7-91a6-3827570996ee((kind \
         Probe)(model\"((active_renderer()))\")))(bf9f0592-3813-4e07-ab07-d7faa1a9572d((kind \
         Probe)(model\"((active_renderer()))\")))(7d9fcf27-014c-465e-96a6-a83076b2dc78((kind \
         Probe)(model\"((active_renderer()))\")))(30fb2c71-72b6-4b9d-b463-36cd28100621((kind \
         Probe)(model\"((active_renderer()))\")))(994c942e-0546-4e1e-a586-607cd35ea7ff((kind \
         Probe)(model\"((active_renderer()))\")))(2ccd4743-5f55-47c5-9331-cd7a0de9561c((kind \
         Probe)(model\"((active_renderer()))\")))(a0910dc7-80bb-4302-876c-2ed84398a46b((kind \
         Probe)(model\"((active_renderer()))\")))(45c75e4b-f552-444b-9452-39d32f6eed8e((kind \
         Probe)(model\"((active_renderer()))\")))(bfd2c470-686f-48be-b54b-f78a07ec704c((kind \
         Probe)(model\"((active_renderer()))\")))(99486169-3f8c-4046-ad67-da7125ae0068((kind \
         Probe)(model\"((active_renderer()))\")))(d3f50fdc-45b9-450a-84e5-b26a5c67ff8b((kind \
         Probe)(model\"((active_renderer()))\")))(2ad0416f-1043-4b5f-a78b-5f079049d25c((kind \
         Probe)(model\"((active_renderer()))\")))(b9cb9f8d-d250-4669-880e-6d8ec46b8cbf((kind \
         Probe)(model\"((active_renderer()))\")))(c5152c46-113e-47ab-bbd2-fcfe15ac529a((kind \
         Probe)(model\"((active_renderer()))\")))(265f598f-9d2c-4a8d-8738-ca2d40617388((kind \
         Probe)(model\"((active_renderer()))\")))(48b0d149-f255-4f99-b497-2d619a02b0df((kind \
         Probe)(model\"((active_renderer()))\")))(2290b291-fe12-44dc-8c78-0ef425560816((kind \
         Probe)(model\"((active_renderer()))\")))(ea7d64f1-bae3-4d88-a3e0-51f3c9b23aa9((kind \
         Probe)(model\"((active_renderer()))\")))(e5942290-cf87-4269-8a9f-5abc3930fb2a((kind \
         Probe)(model\"((active_renderer()))\")))(519780ff-d0aa-40eb-947c-c867e98bae41((kind \
         Probe)(model\"((active_renderer()))\")))(78f0539e-defc-4306-903a-6b13fb17b098((kind \
         Probe)(model\"((active_renderer()))\")))(750b2d90-a387-4af4-b75c-735a9f164a60((kind \
         Probe)(model\"((active_renderer()))\")))(bc4b825e-4e64-42b0-98e4-a7bda0856adb((kind \
         Probe)(model\"((active_renderer()))\")))(80345184-fcf0-4a56-b0e8-7536b8860a2b((kind \
         Probe)(model\"((active_renderer()))\")))(696e4a96-af09-4afb-99a3-2ad4980d4d10((kind \
         Probe)(model\"((active_renderer()))\")))(3f99cf1c-0d62-4d90-823f-828bdadd936a((kind \
         Probe)(model\"((active_renderer()))\")))(f3b1e905-4d75-4a04-862a-86378b3d67a1((kind \
         Probe)(model\"((active_renderer()))\")))(a5d45f74-f3a3-422b-9dd7-6bc3277c7d07((kind \
         Probe)(model\"((active_renderer()))\")))(5dee61aa-29d7-4f1f-a037-705dc2c54ffa((kind \
         Probe)(model\"((active_renderer()))\")))(7c187c97-da6d-48e1-a539-fa6e11740d59((kind \
         Probe)(model\"((active_renderer()))\")))(4184660a-a985-4fb8-8bc8-06eb1acd54fd((kind \
         Probe)(model\"((active_renderer()))\")))(a060f95d-3d11-4561-a1c0-3fcd51011847((kind \
         Probe)(model\"((active_renderer()))\")))(9128323f-f471-4cd3-9319-8bc9c45b415c((kind \
         Probe)(model\"((active_renderer()))\")))(5fa7c308-8963-4635-99a1-a453415c4fdb((kind \
         Probe)(model\"((active_renderer()))\")))(78cd744a-e1c2-4d7d-929d-5185369a3121((kind \
         Probe)(model\"((active_renderer()))\")))(7f3051cc-f801-45f1-88a6-70a2388d647a((kind \
         Probe)(model\"((active_renderer()))\")))(a3804efc-4b42-4754-ab62-c1bb98fbf05d((kind \
         Probe)(model\"((active_renderer()))\")))(07ab5fcd-568f-42cd-bf18-2e9d07ab0537((kind \
         Probe)(model\"((active_renderer()))\")))(f7f45c1c-d78c-45ee-b717-e30cbdf12f60((kind \
         Probe)(model\"((active_renderer()))\")))(d000d592-7801-4cf5-9d27-d65feb2150aa((kind \
         Probe)(model\"((active_renderer()))\")))(a1ce4de0-a4ee-4777-ac2a-058f31efd19c((kind \
         Probe)(model\"((active_renderer()))\")))(9e144906-34c2-4548-bab0-66d2e0eb673e((kind \
         Probe)(model\"((active_renderer()))\")))(651e330f-0516-4ee7-82e7-674452735f67((kind \
         Probe)(model\"((active_renderer()))\")))(89e1b3fe-c5e3-43d4-af0d-eb2dd0ae19d8((kind \
         Probe)(model\"((active_renderer()))\")))(b0b04629-16a4-4599-aed6-68f6512cb85e((kind \
         Probe)(model\"((active_renderer()))\")))(5b81c870-29bc-4d7f-9d88-da87517a8a2e((kind \
         Probe)(model\"((active_renderer()))\")))(e9d020c4-8a93-4569-bc8f-74d75d5a3b52((kind \
         Probe)(model\"((active_renderer()))\")))(0efa756f-4e37-4155-bf27-08b5ed56cb52((kind \
         Probe)(model\"((active_renderer()))\")))(d3b7b215-9ea8-4eda-8df8-f229839f9c0a((kind \
         Probe)(model\"((active_renderer()))\"))))";
    } )
