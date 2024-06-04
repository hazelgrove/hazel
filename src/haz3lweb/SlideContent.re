open Virtual_dom.Vdom;
open Node;
let img = create("img");

let slide = (header, content) =>
  div(
    ~key="slide",
    ~attr=Attr.class_("slide"),
    [
      h1(~key="header", ~attr=Attr.class_("slide-header"), [text(header)]),
      div(~key="content", ~attr=Attr.class_("slide-content"), content),
    ],
  );

let code = content => span(~attr=Attr.class_("code"), [text(content)]);

let em = content => span(~attr=Attr.class_("em"), [text(content)]);

let get_content =
  fun
  | "Programming Expressively" =>
    Some(
      slide(
        "Programming Expressively",
        [
          p([
            text(
              "You might not have realized it, but you wrote your
      first computer programs in grade school in the form of
      arithmetic expressions!",
            ),
          ]),
          p([
            text("For example, enter the program "),
            code("2 + 2"),
            text(
              " in the expression editor below.
              Hazel operates like a calculator, computing the value of your expression by equationally simplifying it (i.e. evaluating it), here to the integer value ",
            ),
            code("4"),
            text(". "),
            text("The symbol "),
            code("≡"),
            text(" is pronounced \"is equivalent to\"."),
          ]),
        ],
      ),
    )
  | "Composing Arithmetic Expressions" =>
    Some(
      slide(
        "Composing Arithmetic Expressions",
        [
          p([
            text("Arithmetic expressions are constructed "),
            em("compositionally"),
            text(": by combining smaller expressions using various "),
            em("operators"),
            text(", like addition ("),
            code("+"),
            text(") and multiplication ("),
            code("*"),
            text("), "),
            text(
              " to form larger expressions. The smallest arithmetic expressions are ",
            ),
            em("number literals"),
            text(" like "),
            code("2"),
            text(" and "),
            code("42"),
            text("."),
          ]),
          p([
            text("For example, enter "),
            code("2 * 3 + 4 * 5"),
            text(
              " in the expression editor below, observing that its value is ",
            ),
            code("26"),
            text("."),
            text(
              " The reason is because of the familiar order of operations: multiplication precedes addition.",
            ),
          ]),
          p([
            text(
              " By running your cursor through this program, observe how Hazel's cursor decorations demonstrate this order of operations by grouping together the operands of each operator.",
            ),
          ]),
        ],
      ),
    )
  | "Computing Equationally" =>
    Some(
      slide(
        "Computing Equationally",
        [
          p([
            text("To prove that "),
            code("2 * 3 + 4 * 5 ≡ 26"),
            text(
              " in grade school, we would have written out a series of equational steps, each simplifying the expression from the previous step by performing one elementary arithmetic computation at a time.",
            ),
          ]),
          p([
            text(
              "Use Hazel's stepper by clicking the button to the right of the result below and interactively prove that our evaluation to ",
            ),
            code("26"),
            text(
              " is correct one elementary arithmetic step at a time. This proof is the essence of computation!",
            ),
          ]),
        ],
      ),
    )
  | "Variables" =>
    Some(
      slide(
        "Variables",
        [
          p([
            text(
              "As it turns out, you can enrich arithmetic expressions to go from simple integer computations to general-purpose computations! Over the next several slides, we will see how this works.",
            ),
          ]),
          p([
            text(
              "Our first step is to introduce a way to abbreviate expressions using variables. Type the expression ",
            ),
            code("let x = 2 * 3 in x + 1"),
            text(
              " into the expression editor below, observing that it evaluates to ",
            ),
            code("7"),
            text(". You can insert a new line after the "),
            code("in"),
            text(
              " keyword to make the program more idiomatic (i.e. readable).",
            ),
          ]),
          p([
            text(
              "Variables are given computational meaning simply by substitution. Use the stepper to see how this works.",
            ),
          ]),
        ],
      ),
    )
  | "Compositionality" =>
    Some(
      slide(
        "Compositionality",
        [
          text(
            "Let expressions are expressions, just like arithmetic expressions. As we discussed earlier, expressions are constructed compositionally, so we can even make a let expression an operand of an arithmetic operator.",
          ),
        ],
      ),
    )
  | "Scope" =>
    Some(
      slide(
        "Scope",
        [
          text(
            "The scope of a variable is the expression(s) of the program where it is available for use. For let expressions, the sub-expression that follows the ",
          ),
          code("in"),
          text(
            " keyword has the variable bound by the let expression in scope. Hazel's expression decorations show you where the scope of the variable will end.",
          ),
        ],
      ),
    )
  | "Shadowing" =>
    Some(
      slide(
        "Shadowing",
        [
          p([
            text(
              "Once a variable is defined, it cannot be changed. There is no assignment operator in languages based on pure mathematical expressions, where variables are given meaning by substitution, like Hazel.",
            ),
          ]),
          p([
            text("However, it is possible to define a "),
            em("new variable"),
            text(
              " that shares the name of a previously bound variable. However, this makes it impossible to refer to the previous binding within the scope of the new binding; we say that variable has been ",
            ),
            em("shadowed"),
            text("."),
          ]),
        ],
      ),
    )
  | "Booleans and Types" =>
    Some(
      slide(
        "Booleans and Types",
        [
          p([
            text(
              "So far, we have only seen expressions that evaluate to integers. However, Hazel supports many other types of values, like booleans. There are two boolean values, ",
            ),
            code("true"),
            text(" and "),
            code("false"),
            text("."),
          ]),
          p([
            text(
              "Hazel's type system ensures that expressions are used in ways that make sense. Each expression has a type, which predicts the type of its value. The expressions in the previous slides all had type ",
            ),
            code("Int"),
            text(
              ", which is the type of integers, whereas the boolean values have type ",
            ),
            code("Bool"),
            text(". "),
            text(
              "You can see the type of the expression your cursor is on in the cursor inspector at the bottom of the screen. The symbol ",
            ),
            code(":"),
            text(" is pronounced \"has type\"."),
          ]),
          p([
            text(
              "Boolean values can be constructed by using comparison operators like ",
            ),
            code("<"),
            text(", "),
            code("=="),
            text(", and "),
            code(">"),
            text(" on integers. For example, "),
            code("2 < 3"),
            text(" evaluates to "),
            code("true"),
            text("."),
            text(
              " Booleans can also be combined using logical and (conjunction), ",
            ),
            code("&&"),
            text(", and logical or (disjunction), "),
            code("||"),
            text("."),
          ]),
        ],
      ),
    )
  | "Conditional Expressions" =>
    Some(
      slide(
        "Conditional Expressions",
        [
          p([
            text(
              "Given a boolean expression, we can use it to choose between two expressions using a ",
            ),
            em("conditional expression"),
            text(". For example, "),
            code("if 2 < 3 then 4 * 4 else 5 * 5"),
            text(" evaluates to "),
            code("4"),
            text("."),
          ]),
        ],
      ),
    )
  | "Functions" =>
    Some(
      slide(
        "Functions",
        [
          p([
            text(
              "Functions are expressions that take other expressions as inputs and produce other expressions as outputs. For example, ",
            ),
            code("fun x -> x + 1"),
            text(
              " is a function that takes an integer expression as input and produces an integer expression as output.",
            ),
            text(
              "Functions in Hazel do not themselves have names. Instead, you can use a let expression to name a function.",
            ),
          ]),
          p([
            text(
              "You apply a function to an argument expression by using parentheses in the usual way. For example, ",
            ),
            code("let f = fun x -> x + 1 in f(2)"),
            text(" evaluates to "),
            code("3"),
            text(
              ". Go through this example in the stepper to see how substitution of the function for the variable standing for the function works.",
            ),
          ]),
        ],
      ),
    )
  | "Tuples" => Some(slide("Tuples", []))
  | "Pattern Matching on Tuples" =>
    Some(slide("Pattern Matching on Tuples", []))
  | "Recursion" => Some(slide("Recursion", []))
  | "Lists" => Some(slide("Lists", []))
  | "Pattern Matching on Lists" =>
    Some(slide("Pattern Matching on Lists", []))
  | "Recursion on Lists: length" =>
    Some(slide("Recursion on Lists: length", []))
  | "Recursion on Lists: sum" => Some(slide("Recursion on Lists: sum", []))
  | "Recursion on Lists: num_zeros" =>
    Some(slide("Recursion on Lists: num_zeros", []))
  | "Higher-Order Functions" => Some(slide("Higher-Order Functions", []))
  | _ => None;
