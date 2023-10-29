// type t = EvaluatorMonad.t(StepperResult.t((DHExp.t, int)));
// open StepperResult;
// open EvaluatorMonad;
// open EvaluatorMonad.Syntax;
// module type S = {
//   let evaluate: StepperRequire.t => EvaluatorMonad.t(StepperResult.t(DHExp.t));
// };
// module Make = (S: S) => {
//   let default = f => `Continue(BoxedValue(f), ()) |> return;
//   let combine_indet = (f, d: StepperRequire.t) => {
//     let* f = f;
//     switch (f) {
//     | `Continue(BoxedValue(f), t) =>
//       let* r = S.evaluate(d);
//       let* s = get_step;
//       switch (r, s) {
//       | (BoxedValue(d), 0) => `Return(BoxedValue(f(d))) |> return
//       | (Indet(d), 0) => `Return(Indet(f(d))) |> return
//       | (Expr(d), 0) => `Return(Expr(f(d))) |> return
//       | (BoxedValue(d), _) => `Continue(f(d), (t, `BoxedValue(d))) |> return
//       | (Indet(d), _) => `Continue(f(d), (t, `Indet(d))) |> return
//       | (Expr(d), _) => `Continue(f(d), (t, `Expr(d))) |> return
//       }
//     | `Return(f) => `Return(f(d)) |> return
//     };
//   };
//   let combine_boxed = (f, d) => {
//     let* f = f;
//     switch (f) {
//     | `Continue(f, t) =>
//       let* r = S.evaluate(d)
//       switch (r) {
//       | BoxedValue(d) => `Continue(f(d), (t, `BoxedValue(d))) |> return
//       | Indet(d) => `Return(f(d)) |> return
//       | Expr(d) => `Return(f(d)) |> return
//       }
//     | `Return(f) => `Return(f(d)) |> return
//     };
//   };
//   let bind = (f, k) => {
//     let* f = f;
//     switch (f) {
//     | `Return(f) =>
//     | `Continue(_, t) => k(t)
//     }
//   }
//   let return = EvaluatorMonad.return
//   module Syntax = {
//     let ( let@ ) =
//   }
// }
