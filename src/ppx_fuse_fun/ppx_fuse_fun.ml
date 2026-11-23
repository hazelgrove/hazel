(* See the dune file: fuse [fun a -> fun b -> e] into [fun a b -> e], as the
   OCaml parser would have produced for [fun a b -> e]. Only a function whose
   body is directly another function, with no type constraint on the outer
   function and no attributes on the inner one, is fused, so the result is
   exactly what OCaml syntax gives for the same function. *)
open Ppxlib

let fuse =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      match e.pexp_desc with
      | Pexp_function
          ( (_ :: _ as params)
          , None
          , Pfunction_body
              { pexp_desc = Pexp_function (inner_params, constraint_, body)
              ; pexp_attributes = []
              ; _
              } ) ->
        { e with pexp_desc = Pexp_function (params @ inner_params, constraint_, body) }
      | _ -> e
  end

let () = Driver.register_transformation "ppx_fuse_fun" ~impl:fuse#structure
