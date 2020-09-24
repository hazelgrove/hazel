open Lexing

module I = TestParse.MenhirInterpreter

let rec incr lexbuf c =
        match c with
        | I.InputNeeded _ ->
                let token = TestLex.read lexbuf in
                let startp = lexbuf.lex_start_p
                and endp = lexbuf.lex_curr_p in
                let checkpoint = I.offer c (token, startp, endp) in
                incr lexbuf checkpoint
        | I.Shifting _
        | I.AboutToReduce _ ->
                let checkpoint = I.resume c in
                incr lexbuf checkpoint
        | I.HandlingError _ ->
                let startp = lexbuf.lex_start_p in
                let line = startp.pos_lnum in
                let col = startp.pos_cnum + 1 in
                Types.Error (Types.Int line, Types.Int col)
        | I.Accepted v -> v
        | I.Rejected ->
                Types.Constant (Types.Int 4)
