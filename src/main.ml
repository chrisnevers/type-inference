open Lexer
open Parser
open Ast
open Ftv
open Apply
open Unify
open Infer

let rec repl () =
  print_string "> ";
  let str = read_line () in
  let buffer = Lexing.from_string str in
  let ast = main token buffer in
  let ty = infer StringMap.empty ast in
  print_endline (str_mono_ty ty);
  repl ()

let () = repl ()
