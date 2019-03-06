
open OUnit
open Ast
open Infer

let env () = StringMap.empty

let test_num = fun () ->
  let exp = Lit (Int 5) in
  let actual = infer StringMap.empty exp in
  assert_equal TInt actual

let test_bool = fun () ->
  let exp = Lit (Bool true) in
  let actual = infer StringMap.empty exp in
  assert_equal TBool actual

let test_abs = fun () ->
  let exp = Abs ("x", Abs ("y", Var "x")) in
  let actual = infer StringMap.empty exp in
  let expected = TFun (TVar "a0", TFun (TVar "a1", TVar "a0")) in
  assert_equal expected actual

let test_app = fun () ->
  let exp = App (Abs ("x", Var "x"), Lit (Bool false)) in
  let actual = infer StringMap.empty exp in
  assert_equal TBool actual

let test_let = fun () ->
  let exp = Let ("x", Abs ("y", Var "y"), App (Var "x", Lit (Int 5))) in
  let actual = infer StringMap.empty exp in
  assert_equal TInt actual

let test_add = fun () ->
  let exp = Abs ("y", Binop (Var "y", Add, Lit (Int 5))) in
  let actual = infer StringMap.empty exp in
  let expected = TFun (TInt, TInt) in
  assert_equal expected actual

let test_sub = fun () ->
  let exp = Abs ("y", Binop (Var "y", Sub, Lit (Int 5))) in
  let actual = infer StringMap.empty exp in
  let expected = TFun (TInt, TInt) in
  assert_equal expected actual

let test_mul = fun () ->
  let exp = Abs ("y", Binop (Var "y", Mul, Lit (Int 5))) in
  let actual = infer StringMap.empty exp in
  let expected = TFun (TInt, TInt) in
  assert_equal expected actual

let test_div = fun () ->
  let exp = Abs ("y", Binop (Var "y", Div, Lit (Int 5))) in
  let actual = infer StringMap.empty exp in
  let expected = TFun (TInt, TInt) in
  assert_equal expected actual

let test_and = fun () ->
  let exp = Binop (Lit (Bool true), And, Lit (Bool false)) in
  let actual = infer StringMap.empty exp in
  assert_equal TBool actual

let test_or = fun () ->
  let exp = Binop (Lit (Bool true), Or, Lit (Bool false)) in
  let actual = infer StringMap.empty exp in
  assert_equal TBool actual

let suite =
  "Tests" >:::
  [
    "Num" >:: test_num;
    "Bool" >:: test_bool;
    "Lambda" >:: test_abs;
    "Application" >:: test_app;
    "Let" >:: test_let;
    "Binop Add" >:: test_add;
    "Binop Sub" >:: test_sub;
    "Binop Mul" >:: test_mul;
    "Binop Div" >:: test_div;
    "Binop And" >:: test_and;
    "Binop Or" >:: test_or;
  ]

let _ = run_test_tt_main suite

