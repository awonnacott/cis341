(* Author: Steve Zdancewic  *)
(* Modified by: Zak Kincaid *)
(* Modified by: Andrew Wonnacott *)

open Assert
open Hellocaml

(* Some helper code *)

let _ = Random.self_init ()

let signed_random () = Int64.of_int (Random.int 1000 * if Random.bool () then 1 else -1)

let rec make_random_exp vars p =
  let constructor = Random.float 1. in
  if Random.float 1. > p || constructor < 0.15 then
    if Random.bool () then Const (signed_random ())
    else Var (List.nth vars (Random.int (List.length vars)))
  else if constructor < 0.35 then Neg (make_random_exp vars (p *. 0.95))
  else if constructor < 0.7 then
    Add (make_random_exp vars (p *. 0.9), make_random_exp vars (p *. 0.9))
  else Mult (make_random_exp vars (p *. 0.9), make_random_exp vars (p *. 0.9))

let rec make_random_exps vars n =
  if n <= 0 then [] else make_random_exp vars 1. :: make_random_exps vars (n - 1)

let ctx = [("x", signed_random ()); ("y", signed_random ()); ("z", signed_random ())]

let random_exps = make_random_exps (List.map (fun (e, _) -> e) ctx) 1000

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite =
  [ Test
      ( "Student-Provided Tests For Problem 1-3"
      , [ ("case1", assert_eqf (fun () -> failwith "Problem 3 case1 test unimplemented") prob3_ans)
        ; ( "case2"
          , assert_eqf (fun () -> failwith "Problem 3 case2 test unimplemented") (prob3_case2 17)
          )
        ; ("case3", assert_eqf (fun () -> prob3_case3) 0) ] ) ]
