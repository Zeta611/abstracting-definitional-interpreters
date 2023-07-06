open Base
open Stdio
open Abstracting_definitional_interpreters.Definitional_interpreter

let expr =
  Op2
    {
      op = "*";
      lhs = Op2 { op = "+"; lhs = Const 3; rhs = Const 4 };
      rhs = Const 9;
    }

let _expr =
  App
    {
      operator =
        Lambda { arg = "x"; body = Lambda { arg = "y"; body = Var "x" } };
      operand = Op2 { op = "+"; lhs = Const 1; rhs = Const 2 };
    }

let { result; store; trace } = Collecting_interpreter.eval expr

let () =
  printf "expr: %s\n" (show_expr expr);
  (match result with
  | Ok value -> printf "value: %s\n" (show_value value)
  | Error exn -> printf "exception: %s\n" (Exn.to_string exn));
  printf "store: %s\n" (show_store store);
  printf "trace: %s\n" (show_trace trace)
