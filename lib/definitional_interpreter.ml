open Base
open Monads.Std

type expr =
  | Const of int
  | Var of string
  | If0 of { pred : expr; conseq : expr; alt : expr }
  | Op2 of { op : string; lhs : expr; rhs : expr }
  | Rec of { name : string; body : expr }
  | Lambda of { arg : string; body : expr }
  | App of { operator : expr; operand : expr }
[@@deriving show { with_path = false }]

type loc = int

type value =
  | Num of int
  | Closure of { arg : string; body : expr; env : environment }

and environment = (string, loc, String.comparator_witness) Map.t
and store = (loc, value, Int.comparator_witness) Map.t

let show_env env =
  let show_pair (x, loc) = Printf.sprintf "%s : %d" x loc in
  let pairs = Map.to_alist env in
  let pairs_str = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  Printf.sprintf "{ %s }" pairs_str

let show_value = function
  | Num n -> "Num" ^ Int.to_string n
  | Closure { arg; body; env } ->
      Printf.sprintf "<(fun %s -> %s), %s>" arg (show_expr body)
        (show_env env)

let show_store store =
  let show_pair (loc, v) = Printf.sprintf "%d : %s" loc (show_value v) in
  let pairs = Map.to_alist store in
  let pairs_str = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  Printf.sprintf "{ %s }" pairs_str

module Store = struct
  module T = struct
    type t = store
  end

  include Monad.State.T1 (T) (Monad.Ident)
  include Monad.State.Make (T) (Monad.Ident)

  let empty = Map.empty (module Int)
end

module ExnState = struct
  include Monad.Result.Exception.T (Store)
  include Monad.Result.Exception.Make (Store)
end

module Environment = struct
  module T = struct
    type t = environment
  end

  include Monad.Reader.T1 (T) (ExnState)
  include Monad.Reader.Make (T) (ExnState)

  let empty = Map.empty (module String)
end

module M = Environment

exception Unbound_operator of string
exception Type_error of { op : string; value : value }

let run_op2 (op : string) (lhs : value) (rhs : value) : value M.t =
  let m =
    let open ExnState in
    match (op, lhs, rhs) with
    | "+", Num lhs, Num rhs -> return (Num (lhs + rhs))
    | "-", Num lhs, Num rhs -> return (Num (lhs - rhs))
    | "*", Num lhs, Num rhs -> return (Num (lhs * rhs))
    | "/", Num lhs, Num rhs ->
        if rhs = 0 then fail Division_by_zero else return (Num (lhs / rhs))
    | op, Num _, Num _ -> fail (Unbound_operator op)
    | op, Num _, _ -> fail (Type_error { op; value = rhs })
    | op, _, _ -> fail (Type_error { op; value = lhs })
  in
  M.lift @@ m

let fail (e : exn) : value M.t = M.lift @@ ExnState.fail e

let find (loc : loc) : value M.t =
  let m =
    let open Store in
    let* store = get () in
    let value = Map.find_exn store loc in
    return value
  in
  M.lift @@ ExnState.lift @@ m

let alloc (_x : string) : loc M.t =
  let m =
    let open Store in
    let* store = get () in
    let new_loc = Map.length store in
    return new_loc
  in
  M.lift @@ ExnState.lift @@ m

let extend (loc : loc) (v : value) : unit M.t =
  let m =
    let open Store in
    let* store = get () in
    let store' = Map.set store ~key:loc ~data:v in
    put store'
  in
  M.lift @@ ExnState.lift @@ m

let ask_env : unit -> environment M.t = M.read
let find_env (env : environment) (x : string) : loc = Map.find_exn env x

let augment (env : environment) (x : string) (loc : loc) : environment =
  Map.set env ~key:x ~data:loc

let local_env (env : environment) (m : value M.t) : value M.t =
  M.(lift (run m env))

type eval = expr -> value M.t

let ev (ev : eval) : eval =
  let open M in
  function
  | Const n -> return (Num n)
  | Var x ->
      let* env = ask_env () in
      find (find_env env x)
  | If0 { pred; conseq; alt } -> (
      let* v = ev pred in
      match v with
      | Num 0 -> ev conseq
      | Num _ -> ev alt
      | _ -> fail (Type_error { op = "if0"; value = v }))
  | Op2 { op; lhs; rhs } ->
      let* lhs = ev lhs in
      let* rhs = ev rhs in
      run_op2 op lhs rhs
  | Rec { name; body } ->
      let* env = ask_env () in
      let* loc = alloc name in
      let env' = augment env name loc in
      let* v = local_env env' (ev body) in
      let* () = extend loc v in
      return v
  | Lambda { arg; body } ->
      let* env = ask_env () in
      return (Closure { arg; body; env })
  | App { operator; operand } -> (
      let* closure = ev operator in
      match closure with
      | Closure { arg = x; body; env } ->
          let* v = ev operand in
          let* loc = alloc x in
          let* () = extend loc v in
          let env' = augment env x loc in
          local_env env' (ev body)
      | _ -> fail (Type_error { op = "application"; value = closure }))

type m_result = { result : (value, exn) Result.t; store : store }

let mrun (m : value M.t) : m_result =
  let result, store = Store.(run ExnState.(run M.(run m empty)) empty) in
  { result; store }

let rec fix f x = f (fix f) x
let eval e = mrun ((fix ev) e)
