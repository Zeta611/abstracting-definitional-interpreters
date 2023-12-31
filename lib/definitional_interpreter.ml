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

exception Unbound_operator of string
exception Type_error of { op : string; value : value }

let show_env env =
  let show_pair (x, loc) = Printf.sprintf "%s : %d" x loc in
  let pairs = Map.to_alist env in
  let pairs_str = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  if String.is_empty pairs_str then "{}" else Printf.sprintf "{ %s }" pairs_str

let show_value = function
  | Num n -> "Num" ^ Int.to_string n
  | Closure { arg; body; env } ->
      Printf.sprintf "<(fun %s -> %s), %s>" arg (show_expr body) (show_env env)

let show_store store =
  let show_pair (loc, v) = Printf.sprintf "%d : %s" loc (show_value v) in
  let pairs = Map.to_alist store in
  let pairs_str = List.map pairs ~f:show_pair |> String.concat ~sep:", " in
  if String.is_empty pairs_str then "{}" else Printf.sprintf "{ %s }" pairs_str

let rec fix f x = f (fix f) x

module type Computation = sig
  type 'a t

  include Monad.Core with type 'a t := 'a t
  include Monad.Trans.S with type 'a t := 'a t
  include Monad.Syntax.Let.S with type 'a t := 'a t
end

module type Components = sig
  type 'a t
  type result
  type eval = expr -> value t

  module M : Computation with type 'a t := 'a t

  val run_op2 : string -> value -> value -> value t
  val fail : exn -> value t
  val find : loc -> value t
  val alloc : string -> loc t
  val extend : loc -> value -> unit t
  val ask_env : unit -> environment t
  val find_env : environment -> string -> loc
  val augment : environment -> string -> loc -> environment
  val local_env : environment -> value t -> value t
  val eval : (eval -> eval) -> expr -> result
end

module type Interpreter = sig
  type result

  val eval : expr -> result
end

module Make_interpreter (C : Components) :
  Interpreter with type result := C.result = struct
  type result = C.result
  type eval = C.eval

  let ev (ev : eval) : eval =
    let open C in
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

  let eval (e : expr) : result = C.eval ev e
end

type default_result = { result : (value, exn) Result.t; store : store }

module Default_components : Components with type result = default_result =
struct
  type result = default_result

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

  type 'a t = 'a M.t
  type eval = expr -> value t

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

  let mrun (m : value M.t) : result =
    let result, store = Store.(run ExnState.(run M.(run m empty)) empty) in
    { result; store }

  let eval (ev : eval -> eval) (e : expr) : result = mrun ((fix ev) e)
end

module Default_interpreter = Make_interpreter (Default_components)

type trace_elem = expr * environment * store
type trace = trace_elem list

let show_trace (trace : trace) : string =
  let show_tuple (e, env, store) =
    Printf.sprintf "<%s, %s, %s>" (show_expr e) (show_env env)
      (show_store store)
  in
  let tuples_str =
    trace |> List.map ~f:show_tuple |> String.concat ~sep:";\n"
  in
  if String.is_empty tuples_str then "[]"
  else Printf.sprintf "[ %s ]" tuples_str

type collecting_result = {
  result : (value, exn) Result.t;
  store : store;
  trace : trace;
}

module Collecting_components : Components with type result = collecting_result =
struct
  type result = collecting_result

  module Trace = struct
    module T = struct
      type t = trace

      include Monoid.List.Make (struct
        type t = trace_elem
      end)
    end

    include Monad.Writer.T1 (T) (Monad.Ident)
    include Monad.Writer.Make (T) (Monad.Ident)
  end

  module Store = struct
    module T = struct
      type t = store
    end

    include Monad.State.T1 (T) (Trace)
    include Monad.State.Make (T) (Trace)

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

  type 'a t = 'a M.t
  type eval = expr -> value t

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

  let ev_tell (ev : eval -> eval) (ev' : eval) (e : expr) : value M.t =
    let open M in
    let* env = ask_env () in
    let* store = lift @@ ExnState.lift @@ Store.get () in
    let* () =
      lift @@ ExnState.lift @@ Store.lift @@ Trace.write [ (e, env, store) ]
    in
    (ev ev') e

  let mrun (m : value M.t) : result =
    let (result, store), trace =
      Trace.(run Store.(run ExnState.(run M.(run m empty)) empty))
    in
    { result; store; trace }

  let eval (ev : eval -> eval) (e : expr) : result = mrun ((fix (ev_tell ev)) e)
end

module Collecting_interpreter = Make_interpreter (Collecting_components)
