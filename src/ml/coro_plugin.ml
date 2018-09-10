open Globals
open EvalValue
open EvalContext
open EvalEncode
open Common
open MacroApi
open Ast
open Type

type sec_fields = {
	sec_finished : int;
	sec_interrupted : int;
}

let state_exit_code = {
	sec_finished = -1;
	sec_interrupted = -2;
}

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue.
*)
let fail ?msg hxpos mlpos =
	let msg =
		(Lexer.get_error_pos (Printf.sprintf "%s:%d:") hxpos) ^ ": "
		^ "Coro: " ^ (match msg with Some msg -> msg | _ -> " unexpected expression.") ^ "\n"
		^ "Submit an issue to https://github.com/RealyUniqueName/Coro/issues with expression example and following information:"
	in
	match mlpos with
		| (file, line, _, _) ->
			Printf.eprintf "%s\n" msg;
			Printf.eprintf "%s:%d\n" file line;
			assert false

(**
	Returns human-readable string representation of specified type
*)
let str_type t = s_type (print_context()) t

let coroutine_type_path = (["coro"], "Coroutine")

let generated_type_path = (["coro"], "Generated")

let state_machine_type_path = (["coro"], "StateMachine")

let throwing_state_machine_type_path = (["coro"], "ThrowingStateMachine")

let coroutine_exception_type_path = (["coro"], "CoroutineException")

let coroutine_state_exception_type_path = (["coro"], "CoroutineStateException")

let suspend_type_path = (["coro"], "Suspend")

let loop_control_break = "CORO_BREAK"
let loop_control_continue = "CORO_CONTINUE"

(**
	Check if the list is empty
*)
let is_empty lst =
	match lst with
		| [] -> true
		| _ -> false

let null_expr t pos nullifier =
	if is_nullable t then null t pos
	else null (nullifier t) pos

(**
	If `expr` is a TCast or TMeta, then returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

let check_type predicate t =
	match t with
		| TMono r ->
			(match !r with
				| Some t -> predicate t
				| None -> false
			)
		| TLazy f ->
			predicate (lazy_type f)
		| TType (td,tl) ->
			predicate (apply_params td.t_params tl td.t_type)
		| TAbstract ({a_path = [],"Null"}, [t]) ->
			predicate t
		| _ ->
			false

let rec is_void_type t =
	match t with
		| TAbstract (a, _) when a.a_path = ([], "Void") ->
			true
		| _ -> check_type is_void_type t

let rec is_coroutine_type t =
	match t with
		| TType (t,tl) when t.t_path = coroutine_type_path ->
			true
		| _ -> check_type is_coroutine_type t

let rec is_generated_type t =
	match t with
		| TType (t,tl) when t.t_path = generated_type_path ->
			true
		| _ -> check_type is_generated_type t

let rec is_suspend_type t =
	match t with
		| TType (t,tl) when t.t_path = suspend_type_path ->
			true
		| _ -> check_type is_suspend_type t

(**
	Check if metadata contains `@:suspend` meta
*)
let rec contains_suspend_meta metadata =
	match metadata with
		| [] -> false
		| (Meta.Custom ":suspend", _, _) :: _ -> true
		| _ :: rest -> contains_suspend_meta rest


(**
	Check if provided `callee` expression being called will suspend execution of a coroutine.
*)
let is_suspending callee =
	is_suspend_type callee.etype
	|| match callee.eexpr with
		| TField (_, FInstance (_, _, field))
		| TField (_, FStatic (_, field))
		| TField (_, FAnon field)
		| TField (_, FClosure (_, field)) ->
			is_suspend_type field.cf_type
			|| contains_suspend_meta field.cf_meta
		| _ -> false

let rec is_state_machine_class cls =
	cls.cl_path = state_machine_type_path
	|| match cls.cl_super with
		| None -> false
		| Some (super, _) -> is_state_machine_class super

let rec is_throwing_state_machine_class cls =
	cls.cl_path = throwing_state_machine_type_path
	|| match cls.cl_super with
		| None -> false
		| Some (super, _) -> is_throwing_state_machine_class super

let is_state_machine_type t =
	match follow t with
		| TInst (cls, _) -> is_state_machine_class cls
		| _ -> false

let is_throwing_state_machine_type t =
	match follow t with
		| TInst (cls, _) -> is_throwing_state_machine_class cls
		| _ -> false

let is_function_expr e =
	match (reveal_expr e).eexpr with
		| TFunction _ -> true
		| _ -> false

let is_function_type t =
	match follow t with
		| TFun _ -> true
		| _ -> false

let check_expr predicate e =
	match e.eexpr with
		| TConst _ | TLocal _ | TBreak | TContinue | TTypeExpr _ | TIdent _ ->
			false
		| TArray (e1,e2) | TBinop (_,e1,e2) | TFor (_,e1,e2) | TWhile (e1,e2,_) ->
			predicate e1 || predicate e2;
		| TThrow e | TField (e,_) | TEnumParameter (e,_,_) | TEnumIndex e | TParenthesis e
		| TCast (e,_) | TUnop (_,_,e) | TMeta(_,e) ->
			predicate e
		| TArrayDecl el | TNew (_,_,el) | TBlock el ->
			List.exists predicate el
		| TObjectDecl fl ->
			List.exists (fun (_,e) -> predicate e) fl
		| TCall (e,el) ->
			predicate e ||  List.exists predicate el
		| TVar (_,eo) | TReturn eo ->
			(match eo with None -> false | Some e -> predicate e)
		| TFunction fu ->
			predicate fu.tf_expr
		| TIf (e,e1,e2) ->
			predicate e || predicate e1 || (match e2 with None -> false | Some e -> predicate e)
		| TSwitch (e,cases,def) ->
			predicate e
			|| List.exists (fun (el,e2) -> List.exists predicate el || predicate e2) cases
			|| (match def with None -> false | Some e -> predicate e)
		| TTry (e,catches) ->
			predicate e || List.exists (fun (_,e) -> predicate e) catches

let is_null_expr expr =
	match expr with
		| { eexpr = TConst TNull } -> true
		| _ -> false

let get_func_args fn_type pos =
	match follow fn_type with
		| TFun (args, _) -> args
		| _ -> fail pos __POS__

let rec same_types t1 t2 =
	match follow t1, follow t2 with
		| TEnum ({ e_path = path1 }, params1), TEnum ({ e_path = path2 }, params2)
		| TInst ({ cl_path = path1}, params1), TInst ({ cl_path = path2}, params2)
		| TAbstract ({ a_path = path1}, params1), TAbstract ({ a_path = path2}, params2) when path1 = path2 ->
			let rec traverse_params p1 p2 =
				match p1, p2 with
					| [], [] -> true
					| t1 :: rest1, t2 :: rest2 when same_types t1 t2 ->
						traverse_params rest1 rest2
					| _ -> false
			in
			traverse_params params1 params2
		| TDynamic t1, TDynamic t2 ->
			same_types t1 t2
		| TFun (args1, ret1), TFun (args2, ret2) when same_types ret1 ret2 ->
			let rec traverse args1 args2 =
				match args1, args2 with
					| [], [] -> true
					| (_, opt1, t1) :: rest1, (_, opt2, t2) :: rest2 when opt1 = opt2 && same_types t1 t2 ->
						traverse rest1 rest2
					| _ -> false
			in
			traverse args1 args2
		| TAnon { a_fields = fields1 }, TAnon { a_fields = fields2 } ->
			let same = ref false in
			PMap.iter
				(fun name1 f1 ->
					if not !same then
						try
							let f2 = PMap.find name1 fields2 in
							if f1.cf_name = f2.cf_name && same_types f1.cf_type f2.cf_type then
								same := true
						with Not_found -> ()
				)
				fields1;
			!same
		| _ -> false

(**
	Check if `generated_type` is of `coro.Coroutine.Generated` and matches the arguments list of `coroutine_type`.
*)
let is_generated_for_coroutine generated_type coroutine_type pos =
	let rec traverse gen_args coro_args =
		match gen_args, coro_args with
			(* without resumeValue *)
			| [(_, _, sm_type)], []
			(* with resumeValue *)
			| [(_, _, sm_type); (_, _, _)], []
				when is_state_machine_type sm_type -> true
			| (_, _, g) :: rest_gen, (_, _, c) :: rest_coro when same_types g c ->
				traverse rest_gen rest_coro
			| _ -> false
	in
	traverse (get_func_args generated_type pos) (get_func_args coroutine_type pos)

(**
	Search `coroutines` for a croroutine, which can be transformed to be passed to the `generated_type`
*)
let rec find_coroutine_for_generated coroutines generated_type pos =
	match coroutines with
		| [] -> None
		| { etype = t } as e :: rest ->
			if is_generated_for_coroutine generated_type t pos then
				Some e
			else
				find_coroutine_for_generated rest generated_type pos

(**
	Check if `types` contains a `Coroutine.Generated<T>`, where `T` matches the `coroutine_type`
	and there is no value passed to that type in `args`.
*)
let rec is_expecting_for_generated coroutine_type args types pos =
	match args, types with
		| [], t :: _ when is_generated_type t && is_generated_for_coroutine t coroutine_type pos -> true
		| arg :: _, t :: _ when is_generated_type t && is_null_expr arg && is_generated_for_coroutine t coroutine_type pos -> true
		| [], _ :: rest_types ->
			is_expecting_for_generated coroutine_type [] rest_types pos
		| _ :: rest_args, _ :: rest_types ->
			is_expecting_for_generated coroutine_type rest_args rest_types pos
		| _ -> false

let is_transformable_coroutine arg t rest_args rest_types =
	is_function_expr arg
	&& is_coroutine_type t
	&& is_expecting_for_generated t rest_args rest_types arg.epos

(**
	Check if `args` has function declaration at the position where `types` expects `Coroutine` type.
*)
let contains_transformable_coroutines args types =
	let rec traverse types args =
		match types, args with
			| coro_type :: rest_types, arg :: rest_args ->
				if is_transformable_coroutine arg coro_type rest_args rest_types then
					true
				else
					traverse rest_types rest_args
			| _ -> false
	in
	traverse types args

(**
	Get the list of types of constructor arguments with generic type parameters substituted with `params`.
	Returns empty list if the class does not have a constructor.
*)
let get_constructor_arg_types cls params =
	match cls.cl_constructor with
		| Some { cf_type = TFun (arg_types, _) } ->
			(match arg_types with
				| [] -> []
				| _ -> List.map (fun (_, _, t) -> apply_params cls.cl_params params t) arg_types
			)
		| _ -> []

(**
	Check if specified expression contains subexpressions of anon function declaration passed to places where `coro.Coroutine` is expected.
*)
let rec contains_coroutines expr =
	match expr.eexpr with
		| TNew (cls, params, arg_exprs) ->
			let arg_types = get_constructor_arg_types cls params in
			contains_transformable_coroutines arg_exprs arg_types
		| TCall (callee, arg_exprs) when is_function_type callee.etype ->
			let arg_types = List.map (fun (_, _, t) -> t) (get_func_args callee.etype expr.epos) in
			contains_transformable_coroutines arg_exprs arg_types
		| _ -> check_expr contains_coroutines expr

let rec contains_suspending_calls expr =
	match expr.eexpr with
		| TCall (callee, _) when is_suspending callee -> true
		| _ -> check_expr contains_suspending_calls expr

(**
	Collect all the local variables used or declared in this expression.
*)
let collect_local_vars expr =
	let result = Hashtbl.create 10 in
	let add_var v = Hashtbl.replace result v.v_name v in
	let rec traverse e =
		match e.eexpr with
			| TLocal v | TVar (v, None) ->
				add_var v
			| TVar (v, Some e) ->
				add_var v;
				traverse e
			| _ ->
				iter traverse e
	in
	traverse expr;
	result

let block_expr pos exprs =
	let t =
		match exprs with
			| [] -> mk_mono()
			| _ -> (List.hd (List.rev exprs)).etype
	in
	{
		epos = pos;
		etype = t;
		eexpr = TBlock exprs;
	}

(**
	Returns a single expression containing all the provided expressions.
	For the list of a single expression returns that expression.
	For the empty list returns empty block.
*)
let merge_exprs exprs =
	match exprs with
		| [expr] -> expr
		| { epos = pos } :: _ -> block_expr pos exprs
		| [] -> block_expr null_pos []

type sm_state = {
	ss_num : int;
	(* the position of the suspending call, after which this state starts *)
	mutable ss_suspend_pos : pos option;
	mutable ss_expr : texpr option;
	ss_own_locals : (int, tvar) Hashtbl.t;
}

type coro_types = {
	ct_sm_cls : tclass;
	ct_throwing_sm_cls : tclass;
	ct_exception_cls : tclass;
	ct_state_exception_cls : tclass;
	ct_sm_state_field : tclass_field;
	ct_sm_next_state_field : tclass_field;
	ct_sm_throw_on_resume_field : tclass_field;
	ct_sm_return_value_field : tclass_field;
	ct_string : t;
	ct_int : t;
	ct_void : t;
	ct_bool : t;
	ct_null : t->t;
}

type states_context = {
	mutable sc_next : sm_state;
	mutable sc_current : sm_state;
}

let duplicate_ctx ctx =
	{
		sc_current = ctx.sc_current;
		sc_next = ctx.sc_next;
	}

let const_int_expr coro_types value pos =
	{
		epos = pos;
		etype = coro_types.ct_int;
		eexpr = TConst (TInt (Int32.of_int value));
	}

let const_bool_expr coro_types value pos =
	{
		epos = pos;
		etype = coro_types.ct_bool;
		eexpr = TConst (TBool value);
	}

let invalid_state_expr coro_types state_expr pos =
	let mono = mk_mono ()
	and const_null = null_expr coro_types.ct_string pos coro_types.ct_null in
	{
		etype = mono; epos = pos;
		eexpr = TBlock [
			{
				etype = mono; epos = pos;
				eexpr = TThrow {
					etype = TInst(coro_types.ct_state_exception_cls, []); epos = pos;
					eexpr = TNew(
						coro_types.ct_state_exception_cls,
						[],
						[state_expr; const_null]
					)
				}
			}
		]
	}

let local_expr var pos =
	{ epos = pos; etype = var.v_type; eexpr = TLocal var; }

let rec ends_with_return expr =
	match (reveal_expr expr).eexpr with
		| TReturn _ -> true
		| TBlock [] -> false
		| TBlock exprs -> ends_with_return (List.hd (List.rev(exprs)))
		| TIf (_, if_block, Some else_block) -> ends_with_return if_block && ends_with_return else_block
		| _ -> false

let rec is_var_modified_in_expr var expr =
	match expr.eexpr with
		| TBinop (OpAssign, { eexpr = TLocal var }, _) -> true
		| TBinop (OpAssignOp _, { eexpr = TLocal var }, _) -> true
		| _ -> check_expr (is_var_modified_in_expr var) expr

let rec is_var_modified_in_exprs var exprs =
	match exprs with
		| [] -> false
		| current :: rest ->
			is_var_modified_in_expr var current
			|| is_var_modified_in_exprs var rest

type try_data = {
	mutable td_catches : (tvar * texpr) list;
	mutable td_states : sm_state list;
	mutable td_nested_trys : try_data list;
}

let rec remove_nulls args =
	match args with
		| { eexpr = TConst TNull } :: rest -> remove_nulls rest
		| _ -> args

(**
 	Build the final expression of a state machine based on the expression of a coroutine
*)
class sm_builder coro_types coroutine_expr generated_type gen_var =
	let entry_pos = coroutine_expr.epos in
	let sm_var, resume_value_var =
		match (reveal_expr coroutine_expr).eexpr, follow generated_type with
			| TFunction fn, TFun (gen_arg_types, _) ->
				let additional_args_count = (List.length gen_arg_types) - (List.length fn.tf_args) in
				(match additional_args_count, List.rev gen_arg_types with
					(* without resumeValue *)
					| 1, (_, _, sm_type) :: _ ->
						(gen_var "sm" sm_type entry_pos, None)
					(* with resumeValue *)
					| 2, (_, _, resume_type) :: (_, _, sm_type) :: _ ->
						(gen_var "sm" sm_type entry_pos, Some (gen_var "resumeValue" resume_type entry_pos))
					| _ -> fail entry_pos __POS__
				)
			| _ ->
				fail entry_pos __POS__
	in
	object(self)
		val is_throwing_state_machine = is_throwing_state_machine_type sm_var.v_type
		val mutable states = []
		val mutable latest_state_num = -1
		val local_state = gen_var "state" coro_types.ct_int entry_pos;
		(** Local variables, which are declared in the body of this coroutine *)
		val own_locals = Hashtbl.create 10
		(** Local variables used in several states*)
		val shared_locals = Hashtbl.create 10
		(** Own local variables which are captured and modified in closures of this coroutine *)
		val captured_modified_locals = Hashtbl.create 5
		val mutable has_goto_state = false
		val mutable next_loop_id = 1
		val mutable loops_hierarchy = [0]
		val loops_states = Hashtbl.create 5
		val mutable current_try = None
		val mutable root_trys = []
		(**
		*)
		method build : texpr =
			match (reveal_expr coroutine_expr).eexpr with
				| TFunction fn ->
					let additional_args =
						if self#has_resume_value_var then [(sm_var, None); (self#get_resume_value_var, None)]
						else [(sm_var, None)]
					in
					(* print_endline "---";
					print_endline (s_expr (fun t -> "") fn.tf_expr); *)
					self#collect_states fn;
					let fn =
						{
							tf_type = coro_types.ct_int;
							tf_args = fn.tf_args @ additional_args;
							tf_expr = self#build_body;
							(* tf_expr = fn.tf_expr; *)
						}
					in
					(* (string * bool * t) list * t *)
					let signature =
						(
							List.map (fun (arg, _) -> (arg.v_name, false, arg.v_type)) fn.tf_args,
							fn.tf_type
						)
					in
					let exprs =
						ref [{  coroutine_expr with
							eexpr = TFunction fn;
							etype = TFun signature
						}]
					in
					Hashtbl.iter
						(fun _ v ->
							exprs :=
								{ eexpr = TVar (v, None); epos = entry_pos; etype = coro_types.ct_void; }
								:: !exprs
						)
						shared_locals;
					block_expr entry_pos !exprs
				| _ -> fail entry_pos __POS__
		(**
			Split expression into states by suspending calls.
		*)
		method private collect_states fn =
			let state_num_final =
				if is_void_type fn.tf_type then
					state_exit_code.sec_finished
				else
					state_exit_code.sec_finished
			in
			let ctx =
				{
					sc_current = self#create_state (Some fn.tf_expr.epos);
					sc_next = {
						ss_num = state_num_final;
						ss_suspend_pos = None;
						ss_expr = None;
						ss_own_locals = Hashtbl.create 0;
					};
				}
			in
			let current = ctx.sc_current in
			current.ss_expr <- Some (self#map_expr ctx fn.tf_expr);
			ctx.sc_current.ss_expr <- match ctx.sc_current.ss_expr with
				| None ->
					Some (self#suspend_expr state_exit_code.sec_finished entry_pos)
				| Some expr ->
					if ends_with_return expr then
						Some expr
					else
						Some (block_expr expr.epos [
							expr;
							self#suspend_expr state_exit_code.sec_finished expr.epos;
						])
		(**
			Create empty state
		*)
		method private create_state suspend_pos =
			latest_state_num <- latest_state_num + 1;
			let state =
				{
					ss_num = latest_state_num;
					ss_suspend_pos = suspend_pos;
					ss_expr = None;
					ss_own_locals = Hashtbl.create 5;
				}
			in
			(match current_try with
				| None -> states <- states @ [state];
				| Some current_try -> current_try.td_states <- current_try.td_states @ [state]
			);
			state
		(**
			Create id for a loop
		*)
		method private create_loop_id =
			let result = next_loop_id in
			next_loop_id <- next_loop_id + 1;
			result
		(**
			Store the condition state and the exit state of a loop.
		*)
		method private register_loop id condition_state_num exit_state_num =
			if Hashtbl.mem loops_states id then
				fail ~msg:"Trying to register the same loop again" null_pos __POS__
			else
				Hashtbl.add loops_states id (condition_state_num, exit_state_num)
		(**
			A hub which selects the correct method to split expression into states and executes selected method.
		*)
		method private map_expr ctx expr =
			match expr.eexpr with
				| TField (target, field_access) ->
					self#map_one_expr ctx expr target (fun target -> TField (target, field_access))
				| TVar (v, init_expr) ->
					self#map_var_declaration ctx expr v init_expr
				| TLocal v ->
					self#map_local_var ctx expr v
				| TCall (callee, args) ->
					if is_suspending callee then
						self#map_suspending_call ctx expr callee args
					else
						self#map_call ctx expr callee args
				| TBlock exprs ->
					self#map_block ctx expr exprs
				| TReturn None ->
					self#map_return_void ctx expr
				| TReturn (Some e) ->
					self#map_return_value ctx expr e
				| TIf (condition, if_block, else_block) ->
					if not (same_types expr.etype coro_types.ct_void) && contains_suspending_calls expr then
						self#map_if_as_value ctx expr condition if_block else_block
					else
						self#map_if ctx expr condition if_block else_block
				| TBinop (OpAssignOp op, left, right) ->
					self#map_assign_op ctx expr op left right
				| TBinop (op, left, right) ->
					self#map_binop ctx expr op left right
				| TArray (array, index) ->
					self#map_two_exprs ctx expr array index (fun array index -> TArray (array, index))
				| TParenthesis e ->
					self#map_one_expr ctx expr e (fun e -> TParenthesis e)
				| TObjectDecl fields ->
					self#map_object_declaration ctx expr fields
				| TArrayDecl [] ->
					expr
				| TArrayDecl exprs ->
					self#map_list_exprs ctx expr exprs (fun exprs -> TArrayDecl exprs)
				| TNew (_, _, []) ->
					expr
				| TNew (cls, params, args) ->
					self#map_list_exprs ctx expr args (fun args -> TNew (cls, params, args))
				| TUnop (op, flag, target) ->
					self#map_one_expr ctx expr target (fun target -> TUnop (op, flag, target))
				| TFunction fn ->
					self#map_function ctx expr fn
				(* At this point `for` loops should be transformed into `while` loops by the compiler *)
				| TFor _ ->
					fail ~msg:"Unexpected `for` loop" expr.epos __POS__
				| TWhile (condition, body, flag) ->
					self#map_while ctx expr condition body flag
				| TMeta ((Meta.Ast, _, _), ({ eexpr = TSwitch _ } as expr)) ->
					self#map_expr ctx expr
				| TSwitch (target, cases, default) ->
					if not (same_types expr.etype coro_types.ct_void) && contains_suspending_calls expr then
						self#map_switch_as_value ctx expr target cases default
					else
						self#map_switch ctx expr target cases default
				| TTry (try_block, catches) ->
					if not (same_types expr.etype coro_types.ct_void) && contains_suspending_calls expr then
						self#map_try_as_value ctx expr try_block catches
					else
						self#map_try ctx expr try_block catches
				(* | TTry of texpr * (tvar * texpr) list *)
				| TBreak ->
					self#map_loop_control ctx expr loop_control_break
				| TContinue ->
						self#map_loop_control ctx expr loop_control_continue
				| TThrow e ->
					self#map_one_expr ctx expr e (fun e -> TThrow e)
				| TCast (e, t) ->
					self#map_one_expr ctx expr e (fun e -> TCast (e, t))
				| TMeta (meta, e) ->
					self#map_one_expr ctx expr e (fun e -> TMeta (meta, e))
				| TEnumParameter (e, field, i) ->
					self#map_one_expr ctx expr e (fun e -> TEnumParameter (e, field, i))
				| TEnumIndex e ->
					self#map_one_expr ctx expr e (fun e -> TEnumIndex e)
				| _ ->
					map_expr (self#map_expr ctx) expr
		(**
		*)
		method private transform_exprs ctx exprs : (texpr list * texpr list) =
			match exprs with
				| [] -> ([], [])
				| [expr] ->
					let initial_state = ctx.sc_current
					and mapped_expr = self#map_expr ctx expr in
					let final_state = ctx.sc_current in
					if initial_state == final_state then
						([], [mapped_expr])
					else
						([mapped_expr], [self#state_resume_expr final_state expr])
				| first :: rest ->
					let initial_state = ctx.sc_current
					and mapped_first = self#map_expr ctx first in
					let second_state = ctx.sc_current in
					let result_rest, mapped_rest = self#transform_exprs ctx rest in
					let final_state = ctx.sc_current in
					(* no suspension at all *)
					if initial_state == final_state then
						([], mapped_first :: mapped_rest)
					(* expressions contain suspending calls *)
					else
						(* no suspension in the first expression *)
						if initial_state == second_state then
							match (reveal_expr first).eexpr with
								| TTypeExpr _ | TConst _ ->
									(result_rest, first :: mapped_rest)
								| TLocal var when not (self#could_be_changed_outside var) && not (is_var_modified_in_exprs var rest) ->
									self#register_var_usage var final_state;
									(result_rest, mapped_first :: mapped_rest)
								| _ ->
									let _, first_tmp =
										self#declare_cross_state_tmp_var first.etype first.epos initial_state final_state
									in
									let result =
										(self#assign_expr first_tmp mapped_first mapped_first.epos) :: result_rest
									in
									(result, first_tmp :: mapped_rest)
						(* first expression suspends *)
						else
							begin
								let first_resume = self#state_resume_expr second_state first in
								match result_rest with
									(* the rest of expressions don't suspend *)
									| [] ->
										([mapped_first], first_resume :: mapped_rest)
									(* there are suspension calls in the rest of expressions *)
									| _ ->
										let _, first_tmp =
											self#declare_cross_state_tmp_var first.etype first.epos second_state final_state
										in
										let second_exprs =
											(self#assign_expr first_tmp first_resume first.epos) :: result_rest
										in
										second_state.ss_expr <- Some (block_expr first.epos second_exprs);
										([mapped_first], first_tmp :: mapped_rest)
							end
		(**
		*)
		method private map_one_expr ctx origin expr factory =
			match self#transform_exprs ctx [expr] with
				| [], [expr] -> { origin with eexpr = factory expr }
				| result_exprs, [expr] ->
					ctx.sc_current.ss_expr <- Some { origin with eexpr = factory expr };
					merge_exprs result_exprs
				| _ -> fail origin.epos __POS__
		(**
		*)
		method private map_two_exprs ctx origin first second factory : texpr =
			match self#transform_exprs ctx [first; second] with
				| [], [first; second] -> { origin with eexpr = factory first second }
				| result_exprs, [first; second] ->
					ctx.sc_current.ss_expr <- Some { origin with eexpr = factory first second };
					merge_exprs result_exprs
				| _ -> fail origin.epos __POS__
		(**
		*)
		method private map_three_exprs ctx origin first second third factory : texpr =
			match self#transform_exprs ctx [first; second; third] with
				| [], [first; second; third] -> { origin with eexpr = factory first second third }
				| result_exprs, [first; second; third] ->
					ctx.sc_current.ss_expr <- Some { origin with eexpr = factory first second third };
					merge_exprs result_exprs
				| _ -> fail origin.epos __POS__
		(**
		*)
		method private map_list_exprs ctx origin exprs factory : texpr =
			match self#transform_exprs ctx exprs with
				| [], exprs -> { origin with eexpr = factory exprs }
				| result_exprs, exprs ->
					ctx.sc_current.ss_expr <- Some { origin with eexpr = factory exprs };
					merge_exprs result_exprs
		(**
		*)
		method private map_loop_control ctx origin control =
			let loop_id = List.hd loops_hierarchy in
			{
				epos = origin.epos; etype = coro_types.ct_int;
				eexpr = TMeta (
					(Meta.Custom control, [], origin.epos),
					const_int_expr coro_types loop_id origin.epos
				)
			}
		(**
		*)
		method private map_while ctx origin condition body flag =
			if not (contains_suspending_calls origin) then
				{ origin with eexpr = TWhile (self#map_expr ctx condition, self#map_expr ctx body, flag) }
			else begin
				let new_state = self#create_state None in
				ctx.sc_current <- new_state;
				let loop_id = self#create_loop_id in
				loops_hierarchy <- loop_id :: loops_hierarchy;
				let mapped =
					match flag with
						| NormalWhile -> self#map_normal_while ctx origin condition body loop_id
						| DoWhile -> self#map_do_while ctx origin condition body loop_id
				in
				new_state.ss_expr <- Some mapped;
				loops_hierarchy <- List.tl loops_hierarchy;
				self#goto_state_expr new_state.ss_num origin.epos
			end
		(**
			Transform
			```
			while(suspend1()) {
				suspend2();
			}
			```
			into
			```
			suspend1()
			<goto next state>
			if(resumeValue) {
				suspend2();
				<goto `suspend1`>
			}
			```
		*)
		method private map_normal_while ctx origin condition body loop_id =
			let initial_state = ctx.sc_current
			and mapped_condition = self#map_expr ctx condition in
			let condition_state = ctx.sc_current
			and mapped_body = self#map_expr ctx body in
			let final_state = ctx.sc_current in
			let repeat = self#goto_state_expr initial_state.ss_num origin.epos in
			let mapped_body =
				(* body does not suspend  *)
				if condition_state == final_state then
					merge_exprs [mapped_body; repeat]
				(* body suspends  *)
				else begin
					final_state.ss_expr <- Some (match final_state.ss_expr with
						| None -> repeat
						| Some leftovers ->
							merge_exprs [leftovers; repeat]
					);
					mapped_body
				end
			in
			ctx.sc_current <- self#create_state None;
			self#register_loop loop_id initial_state.ss_num ctx.sc_current.ss_num;
			let else_block = Some (self#goto_state_expr ctx.sc_current.ss_num origin.epos) in
			(* condition does not suspend *)
			if initial_state == condition_state then
				{ condition with eexpr = TIf (mapped_condition, mapped_body, else_block) }
			(* condition suspends *)
			else
				begin
					let condition =
						match condition_state.ss_expr with
							| None -> self#resume_value_expr ~etype:condition.etype condition.epos
							| Some leftovers -> leftovers
					in
					condition_state.ss_expr <- Some {
						epos = condition.epos; etype = coro_types.ct_void;
						eexpr = TIf (condition, mapped_body, else_block)
					};
					mapped_condition
				end
		(**
			Transforms
			```
			do {
				suspend1();
			} while(suspend2())
			```
			into
			```
			suspend1();
			<goto next state>
			suspend2();
			<goto next state>
			if(resumeValue) <goto `suspend1`>
			```
		*)
		method private map_do_while ctx origin condition body loop_id =
			let initial_state = ctx.sc_current
			and mapped_body = self#map_expr ctx body in
			let body_state = ctx.sc_current
			and condition_state = self#create_state None in
			ctx.sc_current <- condition_state;
			let mapped_condition = self#map_expr ctx condition in
			condition_state.ss_expr <- Some mapped_condition;
			let final_state = ctx.sc_current in
			let repeat = self#goto_state_expr initial_state.ss_num origin.epos in
			let result =
				let goto_condition = self#goto_state_expr condition_state.ss_num condition.epos in
				(* body does not suspend *)
				if initial_state == body_state then
					merge_exprs [mapped_body; goto_condition]
				(* body suspends *)
				else begin
					body_state.ss_expr <- Some (match body_state.ss_expr with
						| None -> goto_condition
						| Some leftovers -> merge_exprs [leftovers; goto_condition]
					);
					mapped_body
				end
			in
			let condition =
				match final_state.ss_expr with
					| None -> self#resume_value_expr ~etype:condition.etype condition.epos
					| Some leftovers -> leftovers
			in
			ctx.sc_current <- self#create_state None;
			self#register_loop loop_id condition_state.ss_num ctx.sc_current.ss_num;
			let else_block = Some (self#goto_state_expr ctx.sc_current.ss_num origin.epos) in
			final_state.ss_expr <- Some {
				epos = condition.epos; etype = coro_types.ct_void;
				eexpr = TIf (condition, repeat, else_block)
			};
			result
		(**
		*)
		method private map_object_declaration ctx origin fields =
			match fields with
				| [] -> origin
				| _ ->
					let (names, exprs) = List.split fields in
					self#map_list_exprs ctx origin exprs (fun exprs -> TObjectDecl (List.combine names exprs))
		(**
			Handle closures (e.g. capturing variables)
		*)
		method private map_function ctx origin fn =
			let rec traverse expr =
				match expr.eexpr with
					| TBinop (OpAssign, ({ eexpr = TLocal var } as left), right)
					| TBinop (OpAssignOp _, ({ eexpr = TLocal var } as left), right) ->
						if (Hashtbl.mem own_locals var.v_id) && not (Hashtbl.mem captured_modified_locals var.v_id) then
							Hashtbl.add captured_modified_locals var.v_id var;
						traverse left;
						traverse right;
					| TLocal var -> self#register_var_usage var ctx.sc_current
					| _ -> iter traverse expr
			in
			traverse fn.tf_expr;
			origin
		(**
			Transforms `left += suspend()` into `left = left + suspend()` if needed
			and then invokes `map_binop` on that expression.
		*)
		method private map_assign_op ctx origin op left right =
			if not (contains_suspending_calls right) then
				self#map_binop ctx origin (OpAssignOp op) left right
			else begin
				let store_tmp expr =
					let tmp_var = gen_var "tmp" expr.etype expr.epos in
					let init_tmp = { etype = coro_types.ct_void; epos = expr.epos; eexpr = TVar (tmp_var, Some expr)}
					and stored = local_expr tmp_var expr.epos in
					(init_tmp, stored)
				in
				let (left, stored, prepend_exprs) =
					match (reveal_expr left).eexpr with
						| TLocal var ->
							let init_tmp, tmp = store_tmp left in
							(left, tmp, [init_tmp])
						| TField (target, field_access) ->
							(match (reveal_expr target).eexpr with
								| TTypeExpr _ | TConst _ ->
									let init_tmp, tmp = store_tmp left in
									(left, tmp, [init_tmp]);
								| _ ->
									let init_target, target = store_tmp target in
									let left = { left with eexpr = TField (target, field_access) } in
									let init_stored, stored = store_tmp left in
									(left, stored, [init_target; init_stored])
							)
						| TArray (array, index) ->
							let init_array, array = store_tmp array
							and init_index, index =
								match (reveal_expr index).eexpr with
									| TConst _ -> (block_expr index.epos [], index)
									| _ -> store_tmp index
							in
							let left = { left with eexpr = TArray (array, index) } in
							let init_stored, stored = store_tmp left in
							(left, stored, [init_array; init_index; init_stored])
						| _ -> fail left.epos __POS__
				in
				let right = { right with eexpr = TBinop (op, stored, right) } in
				let binop = { origin with eexpr = TBinop (OpAssign, left, right) } in
				self#map_expr ctx (block_expr origin.epos (prepend_exprs @ [binop]))
			end
		(**
		*)
		method private map_binop ctx origin op left right =
			match op with
				| OpAssign | OpAssignOp _ ->
					(match left.eexpr with
						| TLocal var ->
							self#map_one_expr ctx origin right (fun right ->
								self#register_var_usage var ctx.sc_current;
								TBinop (op, left, right)
							)
						| TField (target, field_access) ->
							let factory target right =
								TBinop (op, { left with eexpr = TField (target, field_access)}, right)
							in
							self#map_two_exprs ctx origin target right factory
						| TArray (array, index) ->
							let factory array index right =
								TBinop (op, { left with eexpr = TArray (array, index)}, right)
							in
							self#map_three_exprs ctx origin array index right factory
						| _ ->
							self#map_two_exprs ctx origin left right (fun left right -> TBinop (op, left, right))
					)
				| OpBoolOr when contains_suspending_calls right ->
					self#map_expr ctx { origin with eexpr = TIf (left, { left with eexpr = TConst (TBool true) }, Some right) }
				| OpBoolAnd when contains_suspending_calls right ->
					self#map_expr ctx { origin with eexpr = TIf (left, right, Some { left with eexpr = TConst (TBool false) }) }
				| _ ->
					self#map_two_exprs ctx origin left right (fun left right -> TBinop (op, left, right))
		(**
			Transform `switch(...)...` used as a value.
		*)
		method private map_switch_as_value ctx origin target cases default =
			let var = gen_var "tmp" origin.etype origin.epos in
			let save_to_var expr = { expr with eexpr = TBinop (OpAssign, local_expr var expr.epos, expr) } in
			let cases =
				List.map (fun (conditions, expr) -> (conditions, save_to_var expr)) cases
			and default =
				match default with
					| None -> None
					| Some expr -> Some (save_to_var expr)
			in
			self#map_expr
				ctx
				{ origin with eexpr = TBlock [
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TVar (var, None) };
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TSwitch (target, cases, default) };
					{ origin with eexpr = TLocal var}
				]}
		(**
			Transform `switch(...)...`
		*)
		method private map_switch ctx origin target cases default =
			let exit_state = ref None in
			let result =
				self#map_one_expr ctx origin target (fun target ->
					let target_state = ctx.sc_current in
					let next_state = ref target_state in
					let transform_case ctx expr =
						self#map_one_expr ctx expr expr (fun expr ->
							if ctx.sc_current == target_state then
								expr.eexpr
							else begin
								if !next_state == target_state then next_state := self#create_state None;
								TBlock [expr; self#goto_state_expr !next_state.ss_num expr.epos]
							end
						)
					in
					let cases =
						List.map
							(fun (conditions, expr) -> (conditions, transform_case (duplicate_ctx ctx) expr))
							cases
					and default =
						match default with
							| None -> None
							| Some expr -> Some (transform_case (duplicate_ctx ctx) expr)
					in
					if target_state != !next_state then begin
						exit_state := Some !next_state;
					end;
					TSwitch (target, cases, default)
				)
			in
			match !exit_state with
				| None -> result
				| Some state ->
					ctx.sc_current <- state;
					merge_exprs [result; self#goto_state_expr state.ss_num origin.epos]
		(**
			Transform `try...catch...` used as a value.
		*)
		method private map_try_as_value ctx origin try_block catches =
			let var = gen_var "tmp" origin.etype origin.epos in
			let save_to_var expr = { expr with eexpr = TBinop (OpAssign, local_expr var expr.epos, expr) } in
			let try_block =  save_to_var try_block
			and catches =
				List.map (fun (catch_var, expr) -> (catch_var, save_to_var expr)) catches
			in
			self#map_expr
				ctx
				{ origin with eexpr = TBlock [
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TVar (var, None) };
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TTry (try_block, catches) };
					{ origin with eexpr = TLocal var}
				]}
		(**
			Transform `try {...} catch(e:Some) {...}`
			into
			```
			<goto the first try-state>
			try {
				<first try-state>
				...
				<last try-state>
				...
				continue; //this is required to go back to the state selection
			} catch(tmp:Some) {
				var e = tmp;
				<goto catch-state>
			}
			```
		*)
		method private map_try ctx origin try_block catches =
			let initial_state = ctx.sc_current in
			let exit_state = self#create_state None in
			let parent_try = current_try
			and this_try = { td_catches = []; td_states = []; td_nested_trys = []; } in
			current_try <- Some this_try;
			(match parent_try with
				| None -> root_trys <- root_trys @ [this_try]
				| Some parent_try -> parent_try.td_nested_trys <- this_try :: parent_try.td_nested_trys
			);
			let try_state = self#create_state None in
			ctx.sc_current <- try_state;
			try_state.ss_expr <- Some (
				self#map_one_expr ctx try_block try_block (fun expr ->
					TBlock [
						expr;
						self#goto_state_expr exit_state.ss_num origin.epos
					]
				)
			);
			current_try <- parent_try;
			(*
				Patch catch blocks with tmp vars.
				E.g. transforms `catch(e:Dynamic) {...` into `catch(tmp:Dynamic) { var e = tmp; ...`
			*)
			let catches =
				List.map
					(fun (var, expr) ->
						if not (contains_suspending_calls expr) then
							(var, merge_exprs [self#map_expr ctx expr; self#goto_state_expr ~add_continue:true exit_state.ss_num origin.epos])
						else begin
							let tmp = gen_var "tmp" var.v_type var.v_pos in
							self#register_var_declaration tmp initial_state;
							self#register_var_declaration var initial_state;
							let catch_state = self#create_state None in
							ctx.sc_current <- catch_state;
							catch_state.ss_expr <- Some (
								self#map_one_expr ctx expr expr (fun expr ->
									TBlock [
										expr;
										self#goto_state_expr  exit_state.ss_num origin.epos
									])
							);
							let catch_result =
								block_expr expr.epos [
									self#assign_expr (local_expr var var.v_pos) (local_expr tmp var.v_pos) var.v_pos;
									self#goto_state_expr catch_state.ss_num expr.epos;
									{ etype = coro_types.ct_void; epos = expr.epos; eexpr = TContinue }
								]
							in
							self#register_var_usage var catch_state;
							(tmp, catch_result)
						end
					)
					catches
			in
			this_try.td_catches <- catches;
			ctx.sc_current <- exit_state;
			self#goto_state_expr try_state.ss_num try_block.epos
		(**
			Transform `if(...) ... ele ...` passed as a value.
		*)
		method private map_if_as_value ctx origin condition if_block else_block =
			let var = gen_var "tmp" origin.etype origin.epos in
			let if_block =
				{ if_block with eexpr = TBinop (OpAssign, local_expr var if_block.epos, if_block) }
			and else_block =
				match else_block with
					| None -> fail origin.epos __POS__
					| Some expr ->
						Some { expr with eexpr = TBinop (OpAssign, local_expr var expr.epos, expr) }
			in
			self#map_expr
				ctx
				{ origin with eexpr = TBlock [
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TVar (var, None) };
					{ epos = origin.epos; etype = coro_types.ct_void; eexpr = TIf (condition, if_block, else_block) };
					{ origin with eexpr = TLocal var}
				]}
		(**
			Transform `if(...) ... else ...`
		*)
		method private map_if ctx origin condition if_block else_block =
			let current = ctx.sc_current
			and condition = self#map_expr ctx condition
			and if_state = ctx.sc_current in
			let next_state = ref if_state in
			let if_block =
				let if_block = self#map_expr ctx if_block in
				(* `if` block contains suspending expressions *)
				if ctx.sc_current != if_state then begin
					next_state := self#create_state None;
					ctx.sc_current.ss_expr <- match ctx.sc_current.ss_expr with
						| None ->
							Some (self#goto_state_expr !next_state.ss_num origin.epos)
						| Some leftovers ->
							Some (block_expr origin.epos [
								leftovers;
								self#goto_state_expr !next_state.ss_num origin.epos;
							])
				end;
				if_block
			in
			ctx.sc_current <- if_state;
			let else_block =
				match else_block with
					| None -> None
					| Some else_block ->
						let else_block = self#map_expr ctx else_block in
						(* `else` block contains suspending expressions *)
						if ctx.sc_current != if_state then begin
							if !next_state == if_state then
								next_state := self#create_state None;
							ctx.sc_current.ss_expr <- match ctx.sc_current.ss_expr with
								| None ->
									Some (self#goto_state_expr !next_state.ss_num origin.epos)
								| Some leftovers ->
									Some (block_expr origin.epos [
										leftovers;
										self#goto_state_expr !next_state.ss_num origin.epos;
									])
						end;
						Some else_block
			in
			ctx.sc_current <- !next_state;
			(* condition does not contain suspending expressions *)
			if current == if_state then
				(* no suspensions at all *)
				if if_state == !next_state then
					{ origin with eexpr = TIf (condition, if_block, else_block) }
				(* `if` or `else` blocks contain suspendign expressions *)
				else
					block_expr origin.epos [
						{ origin with eexpr = TIf (condition, if_block, else_block); };
						self#goto_state_expr !next_state.ss_num origin.epos;
					]
			(* condition contains suspending expression *)
			else begin
				let if_expr =
					match if_state.ss_expr with
						| None ->
							{ origin with eexpr = TIf (self#resume_value_expr origin.epos, if_block, else_block); }
						| Some condition_leftovers ->
							{ condition_leftovers with eexpr = TIf (condition_leftovers, if_block, else_block); }
				in
				if_state.ss_expr <- Some (
					if if_state == !next_state then
						if_expr
					else
						block_expr if_expr.epos [
							if_expr;
							self#goto_state_expr !next_state.ss_num if_expr.epos
						]
				);
				condition
			end
		(**
			Handle an access to the local variable
		*)
		method private map_local_var ctx origin var =
			self#register_var_usage var ctx.sc_current;
			origin
		(**
			Check if `var` was declared in the body of this coroutine
		*)
		method private is_own_local var =
			Hashtbl.mem own_locals var.v_id
		(**
			Check if `var` could be changed outside of this coroutine.
		*)
		method private could_be_changed_outside var =
			not (self#is_own_local var) || Hashtbl.mem captured_modified_locals var.v_id
		(**
			Check if `var` was declared in the body of this coroutine and is used in several states
		*)
		method private is_shared_local var current_state =
			self#is_own_local var
			&& not (Hashtbl.mem current_state.ss_own_locals var.v_id)
		(**
			Handle variable declaration
		*)
		method private register_var_declaration var state =
			(* print_endline ("Decl var: " ^ var.v_name ^ ", state: " ^ (string_of_int state.ss_num)); *)
			Hashtbl.add own_locals var.v_id var;
			Hashtbl.add state.ss_own_locals var.v_id var
		(**
			Handle local variable usage
		*)
		method private register_var_usage var state =
			(* print_endline ("Usage var: " ^ var.v_name ^ ", state: " ^ (string_of_int state.ss_num)); *)
			if self#is_shared_local var state then
				if not (Hashtbl.mem shared_locals var.v_id) then
					Hashtbl.add shared_locals var.v_id var
		(**
			Declare a local variable in `declare_state` and register the usage of the declared var in `usage_state`.
			Returns the variable itself and an expression of accessing that variable.
		*)
		method private declare_cross_state_tmp_var etype pos declare_state usage_state : (tvar * texpr) =
			let var = gen_var "tmp" etype pos in
			self#register_var_declaration var declare_state;
			self#register_var_usage var declare_state;
			self#register_var_usage var usage_state;
			(var, local_expr var pos)
		(**
		*)
		method private map_var_declaration ctx origin var init_expr =
			match init_expr with
				| None ->
					self#register_var_declaration var ctx.sc_current;
					origin
				| Some init_expr ->
					let result =
						self#map_one_expr ctx origin init_expr (fun init_expr -> TVar (var, Some init_expr))
					in
					self#register_var_declaration var ctx.sc_current;
					result
		(**
		*)
		method private map_suspending_call ctx origin callee args =
			(* add `sm` to the arguments if required*)
			let args =
				match follow callee.etype with
					| TFun (arg_types, _) ->
						let remove_trailing_nulls = (List.length args) <> (List.length arg_types) in
						let rec traverse args types =
							match args, types with
								| { eexpr = TConst TNull } :: rest_args, (_, true, t) :: _ when does_unify sm_var.v_type t ->
									(self#sm_expr origin.epos) :: rest_args
								| [], (_, true, t) :: _ when does_unify sm_var.v_type t  ->
									[self#sm_expr origin.epos]
								| arg :: rest_args, _ :: rest_types ->
									arg :: (traverse rest_args rest_types)
								| [], (_, _, t) :: rest_types ->
									(null_expr t origin.epos coro_types.ct_null) :: (traverse [] rest_types)
								| _ -> []
						in
						let args = traverse args arg_types in
						if remove_trailing_nulls then
							List.rev (remove_nulls (List.rev args))
						else
							args
					| _ -> args
			in
			let finish_state_expr next_state_num callee args =
				merge_exprs [
					let call = { origin with eexpr = TCall (callee, args)} in
					self#suspend_expr ~before_return:call next_state_num origin.epos;
				]
			in
			match self#transform_exprs ctx (callee :: args) with
				| [], callee :: args ->
					ctx.sc_current <- self#create_state (Some callee.epos);
					finish_state_expr ctx.sc_current.ss_num callee args
				| result_exprs, callee :: args ->
					let final_state = ctx.sc_current in
					ctx.sc_current <- self#create_state (Some callee.epos);
					final_state.ss_expr <- Some (finish_state_expr ctx.sc_current.ss_num callee args);
					merge_exprs result_exprs
				| _ -> fail origin.epos __POS__
		(**
		*)
		method private map_call ctx origin callee args =
			let factory exprs =
				match exprs with
					| callee :: args -> TCall (callee, args)
					| _ -> fail origin.epos __POS__
			in
			self#map_list_exprs ctx origin (callee :: args) factory
		(**
		*)
		method private map_return_void ctx origin =
			self#suspend_expr state_exit_code.sec_finished origin.epos;
		(**
			Split `return expr` into states
		*)
		method private map_return_value ctx origin expr =
			let current = ctx.sc_current in
			let result = self#map_expr ctx expr in
			if current == ctx.sc_current then
				block_expr origin.epos [
					self#set_sm_return_value_expr result;
					self#suspend_expr state_exit_code.sec_finished origin.epos;
				]
			else
				begin
					(match ctx.sc_current.ss_expr with
						(* No expressions after the last suspending call means it was `return suspending()`. *)
						| None ->
							ctx.sc_current.ss_expr <- Some (block_expr origin.epos [
								self#set_sm_return_value_expr (self#resume_value_expr expr.epos);
								self#suspend_expr state_exit_code.sec_finished origin.epos;
							])
						(* Store return value and set final vaue for sm.state *)
						| Some last_expr ->
							ctx.sc_current.ss_expr <- Some (block_expr last_expr.epos [
								self#set_sm_return_value_expr last_expr;
								self#suspend_expr state_exit_code.sec_finished last_expr.epos;
							])
					);
					result
				end;
		(**
			Split exressions of a block into states
		*)
		method private map_block ctx origin exprs =
			let rec traverse exprs =
				match exprs with
					| expr :: rest ->
						let current = ctx.sc_current
						and expr = self#map_expr ctx expr in
						(* was that a not-suspending expr? *)
						if current == ctx.sc_current then
							expr :: (traverse rest)
						(* it was a suspending expr *)
						else begin
							if rest <> [] then begin
								let current = ctx.sc_current in
								let expr = self#map_block ctx origin rest in
								(match current.ss_expr with
									| None ->
										current.ss_expr <- Some expr
									| Some leftovers ->
										current.ss_expr <- Some (block_expr leftovers.epos [leftovers; expr;])
								);
							end;
							[expr]
						end
					| _ -> []
			in
			block_expr origin.epos (traverse exprs)
		(**
			Builds the body expression of the state machine
			```
			var state = sm.state;
			sm.state = Interrupted;
			if(state == 0) ... else if(state == 1)...
			```
		*)
		method private build_body =
			block_expr entry_pos [
				{ eexpr = TVar (local_state, Some (self#sm_state_expr entry_pos)); epos = entry_pos; etype = coro_types.ct_void; };
				self#set_sm_state_expr state_exit_code.sec_interrupted entry_pos;
				self#switch_states_expr states root_trys
			]
		(**
			Generates expression like `if(sm.state == 0) <...> else if(sm.state == 1) <...> else if...`
		*)
		method private switch_states_expr ?(is_root=true) states trys =
			let rec traverse_trys trys pos =
				match trys with
					| current :: rest ->
						let try_block =
							self#switch_states_expr ~is_root:false current.td_states current.td_nested_trys
						in
						{
							epos = pos; etype = coro_types.ct_void;
							eexpr = TTry (try_block, current.td_catches)
						} :: traverse_trys rest pos
					| [] -> []
			in
			let rec traverse_states states pos =
				match states with
					| { ss_expr = None; ss_num = num } :: rest ->
						fail ~msg:("Non-expr state: " ^ (string_of_int num)) pos __POS__
					| ({ ss_expr = Some expr; ss_num = num; } as state) :: rest ->
						let condition = self#state_condition_expr num pos
						and next_if =
							match rest with
								| [] ->
									let exprs = traverse_trys trys entry_pos in
									if is_root then
										begin
											let final_expr =
												invalid_state_expr coro_types (self#local_state_expr entry_pos) entry_pos
											in
											Some ({ epos = expr.epos; etype = coro_types.ct_void; eexpr = TBlock (exprs @ [final_expr]) })
										end
									else if List.length exprs > 0 then
										Some ({ epos = expr.epos; etype = coro_types.ct_void; eexpr = TBlock exprs })
									else
										None
								| _ -> Some (traverse_states rest expr.epos)
						in
						let if_block =
							match state.ss_suspend_pos with
								| Some pos when is_throwing_state_machine ->
									block_expr pos [self#throw_on_resume_expr pos; self#final_replacements expr]
								| _ ->
									self#final_replacements expr
						in
						{
							epos = pos;
							etype = coro_types.ct_int;
							eexpr = TIf(condition, if_block, next_if);
						}
					| [] ->
						fail pos __POS__
			in
			let switch = traverse_states states entry_pos in
			(*
				Generate `while(true) ...` if we need the ability to proceed to the next state
				without suspending the state machine.
			*)
			if is_root && has_goto_state then
				begin
					let const_true = const_bool_expr coro_types true entry_pos in
					{
						etype = coro_types.ct_void;
						epos = entry_pos;
						eexpr = TWhile (const_true, switch, NormalWhile);
					}
				end
			else
				switch
		(**
			`if(throwOnResume != null) throw throwOnResume;`
		*)
		method private throw_on_resume_expr pos =
			let field_expr =
				{
					etype = t_dynamic; epos = pos;
					eexpr = TField (
						self#sm_expr pos,
						FInstance (coro_types.ct_throwing_sm_cls, [], coro_types.ct_sm_throw_on_resume_field)
					);
				}
			in
			let condition_expr =
				{
					etype = coro_types.ct_bool; epos = pos;
					eexpr = TBinop (OpNotEq, field_expr, null_expr t_dynamic pos coro_types.ct_null)
				}
			in
			let throw_expr = { etype = coro_types.ct_void; epos = pos; eexpr = TThrow field_expr } in
			{
				etype = coro_types.ct_void; epos = pos;
				eexpr = TIf(condition_expr, throw_expr, None)
			}
		(**
			1. Find `@CORO_BREAK id`, `@CORO_CONTINUE id` and replace them with the appropriate states switches;
			2. Find declarations of variables, which are used in several states,
				and replace those declarations with assignments or empty blocks.
				E.g. `var v = value` is replaced with `v = value`. And `var v;` is replaced by an empty block `{}`.
		*)
		method private final_replacements expr =
			match expr.eexpr with
				| TVar (v, None) when Hashtbl.mem shared_locals v.v_id ->
					{ expr with eexpr = TBlock [] }
				| TVar (v, Some init_expr) when Hashtbl.mem shared_locals v.v_id ->
					let init_expr = self#final_replacements init_expr in
					{ init_expr with eexpr = TBinop(OpAssign, { init_expr with eexpr = TLocal v }, init_expr) }
				| TMeta ((Meta.Custom meta, [], _), { eexpr = TConst (TInt loop_id) }) ->
					let loop_id = Int32.to_int loop_id in
					if meta = loop_control_break then
						try
							let _, exit_state_num = Hashtbl.find loops_states loop_id in
							block_expr expr.epos [
								self#goto_state_expr exit_state_num expr.epos;
								{ epos = expr.epos; etype = coro_types.ct_void; eexpr = TContinue }
							]
						with Not_found -> expr
					else if meta = loop_control_continue then
						try
							let condition_state_num, exit_state_num = Hashtbl.find loops_states loop_id in
							block_expr expr.epos [
								self#goto_state_expr condition_state_num expr.epos;
								{ epos = expr.epos; etype = coro_types.ct_void; eexpr = TContinue }
							]
						with Not_found -> expr
					else
						map_expr self#final_replacements expr
				| _ ->
					map_expr self#final_replacements expr
		(**
			Get tvar for "resumeValue" argument of the state machine
		*)
		method private get_resume_value_var =
			match resume_value_var with
				| Some v -> v
				| None -> fail coroutine_expr.epos __POS__
		(**
			Check if "resumeValue" argument of the state machine exists
		*)
		method private has_resume_value_var =
			match resume_value_var with
				| Some _ -> true
				| None -> false
		(**
			`resumeValue`
		*)
		method private resume_value_expr ?etype pos =
			let v = self#get_resume_value_var in
			let expr = local_expr v pos in
			match etype with
				| None -> expr
				| Some t ->
					if same_types v.v_type t then
						expr
					else
						{
							etype = t;
							epos = pos;
							eexpr = TCast (expr, None)
						}
		(**
		*)
		method private state_resume_expr state suspended_expr =
			match state.ss_expr with
				| None -> self#resume_value_expr ~etype:suspended_expr.etype suspended_expr.epos
				| Some leftovers -> leftovers
		(**
			Integer constant expression
		*)
		method private num_expr num pos =
			const_int_expr coro_types num pos
		(**
			Generates expression like `sm_var.state == state_num`
		*)
		method private state_condition_expr state_num pos =
			{
				epos = pos;
				etype = coro_types.ct_bool;
				eexpr = TBinop (OpEq, self#local_state_expr pos, self#num_expr state_num pos);
			}
		(**
			Expression of a read access to sm_var
		*)
		method private sm_expr pos =
			local_expr sm_var pos
		(**
			`sm.state`
		*)
		method private sm_state_expr pos =
			{
				etype = coro_types.ct_int;
				epos = pos;
				eexpr = TField (self#sm_expr pos, FInstance (coro_types.ct_sm_cls, [], coro_types.ct_sm_state_field));
			}
		(**
			`sm.nextState`
		*)
		method private sm_next_state_expr pos =
			{
				etype = coro_types.ct_int;
				epos = pos;
				eexpr = TField (self#sm_expr pos, FInstance (coro_types.ct_sm_cls, [], coro_types.ct_sm_next_state_field));
			}
		(**
			Local variable `state`
		*)
		method private local_state_expr pos =
			local_expr local_state pos
		(**
			`sm.returnValue`
		*)
		method private sm_return_value_expr pos =
			{
				etype = t_dynamic;
				epos = pos;
				eexpr = TField (self#sm_expr pos, FInstance (coro_types.ct_sm_cls, [], coro_types.ct_sm_return_value_field));
			}
		(**
			`$left = $right`
		*)
		method private assign_expr left right pos =
			{
				epos = pos;
				etype = right.etype;
				eexpr = TBinop (OpAssign, left, right);
			}
		(**
			`sm.state = $state_num`
		*)
		method private set_sm_state_expr state_num pos =
			self#assign_expr (self#sm_state_expr pos) (self#num_expr state_num pos) pos
		(**
			`state = $state_num`
		*)
		method private set_local_state_expr state_num pos =
			self#assign_expr (self#local_state_expr pos) (self#num_expr state_num pos) pos
		(**
			`sm.returnValue = $expr`
		*)
		method private set_sm_return_value_expr (expr:texpr) : texpr =
			self#assign_expr (self#sm_return_value_expr expr.epos) expr expr.epos
		(**
			Set next state and suspend execution.
			Returns `return sm.state = sm.nextState = $next_state_num` if `before_return` is not provided.
			Otherwise returns `sm.nextState = $next_state_num; $expr; return sm.state = sm.nextState`.
		*)
		method private suspend_expr ?before_return next_state_num pos =
			let next_state = self#sm_next_state_expr pos in
			let set_next_state =
				self#assign_expr next_state (const_int_expr coro_types next_state_num pos) pos
			in
			let set_state expr =
				self#assign_expr (self#sm_state_expr pos) expr pos
			in
			let return expr =
				{
					epos = pos; etype = coro_types.ct_int;
					eexpr = TReturn (Some (set_state expr))
				}
			in
			match before_return with
				| None ->
					return set_next_state
				| Some expr ->
					block_expr pos [set_next_state; expr; return next_state]
		(**
			Immediately proceed to the next state.
		*)
		method private goto_state_expr ?(add_continue=false) next_state_num pos =
			has_goto_state <- true;
			match current_try with
				| None when not add_continue ->
					self#set_local_state_expr next_state_num pos
				| _ ->
					block_expr pos [
						self#set_local_state_expr next_state_num pos;
						{ etype = coro_types.ct_void; epos = pos; eexpr = TContinue }
					]
	end

(**
	Transforms expression of a field with coroutines inside.
*)
class field_processor (field:tclass_field) (coro_types:coro_types) =
	object (self)
		(** Local variables declared/used in the expression of this field *)
		val vars = match field.cf_expr with Some e -> collect_local_vars e | None -> fail field.cf_pos __POS__
		(**
			Generate final expression with all required transformations applied.
		*)
		method build_field_expression : texpr =
			match field.cf_expr with
				| Some e -> self#transform_expr e
				| None -> fail field.cf_pos __POS__
		(**
			Transforms local function declarations to state machines if passed to the argument
			of `coro.Coroutine` type followed by the argument with `coro.Coroutine.Generated` type.
			Transformed function is then passed to the argument of `Generated` type while the original function
			is replaced with `null`.
			Function arguments list gets appended with the one or two new arguments: `sm` or `sm, resumeValue`
			E.g. if the signature of `generator` function is
			```
				function generator(
					userFunction:Coroutine<yield:YieldType->ReturnType>,
					?genFunction:Generated<(yield:YieldType, sm:StateMachineType, resumeValue:ResumeType)->Void>
				)
			```
			then this expression
			```
				generator(yield -> {/* body */});
			```
			is transformed to
			```
				generator(null, (yield, sm, resumeValue) -> {/* transformed body */});
			```
		*)
		method private transform_expr expr : texpr =
			match expr.eexpr with
				| TNew (cls, params, arg_exprs) ->
					let arg_exprs = List.map self#transform_expr arg_exprs in
					let arg_types = get_constructor_arg_types cls params in
					if contains_transformable_coroutines arg_exprs arg_types then
						let arg_exprs = self#transform_args arg_exprs arg_types in
						{ expr with eexpr = TNew(cls, params, arg_exprs) }
					else
						{ expr with eexpr = TNew(cls, params, arg_exprs) }
				| TCall (callee, arg_exprs) when is_function_type callee.etype ->
					let arg_exprs = List.map self#transform_expr arg_exprs in
					let arg_types = List.map (fun (_, _, t) -> t) (get_func_args callee.etype expr.epos) in
					if contains_transformable_coroutines arg_exprs arg_types then
						let arg_exprs = self#transform_args arg_exprs arg_types in
						{ expr with eexpr = TCall(callee, arg_exprs) }
					else
						{ expr with eexpr = TCall(callee, arg_exprs) }
				| _ ->
					map_expr self#transform_expr expr
		(**
			Transform coroutines in the list of arguments expressions `args`
			passed to the function call with the list of arguments `types`.
		*)
		method private transform_args args types : texpr list =
			let coroutines = ref []
			and final_args_reversed = ref [] in
			(* generates an expression of state machine using a coroutine from `coroutines`, which match the `generated_type`  *)
			let try_generate pos t =
				if is_generated_type t then
					match find_coroutine_for_generated !coroutines t pos with
						| Some coro ->
							coroutines := List.filter (fun c -> coro != c) !coroutines;
							(new sm_builder coro_types coro t self#gen_var)#build
						| _ -> null_expr t pos coro_types.ct_null
				else
					null_expr t pos coro_types.ct_null
			in
			let rec traverse args types =
				match args, types with
					| arg :: rest_args, t :: rest_types ->
						let arg =
							if is_transformable_coroutine arg t rest_args rest_types then
								begin
									coroutines := arg :: !coroutines;
									(* replace function declaration with `null` *)
									null arg.etype arg.epos
								end
							else if is_null_expr arg then
								try_generate arg.epos t
							else
								arg
						in
						final_args_reversed := arg :: !final_args_reversed;
						traverse rest_args rest_types
					| [], rest_types ->
						let trailing_args = List.map (try_generate null_pos) rest_types in
						final_args_reversed := (remove_nulls (List.rev trailing_args)) @ !final_args_reversed
					| _ -> ()
			in
			traverse args types;
			List.rev !final_args_reversed
		(**
			Check if a local variable with such `name` exists in the expression of this field.
		*)
		method private has_var name = Hashtbl.mem vars name
		(**
			Add variable to the list of local variables used in this field.
		*)
		method private add_var v = Hashtbl.add vars v.v_name v
		(**
			Generate local variable with unique `name`.
			Automatically adds a number postfix if such name is already used.
		*)
		method private gen_var ?counter name t pos =
			let count, real_name =
				match counter with
					| Some count -> count + 1, name ^ (string_of_int count)
					| _ -> 1, name
			in
			if not (self#has_var real_name) then
				begin
					let v = alloc_var (VUser TVOLocalVariable) real_name t pos in
					self#add_var v;
					v
				end
			else
				self#gen_var ~counter:count name t pos
	end

class plugin =
	object (self)
		val mutable executed = false
		(**
			Plugin API: this method should be executed at initialization macro time
		*)
		method run () =
			if not executed then begin
				executed <- true;
				let compiler = (get_ctx()).curapi in
				let com = compiler.get_com() in
				add_typing_filter com (fun types ->
					let t = Timer.timer ["plugin"; "coro"] in
					let coro_types = self#find_coro_types compiler.typer_ctx types in
					let process_field field =
						match field.cf_expr with
							| Some e when contains_coroutines e ->
								(* print_endline field.cf_name; *)
								field.cf_expr <- Some ((new field_processor field coro_types)#build_field_expression)
							| Some _ | None -> ()
					in
					let rec traverse com_type =
						match com_type with
							| TClassDecl cls ->
								(* let cls_name = TInst (cls,[]) in *)
								List.iter process_field cls.cl_ordered_statics;
								List.iter process_field cls.cl_ordered_fields
							| _ -> ()
					in
					List.iter traverse types;
					t()
				)
			end;
			(* This is because of vfun should return something *)
			vnull
		(**
			Build coro_types structure
		*)
		method private find_coro_types typer types =
			let ct_sm_cls = ref None
			and ct_throwing_sm_cls = ref None
			and ct_exception_cls = ref None
			and ct_state_exception_cls = ref None
			in
			let rec traverse types =
				(match types with
					| TClassDecl cls :: _ when cls.cl_path = state_machine_type_path -> ct_sm_cls := Some cls
					| TClassDecl cls :: _ when cls.cl_path = throwing_state_machine_type_path -> ct_throwing_sm_cls := Some cls
					| TClassDecl cls :: _ when cls.cl_path = coroutine_exception_type_path -> ct_exception_cls := Some cls
					| TClassDecl cls :: _ when cls.cl_path = coroutine_state_exception_type_path -> ct_state_exception_cls := Some cls
					| _ -> ()
				);
				match types with
					| [] -> ()
					| _ :: rest -> traverse rest
			in
			traverse types;
			let ct_sm_cls = match !ct_sm_cls with Some c -> c | None -> fail null_pos __POS__
			and ct_throwing_sm_cls = match !ct_throwing_sm_cls with Some c -> c | None -> fail null_pos __POS__
			and ct_exception_cls = match !ct_exception_cls with Some c -> c | None -> fail null_pos __POS__
			and ct_state_exception_cls = match !ct_state_exception_cls with Some c -> c | None -> fail null_pos __POS__
			in
			{
				ct_sm_cls = ct_sm_cls;
				ct_throwing_sm_cls = ct_throwing_sm_cls;
				ct_exception_cls = ct_exception_cls;
				ct_state_exception_cls = ct_state_exception_cls;
				ct_sm_state_field = List.find (fun f -> f.cf_name = "state") ct_sm_cls.cl_ordered_fields;
				ct_sm_next_state_field = List.find (fun f -> f.cf_name = "nextState") ct_sm_cls.cl_ordered_fields;
				ct_sm_return_value_field = List.find (fun f -> f.cf_name = "returnValue") ct_sm_cls.cl_ordered_fields;
				ct_sm_throw_on_resume_field = List.find (fun f -> f.cf_name = "throwOnResume") ct_throwing_sm_cls.cl_ordered_fields;
				ct_string = typer.t.tstring;
				ct_int = typer.t.tint;
				ct_bool = typer.t.tbool;
				ct_void = typer.t.tvoid;
				ct_null = typer.t.tnull;
			}
	end
;;

let api = new plugin in

EvalStdLib.StdContext.register [
	("run", vfun0 api#run);
]