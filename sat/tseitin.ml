(**************************************************************************)
(*                                                                        *)
(*                          Alt-Ergo Zero                                 *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                      Universite Paris-Sud 11                           *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

module type S = Tseitin_intf.S

module Make (F : Formula_intf.S) = struct

  exception Empty_Or
  type combinator = And | Or | Imp | Not

  type atom = F.t
  type t =
    | Lit of atom
    | Comb of combinator * t list

  let rec print fmt phi =
    match phi with
    | Lit a -> F.print fmt a
    | Comb (Not, [f]) ->
      Format.fprintf fmt "not (%a)" print f
    | Comb (And, l) -> Format.fprintf fmt "(%a)" (print_list "and") l
    | Comb (Or, l) ->  Format.fprintf fmt "(%a)" (print_list "or") l
    | Comb (Imp, [f1; f2]) ->
      Format.fprintf fmt "(%a => %a)" print f1 print f2
    | _ -> assert false
  and print_list sep fmt = function
    | [] -> ()
    | [f] -> print fmt f
    | f::l -> Format.fprintf fmt "%a %s %a" print f sep (print_list sep) l

  let make comb l = Comb (comb, l)

  let value env c =
    if List.mem c env then Some true
    else if List.mem (make Not [c]) env then Some false
    else None

  (*
  let rec lift_ite env op l =
    try
      let left, (c, t1, t2), right = Term.first_ite l in
      begin
        match value env c with
        | Some true ->
          lift_ite (c::env) op (left@(t1::right))
        | Some false ->
          lift_ite ((make Not [c])::env) op (left@(t2::right))
        | None ->
          Comb
            (And,
             [Comb
                (Imp, [c; lift_ite (c::env) op (left@(t1::right))]);
              Comb (Imp,
                    [(make Not [c]);
                     lift_ite
                       ((make Not [c])::env) op (left@(t2::right))])])
      end
    with Not_found ->
      begin
        let lit =
          match op, l with
          | Eq, [Term.T t1; Term.T t2] ->
            Literal.Eq (t1, t2)
          | Neq, ts ->
            let ts =
              List.rev_map (function Term.T x -> x | _ -> assert false) ts in
            Literal.Distinct (false, ts)
          | Le, [Term.T t1; Term.T t2] ->
            Literal.Builtin (true, Hstring.make "<=", [t1; t2])
          | Lt, [Term.T t1; Term.T t2] ->
            Literal.Builtin (true, Hstring.make "<", [t1; t2])
          | _ -> assert false
        in
        Lit (F.make lit)
      end

  let make_lit op l = lift_ite [] op l
  *)

  let make_atom p = Lit p

  let make_not f = make Not [f]
  let make_and l = make And l
  let make_imply f1 f2 = make Imp [f1; f2]
  let make_equiv f1 f2 = make And [ make_imply f1 f2; make_imply f2 f1]
  let make_xor f1 f2 = make Or [ make And [ make Not [f1]; f2 ];
                                 make And [ f1; make Not [f2] ] ]

  (* simplify formula *)
  let rec sform = function
    | Comb (Not, [Lit a]) -> Lit (F.neg a)
    | Comb (Not, [Comb (Not, [f])]) -> sform f
    | Comb (Not, [Comb (Or, l)]) ->
      let nl = List.rev_map (fun a -> sform (Comb (Not, [a]))) l in
      Comb (And, nl)
    | Comb (Not, [Comb (And, l)]) ->
      let nl = List.rev_map (fun a -> sform (Comb (Not, [a]))) l in
      Comb (Or, nl)
    | Comb (Not, [Comb (Imp, [f1; f2])]) ->
      Comb (And, [sform f1; sform (Comb (Not, [f2]))])
    | Comb (And, l) ->
      Comb (And, List.rev_map sform l)
    | Comb (Or, l) ->
      Comb (Or, List.rev_map sform l)
    | Comb (Imp, [f1; f2]) ->
      Comb (Or, [sform (Comb (Not, [f1])); sform f2])
    | Comb ((Imp | Not), _) -> assert false
    | Lit _ as f -> f


  let ( @@ ) l1 l2 = List.rev_append l1 l2
  let ( @ ) = `Use_rev_append_instead   (* prevent use of non-tailrec append *)

  let make_or = function
    | [] -> raise Empty_Or
    | [a] -> a
    | l -> Comb (Or, l)

  let distrib l_and l_or =
    let l =
      if l_or = [] then l_and
      else
        List.rev_map
          (fun x ->
             match x with
             | Lit _ -> Comb (Or, x::l_or)
             | Comb (Or, l) -> Comb (Or, l @@ l_or)
             | _ -> assert false
          ) l_and
    in
    Comb (And, l)

  let rec flatten_or = function
    | [] -> []
    | Comb (Or, l)::r -> l @@ (flatten_or r)
    | Lit a :: r -> (Lit a)::(flatten_or r)
    | _ -> assert false

  let rec flatten_and = function
    | [] -> []
    | Comb (And, l)::r -> l @@ (flatten_and r)
    | a :: r -> a::(flatten_and r)


  let rec cnf f =
    match f with
    | Comb (Or, l) ->
      begin
        let l = List.rev_map cnf l in
        let l_and, l_or =
          List.partition (function Comb(And,_) -> true | _ -> false) l in
        match l_and with
        | [ Comb(And, l_conj) ] ->
          let u = flatten_or l_or in
          distrib l_conj u

        | Comb(And, l_conj) :: r ->
          let u = flatten_or l_or in
          cnf (Comb(Or, (distrib l_conj u)::r))

        | _ ->
          begin
            match flatten_or l_or with
            | [] -> assert false
            | [r] -> r
            | v -> Comb (Or, v)
          end
      end
    | Comb (And, l) ->
      Comb (And, List.rev_map cnf l)
    | f -> f

  let rec mk_cnf = function
    | Comb (And, l) ->
      List.fold_left (fun acc f ->  (mk_cnf f) @@ acc) [] l

    | Comb (Or, [f1;f2]) ->
      let ll1 = mk_cnf f1 in
      let ll2 = mk_cnf f2 in
      List.fold_left
        (fun acc l1 -> (List.rev_map (fun l2 -> l1 @@ l2)ll2) @@ acc) [] ll1

    | Comb (Or, f1 :: l) ->
      let ll1 = mk_cnf f1 in
      let ll2 = mk_cnf (Comb (Or, l)) in
      List.fold_left
        (fun acc l1 -> (List.rev_map (fun l2 -> l1 @@ l2)ll2) @@ acc) [] ll1

    | Lit a -> [[a]]
    | Comb (Not, [Lit a]) -> [[F.neg a]]
    | _ -> assert false


  let rec unfold mono f =
    match f with
    | Lit a -> a::mono
    | Comb (Not, [Lit a]) ->
      (F.neg a)::mono
    | Comb (Or, l) ->
      List.fold_left unfold mono l
    | _ -> assert false

  let rec init monos f =
    match f with
    | Comb (And, l) ->
      List.fold_left init monos l
    | f -> (unfold [] f)::monos

  let make_cnf f =
    let sfnc = cnf (sform f) in
    init [] sfnc

  let mk_proxy = F.fresh
      (*
    let cpt = ref 0 in
    fun () ->
      let t = AETerm.make
          (Symbols.name (Hstring.make ("PROXY__"^(string_of_int !cpt))))
          [] Ty.Tbool
      in
      incr cpt;
      F.make (Literal.Eq (t, AETerm.true_))
      *)

  let acc_or = ref []
  let acc_and = ref []

  (* build a clause by flattening (if sub-formulas have the
     same combinator) and proxy-ing sub-formulas that have the
     opposite operator. *)
  let rec cnf f = match f with
    | Lit a -> None, [a]
    | Comb (Not, [Lit a]) -> None, [F.neg a]

    | Comb (And, l) ->
      List.fold_left
        (fun (_, acc) f ->
           match cnf f with
           | _, [] -> assert false
           | cmb, [a] -> Some And, a :: acc
           | Some And, l ->
             Some And, l @@ acc
           (* let proxy = mk_proxy () in *)
           (* acc_and := (proxy, l) :: !acc_and; *)
           (* proxy :: acc *)
           | Some Or, l ->
             let proxy = mk_proxy () in
             acc_or := (proxy, l) :: !acc_or;
             Some And, proxy :: acc
           | None, l -> Some And, l @@ acc
           | _ -> assert false
        ) (None, []) l

    | Comb (Or, l) ->
      List.fold_left
        (fun (_, acc) f ->
           match cnf f with
           | _, [] -> assert false
           | cmb, [a] -> Some Or, a :: acc
           | Some Or, l ->
             Some Or, l @@ acc
           (* let proxy = mk_proxy () in *)
           (* acc_or := (proxy, l) :: !acc_or; *)
           (* proxy :: acc *)
           | Some And, l ->
             let proxy = mk_proxy () in
             acc_and := (proxy, l) :: !acc_and;
             Some Or, proxy :: acc
           | None, l -> Some Or, l @@ acc
           | _ -> assert false
        ) (None, []) l

    | _ -> assert false

  let cnf f =
    let acc = match f with
      | Comb (And, l) -> List.rev_map (fun f -> snd(cnf f)) l
      | _ -> [snd (cnf f)]
    in
    let proxies = ref [] in
    (* encore clauses that make proxies in !acc_and equivalent to
       their clause *)
    let acc =
      List.fold_left
        (fun acc (p,l) ->
           proxies := p :: !proxies;
           let np = F.neg p in
           (* build clause [cl = l1 & l2 & ... & ln => p] where [l = [l1;l2;..]]
              also add clauses [p => l1], [p => l2], etc. *)
           let cl, acc =
             List.fold_left
               (fun (cl,acc) a -> (F.neg a :: cl), [np; a] :: acc)
               ([p],acc) l in
           cl :: acc
        ) acc !acc_and
    in
    (* encore clauses that make proxies in !acc_or equivalent to
       their clause *)
    let acc =
      List.fold_left
        (fun acc (p,l) ->
           proxies := p :: !proxies;
           (* add clause [p => l1 | l2 | ... | ln], and add clauses
              [l1 => p], [l2 => p], etc. *)
           let acc = List.fold_left (fun acc a -> [p; F.neg a]::acc)
               acc l in
           (F.neg p :: l) :: acc
        ) acc !acc_or
    in
    acc

  let make_cnf f =
    acc_or := [];
    acc_and := [];
    cnf (sform f)

  (* Naive CNF XXX remove???
     let make_cnf f = mk_cnf (sform f)
  *)
end
