(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon, Evelyne Contejean                    *)
(*                  Francois Bobot, Mohamed Iguernelala, Alain Mebsout    *)
(*                  CNRS, Universite Paris-Sud 11                         *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

type answer = Yes of Explanation.t | No

type 'a literal =
  | LSem of 'a Literal.view
  | LTerm of Literal.LT.t

exception Inconsistent of Explanation.t
exception Unsolvable

module Ex = Explanation
module Sy = Symbols
module T = Term
module MapT = Term.Map
module SetT = Term.Set

module Lit = Literal.Make(T)
module MapL = Lit.Map

module MapR = MapT
module SetR = SetT

module R = struct
  include T
  type r = t

  let leaves t : r list = match T.head t with
    | T.Ite (a, _, _) -> [a]
    | T.App (_, l) -> l
end

type r = R.r

module SetRR = Set.Make(struct
    type t = r * r
    let compare (r1, r1') (r2, r2') =
      let c = T.compare r1 r2 in
      if c <> 0 then c
      else T.compare r1' r2'
  end)


type t = {
  (* term -> [t] *)
  make : r MapT.t;

  (* representative table *)
  repr : (r * Ex.t) MapR.t;

  (* r -> class (of terms) *)
  classes : SetT.t MapR.t;

  (*associates each value r with the set of semantical values whose
    representatives contains r *)
  gamma : SetR.t MapR.t;

  (* the disequations map *)
  neqs: Ex.t MapL.t MapR.t;

}

let empty = {
  make = MapT.empty;
  repr = MapR.empty;
  classes = MapR.empty;
  gamma = MapR.empty;
  neqs = MapR.empty;
}

module Env = struct
  let mem env t = MapT.mem t env.make

  let lookup_by_t t env =
    try MapR.find (MapT.find t env.make) env.repr
    with Not_found ->
      assert false (*R.make t, Ex.empty*) (* XXXX *)

  let lookup_by_r r env =
    try MapR.find r env.repr with Not_found -> r, Ex.empty

  let lookup_for_neqs env r =
    try MapR.find r env.neqs with Not_found -> MapL.empty

  let add_to_classes t r classes =
    MapR.add r
      (SetT.add t (try MapR.find r classes with Not_found -> SetT.empty))
      classes

  let update_classes c nc classes =
    let s1 = try MapR.find c classes with Not_found -> SetT.empty in
    let s2 = try MapR.find nc classes with Not_found -> SetT.empty in
    MapR.remove c (MapR.add nc (SetT.union s1 s2) classes)

  let add_to_gamma r c gamma =
    List.fold_left
      (fun gamma x ->
         let s = try MapR.find x gamma with Not_found -> SetR.empty in
         MapR.add x (SetR.add r s) gamma)
      gamma
      (R.leaves c)

  (* r1 = r2 => neqs(r1) \uplus neqs(r2) *)
  let update_neqs r1 r2 dep env =
    let nq_r1 = lookup_for_neqs env r1 in
    let nq_r2 = lookup_for_neqs env r2 in
    let mapl =
      MapL.fold
        (fun l1 ex1 mapl ->
           try
             let ex2 = MapL.find l1 mapl in
             let ex = Ex.union (Ex.union ex1 ex2) dep in (* VERIF *)
             raise (Inconsistent ex)
           with Not_found ->
             MapL.add l1 (Ex.union ex1 dep) mapl)
        nq_r1 nq_r2
    in
    MapR.add r2 mapl (MapR.add r1 mapl env.neqs)

  let filter_leaves r =
    List.fold_left (fun p r -> SetR.add r p) SetR.empty (R.leaves r)

  let find_or_normal_form env r =
    try MapR.find r env.repr with Not_found -> r, Ex.empty

  let init_leaf env p =
    let in_repr = MapR.mem p env.repr in
    let in_neqs = MapR.mem p env.neqs in
    { env with
        repr    =
          if in_repr then env.repr
          else MapR.add p (p, Ex.empty) env.repr;
        classes =
          if in_repr then env.classes
          else update_classes p p env.classes;
        gamma   =
          if in_repr then env.gamma
          else add_to_gamma p p env.gamma ;
        neqs    =
          if in_neqs then env.neqs
          else update_neqs p p Ex.empty env }

  let init_term env t =
    let mkr, ctx = R.make t in
    let rp, ex = normal_form env mkr in
    { make    = MapT.add t mkr env.make;
      repr    = MapR.add mkr (rp,ex) env.repr;
      classes = add_to_classes t rp env.classes;
      gamma   = add_to_gamma mkr rp env.gamma;
      neqs    =
        if MapR.mem rp env.neqs then env.neqs (* pourquoi ce test *)
        else MapR.add rp MapL.empty env.neqs}, ctx


  let update_aux dep set env=
    SetRR.fold
      (fun (rr, nrr) env ->
         { env with
             neqs = update_neqs rr nrr dep env ;
             classes = update_classes rr nrr env.classes})
      set env

  let apply_sigma_uf env (p, v, dep) =
    assert (MapR.mem p env.gamma);
    let use_p = MapR.find p env.gamma in
    try
      let env, tch, neqs_to_up = SetR.fold
          (fun r (env, touched, neqs_to_up) ->
             let rr, ex = MapR.find r env.repr in
             let nrr = R.subst p v rr in
             if R.equal rr nrr then env, touched, neqs_to_up
             else
               let ex  = Ex.union ex dep in
               let env =
                 {env with
                    repr = MapR.add r (nrr, ex) env.repr;
                    gamma = add_to_gamma r nrr env.gamma }
               in
               env, (r, nrr, ex)::touched, SetRR.add (rr, nrr) neqs_to_up
          ) use_p (env, [], SetRR.empty) in
      (* Correction : Do not update neqs twice for the same r *)
      update_aux dep neqs_to_up env, tch

    with Not_found -> assert false

  let apply_sigma eqs env tch ((p, v, dep) as sigma) =
    let env = init_leaf env p in
    let env, touched = apply_sigma_uf env sigma in
    env, ((p, touched, v) :: tch)
end

let add env t =
  if MapT.mem t env.make then env, [] else Env.init_term env t

let ac_solve eqs dep (env, tch) (p, v) =
  (* pourquoi recuperer le representant de rv? r = rv d'apres testopt *)
  (* assert ( let rp, _ = Env.find_or_normal_form env p in R.equal p rp); *)
  let rv, ex_rv = Env.find_or_normal_form env v in
  (* let rv = v in *)
  (* assert ( let rv, _ = Env.find_or_normal_form env v in R.equal v rv); *)
  let dep = Ex.union ex_rv dep in
  Env.apply_sigma eqs env tch (p, rv, dep)

let check_consistent env r1 r2 dep : unit =
  let rr1, ex_r1 = Env.find_or_normal_form env r1 in
  let rr2, ex_r2 = Env.find_or_normal_form env r2 in
  if R.equal rr1 rr2
  then () (* Remove rule *)
  else
    let dep = Ex.union dep (Ex.union ex_r1 ex_r2) in
    begin
      ignore (Env.update_neqs rr1 rr2 dep env);
      if R.can_be_equal rr1 rr2
      then ()
      else raise (Inconsistent dep)
    end

let union env r1 r2 dep =
  check_consistent env r1 r2 dep;
  ()

let rec distinct env rl dep =
  let d = Lit.make (Literal.Distinct (false,rl)) in
  let env, _, newds =
    List.fold_left
      (fun (env, mapr, newds) r ->
         let rr, ex = Env.find_or_normal_form env r in
         try
           let exr = MapR.find rr mapr in
           raise (Inconsistent (Ex.union ex exr))
         with Not_found ->
           let uex = Ex.union ex dep in
           let mdis =
             try MapR.find rr env.neqs with Not_found -> MapL.empty in
           let mdis =
             try
               MapL.add d (Ex.merge uex (MapL.find d mdis)) mdis
             with Not_found ->
               MapL.add d uex mdis
           in
           let env = Env.init_leaf env rr in
           let env = {env with neqs = MapR.add rr mdis env.neqs} in
           env, MapR.add rr uex mapr, (rr, ex, mapr)::newds
      )
      (env, MapR.empty, [])
      rl
  in
  List.fold_left
    (fun env (r1, ex1, mapr) ->
       MapR.fold (fun r2 ex2 env ->
         let ex = Ex.union ex1 (Ex.union ex2 dep) in
         try match R.solve r1 r2 with
           | [a, b] ->
             if (R.equal a r1 && R.equal b r2) ||
                (R.equal a r2 && R.equal b r1) then env
             else
               distinct env [a; b] ex
           | []  ->
             raise (Inconsistent ex)
           | _   -> env
         with Unsolvable -> env) mapr env)
    env newds

let are_equal env t1 t2 =
  let r1, ex_r1 = Env.lookup_by_t t1 env in
  let r2, ex_r2 = Env.lookup_by_t t2 env in
  if R.equal r1 r2 then Yes(Ex.union ex_r1 ex_r2) else No

let are_distinct env t1 t2 =
  let r1, ex_r1 = Env.lookup_by_t t1 env in
  let r2, ex_r2 = Env.lookup_by_t t2 env in
  try
    ignore (union env r1 r2 (Ex.union ex_r1 ex_r2));
    No
  with Inconsistent ex -> Yes(ex)

let already_distinct env lr =
  let d = Lit.make (Literal.Distinct (false,lr)) in
  try
    List.iter (fun r ->
      let mdis = MapR.find r env.neqs in
      ignore (MapL.find d mdis)
    ) lr;
    true
  with Not_found -> false

let find env t =
  Env.lookup_by_t t env

let find_r = Env.find_or_normal_form

let mem = Env.mem

let class_of env t =
  try
    let rt, _ = MapR.find (MapT.find t env.make) env.repr in
    SetT.elements (MapR.find rt env.classes)
  with Not_found -> [t]
