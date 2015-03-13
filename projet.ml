(*-------------transformation du graphe en listes (visibles)----------------------
*)

(* fonction qui liste les sommets *)
(* liste_v: Digraph.t -> int list *)
let liste_v g = (fold_vertex (fun v qt -> (V.label v)::qt) 
                               g
                               [])
;;

(* fonction qui liste les sommets et leur label *)
(* liste_v: Digraph.t -> int*int list *)
let liste_vl g = (fold_vertex (fun v qt -> (V.label v,Mark.get v)::qt) 
                               g
                               [])
;;


(* fonction qui liste les aretes *)
(* liste_e: Digraph.t -> int*int list *) 
let liste_e g = 
        (fold_edges  (fun v1 v2 qt -> (V.label v1,V.label v2)::qt) 
                     g
                     [])
;;

(* fonction qui liste les aretes et leur label *)
(* liste_e: Digraph.t -> int*int*int list *) 
let liste_el g = 
        (fold_edges  (fun v1 v2 qt -> (V.label v1,V.label v2,E.label (find_edge g v1 v2))::qt) 
                     g
                     [])
;;

(* fct liste les predecesseur*)

let liste_pred v g =
  fold_pred (fun v l -> v::l ) g v [];;

let sans_dependance g =
  fold_vertex (fun v l -> if(in_degree g v = 0) then v::l else l) g [];; 

let appartient v l = 
  List.fold_right (fun s b -> b||v=s) l false;;

let inclu ens1 ens2 =
  List.fold_right (fun v b -> (appartient v ens2) && b) ens1 true;;

let tri_topologique g = 
  let y = sans_dependance g in
     let z = [] in

         let rec iter y z ncourant = 
           match y with
           |t::q -> 
               begin
                 Mark.set t ncourant;
                 let newz = t::z in
                     let newy  = fold_succ (fun v l -> if(inclu (liste_pred v g) newz) then v::l else l) g t q
                       in iter newy newz (ncourant+1)
               end
           |[] -> failwith("Fuck you Mother G7")

         in iter y z 1;;





