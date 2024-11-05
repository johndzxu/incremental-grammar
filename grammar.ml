type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

type category =
  | A
  | B
  | C
  | X

let words = [Node(A, Empty, Empty);
             Node(B, Empty, Empty);
             Node(C, Empty, Empty)]


(* Combine function *)
let combine (a: 'a tree) (b: 'a tree): 'a tree = Node(X, a, b)


(* Left branching - Tail-recursive function *)
let rec build_tree_l (list: ('a tree) list): 'a tree =
  match list with
  | [] -> Empty
  | [n] -> n
  | n1::n2::ns -> build_tree_l ((combine n1 n2)::ns)


(* Right branching - Non-tail-recursive function *)
let rec build_tree_r (list: ('a tree) list): 'a tree =
  match list with
  | [] -> Empty
  | [n] -> n
  | n::ns -> combine n (build_tree_r ns)


(* Function composition - Continuations *)
let rec build_tree_comp (list: ('a tree) list) (cont: 'a tree -> 'b): 'a tree =
  match list with
  | [] -> cont Empty
  | [n] -> cont n
  | n::ns -> build_tree_comp ns (fun r -> cont (combine n r))
 