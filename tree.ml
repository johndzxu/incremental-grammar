type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

type category =
  | V of string
  | N of string
  | P of string

let words = [3; 1; 4; 1; 5; 9; 2; 6; 5]

(* Left branching *)
let rec combine_l tree l =
  match l with
  | [] -> tree
  | x::xs -> combine_l (Node (0, tree, Node(x, Empty, Empty))) xs


(* right branching workspaces*)
let rec combine_r tree l =
  let combine' x tree = match tree with
  | Empty -> Node(x, Empty, Empty)
  | t -> Node(0, Node(x, Empty, Empty), tree) in
  match l with
  | [] -> tree
  | x::xs -> combine' x (combine_r Empty xs)

(* continuation *)
let rec combine_cont tree l (cont: 'a tree -> 'b) =
  let combine' x tree = match tree with
  | Empty -> Node(x, Empty, Empty)
  | t -> Node(0, Node(x, Empty, Empty), tree) in
  match l with
  | [] -> cont tree
  | x::xs -> combine_cont Empty xs (fun r -> cont (combine' x r))
 