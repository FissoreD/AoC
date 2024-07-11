(** Doubly linked list *)

type 'a node = { value : 'a; (*mutable prev : 'a t;*) mutable next : 'a t }
[@@deriving show]

and 'a t = { mutable content : 'a node option } [@@deriving show]

exception Empty_ddl

let create () = { content = None }
let empty_node value = { value; next = create () }
let singleton value = { content = Some (empty_node value) }

let get_next e =
  match e.content with None -> raise Empty_ddl | Some e -> e.next

(* let get_prev e =
   match e.content with None -> raise Empty_ddl | Some e -> e.prev *)

let get_node a = match a.content with None -> raise Empty_ddl | Some e -> e
let get_node_opt a = a.content
let get_value a = (get_node a).value

let add_next_node new_node dll =
  let new_nodeS = { content = Some new_node } in
  (match dll.content with
  | None -> dll.content <- new_nodeS.content
  | Some node -> (
      match node.next.content with
      | None -> node.next <- new_nodeS
      | Some nnext as t ->
          if nnext == node then (
            node.next <- new_nodeS;
            new_node.next.content <- t)
          else
            let nnext = node.next in
            new_node.next <- nnext;
            node.next <- new_nodeS));
  new_nodeS

(* let add_prev_node new_node dll =
   match dll.content with
   | None -> dll.content <- Some new_node
   | Some node ->
       (match node.prev.content with
       | None -> ()
       | Some e -> e.next.content <- Some new_node);
       new_node.prev <- node.prev;
       new_node.next <- dll;
       node.prev.content <- Some new_node *)

let add_next_value e = add_next_node (empty_node e)

(* let add_prev_value e = add_prev_node (empty_node e) *)
let rec skip dll = function 0 -> dll | n -> skip (get_next dll) (n - 1)

let singleton_circ e =
  let e = singleton e in
  (Option.get e.content).next <- e;
  e

let rec find ar n = if get_value ar = n then ar else find (get_next ar) n
