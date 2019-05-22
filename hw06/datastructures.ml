(** Data structures, signatures  *)

(* Author: Steve Zdancewic *)

(** Comparable, printable type *)
module type OrdPrintT = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

(** Extended sets *)
module type SetS = sig
  include Set.S

  val to_string : t -> string

  val string_of_elt : elt -> string

  val printer : Format.formatter -> t -> unit
end

module MakeSet (Ord : OrdPrintT) : SetS with type elt = Ord.t = struct
  include Set.Make (Ord)

  let to_string t =
    let s = elements t |> List.map Ord.to_string |> String.concat ", " in
    "{" ^ s ^ "}"

  let string_of_elt = Ord.to_string

  let printer f t = Format.pp_print_string f (to_string t)
end

(** Extended maps *)
module type MapS = sig
  include Map.S

  val diff_keys : ('a -> 'a -> int) -> 'a t -> 'a t -> key list

  val to_string : (key -> 'a -> string) -> 'a t -> string

  val printer : (key -> 'a -> string) -> Format.formatter -> 'a t -> unit
end

module MakeMap (Ord : OrdPrintT) : MapS with type key = Ord.t = struct
  include Map.Make (Ord)

  let diff_keys cmp_v m n =
    let module S = MakeSet (Ord) in
    let has_binding_or_add m k v l =
      try if cmp_v v @@ find k m == 0 then l else S.add k l with Not_found -> S.add k l
    in
    S.empty |> fold (has_binding_or_add n) m |> fold (has_binding_or_add m) n |> S.elements

  let to_string val_str t =
    let s =
      bindings t
      |> List.map (fun (k, v) -> Ord.to_string k ^ "=" ^ val_str k v)
      |> String.concat ", "
    in
    "{" ^ s ^ "}"

  let printer val_str f t = Format.pp_print_string f (to_string val_str t)
end

(** Useful instances *)

module Lbl = struct
  type t = Ll.lbl

  let compare = String.compare

  let to_string l = l
end

module LblM = MakeMap (Lbl)
module LblS = MakeSet (Lbl)

module Uid = struct
  type t = Ll.uid

  let compare = String.compare

  let to_string u = "%" ^ u
end

module UidS = MakeSet (Uid)
module UidM = MakeMap (Uid)

(** Undirected graphs with Uid vertices *)
module UidGraph = struct
  type t = UidS.t UidM.t

  (** Create empty graph *)
  let empty = UidM.empty

  (** Add edge u <-> v *)
  let add_edge graph u v =
    UidM.update u
      (function None -> Some UidS.empty | Some s -> Some (UidS.add v s))
      (UidM.update v (function None -> Some UidS.empty | Some s -> Some (UidS.add u s)) graph)

  (** Add a vertex with no neighbors *)
  let add_vertex graph v = UidM.update v (function None -> Some UidS.empty | x -> x) graph

  (** For each uid in uids *)
  let add_clique graph (uids : Ll.uid list) =
    let rec go graph = function
      | x :: xs ->
          go (List.fold_left (fun graph v -> add_edge graph x v) (add_vertex graph x) xs) xs
      | [] ->
          graph
    in
    go graph uids

  (** Number of vertices adjacent to a given one *)
  let degree graph v =
    UidS.cardinal (match UidM.find_opt graph v with None -> UidS.empty | Some s -> s)

  (** Remove a vertex (and all of its incident edges) from a graph *)
  let remove_vertex graph v = UidM.map (UidS.remove v) (UidM.remove v graph)

  (** List of neighbors of a vertex *)
  let neighbors graph v = match UidM.find_opt graph v with None -> UidS.empty | Some s -> s

  (** Find a minimal vertex, according to a given min function *)
  let min_vertex min graph =
    match UidM.bindings graph with
    | [] ->
        None
    | (x, _) :: xs ->
        Some (List.fold_left (fun v (y, _) -> min v y) x xs)

  (** Print the edges of a graph *)
  let print graph =
    Llutil.mapcat "\n"
      (fun (u, s) -> Printf.sprintf "%s: %s" u (UidS.to_string s))
      (UidM.bindings graph)
end

(** For testing   *)
let uidm (b : (Ll.uid * 'a) list) : 'a UidM.t =
  List.fold_left (fun m (k, v) -> UidM.add k v m) UidM.empty b

let lblm (b : (Ll.lbl * 'a) list) : 'a LblM.t =
  List.fold_left (fun m (k, v) -> LblM.add k v m) LblM.empty b

let uids (l : Ll.uid list) : UidS.t = UidS.of_list l

let lbls (l : Ll.lbl list) : LblS.t = LblS.of_list l
