open! Core

module List_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = unit

  let create () = ()
  let set t ~key ~data = ignore (t, key, data)

  let get t key =
    ignore (t, key);
    None
  ;;
end

module Dynarray_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = unit

  let create () = ()
  let set t ~key ~data = ignore (t, key, data)

  let get t key =
    ignore (t, key);
    None
  ;;
end
