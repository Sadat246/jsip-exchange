open! Core

module Map_int = struct
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

module Hashtable_int = struct
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

module Map_string = struct
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

module Hashtable_string = struct
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

module Fat_record = struct
  module T = struct
    type t =
      { a : int
      ; b : string
      ; c : float
      ; d : int
      ; e : string
      ; f : bool
      ; g : int
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let of_index i =
    { a = i
    ; b = Int.to_string i
    ; c = Float.of_int i
    ; d = i * 2
    ; e = sprintf "key-%d" i
    ; f = Int.equal (i land 1) 0
    ; g = i * i
    }
  ;;
end

module Map_record = struct
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

module Hashtable_record = struct
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
