open! Core

module Map_int = struct
  type t = int Int.Map.t ref

  let create () = ref Int.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_int = struct
  type t = int Int.Table.t

  let create () = Int.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
end

module Map_string = struct
  type t = int String.Map.t ref

  let create () = ref String.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_string = struct
  type t = int String.Table.t

  let create () = String.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
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
  type t = int Fat_record.Map.t ref

  let create () = ref Fat_record.Map.empty
  let set t ~key ~data = t := Map.set !t ~key ~data
  let get t key = Map.find !t key
end

module Hashtable_record = struct
  type t = int Fat_record.Table.t

  let create () = Fat_record.Table.create ()
  let set t ~key ~data = Hashtbl.set t ~key ~data
  let get t key = Hashtbl.find t key
end
