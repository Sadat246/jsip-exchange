open! Core

module Silly_store = struct
  type t = (int * int) list ref

  let equal = Int.equal
  let create () = ref []
  let remove t key = t := List.Assoc.remove !t key ~equal

  (* Look it up. If it's not there, look it up again -- just to be sure lol. *)
  let get t key =
    let result = List.Assoc.find !t key ~equal:Int.equal in
    match result with
    | None -> (* try again just to be sure *) List.Assoc.find !t key ~equal
    | result -> result
  ;;

  (* Deliberately naive: [remove] the old binding (a no-op if absent), append
     the new one at the end, then [get] it back just to make sure it really
     landed. *)
  let set t ~key ~data =
    remove t key;
    t := List.append !t [ key, data ];
    match get t key with None -> failwith "Set failed" | Some _ -> ()
  ;;
end
