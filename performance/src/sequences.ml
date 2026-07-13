open! Core

module List_seq = struct
  type t = int list ref

  let create () = ref []

  let set t ~key ~data =
    let len = List.length !t in
    if key = len
    then t := List.append !t [ data ]
    else if key > len
    then
      raise_s
        [%message
          "set: key past end of sequence" (key : int) ~length:(len : int)]
    else
      t := List.mapi !t ~f:(fun i value -> if i = key then data else value)
  ;;

  let get t key =
    List.findi !t ~f:(fun index _ -> index = key) |> Option.map ~f:snd
  ;;
end

module Dynarray_seq = struct
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    let len = Dynarray.length t in
    if key = len
    then Dynarray.add_last t data
    else if key > len
    then
      raise_s
        [%message
          "set: key past end of sequence" (key : int) ~length:(len : int)]
    else Dynarray.set t key data
  ;;

  let get t key =
    if key >= 0 && key < Dynarray.length t
    then Some (Dynarray.get t key)
    else None
  ;;
end
