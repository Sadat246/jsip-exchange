open! Core

module Build_list = struct
  (* [acc @ [ x ]] copies the whole accumulator each step -> O(n^2)
     allocation. *)
  let silly xs = List.fold xs ~init:[] ~f:(fun acc x -> acc @ [ x ])

  (* Prepend (O(1) per step) then reverse once -> O(n) allocation. Same
     result. *)
  let non_silly xs =
    List.fold xs ~init:[] ~f:(fun acc x -> x :: acc) |> List.rev
  ;;
end

module First_match = struct
  (* Allocate a fresh list of *every* match, then throw all but the head
     away. *)
  let silly xs ~f = List.filter xs ~f |> List.hd

  (* Stop at the first match; allocate nothing but the returned [Some]. *)
  let non_silly xs ~f = List.find xs ~f
end
