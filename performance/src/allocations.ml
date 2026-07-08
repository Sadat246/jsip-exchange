open! Core

module Build_list = struct
  (* [acc @ [ x ]] copies the whole accumulator each step -> O(n^2)
     allocation. *)
  let silly xs =
    ignore xs;
    failwith "TODO: part 4, 0d"
  ;;

  (* Prepend (O(1) per step) then reverse once -> O(n) allocation. Same
     result. *)
  let non_silly xs =
    ignore xs;
    failwith "TODO: part 4, 0d"
  ;;
end

module First_match = struct
  (* Allocate a fresh list of *every* match, then throw all but the head
     away. *)
  let silly xs ~f =
    ignore (xs, f);
    failwith "TODO: part 4, 0d"
  ;;

  (* Stop at the first match; allocate nothing but the returned [Some]. *)
  let non_silly xs ~f =
    ignore (xs, f);
    failwith "TODO: part 4, 0d"
  ;;
end
