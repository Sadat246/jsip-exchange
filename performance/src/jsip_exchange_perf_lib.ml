open! Core
open Core_bench

(* Sizes are kept modest: [Silly_store] is O(n) per operation, so [build] is
   ~O(n^2). *)
let sizes = [ 10; 100; 1000 ]
let present_key n = n / 2
let absent_key = -1

let bench_silly =
  let build n =
    let store = Key_value_stores.Silly_store.create () in
    let (_ : unit list) =
      List.init n ~f:(fun key ->
        Key_value_stores.Silly_store.set store ~key ~data:key)
    in
    store
  in
  List.concat_map sizes ~f:(fun n ->
    let prebuilt = build n in
    [ Bench.Test.create
        ~name:(sprintf "Silly_store build (n=%d)" n)
        (fun () -> ignore (build n : Key_value_stores.Silly_store.t))
    ; Bench.Test.create
        ~name:(sprintf "Silly_store get_hit (n=%d)" n)
        (fun () ->
           ignore
             (Key_value_stores.Silly_store.get prebuilt (present_key n)
              : int option))
    ; Bench.Test.create
        ~name:(sprintf "Silly_store get_miss (n=%d)" n)
        (fun () ->
           ignore
             (Key_value_stores.Silly_store.get prebuilt absent_key
              : int option))
    ])
;;

(* One set of positional benchmarks for any contender, parameterized by its
   operations -- so we can share it across the three backings without a
   module type. Positional [set] appends at the end, so keys must be inserted
   in index order: hence [List.iter] over [List.init] rather than a
   side-effecting [List.init], whose evaluation order isn't guaranteed
   left-to-right. *)
let seq_tests ~name ~create ~set ~get =
  let build n =
    let t = create () in
    (* Insert 0,1,…,n-1 strictly in index order: positional [set] appends at
       the end, and [List.init]'s evaluation order isn't guaranteed
       left-to-right, so materialize the indices first, then [List.iter] them
       (which is left-to-right) doing the side-effecting [set]. *)
    List.iter (List.init n ~f:Fn.id) ~f:(fun key -> set t ~key ~data:key);
    t
  in
  List.concat_map sizes ~f:(fun n ->
    let prebuilt = build n in
    [ Bench.Test.create ~name:(sprintf "%s build (n=%d)" name n) (fun () ->
        ignore (build n))
    ; Bench.Test.create ~name:(sprintf "%s get_hit (n=%d)" name n) (fun () ->
        ignore (get prebuilt (present_key n) : int option))
    ; Bench.Test.create
        ~name:(sprintf "%s get_miss (n=%d)" name n)
        (fun () -> ignore (get prebuilt absent_key : int option))
    ])
;;

let bench_sequential =
  List.concat
    [ seq_tests
        ~name:"List_seq"
        ~create:Sequences.List_seq.create
        ~set:Sequences.List_seq.set
        ~get:Sequences.List_seq.get
    ; seq_tests
        ~name:"Dynarray_seq"
        ~create:Sequences.Dynarray_seq.create
        ~set:Sequences.Dynarray_seq.set
        ~get:Sequences.Dynarray_seq.get
    ]
;;

(* Associative benchmarks, parameterized by the container ops AND the key
   type. [key_of_index] maps an index to a key ([Fn.id] for int keys,
   [Int.to_string] for string keys), so the same helper covers both int- and
   string-keyed stores. *)
let assoc_tests ~name ~create ~set ~get ~key_of_index =
  let build n =
    let store = create () in
    (* associative set works at any key, so insertion order doesn't matter *)
    List.iter (List.init n ~f:Fn.id) ~f:(fun i ->
      set store ~key:(key_of_index i) ~data:i);
    store
  in
  List.concat_map sizes ~f:(fun n ->
    let prebuilt = build n in
    [ Bench.Test.create ~name:(sprintf "%s build (n=%d)" name n) (fun () ->
        ignore (build n))
    ; Bench.Test.create ~name:(sprintf "%s get_hit (n=%d)" name n) (fun () ->
        ignore (get prebuilt (key_of_index (present_key n)) : int option))
    ; Bench.Test.create
        ~name:(sprintf "%s get_miss (n=%d)" name n)
        (fun () ->
           ignore (get prebuilt (key_of_index absent_key) : int option))
    ])
;;

let bench_associative =
  List.concat
    [ assoc_tests
        ~name:"Map_int"
        ~create:Associatives.Map_int.create
        ~set:Associatives.Map_int.set
        ~get:Associatives.Map_int.get
        ~key_of_index:Fn.id
    ; assoc_tests
        ~name:"Hashtable_int"
        ~create:Associatives.Hashtable_int.create
        ~set:Associatives.Hashtable_int.set
        ~get:Associatives.Hashtable_int.get
        ~key_of_index:Fn.id
    ; assoc_tests
        ~name:"Map_string"
        ~create:Associatives.Map_string.create
        ~set:Associatives.Map_string.set
        ~get:Associatives.Map_string.get
        ~key_of_index:Int.to_string
    ; assoc_tests
        ~name:"Hashtable_string"
        ~create:Associatives.Hashtable_string.create
        ~set:Associatives.Hashtable_string.set
        ~get:Associatives.Hashtable_string.get
        ~key_of_index:Int.to_string
    ; assoc_tests
        ~name:"Map_record"
        ~create:Associatives.Map_record.create
        ~set:Associatives.Map_record.set
        ~get:Associatives.Map_record.get
        ~key_of_index:Associatives.Fat_record.of_index
    ; assoc_tests
        ~name:"Hashtable_record"
        ~create:Associatives.Hashtable_record.create
        ~set:Associatives.Hashtable_record.set
        ~get:Associatives.Hashtable_record.get
        ~key_of_index:Associatives.Fat_record.of_index
    ]
;;

(* Silly-vs-non-silly allocation: the same task done two ways, differing only
   in how much they allocate (watch the mWd/Run column). Inputs are prebuilt
   so the timed op is just the pattern under test. *)
let bench_allocation =
  List.concat_map sizes ~f:(fun n ->
    let input = List.init n ~f:Fn.id in
    [ Bench.Test.create
        ~name:(sprintf "Build_list.silly (n=%d)" n)
        (fun () -> ignore (Allocations.Build_list.silly input : int list))
    ; Bench.Test.create
        ~name:(sprintf "Build_list.non_silly (n=%d)" n)
        (fun () ->
           ignore (Allocations.Build_list.non_silly input : int list))
    ; Bench.Test.create
        ~name:(sprintf "First_match.silly (n=%d)" n)
        (fun () ->
           ignore
             (Allocations.First_match.silly input ~f:(fun _ -> true)
              : int option))
    ; Bench.Test.create
        ~name:(sprintf "First_match.non_silly (n=%d)" n)
        (fun () ->
           ignore
             (Allocations.First_match.non_silly input ~f:(fun _ -> true)
              : int option))
    ])
;;

let command =
  Command.group
    ~summary:"JSIP performance pre-exercises"
    [ "silly", Bench.make_command bench_silly
    ; "sequential", Bench.make_command bench_sequential
    ; "associative", Bench.make_command bench_associative
    ; "allocation", Bench.make_command bench_allocation
    ]
;;
