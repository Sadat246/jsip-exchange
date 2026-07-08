# JSIP Exercises - Part 4

Part 4 is about **performance**, and about the data structures and algorithms that determine it. Up to now you've mostly been reasoning about correctness. Does the exchange match orders by the right rules? Do the client and server speak the same protocol? In this part you'll turn to the _resource_ dimension: how fast the hot paths run, how much memory they allocate, and how to _measure_ those things instead of guessing at them.

You started thinking about the resource dimension in Part 3's adversarial bots. In this part, you'll be considering resources from inside the exchange. You'll be employing data structures and code that seeks to make the core of the exchange faster and leaner, and back up those changes with meaningful benchmarks and measurements. The modifications you'll make come in two flavors:

- **Interface-preserving internal changes.** You swap out a data structure behind an unchanged `.mli`, and every existing test still passes untouched. The improvement is invisible to callers; only the benchmark moves. Replacing the order book's list with a smarter structure is the archetype here.
- **Cross-cutting representation changes.** You change how a value is _represented_. Figuring out how to replace a string with an int (more memory-efficient), for example, has representation changes that ripple across modules and even process boundaries. You'll have to decide in more detail not only _what_ representations you want to change, but _how_ to do so in a way that preserves correctness, readability, and maintainability.

For this part you're back to working individually, and you're welcome to use Claude Code.

Exercise 0 is a standalone sequence that doesn't touch the exchange code at all and will introduce you to the mechanics of benchmarking with ocaml using the `Core_bench` library. It starts with a deliberately silly data structure and progresses through a variety of common ones. The subsequent exercises bring you back to the exchange code and invite you to make an assortment of performance-oriented changes.

**Learning goals for Part 4:**

- Measure before you optimize. Write and read `core_bench` benchmarks, interpret the `Time/Run`, `mWd/Run`, and GC columns, and learn to distinguish a real regression or speedup from run-to-run noise.
- Reason about the time and space complexity of a number of core data structures such as list and array, and understand their strengths and weaknesses.
- Understand the cost of memory allocation (even in a garbage-collected language) by working with patterns that allocate poorly.
- Represent values compactly and understand _boundary design_: where a compact representation should live inside the system, and where it must be translated back to something more human-friendly.

## Exercise 0: Benchmarking warm-up

Read the "Benchmarks" section of this project's [README.md](../README.md). Build and run the benchmarks in `lib/order_book/bench`. Make sure you can run the benchmarks with the `-ascii` flag and that you can understand each column's meaning; the README explains these briefly and they are comprehensively documented online.

The goal of Exercise 0 is not to write clever code. It's to learn to _measure_: to run a benchmark, read its output, and connect what the numbers say to what the code does. Every sub-exercise here invites you to write some code and pair it with a benchmarking run. You will lean heavily on this skill for the rest of Part 4, in which every change you make to the exchange has to be justified by a measurement.

### 0a: Using `bench`

You're given two files in `performance/src`:

- `key_value_stores.ml` / `.mli`, which define `Silly_store`: a deliberately naive `int -> int` key-value store backed by an association list. Read the `.mli` first for the intended behavior, then read the `.ml`. It is written to be slow on purpose: `set` removes any old binding, appends the new one at the _end_ of the list with `List.append`, and then reads it back just to make sure. `get`, too, retries misses a second time "just in case". These are dumb choices but serve to highlight certain behavior and let you get comfortable with the tooling.
- `jsip_exchange_perf_lib.ml`: the `sizes`, `present_key`, and `absent_key` helpers, the `bench_silly` definition, and the `command` group at the bottom.

**What to do:**

1. **Run it and match rows to code.**
   Build the project and run the `silly` benchmark:
   ```sh
   dune exec performance/bin/main.exe -- silly -ascii -quota 1
   ```
   In `jsip_exchange_perf_lib.ml`, `Bench.Test.create ~name (fun () -> ...)` defines one benchmark; the anonymous function that just takes unit (called a _thunk_) is the thing that gets run over and over and timed. `List.concat_map sizes ~f:...` is why you see the same three tests (`build`, `get_hit`, `get_miss`) repeated at `n = 10`, `100`, and `1000`. `Bench.make_command` turns that list of tests into the `silly` subcommand.
2. **Notice what is and isn't inside the timed thunk.** For the `get` benchmarks, `prebuilt = build n` is computed _once_, outside the thunk, so the benchmark measures only the lookup, not the cost of building the store. The `build` benchmark, by contrast, calls `build n` _inside_ the thunk, so it measures construction. This "set up fixtures outside, measure only the operation inside" split is the single most important habit in writing an honest benchmark.
3. **Understand the `ignore (expr : t)` wrapper.** Each thunk wraps its result in `ignore (... : _)`. If you simply computed a value and threw it away with nothing observing it, the compiler would be free to optimize the whole call away and you'd be timing nothing. Forcing the result (here, by `ignore`-ing it with an explicit type annotation, as our style requires) keeps the work alive.
4. **Vary the quota.** Re-run with `-quota 1`, `-quota 5`, `-quota 10`. The quota is roughly how long `core_bench` spends sampling each benchmark. A bigger quota means more samples, more stable numbers, and a longer wait. Watch how much the reported `Time/Run` changes between runs at quota 1 versus quota 5; that difference gives you a sense of how much noise there is in these measurements.
5. **Vary the sizes.** Change `let sizes = [ 10; 100; 1000 ]`. Add `10_000` and/or intermediate points like `[ 10; 30; 100; 300; 1000 ]`. (Be patient: because `build` is $O(n^2)$, `n = 10_000` takes a while.)

**Things to consider:**

- **How do these operations scale, and why? does `build` scale, and why?** In addition to observing the asymptotic performance, make sure you understand _from the code_ why it's behaving that way. How does `build` scale? `get`?
- **What does `mWd/Run` tell you about how each of these allocates?**
- Differences down near a few nanoseconds, or a handful of words, are in the noise. Don't over-interpret them; if a difference looks marginal, re-run with a larger quota before drawing a conclusion.
- The command `core_bench` builds for you has more flags than `-quota` and `-ascii`. Run it with `-help` to see how to filter which benchmarks run by name and how to choose which columns to display.

### 0b: Sequential access — list vs. `Dynarray`

Now it's time for you to fill in your own implementations of positional containers in `sequences.ml` and benchmark them head to head. In both the key is a 0-based index into the sequence, and both support the same operations:

- `create : unit -> t`
- `set : t -> key:int -> data:int -> unit` update the element at index `key`; if `key` equals the current length, append (growing the sequence); raise if `key` is out of range.
- `get : t -> int -> int option` the element at that index, or `None` if the index is out of range.

(Growing the store only when the key is one past the current end ensures that our data structure never has any holes and remains dense.)

Back the two implementations with, respectively, a `list` and the stdlib `Dynarray`.

Add benchmarks to `jsip_exchange_perf_lib.ml` for these implementations. Use the same approach as 0a's `silly` command does — a `build` that inserts `n` elements in index order, a `get` at a present index, and a `get` out of range — swept over a range of sizes and wired up as the `sequential` subcommand.

Run the benchmarks! Make sure you understand the performance characteristics of both `set` and `get`, and why.

**Optional:**

- Add a `remove` that drops the element at an index and shifts the later ones down. Try benchmarking while removing from both the beginning and the end. Predict the performance before running the benchmarks.

### 0c: Associative access — `Map` vs. `Hashtbl`, and the cost of the key

Now fill in the implementations in `associatives.ml` to build key→`int` stores backed by both `Map` and `Hashtbl`, for three different key types, and benchmark all six against each other. Each store supports `create`, `set ~key ~data`, and `get` (returning an `int option`); the value is always an `int` and only the key type varies.

The six stores are the cross product of {`Map`, `Hashtbl`} × {`int` key, `string` key, fat-record key}:

- For the int and string keys, generate keys from an index (e.g. `Fn.id` and `Int.to_string`).
- For the fat-record key, we use a record with several fields of mixed types — a few ints, a couple of strings, a bool, a variant. We derive `compare` and `hash`, and implement a way to build a distinct key from an index. This is a deliberately expensive operation, so the cost of comparing and hashing it is visible.

Benchmark `build`, `get_hit` (a key that's present), and `get_miss` (a key that isn't) across a range of sizes, via the `associative` subcommand.

**Questions:**

- How does `Map` scale vs `Hashtbl` as `n` grows, and why?
- Where is the crossover point? (Use intermediate sizes to pin it down.) At roughly what `n` do `Map` and `Hashtbl` beat the association list from 0a? At what `n` does `Hashtbl` pull meaningfully away from `Map` for int keys.

### 0d: Allocation — the same answer, computed wastefully

This one trains your eye on the `mWd/Run` column. Take two small tasks, and fill in the implementations in `allocations.ml` that each return the same answer but allocate very differently, and benchmark the pairs against each other.

- **Copy a list.** One version builds the result by appending with `@` at every step (`acc @ [x]`); the other prepends and reverses once at the end.
- **First match.** Find the first element of a list satisfying a predicate. One version filters the whole list and takes the head; the other uses `List.find`.

Benchmark all four as an `allocation` subcommand, and read `mWd/Run` before `Time/Run`.

**Questions:**

- For each pair, where exactly does the extra allocation come from?
- How does each version's `mWd/Run` scale with `n`? Which are $O(n)$ and which $O(n^2)$ in allocation — and does `Time/Run` follow?
- Which of these wasteful patterns is easiest to write by accident, and why?

### 0e: Give `Hashtbl_int` an $O(1)$ `random_element` [OPTIONAL]

Let's start with `Hashtbl_int`: a hashtable that uses ints as keys. We might use this inside an exchange to hold onto orders. But picture a chaos bot that, on every tick, cancels one _uniformly random_ resting order while orders are constantly being added and removed. It needs three things to all be "fast": add an order, remove an order, and pick a random one.

Start from `Hashtbl_int`, the int→int hash table from 0c. It already gives you $O(1)$ `set`, `get`, and `remove`. You're going to add a fourth operation:

```ocaml
val random_element : t -> Random.State.t -> int option
```

which returns the value associated with a uniformly random _key_ in the store (or `None` if it's empty). Thread an explicit `Random.State.t` so your tests are reproducible rather than relying on global randomness.

First, try implementing it naively and for correctness, i.e., without changing the underlying data structure. Benchmark it! You'll find that `random_element` isn't very performant. How bad is it?

Next, try to fix `random_element` by changing the representation. How should you change/augment the existing `Hashtbl`? Make sure to benchmark to measure the results and payoff.

## Exercise 1: Snapshot side

This is your first change to the exchange itself, and the gentlest one in Part 4. You'll fix how the order book produces a _snapshot_ - the read-only `Book.t` a client gets back when it runs the book-query RPC - making it both more correct and faster. Note this path is **display-only**: it runs when someone asks to _see_ the book, not on the matching hot path (which uses `find_match` / `best_price` / `best_bid_offer`). So the payoff here is correctness plus a cheaper query, and it's a lower-stakes warm-up before the hot-path work later in Part 4. Along the way you'll get the existing order-book benchmark running, which you'll lean on for the rest of Part 4.

**First, run the existing benchmark.** Before changing anything, get `lib/order_book/bench/bench_order_book.ml` building and running:

```sh
dune exec lib/order_book/bench/bench_order_book.exe -- -ascii -quota 5
```

Quota 5 takes a few minutes; use `-quota 1` for a quick, noisier look while you iterate. You already know how to read this output from Ex 0. Match the benchmark names to the table in the "Benchmarks" section of the [`README.md`](../README.md) — `find_match`, `find_match_miss`, `best_bid_offer`, `add+remove`, `submit_ioc_cross`, `submit_ioc_miss`, `submit_sweep_*`, `find_match_alloc` — and note the `(n=...)` suffix is the number of resting orders. Skim `bench_order_book.ml` itself: you'll recognize the Ex-0 habits: fixtures (`book_with_n_asks`, `engine_with_n_asks`) built once _outside_ the timed thunk, `ignore (... : _)` to keep results alive, and a size sweep to show scaling. You don't need to change anything yet; this is just to confirm your toolchain works and you can read the numbers.

**Restructure the benchmark into subcommands.** Today `bench_order_book.ml` ends in a single flat `Command_unix.run (Bench.make_command tests)`: one giant suite that runs everything at once. As you add benchmarks over the next few exercises you'll want to run just the family you care about rather than re-run the whole multi-minute suite. You already know the shape from Ex 0's `performance/src/jsip_exchange_perf_lib.ml`: a `Command.group` of `Bench.make_command`s. Refactor the bottom of the file into a group whose first subcommand, `existing`, holds the current tests:

```ocaml
let () =
  Command_unix.run
    (Command.group
       ~summary:"JSIP order-book benchmarks"
       [ "existing", Bench.make_command tests ])
;;
```

You now invoke a subcommand, e.g. `dune exec lib/order_book/bench/bench_order_book.exe -- existing -ascii -quota 1`. Every Part 4 exercise that adds a benchmark from here on will add a new subcommand alongside `existing`.

**Now fix `snapshot_side`.** `Order_book.snapshot` builds the `Book.t` that clients see. By contract (read `book.mli`), a `Book.t`'s `bids` and `asks` are price levels **aggregated by price**: one `Level.t` per distinct price, holding the _total_ resting size there, best price first (bids high→low, asks low→high). That's exactly how `best_bid_offer` already reports the top of book — it sums all the size resting at the best price.

But `snapshot_side` doesn't aggregate:

```ocaml
let snapshot_side t (side : Side.t) =
  let compare =
    match side with
    | Buy -> Comparable.reverse Level.compare
    | Sell -> Level.compare
  in
  orders_on_side t side |> List.map ~f:Level.of_order |> List.sort ~compare
;;
```

It maps _each order_ to its own `Level.t` and then sorts. Two problems:

1. **It's wrong when multiple orders rest at the same price.** That contradicts the `Book.t` contract and disagrees with the BBO, which _does_ aggregate. (The existing snapshot test doesn't catch this because every order in it sits at a distinct price.)
2. **The sort is redundant.** `orders_on_side` returns `Map.data`, and the book's map is keyed by `(price, order_id)`, so the orders already come out sorted by price ascending, ties broken by arrival time. Sorting them again is $O(n \log n)$ work on already-sorted data.

Rewrite `snapshot_side` so it:

- **aggregates** runs of same-price orders into a single `Level.t` whose size is the sum of their remaining sizes, and **exploits the order the map already gives you** instead of re-sorting:
  for asks, the ascending map order is already lowest-price-first; for bids you only need to _reverse_ it to get highest-price-first. One linear pass, no `List.sort`.

The `snapshot` interface doesn't change — this is a pure internal improvement, the same flavor of interface-preserving optimization you'll do throughout Part 4.

**Write the benchmark:** `book_with_n_asks` puts every order at a _distinct_ price, so aggregation would do nothing there. This is a good reminder that a benchmark only measures the case you actually build. Add a fixture that stacks many orders at the **same** price (say `n` sells all at $150.00) and a `bench_snapshot ~n` that times `Order_book.snapshot` on it. Add these as a new `snapshot` subcommand next to `existing`:

```ocaml
[ "existing", Bench.make_command tests
; ( "snapshot"
  , Bench.make_command (List.map sizes ~f:(fun n -> bench_snapshot ~n)) )
]
```

(You may need to lift `sizes` out of `main` so both subcommands can see it.)

Run the benchmarks with `dune exec lib/order_book/bench/bench_order_book.exe -- snapshot -ascii -quota 1`. The previous code allocated a list of `n` levels and sorts it, so `Time/Run` should grow at $O(n \log n)$ and `mWd/Run` grows with `n`. After the changes, how fast are these?

Add a new expect test to the "snapshot" section of `test_order_book.ml` that stacks **multiple participants at the same price level**. Don't let a bug creep back in if you decide to change this implementation later.

## Exercise 2: Internal symbol-as-int

Your second change is a pure **internal** optimization: swap a data structure inside the matching engine while its `.mli` stays the same. The theme is one you'll see again: **strings at the edges, ints (and arrays) on the inside.**

The engine holds one order book per symbol:

```ocaml
type t =
  { books : Order_book.t Symbol.Map.t
  ; ...
  }
```

and every lookup goes through the map — `book`, `submit`, and `cancel` all do `Map.find t.books symbol` (or `Map.find_exn`). A `Symbol.t` is a string, so each `Map.find` walks the balanced tree doing $O(\log n)$ **string comparisons**, where $n$ is the number of symbols the engine trades. With a handful of symbols that's nothing; with thousands, it's increasingly bad. The fix is to give each symbol a small integer id and index the books by that id in a linear data structure instead of by the symbol string. However you build it, the shape of the win is the same: a lookup becomes one string **hash** plus an $O(1)$ array index, instead of $O(\log n)$ string comparisons that grow with the number of symbols.

In other words, we want to do something like replace the `Order_book.t Symbol.Map.t` with an `Order_book.t array`, and set up a new table mapping symbol to int id.

Start by writing a new benchmark. The existing ones are all **single-symbol**, so they never touch the `books` lookup. You need to add new tests (and a new `Command.group` to run them) to exercise them. But make sure that they only run `book` and not `submit`/`cancel`; the former is the pure lookup whereas the latter bury the symbol lookup under matching work. It's helpful to write the benchmark before you make changes in `matching_engine.ml` because then you can capture the "before" numbers and compare them against the "after" ones.

In terms of the matching engine changes, the natural place to assign those ids is `create`, which already receives the engine's full symbol list: walk it once, handing symbol _i_ the id _i_, and build the symbol→id table and the `Order_book.t array` together. Because the symbol set is fixed at `create` and never grows, the ids stay stable for the engine's lifetime and the books can live in a plain fixed-size array.

How you structure it is up to you. A bare hashtable paired with an array will work, but consider a small `Symbol_registry`-type module that owns the mapping and the order books. In all cases, keep all changes in the ml file; the point is that the interface stays the same while the implementation changes. Whatever implementation you pick, you'll need to also modify `book`, `submit`, and `cancel`, which also have to resolve a symbol. Remember, the `matching_engine.mli` doesn't change at all — this is an interface-preserving swap, and the proof you did it right is that the existing engine tests still pass untouched.

Re-run your new benchmark. At just a few symbols you won't see much of a win by doing this interning work. Try generating 100 symbols, or 10_000, and see what results you get!

## Exercise 3: Internal participant-as-int

Exercises 1 and 2 were internal-only: you changed a data structure behind an unchanged `.mli` and no other file noticed. This one is bigger - change that spans a few modules inside the server - but by design it still **never touches the wire**. It's the same "string at the edge, int on the inside" idea as Exercise 2, except the edge is now a **login handshake**, and the real content isn't raw speed (that's just Ex 0's `Hashtable_string` vs `Hashtable_int`, and with a handful of participants it's negligible). It's **boundary design**. Keeping the change entirely server-side is the whole point: it's the warm-up for Exercise 4, which takes the same idea and deliberately pushes it across the wire.

**How participant identity flows today.** Identity is established once, at login (`login_rpc` takes a name string and returns a `Participant.t`, itself a string); `submit` and `cancel` carry no participant, so the server injects it from the session. From there the name is used as a key in `logged_in_participants` (exchange_server) and `sessions` (dispatcher), and embedded in the events the engine emits (a `Fill` names two participants).

Intern each name to a small integer id at login, key the server's own lookup tables by that id, and resolve back to the name at the edges. Nothing outside the server ever sees the id.

**The design decisions are the exercise.** Three of them:

1. **Two types, not one — and the id is server-local.** The tempting move is to make `Participant.t` _itself_ an int. Do not do this! It's a pure string with pure `of_string`/`to_string`, and turning it into an int would drag a stateful, server-global registry into a pure type. Instead leave `Participant.t` as the human **name**, and add a separate type for the id: call it `Participant_id.t` and have types type be a private int. Because it never crosses the wire, it belongs in the **server** (the gateway), _not_ in `lib/types` beside the wire types. All the statefulness stays quarantined in the registry that maps between name and id.

2. **A server-global, additive registry.** This needs to be distinct from `logged_in_participants`. The registry (name->id, id->name) be **shared across all connections**: a fill names two participants, and an id has to mean the same thing to everyone who sees it. And it must be **additive**: an id stays valid for the whole run, so a participant keeps the same id across reconnects. That's a different structure with a different lifetime from `logged_in_participants`, which tracks who is _currently connected_ and is pruned on disconnect. These are different jobs and require distinct structures. Participants are interned _dynamically_ at login (not fixed at startup like symbols in Ex 2), so the registry will need to grow. What kind of data structure do you want to use?

3. **Find the boundary.** Don't change anything that goes over the wire for this exercise. The name enters at the login edge; inside the server, the id is what you key on. But the id stops at the server's own edges: whenever you hand a participant to something that speaks _names_, like exchange events, or any human-facing display, you resolve the id back to a name through the registry. The client never sees the id, the wire types don't change, and no participant directory is needed.

Stopping at the gateway is a deliberate choice. Pushing the id further, eg into the engine's `orders_by_client_id`, would mean either changing the engine's participant interface or teaching it about the registry, and it would pull the id toward the wire. We'll have a go at this later. For now, keep it server-side.

**No benchmark for this exercise.** Unlike every other exercise in Part 4, this one adds _no_ `core_bench` subcommand to `bench_order_book.ml`. The per-lookup difference (an int key vs a string key) is real but tiny and buried under everything else an order does, and with only a handful of participants a timing run would be pure noise. The payoff here is architectural, so the evidence is correctness, not speed.

## Exercise 4: External symbol-as-int

In Exercise 2 you interned symbols to ints _inside_ the matching engine, but the outside world never noticed: the wire still carried `Symbol.t` strings, and the engine hashed each one back to an id on the way in. Here you finish the job. You push the id all the way **out**, onto the wire and into the clients, so a symbol is an int from the moment it leaves a client to the moment it comes back.

This is the mirror image of Exercise 3. There, the interned id stayed inside the server — no wire change, no directory, because a participant only ever renders itself. Here the id deliberately **crosses the wire**: the `bin_io` digests change and every message shrinks (and int uses fewer bytes than a string), and because a client deals with _many_ symbols, you'll add a **directory** so humans still see names.

Symbols cross the wire in the order, book-query, market-data, and event-stream RPCs. Every one carries the symbol as a string, on every message. What makes this tractable is that the server never actually _renders_ a symbol; turning a symbol into text is entirely consumer- and test-side. So the server can run on the integer id end-to-end, and its only new jobs are to _serve_ the directory and to _validate_ incoming ids — never to turn one back into a name. You should approach this exercise in two phases: first refactor to use ints everywhere (even when it's awkward) and then recover the human-readable names where relevant.

### Phase 1: ints everywhere

Get the whole system working on raw ints first, and prove it's _functionally complete_ with no name-recovery machinery at all. Humans type ids (`BUY 7 100` for symbol id 7), and output prints ids. Some things to get you started:

- Add `Symbol_id.t` in `lib/types`. Make it a private int. Put it next to the other id types. (This one crosses the wire, so it must live with the wire types, not in the gateway.)
- Modify the payload and query types to use `Symbol_id.t` instead of `Symbol.t`: `Order.Request`, `Book`, `Fill`, the `Exchange_event` variants, and the `book-query` / `market-data` query types.
- Make sure you validate the ids. Don't trust the client! If one sends malformed or out-of-range ids, they have to be rejected.
- Keep `lib/types` pure data: it carries ints, and its `to_string` can only print ints. Focus on correctness and do **not** thread a registry into `lib/types` to recover names - not yet!

At the end of Phase 1 the exchange is fully working. You can submit, match, query the book, and stream market data entirely in ints. That's the proof that the directory is a _readability_ feature, not a correctness one.

### Phase 2: add a directory to recover names

Now you're going to recover human names without giving up the int on the wire. The directory is the new piece:

- Add a `symbol-directory` RPC that serves the `(name, id)` pairs. The authoritative registry lives in `main` (built from the symbol set the server already creates) and is passed into `Exchange_server.start`; the directory RPC just reads it.
- The client and monitor fetch the directory once at connect and build a local mirror (both directions).
- Resolve **name->id at parse** (when a human types `BUY AAPL 100`, `Exchange_command` looks up the id) and **id->name at render** (when printing an event or book).
- **Hydration is a consumer concern.** `lib/types` stays int-only. At the consumer's ingress you can either hydrate the decoded int into a small `{ id; name }` record so the shared `to_string` prints the name, or look up id->name at the render site via the directory. Either keeps `lib/types`' `to_string` pure and puts the resolution where the directory already lives: in the client and monitor.

**Two types, as before.** `Symbol.t` stays the human **name** at the edges; `Symbol_id.t` is the private int on the wire and inside the server. The registry maps between them; it's authoritative in the server's `main` and mirrored on each client via the directory. Don't collapse them, and don't make `Symbol.t` itself an int (same reasoning as Exercise 3).

**Write the benchmark:** two things, both in `bench_order_book.ml`.

- **Lookup:** extend the `symbol-lookup` subcommand from Exercise 2. The request now carries the id directly, so the server does _no hashing at all_ — just a bounds check and an array index. Compare against Exercise 2's hash-then-index across the same `[ 10; 100; 10_000 ]` symbol counts.
- **Payload size (optional)** This is where the external change pays off. Add `bin_size_t` measurements on `Order.Request.t`, `Book.t`, `Fill.t`, and `Exchange_event.t`, showing the per-message shrink from a string symbol to an int. It's a deterministic byte count (so it belongs in an expect test in `lib/types`, like Exercise 3's participant payload, except this one actually ships on the wire), and multiplied across every order and every streamed event it's real bandwidth.

There might be some changes that affect some existing tests:

- The RPC shapes changed, so if you have expect tests that validate this, update them.
- Symbol ordering is now **numeric, not lexicographic**, so some multi-symbol expect outputs reorder.

Write new tests for id validation, including rejection of an out-of-range id.

Make sure that everything continues to round-trip. Make sure you can still use, say, `BUY AAPL` on the client, that it maps to the book with the correct id on the server, and that it renders back as `AAPL` on the client.

## Exercise 5: Book by price [OPTIONAL]

This is an open-ended exercise. Since Part 1 the order book has stored orders in a flat `(Price.t, Order_id.t)` map. That structure is good at some things and not so good at others. This exercise has no prescribed design: your job is to find the operations that scale badly, work out _why_, and redesign the structure. Make sure you have a good theory for the expected behavior changes and justify your choice with measurements.

It stays a pure **internal** change. `order_book.mli` must not move, nothing on the wire or in the client is involved, and the existing `order_book` tests are your safety net. But within those bounds, the structure is yours to choose.

**First, find the slow operations.** Run the `existing` benchmark and watch how each operation scales as the number of resting orders grows — some stay flat, some climb. Then read `order_book.ml` and, for anything that climbs, work out what it's doing that's proportional to the whole book. A gentle nudge: pay closest attention to the queries that _sound_ like they should be cheap — the top of book, a book snapshot — and check whether the benchmark agrees.

**Then ask why.** For each slow operation, ask what the current structure _lacks_ that forces the work — what would have to be true of the data for that query to be cheap instead? And characterize the flip side: what is the flat composite-key map genuinely good at? A good redesign keeps those wins rather than trading them away.

**Then design something better — and defend it.** Using what Exercise 0 taught you about what each container is fast at, propose a structure that makes the slow operations cheap. (Part 1 Exercise 10 sketched an alternative in passing that's worth revisiting.) Whatever you choose, argue it explicitly:

- Which operations does it make faster, and to what complexity?
- What does it cost? In other operations, in memory, in code complexity?
- How does cancellation stay cheap? Removing an arbitrary resting order by id is the operation that quietly punishes a naive choice: some structures let you remove an element you already have a reference to in constant time, while others force a scan to find it first. Think about what you'd need to hold onto to avoid the scan.

Then _measure_: predict the before/after for each affected benchmark, make the change, and check whether reality matches your prediction. A prediction that's wrong is more informative than one that's right. Chase down why!

If your redesign is the right shape, some of your earlier Part 4 work stops being necessary. Watch whether the snapshot aggregation you bolted on in Exercise 1, or the top-of-book size that
`best_bid_offer` recomputes, becomes something the structure simply _gives_ you instead of something you calculate. When a whole class of "compute it each time" work disappears, that's the signal your structure finally matches the questions the book is being asked.

You should be able to use existing benchmarks for this, but feel free to add new ones to measure some new improved performance you might have unlocked.
