signature PRIMITIVES =
sig

  val numberOfProcessors : int

  val par : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)
  val par3 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) -> ('a * 'b * 'c)
  val par4 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) * (unit -> 'd) -> ('a * 'b * 'c * 'd)

  val for : int * int -> (int -> unit) -> unit
  val forBackwards : int * int -> (int -> unit) -> unit
  val loop : int * int -> 'a -> ('a * int -> 'a) -> 'a
  val parfor : int -> (int * int) -> (int -> unit) -> unit

  val alloc : int -> 'a Array.array

  (* Optimized updates. May only do `refAssignUp (r, x)` if x was allocated
   * before r (and similarly for arrays with arrayUpdateUp). *)
  val refAssignUp : 'a ref * 'a -> unit
  val arrayUpdateUp : 'a array * int * 'a -> unit

  (* cas r (old, new) is equivalent to
   *   let
   *     val x = !r
   *     val _ = if x = old then r := new else ()
   *   in
   *     x
   *   end
   *)
  val compareAndSwap : int ref -> int * int -> int
  val arrayCompareAndSwap : int array * int -> int * int -> int

  val fetchAndAdd : int ref -> int -> int
  val arrayFetchAndAdd : int array * int -> int -> int

  (* (if the scheduler supports it) a scheduler hook to perform some load
   * balancing. this can be used as an optimization in long sequential
   * sections, particularly at the "leaves" of a divide-and-conquer
   * computation. *)
  val communicate : unit -> unit

end
