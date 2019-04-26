signature INT_ARRAY_SLICE =
sig
  type t
  type slice = t
  type idx = int
  type elem = int

  exception Range
  exception Size

  (* allocate a fresh array of the specified length *)
  val allocate : int -> slice

  (* `tabulate f n` allocates a fresh array of length n and
   * fills it with the results of `f i` for each index i *)
  val tabulate : (idx -> elem) -> int -> slice

  val filter : (elem -> bool) -> slice -> slice
  val scan : (elem * elem -> elem) -> elem -> slice -> (slice * elem)

  val get : slice -> idx -> elem
  val set : slice -> (idx * elem) -> unit

  (* `cas a (i, old, new)` atomically performs the following:
   *   let
   *     val x = get a i
   *   in
   *     if x = old then
   *       (set a (i, new); true)
   *     else
   *       false
   *   end
   *)
  val cas : slice -> (idx * elem * elem) -> bool

  (* `copy input output` copies the contents of the input into
   * the output. input and output must be the same length *)
  val copy : slice -> slice -> unit

  val subslice : slice -> (idx * int) -> slice

  val length : slice -> int

  val toString : slice -> string
end
