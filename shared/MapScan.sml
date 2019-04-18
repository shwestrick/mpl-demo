structure MapScan :
sig
  (* `scan f b (n, lookup)` produces an array of length n+1 where the total is
   * written at index n. This makes it easy to implement both SEQUENCE.scan
   * SEQUENCE.scanIncl in terms of this function.
   *
   * The input sequence is defined as [lookup 0, ..., lookup (n-1)] *)
  val scan : ('a * 'a -> 'a) -> 'a -> int * (int -> 'a) -> 'a array

  val writeScan : 'a array -> ('a * 'a -> 'a) -> 'a -> int * (int -> 'a) -> unit
end =
struct

  structure P = Primitives
  val GRAIN = 4096

  fun writeScan result (f : 'a * 'a -> 'a) (b : 'a) (length, nth : int -> 'a) =
    if length <= GRAIN then
      let
        val total = P.loop (0, length) b (fn (x, i) =>
          (Array.update (result, i, x); f (x, nth i)))
      in
        Array.update (result, length, total)
      end
    else
      let
        val n = length
        val m = 1 + ((n - 1) div GRAIN) (* number of blocks *)

        val sums = P.alloc m
        val _ = P.parfor 1 (0, m) (fn i =>
          let
            val lo = i * GRAIN
            val hi = Int.min (lo + GRAIN, n)
            val sum = P.loop (lo, hi) b (fn (x, i) => f (x, nth i))
          in
            Array.update (sums, i, sum)
          end)

        val partials = scan f b (m, fn i => Array.sub (sums, i))

        val _ = P.parfor 1 (0, m) (fn i =>
          let
            val lo = i * GRAIN
            val hi = Int.min (lo + GRAIN, n)
            val b' = Array.sub (partials, i)
          in
            P.loop (lo, hi) b' (fn (x, i) =>
              (Array.update (result, i, x); f (x, nth i)));
            ()
          end)
      in
        Array.update (result, length, Array.sub (partials, m))
      end

  and scan f b (length, nth) =
    let
      val result = P.alloc (length + 1)
      val _ = writeScan result f b (length, nth)
    in
      result
    end

end
