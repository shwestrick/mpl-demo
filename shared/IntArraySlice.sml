structure IntArraySlice :> INT_ARRAY_SLICE
  where type t = int ArraySlice.slice =
struct

  structure P = Primitives
  structure A = Array
  structure AS = ArraySlice
  structure Seq = ArraySequence

  type t = int AS.slice
  type slice = t
  type idx = int
  type elem = int

  exception Range = Seq.Range
  exception Size = Seq.Size

  fun newArray n =
    if n < 0 then
      raise Size
    else
      P.alloc n

  fun allocate n =
    AS.full (newArray n)

  fun tabulate f n =
    let
      val t = newArray n
    in
      P.parfor 8192 (0, n) (fn i => A.update (t, i, f i));
      AS.full t
    end

  fun filter p s =
    Seq.filter p s

  fun scan f b s =
    Seq.scan f b s

  fun copy s t =
    let
      val (sa, slo, n) = AS.base s
      val (ta, tlo, nt) = AS.base t
      val disjoint =
        sa <> ta orelse slo+n <= tlo orelse tlo+nt <= slo
    in
      if n <> nt then
        raise Size
      else if not disjoint then
        raise Range
      else
        P.parfor 8192 (0, n) (fn i =>
          A.update (ta, tlo+i, A.sub (sa, slo+i))
        )
    end

  fun length a = AS.length a

  fun get a i =
    AS.sub (a, i)
    handle Subscript => raise Range

  fun set a (i, x) =
    AS.update (a, i, x)
    handle Subscript => raise Range

  fun cas a (i, old, new) =
    if i < 0 orelse i >= length a then
      raise Range
    else
      let
        val (data, start, len) = AS.base a
        val result = P.arrayCompareAndSwap (data, start+i) (old, new)
      in
        result = old
      end

  fun subslice a (i, len) =
    Seq.subseq a (i, len)

  fun toString s =
    "[" ^ String.concatWith "," (List.tabulate (length s, Int.toString o (get s))) ^ "]"

end
