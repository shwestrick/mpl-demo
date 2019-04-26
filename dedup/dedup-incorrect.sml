local

structure S = IntArraySlice
val parfor = Primitives.parfor
val hash = Util.hash

in

(* deduplicate a bunch of non-negative integers *)

(* THIS CODE IS INCORRECT. CAN YOU SPOT WHERE?
 *
 * bug seems fairly reproducible on large core counts for large inputs of a
 * small number of keys, for example:
 *   -N 10M -K 25
 * you will see that there are a small number of duplicates still in the
 * output... *)

fun dedupIncorrect input =
  let
    val n = S.length input

    (* tune this? *)
    val tableSize = 2 * n

    (* a hash table, open addressing. ~1 means empty slot *)
    val table = S.tabulate (fn _ => ~1) tableSize

    (* insert x at slot i; try again with linear probing *)
    fun insert x i =
      let
        val y = S.get table i
      in
        if y = ~1 andalso S.cas table (i, ~1, x) then
          () (* done. succesfully inserted. *)
        else if y = x then
          () (* done. someone else already inserted x. *)
        else
          (* try again, possibly wraparound to front *)
          insert x (if i = tableSize-1 then 0 else i+1)
      end

    val _ = parfor 8192 (0, n) (fn i =>
      let
        val x = S.get input i
      in
        insert x (hash x mod tableSize)
      end)

  in
    (* compact the table *)
    S.filter (fn x => x <> ~1) table
  end

end
