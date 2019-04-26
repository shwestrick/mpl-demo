local

structure S = IntArraySlice
val parfor = Primitives.parfor
val hash = Util.hash

in

(* deduplicate a bunch of non-negative integers *)
fun dedup input =
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
        if y = ~1 then
          if S.cas table (i, ~1, x) then
            () (* done. succesfully inserted. *)
          else
            (* retry at same location! someone else beat us, but they could
             * have just inserted the same item. *)
            insert x i
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
