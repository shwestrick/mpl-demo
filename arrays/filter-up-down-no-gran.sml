local

open A
val parfor = Primitives.parfor
val par = Primitives.par

datatype 'a rtree = Leaf of 'a | Node of 'a * 'a rtree * 'a rtree
fun rval (Node (rv, _, _)) = rv
  | rval (Leaf rv) = rv

in

fun filterUpDownNoGran p a =
  let
    fun upsweep i j =
      if j - i = 0 then
        Leaf 0
      else if j - i = 1 then
        Leaf (if p (get a i) then 1 else 0)
      else
        let
          val mid = i + (j - i) div 2
          val (l, r) = par (fn _ => upsweep i mid, fn _ => upsweep mid j)
        in
          Node (rval l + rval r, l, r)
        end

    val tree = upsweep 0 (length a)
    val count = rval tree
    val output = allocate count

    fun downsweep offset t i j =
      case t of
        Leaf c =>
          if j - i <> 1 orelse c <> 1 then ()
          else set output (offset, get a i)
      | Node (_, l, r) =>
          let
            val mid = i + (j - i) div 2
            val offset' = offset + rval l
          in
            par (fn _ => downsweep offset l i mid,
                 fn _ => downsweep offset' r mid j);
            ()
          end

    val _ = downsweep 0 tree 0 (length a)
  in
    output
  end

end
