local

open A
val parfor = Primitives.parfor
val par = Primitives.par
val loop = Primitives.loop

datatype 'a rtree = Leaf of 'a | Node of 'a * 'a rtree * 'a rtree
fun rval (Node (rv, _, _)) = rv
  | rval (Leaf rv) = rv

val filterGrain = 16384

in

fun filterUpDown p a =
  let
    fun upsweep i j =
      if j - i <= filterGrain then
        Leaf (loop (i, j) 0 (fn (c, k) => if p (get a k) then c+1 else c))
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
        Leaf _ =>
          (loop (i, j) offset (fn (off, k) =>
             let
               val x = get a k
             in
               if p x then
                 (set output (off, x); off+1)
               else
                 off
             end);
           ())
      | Node (_, l, r) =>
          let
            val mid = i + (j - i) div 2
            val offset' = offset + rval l
          in
            Primitives.par (fn _ => downsweep offset l i mid,
                            fn _ => downsweep offset' r mid j);
            ()
          end

    val _ = downsweep 0 tree 0 (length a)
  in
    output
  end

end
