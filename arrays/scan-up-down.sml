local

open A
val loop = Primitives.loop
val par = Primitives.par

datatype 'a rtree = Leaf of 'a | Node of 'a * 'a rtree * 'a rtree
fun rval (Node (rv, _, _)) = rv
  | rval (Leaf rv) = rv

val scanGrain = 4096

in

fun scanUpDown f b a =
  let
    fun upsweep i j =
      if j - i <= scanGrain then
        Leaf (loop (i, j) b (fn (acc, k) => f (acc, get a k)))
      else
        let
          val mid = i + (j - i) div 2
          val (l, r) = par (fn _ => upsweep i mid, fn _ => upsweep mid j)
        in
          Node (f (rval l, rval r), l, r)
        end

    val tree = upsweep 0 (length a)
    val total = rval tree
    val result = allocate (length a)

    fun downsweep b t i j =
      case t of
        Leaf _ =>
          (loop (i, j) b (fn (b, k) =>
             (set result (k, b); f (b, get a k)));
           ())
      | Node (_, l, r) =>
          let
            val mid = i + (j - i) div 2
            val b' = f (b, rval l)
          in
            par (fn _ => downsweep b l i mid,
                 fn _ => downsweep b' r mid j);
            ()
          end

    val _ = downsweep b tree 0 (length a)
  in
    (result, total)
  end

end
