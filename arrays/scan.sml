local
open A
val scanGrain = 4096
  fun seqScan f b a =
    let
      val n = length a
      val r = allocate n
      fun loop b i =
        if i = n then b
        else (set r (i, b); loop (f (b, get a i)) (i+1))
    in
      (r, loop b 0)
    end
in
fun scan f b a =
    if length a <= scanGrain then
      seqScan f b a
    else
      let
        val n = length a
        val nb = 1 + (n-1) div scanGrain

        val a' = allocate nb
        val _ = Primitives.parfor 1 (0, nb) (fn i =>
          let
            val lo = i*scanGrain
            val hi = Int.min (lo + scanGrain, n)
            fun loop b j =
              if j = hi then b
              else loop (f (b, get a j)) (j+1)
          in
            set a' (i, loop b lo)
          end)

        val (p, t) = scan f b a'

        val r = allocate n
        val _ = Primitives.parfor 1 (0, nb) (fn i =>
          let
            val lo = i*scanGrain
            val hi = Int.min (lo + scanGrain, n)
            fun loop b j =
              if j = hi then ()
              else (set r (j, b); loop (f (b, get a j)) (j+1))
          in
            loop (get p i) lo
          end)
      in
        (r, t)
      end
end

(*fun scan f b a =
  case A.length a of
    0 => (A.fromList [], b)
  | 1 => (A.fromList [b], A.get a 0)
  | n =>
      let
        val nb = 1 + (n-1) div 2

        fun contract i =
          if 2*i <> n-1 then
            f (A.get a (2*i), A.get a (2*i+1))
          else
            A.get a (2*i)

        val a' = tabulate contract nb

        val (p, t) = scan f b a'

        fun expand i =
          if i mod 2 = 0 then
            A.get p (i div 2)
          else
            f (A.get p (i div 2), A.get a (i-1))

        val p' = tabulate expand n
      in
        (p', t)
      end
*)
