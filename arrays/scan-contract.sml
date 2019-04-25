local

open A
val parfor = Primitives.parfor
val loop = Primitives.loop

val scanGrain = 4096

in

fun scanContract f b a =
  if length a <= scanGrain then
    let
      val n = length a
      val r = allocate n
      val t = loop (0, n) b (fn (acc, i) => (set r (i, acc); f (acc, get a i)))
    in
      (r, t)
    end
  else
    let
      val n = length a
      val nb = 1 + (n-1) div scanGrain

      val a' = allocate nb
      val _ = parfor 1 (0, nb) (fn i =>
        let
          val lo = i*scanGrain
          val hi = Int.min (lo + scanGrain, n)
          val t = loop (lo, hi) b (fn (acc, j) => f (acc, get a j))
        in
          set a' (i, t)
        end)

      val (p, t) = scanContract f b a'

      val r = allocate n
      val _ = parfor 1 (0, nb) (fn i =>
        let
          val lo = i*scanGrain
          val hi = Int.min (lo + scanGrain, n)
          val _ = loop (lo, hi) (get p i) (fn (acc, j) =>
            (set r (j, acc); f (acc, get a j)))
        in
          ()
        end)
    in
      (r, t)
    end

end
