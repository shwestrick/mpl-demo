local

open A
val parfor = Primitives.parfor
val loop = Primitives.loop

val filterGrain = 4096

in

fun filterContract p a =
  let
    val n = length a
    val nb = 1 + (n-1) div filterGrain

    val counts = allocate nb
    val _ = parfor 1 (0, nb) (fn i =>
      let
        val lo = i * filterGrain
        val hi = Int.min (lo + filterGrain, n)
        val count =
          loop (lo, hi) 0 (fn (c, j) => if p (get a j) then c+1 else c)
      in
        set counts (i, count)
      end)

    val (offsets, count) = scan op+ 0 counts
    val result = allocate count

    val _ = parfor 1 (0, nb) (fn i =>
      let
        val lo = i * filterGrain
        val hi = Int.min (lo + filterGrain, n)
        val offset = get offsets i
        val _ = loop (lo, hi) offset (fn (off, j) =>
          let
            val x = get a j
          in
            if p x then
              (set result (off, x); off+1)
            else
              off
          end)
      in
        ()
      end)
  in
    result
  end

end
