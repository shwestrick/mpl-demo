local
open A
val filterGrain = 4096
in
  fun filter p a =
    let
      val n = length a
      val nb = 1 + (n-1) div filterGrain

      val counts = allocate nb
      val _ = Primitives.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * filterGrain
          val hi = Int.min (lo + filterGrain, n)
          fun loop c j =
            if j = hi then c
            else loop (if p (get a j) then c+1 else c) (j+1)
        in
          set counts (i, loop 0 lo)
        end)

      val (offsets, count) = scan op+ 0 counts
      val result = allocate count

      val _ = Primitives.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * filterGrain
          val hi = Int.min (lo + filterGrain, n)
          fun loop off j =
            if j = hi then () else
            let
              val x = get a j
            in
              if p x then
                (set result (off, x); loop (off+1) (j+1))
              else
                loop off (j+1)
            end
        in
          loop (get offsets i) lo
        end)
    in
      result
    end
end


(*
fun filter p a =
  let
    val counts = A.map (fn x => if p x then 1 else 0) a
    val (offsets, count) = A.scan op+ 0 counts
    val result = A.allocate count
    val _ = Primitives.parfor 8192 (0, A.length a) (fn i =>
      let
        val x = A.get a i
      in
        if not (p x) then
          ()
        else
          A.set result (A.get offsets i, x)
      end)
  in
    result
  end
*)
