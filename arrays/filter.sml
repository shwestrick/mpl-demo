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
