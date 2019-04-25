local

open A
val parfor = Primitives.parfor

in

fun filterSimple p a =
  let
    val counts = map (fn x => if p x then 1 else 0) a
    val (offsets, count) = scan op+ 0 counts
    val result = allocate count
    val _ = parfor 4096 (0, length a) (fn i =>
      let
        val x = get a i
      in
        if not (p x) then
          ()
        else
          set result (get offsets i, x)
      end)
  in
    result
  end

end
