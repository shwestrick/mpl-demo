local

open A
val parfor = Primitives.parfor

in

fun inject a u =
  let
    val a' = tabulate (get a) (length a)
  in
    parfor 16384 (0, length u) (fn i =>
      let
        val (j, x) = get u i
      in
        set a' (j, x)
      end);
    a'
  end

end
