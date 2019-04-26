local
open A
val par = Primitives.par
in

fun par' doPar (f, g) =
  if doPar then par (f, g) else (f (), g ())

fun parfor grain (i, j) f =
  if      j - i = 0 then ()
  else if j - i = 1 then f i
  else
    let val mid = i + (j - i) div 2
        val doPar = j - i > grain
    in par' doPar (fn _ => parfor grain (i, mid) f,
                   fn _ => parfor grain (mid, j) f);
       ()
    end

fun tabulateBadGran f n =
  let
    val a = allocate n
  in
    parfor 4096 (0, n) (fn i => set a (i, f i));
    a
  end

end
