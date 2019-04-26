local
open A
val par = Primitives.par
in

fun parforElision (i, j) f =
  if      j - i = 0 then ()
  else if j - i = 1 then f i
  else
    let val mid = i + (j - i) div 2
    in (parforElision (i, mid) f;
        parforElision (mid, j) f)
    end

fun parfor grain (i, j) f =
  if j - i <= grain then
    parforElision (i, j) f
  else
    let val mid = i + (j - i) div 2
    in par (fn _ => parfor grain (i, mid) f,
            fn _ => parfor grain (mid, j) f);
       ()
    end

fun tabulateGranElision f n =
  let
    val a = allocate n
  in
    parfor 4096 (0, n) (fn i => set a (i, f i));
    a
  end

end
