local
open A
val par = Primitives.par
in

fun parfor (i, j) f =
  if      j - i = 0 then ()
  else if j - i = 1 then f i
  else
    let val mid = i + (j - i) div 2
    in par (fn _ => parfor (i, mid) f,
            fn _ => parfor (mid, j) f);
       ()
    end

fun tabulateNoGran f n =
  let
    val a = allocate n
  in
    parfor (0, n) (fn i => set a (i, f i));
    a
  end

end
