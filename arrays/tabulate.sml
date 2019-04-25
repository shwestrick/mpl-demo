local
open A
val par = Primitives.par
in

fun for (i, j) f =
  if i = j then ()
  else (f i; for (i+1, j) f)

fun parfor grain (i, j) f =
  if j - i <= grain then
    for (i, j) f
  else
    let val mid = i + (j - i) div 2
    in par (fn _ => parfor grain (i, mid) f,
            fn _ => parfor grain (mid, j) f);
       ()
    end

fun tabulate f n =
  let
    val a = allocate n
  in
    parfor 4096 (0, n) (fn i => set a (i, f i));
    a
  end

end
