fun tabulate f n =
  let
    val a = A.allocate n
  in
    Primitives.parfor 8192 (0, n) (fn i => A.set a (i, f i));
    a
  end
