structure Seq = ArraySequence

fun reduce grain f b s =
  let
    fun red s =
      if Seq.length s <= grain then
        Seq.iterate f b s
      else
        let
          val n = Seq.length s
          val half = n div 2
          val l = Seq.take s half
          val r = Seq.drop s half
          val (x, y) = Primitives.par (fn _ => red l, fn _ => red r)
        in
          f (x, y)
        end
  in
    red s
  end
