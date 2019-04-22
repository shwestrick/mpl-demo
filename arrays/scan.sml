fun scan f b a =
  case A.length a of
    0 => (A.fromList [], b)
  | 1 => (A.fromList [b], A.get a 0)
  | n =>
      let
        val nb = 1 + (n-1) div 2

        fun contract i =
          if 2*i <> n-1 then
            f (A.get a (2*i), A.get a (2*i+1))
          else
            A.get a (2*i)

        val a' = tabulate contract nb

        val (p, t) = scan f b a'

        fun expand i =
          if i mod 2 = 0 then
            A.get p (i div 2)
          else
            f (A.get p (i div 2), A.get a (i-1))

        val p' = tabulate expand n
      in
        (p', t)
      end
