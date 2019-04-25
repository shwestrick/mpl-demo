local

open A

in

fun scanContractNoGran f b a =
  case length a of
    0 => (fromList [], b)
  | 1 => (fromList [b], get a 0)
  | n =>
      let
        val nb = 1 + (n-1) div 2

        fun contract i =
          if 2*i <> n-1 then
            f (get a (2*i), get a (2*i+1))
          else
            get a (2*i)

        val a' = tabulate contract nb

        val (p, t) = scanContractNoGran f b a'

        fun expand i =
          if i mod 2 = 0 then
            get p (i div 2)
          else
            f (get p (i div 2), get a (i-1))

        val p' = tabulate expand n
      in
        (p', t)
      end

end
