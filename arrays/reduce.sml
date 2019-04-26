local

open A
val par = Primitives.par
val loop = Primitives.loop

val reduceGrain = 16384

in

fun reduce f b a =
  let
    fun red i j =
      if j - i <= reduceGrain then
        loop (i, j) b (fn (c, k) => f (c, get a k))
      else
        let val mid = i + (j - i) div 2
        in f (par (fn _ => red i mid,
                   fn _ => red mid j))
        end
  in
    red 0 (length a)
  end

end
