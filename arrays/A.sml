structure A : PAR_ARRAY =
struct
  type 'a t = 'a array

  val allocate = Primitives.alloc

  val length = Array.length

  val fromList = Array.fromList

  fun get a i = Array.sub (a, i)
  fun set a (i, x) = Array.update (a, i, x)

  fun tabulate f n =
    let
      val a = allocate n
    in
      Primitives.parfor 8192 (0, n) (fn i => set a (i, f i));
      a
    end

  fun map f a =
    tabulate (f o get a) (length a)

  fun iterate f b a =
    let
      val n = length a
      fun loop b i =
        if i = n then b
        else loop (f (b, get a i)) (i+1)
    in
      loop b 0
    end

  fun reduce f b a =
    let
      fun loop b lo hi =
        if lo = hi then b
        else loop (f (b, get a lo)) (lo+1) hi

      fun red lo hi =
        if hi - lo <= 8192 then
          loop b lo hi
        else
          let
            val mid = lo + (hi-lo) div 2
          in
            f (Primitives.par (fn _ => red lo mid, fn _ => red mid hi))
          end
    in
      red 0 (length a)
    end

  fun equal f (a, b) =
    (length a = length b) andalso
    reduce (fn (x, y) => x andalso y) true
    (tabulate (fn i => f (get a i, get b i)) (length a))

  fun seqScan f b a =
    let
      val n = length a
      val r = allocate n
      fun loop b i =
        if i = n then b
        else (set r (i, b); loop (f (b, get a i)) (i+1))
    in
      (r, loop b 0)
    end

  val scanGrain = 4096

  fun scan f b a =
    if length a <= scanGrain then
      seqScan f b a
    else
      let
        val n = length a
        val nb = 1 + (n-1) div scanGrain

        val a' = allocate nb
        val _ = Primitives.parfor 1 (0, nb) (fn i =>
          let
            val lo = i*scanGrain
            val hi = Int.min (lo + scanGrain, n)
            fun loop b j =
              if j = hi then b
              else loop (f (b, get a j)) (j+1)
          in
            set a' (i, loop b lo)
          end)

        val (p, t) = scan f b a'

        val r = allocate n
        val _ = Primitives.parfor 1 (0, nb) (fn i =>
          let
            val lo = i*scanGrain
            val hi = Int.min (lo + scanGrain, n)
            fun loop b j =
              if j = hi then ()
              else (set r (j, b); loop (f (b, get a j)) (j+1))
          in
            loop (get p i) lo
          end)
      in
        (r, t)
      end

  val filterGrain = 4096

  fun filter p a =
    let
      val n = length a
      val nb = 1 + (n-1) div filterGrain

      val counts = allocate nb
      val _ = Primitives.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * filterGrain
          val hi = Int.min (lo + filterGrain, n)
          fun loop c j =
            if j = hi then c
            else loop (if p (get a j) then c+1 else c) (j+1)
        in
          set counts (i, loop 0 lo)
        end)

      val (offsets, count) = scan op+ 0 counts
      val result = allocate count

      val _ = Primitives.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * filterGrain
          val hi = Int.min (lo + filterGrain, n)
          fun loop off j =
            if j = hi then () else
            let
              val x = get a j
            in
              if p x then
                (set result (off, x); loop (off+1) (j+1))
              else
                loop off (j+1)
            end
        in
          loop (get offsets i) lo
        end)
    in
      result
    end

end
