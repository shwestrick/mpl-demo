structure A : PAR_ARRAY =
struct

  structure AS = ArraySlice

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

  datatype 'a rtree = Node of 'a * 'a rtree * 'a rtree | Leaf of 'a
  fun rval (Node (rv, _, _)) = rv
    | rval (Leaf rv) = rv

  (* map f on s and combine with g (identity b) *)
  fun upsweep grain (f : 'a -> 'b) (g : 'b * 'b -> 'b) (b : 'b) (s : 'a AS.slice) =
    if AS.length s <= grain then
      Leaf (AS.foldl (fn (x, b) => g (b, f x)) b s)
    else
      let
        val n = AS.length s
        val half = n div 2
        val (l, r) = Primitives.par
          (fn _ => upsweep grain f g b (AS.subslice (s, 0, SOME half)),
           fn _ => upsweep grain f g b (AS.subslice (s, half, NONE)))
      in
        Node (g (rval l, rval r), l, r)
      end

  val scanGrain = 4096

  fun scan f b a =
    let
      fun upsweep s =
        if AS.length s <= scanGrain then
          let
            val (a, k, n) = AS.base s
          in
            Leaf (Primitives.loop (k, k+n) b (fn (b, i) => f (b, get a i)))
          end
        else
          let
            val n = AS.length s
            val half = n div 2
            val (l, r) = Primitives.par
              (fn _ => upsweep (AS.subslice (s, 0, SOME half)),
               fn _ => upsweep (AS.subslice (s, half, NONE)))
          in
            Node (f (rval l, rval r), l, r)
          end

      val tree = upsweep (AS.full a)

      val total = rval tree
      val result = allocate (length a)

      fun downsweep b t lo hi =
        case t of
          Leaf _ =>
            (Primitives.loop (lo, hi) b (fn (b, i) =>
               (set result (i, b); f (b, get a i)));
             ())
        | Node (_, l, r) =>
            let
              val mid = lo + (hi - lo) div 2
            in
              Primitives.par (fn _ => downsweep b l lo mid,
                              fn _ => downsweep (f (b, rval l)) r mid hi);
              ()
            end

      val _ = downsweep b tree 0 (length a)
    in
      (result, total)
    end


  val filterGrain = 4096

  fun filter p a =
    let
      fun c x = if p x then 1 else 0
      val tree = upsweep filterGrain c op+ 0 (AS.full a)
      val count = rval tree

      val output = allocate count

      fun downsweep offset t lo hi =
        case t of
          Leaf _ =>
            (Primitives.loop (lo, hi) offset (fn (off, i) =>
               let
                 val x = get a i
               in
                 if p x then
                   (set output (off, x); off+1)
                 else
                   off
               end);
             ())
        | Node (_, l, r) =>
            let
              val mid = lo + (hi - lo) div 2
              val offset' = offset + rval l
            in
              Primitives.par (fn _ => downsweep offset l lo mid,
                              fn _ => downsweep offset' r mid hi);
              ()
            end

      val _ = downsweep 0 tree 0 (length a)
    in
      output
    end



end
