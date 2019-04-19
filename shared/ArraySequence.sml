structure ArraySequence
  (* : SEQUENCE where type 'a t = 'a ArraySlice.slice *) =
struct

  structure A = Array
  structure AS = ArraySlice
  structure P = Primitives

  val GRAIN = 4096

  val alloc = Primitives.alloc
  val update = Unsafe.Array.update
  val updateUp = P.arrayUpdateUp
  val sub = Unsafe.Array.sub

  fun ASupdate (s, i, x) =
    let val (a, start, _) = AS.base s
    in update (a, start+i, x)
    end

  fun ASsub (s, i) =
    let val (a, start, _) = AS.base s
    in sub (a, start+i)
    end

  type 'a t = 'a AS.slice
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  fun length x = AS.length x
  fun empty _ = AS.full (A.fromList [])
  fun singleton x = AS.full (A.array (1, x))
  val $ = singleton

  fun tabulate' g n f =
    if n < 0 then raise Size else
    let
      val a = P.alloc n
    in
      P.parfor g (0, n) (fn i => update (a, i, f i));
      AS.full a
    end

  fun tabulate f n = tabulate' GRAIN n f

  fun app f s =
    let
      val (a, start, n) = AS.base s
    in
      P.parfor GRAIN (start, start+n) (fn i => f(sub(a, i)))
    end

  fun nth s i = ASsub (s, i)
  fun first s = nth s 0
  fun last s = nth s (length  s - 1)

  fun toString f s =
    "<" ^ String.concatWith "," (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l = AS.full (A.fromList l)

  val % = fromList

  fun subseq s (i, len) = AS.subslice (s, i, SOME len)

  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)

  fun splitHead s =
    if length s = 0 then NIL else CONS (nth s 0, drop s 1)

  fun splitMid s =
    case length s of
      0 => EMPTY
    | 1 => ONE (nth s 0)
    | n => PAIR (take s (n div 2), drop s (n div 2))

  fun rev s =
    tabulate (fn i => nth s (length s - 1 - i)) (length s)

  fun append (s, t) =
    let val (ns, nt) = (length s, length t)
        fun ith i = if i < ns then nth s i else nth t (i - ns)
    in tabulate ith (ns + nt)
    end

  fun append3 (a, b, c) =
    let val (na, nb, nc) = (length a, length b, length c)
        fun ith i =
          if i < na      then nth a i else
          if i < na + nb then nth b (i - na)
          else                nth c (i - na - nb)
    in tabulate ith (na + nb + nc)
    end

  fun iterate f b s =
    let
      val (a, lo, n) = AS.base s
      val hi = lo + n
      fun iter b i = if i = hi then b else iter (f (b, sub (a, i))) (i+1)
    in
      iter b lo
    end

  (* useful for things like toList, below *)

  fun writeIteratePrefixes result f b s =
    let
      val n = length s
      fun iter cur i =
        if i = n then cur
        else (ASupdate (result, i, cur); iter (f (cur, nth s i)) (i+1))
    in iter b 0
    end

  fun iteratePrefixes f b s =
    let val result = AS.full (alloc (length s))
    in (result, writeIteratePrefixes result f b s)
    end

  fun writeIteratePrefixesIncl result f b s =
    case length s of
      0 => ()
    | n =>
        let
          fun iter cur i =
            (ASupdate (result, i, cur);
             if i = n-1 then () else iter (f (cur, nth s (i+1))) (i+1))
        in iter (f (b, nth s 0)) 0
        end

  fun iteratePrefixesIncl f b s =
    let val result = AS.full (alloc (length s))
    in writeIteratePrefixesIncl result f b s;
       result
    end

  fun revIterate f b s =
    let
      fun iter x n =
        if n = 0 then x else iter (f (nth s (n-1), x)) (n-1)
    in
      iter b (length s)
    end
  fun toList s = revIterate op:: [] s

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)

  fun map f s =
    let
      val (ss, start, n) = AS.base s
    in
      if start = 0 then
        tabulate (fn i => f (sub (ss, i))) n
      else
        tabulate (fn i => f (sub (ss, start+i))) n
    end

  fun mapIdx f s =
    tabulate (fn i => f (i, nth s i)) (length s)

  fun zipWith f (s, t) =
    tabulate
      (fn i => f (nth s i, nth t i))
      (Int.min (length s, length t))

  fun zip (s, t) = zipWith (fn x => x) (s, t)

  fun scan (f : 'a * 'a -> 'a) (b : 'a) (s : 'a seq) : 'a seq * 'a =
    if length s <= GRAIN then
      iteratePrefixes f b s
    else
      let
        val k = GRAIN (* block size *)
        val n = length s
        val m = 1 + ((n - 1) div k) (* number of blocks *)

        val sums = tabulate' 1 m (fn i =>
          iterate f b (subseq s (i * k, Int.min (k, n - i * k))))
        val (partials, final) = scan f b sums

        val result = AS.full (alloc n)
      in
        P.parfor 1 (0, m) (fn i =>
          (writeIteratePrefixes
            (subseq result (i * k, Int.min (k, n - i * k)))
            f
            (nth partials i)
            (subseq s (i * k, Int.min (k, n - i * k))); ()));
        (result, final)
      end

  structure Fusion =
  struct

    fun writeSerialMapMerge result cmp (f, g) (s, t) =
      let
        val (sn, tn) = (length s, length t)
        val n = sn + tn
      in
        P.loop (0, n) (0,0) (fn ((i,j), k) =>
          if j = tn then (AS.update (result, k, f (nth s i)); (i+1, j)) else
          if i = sn then (AS.update (result, k, g (nth t j)); (i, j+1)) else
          let val (x, y) = (nth s i, nth t j)
          in if cmp (x, y) <> GREATER
             then (AS.update (result, k, f x); (i+1, j))
             else (AS.update (result, k, g y); (i, j+1))
          end);
        ()
      end

    (* Returns number of elements of s which are strictly less than x *)
    fun lowRank cmp x s =
      let
        fun search (lo, hi) =
          case hi - lo of
            0 => lo
          | n =>
              let val half = n div 2
                  val mid = lo + half
              in case cmp (x, nth s mid) of
                   LESS => search (lo, mid)
                 | GREATER => search (mid + 1, hi)
                 | EQUAL =>
                     if (mid = 0) orelse (cmp (x, nth s (mid-1)) = GREATER)
                     then mid
                     else search (lo, mid)
              end
      in
        search (0, length s)
      end

    (* Returns number of elements of s which are <= x *)
    fun highRank cmp x s =
      let
        val m = length s
        fun search (lo, hi) =
          case hi - lo of
            0 => lo
          | n =>
              let val half = n div 2
                  val mid = lo + half
              in case cmp (x, nth s mid) of
                   LESS => search (lo, mid)
                 | GREATER => search (mid + 1, hi)
                 | EQUAL =>
                     if (mid = m-1) orelse (cmp (x, nth s (mid+1)) = LESS)
                     then mid+1
                     else search (mid+1, hi)
              end
      in
        search (0, m)
      end

    fun rankRange cmp x s = (lowRank cmp x s, highRank cmp x s)

    fun split s (i, j) =
      (subseq s (0, i), subseq s (i, j-i), subseq s (j, length s - j))

    fun writeMapMerge r cmp (f, g) (s, t) =
      case (length s, length t) of
        (n, 0) =>
          P.parfor GRAIN (0, n) (fn i => AS.update (r, i, f (nth s i)))

      | (0, m) =>
          P.parfor GRAIN (0, m) (fn i => AS.update (r, i, g (nth t i)))

      | (1, 1) =>
          let val (x, y) = (nth s 0, nth t 0)
              val (min, max) = if cmp (x, y) <> GREATER then (f x, g y) else (g y, f x)
          in AS.update (r, 0, min); AS.update (r, 1, max)
          end

      | (n, m) =>
          if n + m <= GRAIN then writeSerialMapMerge r cmp (f, g) (s, t) else
          let val pivot = nth s (n div 2)
              val (smid1, smid2) = rankRange cmp pivot s
              val (tmid1, tmid2) = rankRange cmp pivot t
              val (sl, se, sg) = split s (smid1, smid2)
              val (tl, te, tg) = split t (tmid1, tmid2)
              val (rl, re, rg) = split r (smid1 + tmid1, smid2 + tmid2)
          in Primitives.par4 (fn () => writeMapMerge rl cmp (f, g) (sl, tl),
                              fn () => writeMapMerge rg cmp (f, g) (sg, tg),
                              fn () => P.parfor GRAIN (0, length se) (fn i => AS.update (re, i, f (nth se i))),
                              fn () => P.parfor GRAIN (0, length te) (fn i => AS.update (re, length se + i, g (nth te i))));
             ()
          end

    (* (mapMerge cmp (f, g) (s, t)) merges (map f s) with (map f t), except
     * that it compares elements according to their values before the map. *)
    fun mapMerge (cmp : 'a ord) (f : 'a -> 'b, g : 'a -> 'b) (s : 'a seq, t : 'a seq) : 'b seq =
      let val result = AS.full (alloc (length s + length t))
      in writeMapMerge result cmp (f, g) (s, t);
         result
      end

  end (* ArraySequence.Fusion *)

  (*fun reduce f b s = Fusion.mapReduce (fn x => x) f b s*)

  fun reduce f b s =
    if length s <= GRAIN then
      iterate f b s
    else
      let
        val half = length s div 2
        val (l, r) = (take s half, drop s half)
      in
        f (P.par (fn _ => reduce f b l, fn _ => reduce f b r))
      end

  (* TODO: direct implementation will be faster. *)
  fun scanIncl f b s =
    let val (prefixes, final) = scan f b s
    in drop (append (prefixes, singleton final)) 1
    end

  fun merge cmp (s, t) = Fusion.mapMerge cmp (fn x => x, fn x => x) (s, t)

  (* TODO: can we make it faster? can we use techniques from filter, below,
   * i.e. explicit blocks? difficulty is that the elements of ss do not have
   * bounded size. one idea would be to allow blocks to cross the boundaries
   * of the nested sequences, although it is likely too costly to compute the
   * block boundaries. *)
  fun flatten ss =
    let
      val (offsets, n) = scan op+ 0 (map (length) ss)
      val result = alloc n
    in
      P.parfor GRAIN (0, length ss) (fn i =>
        let
          val offset = nth offsets i
          val elems = nth ss i
        in
          P.parfor GRAIN (0, length elems) (fn j =>
            A.update (result, offset + j, nth elems j))
        end);
      AS.full result
    end

  (* version that does not save predicate -- saving would be OK if done into boolean array *)
  fun filterIdx p s =
    let
      val n = length s

      val bsize = GRAIN
      val nb = 1 + (n-1) div bsize (* number of blocks *)
      val bcounts = tabulate' 1 nb (fn i =>
        let val lo = i * bsize
            val hi = Int.min ((i+1) * bsize, n)
            val block = subseq s (lo, hi-lo)
        in #1 (iterate (fn ((off,j), x) => if p(j,x) then (off+1,j+1) else (off,j+1))
                            (0,lo) block)
        end)

      val (offsets, m) = scan op+ 0 bcounts
      val result = alloc m
    in
      P.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * bsize
          val hi = Int.min ((i+1) * bsize, n)
          val block = subseq s (lo, hi-lo)
          fun update ((offset, j), x) =
            if p(j,x) (* nth ps j *)
            then (A.update (result, offset, x); (offset+1, j+1))
            else (offset, j+1)
        in
          iterate update (nth offsets i, lo) block; ()
        end);
      AS.full result
    end

  fun filter p s =
    let
      val n = length s

      val bsize = GRAIN
      val nb = 1 + (n-1) div bsize (* number of blocks *)
      val bcounts = tabulate' 1 nb (fn i =>
        let val lo = i * bsize
            val hi = Int.min ((i+1) * bsize, n)
            val block = subseq s (lo, hi-lo)
        in iterate (fn (off, x) => if p x then off+1 else off) 0 block
        end)

      val (offsets, m) = scan op+ 0 bcounts
      val result = alloc m
    in
      P.parfor 1 (0, nb) (fn i =>
        let
          val lo = i * bsize
          val hi = Int.min ((i+1) * bsize, n)
          val block = subseq s (lo, hi-lo)
          fun update (offset, x) =
            if p x
            then (updateUp (result, offset, x); offset+1)
            else offset
        in
          iterate update (nth offsets i) block; ()
        end);
      AS.full result
    end

  fun equal  cmp (s1, s2) =
    length s1 = length s2 andalso
    reduce (fn (x,y) => x andalso y) true (zipWith cmp (s1, s2))

  fun inject (s, updates) =
    let
      val result = map (fn x => x) s
    in
      P.parfor GRAIN (0, length updates) (fn i =>
        let val (idx, r) = nth updates i
        in AS.update (result, idx, r) handle Subscript => raise Range
        end);
      result
    end

  (* TODO: direct implementation may be slightly faster. Although, this function
   * is pretty much never used for ArraySequences, so perhaps we'll just ignore
   * it. *)
  fun update (s, (i, x)) = inject (s, singleton (i, x))

  fun collate cmp (s1, s2) =
    case (splitHead s1, splitHead s2) of
      (NIL, NIL) => EQUAL
    | (NIL, _  ) => LESS
    | (_  , NIL) => GREATER
    | (CONS (x, xs), CONS (y, ys)) =>
        case cmp (x, y) of
          EQUAL => collate cmp (xs, ys)
        | ord => ord

  fun casArraySequence (s, i) (old, new) =
    let
      val (a, start, len) = ArraySlice.base s
    in
      Primitives.arrayCompareAndSwap (a, start+i) (old, new)
    end

end
