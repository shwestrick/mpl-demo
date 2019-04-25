structure Primitives :> PRIMITIVES =
struct

  exception Primitives

  val numberOfProcessors = MLton.Parallel.numberOfProcessors

  val _ = MLton.Rusage.measureGC true

  fun getGCTime () =
    Vector.tabulate (numberOfProcessors, fn p =>
      Time.+ (MLton.GC.Statistics.localGCTimeOfProc p,
              MLton.GC.Statistics.promoTimeOfProc p))

  val par = ForkJoin.fork

  (* ForkJoin.alloc is parallelized. Alternatively we can sequentially
   * initialize an array with
   *   MLton.Parallel.Unsafe.arrayUninit
   *)
  val alloc = ForkJoin.alloc

  val compareAndSwap = MLton.Parallel.compareAndSwap
  val arrayCompareAndSwap = MLton.Parallel.arrayCompareAndSwap
  val fetchAndAdd = MLton.Parallel.fetchAndAdd
  val arrayFetchAndAdd = MLton.Parallel.arrayFetchAndAdd

  fun arrayUpdateUp (a, i, x) =
    if i < 0 orelse i >= Array.length a then
      raise Subscript
    else
      MLton.HM.arrayUpdateNoBarrier (a, Int64.fromInt i, x)

  val refAssignUp = MLton.HM.refAssignNoBarrier

  fun par3 (f, g, h) =
    let
      val ((a, b), c) = par (fn _ => par (f, g), h)
    in
      (a, b, c)
    end

  fun par4 (f, g, h, i) =
    let
      val ((a, b), (c, d)) = par (fn _ => par (f, g), fn _ => par (h, i))
    in
      (a, b, c, d)
    end

  fun for (i, j) f =
    if i = j then () else (f i; for (i+1, j) f)

  fun forBackwards (i, j) f =
    if i = j then () else (f (j-1); forBackwards (i, j-1) f)

  fun loop (lo, hi) b f =
    if (lo >= hi) then b else loop (lo+1, hi) (f (b, lo)) f

  fun parfor grain (i, j) f =
    let
      val n = j - i
    in
      if n <= grain then
        for (i, j) f
      else
        (par (fn _ => parfor grain (i, i + n div 2) f,
              fn _ => parfor grain (i + n div 2, j) f); ())
    end

  val communicate = ForkJoin.communicate

end

