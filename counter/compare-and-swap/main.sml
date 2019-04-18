fun counter n =
  let
    val r = ref 0

    fun increment () =
      let
        val c = !r
      in
        if c = Primitives.compareAndSwap r (c, c+1) then
          ()
        else
          increment ()
      end
  in
    Primitives.parfor 1000 (0, n) (fn _ =>
      increment ()
    );

    !r
  end

structure CLA = CommandLineArgs
val N = CLA.parseInt "N" 100000000
val _ = print ("N      " ^ Int.toString N ^ "\n")
val (result, elapsed) = Util.timeOnce (fn _ => counter N)
val _ = print ("result " ^ Int.toString result ^ "\n")
val _ = print ("time   " ^ Util.realToString 3 elapsed ^ "s\n")
