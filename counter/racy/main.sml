fun counter n =
  let
    val r = ref 0
  in

    Primitives.parfor 1000 (0, n) (fn _ =>
      r := !r + 1
    );

    !r
  end

structure CLA = CommandLineArgs
val N = CLA.parseInt "N" 100000000
val _ = print ("N      " ^ Int.toString N ^ "\n")
val (result, elapsed) = Util.timeOnce (fn _ => counter N)
val _ = print ("result " ^ Int.toString result ^ "\n")
val _ = print ("time   " ^ Util.realToString 3 elapsed ^ "s\n")
