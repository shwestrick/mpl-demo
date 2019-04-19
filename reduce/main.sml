structure CLA = CommandLineArgs

val N = CLA.parseInt "N" 1000000000
val GRAIN = 8192

val _ = print ("N      " ^ Int.toString N ^ "\n")

val input = Seq.tabulate (fn i => Util.hash i mod 10) N

val (result, time) = Util.timeOnce (fn _ => reduce GRAIN op+ 0 input)
val _ = print ("result " ^ Int.toString result ^ "\n")
val _ = print ("time   " ^ Util.realToString 3 time ^ "s\n")
