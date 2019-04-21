fun drseuss n =
  Primitives.parfor 1 (0, n) (fn _ =>
    (print "i am sam, ";
     print "sam i am. ";
     print "do you like ";
     print "green eggs and ham?\n")
  )

structure CLA = CommandLineArgs
val N = CLA.parseInt "N" 10000
val (result, elapsed) = Util.timeOnce (fn _ => drseuss N)
