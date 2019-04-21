fun drseuss n =
  let
    val m = Mutex.new ()
  in
    Primitives.parfor 1 (0, n) (fn _ =>
      (Mutex.lock m;
       print "i am sam, ";
       print "sam i am. ";
       print "do you like ";
       print "green eggs and ham?\n";
       Mutex.unlock m))
  end

structure CLA = CommandLineArgs
val N = CLA.parseInt "N" 10000
val (result, elapsed) = Util.timeOnce (fn _ => drseuss N)
