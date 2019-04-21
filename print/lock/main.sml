type lock = int ref

fun newLock () = ref 0

fun takeLock r =
  if (Primitives.compareAndSwap r (0, 1) = 0)
  then ()
  else takeLock r

fun releaseLock r =
  (r := 0)

fun drseuss n =
  let
    val m = newLock ()
  in
    Primitives.parfor 1 (0, n) (fn _ =>
      (takeLock m;
       print "i am sam, ";
       print "sam i am. ";
       print "do you like ";
       print "green eggs and ham?\n";
       releaseLock m))
  end

structure CLA = CommandLineArgs
val N = CLA.parseInt "N" 10000
val (result, elapsed) = Util.timeOnce (fn _ => drseuss N)
