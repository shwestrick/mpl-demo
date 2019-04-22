structure CLA = CommandLineArgs

val sizeStr = CLA.parseString "N" "1B"
val funcName = CLA.parseString "func" "tabulate"
val doCheck = CLA.parseBool "check" false

val N =
  case Int.fromString sizeStr of
    NONE => Util.die ("error parsing N")
  | SOME x =>
      case String.sub (sizeStr, String.size sizeStr - 1) of
        #"K" => x * 1000
      | #"M" => x * 1000 * 1000
      | #"B" => x * 1000 * 1000 * 1000
      | _ => x

val _ = print ("N      " ^ sizeStr ^ " (" ^ Int.toString N ^ ")\n")
val _ = print ("func   " ^ funcName ^ "\n")

fun check f =
  if not doCheck then
    ()
  else
    (print "check  "; TextIO.flushOut TextIO.stdOut;
     if (f ()) then
       print ("correct\n")
     else
       print ("incorrect\n"))

fun ptm tm =
  print ("time   " ^ Util.realToString 3 tm ^ "s\n")

val _ =
  case funcName of
    "tabulate" =>
      let
        fun elem i = i
        val (r, tm) = Util.timeOnce (fn _ => tabulate elem N)
      in
        ptm tm;
        check (fn _ => A.equal op= (r, A.tabulate elem N))
      end

  | "ref-tabulate" =>
      let
        fun elem i = i
        val (r, tm) = Util.timeOnce (fn _ => A.tabulate elem N)
      in
        ptm tm;
        check (fn _ => true) (* :) *)
      end

  | "scan" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val ((r, t), tm) = Util.timeOnce (fn _ => scan op+ 0 input)
      in
        ptm tm;
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "ref-scan" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val ((r, t), tm) = Util.timeOnce (fn _ => A.scan op+ 0 input)
      in
        ptm tm;
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "filter" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val (r, tm) = Util.timeOnce (fn _ => filter keep input)
      in
        ptm tm;
        check (fn _ => A.equal op= (r, A.filter keep input))
      end

  | "ref-filter" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val (r, tm) = Util.timeOnce (fn _ => A.filter keep input)
      in
        ptm tm;
        check (fn _ => true) (* :) *)
      end

  | other => Util.die ("unknown func name " ^ other)
