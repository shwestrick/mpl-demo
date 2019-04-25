structure CLA = CommandLineArgs

val sizeStr = CLA.parseString "N" "1B"
val funcName = CLA.parseString "func" "tabulate"
val doCheck = CLA.parseBool "check" false
val runs = CLA.parseInt "runs" 3
val reportGC = CLA.parseBool "reportgc" false

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

(* fun ptm tm =
  print ("time   " ^ Util.realToString 3 tm ^ "s\n") *)

fun doit f =
  let
    val (rs, tms) = Util.timeMany reportGC runs f
  in
    List.hd rs
  end

val _ =
  case funcName of
    "tabulate" =>
      let
        fun elem i = i
        val r = doit (fn _ => tabulate elem N)
      in
        check (fn _ => A.equal op= (r, A.tabulate elem N))
      end

  | "tabulate-no-gran" =>
      let
        fun elem i = i
        val r = doit (fn _ => tabulateNoGran elem N)
      in
        check (fn _ => A.equal op= (r, A.tabulate elem N))
      end

  | "scan" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val (r, t) = doit (fn _ => scan op+ 0 input)
      in
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "filter-up-down-no-gran" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val r = doit (fn _ => filterUpDownNoGran keep input)
      in
        check (fn _ => A.equal op= (r, A.filter keep input)) (* :) *)
      end

  | "filter-up-down" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val r = doit (fn _ => filterUpDown keep input)
      in
        check (fn _ => A.equal op= (r, A.filter keep input)) (* :) *)
      end

  | "filter-contract" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val r = doit (fn _ => filterContract keep input)
      in
        check (fn _ => A.equal op= (r, A.filter keep input)) (* :) *)
      end

  | other => Util.die ("unknown func name " ^ other)
