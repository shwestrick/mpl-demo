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
    val avgTm = Util.realAvg tms
    val _ = print ("avg    " ^ Util.realToString 3 avgTm ^ "s\n")
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

  | "tabulate-bad-gran" =>
      let
        fun elem i = i
        val r = doit (fn _ => tabulateBadGran elem N)
      in
        check (fn _ => A.equal op= (r, A.tabulate elem N))
      end

  | "tabulate-gran-elision" =>
      let
        fun elem i = i
        val r = doit (fn _ => tabulateGranElision elem N)
      in
        check (fn _ => A.equal op= (r, A.tabulate elem N))
      end

  | "reduce" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val r = doit (fn _ => reduce op+ 0 input)
      in
        check (fn _ => r = A.reduce op+ 0 input)
      end

  | "scan-contract-no-gran" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val (r, t) = doit (fn _ => scanContractNoGran op+ 0 input)
      in
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "scan-contract" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val (r, t) = doit (fn _ => scanContract op+ 0 input)
      in
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "scan-up-down" =>
      let
        fun elem i = 1
        val input = A.tabulate elem N
        val (r, t) = doit (fn _ => scanUpDown op+ 0 input)
      in
        check (fn _ => t = N andalso A.equal op= (r, A.tabulate (fn i => i) N))
      end

  | "filter-simple" =>
      let
        fun elem i = (Util.hash i mod N)
        fun keep x = Util.isEven x
        val input = A.tabulate elem N
        val r = doit (fn _ => filterSimple keep input)
      in
        check (fn _ => A.equal op= (r, A.filter keep input)) (* :) *)
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

  | "inject" =>
      let
        fun elem i = (Util.hash i mod N, i)
        val updates = A.tabulate elem N
        val input = A.tabulate (fn _ => ~1) N
        val r = doit (fn _ => inject input updates)

        fun checkIdx i =
          let
            val x = A.get r i
          in
            x < 0 orelse (i = Util.hash x mod N)
          end
      in
        check (fn _ => A.reduce (fn (a, b) => a andalso b) true
          (A.tabulate checkIdx N))
      end

  | other => Util.die ("unknown func name " ^ other)
