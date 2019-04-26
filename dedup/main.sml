structure CLA = CommandLineArgs
structure Seq = ArraySequence

val sizeStr = CLA.parseString "N" "1B"
val keysStr = CLA.parseString "K" "1M"
val doCheck = CLA.parseBool "check" false
val funcName = CLA.parseString "func" "dedup"
val runs = CLA.parseInt "runs" 3
val reportGC = CLA.parseBool "reportgc" false

fun parseSizeString X =
  case Int.fromString X of
    NONE => Util.die ("error parsing " ^ X)
  | SOME x =>
      case String.sub (X, String.size X - 1) of
        #"K" => x * 1000
      | #"M" => x * 1000 * 1000
      | #"B" => x * 1000 * 1000 * 1000
      | _ => x

val N = parseSizeString sizeStr
val K = parseSizeString keysStr

val _ = print ("N      " ^ sizeStr ^ " (" ^ Int.toString N ^ ")\n")
val _ = print ("K      " ^ keysStr ^ " (" ^ Int.toString K ^ ")\n")
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

fun doit f =
  let
    val (rs, tms) = Util.timeMany reportGC runs f
    val avgTm = Util.realAvg tms
    val _ = print ("avg    " ^ Util.realToString 3 avgTm ^ "s\n")
  in
    List.hd rs
  end

val input = Seq.tabulate (fn i => Util.hash i mod K) N

val result =
  case funcName of
    "dedup" => doit (fn _ => dedup input)
  | "dedup-incorrect" => doit (fn _ => dedupIncorrect input)
  | other => Util.die ("unknown function name " ^ other)

val _ = print ("len    " ^ Int.toString (Seq.length result) ^ "\n")
val _ = if Seq.length result <= 50 then print (Seq.toString Int.toString result ^ "\n") else ()
