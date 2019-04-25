structure Util :>
sig
  exception NotYetImplemented of string

  val hashWord : Word64.word -> Word64.word
  val hash : int -> int

  val wordToBinaryString : Word64.word -> string

  val swap : 'a Array.array * int * int -> unit

  (* `shuffle xs r` uses `r` as a random seed to randomly shuffle `xs` via the
   * Knuth (Fisher-Yates) shuffle. *)
  val shuffle : 'a Array.array -> int -> unit

  val pow2 : int -> int
  val log2 : int -> int

  val isOdd : int -> bool
  val isEven : int -> bool

  (* `boundPow2 n` returns the smallest power of 2 `b` such that `n <= b` *)
  val boundPow2 : int -> int

  (* `boundLog2 n` returns the smallest `b` such that `n <= 2^b` *)
  val boundLog2 : int -> int

  type timer = Time.time
  val startTiming : unit -> timer
  val tick : timer -> string -> timer

  val timeOnce : (unit -> 'a) -> 'a * real
  val timeOnceWithPercentGC : (unit -> 'a) -> 'a * real * real
  val timeMany : bool -> int -> (unit -> 'a) -> 'a list * real list

  val realSum : real list -> real
  val realAvg : real list -> real
  val realMin : real list -> real
  val realMax : real list -> real

  val realToString : int -> real -> string
  val listToString : ('a -> string) -> 'a list -> string

  (* sizes, in bytes *)
  val sizeOfInt : int
  val sizeOfReal : int

  val exit : string -> 'a (* Terminate successfully with a message *)
  val die : string -> 'a  (* Terminate unsuccessfully with a message *)

  val medianOf3 : ('a * 'a -> order) -> ('a * 'a * 'a) -> 'a

  val readFile : string -> char Array.array

  type padder = int -> string -> string
  val rightpad : padder
  val leftpad  : padder

  val formatTable : {outerSep : string, innerSep : string, padder : padder}
                 -> string list list
                 -> string
end =
struct

  exception NotYetImplemented of string
  val _ = MLton.Exn.addExnMessager
    (fn (NotYetImplemented s) => SOME ("NotYetImplemented: " ^ s) | _ => NONE)

  type timer = Time.time
  fun startTiming () = Time.now ()
  fun tick t msg =
    let
      val t' = Time.now ()
      val elapsed = Time.toMilliseconds (Time.- (t', t))
      val _ = print ("+" ^ LargeInt.toString elapsed ^ ": " ^ msg ^ "\n")
    in
      t'
    end

  fun searchPow2 n m = if m >= n then m else searchPow2 n (2*m)
  fun boundPow2 n = searchPow2 n 1

  fun searchLog2 n m i = if m >= n then i else searchLog2 n (2*m) (i+1)
  fun boundLog2 n = searchLog2 n 1 0

  fun pow2 i = if (i<1) then 1 else 2*pow2(i-1)

  (* NOTE: this actually computes 1 + floor(log_2(n)), i.e. the number of
   * bits required to represent n in binary *)
  fun log2 n = if (n < 1) then 0 else 1 + log2(n div 2)

  val sizeOfInt =
    case Int.precision of
      NONE => 0
    | SOME p => (boundPow2 p) div 8

  val sizeOfReal =
    if Real.radix = 2 then (boundPow2 Real.precision) div 8
    else ( print "Warning: guessing Util.sizeOfReal = 8\n"
         ; 8
         )

  fun exit msg =
    ( print (msg ^ "\n")
    ; OS.Process.exit OS.Process.success
    )

  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; OS.Process.exit OS.Process.failure
    )

  fun realToString decDigits r =
    Real.fmt (StringCvt.FIX (SOME decDigits)) r

  fun timeOnceWithPercentGC f =
    let
      val t0 = Time.now ()
      val gc0 = Primitives.getGCTime ()
      val result = f ()
      val gc1 = Primitives.getGCTime ()
      val t1 = Time.now ()

      val elapsed = Time.toReal (Time.- (t1, t0))
      val nproc = Vector.length gc0
      val gc = Vector.tabulate (nproc, fn p =>
        Time.toReal (Time.- (Vector.sub (gc1, p), Vector.sub (gc0, p))))
      val gcAvg = (Vector.foldl op+ 0.0 gc) / (Real.fromInt nproc)
      val gcPercent = 100.0 * gcAvg / elapsed
    in
      (result, elapsed, gcPercent)
    end

  fun timeOnce f =
    let
      val t0 = Time.now ()
      val result = f ()
      val t1 = Time.now ()
    in
      (result, Time.toReal (Time.- (t1, t0)))
    end

  fun timeMany reportGC k f =
    Primitives.loop (0, k) ([], []) (fn ((results, times), i) =>
      let
        val (result, elapsed, pgc) = timeOnceWithPercentGC f
        val _ = print ("time" ^ Int.toString i ^ "  " ^ realToString 3 elapsed ^ "s")
        val _ = print (if not reportGC then "\n" else " (" ^ realToString 0 pgc ^ "% gc)\n")
      in
        (result :: results, elapsed :: times)
      end)

  fun realSum rs = List.foldr op+ 0.0 rs
  fun realAvg rs = (realSum rs) / Real.fromInt (List.length rs)
  fun realMin rs = List.foldr Real.min Real.posInf rs
  fun realMax rs = List.foldr Real.max Real.negInf rs

  fun listToString f xs =
    "[" ^ String.concatWith "," (List.map f xs) ^ "]"

  fun hashWord w =
    let
      open Word64
      infix 2 >> infix 2 << infix 2 xorb infix 2 andb
      val v = w * 0w3935559000370003845 + 0w2691343689449507681
      val v = v xorb (v >> 0w21)
      val v = v xorb (v << 0w37)
      val v = v xorb (v >> 0w4)
      val v = v * 0w4768777513237032717
      val v = v xorb (v << 0w20)
      val v = v xorb (v >> 0w41)
      val v = v xorb (v << 0w5)
    in
      v
    end

  val intMask =
    case Int.precision of
      NONE => raise Fail "Util: Int.precision = NONE"
    | SOME p =>
        let
          val p' = p - 1
          open Word64
          infix 2 <<
        in
          (0w1 << Word.fromInt p') - 0w1
        end

  fun hash i =
    let
      open Word64
      infix 2 >> infix 2 << infix 2 xorb infix 2 andb
      val v = hashWord (Word64.fromInt i)
    in
      Word64.toInt (v andb intMask)
    end

  fun isEven x =
    Word.andb (0w1, Word.fromInt x) = 0w0
  fun isOdd x =
    Word.andb (0w1, Word.fromInt x) = 0w1

  fun charbit w i =
    if Word64.andb (Word64.>> (w, Word.fromInt i), 0w1) = 0w0 then #"0" else #"1"
  (* NOTE: this actually computes 1 + floor(log_2(w)), i.e. the number of
   * bits required to represent w in binary *)
  fun wordlog2 w =
    if Word64.< (w, 0w1) then 0
    else 1 + wordlog2 (Word64.>> (w, 0w1))
  fun wordToBinaryString w =
    let
      val len = wordlog2 w
    in
      CharVector.tabulate (len, charbit w)
    end

  fun swap (xs, i, j) =
    let
      val xi = Array.sub (xs, i)
      val xj = Array.sub (xs, j)
    in
      ( Array.update (xs, i, xj)
      ; Array.update (xs, j, xi)
      )
    end

  fun shuffle xs seed =
    let
      val n = Array.length xs
      fun loop i =
        if i >= n - 1 then ()
        else ( swap (xs, i, i + (hash (seed + i) mod (n - i)))
             ; loop (i+1)
             )
    in
      loop 0
    end

  fun medianOf3 cmp (x, y, z) =
    let
      val (a, b) =
        if cmp (x, y) <> GREATER
        then (x, y)
        else (y, x)
    in
      if cmp (b, z) <> GREATER
      then b
      else if cmp (a, z) <> GREATER
           then z
           else a
    end

  type padder = int -> string -> string

  fun rightpad n s =
    if String.size s >= n then s
    else s ^ String.implode (List.tabulate (n - String.size s, fn _ => #" "))

  fun leftpad n s =
    if String.size s >= n then s
    else String.implode (List.tabulate (n - String.size s, fn _ => #" ")) ^ s

  fun formatTable {outerSep, innerSep, padder} rows =
    let
      fun max vec = Vector.foldl Int.max 0 vec

      val rows = Vector.fromList (List.map Vector.fromList rows)
      fun getrc i j = (Vector.sub (Vector.sub (rows, i), j) handle Subscript => "")
      fun getcr j i = getrc i j
      val numRows = Vector.length rows
      val numCols = max (Vector.map Vector.length rows)

      fun column j = Vector.tabulate (numRows, getcr j)
      fun colWidth j = max (Vector.map String.size (column j))
      val widths = Vector.tabulate (numCols, colWidth)
      fun width j = Vector.sub (widths, j)

      fun padrow i =
        List.tabulate (numCols, fn j => padder (width j) (getrc i j))

      val padded = List.tabulate (numRows, padrow)
    in
      String.concatWith outerSep (List.map (String.concatWith innerSep) padded)
    end

  structure P = Primitives
  structure A = Array
  structure AS = ArraySlice

  fun readFile filename =
    let
      val inStream = TextIO.openIn filename
	    fun loop () =
	      let
          val str = TextIO.inputN (inStream, 100000)
	      in
          if String.size str = 0 then []
	        else str :: loop ()
	      end
	    val blocks = (A.fromList (loop ()))
      (* val _ = TextIO.closeIn inStream *)
      val nb = A.length blocks
      fun blockLen i = Vector.length (A.sub (blocks, i))
	    val offsets = MapScan.scan op+ 0 (nb, blockLen)
      val total = A.sub (offsets, nb)
	    val r = P.alloc total
      fun writeBlock i =
        let
         	val off = A.sub (offsets, i)
          val block = A.sub (blocks, i)
		    in
          Vector.appi (fn (j, x) => A.update (r, off + j, x)) block
        end
      val _ = Primitives.parfor 1 (0, nb) writeBlock
    in
      r
    end

end
