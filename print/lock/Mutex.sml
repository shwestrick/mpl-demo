structure Mutex :>
sig
  type t
  val new : unit -> t
  val lock : t -> unit
  val unlock : t -> unit
end =
struct
  type t = int ref

  fun new () = ref 0

  fun lock r =
    if (Primitives.compareAndSwap r (0, 1) = 0)
    then ()
    else lock r

  fun unlock r =
    r := 0
end

