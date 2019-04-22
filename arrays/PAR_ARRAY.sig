signature PAR_ARRAY =
sig
  type 'a t
  val allocate : int -> 'a t
  val fromList : 'a list -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> (int * 'a) -> unit

  val tabulate : (int -> 'a) -> int -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val scan : ('a * 'a -> 'a) -> 'a -> 'a t -> ('a t * 'a)
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a t -> 'a
  val filter : ('a -> bool) -> 'a t -> 'a t

  val equal : ('a * 'a -> bool) -> ('a t * 'a t) -> bool
end
