type 'a t (* opaque *)

val to_list : 'a t -> 'a list
val singleton : 'a -> 'a t
val empty : 'a t
val append : 'a t -> 'a t -> 'a t
val ($$) : 'a t -> 'a t -> 'a t
val fmap : ('a -> 'b) -> 'a t -> 'b t
val (<$>) : ('a -> 'b) -> 'a t -> 'b t
