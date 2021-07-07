type 'a t

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val choose : 'a t -> 'a t -> 'a t
val (<|>) : 'a t -> 'a t -> 'a t

val from_list : 'a list -> 'a t
val run : 'a t -> 'a list
