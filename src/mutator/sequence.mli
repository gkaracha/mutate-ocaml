type 'a sequence (* opaque *)

val to_list : 'a sequence -> 'a list
val unit_sequence : 'a -> 'a sequence
val empty : 'a sequence
val append : 'a sequence -> 'a sequence -> 'a sequence
val ($$) : 'a sequence -> 'a sequence -> 'a sequence
val fmap : ('a -> 'b) -> 'a sequence -> 'b sequence
