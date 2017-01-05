type ('key, 'value) t

val empty : ('key, 'value) t

val lookup : 'key -> ('key, 'value) t -> 'value

val insert : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
