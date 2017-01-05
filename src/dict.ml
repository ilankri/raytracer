type ('key, 'value) t = ('key * 'value) list

let empty = []

let lookup key dict = List.assoc key dict

let insert key value dict = (key, value) :: dict
