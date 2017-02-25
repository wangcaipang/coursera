datatype mytype = TwoInts of int * int
		| Str of string
		|Pizza
fun f x =
  case x of
      Pizza => 3
    | TwoInts(i1, i2) => i1+ i2
    | Str s => String.size s
			   
datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Mutiply of exp * exp


fun eval x =
  case x of
      Constant i => i
    | Negate e => eval e
    | Add(e1, e2) => eval e1 + eval e2
    | Mutiply(e1, e2) => eval e1 * eval e2 

fun findMax x =
  let
      fun bigger(e1, e2) =
	let
	    val e1_max = findMax e1;
	    val e2_max = findMax e2;  
	in
	    if e1_max > e2_max then e1_max else e2_max
	end
  in
      case x of
	  Constant i => i
	| Negate e => ~ (findMax e)
	| Add(e1, e2) => bigger(e1, e2)
	| Mutiply(e1, e2) => bigger(e1, e2)
  end

datatype my_int_list =
	 Empty
	 |Cons of int * my_int_list

val x = Cons(3, Cons(4, Cons(2008, Empty)))

fun append_list(xs, ys:my_int_list) =
  case xs of
      Empty => ys
    | Cons(x, xs') => Cons(x, append_list(xs', ys)) 
