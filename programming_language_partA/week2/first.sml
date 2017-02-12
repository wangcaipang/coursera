(* This is a comment. This is our first program. *)


val x = 34;
(* static environment: x : int *)
(* dynamic environment: x --> 34 *)

val y = 17;
(* dynamic environment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* dynamic environment: x --> 34, y --> 17, z --> 70 *)

val q = z + 1;

val abs_of_z = if z < 0 then 0 - z else z;

fun pow(x: int, y: int) =
  if y = 0 then 1 else x * pow(x, y -1)

fun cube(x: int) =
  pow(x, 3)

fun swap(pr: int*bool) =
  (#2 pr, #1 pr)


fun append(xs: int list, ys: int list) =
  if null xs then ys else hd xs::append(tl xs, ys)



				       
				       
								     
								 

