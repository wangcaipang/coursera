(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s:string, l: string list) =
  case l of
      [] => NONE
    | x::xs' => if same_string(s, x) then SOME (xs') else
		case all_except_option(s, xs') of
		    NONE => NONE
		  | SOME xs => SOME (x::xs)

fun get_substitutions1(l:(string list) list, s:string) =
  case l of
      [] => []
    | x::xs' => case all_except_option(s, x) of
		    NONE => get_substitutions1(xs', s)
		  | SOME ys => ys@get_substitutions1(xs', s) 
						    

fun get_substitutions2(l:(string list) list, s:string) =
  let
      fun helper(l:(string list) list, s: string, ans) =
	case l of
	    [] => ans
	  | x::xs' =>
	    let
		val cur = case all_except_option(s, x) of
			      NONE => []
			    | SOME ys => ys
	    in
		helper(xs', s, ans@cur)
	    end
		
  in
      helper(l,s,[])
  end

fun similar_names(l:(string list) list, full:{first:string,last:string,middle:string}) =
  let
      val names = case full of
		      {first=x, last=_, middle=_ } => get_substitutions2(l, x)
      fun helper(ls: string list,ans) =
	case (ls,full) of
	    ([],_) => ans
	  | (x::xs',{first=a, last=b, middle=c}) => helper(xs', ans@[{first=x, last=b, middle=c}])  
  in
      helper(names, [full])
  end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
	      
(* put your solutions for problem 2 here *)


fun card_color(card) =
  case card of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (_,_) => Red 

fun card_value(card) =
  case card of
      (_, Num i) =>i
    | (_,Ace) => 11
    | (_,_) => 10 

fun remove_card(card_list, card, ex) =
  let
      fun helper(l, ans) =
	case l of
	    [] => raise ex
	  | [x] => if x = card then ans else ans@[x]
	  | x::xs' => if x = card then xs' else helper(xs', ans@[x])
						      
  in
      helper(card_list, [])
  end
      
      
      
fun all_same_color(card_list) =
  case card_list of
      [] => true
    | [x] => true
    | h::n::rest => if card_color h = card_color n then all_same_color(n::rest) else false
											 
fun sum_cards(card_list) =
  let
      fun helper(l, acc) =
	case l of
	    [] => acc
	  | card::rest => helper(rest, acc + card_value card) 
				
  in
      helper(card_list, 0)
  end
      
fun score(card_list, goal) =
  let
      val sum = sum_cards card_list;
      val score = if sum > goal then 3 * (sum - goal) else goal - sum;
  in
      if all_same_color card_list then score div 2 else score
  end
      
      

      
