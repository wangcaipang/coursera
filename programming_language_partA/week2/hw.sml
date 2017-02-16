fun is_older(fDate: int * int * int, sDate: int * int * int) =
  let
     val f_Date = [#1 fDate, #2 fDate, #3 fDate]
     val s_Date = [#1 sDate, #2 sDate, #3 sDate]
     fun order(fd: int list, sd: int list) =
	if null fd
	then false
	else (if hd fd = hd sd then order(tl fd, tl sd) else hd fd < hd sd)
  in
      order(f_Date, s_Date)
  end

      
fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else (if (#2 (hd dates)) = month then 1 else 0) + number_in_month(tl dates, month)

								
fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

							   
fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else
      if (#2(hd dates)) = month then hd dates::dates_in_month(tl dates, month) else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
  
fun get_nth(list: string list,n:int) =
  if n = 1
  then hd list
  else get_nth(tl list, n - 1)

fun date_to_string(date: int * int * int) =
  let
      val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  in
      get_nth(month_list, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end
		     
fun number_before_reaching_sum(sum: int, list: int list) =
  if (sum - hd list) <= 0
  then 0
  else 1 + number_before_reaching_sum((sum - hd list), tl list)
	 
fun what_month(day: int) =
  let
      val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, months) + 1
  end

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else
      let
	  val rest_oldest = oldest(tl dates)
      in
	  if isSome rest_oldest andalso is_older(valOf rest_oldest, hd dates)
	  then rest_oldest
	  else SOME(hd dates)
      end


fun remove_duplicates(list: int list) =
  if null list
  then []
  else
      let
	  fun find(list: int list, num: int) =
	    if null list
	    then false
	    else (if hd list = num then true else find(tl list, num))
	  fun remove(list: int list, visited: int list) =
	    if null list
	    then []
	    else (if find(visited, hd list) then remove(tl list, visited) else hd list :: remove(tl list, hd list::visited) )
      in
	  remove(list, [])
      end

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
  number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
  dates_in_months(dates, remove_duplicates(months))
  
fun reasonable_date(date: int * int * int) =
  let
      val year = #1 date;
      val month = #2 date;
      val day = #3 date;
      fun check_day(is_leap: bool) =
	let
	    val month_list =  [31,28,31,30,31,30,31,31,30,31,30,31];
	    fun get_nth(list: int list, pos: int) =
	      if pos = 1
	      then hd list
	      else get_nth(tl list, pos -1)
	in
	    if is_leap andalso month = 2
	    then day <= 29
	    else day <= get_nth(month_list, month)
	end
  in
      if year > 0 andalso  month > 0 andalso month <= 12 andalso day > 0 andalso day < 32
      then check_day(year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0))
      else false
  end
