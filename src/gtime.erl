%%% File    : gtime.erl
%%% Description : 
%%% Created : 18 Dec 2004 by  <klacke@hyber.org>

%% @author klacke@hyber.org
%% @doc Gregorian seconds time manipulation.
%%
%% @type seconds()   = integer()
%% @type minutes()   = integer()
%% @type hours()     = integer()
%% @type day()       = integer()
%% @type month()     = integer()
%% @type year()      = integer()
%% @type time()      = {hours(), minutes(), seconds()}
%% @type date()      = {year(), month(), day()}
%% @type greg_secs() = integer()
%% @type day_no()    = 1 | 2 | 3 | 4 | 5 | 6 | 7. Weekday, Monday == 1.
%% @type format()    = date | xdate | time | days | date_time.
%%
%% xxx_format_msg/0 is used to return error messages about how the input
%% string to xxx(...), should be formated, to be valid.
%%
%% @type compact_date() = int(). 
%% Is used to store a date() compactly, 1 RAM word instead of 
%% 5 (2 + 3*1) as used by date() and 
%% 3 used by gsec().
%%
%% example:
%% date() = {1999,9,1} -> compact_date() = 19990901
%% date() = {99,9,1}   -> compact_date() = 990901
%%
%% The external format uses 5 bytes (for compact date) rather than 
%% 11 bytes = 2 (tuple) + 5 (year int) + 2*2 (small ints) for a date() and  
%% 8 bytes  = 3 (fixed) + 5 (data part) for gsec()
%% @end
%%-----------------------------------------------------------------------------

-module(gtime).

%%-----------------------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------------------

-export([
	 compact_date_to_date/1,
	 date_to_compact_date/1,
	 now_plus/1,
	 days_diff/2,
	 week_day/1,
	 next_day/1,
	 next_monday/1,
	 next_month/1,
	 end_of_day/1,
	 end_of_month/1,
	 yesterday/0,
         day_before_yesterday/0,
         today/0,
	 tomorrow/0,
	 plus/2,
	 time/0,
	 date/0,
	 local_time/0,
	 gnow/0,
	 gdate/0,
	 gsecs2gdate/1, gdate2date/1, date2gdate/1, datetime2gdate/1,
	 gdate2year/1, gdate2month/1, gdate2day/1, gdate2datetime/1,
	 gsecs2epochtime/1, epochtime2gsecs/1,
	 gtostr/1,
	 gtostr/2,
	 strtog/1,
	 daystr/1,
	 day_of_week/1,
	 add_days_to_date/2,
	 sub_days_from_date/2,
	 add_months_to_date/2,
	 sub_months/2,
	 days_in_month/1,
	 strtodate/1,
	 datetostr/1,
	 date_time_to_str/1,
	 day_to_str/1,
         one_month_from_today/0
	]).

%%-----------------------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------------------

%%=============================================================================
%% External functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @spec   compact_date_to_date(Comp::integer()) -> date()
%% @doc    convert a date stored as a ...YMMDD int(), into date()
%% @end------------------------------------------------------------------------
compact_date_to_date(Comp) ->
    Y = Comp div 10000, 
    Comp2 = Comp - (Y * 10000),
    M = Comp2 div 100,
    Comp3 = Comp2 - (M * 100),
    D = Comp3,
    {Y,M,D}.

%%-----------------------------------------------------------------------------
%% @spec   date_to_compact_date({Y,M,D}) -> integer()
%% @doc    convert date() -> to a ....YMMDD int() 
%% @end------------------------------------------------------------------------
date_to_compact_date({Y,M,D}) ->
    (Y * 10000) + (M * 100) + D.

%%-----------------------------------------------------------------------------
%% @spec now_plus({Op::op(), {Amount::integer(), Type::type()}}) -> greg_secs()
%%           op() = plus | minus
%%           type() = seconds|minutes|hours|days
%% @doc This function returns the time as gregorian_secs with
%%           the Arg added to it.
%% @end------------------------------------------------------------------------
now_plus(Arg) ->
    Now = calendar:datetime_to_gregorian_seconds(gtime:local_time()),
    plus(Now, Arg).

%%-----------------------------------------------------------------------------
%% @spec days_diff(A::atime(), B::atime()) -> integer()
%%           atime() = greg_secs() | date()
%% @doc get A - B diff (in days).
%% @end------------------------------------------------------------------------
days_diff(A, B) when integer(A), integer(B) ->
    DiffSecs = A - B,
    DiffSecs div 86400;
days_diff(A, B) when tuple(A), tuple(B) ->
    days_diff(date2gdate(A), date2gdate(B)).
    
%%-----------------------------------------------------------------------------
%% @spec week_day(Secs::greg_sec()) -> day_no()
%% @doc
%% @end------------------------------------------------------------------------
week_day(Secs) ->
    Gdate = gsecs2gdate(Secs),
    calendar:day_of_the_week(gdate2date(Gdate)).

%% @spec(date::date(), days::integer()) -> date()
add_days_to_date(Date, Days) ->
    gdate2date(plus(date2gdate(Date), {plus, {Days, days}})).


%% @spec(date::date(), days::integer()) -> date()
sub_days_from_date(Date, Days) ->
    gdate2date(plus(date2gdate(Date), {minus, {Days, days}})).

%%-----------------------------------------------------------------------------
%% Function: plus(Time, Arg)
%%           Time = greg_sec()
%%           Arg  = {plus|minus, {Int, seconds|minutes|hours|days}}
%% Descrip.: This function returns Time with Arg added
%% Returns : greg_sec()
%%-----------------------------------------------------------------------------
plus(Time, {Sign, Type}) ->
    Add = case Type of
	      {Int, seconds} ->
		  Int;
	      {Int, minutes} ->
		  60 * Int;
	      {Int, hours} ->
		  60 * 60  * Int;
	      {Int, days} ->
		  60 * 60  * 24 * Int;
	      {Int, years} -> 
		  A = {Y, M, D} = gdate2date(Time),
		  case Sign of
		      plus ->
			  B = {Y+Int, M, D},
			  safe_date2gdate(B) - date2gdate(A);
		      minus ->
			  B = {Y-Int, M, D},
			  date2gdate(A) - safe_date2gdate(B)
		  end
	  end,
    case Sign of
	plus ->  Time + Add;
	minus -> Time - Add
    end.

safe_date2gdate(Date) ->
    date2gdate(adjust_month_day_overrun(Date)).

%% @spec local_time() -> {date(), time()}
%% @doc Returns virtual time.
local_time() ->
    calendar:gregorian_seconds_to_datetime(gnow()).

%% @spec time() -> {hours(), minutes(), seconds()}
%% @doc Returns virtual time.
time() ->
    element(2, local_time()).

%% @doc Returns virtual date.
date() ->
    element(1, local_time()).

%%-----------------------------------------------------------------------------
%% Function: gnow()  - local current time
%%           gdate() - local current date()
%% Descrip.: 
%% Returns : greg_sec()
%%-----------------------------------------------------------------------------
%% @doc Return gregorian seconds as of now()

gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

gdate() -> calendar:datetime_to_gregorian_seconds({gtime:date(), {0,0,0}}).

%%-----------------------------------------------------------------------------
%% Function: 
%% Descrip.: strip non-date seconds (hh-mm-ss) from greg_sec()
%% Returns : 
%%-----------------------------------------------------------------------------
gsecs2gdate(Secs) ->
    date2gdate(gdate2date(Secs)).

%%-----------------------------------------------------------------------------
%% Function: 
%% Descrip.: greg_sec() -> date()
%% Returns : 
%%-----------------------------------------------------------------------------
gdate2date(Secs) ->
    {YMD, _} = calendar:gregorian_seconds_to_datetime(Secs),
    YMD.

gdate2datetime(Secs) ->
    calendar:gregorian_seconds_to_datetime(Secs).

gdate2year(Secs) ->
    {{Y, _M, _D}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    Y.

gdate2month(Secs) ->
    {{_Y, M, _D}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    M.

gdate2day(Secs) ->
    {{_Y, _M, D}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    D.

gsecs2epochtime(Secs) ->
    Secs - 719528 * 86400. %% Numbers from documentation for calendar

epochtime2gsecs(Secs) ->
    Secs + 719528 * 86400. %% Numbers from documentation for calendar

%%-----------------------------------------------------------------------------
%% Function: 
%% Descrip.: date() -> greg_sec() (use 00:00:00 for hh:mm:ss)
%% Returns : 
%%-----------------------------------------------------------------------------
date2gdate(YMD) ->
    calendar:datetime_to_gregorian_seconds({YMD, {0,0,0}}).

%%-----------------------------------------------------------------------------
%% Function: 
%% Descrip.: {date(),time()} -> greg_sec()
%% Returns : 
%%-----------------------------------------------------------------------------
datetime2gdate({YMD, Time}) ->
    calendar:datetime_to_gregorian_seconds({YMD, Time}).

%%-----------------------------------------------------------------------------
%% @spec gtostr(Gsecs::greg_secs()) -> string()
%% @doc Equivalent to <code>gtostr(Gsecs, date_time)</code>.
%%-----------------------------------------------------------------------------
gtostr(Secs) -> gtostr(Secs, date_time).

%%-----------------------------------------------------------------------------
%% @spec gtostr(Gsecs::greg_secs(), Format::format()) -> string()
%% @doc Returns standard format string
%% The returned formats look like this:
%% <pre>
%% Returns : date      -> "YYYY-MM-DD"
%%           xdate     -> "YYYYMMDD"
%%           time      -> "HH:MM:SS"
%%           date_time -> "YYYY-MM-DD HH:MM:SS"  (default)
%%           iso8106   -> "YYYYMMDDTHHMMSS"
%% </pre>
%% Note    : the YYYY part can be 1+ chars
%%-----------------------------------------------------------------------------
gtostr(undefined, _) -> "-";
gtostr(Secs, date) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w", [Year, Month, Day]));
gtostr(Secs, xdate) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w", [Year, Month, Day]));
gtostr(Secs, xdatex) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    Year2 = Year rem 1000,
    lists:flatten(io_lib:format("~2.2.0w~2.2.0w~2.2.0w", [Year2, Month, Day]));
gtostr(Secs, days) ->
    {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w", [calendar:date_to_gregorian_days(
					 Year, Month, Day)]));
gtostr(Secs, time) ->
    {_, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				[Hour, Minute, Second]));
gtostr(Secs, date_time) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
				[Year, Month, Day, Hour, Minute, Second]));
gtostr(Secs, iso8601) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w~2.2.0w~2.2.0wT~2.2.0w~2.2.0w~2.2.0w",
				[Year, Month, Day, Hour, Minute, Second])).

%%-----------------------------------------------------------------------------
%% Function: strtog(Date)
%%           Date = string() on the "YYYY-MM-DD" | "YYYY-MM-DD HH:MM:DD" format
%%                  note: H,M,D have no size limit so either one or two chars
%%                        will work (if the num is < 10)
%% Descrip.: convert a date string to gregorian seconds
%% Returns : greg_sec() | error
%%-----------------------------------------------------------------------------
strtog(Date) ->
    case catch io_lib:fread("~4d-~2d-~2d", Date) of
	{ok, [Year, Month, Day], []} ->
	    DateTime = {{Year, Month, Day}, {0, 0, 0}},
	    case catch calendar:datetime_to_gregorian_seconds(DateTime) of
		{'EXIT', _} -> error;
		Secs ->	{ok, Secs}
	    end;
	_ ->
	    case catch io_lib:fread("~4d-~2d-~2d ~d:~d:~d", Date) of
		{ok, [Year, Month, Day, Hours,Minutes,Seconds], []} ->
		    DateTime = {{Year, Month, Day}, {Hours, Minutes, Seconds}},
		    case catch calendar:datetime_to_gregorian_seconds(
				 DateTime) of
			{'EXIT', _} -> error;
			Secs ->	{ok, Secs}
		    end;
		_ ->
		    error
	    end
    end.

%%-----------------------------------------------------------------------------
%% @spec tomorrow() -> greg_secs()
%% @doc This function returns the date of tomorrow as gregorian_secs
%% @end------------------------------------------------------------------------
tomorrow() ->
    plus(gdate(), {plus, {1, days}}).

%%-----------------------------------------------------------------------------
%% @spec yesterday() -> greg_secs()
%% @doc This function returns the date of yesterday as gregorian_secs
%% @end------------------------------------------------------------------------
yesterday() ->
    plus(gdate(), {minus, {1, days}}).

day_before_yesterday() ->
    plus(gdate(), {minus, {2, days}}).

%%-----------------------------------------------------------------------------
%% @spec today() -> greg_secs()
%% @doc This function returns the date of today, i.e the same as: gtime:gdate()
%% @end------------------------------------------------------------------------
today() ->
    gdate().

%%-----------------------------------------------------------------------------
%% @spec next_day(GDate::greg_secs()) -> greg_secs()
%% @doc This function returns the date of next day as gregorian_secs
%% @end------------------------------------------------------------------------
next_day(GDate) ->
    plus(GDate, {plus, {1, days}}).


day_of_week(GDate) ->
    calendar:day_of_the_week(gdate2date(GDate)).

daystr(1) -> "Monday";
daystr(2) -> "Tuesday";
daystr(3) -> "Wednesday";
daystr(4) -> "Thursday";
daystr(5) -> "Friday";
daystr(6) -> "Saturday";
daystr(7) -> "Sunday".

%% @spec next_monday(date()) -> date()
%% @doc This function returns the date of next Monday as a {Y,M,D} tuple.
next_monday(Date) ->
    Days = 8 - calendar:day_of_the_week(Date),
    add_days_to_date(Date, Days).
	    
%% @spec next_month(date()) -> date()
%% @doc This function returns the date of first of the next month
%%      as a {Y,M,D} tuple.
next_month({Y,M,_}) ->
    add_months_to_date2({Y,M,1}, 1).

%% @spec one_month_from_today() -> date()
%% @doc This function returns the date one month from today.
%%      as a {Y,M,D} tuple.
one_month_from_today() ->
    add_months_to_date2(gtime:date(), 1).


%% @spec end_of_day(GDate::greg_secs()) -> greg_secs()
%% @doc This function returns the Greg.Secs of the end of day,
%%      for the given input, as Greg.Secs.
end_of_day(GTime) ->
    next_day(date2gdate(gdate2date(GTime))) - 1.

%% @spec end_of_month(GDate::greg_secs()) -> greg_secs()
%% @doc This function returns the Greg.Secs of the end of month,
%%      for the given input, as Greg.Secs.
end_of_month(GTime) ->
    date2gdate(next_month(gdate2date(GTime))) - 1.

%%-----------------------------------------------------------------------------
%% @spec add_months_to_date(Date::date(), Months::interger()) -> date()
%% @doc  increment Date by Month months (Month >= 0)
%% @end------------------------------------------------------------------------
add_months_to_date(Date, Months) when Months >= 0 ->
    Date2 = add_months_to_date2(Date, Months),
    adjust_month_day_overrun(Date2).

add_months_to_date2({Y,M,D}, Months) when Months >= 0 ->
    if
	M + Months > 12 ->
	    add_months_to_date2({Y+1,1,D}, Months - (12 - M) -1);
	true ->
	    {Y,M+Months,D}
    end.

adjust_month_day_overrun({Y,M,_D} = Date) ->
    case calendar:valid_date(Date) of
	true -> Date;
	%% D > last day of month, move to next month
	false -> 
	    if M /= 12 ->
		    {Y,M+1,1};
	       M == 12 ->
		    {Y+1,1,1}
	    end
    end.

%%-----------------------------------------------------------------------------
%% @spec sub_months(Date::date(), Months::integer()) -> date()
%% @doc  subtract Month months from Date. Note that the last day of the new
%%       month is used as day date, if the orginal dates day is not availible
%%       e.g. 2005-3-31 - 1 month -> 2005-2-28
%% @end------------------------------------------------------------------------
sub_months({Y,M,D}, Months) when Months >= 0 ->
    SubMonths = Months rem 12,
    SubYears = Months div 12,
    
    Y1 = Y - SubYears,
    M1 = M - SubMonths,
    %% check if SubMonths move into the previous year (M1 = 0 or negativ)
    {Y2, M2} = case M1 < 1 of
		   true -> {Y1 - 1, 12 + M1};
		   false -> {Y1, M1}
	       end,
    
    NewDate = {Y2,M2,D},
    case calendar:valid_date(NewDate) of
	true -> 
	    NewDate;
	false ->
	    {NewY, NewM, _} = NewDate,
	    NewDay = calendar:last_day_of_the_month(NewY, NewM),
	    {NewY, NewM, NewDay}
    end.
	 
%%-----------------------------------------------------------------------------
    
%% This function is used for checking if it is ok to
%% use M/D as a valid recurring date. (i.e. 2/29 is not ok.)
days_in_month(1)  -> 31;
days_in_month(2)  -> 28;
days_in_month(3)  -> 31;
days_in_month(4)  -> 30;
days_in_month(5)  -> 31;
days_in_month(6)  -> 30;
days_in_month(7)  -> 31;
days_in_month(8)  -> 31;
days_in_month(9)  -> 30;
days_in_month(10) -> 31;
days_in_month(11) -> 30;
days_in_month(12) -> 31.


%%-----------------------------------------------------------------------------

%% convert date string() of numbers to date(), where the number are
%% separated by ":" or "-" e.g. "1999-04-23" -> {1999,4,23}
strtodate(S) ->
    [Y,M,D] = lists:map(fun(X) -> list_to_integer(X) end, 
			string:tokens(S, "-:")),
    true = calendar:valid_date(Ret = {Y,M,D}),
    Ret.


%% XXX types:date2str does this with proper 0 padding 
%%     should this be replaced/removed ???
datetostr({Y,M,D}) ->
    io_lib:format("~w-~w-~w", [Y,M,D]).
    
				      
date_time_to_str({Date, Time}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = {Date, Time},
     lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
				 [Year, Month, Day, Hour, Minute, Second])).

day_to_str(Day) ->
    DStr = integer_to_list(Day),
    case [lists:last(DStr)] of
	D when D == "1";
	       D == "2" ->
	    DStr ++ ":a";
	_ ->
	    DStr ++ ":e"
    end.

