%%%-------------------------------------------------------------------
%%% @doc A library for common time operations.
%%%
%%% Provides utility functions for working with Gregorian seconds,
%%% timestamps, time zones, and datetime formatting.
%%%
%%% Key concepts:
%%% <ul>
%%%   <li>`gregorian_seconds()' - seconds since year 0 (as defined by
%%%       the `calendar' module)</li>
%%%   <li>`gus()' - a tuple `{GregorianSeconds, MicroSeconds}' suitable
%%%       for precise time comparison using standard operators</li>
%%%   <li>`yang_datetime()' - a 9-element tuple representing date, time,
%%%       microseconds, and timezone offset (hours + minutes)</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(gtime).

-export([
         calculate_local_time_zone/0,
         calculate_time_zone/2,
         date_to_gregorian_seconds/1,
         datetime_to_string/1,
         gregorian_seconds_to_date/1,
         gregorian_seconds_to_datetime_string/1,
         gregorian_seconds_to_datetime_string/2,
         gregorian_seconds_to_time/1,
         gus_ms_diff/2,
         gus_now/0,
         local_datetime_in_gregorian_seconds/0,
         now_in_gregorian_seconds/0,
         now_to_gregorian_seconds/1,
         today_in_gregorian_seconds/0,
         unit_to_now/2,
         universal_seconds_to_local_seconds/1,
         yang_datetime_to_gus/1,
         yang_datetime_to_local_datetime/1,
         yang_datetime_to_universal_datetime/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%% types
-type gregorian_seconds() :: non_neg_integer().
-type micro_seconds() :: non_neg_integer().
-type milli_seconds() :: non_neg_integer().
-type centi_seconds() :: non_neg_integer().
-type timezone() :: {integer(), integer()}.
-type gus() :: {gregorian_seconds(), micro_seconds()}.
-type yang_datetime() :: {Year::integer(), Month::integer(), Day::integer(),
                          Hour::integer(), Min::integer(), Sec::integer(),
                          Us::integer(),TzHour::integer(),TzMin::integer()}.
-type datetime_string_format() :: date | xdate | time | time12 | time12ampm |
                                  time24hm | date_time.

-export_type([micro_seconds/0, milli_seconds/0, centi_seconds/0]).


%% @doc Convert Gregorian Seconds into a formatted string.
%%
%% Equivalent to: gregorian_seconds_to_datetime_string(Secs, date_time)
%%
%% @end
gregorian_seconds_to_datetime_string(Secs) ->
    gregorian_seconds_to_datetime_string(Secs, date_time).

%% @doc Convert Gregorian Seconds into a formatted string.
%%
%% The following formats is supported, resulting in a formatted
%% string according to the examples shown below.
%%
%%         date: "2021-02-16"
%%        xdate: "20210216"
%%         time: "13:21:47"
%%       time12: "01:21:47"
%%   time12ampm: "01:21 PM"
%%     time24hm: "13:21"
%%    date_time: "2021-02-16 13:21:47"
%%
%% @end
-spec gregorian_seconds_to_datetime_string(Seconds, Format) -> string() when
      Seconds :: gregorian_seconds(),
      Format :: datetime_string_format().

gregorian_seconds_to_datetime_string(Secs, date) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w", [Y, M, D]));
%%
gregorian_seconds_to_datetime_string(Secs, xdate) ->
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w", [Y, M, D]));
%%
gregorian_seconds_to_datetime_string(Secs, time) ->
    {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H, M, S]));
%%
gregorian_seconds_to_datetime_string(Secs, time12) ->
    {_, {H, M, S}} = calendar:gregorian_seconds_to_datetime(Secs),
    H12 = if H > 12 -> H - 12;
                true -> H
             end,
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H12, M, S]));
%%
gregorian_seconds_to_datetime_string(Secs, time12ampm) ->
    {_, {H, M, _S}} = calendar:gregorian_seconds_to_datetime(Secs),
    {H12, AmPm} = if H > 12 -> {H - 12, 'PM'};
                     true -> {H, 'AM'}
                  end,
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w ~s",
                                [H12, M, AmPm]));
%%
gregorian_seconds_to_datetime_string(Secs, time24hm) ->
    {_, {H, M, _S}} = calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~2.2.0w:~2.2.0w", [H, M]));
%%
gregorian_seconds_to_datetime_string(Secs, date_time) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Secs),
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                                [Year, Month, Day, Hour, Minute, Second])).

-ifdef(EUNIT).
gregorian_seconds_to_datetime_string_test_() ->
    Gsecs = calendar:datetime_to_gregorian_seconds({{2021,2,16},
                                                    {13,21,47}}),
    [?_assertMatch("2021-02-16",
                   gregorian_seconds_to_datetime_string(Gsecs, date))
     ,?_assertMatch("20210216",
                    gregorian_seconds_to_datetime_string(Gsecs, xdate))
     ,?_assertMatch("13:21:47",
                    gregorian_seconds_to_datetime_string(Gsecs, time))
     ,?_assertMatch("01:21:47",
                    gregorian_seconds_to_datetime_string(Gsecs, time12))
     ,?_assertMatch("01:21 PM",
                    gregorian_seconds_to_datetime_string(Gsecs, time12ampm))
     ,?_assertMatch("13:21",
                    gregorian_seconds_to_datetime_string(Gsecs, time24hm))
     ,?_assertMatch("2021-02-16 13:21:47",
                    gregorian_seconds_to_datetime_string(Gsecs, date_time))
    ].

-endif.

%% @doc Return Today in Gregorian Seconds.
%%
%% Note that 'erlang:date/0' returns the date in the current local time zone.
%%
%% @end
-spec today_in_gregorian_seconds() -> gregorian_seconds().

today_in_gregorian_seconds() ->
    date_to_gregorian_seconds(date()).


%% @doc Convert given Date to Gregorian Seconds.
-spec date_to_gregorian_seconds(YMD) -> gregorian_seconds() when
      YMD :: calendar:date().

date_to_gregorian_seconds({_,_,_} = YMD)  ->
    calendar:datetime_to_gregorian_seconds({YMD, {0,0,0}}).


%% @doc Convert Gregorian Seconds to a Date.
-spec gregorian_seconds_to_date(Seconds) -> calendar:date() when
      Seconds :: gregorian_seconds().

gregorian_seconds_to_date(Seconds)  ->
    {YMD, _} = calendar:gregorian_seconds_to_datetime(Seconds),
    YMD.


%% @doc Convert Gregorian Seconds to a Time.
-spec gregorian_seconds_to_time(Seconds) -> calendar:time() when
      Seconds :: gregorian_seconds().

gregorian_seconds_to_time(Seconds)  ->
    {_, HMS} = calendar:gregorian_seconds_to_datetime(Seconds),
    HMS.


%% @doc Return Local DateTime to Gregorian Seconds
-spec local_datetime_in_gregorian_seconds() -> gregorian_seconds().

local_datetime_in_gregorian_seconds()  ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).


%% @doc (Universal) Now in Gregorian Seconds.
-spec now_in_gregorian_seconds() -> gregorian_seconds().

now_in_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% @doc Convert (Universal) Now into Gregorian Seconds.
-spec now_to_gregorian_seconds(Now) -> gregorian_seconds() when
      Now :: erlang:timestamp().

now_to_gregorian_seconds(Now) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)).


%% @doc Convert Universal Seconds into Local Seconds.
-spec universal_seconds_to_local_seconds(Seconds) -> gregorian_seconds() when
      Seconds :: gregorian_seconds().

universal_seconds_to_local_seconds(USecs) ->
    UniversalDateTime = calendar:gregorian_seconds_to_datetime(USecs),
    LocalDateTime = calendar:universal_time_to_local_time(UniversalDateTime),
    calendar:datetime_to_gregorian_seconds(LocalDateTime).



%% @doc Format a DateTime tuple into a string.
-spec datetime_to_string(DateTime) -> string() when
      DateTime :: calendar:datetime().

datetime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
     lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                                 [Year, Month, Day, Hour, Minute, Second])).


%% @doc Convert the 'yang:date-and-time' into Local 'yang:date-and-time'.
-spec yang_datetime_to_local_datetime(DateTime) -> yang_datetime() when
      DateTime :: yang_datetime().

yang_datetime_to_local_datetime({Year, Month, Day, Hour, Min,
                                 Sec, Us, TzHour, TzMin}) ->
    Tsecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                    {Hour, Min, Sec}}),

    Usecs = subtract_time_zone_from_seconds(Tsecs, TzHour, TzMin),
    Lsecs = universal_to_local(Usecs),
    Offset = Lsecs - Usecs,
    OffMins = Offset div 60,
    TzHour1 = OffMins div 60,
    TzMin1 = OffMins rem 60,
    {{Year1, Month1, Day1}, {Hour1, Min1, Sec1}} =
        calendar:gregorian_seconds_to_datetime(Lsecs),
    {Year1, Month1, Day1, Hour1, Min1, Sec1, Us, TzHour1, TzMin1}.

universal_to_local(UnivGregSecs) ->
    DTime = calendar:gregorian_seconds_to_datetime(UnivGregSecs),
    LDtime = calendar:universal_time_to_local_time(DTime),
    calendar:datetime_to_gregorian_seconds(LDtime).


%% @doc Convert the 'yang:date-and-time' into Universal 'yang:date-and-time'.
-spec yang_datetime_to_universal_datetime(DateTime) -> yang_datetime() when
      DateTime :: yang_datetime().

yang_datetime_to_universal_datetime({Year, Month, Day, Hour, Min,
                                     Sec, Us, TzHour, TzMin}) ->
    Tsecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                    {Hour, Min, Sec}}),
    Usecs = subtract_time_zone_from_seconds(Tsecs, TzHour, TzMin),
    {{Year1, Month1, Day1}, {Hour1, Min1, Sec1}} =
        calendar:gregorian_seconds_to_datetime(Usecs),
    {Year1, Month1, Day1, Hour1, Min1, Sec1, Us, 0, 0}.


%% @doc Return (Universal) Now as a GUS tuple {UniversalGregorianSecs,MicroSecs}
%%
%% Good function to use when comparing time, as time
%% is expressed in a format that can use the comparison
%% operators '<', '>', '==', '=<', '>='.
%%
%% @end
-spec gus_now() -> gus().

gus_now() ->
    Now = erlang:timestamp(),
    {_MSec, _Sec, Us} = Now,
    {calendar:datetime_to_gregorian_seconds(
       calendar:now_to_universal_time(Now)), Us}.


%% @doc Return the difference between two Gus values, in milliseconds.
-spec gus_ms_diff(Gus1, Gus2) -> integer() when
      Gus1 :: gus(),
      Gus2 :: gus().

gus_ms_diff({GSecs, Us}, {GSecs2, Us2}) ->
    trunc((GSecs2-GSecs)*1000+(Us2-Us)/1000).


%% @doc Convert YANG-DATETIME to a GUS tuple: {UniversalGregorianSecs,MicroSecs}
-spec yang_datetime_to_gus(DateTime) -> gus() when
      DateTime :: yang_datetime().

yang_datetime_to_gus({Year,Month,Day,Hour,Min,Sec,Us,TzHour0,TzMin}) ->
    TzHour = if is_integer(TzHour0) -> TzHour0;
                true                -> 0
             end,
    TSecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                    {Hour, Min, Sec}}),
    GSecs = subtract_time_zone_from_seconds(TSecs, TzHour, TzMin),
    {GSecs, Us}.


-ifdef(EUNIT).
yang_datetime_to_gus_test_() ->
    LDT = {2021,3,1,0,35,0,Us=10,1,0},
    UGS = calendar:datetime_to_gregorian_seconds({{2021,2,28},{23,35,0}}),

    %% Marquesas Islands
    LDT2 = {2021,2,28,23,21,21,Us2=10,-9,30},
    UGS2 = calendar:datetime_to_gregorian_seconds({{2021,3,1},{8,51,21}}),

    %% Chatham Islands
    LDT3 = {2021,3,1,12,59,21,Us3=10,13,45},
    UGS3 = calendar:datetime_to_gregorian_seconds({{2021,2,28},{23,14,21}}),

    [?_assertMatch({UGS, Us}, yang_datetime_to_gus(LDT))
     ,?_assertMatch({UGS2,Us2}, yang_datetime_to_gus(LDT2))
     ,?_assertMatch({UGS3,Us3}, yang_datetime_to_gus(LDT3))
    ].
-endif.


%% @doc Return unit milliseconds or microseconds to now.
%%
%%
%% @end
-spec unit_to_now(Unit, non_neg_integer()) -> erlang:timestamp() when
      Unit :: atom().

unit_to_now(millisecond, TimeMs) ->
    %% millisecond type is taken from erlang:time_unit()
    SecondsTmp = TimeMs div 1000,
    MegaSeconds = SecondsTmp div 1000000,
    Seconds = SecondsTmp - MegaSeconds * 1000000,
    MicroSeconds = 1000 * (TimeMs - SecondsTmp * 1000),
    {MegaSeconds, Seconds, MicroSeconds};
unit_to_now(microsecond, TimeUs) ->
    MegaSeconds  = TimeUs div 1000000000000,
    Seconds      = TimeUs div 1000000 - MegaSeconds * 1000000,
    MicroSeconds = TimeUs rem 1000000,
    {MegaSeconds, Seconds, MicroSeconds}.


%% @doc Calculate the current local Time Zone.
-spec calculate_local_time_zone() -> timezone().

calculate_local_time_zone() ->
    Now = erlang:timestamp(),
    calculate_time_zone(Now).


%% @doc Calculate the local Time Zone based on the (Universal) Now value.
-spec calculate_time_zone(Now) -> timezone() when
      Now :: erlang:timestamp().

calculate_time_zone(Now) ->
    LDT = calendar:now_to_local_time(Now),
    UDT = calendar:now_to_universal_time(Now),
    calculate_time_zone(LDT, UDT).


%% @doc
%% We transform the Local and Universal DateTime to Gregorian seconds and
%% then calculate the difference in Hours and Minutes which constitutes
%% the TimeZone. This way we avoid borderline problems between Days,
%% Months and Years.
%%
%% Note: We compute the difference counted in 15 minutes intervals.
%% We also add/subtract '450' (60*15/2) to the calculation in order
%% to compensate for any rounding off error if the Local Time should
%% be out of sync. The accuracy is in 15 minutes intervals which is
%% the minimum time zone difference that may exist in the World.
%%
%% Note: Times behind UTC, e.g 'UTC-9:30h' is represented as: {-9,30}
%%                             'UTC-0:30h' is represented as: {0,-30}
%%
%% Example: Cocos Islands
%%
%% {6,30} = calculate_time_zone({{2021,1,28},{19,32,12}},
%%                              {{2021,1,28},{13,02,12}}).
%%
%% See the EUnit tests for more examples!
%%
%% @end
-spec calculate_time_zone(LocalDateTime,
                          UniversalDateTime) -> timezone() when
      LocalDateTime :: calendar:datetime(),
      UniversalDateTime :: calendar:datetime().

calculate_time_zone(LocalDateTime, UniversalDateTime) ->
    LocalGsec = calendar:datetime_to_gregorian_seconds(LocalDateTime),
    UniversalGsec = calendar:datetime_to_gregorian_seconds(UniversalDateTime),

    QuarterMinutes =
        if LocalGsec >= UniversalGsec ->
                (LocalGsec - UniversalGsec + 450) div (60*15);
           true ->
                (LocalGsec - UniversalGsec - 450) div (60*15)
        end,

    Hours = QuarterMinutes div 4,
    Minutes = (QuarterMinutes  rem 4) * 15,

    handle_neg_minutes(Hours, Minutes).


%% If Hours is Zero then keep the sign of the Minutes.
%% Else, only keep the sign of the Hours.
handle_neg_minutes(0 = Hours, Minutes) ->
    {Hours, Minutes};
handle_neg_minutes(Hours, Minutes) ->
    {Hours, abs(Minutes)}.

subtract_time_zone_from_seconds(Tsecs, TzHour, TzMin) ->
    if TzHour < 0 ->
            Tsecs - (TzHour*3600 + (-TzMin)*60);
       true ->
            Tsecs - (TzHour*3600 + TzMin*60)
    end.


-ifdef(EUNIT).

cocos_island_time_zone_test_() ->
    [?_assertMatch({6,30},
                   calculate_time_zone({{2021,1,28},{19,32,12}},
                                       {{2021,1,28},{13,02,12}})),
     %% different days
     ?_assertMatch({6,30},
                   calculate_time_zone({{2021,1,26},{1,32,12}},
                                       {{2021,1,25},{19,02,12}})),
     %% different days + 1 minute diff
     ?_assertMatch({6,30},
                   calculate_time_zone({{2021,1,26},{1,33,12}},
                                       {{2021,1,25},{19,02,12}}))
    ].

chatham_island_time_zone_test_() ->
    [?_assertMatch({13,45},
                   calculate_time_zone({{2021,1,29},{2,51,51}},
                                       {{2021,1,28},{13,06,51}})),
     %% 1 minute diff
     ?_assertMatch({13,45},
                   calculate_time_zone({{2021,1,29},{2,52,51}},
                                       {{2021,1,28},{13,06,51}})),
     ?_assertMatch({13,45},
                   calculate_time_zone({{2021,1,29},{2,51,51}},
                                       {{2021,1,28},{13,07,51}})),
     %% 2 minute diff
     ?_assertMatch({13,45},
                   calculate_time_zone({{2021,1,29},{2,53,51}},
                                       {{2021,1,28},{13,06,51}})),
     ?_assertMatch({13,45},
                   calculate_time_zone({{2021,1,29},{2,51,51}},
                                       {{2021,1,28},{13,08,51}}))
     ].

marquesas_islands_time_zone_test_() ->
    [?_assertMatch({-9,30},
                   calculate_time_zone({{2021,1,28},{5,21,51}},
                                       {{2021,1,28},{14,51,51}})),
     ?_assertMatch({-9,30},
                   calculate_time_zone({{2021,2,28},{23,21,51}},
                                       {{2021,3,1}, {8,51,51}}))
    ].

newfoundland_time_zone_test_() ->
    [?_assertMatch({-3,30},
                   calculate_time_zone({{2021,1,25},{12,17,08}},
                                       {{2021,1,25},{15,47,08}})),
     %% different days
     ?_assertMatch({-3,30},
                   calculate_time_zone({{2021,1,24},{23,17,08}},
                                       {{2021,1,25},{2,47,08}}))
    ].

kiribati_line_islands_time_zone_test_() ->
    [?_assertMatch({14,0},
                   calculate_time_zone({{2021,2,1},{23,55,23}},
                                       {{2021,2,1},{9,55,23}})),
     %% different days
     ?_assertMatch({14,0},
                   calculate_time_zone({{2021,2,2},{0,0,7}},
                                       {{2021,2,1},{10,0,7}}))
    ].

-endif.
