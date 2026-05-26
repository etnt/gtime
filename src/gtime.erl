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
         add_duration/2,
         calculate_local_time_zone/0,
         calculate_time_zone/2,
         convert_timezone/2,
         date_to_gregorian_seconds/1,
         datetime_to_string/1,
         format_rfc3339/1,
         get_tz_offset/1,
         get_tz_offset/2,
         gregorian_seconds_to_date/1,
         gregorian_seconds_to_datetime_string/1,
         gregorian_seconds_to_datetime_string/2,
         gregorian_seconds_to_time/1,
         gus_ms_diff/2,
         gus_now/0,
         local_datetime_in_gregorian_seconds/0,
         now_in_gregorian_seconds/0,
         now_to_gregorian_seconds/1,
         parse_rfc3339/1,
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

-type tz_name() :: string() | binary().
-type duration() :: #{years => integer(), months => integer(),
                      days => integer(), hours => integer(),
                      minutes => integer(), seconds => integer()}.

-export_type([micro_seconds/0, milli_seconds/0, centi_seconds/0,
              yang_datetime/0, timezone/0, duration/0]).

%% Unix epoch expressed in Gregorian seconds.
-define(UNIX_EPOCH, 62167219200).

%% Default zoneinfo directory.
-define(ZONEINFO_DIR, "/usr/share/zoneinfo").


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
%% operators `&lt;', `&gt;', `==', `=&lt;', `&gt;='.
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


%%%===================================================================
%%% RFC 3339 Parsing and Formatting
%%%===================================================================

%% @doc Parse an RFC 3339 datetime string into a yang_datetime() tuple.
%%
%% Accepts strings like:
%% <ul>
%%   <li>"2026-05-26T14:30:00Z"</li>
%%   <li>"2026-05-26T14:30:00+02:00"</li>
%%   <li>"2026-05-26T14:30:00.123456+02:00"</li>
%% </ul>
%%
%% @end
-spec parse_rfc3339(Input) -> {ok, yang_datetime()} | {error, term()} when
      Input :: binary() | string().

parse_rfc3339(Bin) when is_binary(Bin) ->
    parse_rfc3339(binary_to_list(Bin));
parse_rfc3339(Str) ->
    try
        {ok, do_parse_rfc3339(Str)}
    catch
        _:_ -> {error, invalid_format}
    end.

do_parse_rfc3339(Str) ->
    %% "2026-05-26T14:30:00.123456+02:00"
    {Year, [$- | R1]} = string_to_integer(Str, 4),
    {Month, [$- | R2]} = string_to_integer(R1, 2),
    {Day, [Sep | R3]} = string_to_integer(R2, 2),
    true = lists:member(Sep, "Tt "),
    {Hour, [$: | R4]} = string_to_integer(R3, 2),
    {Min, [$: | R5]} = string_to_integer(R4, 2),
    {Sec, R6} = string_to_integer(R5, 2),
    {Us, R7} = parse_frac(R6),
    {TzHour, TzMin} = parse_tz_offset(R7),
    {Year, Month, Day, Hour, Min, Sec, Us, TzHour, TzMin}.

string_to_integer(Str, Len) ->
    {Digits, Rest} = lists:split(Len, Str),
    {list_to_integer(Digits), Rest}.

parse_frac([$. | Rest]) ->
    {Digits, Remainder} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, Rest),
    %% Normalize to microseconds (6 digits)
    Padded = case length(Digits) of
                 N when N >= 6 -> lists:sublist(Digits, 6);
                 N -> Digits ++ lists:duplicate(6 - N, $0)
             end,
    {list_to_integer(Padded), Remainder};
parse_frac(Rest) ->
    {0, Rest}.

parse_tz_offset("Z") -> {0, 0};
parse_tz_offset("z") -> {0, 0};
parse_tz_offset([$+ | Rest]) ->
    {H, M} = parse_tz_hm(Rest),
    {H, M};
parse_tz_offset([$- | Rest]) ->
    {H, M} = parse_tz_hm(Rest),
    case H of
        0 -> {0, -M};
        _ -> {-H, M}
    end.

parse_tz_hm(Str) ->
    {H, R} = string_to_integer(Str, 2),
    case R of
        [$: | R2] -> {M, _} = string_to_integer(R2, 2), {H, M};
        []        -> {H, 0};
        _         -> {M, _} = string_to_integer(R, 2), {H, M}
    end.


%% @doc Format a yang_datetime() tuple as an RFC 3339 binary string.
%%
%% Microseconds are included only when non-zero.
%%
%% @end
-spec format_rfc3339(DateTime) -> binary() when
      DateTime :: yang_datetime().

format_rfc3339({Year, Month, Day, Hour, Min, Sec, Us, TzHour, TzMin}) ->
    DatePart = io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w",
                             [Year, Month, Day, Hour, Min, Sec]),
    FracPart = case Us of
                   0 -> "";
                   _ -> io_lib:format(".~6.6.0w", [Us])
               end,
    TzPart = format_tz_offset(TzHour, TzMin),
    iolist_to_binary([DatePart, FracPart, TzPart]).

format_tz_offset(0, 0) ->
    "+00:00";
format_tz_offset(H, M) when H < 0 ->
    io_lib:format("-~2.2.0w:~2.2.0w", [abs(H), M]);
format_tz_offset(0, M) when M < 0 ->
    io_lib:format("-00:~2.2.0w", [abs(M)]);
format_tz_offset(H, M) ->
    io_lib:format("+~2.2.0w:~2.2.0w", [H, M]).


%%%===================================================================
%%% Timezone Conversion (DST-aware)
%%%===================================================================

%% @doc Convert a yang_datetime() to a different timezone.
%%
%% The target timezone can be:
%% <ul>
%%   <li>A fixed offset tuple `{Hours, Minutes}', e.g. `{-5, 0}'</li>
%%   <li>An IANA timezone name (string or binary), e.g. `"Europe/Stockholm"'</li>
%% </ul>
%%
%% When an IANA name is used, daylight saving time is resolved automatically
%% by reading the system's zoneinfo (TZif) database.
%%
%% @end
-spec convert_timezone(DateTime, Target) -> {ok, yang_datetime()} | {error, term()} when
      DateTime :: yang_datetime(),
      Target :: {integer(), integer()} | tz_name().

convert_timezone({Year, Month, Day, Hour, Min, Sec, Us, TzH, TzM},
                 {TargetH, TargetM}) when is_integer(TargetH),
                                          is_integer(TargetM) ->
    %% Convert to UTC first
    Gsecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                    {Hour, Min, Sec}}),
    Usecs = subtract_time_zone_from_seconds(Gsecs, TzH, TzM),
    %% Apply target offset
    TargetSecs = add_time_zone_to_seconds(Usecs, TargetH, TargetM),
    {{Y2, Mo2, D2}, {H2, Mi2, S2}} =
        calendar:gregorian_seconds_to_datetime(TargetSecs),
    {ok, {Y2, Mo2, D2, H2, Mi2, S2, Us, TargetH, TargetM}};

convert_timezone(DateTime, TzName) when is_binary(TzName) ->
    convert_timezone(DateTime, binary_to_list(TzName));
convert_timezone({Year, Month, Day, Hour, Min, Sec, Us, TzH, TzM}, TzName) ->
    %% Convert to UTC gregorian seconds
    Gsecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
                                                    {Hour, Min, Sec}}),
    Usecs = subtract_time_zone_from_seconds(Gsecs, TzH, TzM),
    %% Look up the target timezone offset at this UTC time
    case get_tz_offset(TzName, Usecs) of
        {ok, {TargetH, TargetM}} ->
            TargetSecs = add_time_zone_to_seconds(Usecs, TargetH, TargetM),
            {{Y2, Mo2, D2}, {H2, Mi2, S2}} =
                calendar:gregorian_seconds_to_datetime(TargetSecs),
            {ok, {Y2, Mo2, D2, H2, Mi2, S2, Us, TargetH, TargetM}};
        {error, _} = Err ->
            Err
    end.

add_time_zone_to_seconds(Secs, TzHour, TzMin) ->
    if TzHour < 0 ->
            Secs + (TzHour*3600 + (-TzMin)*60);
       true ->
            Secs + (TzHour*3600 + TzMin*60)
    end.


%% @doc Get the current UTC offset for an IANA timezone name.
%%
%% Example: `get_tz_offset("America/New_York")' might return `{ok, {-4, 0}}'
%% during EDT or `{ok, {-5, 0}}' during EST.
%%
%% @end
-spec get_tz_offset(TzName) -> {ok, timezone()} | {error, term()} when
      TzName :: tz_name().

get_tz_offset(TzName) ->
    Gsecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    get_tz_offset(TzName, Gsecs).


%% @doc Get the UTC offset for an IANA timezone at a given UTC time
%% (expressed in Gregorian seconds).
%%
%% This correctly handles daylight saving transitions.
%%
%% @end
-spec get_tz_offset(TzName, GregorianSeconds) -> {ok, timezone()} | {error, term()} when
      TzName :: tz_name(),
      GregorianSeconds :: gregorian_seconds().

get_tz_offset(TzName, Gsecs) when is_binary(TzName) ->
    get_tz_offset(binary_to_list(TzName), Gsecs);
get_tz_offset(TzName, Gsecs) ->
    UnixTime = Gsecs - ?UNIX_EPOCH,
    case read_tzif(TzName) of
        {ok, {Transitions, Types}} ->
            Offset = lookup_offset(UnixTime, Transitions, Types),
            OffsetMins = Offset div 60,
            Hours = OffsetMins div 60,
            Mins = abs(OffsetMins rem 60),
            {ok, handle_neg_minutes(Hours, Mins)};
        {error, _} = Err ->
            Err
    end.


%%%===================================================================
%%% Duration Addition
%%%===================================================================

%% @doc Add a duration to a yang_datetime().
%%
%% The duration is a map with optional keys:
%% `years', `months', `days', `hours', `minutes', `seconds'.
%%
%% Year and month arithmetic handles variable month lengths (e.g. adding
%% 1 month to Jan 31 yields Feb 28/29). Day/time arithmetic properly
%% rolls over across month/year boundaries.
%%
%% The timezone offset is preserved unchanged.
%%
%% Example:
%% ```
%% {ok, _} = gtime:parse_rfc3339("2026-03-28T10:00:00+01:00"),
%% Result = gtime:add_duration({2026,3,28,10,0,0,0,1,0},
%%                             #{hours => 2, days => 1}).
%% %% => {2026,3,29,12,0,0,0,1,0}
%% '''
%%
%% @end
-spec add_duration(DateTime, Duration) -> yang_datetime() when
      DateTime :: yang_datetime(),
      Duration :: duration().

add_duration({Year, Month, Day, Hour, Min, Sec, Us, TzH, TzM}, Duration) ->
    %% Add years and months first (calendar arithmetic)
    Year1 = Year + maps:get(years, Duration, 0),
    Month1 = Month + maps:get(months, Duration, 0),
    %% Normalize month (may overflow or underflow)
    {Year2, Month2} = normalize_month(Year1, Month1),
    %% Clamp day to valid range for target month
    MaxDay = calendar:last_day_of_the_month(Year2, Month2),
    Day1 = min(Day, MaxDay),
    %% Convert to seconds, add days/hours/minutes/seconds
    Gsecs = calendar:datetime_to_gregorian_seconds({{Year2, Month2, Day1},
                                                    {Hour, Min, Sec}}),
    AddSecs = maps:get(days, Duration, 0) * 86400 +
              maps:get(hours, Duration, 0) * 3600 +
              maps:get(minutes, Duration, 0) * 60 +
              maps:get(seconds, Duration, 0),
    NewGsecs = Gsecs + AddSecs,
    {{Y3, Mo3, D3}, {H3, Mi3, S3}} =
        calendar:gregorian_seconds_to_datetime(NewGsecs),
    {Y3, Mo3, D3, H3, Mi3, S3, Us, TzH, TzM}.

normalize_month(Year, Month) when Month > 12 ->
    normalize_month(Year + 1, Month - 12);
normalize_month(Year, Month) when Month < 1 ->
    normalize_month(Year - 1, Month + 12);
normalize_month(Year, Month) ->
    {Year, Month}.


%%%===================================================================
%%% TZif File Reader (system zoneinfo database)
%%%===================================================================

%% @private
read_tzif(TzName) ->
    Path = filename:join(?ZONEINFO_DIR, TzName),
    case file:read_file(Path) of
        {ok, Bin} ->
            parse_tzif(Bin);
        {error, Reason} ->
            {error, {tzfile_not_found, TzName, Reason}}
    end.

%% @private Parse a TZif (version 2 or 3) file.
%% We skip the v1 data block and parse the v2/v3 64-bit data.
parse_tzif(<<"TZif", Version, _Reserved:15/binary,
             IsUtCnt:32/big, IsStdCnt:32/big, LeapCnt:32/big,
             TimeCnt:32/big, TypeCnt:32/big, CharCnt:32/big,
             Rest/binary>>) when Version =:= $2; Version =:= $3; Version =:= $\0 ->
    case Version of
        $\0 ->
            %% Version 1 only - parse 32-bit data
            parse_tzif_v1(TimeCnt, TypeCnt, CharCnt, LeapCnt,
                          IsStdCnt, IsUtCnt, Rest);
        _ ->
            %% Skip v1 data block to get to v2/v3 data
            V1Size = TimeCnt * 4 + TimeCnt + TypeCnt * 6 + CharCnt +
                     LeapCnt * 8 + IsStdCnt + IsUtCnt,
            <<_V1Data:V1Size/binary, V2Block/binary>> = Rest,
            parse_tzif_v2(V2Block)
    end;
parse_tzif(_) ->
    {error, invalid_tzif}.

parse_tzif_v1(TimeCnt, TypeCnt, _CharCnt, _LeapCnt, _IsStdCnt, _IsUtCnt, Data) ->
    %% Transition times (32-bit signed)
    TransSize = TimeCnt * 4,
    <<TransBin:TransSize/binary, AfterTrans/binary>> = Data,
    Transitions = [T || <<T:32/big-signed>> <= TransBin],
    %% Transition type indices
    <<TypeIndices:TimeCnt/binary, AfterIndices/binary>> = AfterTrans,
    %% ttinfo structures: utoff(32/signed), dst(8), idx(8)
    TypesSize = TypeCnt * 6,
    <<TypesBin:TypesSize/binary, _Rest/binary>> = AfterIndices,
    Types = parse_ttinfos(TypesBin),
    TransList = lists:zip(Transitions, binary_to_list(TypeIndices)),
    {ok, {TransList, Types}}.

parse_tzif_v2(<<"TZif", _Version, _Reserved:15/binary,
                _IsUtCnt2:32/big, _IsStdCnt2:32/big, _LeapCnt2:32/big,
                TimeCnt2:32/big, TypeCnt2:32/big, _CharCnt2:32/big,
                Rest2/binary>>) ->
    %% Transition times (64-bit signed)
    TransSize2 = TimeCnt2 * 8,
    <<TransBin2:TransSize2/binary, AfterTrans2/binary>> = Rest2,
    Transitions2 = [T || <<T:64/big-signed>> <= TransBin2],
    %% Transition type indices
    <<TypeIndices2:TimeCnt2/binary, AfterIndices2/binary>> = AfterTrans2,
    %% ttinfo structures
    TypesSize2 = TypeCnt2 * 6,
    <<TypesBin2:TypesSize2/binary, _Rest2/binary>> = AfterIndices2,
    Types2 = parse_ttinfos(TypesBin2),
    TransList2 = lists:zip(Transitions2, binary_to_list(TypeIndices2)),
    {ok, {TransList2, Types2}};
parse_tzif_v2(_) ->
    {error, invalid_tzif_v2}.

parse_ttinfos(<<>>) -> [];
parse_ttinfos(<<UtOff:32/big-signed, IsDst:8, _DesigIdx:8, Rest/binary>>) ->
    [{UtOff, IsDst} | parse_ttinfos(Rest)].

%% @private Find the UTC offset for a given unix timestamp.
lookup_offset(_UnixTime, [], Types) ->
    %% No transitions - use the first type (typically for UTC)
    case Types of
        [{Offset, _} | _] -> Offset;
        [] -> 0
    end;
lookup_offset(UnixTime, Transitions, Types) ->
    %% Find the last transition at or before UnixTime
    case find_transition(UnixTime, Transitions) of
        {ok, TypeIdx} ->
            case lists:nth(TypeIdx + 1, Types) of
                {Offset, _IsDst} -> Offset
            end;
        none ->
            %% Before all transitions - use the first non-DST type,
            %% or the first type if all are DST.
            case [Offset || {Offset, 0} <- Types] of
                [First | _] -> First;
                [] ->
                    case Types of
                        [{Offset, _} | _] -> Offset;
                        [] -> 0
                    end
            end
    end.

find_transition(_UnixTime, []) ->
    none;
find_transition(UnixTime, TransList) ->
    find_transition(UnixTime, TransList, none).

find_transition(_UnixTime, [], Acc) ->
    Acc;
find_transition(UnixTime, [{TransTime, TypeIdx} | Rest], _Acc)
  when TransTime =< UnixTime ->
    find_transition(UnixTime, Rest, {ok, TypeIdx});
find_transition(_UnixTime, [{TransTime, _} | _Rest], Acc)
  when TransTime > _UnixTime ->
    Acc.


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

parse_rfc3339_test_() ->
    [?_assertMatch({ok, {2026,5,26,14,30,0,0,0,0}},
                   parse_rfc3339("2026-05-26T14:30:00Z")),
     ?_assertMatch({ok, {2026,5,26,14,30,0,0,2,0}},
                   parse_rfc3339(<<"2026-05-26T14:30:00+02:00">>)),
     ?_assertMatch({ok, {2026,5,26,14,30,0,123456,2,0}},
                   parse_rfc3339("2026-05-26T14:30:00.123456+02:00")),
     ?_assertMatch({ok, {2021,3,1,8,51,21,0,-9,30}},
                   parse_rfc3339("2021-03-01T08:51:21-09:30")),
     ?_assertMatch({error, invalid_format},
                   parse_rfc3339("not-a-date"))
    ].

format_rfc3339_test_() ->
    [?_assertEqual(<<"2026-05-26T14:30:00+02:00">>,
                   format_rfc3339({2026,5,26,14,30,0,0,2,0})),
     ?_assertEqual(<<"2026-05-26T14:30:00.123456+02:00">>,
                   format_rfc3339({2026,5,26,14,30,0,123456,2,0})),
     ?_assertEqual(<<"2021-03-01T08:51:21+00:00">>,
                   format_rfc3339({2021,3,1,8,51,21,0,0,0})),
     ?_assertEqual(<<"2021-03-01T08:51:21-09:30">>,
                   format_rfc3339({2021,3,1,8,51,21,0,-9,30}))
    ].

convert_timezone_fixed_offset_test_() ->
    %% 14:30 UTC+2 => 08:30 UTC-4
    [?_assertMatch({ok, {2026,5,26,8,30,0,0,-4,0}},
                   convert_timezone({2026,5,26,14,30,0,0,2,0}, {-4, 0})),
     %% 23:30 UTC+2 => 02:30 next day UTC+5
     ?_assertMatch({ok, {2026,5,27,2,30,0,0,5,0}},
                   convert_timezone({2026,5,26,23,30,0,0,2,0}, {5, 0})),
     %% Chatham Islands UTC+13:45
     ?_assertMatch({ok, {2026,5,27,1,15,0,0,13,45}},
                   convert_timezone({2026,5,26,11,30,0,0,0,0}, {13, 45}))
    ].

add_duration_test_() ->
    [%% Simple hour addition
     ?_assertMatch({2026,5,26,16,30,0,0,2,0},
                   add_duration({2026,5,26,14,30,0,0,2,0},
                                #{hours => 2})),
     %% Day rollover
     ?_assertMatch({2026,5,27,1,30,0,0,2,0},
                   add_duration({2026,5,26,23,30,0,0,2,0},
                                #{hours => 2})),
     %% Add days
     ?_assertMatch({2026,6,2,14,30,0,0,2,0},
                   add_duration({2026,5,26,14,30,0,0,2,0},
                                #{days => 7})),
     %% Add months (with day clamping: Jan 31 + 1 month => Feb 28)
     ?_assertMatch({2026,2,28,10,0,0,0,1,0},
                   add_duration({2026,1,31,10,0,0,0,1,0},
                                #{months => 1})),
     %% Leap year: Jan 31 2024 + 1 month => Feb 29
     ?_assertMatch({2024,2,29,10,0,0,0,1,0},
                   add_duration({2024,1,31,10,0,0,0,1,0},
                                #{months => 1})),
     %% Add years
     ?_assertMatch({2030,5,26,14,30,0,0,0,0},
                   add_duration({2026,5,26,14,30,0,0,0,0},
                                #{years => 4})),
     %% Combined duration
     ?_assertMatch({2027,8,2,16,45,30,0,2,0},
                   add_duration({2026,5,26,14,30,0,0,2,0},
                                #{years => 1, months => 2, days => 7,
                                  hours => 2, minutes => 15, seconds => 30}))
    ].

get_tz_offset_test_() ->
    %% Test that we can read timezone data from the system.
    %% UTC should always be {0, 0}.
    [?_assertMatch({ok, {0, 0}}, get_tz_offset("UTC")),
     ?_assertMatch({ok, {0, 0}}, get_tz_offset(<<"UTC">>))
    ].

convert_timezone_iana_test_() ->
    %% Summer time in Stockholm (CEST = UTC+2): June 15
    %% 12:00 UTC => 14:00 CEST
    GsecsJune = calendar:datetime_to_gregorian_seconds({{2026,6,15},{12,0,0}}),
    %% Winter time in Stockholm (CET = UTC+1): January 15
    %% 12:00 UTC => 13:00 CET
    GsecsJan = calendar:datetime_to_gregorian_seconds({{2026,1,15},{12,0,0}}),
    [?_assertMatch({ok, {2,0}},
                   get_tz_offset("Europe/Stockholm", GsecsJune)),
     ?_assertMatch({ok, {1,0}},
                   get_tz_offset("Europe/Stockholm", GsecsJan)),
     %% New York: EDT (UTC-4) in summer, EST (UTC-5) in winter
     ?_assertMatch({ok, {-4,0}},
                   get_tz_offset("America/New_York", GsecsJune)),
     ?_assertMatch({ok, {-5,0}},
                   get_tz_offset("America/New_York", GsecsJan))
    ].

-endif.
