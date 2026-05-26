# gtime

An Erlang library for common time operations.

## Overview

`gtime` provides utility functions for working with Gregorian seconds, timestamps, time zones, and datetime formatting.

## Build

```sh
make
```

## Test

```sh
make test
```

## API Highlights

- `gregorian_seconds_to_datetime_string/2` – format Gregorian seconds into various string representations (date, time, date_time, etc.)
- `gus_now/0` – current universal time as a `{GregorianSeconds, MicroSeconds}` tuple for easy comparison
- `gus_ms_diff/2` – millisecond difference between two GUS values
- `calculate_time_zone/2` – compute timezone offset from local and universal datetimes
- `yang_datetime_to_gus/1` – convert a YANG date-and-time tuple to GUS format
- `unit_to_now/2` – convert milliseconds or microseconds to an `erlang:timestamp()` tuple

## License

See [LICENSE](LICENSE).
