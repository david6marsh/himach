# Make aircraft data from minimum dataset

`make_aircraft` ensures a minimum set of variables describing aircraft

## Usage

``` r
make_aircraft(ac = NA, sound_kph = himach::mach_kph, warn = TRUE)
```

## Arguments

- ac:

  Dataframe containing the minimum fields, or NA (default)

- sound_kph:

  Speed of sound used to convert from Mach to kph, default
  `mach_kph`=1062 at a suitable altitude.

- warn:

  Warn if no `ac` supplied, so default set is used. Default TRUE.

## Value

Dataframe with at least 11 variables describing the performance of one
or more aircraft

## Details

This function provides a test set of aircraft if necessary and adds
variables to a minimal set of data to give all the information that will
be needed.

This minimal set needs to have the following fields:

- `id, type`: a very short, and longer text identifier for this aircraft

- `over_sea_M, over_land_M`: the eponymous two speeds, given as a Mach
  number

- `accel_Mpm`: acceleration in Mach per minute between these two

- `arrdep_kph`: the speed on arrival and departure from airports, given
  in km per hour

- `range_km`: range in km

An attribute is set to help keep track of where the aircraft data came
from (and whether a new cache is needed). If the `aircraftSet` attribute
of the `ac` parameter is not set, the set is treated as 'disposable'.

For more details see the help vignette:
`vignette("SupersonicRouting", package = "himach")`

## Examples

``` r
# do minimal version (we know it will use the default so turn off warning)
ac <- make_aircraft(warn = FALSE)

# on-the-fly example
ac <- data.frame(id = "test", type = "test aircraft",
                 over_sea_M = 2.0, over_land_M = 0.9, accel_Mpm = 0.2,
                 arrdep_kph = 300, range_km = 6000, stringsAsFactors=FALSE)
ac <- make_aircraft(ac, warn = FALSE)

if (FALSE) { # \dontrun{
# example for your own data
aircraft <- utils::read.csv("data/aircraft.csv", stringsAsFactors = FALSE)
aircraft <- make_aircraft(aircraft)
# strongly recommended to record the file name for later reference
attr(aircraft, "aircraftSet") <- "aircraft.csv"
} # }
```
