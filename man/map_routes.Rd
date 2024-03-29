% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{map_routes}
\alias{map_routes}
\title{Map a set of routes}
\usage{
map_routes(
  thin_map,
  routes = NA,
  crs = himach::crs_Atlantic,
  show_route = c("speed", "aircraft", "time", "circuity", "acceleration", "traffic"),
  fat_map = NA,
  avoid_map = NA,
  ap_loc = NA,
  ap_col = "darkblue",
  ap_size = 0.4,
  forecast = NA,
  fc_var = NA_character_,
  fc_text = NA_character_,
  crow = FALSE,
  crow_col = "grey70",
  crow_size = 0.2,
  route_envelope = FALSE,
  bound = TRUE,
  bound_margin_km = 200,
  simplify_km = 8,
  land_f = "grey90",
  buffer_f = "grey60",
  land_c = "grey85",
  land_s = 0.2,
  avoid_f = "grey80",
  avoid_c = "grey95",
  avoid_s = 0.3,
  l_alpha = 0.8,
  l_size = 0.5,
  e_alpha = 0.4,
  e_size = 0.6,
  e_col = "grey70",
  refuel_airports = ap_loc,
  rap_col = "red",
  rap_size = 0.4,
  scale_direction = -1,
  title = "",
  subtitle = "",
  warn = FALSE,
  ...
)
}
\arguments{
\item{thin_map}{The minimum is a \code{MULTIPOLYGON} map, 'thin' in that it
is without buffer, so a normal coastline map.}

\item{routes}{as generated by \code{\link{find_route}}}

\item{crs}{Coordinate reference system, default \code{crs_Atlantic}.}

\item{show_route}{one of "speed", "aircraft", "time", "circuity", "accel",
"traffic" to indicate what goes in the legend.}

\item{fat_map}{optional coast + buffer map, default NA.}

\item{avoid_map}{optional map of no-fly zones, default NA.}

\item{ap_loc}{Show used origin and destination airports if this is a set of
airports from \code{\link{make_airports}}, or not if NA (default). This
dataset can be all airports, and is filtered to those used by
\code{routes}.}

\item{ap_col, ap_size}{Colour and size of used airport markers (dark blue,
0.4)}

\item{forecast, fc_var, fc_text}{Forecast set and two strings. See details,
default NA.}

\item{crow, crow_col, crow_size}{If TRUE, show the 'crow-flies' direct great
circle, in colour \code{crow_col} and thickness \code{crow_size}. Default
FALSE, "grey70", 0.2}

\item{route_envelope}{show the route envelope (default FALSE).}

\item{bound, bound_margin_km}{If bound=TRUE (default) crop to bounding box of
the \code{routes}, with additional \code{bound_margin_km} in km (default
200)}

\item{simplify_km}{Simplify the two maps to this scale before plotting
(default 10).}

\item{land_f, buffer_f, avoid_f}{fill colours for thin, fat and no-fly maps,
default grey 90, 70 and 80, respectively}

\item{land_c, land_s}{boundary colour and size for land areas (countries),
default grey 85 and 0.2, respectively (use NA to turn off)}

\item{avoid_c, avoid_s}{boundary colour and size for avoid areas, default grey
95 and 0.3, respectively}

\item{l_alpha, l_size}{line (route) settings for alpha (transparency) and
width, defaults 0.6 and 0.4.}

\item{e_col, e_alpha, e_size}{colour, alpha and width for the range envelope.
Default "grey70", 0.4, 0.6}

\item{refuel_airports}{Show the used refuel airports using these locations,
or nothing if NA. (Defaults to same as \code{ap_loc}.)}

\item{rap_col, rap_size}{Colour and size of refuel airport markers (red, 0.4)}

\item{scale_direction}{Passed to scale_colour_viridis, either -1 (default) or
or 1.}

\item{title, subtitle}{Passed to ggplot.}

\item{warn}{if TRUE show some warnings (when defaults loaded) (default FALSE)}

\item{...}{further parameters passed to \code{scale_colour_viridis_b} (or _c,
_d), such as \code{breaks = }.}
}
\value{
A \code{ggplot}.
}
\description{
\code{map_routes} plots routes, with many options
}
\details{
This function plots the routes, with options for additional layers. Multiple
routes are expected, and they can be coloured by time advantage, by speed
along each segment, or by aircraft type.

The option \code{show_route} "time" requires 'advantage_h' to have been added
to the routes set, from the route summary. If it hasn't then this is done in
a local version, then discarded. Running \code{summarise_routes} to do this
requires an airport dataset; if \code{is.na(ap_loc)} then this is not
available, so a default set is used. You can turn on \code{warn} to see if
this is happening, but by default it is silent.



For \code{show_route = } "speed", "aircraft", "time", "circuity" or "accel",
the information is already available in the \code{routes} dataset. For
\code{show_route = "traffic"} you need to provide a forecast dataset that
contains at least the \code{fullRouteID and acID} fields which are normal in
the \code{routes} dataset, and a field giving the volume of the forecast
\code{fc_var}. This could be flights, seats, or something else: use
\code{fc_text} for the legend title to show the units of \code{fc_var}.
Combinations of \code{fullRouteID and acID} must be unique, which probably
means you must filter by forecast year and forecast scenario before passing
to \code{map_routes}.

The time to compute the map may not be very different with \code{simplify_km}
varying between 2km and 20km, but the time to plot on the screen, or
\code{ggsave} to a file, is longer than the compute time. It is this latter
time that's reduced by simplifying the maps. For single, or short routes, you
can probably see the difference between 2km and 10km, so it's your choice to
prefer speed or beauty.
}
\examples{
#see introductory vignette

}
