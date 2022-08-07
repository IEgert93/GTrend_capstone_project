Improbable Connections Among Random Google Trends in Germany
================

### The Introduction

This document aims to give an overview of possible random variable in
its human thinking form by analyzing three Google trends in Germany.
Yeah, sure the sample of the data is very small and is not definitively
conclusive in any way, but using seemingly completely random searches
and analyzing their trends can lead to unusual discoveries. The time
period is two years (2020-08-01 — 2022-07-31). The main target is trying
to understand, if and how couple of possibly random searches lead to
search the term “natural gas” (since Germany is highly dependent on it).
That should be achieved by finding the answers to following question:

Are the connections completely random with unheard coincidence or is it
way of German thinking based on information consumption?

### The Data

As mentioned before, this a bit wild experiment has very small sample of
three keywords used in Google search in Germany. The keywords are
“Natural Gas”, “Roberto Carlos”, and “North Korea”. Seemingly random,
considering that the keyword are one of the representatives of fossil
fuel, football (or soccer) player, and cult of personality. But who
knows, what similarities will see the daylight?

Also, state level German map data is used.

### Timeline Trends of Sample Keywords

Before going into explanation, I lay out bunch of code for you to read.
Explanation is provided after the timeline plot.

``` r
#Loading the necessary libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(gtrendsR)
library(ggrepel)
library(rnaturalearth)
library(tmap)
library(raster)
library(spData)
library(spDataLarge)
library(graphics)
library(Rmisc)
library(ggpubr)
```

``` r
#Data Extraction from Google Trends and creating necessary data frames
DE_randomtrends <- gtrends(keyword = c("North Korea", "Roberto Carlos", "Natural Gas"), geo = "DE", time = "2020-08-01 2022-08-01")

DE_over_time <- DE_randomtrends$interest_over_time
DE_regional <- DE_randomtrends$interest_by_region

#Creating date_id column
DE_over_time$date <- as.Date(as.POSIXct(DE_over_time$date, "GMT"))

DE_over_time <- DE_over_time %>%
  group_by(date) %>%
  transform(., date_id = match(date, unique(date)))
```

``` r
#Timeline graph for the Google Trend keywords
DE_over_time_plot <- DE_over_time %>%
  ggplot() + 
  geom_line(aes(x = date, y = hits, color = keyword), size = 1.1) + 
  scale_color_discrete(name = "Keyword", labels = c("Natural Gas", "North Korea", "Roberto Carlos")) + 
  labs(title = "Sample Keyword Search Trends in Germany", x = "Date", y = "Hit Rate") + 
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

DE_over_time_plot
```

![](Capstone_Project_GTrends_in_GER_files/figure-gfm/timeline%20graphs-1.png)<!-- -->

There is some distinctive differences between the keyword trends over
the period of two years. Lets split them into Year 1 (2020-08-01 —
2021-07-31) and Year 2 (2021-08-01 — 2022-07-31) to analyze the chart a
bit further.

Year 1 seems to have rather expected patterns of randomness, then Year 2
clearly brings out few weekly peaks, which happen simultaneously or are
off by single week. Since that occurs more than once, then it is
justified to ask if that is a mere coincidence or are Germans actually
thinking of Brazilian football legends, when searching information about
natural gas or North Korea. Although, the randomness seems to return
around late spring 2022 to its “usual” form. Other thing, what seems to
stand out, is the hit rate increase of “Natural Gas” keyword during Year
2. That is not a surprise, considering what triggered intensified search
of that term.

### Digging into regional hit rate of sample keywords

This section goes into regional analysis of sample keyword hit rates in
Germany. Little context here is that Germany is divided to 16 states and
that division will be used to map the data of keyword hits.

``` r
#Fetch German map data (state level) and do minor fixes
ger_sf <- ne_states(geounit = "germany", returnclass = "sf")
ger_sf <- ger_sf %>%
  mutate(name_en = ifelse(name_en == "Mecklenburg-Western Pomerania", "Mecklenburg-Vorpommern", name_en))

#Google trends samples data - splitting trends data by keyword
DE_NorthKorea <- DE_regional %>%
  filter(keyword == "North Korea")
DE_NaturalGas <- DE_regional %>%
  filter(keyword == "Natural Gas")
DE_RobertoCarlos <- DE_regional %>%
  filter(keyword == "Roberto Carlos")

#Left joins with map data and then binding rows
DE_ger_sf_NorthKorea <- left_join(ger_sf, DE_NorthKorea, by = c("name_en" = "location"))
DE_ger_sf_NaturalGas <- left_join(ger_sf, DE_NaturalGas, by = c("name_en" = "location"))
DE_ger_sf_RobertoCarlos <- left_join(ger_sf, DE_RobertoCarlos, by = c("name_en" = "location"))

DE_ger_sf_ALL <- bind_rows(DE_ger_sf_NorthKorea, DE_ger_sf_NaturalGas, DE_ger_sf_RobertoCarlos)

#Plotting the data on the map
germany_map <- ggplot(DE_ger_sf_ALL) +
  geom_sf(aes(fill = hits)) + 
  facet_wrap(. ~ keyword) + 
  labs(title = "Sample Keyword Hit Rate in Germany by Region",
       x = "Longitude",
       y = "Latitude") + 
  scale_fill_continuous(name = "Hits (Scaled to 100)") + 
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))

germany_map
```

![](Capstone_Project_GTrends_in_GER_files/figure-gfm/plotting%20maps-1.png)<!-- -->

Relatively speaking, then these three maps bring out the fact that
Brazilian legend Roberto Carlos is searched across Germany fairly often,
with western part having higher hits rates. That can be explained with
West Germany being the powerhouse of world football prior to
reunification of Germany, therefore culture is stronger in that regard.
When it comes to “political keywords”, then North Korea and Natural Gas
are not that popular searches outside Berlin or Hamburg over the past
two years. Hesse region (in mid-west Germany) is an outlier here
regarding Natural Gas searching. North Korea is North Korea and not
popular search term (or in general).

### Check for Linear Regression

It can be assumed that linear regression analysis is not applicable
here, however, checking (or finding out) the correlation between the
combinations of two keyword will give something, what is not exactly
expected or usual. Time to make LOESS smoother and find correlation
coefficients.

``` r
#Making time-series keyword data to wide format
DE_over_time_wide <- DE_over_time %>%
  pivot_wider(names_from = keyword, values_from = hits)

#Combination 1 - hit rates correlations of "Natural Gas" and "North Korea"
DE_loess_plot1 <- ggplot(data = DE_over_time_wide, aes(x = `Natural Gas`, y = `North Korea`)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_smooth(se = FALSE, color = "red") + 
  labs(title = "Natural Gas vs North Korea",
       x = "Natural Gas",
       y = "North Korea")
#Combination 2 - hit rates correlations of "Natural Gas" and "Roberto Carlos"
DE_loess_plot2 <- ggplot(data = DE_over_time_wide, aes(x = `Natural Gas`, y = `Roberto Carlos`)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_smooth(se = FALSE, color = "red") + 
  labs(title = "Natural Gas vs Roberto Carlos",
       x = "Natural Gas",
       y = "Roberto Carlos")
#Combination 3 - hit rates correlations of "Roberto Carlos" and "North Korea"
DE_loess_plot3 <- ggplot(data = DE_over_time_wide, aes(x = `North Korea`, y = `Roberto Carlos`)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_smooth(se = FALSE, color = "red") + 
  labs(title = "North Korea vs Roberto Carlos",
       x = "North Korea",
       y = "Roberto Carlos")

loess_multi <- multiplot(DE_loess_plot1, DE_loess_plot2, DE_loess_plot3, cols = 3)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## `geom_smooth()` using formula 'y ~ x'

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Capstone_Project_GTrends_in_GER_files/figure-gfm/plotting%20loess-1.png)<!-- -->

Above is the combo on regression checks (or correlation searches) and
here is what I see:

1)  Left graph: there is some correlation between, but this isn’t much
    of a surprise, if the reader is avid reader of geopolitical
    news/information. Long story short: Germany gets its gas supply from
    Russia and North Korea is one of the very few allies remaining for
    Russians. Not too long of a connection, if you ask me. That can
    explain the correlation.

2)  Middle graph: as expected, not much happening, though correlation is
    higher than I expected it to be. Regardless, it isn’t high enough to
    consider this something more than a random chance.

3)  Right graph: here is nothing. It would be weird to see Carlos being
    linked to North Korea, unless there are some skeletons in the closet
    still hidden by either side from the public eye.

### The Conclusion

Obviously this was an experiment, can’t stress that enough, with very
small sample. But based on the results seen here, the answer to the
question is that these keyword peaks, highs and lows are coinciding with
one another randomly rather than here being possibility of German
inhabitance thinking about Roberto Carlos, while googling about North
Korea or natural gas. Yet, some correlation does exist between North
Korea and natural gas searches and for a reason (a bad one). Of course,
there are more ways to explore this data, but decided to keep short and
simple. Also, because this being unorthodox “research”.

However, this look into Google Trend data helped to learn more about its
data capabilities or how to make better graphs in general. Also,
somewhat eye opening. Learning is power. If someone would take thousands
of keywords and would analyze it spatial-time relationships between,
then go for it.

Thank you reading this. Feedback is appreciated.
