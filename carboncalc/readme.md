# Carbon Cost Calculator for Events

## Team

- Dr Adrian Wong
- Dr Mateusz Zawadka
- Dr Teddy Tun Win Hla

## Inspiration

## Assumptions

1.Assumed that all international travels are at a 'economy class' travel. 

> 0.140625 kgCO2 e per unit kilometres per person

2. Assumed that all local travel are calculated at a 'national rail' level without additional use of cars, buses, taxis. 

> 0.03549 kgCO2 e per unit kilometers per person

3. We assumed that each participant / instructors are staying in separate individual rooms without sharing for duration of the event.

> 10.4 kgCO2 e per room per night per person in the UK 

4. We do not separately calculate the components of carbon cost but reports as carbon dioxide equivalent.

5. We assumed that all travels are a two-way (return) journey

6. We assumed that all travels are normally distributed around a mean of 75km for local and 1000km for international travels.

7.  We have  not included the carbon cost of POCUS venue, waste and equipments transfer. This is because these are likely to be similar whether a course is local or international.

## Variables

attendee = number of attendees

faculty = number of faculty

number of international attendees = intl_attn_perc

number of international faculty = intl_fac_perc

number of hotel rooms = hotel_perc

local travel distance = loc_dist

international travel dist = int_disc 

## Internal Logic

- Given attendees can be 'local' or 'international', number of international attendees) cannot exceed total number of attendees. 

- Given faculty can be 'local' or 'international', number of international faculty cannot exceed total number of faculty.

- Most travel distances tend to follow gamma distribution and we have modelled our distance parameter as such.

- Total carbon cost per person is the sum of carbon cost for return travel journey plus the sum of accommodation using this formula.

$$ Total\quad carbon \quad cost \quad per \quad person = \sum_{n=1}^{2} travel  +  \sum_{n=1}^{2} accommodation $$

## Known bugs 

At the moment, there are no known bugs. 



## References 


1. Average CO2 emissions from new cars and new vans increased again in 2019; European Environment Agency. https://www.eea.europa.eu/highlights/average-co2-emissions-from-new-cars-vans-2019. Accessed: 3 Aug 2023



2. UK Government National Travel Survey 2021: Mode share, journey lengths and public transport use. https://www.gov.uk/government/statistics/national-travel-survey-2021/national-travel-survey-2021-mode-share-journey-lengths-and-public-transport-use. Accessed: 14 Jun 2023



3. Network Manager (2023) EUROCONTROL Data Snapshot, https://www.eurocontrol.int/our-data;. Accessed: 10 July 2023



4. Plötz P, Jakobsson N, Sprei F (2017) On the distribution of individual daily driving distances. Transportation Research Part B: Methodological 101:213–227. https://doi.org/10.1016/j.trb.2017.04.008



5. Veloso M, Phithakkitnukoon S, Bento C, et al (2011) Exploratory Study of Urban Flow using Taxi Traces, Proceedings of the 2011 international workshop on Trajectory data mining and analysisACM, pp. 23



6. UK Government,Greenhouse gas reporting: conversion factors 2022. (2022)In: GOV.UK. https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022. Accessed 20 July 2023



7. European Union Air transport statistics. Link . Accessed 4 Aug 2023.


## Session Info
─ Session info ────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.2.1 (2022-06-23)
 os       macOS Monterey 12.6.6
 system   x86_64, darwin17.0
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       Europe/London
 date     2023-08-09
 rstudio  2023.06.0+421 Mountain Hydrangea (desktop)
 pandoc   2.10.1 @ /usr/local/bin/pandoc

─ Packages ────────────────────────────────────────────────────────────────────
 package     * version date (UTC) lib source
 bsicons       0.1     2022-11-22 [1] CRAN (R 4.2.0)
 bslib       * 0.5.0   2023-06-09 [1] CRAN (R 4.2.0)
 cachem        1.0.8   2023-05-01 [1] CRAN (R 4.2.0)
 cli           3.6.1   2023-03-23 [1] CRAN (R 4.2.0)
 colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.2.0)
 crosstalk     1.2.0   2021-11-04 [1] CRAN (R 4.2.0)
 data.table    1.14.8  2023-02-17 [1] CRAN (R 4.2.0)
 digest        0.6.33  2023-07-07 [1] CRAN (R 4.2.0)
 dplyr         1.1.2   2023-04-20 [1] CRAN (R 4.2.0)
 ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.2.0)
 fansi         1.0.4   2023-01-22 [1] CRAN (R 4.2.0)
 fastmap       1.1.1   2023-02-24 [1] CRAN (R 4.2.0)
 fontawesome   0.5.1   2023-04-18 [1] CRAN (R 4.2.0)
 fs            1.6.3   2023-07-20 [1] CRAN (R 4.2.0)
 generics      0.1.3   2022-07-05 [1] CRAN (R 4.2.0)
 ggplot2     * 3.4.2   2023-04-03 [1] CRAN (R 4.2.0)
 glue          1.6.2   2022-02-24 [1] CRAN (R 4.2.0)
 gtable        0.3.3   2023-03-21 [1] CRAN (R 4.2.1)
 htmltools     0.5.5   2023-03-23 [1] CRAN (R 4.2.0)
 htmlwidgets   1.6.2   2023-03-17 [1] CRAN (R 4.2.0)
 httpuv        1.6.11  2023-05-11 [1] CRAN (R 4.2.0)
 httr          1.4.6   2023-05-08 [1] CRAN (R 4.2.0)
 jquerylib     0.1.4   2021-04-26 [1] CRAN (R 4.2.0)
 jsonlite      1.8.7   2023-06-29 [1] CRAN (R 4.2.0)
 labeling      0.4.2   2020-10-20 [1] CRAN (R 4.2.0)
 later         1.3.1   2023-05-02 [1] CRAN (R 4.2.0)
 lazyeval      0.2.2   2019-03-15 [1] CRAN (R 4.2.0)
 lifecycle     1.0.3   2022-10-07 [1] CRAN (R 4.2.0)
 magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.2.0)
 memoise       2.0.1   2021-11-26 [1] CRAN (R 4.2.0)
 mime          0.12    2021-09-28 [1] CRAN (R 4.2.0)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.2.0)
 pillar        1.9.0   2023-03-22 [1] CRAN (R 4.2.1)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.2.0)
 plotly      * 4.10.2  2023-06-03 [1] CRAN (R 4.2.0)
 promises      1.2.0.1 2021-02-11 [1] CRAN (R 4.2.0)
 purrr         1.0.1   2023-01-10 [1] CRAN (R 4.2.0)
 R6            2.5.1   2021-08-19 [1] CRAN (R 4.2.0)
 Rcpp          1.0.11  2023-07-06 [1] CRAN (R 4.2.0)
 rlang         1.1.1   2023-04-28 [1] CRAN (R 4.2.0)
 rsconnect     0.8.29  2023-01-09 [1] CRAN (R 4.2.0)
 rstudioapi    0.14    2022-08-22 [1] CRAN (R 4.2.0)
 sass          0.4.7   2023-07-15 [1] CRAN (R 4.2.0)
 scales        1.2.1   2022-08-20 [1] CRAN (R 4.2.0)
 sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
 shiny       * 1.7.4   2022-12-15 [1] CRAN (R 4.2.0)
 tibble        3.2.1   2023-03-20 [1] CRAN (R 4.2.0)
 tidyr         1.3.0   2023-01-24 [1] CRAN (R 4.2.0)
 tidyselect    1.2.0   2022-10-10 [1] CRAN (R 4.2.0)
 utf8          1.2.3   2023-01-31 [1] CRAN (R 4.2.0)
 vctrs         0.6.3   2023-06-14 [1] CRAN (R 4.2.0)
 viridisLite   0.4.2   2023-05-02 [1] CRAN (R 4.2.0)
 withr         2.5.0   2022-03-03 [1] CRAN (R 4.2.0)
 xtable        1.8-4   2019-04-21 [1] CRAN (R 4.2.0)
 yaml          2.3.7   2023-01-23 [1] CRAN (R 4.2.0)

 [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library

───────────────────────────────────────────────────────────────────────────────