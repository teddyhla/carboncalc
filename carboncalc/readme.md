# Carbon Cost Calculator for POCUS

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

3. We assumed that each participant / instructors are staying in separate individual rooms without sharing for two nights.

> 10.4 kgCO2 e per room per night per person in the UK 

4. We do not separately calculate the components of carbon cost but reports as carbon dioxide equivalent.

5. We assumed that all travels are a two-way (return) journey

6. We assumed that all travels are normally distributed around a mean (user-set) and standard deviation(set at 10km for local and 50km for international travels)

7.  We have deliberately not included the carbon cost of POCUS venue and equipments transfer. This is because these are likely to be similar whether a course is local or international.

## Variables

attendee = number of attendees

faculty = number of faculty

percentage of international attendees = intl_attn_perc

percentage of international faculty = intl_fac_perc

percentage staying in hotels = hotel_perc

local travel distance = loc_dist

international travel dist = int_disc 

## Internal Logic

- Given attendees can be 'local' or 'international', number of international attendees(variable 3) cannot exceed total number of attendees (variable 1). 

- Given faculty can be 'local' or 'international', number of international faculty (variable 4) cannot exceed total number of faculty (variable 2).

- Number staying in hotels (variable 5) cannot exceed as a maximum total number of attendees and faculty(variable 1 and variable 2). This scenario would be where all the attendees and faculty stay in 'hotel' regardless of local or international travels. Conversely, the minimum cannot exceed sum of international attendees and faculty (i.e., all international attendees and faculty must stay in hotels for accommodation).

- Based on UK National Travel Survey[1], surface rail remains the most common mode of transport for average miles travelled per person per year. As a result, for local travel, we assumed that 'surface rail' will be used as most common mode of travel. For 'typical' local courses, we have modelled using a mean distance of 100km (approximately 1 hour 15 mins surface rail journey time) as a maximum upperlimit of acceptable commute.

- Based on Eurocontrol, an average flight distance travelled in European Union is 981km and as a result, we have used as a 1000 km as default for international flight. 

- Most travel distances tend to follow gamma distribution and we have modelled our distance parameter as such.

- Total carbon cost per person is the sum of carbon cost for return travel journey plus the sum of accommodation using this formula.

$$ Total\quad carbon \quad cost \quad per \quad person = \sum_{n=1}^{2} travel  +  \sum_{n=1}^{2} accommodation $$

> ?May be we should wrangle own data using european air travel dataset
## Known bugs 

## Learnings
- I was making it too complicated by creating a dataframe. 
- I should ideally just use a vector.

This complicated piece of code is using it to generate a reactive dataframe, where a vector would have sufficed. 

daf <- reactive({
                 #first an id is assigned based on number of attendees and faculty
                 # then they are classed as faculty or attendees
                 # then their mode of travel is assigned
                df <- data.frame(
                        #id = (1:(attn() + fac())),
                        type = c(rep("attendee",times = attn()), rep("faculty",times = fac())),
                        travel = c(
                                 rep("nonlocal", times = attn_intl()),
                                 rep("local", times = attn_home()),
                                 rep("nonlocal",times = fac_intl()),
                                 rep("local", times = fac_home())
                        ),
                         accommodation = c(
                                 rep("hotel",times = no_hotel()),
                                 rep("home",times = no_home())
                         ),
                        eventdur = rep(dur(), times = (attn() + fac()))
                        #if else doesnt work because it is not assigned yet!
                )
                 #then their state of accommodation is assigned.
                 df$carbon_accomo_cost <- ifelse(df$accommodation == "hotel",20.8,0)
                 #then lets multiply carbon accommo total by duration of course
                 df$total_carbon_accomo <- df$carbon_accomo_cost * df$eventdur
                 #then dataframe is sorted based on mode of travel
                 df<-df[order(df$travel),]
                 # then we assigned distances computed
                 df$dist <- ifelse(df$travel == "local",distloc(),distintl())
                 
                 # see read me for rationale of this
                 df$carbon_travel_cost <- ifelse(df$travel == "local",0.03549,0.14062)
                 # now total travel 
                 df$total_carbon_travel <- (df$carbon_travel_cost * 2 * df$dist)
                 df$sum_carbon <- df$total_carbon_travel + df$total_carbon_accomo
                 #dont forget to return a reactive dataframe back
                 #change to factors for
                 df$type <- as.factor(df$type)
                 df$accommodation <- as.factor(df$accommodation)
                 df$travel <- as.factor(df$travel)
                df
         })
         df2 <- reactive({
                d1 <- daf()[,c("travel", "total_carbon_travel")]
                names(d1) <- c("breakdown","c_carbon")
                d2 <- daf()[,c("accommodation","total_carbon_accomo")]
                names(d2) <- c("breakdown", "c_carbon")
                d2$breakdown <- as.factor(d2$breakdown)
                df2 <- rbind(d1,d2)
                df2 <- aggregate(df2$c_carbon, by = list(df2$breakdown), FUN = sum)
                names(df2) <- c("breakdown","c_carbon")
                df2$frac <- round((df2$c_carbon / sum(df2$c_carbon))*100, 2)
                #val <- df2$c_carbon
                #names(val) <- paste0(df2$breakdown,"(",df2$frac,"%)")

                
                #df2$ymax <- cumsum(df2$frac)
                #df2$ymin <- c(0,head(df2$ymax, n = -1))
                df2
         })
         
         
### Making complicated doughnut plots
 ggplot(df2(), aes(ymax = ymax, ymin = ymin, xmax =4, xmin = 3, fill = breakdown))+
                #        geom_rect() +
                #        #geom_label( x = 3.5, aes( y = labelPosition, label = label),size = 6) +
                #        scale_fill_brewer(palette = 4) +
                #        geom_label(aes(label = frac)) +
                #        coord_polar(theta = "y") + 
                #        xlim(c(2,4)) +
                #        theme_void()

## Future direction
- bootswatch theme [/]
- embed in custom 

## References 

1.  UK Government National Travel Survey 2021: Mode share, journey lengths and public transport use - GOV.UK. In: www.gov.uk. https://www.gov.uk/government/statistics/national-travel-survey-2021/national-travel-survey-2021-mode-share-journey-lengths-and-public-transport-use. Accessed 14 Jun 2023

2. Network Manager (2023) EUROCONTROL Data Snapshot
3. Paul A, Schmalz U (2017) DATASET2050 Deliverable 3.1 " Current Passenger Demand Profile "
4. Plötz P, Jakobsson N, Sprei F (2017) On the distribution of individual daily driving distances. Transportation Research Part B: Methodological 101:213–227. https://doi.org/10.1016/j.trb.2017.04.008
5. Veloso M, Phithakkitnukoon S, Bento C, et al (2011) Exploratory Study of Urban Flow using Taxi Traces

# current workign commit
- 5ec5f9d

## Session Info


R version 4.2.1 (2022-06-23)

Platform: x86_64-apple-darwin17.0 (64-bit)

Running under: macOS Monterey 12.6.6


Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] plotly_4.10.2 ggplot2_3.4.2 shiny_1.7.4  

loaded via a namespace (and not attached):

 [1] Rcpp_1.0.10       jquerylib_0.1.4   bslib_0.4.2       pillar_1.9.0     
 [5] compiler_4.2.1    later_1.3.1       tools_4.2.1       digest_0.6.31    
 [9] memoise_2.0.1     viridisLite_0.4.2 jsonlite_1.8.5    lifecycle_1.0.3  
[13] tibble_3.2.1      gtable_0.3.3      pkgconfig_2.0.3   rlang_1.1.1      
[17] cli_3.6.1         rstudioapi_0.14   crosstalk_1.2.0   yaml_2.3.7       
[21] fastmap_1.1.1     httr_1.4.6        withr_2.5.0       dplyr_1.1.2      
[25] sass_0.4.6        generics_0.1.3    vctrs_0.6.2       htmlwidgets_1.6.2
[29] grid_4.2.1        DT_0.28           tidyselect_1.2.0  data.table_1.14.8
[33] glue_1.6.2        R6_2.5.1          fansi_1.0.4       tidyr_1.3.0      
[37] purrr_1.0.1       magrittr_2.0.3    scales_1.2.1      promises_1.2.0.1 
[41] htmltools_0.5.5   ellipsis_0.3.2    shinythemes_1.2.0 rsconnect_0.8.29 
[45] mime_0.12         colorspace_2.1-0  xtable_1.8-4      httpuv_1.6.11    
[49] utf8_1.2.3        lazyeval_0.2.2    munsell_0.5.0     cachem_1.0.8     
[53] crayon_1.5.2  