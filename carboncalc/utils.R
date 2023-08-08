##### UTILS FILE ####
##### 2 PARTS;1. CUSTOM FUNCTIONS AND 2. HTML TEXTS

#####################################
#
# CUSTOM FUNCTIONS
#
####################################


#custom function for generating reactive ui output based on 1,2,3 vars 
#uvfunc creates a ui elements for two events, taking arguments id, text and total which is from 
# original set variables
uvfunc <- function(id, text, total, val = 1){
        renderUI({
                numericInput(id, label = text, min = 0,max = total,step = 1,value = val)
        })
}

#sumtravel function is a function that takes two values, and generates a gamma distribution
#then it sums all the output and results it as a value.
sumtravel <- function(x, y, shape, scale, constant){
        vals <- rgamma(n = (x+y), shape = shape, scale = scale)
        vals <- vals * constant * 2
        ans <- sum(vals)
        ans
}

#gen function takes 7 arguments and generates a dataframe of variable and it pulls sumtravel function
gen <- function(a, b, c, d, e, f, g){
        # a = total attendee, b = total fac, c = intl attendee , d = intl fac
        # e = duration, f = no hotel rooms, g = model type
        #varl <- list(a,b,c,d,e,f)
        #varl <- lapply(varl,function(x) ifelse(is.null(x),0,x))
        a <- ifelse(is.null(a),0,a)
        b <- ifelse(is.null(b),0,b)
        c <- ifelse(is.null(c),0,c)
        d <- ifelse(is.null(d),0,d)
        e <- ifelse(is.null(e),0,e)
        f <- ifelse(is.null(f),0,f)
        model <- g
        breakdown <- c("local travel", "intl travel", "hotel stay","home")
        hotelc <- f * e * 10.4 # dur event * no hotel rooms * unit cost
        # local popn = total attendee - intl attendee & total fac - intl fac
        localc <- sumtravel(x= (a - c), y=(b - d), shape= 10, scale= 5, constant = 0.03549)
        intlc <- sumtravel(x = c, y = d , shape =1500, scale = 0.75, constant = 0.14062)
        carbon_values <- c(localc, intlc, hotelc, 0)
        df <- data.frame(model, breakdown, carbon_values)
        df$perc <- round((df$carbon_values/sum(df$carbon_values)*100),2)
        df
}

# custom function that help generate a text 
txrd <- function(a, df){
        sprintf("Total carbon cost for %s is %s kilograms of carbondioxide equivalent",a, round(sum(df()$carbon_values),2))
}

#lets define a custom ggplot theme
theme_cc <- function(){
        font <- "Arial"
        theme_minimal() %+replace%
                theme(
                        #panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(
                                family = font,
                                size = 16,
                                face = 'bold',
                                hjust =0 ,
                                vjust = 2,
                        ),
                        axis.title = element_text(
                                family = font,
                                size = 16,
                        ),
                        axis.text.x = element_text(family = font,size = 12,vjust = 2),
                        axis.text.y = element_text(family = font, size=12),
                        legend.text = element_text(family = font, size =11 )
                )
}

###################################
#
#  HTML TEXTS
#
###################################


# HTML text for setting Modal
motxt <- function(){
        p(
                div(HTML(
                        "<ul>
                                <li>First, please set total attendees, total faculty, and event duration as global variables.</li>
                                <br>
                                <br>
                                <li>Then, change the variables in Model A &amp; B to compare possible carbon cost scenario for your event.</li>
                                <br>
                                <br>
                                <li>As default, 30 attendees, 5 faculty for 2 day course is set with model A where all participants are international versus only 2 in model B</li>
                                <li>As you change variables, graphs will update automatically.</li>
                        </ul>"
                )
                        
                )
        )
}

# TEST - not used
htx1 <- function(){
        p("We FUNC that all international attendees are not sharing rooms in a hotel and not staying locally with friends and family.")
}

# for gen citation

cite1 <- function(){
        p(div(HTML("Hla, Teddy Tun Win. (2023) <em>Event carbon cost calculator</em>. Available at: <a href=https://twhla.shinyapps.io/pocus_carbon_footprints_calculator/>source link</a>.")))
}

# For assumptions
a1 <- function(){
        p(
                div(HTML("
                        <p><em>1. <strong>Total number of people in the event</strong></em></p>
                        <p>We assumed that an events is predominantly made up of attendees and faculty. Event organisors, sponsors are not included in the model.&nbsp;</p>
                        <p><em><strong>2. Carbon cost of venue, equipment, waste processing</strong></em></p>
                        <p>Given the carbon costs of venue, equipment and waste processing are likely to be identical irregardless of local / international attendees, we have not calculated seperately.&nbsp;</p>
                        <p><em><strong>3. Type of transports</strong></em></p>
                        <p>Based on European Union travel data, we assumed that all international travels are via 'flights' , travelling in 'economy class' as direct flights [2].</p>
                        <p>Similarly, surface rail remains the most common mode of transport for average miles travelled per person per year [2,3]. Therefore, we assumed that 'surface rail' will be used for all local travel.&nbsp;</p>
                        <p><em><strong>4. Typical distances</strong></em></p>
                        <p>Travel distances and durations generally follow a 'gamma' distribution [3,4,5]. Based on typical flight and rail durations and distances in the European Union, we have modelled a local travel as a gamma distribution of distances with mean of 75km (in comaparison, London to Cambridge ~ 79km). Equally, for internaitonal travel, a gamma distribution of mean 1000km (in comparison, London to Milan ~ 1200km) is used.</p>
                        <p><em><strong>5. Carbon cost breakdown</strong></em></p>
                        <p>Carbon footprint is more accurately sub-divided into carbondioxide, methane, nitrousoxide levels [6]. For ease of use, we have followed a standard reporting of carbondioxide equivalent, an index combining all three components.</p>
                        <p><em><strong>6. Same event variables having a different total value</strong></em></p>
                        <p>Based on number of local / non-local attendees, a gamma distribution of travel distances with set mean is sampled. Therefore, the same event with same variables will have very similar but slightly different results. e.g., 50.6 vs. 49.7.</p>
                        "))
        )
}

# For references
ref1 <- function(){
        p(
                div(HTML(
                        "<p>1. Average CO2 emissions from new cars and new vans increased again in 2019; European Environment Agency. <a href=https://www.eea.europa.eu/highlights/average-co2-emissions-from-new-cars-vans-2019>https://www.eea.europa.eu/highlights/average-co2-emissions-from-new-cars-vans-2019</a>. Accessed: 3 Aug 2023</p>
                        <br>
                        <p>2. UK Government National Travel Survey 2021: Mode share, journey lengths and public transport use. <a href=https://www.gov.uk/government/statistics/national-travel-survey-2021/national-travel-survey-2021-mode-share-journey-lengths-and-public-transport-use> https://www.gov.uk/government/statistics/national-travel-survey-2021/national-travel-survey-2021-mode-share-journey-lengths-and-public-transport-use</a>. Accessed: 14 Jun 2023</p>
                        <br>
                        <p>3. Network Manager (2023) EUROCONTROL Data Snapshot, <a href=https://www.eurocontrol.int/our-data>https://www.eurocontrol.int/our-data</a>;. Accessed: 10 July 2023</p>
                        <br>
                        <p>4. Pl&ouml;tz P, Jakobsson N, Sprei F (2017) On the distribution of individual daily driving distances. Transportation Research Part B: Methodological 101:213&ndash;227. <a href=https://doi.org/10.1016/j.trb.2017.04.008>https://doi.org/10.1016/j.trb.2017.04.008 </a></p>
                        <br>
                        <p>5. Veloso M, Phithakkitnukoon S, Bento C, et al (2011) Exploratory Study of Urban Flow using Taxi Traces,&nbsp;<em>Proceedings of the 2011 international workshop on Trajectory data mining and analysisACM</em><span>, pp. 23</span></p>
                        <br>
                        <p><span>6. UK Government,</span>Greenhouse gas reporting: conversion factors 2022. (2022)In: GOV.UK. <a href=https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022>https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022</a>. Accessed 20 July 2023</p>
                        <br>
                        <p>7. European Union Air transport statistics. <a href=https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Air_transport_statistics> Link </a>. Accessed 4 Aug 2023. </p>
                        "
                        
                ))
        )
}
