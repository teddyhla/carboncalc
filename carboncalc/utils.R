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
        # e = duration, f = no hotel rooms, g = option type
        #varl <- list(a,b,c,d,e,f)
        #varl <- lapply(varl,function(x) ifelse(is.null(x),0,x))
        a <- ifelse(is.null(a),0,a)
        b <- ifelse(is.null(b),0,b)
        c <- ifelse(is.null(c),0,c)
        d <- ifelse(is.null(d),0,d)
        e <- ifelse(is.null(e),0,e)
        f <- ifelse(is.null(f),0,f)
        option <- g
        breakdown <- c("local travel", "intl travel", "hotel stay","home")
        hotelc <- f * e * 10.4 # dur event * no hotel rooms * unit cost
        # local popn = total attendee - intl attendee & total fac - intl fac
        localc <- sumtravel(x= (a - c), y=(b - d), shape= 10, scale= 5, constant = 0.03549)
        intlc <- sumtravel(x = c, y = d , shape =1500, scale = 0.75, constant = 0.14062)
        carbon_values <- c(localc, intlc, hotelc, 0)
        df <- data.frame(option, breakdown, carbon_values)
        df$perc <- round((df$carbon_values/sum(df$carbon_values)*100),2)
        df
}

# custom function that help generate a text 
txrd <- function(df){
        sprintf("%s",round(sum(df()$carbon_values),2))
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
                        legend.text = element_text(family = font, size =11),
                        legend.title = element_blank()
                )
}

###################################
#
#  HTML TEXTS
#
###################################


# HTML text for setting Modal at launch
motxt <- function(){
        p(
                div(HTML(
                        "<ul>
                                <li>First, please set <b>total attendees, total faculty, and event duration</b> as global variables.</li>
                                <li>Then, change the variables in <b>Option A &amp; B </b> to compare possible carbon cost scenarios for your event.</li>
                                <br>
                                <br>
                                <li>As default, 30 attendees, 5 faculty and 2-day course is set with option A as all participants travelling <b><em>internationally</em></b> versus <b><em>locally</em></b> in option B.</li>
                                <li>As you change the variables, graphs will update automatically.</li>
                        </ul>"
                )
                        
                )
        )
}

# HTML text for second modal at user guide tab 
mot2 <- function(){
        p(
                div(HTML(
                        "<ul>
                                <li>Please set global variables of 'total attendees', 'total faculty', and 'event duration'.</li>
                                <li>Then, set 'the number of hotel rooms' and 'number of international attendees/faculty' in Option A &amp; B to allow comparison of carbon costs.</li>
                                <li>The default option is set at 30 attendees, 5 faculty for 2 day course globally with model A where all participants are international versus only 2 in model B.</li>
                                <li>As you change variables, graphs will update automatically.</li>
                        </ul>"
                )
                
                )
        )
}

# for about page
ver1 <- function(){
        p(
                div(HTML("<li>Version 1.0 & License GPL-3</li>
                         <li>Conceived by <a href='https://twitter.com/avkwong?lang=en'>Dr Adrian Wong</a>, <a href='https://twitter.com/m_zawadka?lang=en'>Dr Mateusz Zawadka</a> and, <a href='https://twitter.com/teddyhla?lang=en-GB'> Dr Teddy Tun Win Hla</a></li>
                         <li>Any issues, please contact Dr Teddy Tun Win Hla.</li>
                         <li>Cite this app as: Hla,Teddy Tun Win(2023),<em>Events carbon cost calculator</em> <a href='https://www.github.com/teddyhla/carboncalc'> Github repository </a></li>")    
                )
        )
}

# For assumptions
a1 <- function(){
        p(
                div(HTML("
                        <ol>
	                        <li> <b>Total Number of people in the event</b> : We assumed that in any event, attendees and faculty contributes to majority of the number of people. Event organisers, sponsors are 						    likely to be small and likely to remain constant irregardless of the locality of an event.</li>
	                        <li> <b>Carbon cost of venue, equipments and waste processing</b> : We assumed that these are likely to be similar or constant irregardless of the locality of an event.</li> 
	                        <li> <b>Types of transports</b> : Based on European Union travel data, we assumed that all international travels are via <em>'flights'</em>, travelling in <em>economy class</em> as <em> direct</em> flights <sup>[2]</sup>. Similarly, surface rail remains most common mode of transport for national travel for average miles per person per year <sup>[2,3]</sup>. We therefore assumed that <em>surface rail</em> will be used for local travels. </li>
	                        <li><b>Typical distances</b>: Travel distances and durations follow a <em>gamma distribution</em> <sup>[3,4,5]</sup>. Based on typical flight and rail durations and distances in the European Union, we have modelled a <em>local</em> travel as a gamma distribution with a mean of 75km(in comparison, London to Cambridge or Milan to Lugano or Paris to Giverny). Equally, for <em>international</em> travel, a gamm distribution with a mean of 1000km (in comparison, London to Milan ~ 1200km, Paris to Berlin or Barcelona to Milan) is used.  </li>
	                        <li><b>Carbon cost breakdown</b> : Carbon footprint is accurately subdivided into carbondioxide, methane, nitrousoxide levels <sup>[6]</sup> For ease of use, we have followed a standard reporting of carbondioxide equivalent, an index combining all three components.</li>
	                        <li><b>Same input variables having similar but not identical carbon costs</b> Based on the input variables, the calculator generates a <em>random</em> gamma distribution of travel distances within the set mean. Therefore, output values will have very similar but slightly different results within the standard deviation. (e.g., 50.6 <em>vs</em> 49.7)</li>
	
                        </ol>
                        "))
        )
}

# For references
ref1 <- function(){
        p(
                div(HTML("
                        <ol>
                        	<li> Average CO2 emissions from new cars and new vans increased again in 2019, European Environment Agency. <a href='https://www.eea.europa.eu/highlights/average-co2-emissions-from-new-cars-vans-2019> Source Link</a> Accessed: 3 Aug 2023</li>
                        	<li> UK Government National Travel Survey 2021: Mode share, journey lengths and public transport use. <a href='https://www.gov.uk/government/statistics/national-travel-survey-2021/national-travel-survey-2021-mode-share'> Source Link </a> Accessed: 14 June 2023</li>
                        	<li> Network Manager(2023) EUROCONTROL Data Snapshot, <a href='https://www.eurocontrol.int/our-data'> Source Link </a> Accessed: 15 June 2023 </li>
                        	<li> Pl&ouml;tz P, Jacobsson N, Sprei F(2017) On the distribution of individual daily driving distances. Transporation Research Part B: Methdological 101:213&ndash;2227. <a href='https://doi.org/10.1016/j.tr.b2017.04.008'> Source Link </a> Accessed: 15 June 2023 </li>
                        	<li> Veloso M, Phithakkitnukoon S, Bento C, et al (2011) Exploratory Study of Urban Flow using Taxi Traces, Proceedings of the 2011 international workshop on Trajectory data mining and analysisACM, pp. 23</li>
                        	<li> UK Government,Greenhouse gas reporting: conversion factors 2022. (2022)In: GOV.UK.<a href='https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2022'>Source Link</a> Accessed 20 July 2023</li>
                        	<li> European Union Air Transport statistics, <a href='https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Air_transport_statistics'> Source Link </a> Accessed 4 August 2023</li>
                        </ol>
                         
                         ")
                )
        )
}



#
