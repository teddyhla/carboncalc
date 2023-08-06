
#custom function for generating reactive ui output based on 1,2,3 vars 
#uvfunc creates a ui elements for two events, taking arguments id, text and total which is from 
# original set variables
uvfunc <- function(id, text, total){
        renderUI({
                numericInput(id, label = text, min = 0,max = total,step = 1,value = 1)
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

motxt <- function(){
        p(
                div(HTML(
                        "First, please set total attendees, total faculty, and event duration of your event.
                        <br>
                        <br>
                        Then, set the variables in Model 1 & 2 to compare possible carbon cost scenario for your event.
                        <br>
                        <br>
                        As you change variables, graphs will update automatically."
                )
                        
                )
        )
}

htx1 <- function(){
        p("We FUNC that all international attendees are not sharing rooms in a hotel and not staying locally with friends and family.")
}

cite1 <- function(){
        p(div(HTML("Hla, T.T.W. (2023) <em>Event carbon cost calculator</em>. Available at: <a href=https://twhla.shinyapps.io/pocus_carbon_footprints_calculator/>source link</a> (Accessed: 06 August 2023).")))
}

a1 <- function(){
        p(
                div(HTML("
                        <p><em>1. <strong>Total number of people in the event</strong></em></p>
                        <p>We assumed that an events is predominantly made up of attendees and faculty. Event organisors, sponsors are not included in the model.&nbsp;</p>
                        <p><em><strong>2. Carbon cost of venue, equipment, waste processing</strong></em></p>
                        <p>Given the carbon costs of venue, equipment and waste processing are likely to be identical irregardless of local / international attendees, we have not calculated seperately.&nbsp;</p>
                        <p><em><strong>3. Type of transports</strong></em></p>
                        <p>Based on European Union travel data, we assumed that all international travels are via 'flights' , travelling in 'economy class' as direct flights.</p>
                        <p>Similarly, surface rail remains the most common mode of transport for average miles travelled per person per year. Therefore, we assumed that 'surface rail' will be used for all local travel.&nbsp;</p>
                        <p><em><strong>4. Typical distances</strong></em></p>
                        <p>Travel distances and durations generally follow a 'gamma' distribution. Based on typical flight and rail durations and distances in the European Union, we have modelled a local travel as a gamma distribution of distances with mean of 75km. Equally, for internaitonal travel, a gamma distribution of mean 1000km is used.</p>
                        <p><em><strong>5. Carbon cost breakdown</strong></em></p>
                        <p>Carbon footprint is more accurately sub-divided into carbondioxide, methane, nitrousoxide levels. For ease of use, we have followed a standard reporting of carbondioxide equivalent, an index combining all three components.</p>
                        <p><em><strong>6. Same event variables having a different total value</strong></em></p>
                        <p>Based on number of local / non-local attendees, a gamma distribution of travel distances with set mean is sampled. Therefore, the same event with same variables will have very similar but slightly different results. e.g., 50.6 vs. 49.7.</p>
                        "))
        )
}
# 
# h4("Carbon cost of venue & equipment"),
# p("We have not calculated the carbon cost of catering, equipments transport, waste and venue set up. This is because these costs are likely to be similar for local / non-local events."),
# h4("Type of transports"),
# p("Based on EU travel data, we assumed that all international travels are via flights, travelling in 'economy class' as direct flights."),
# p("Based on UK National Travel Survey[1], surface rail remains the most common mode of transport for average miles travelled per person per year. Therefore, we assumed that 'surface rail' will be used for local travel. Based on average travel durations in the UK and European Union, for local travel we have assumed a 75km travel distance and for international air travel we have assumed a 1000 km distance."),
# h4("Breakdown of carbon cost"),
# p("Carbon foot print is more accurately subdivided into carbon dioxide, methane, and nitrous oxide levels. For parsimony, we have reported a total equivalent carbon dioxide as a single value."),
# h4("Typical journey"),
# p("We assumed that all travel distance to venue comprises of a return journey and follows a gamma distribution [2].For 'typical' local courses, we have modelled using a mean distance of 75km (approximately 1 hour 15 mins surface rail journey time) as a maximum upperlimit of acceptable commute."),
# p("Based on Eurocontrol, an average flight distance travelled in European Union is 981km and as a result, we have used as a 1000 km as default for international flight [3]. "),

