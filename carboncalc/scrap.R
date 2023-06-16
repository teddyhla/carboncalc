attendee <- 50
faculty <- 5
hotels <- c(rep("yes",15),rep("no",40))
travel <- c(rep("intl",5),rep("local",50))

df <- data.frame(
        attendee = attendee,
        faculty = faculty,
        hotels = hotels,
        travel =travel
)

df$travel <- as.factor(df$travel)
df$hotels <- as.factor(df$hotels)

dist_local <- rnorm(dplyr::count(df,travel)[2,2], mean = 50, sd = 10)
dist_intl <- rnorm(dplyr::count(df,travel)[1,2], mean = 500, sd = 50)
# should really be gamma function 
# mean of gamma function is shape / rate 
# therefore is mean is 1000, then , shape = 1000 * rate
# 




df$dist <- ifelse(df$travel == "intl",dist_intl,dist_local)

df$carbon_travel <- ifelse(df$travel == "local",0.03549,0.14062)
df$carbon_accomo <- ifelse(df$hotel == "yes",20.8,0)

df$total_carbon <- (df$carbon_travel * df$dist) + df$carbon_accomo

fig <- ggplot2::ggplot(data = df, aes(x= hotels, y= carbon_accomo, group = travel)) +
        geom_point() + 
        facet_wrap(~travel) 

fig2 <- ggplot2::ggplot(data= df, aes(x= dist, y=carbon_travel,group= travel))+
        geom_col()+
        facet_wrap(~travel)

