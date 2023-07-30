
# explore gamma distributions with certain means --------------------------


alpha <- 1500.0
theta <- 0.75

g <- rgamma(n = 10000, shape = alpha, scale = theta)
hist(g)
mean(g)

#the combo that gives a mean of 50km distance is alpha 10, theta 5
# the combo that gives a mean of 1125 distance is alpha 1500, theta 0.75 


# make a dataframe of similar type to allow viz ---------------------------


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

dist_local <- rgamma(dplyr::count(df,travel)[2,2], shape  = 10, scale  = 5)
dist_intl <- rgamma(dplyr::count(df,travel)[1,2], shape = 1500, scale = 0.75)
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


# plot dougnut chart ------------------------------------------------------

data <- data.frame(
        category = c(rep("local",5), rep("nonlocal",20), rep("home",3), rep("hotel",17)),
        carbon = rnorm(45, mean = 50, sd = 2)
)

data <- aggregate(data$carbon, by = list(data$category), FUN = sum)
names(data) <- c("category","carbon")

data$frac <- data$carbon / sum(data$carbon)
data$ymax <- cumsum(data$frac)
data$ymin <- c(0,head(data$ymax, n =-1))
data$labelposition <- (data$ymax + data$ymin)/2

#data$label <- paste0 (data$category , "\n value: ", data$carbon)
#data$labelPosition <- (data$ymax + data$ymin) / 2 
library(ggplot2)

ggplot(data, aes(ymax = ymax, ymin = ymin, xmax =4, xmin = 3, fill = category))+
        geom_rect() +
        #geom_label( x = 3.5, aes( y = labelPosition, label = label),size = 6) +
        scale_fill_brewer(palette = 4) +
        geom_text(x=2 ,aes(y = labelposition, label = frac)) +
        coord_polar( theta = "y") + 
        xlim( c(2,4)) +
        theme_void()

waffle::waffle(vals) + ggthemes::scale_fill_tableau(name = NULL)
