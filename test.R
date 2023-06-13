attendee <- 50
faculty <- 5

id <- (1:(attendee+faculty))
type <- c(
        rep('attn',attendee),
        rep('fac',faculty)
        
)

t2 <- c(
        rep('intl', round(0.6 * attendee)),
        rep('home',round(0.4 * attendee)),
        rep('intl',round(0.6 * faculty)),
        rep('home',round(0.4 * faculty))
)

df <- data.frame(id,type,t2)


d <- data.frame(
        id = (1:(attn() +fac() )),
        type = c(rep("attn", attn()),rep("fac", fac())),
        travel = c(
                rep(),
                rep()
        )
)

redf <- reactive({
        d1 <- data.frame(
                id = (1:attn()),
                type = rep("attn", attn()),
                travel = c(
                        rep("intl",round(perc_attn_intl * attn())),
                        rep("home",round((100-perc_attn_intl()) * attn()))
                )
        ),
        d2 <- data.frame(
                id = (1:fac()),
                type = rep("fac", fac()),
                travel = c(
                        rep("intl",round(perc_fac_intl * fac())),
                        rep("home",round((100-perc_fac_intl()) * fac()))
                )
        )
        data <- rbind(d1,d2)
        data        
})