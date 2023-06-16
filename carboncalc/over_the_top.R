#OVer the TOOP



# DOWNLOAD DATA [only need 1 run] -----------------------------------------

#library(anyflights)

#df <- get_flights("JFK",2018)
#to get all from JFK in 2018

#save(df,file= "carboncalc/carboncalc/data/jfk.Rda")
# lets save this as it takes too long to load 


# LOAD  -------------------------------------------------------------------


path = "data/estat_avia_tf_apal_en.csv"

deu <- read.csv(path)