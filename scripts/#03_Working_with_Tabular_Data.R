## Data Importation
q_obs <- read.table(file = "D:/CCR_AOS/Wass2sHydro-Training/data/daily_flows_station.csv",
                    header = TRUE,sep = ","
                    )
View(q_obs)

# Summary
summary(q_obs)

# Dimensions
dim(q_obs)

# Convert to date
q_obs$DATE <- as.Date(q_obs$DATE)
summary(q_obs)

## Package Installation
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
?tidyverse
tidyverse_packages(include_self = TRUE)
?mutate
q_obs <- mutate(.data = q_obs,
                YYYY = year(DATE),
                MM = month(DATE),
                DD = day(DATE))

unique(q_obs$YYYY)

q_obs2 <- select(.data = q_obs,YYYY,MM, Qobs)

q_obs3 <- group_by(.data =q_obs2,YYYY,MM )

q_obs_monthly <- summarise(.data = q_obs3,
                           Qobs = mean(Qobs))


q_obs_annual <- summarise(.data =q_obs_monthly,
                          Qobs = mean(Qobs))


## AGGREGATE DATA

q_obs_monthly2 <- group_by(.data =q_obs2,YYYY,MM ) %>%
  summarise(Qobs = mean(Qobs))

q_obs_annual2 <- group_by(.data = q_obs2,YYYY) %>%
  summarise(Qobs = mean(Qobs))

q25 <- quantile(q_obs_annual2$Qobs,probs = 0.25)
q75 <- quantile(q_obs_annual2$Qobs,probs = 0.75)

q <- 45

ifelse(q<q25,"below","normal")
ifelse(q>q25 & q<q75, "normal","above")

round(q75,2)
q <- q25
ifelse(q<q25,"below",ifelse(q>=q25 & q<=q75, "normal","above"))

q_obs_annual3 <- mutate(q_obs_annual2,
                        Hclass =ifelse(Qobs<q25,"below",
                                       ifelse(Qobs>=q25 & Qobs<=q75, "normal","above")) )


