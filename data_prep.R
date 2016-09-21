library(readxl)
library(dplyr)

wages2015 <- read_excel("data/state_M2015_dl.xlsx", sheet=1)
value100 <- read.csv("data/valueof100.csv", stringsAsFactors=F)
value100$per_change <- (value100$value100-100)/100

wages2015 <- left_join(wages2015, value100)

wages2015$H_MEAN <- as.numeric(wages2015$H_MEAN)
wages2015$H_MEDIAN <- as.numeric(wages2015$H_MEDIAN)
wages2015$A_MEAN <- as.numeric(wages2015$A_MEAN)
wages2015$A_MEDIAN <- as.numeric(wages2015$A_MEDIAN)

wages2015$adjusted_H_MEAN <- wages2015$H_MEAN * wages2015$per_change + wages2015$H_MEAN
wages2015$adjusted_H_MEDIAN <-wages2015$H_MEDIAN * wages2015$per_change + wages2015$H_MEDIAN

wages2015$adjusted_A_MEAN <- wages2015$A_MEAN * wages2015$per_change + wages2015$A_MEAN
wages2015$adjusted_A_MEDIAN <-wages2015$A_MEDIAN * wages2015$per_change + wages2015$A_MEDIAN

wages2015 <- filter(wages2015, STATE!="Guam" & STATE!="Puerto Rico" & STATE!= "Virgin Islands")

wages2015a <- subset(wages2015, !is.na(A_MEAN))
wages2015b <- subset(wages2015, !is.na(A_MEDIAN))

wages2015a_raw <-  wages2015a %>%
  arrange(OCC_TITLE, -A_MEAN) %>%
  group_by(OCC_TITLE) %>%
  mutate(rank_A_MEAN=row_number()) %>%
  select(STATE, OCC_TITLE, rank_A_MEAN)


wages2015a_adjusted <-  wages2015a %>%
  arrange(OCC_TITLE, -adjusted_A_MEAN) %>%
  group_by(OCC_TITLE) %>%
  mutate(rank_adjusted_A_MEAN=row_number()) %>%
  select(STATE, OCC_TITLE, rank_adjusted_A_MEAN)

wages2015b_raw <-  wages2015b %>%
  arrange(OCC_TITLE, -A_MEDIAN) %>%
  group_by(OCC_TITLE) %>%
  mutate(rank_A_MEDIAN=row_number()) %>%
  select(STATE, OCC_TITLE, rank_A_MEDIAN)


wages2015b_adjusted <-  wages2015b %>%
  arrange(OCC_TITLE, -adjusted_A_MEDIAN) %>%
  group_by(OCC_TITLE) %>%
  mutate(rank_adjusted_A_MEDIAN=row_number()) %>%
  select(STATE, OCC_TITLE, rank_adjusted_A_MEDIAN)

wages2015 <- left_join(wages2015, wages2015a_raw)
wages2015 <- left_join(wages2015, wages2015a_adjusted)
wages2015 <- left_join(wages2015, wages2015b_raw)
wages2015 <- left_join(wages2015, wages2015b_adjusted)

wages2015$YEAR <- 2015

wages2015 <- wages2015[c("STATE", "OCC_TITLE", "YEAR", "rank_A_MEAN", "rank_adjusted_A_MEAN", "rank_A_MEDIAN", "rank_adjusted_A_MEDIAN")]



write.csv(wages2015, "data/adjusted_2015.csv")
