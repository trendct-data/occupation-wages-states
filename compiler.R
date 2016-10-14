library(stringr)
library(lubridate)
library(readxl)
library(dplyr)
# 
# w1997 <-read_excel("data/state_M1997_dl.xls", sheet=1, skip=39)
# 
# colnames(w1997) <- w1997[1,]
# w1997 <- w1997[2:65496,]
# w1997 <- subset(w1997, !is.na(state))
# 
# colnames(w1997) <- str_to_upper(colnames(w1997))
# colnames(w1997) <- gsub("WPCT", "PCT", colnames(w1997))
# colnames(w1997)[colnames(w1997) == 'GROUP'] <- 'OCC_GROUP'
# colnames(w1997)[colnames(w1997) == 'OCC_TITL'] <- 'OCC_TITLE'
# w1997$JOBS_1000 <- 0
# w1997$JOBS_1000 [w1997$JOBS_1000 ==0] <- NA
# w1997$LOC_Q <- 0
# w1997$LOC_Q [w1997$LOC_Q ==0] <- NA
# w1997$ANNUAL <- ""
# w1997$ANNUAL [w1997$ANNUAL ==""] <- NA
# w1997$HOURLY <- ""
# w1997$HOURLY [w1997$HOURLY ==""] <- NA
# w1997$YEAR <- 1997
# w1997 <- w1997[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
#                  "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
#                  "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
#                  "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]
# 
# w1998 <-read_excel("data/state_M1998_dl.xls", sheet=1, skip=40)
# 
# colnames(w1998) <- w1998[1,]
# w1998 <- w1998[2:35701,]
# w1998 <- subset(w1998, !is.na(state))
# colnames(w1998) <- str_to_upper(colnames(w1998))
# colnames(w1998) <- gsub("WPCT", "PCT", colnames(w1998))
# colnames(w1998)[colnames(w1998) == 'GROUP'] <- 'OCC_GROUP'
# colnames(w1998)[colnames(w1998) == 'OCC_TITL'] <- 'OCC_TITLE'
# w1998$JOBS_1000 <- 0
# w1998$JOBS_1000 [w1998$JOBS_1000 ==0] <- NA
# w1998$LOC_Q <- 0
# w1998$LOC_Q [w1998$LOC_Q ==0] <- NA
# w1998$ANNUAL <- ""
# w1998$ANNUAL [w1998$ANNUAL ==""] <- NA
# w1998$HOURLY <- ""
# w1998$HOURLY [w1998$HOURLY ==""] <- NA
# w1998$YEAR <- 1998
# w1998 <- w1998[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
#                  "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
#                  "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
#                  "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w1999 <-read_excel("data/state_M1999_dl.xls", sheet=1, skip=42)

colnames(w1999) <- w1999[1,]
w1999 <- w1999[2:30448,]
w1999 <- subset(w1999, !is.na(state))
colnames(w1999) <- str_to_upper(colnames(w1999))
colnames(w1999) <- gsub("WPCT", "PCT", colnames(w1999))
colnames(w1999)[colnames(w1999) == 'GROUP'] <- 'OCC_GROUP'
colnames(w1999)[colnames(w1999) == 'OCC_TITL'] <- 'OCC_TITLE'
w1999$JOBS_1000 <- 0
w1999$JOBS_1000 [w1999$JOBS_1000 ==0] <- NA
w1999$LOC_Q <- 0
w1999$LOC_Q [w1999$LOC_Q ==0] <- NA
w1999$ANNUAL <- ""
w1999$ANNUAL [w1999$ANNUAL ==""] <- NA
w1999$HOURLY <- ""
w1999$HOURLY [w1999$HOURLY ==""] <- NA
w1999$YEAR <- 1999
w1999 <- w1999[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2000 <-read_excel("data/state_M2000_dl.xls", sheet=1, skip=41)
colnames(w2000) <- w2000[1,]
w2000 <- subset(w2000, !is.na(state))
colnames(w2000) <- str_to_upper(colnames(w2000))
colnames(w2000) <- gsub("WPCT", "PCT", colnames(w2000))
colnames(w2000)[colnames(w2000) == 'GROUP'] <- 'OCC_GROUP'
colnames(w2000)[colnames(w2000) == 'OCC_TITL'] <- 'OCC_TITLE'
w2000$JOBS_1000 <- 0
w2000$JOBS_1000 [w2000$JOBS_1000 ==0] <- NA
w2000$LOC_Q <- 0
w2000$LOC_Q [w2000$LOC_Q ==0] <- NA
w2000$ANNUAL <- ""
w2000$ANNUAL [w2000$ANNUAL ==""] <- NA
w2000$HOURLY <- ""
w2000$HOURLY [w2000$HOURLY ==""] <- NA
w2000$YEAR <- 2000
w2000 <- w2000[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]


w2001 <-read_excel("data/state_M2001_dl.xls", sheet=1)
w2001 <- subset(w2001, !is.na(state))
colnames(w2001) <- str_to_upper(colnames(w2001))
colnames(w2001) <- gsub("WPCT", "PCT", colnames(w2001))
colnames(w2001)[colnames(w2001) == 'GROUP'] <- 'OCC_GROUP'
w2001$JOBS_1000 <- 0
w2001$JOBS_1000 [w2001$JOBS_1000 ==0] <- NA
w2001$LOC_Q <- 0
w2001$LOC_Q [w2001$LOC_Q ==0] <- NA

w2001$HOURLY <- ""
w2001$HOURLY [w2001$HOURLY ==""] <- NA
w2001$YEAR <- 2001
w2001 <- w2001[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2002 <-read_excel("data/state_M2002_dl.xls", sheet=1)
w2002 <- subset(w2002, !is.na(state))
colnames(w2002) <- str_to_upper(colnames(w2002))
colnames(w2002) <- gsub("WPCT", "PCT", colnames(w2002))
colnames(w2002)[colnames(w2002) == 'GROUP'] <- 'OCC_GROUP'
w2002$JOBS_1000 <- 0
w2002$JOBS_1000 [w2002$JOBS_1000 ==0] <- NA
w2002$LOC_Q <- 0
w2002$LOC_Q [w2002$LOC_Q ==0] <- NA
w2002$ANNUAL <- ""
w2002$ANNUAL [w2002$ANNUAL ==""] <- NA
w2002$HOURLY <- ""
w2002$HOURLY [w2002$HOURLY ==""] <- NA
w2002$YEAR <- 2002
w2002 <- w2002[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2003 <-read_excel("data/state_M2003_dl.xls", sheet=1)
colnames(w2003)[colnames(w2003) == 'GROUP'] <- 'OCC_GROUP'
w2003$JOBS_1000 <- 0
w2003$JOBS_1000 [w2003$JOBS_1000 ==0] <- NA
w2003$LOC_Q <- 0
w2003$LOC_Q [w2003$LOC_Q ==0] <- NA
w2003$ANNUAL <- ""
w2003$ANNUAL [w2003$ANNUAL ==""] <- NA
w2003$HOURLY <- ""
w2003$HOURLY [w2003$HOURLY ==""] <- NA
w2003$YEAR <- 2003
w2003 <- w2003[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2004 <-read_excel("data/state_M2004_dl.xls", sheet=1)
colnames(w2004)[colnames(w2004) == 'GROUP'] <- 'OCC_GROUP'
w2004$JOBS_1000 <- 0
w2004$JOBS_1000 [w2004$JOBS_1000 ==0] <- NA
w2004$LOC_Q <- 0
w2004$LOC_Q [w2004$LOC_Q ==0] <- NA
w2004$YEAR <- 2004
w2004 <- w2004[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2005 <-read_excel("data/state_M2005_dl.xls", sheet=1)
colnames(w2005)[colnames(w2005) == 'GROUP'] <- 'OCC_GROUP'
w2005$JOBS_1000 <- 0
w2005$JOBS_1000 [w2005$JOBS_1000 ==0] <- NA
w2005$LOC_Q <- 0
w2005$LOC_Q [w2005$LOC_Q ==0] <- NA
w2005$YEAR <- 2005
w2005 <- w2005[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2006 <-read_excel("data/state_M2006_dl.xls", sheet=1)
colnames(w2006)[colnames(w2006) == 'GROUP'] <- 'OCC_GROUP'
w2006$JOBS_1000 <- 0
w2006$JOBS_1000 [w2006$JOBS_1000 ==0] <- NA
w2006$LOC_Q <- 0
w2006$LOC_Q [w2006$LOC_Q ==0] <- NA
w2006$YEAR <- 2006
w2006 <- w2006[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2007 <-read_excel("data/state_M2007_dl.xls", sheet=1)
colnames(w2007)[colnames(w2007) == 'GROUP'] <- 'OCC_GROUP'
w2007$JOBS_1000 <- 0
w2007$JOBS_1000 [w2007$JOBS_1000 ==0] <- NA
w2007$LOC_Q <- 0
w2007$LOC_Q [w2007$LOC_Q ==0] <- NA
w2007$YEAR <- 2007
w2007 <- w2007[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]


w2008 <-read_excel("data/state_M2008_dl.xls", sheet=1)
w2008 <- subset(w2008, !is.na(state))
colnames(w2008)[colnames(w2008) == 'GROUP'] <- 'OCC_GROUP'
w2008$JOBS_1000 <- 0
w2008$JOBS_1000 [w2008$JOBS_1000 ==0] <- NA
w2008$LOC_Q <- 0
w2008$LOC_Q [w2008$LOC_Q ==0] <- NA
w2008$YEAR <- 2008
w2008 <- w2008[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                 "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                 "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                 "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]


w2009 <-read_excel("data/state_M2009_dl.xls", sheet=1)
colnames(w2009)[colnames(w2009) == 'GROUP'] <- 'OCC_GROUP'
w2009$LOC_Q <- 0
w2009$LOC_Q [w2009$LOC_Q ==0] <- NA
w2009$YEAR <- 2009
w2009 <- w2009[c("AREA", "ST", "STATE", "OCC_CODE", "OCC_TITLE", "OCC_GROUP", "TOT_EMP",  
                  "EMP_PRSE","JOBS_1000","LOC_Q","H_MEAN","A_MEAN","MEAN_PRSE","H_PCT10",  
                  "H_PCT25","H_MEDIAN","H_PCT75","H_PCT90","A_PCT10","A_PCT25","A_MEDIAN", 
                  "A_PCT75","A_PCT90","ANNUAL","HOURLY","YEAR")]

w2010 <-read_excel("data/state_M2010_dl.xls", sheet=1)
w2010$YEAR <- 2010
colnames(w2010)[colnames(w2010) == 'GROUP'] <- 'OCC_GROUP'
colnames(w2010)[colnames(w2010) == 'LOC QUOTIENT'] <- 'LOC_Q'

w2011 <-read_excel("data/state_M2011_dl.xls", sheet=1)
w2011$YEAR <- 2011
colnames(w2011)[colnames(w2011) == 'GROUP'] <- 'OCC_GROUP'

w2012 <-read_excel("data/state_M2012_dl.xls", sheet=1)
w2012$YEAR <- 2012

w2013 <-read_excel("data/state_M2013_dl.xls", sheet=1)
w2013$YEAR <- 2013

w2014 <-read_excel("data/state_M2014_dl.xlsx", sheet=1)
w2014$YEAR <- 2014

w2015 <-read_excel("data/state_M2015_dl.xlsx", sheet=1)
w2015$YEAR <- 2015

mega <- rbind(w2015, w2014, w2013, w2012, w2011, w2010, w2009, w2008, w2007,
              w2006, w2005, w2004, w2003, w2002, w2001, w2000, w1999)

library(blscrapeR)
df <- bls_api("CUSR0000SA0")
head(df)

df <- inflation_adjust(2015)
df$year <- year(df$date)
df <- df[c("adj_value", "pct_increase", "year")]
colnames(df) <- c("adj_value", "pct_increase", "YEAR")
mega <- left_join(mega, df)

mega$H_MEAN <- as.numeric(mega$H_MEAN)
mega$adjusted_H_MEAN <- mega$H_MEAN/mega$adj_value
mega$H_MEDIAN <- as.numeric(mega$H_MEDIAN)
mega$adjusted_H_MEDIAN <- mega$H_MEDIAN/mega$adj_value

mega$A_MEAN <- as.numeric(mega$A_MEAN)
mega$adjusted_A_MEAN <- mega$A_MEAN/mega$adj_value
mega$A_MEDIAN <- as.numeric(mega$A_MEDIAN)
mega$adjusted_A_MEDIAN <- mega$A_MEDIAN/mega$adj_value



mega <- data.frame(mega)


for (i in 7:32) {
  mega[,i] <- as.numeric(mega[,i])
}

mega$ANNUAL <- NULL
mega$HOURLY <- NULL


wages2015 <- read_excel("data/state_M2015_dl.xlsx", sheet=1)
wages2015 <- left_join(wages2015, value100)
#wages2015 <- left_join(wages2015, value100s)

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
mega$OCC_GROUP <- NULL



wages2015 <- wages2015[c("STATE", "OCC_TITLE", "YEAR", "rank_A_MEAN", "rank_adjusted_A_MEAN", "rank_A_MEDIAN", "rank_adjusted_A_MEDIAN")]

mega <- left_join(mega, wages2015)

w2015_groups <- w2015[c("OCC_CODE", "OCC_GROUP")]
w2014_groups <- w2014[c("OCC_CODE", "OCC_GROUP")]
w2013_groups <- w2013[c("OCC_CODE", "OCC_GROUP")]
w2012_groups <- w2012[c("OCC_CODE", "OCC_GROUP")]
w2011_groups <- w2011[c("OCC_CODE", "OCC_GROUP")]
w2010_groups <- w2010[c("OCC_CODE", "OCC_GROUP")]
w2009_groups <- w2009[c("OCC_CODE", "OCC_GROUP")]
w2008_groups <- w2008[c("OCC_CODE", "OCC_GROUP")]
w2007_groups <- w2007[c("OCC_CODE", "OCC_GROUP")]
w2006_groups <- w2006[c("OCC_CODE", "OCC_GROUP")]
w2005_groups <- w2005[c("OCC_CODE", "OCC_GROUP")]
w2004_groups <- w2004[c("OCC_CODE", "OCC_GROUP")]
w2003_groups <- w2003[c("OCC_CODE", "OCC_GROUP")]
w2002_groups <- w2002[c("OCC_CODE", "OCC_GROUP")]
w2001_groups <- w2001[c("OCC_CODE", "OCC_GROUP")]
w2000_groups <- w2000[c("OCC_CODE", "OCC_GROUP")]
w1999_groups <- w1999[c("OCC_CODE", "OCC_GROUP")]

groups <- rbind(w1999_groups, w2000_groups, w2001_groups, w2002_groups, w2003_groups,
                w2004_groups, w2005_groups, w2006_groups, w2007_groups, w2008_groups,
                w2009_groups, w2010_groups, w2011_groups, w2012_groups, w2013_groups,
                w2014_groups, w2015_groups)

groups <- subset(groups, !is.na(OCC_GROUP))
groups <- unique(groups)
groups <- subset(groups, OCC_GROUP!="group")
groups$blah <- paste(groups$OCC_CODE, groups$OCC_GROUP)
groups <- subset(groups, blah!="00-0000 major")
groups$blah <- NULL

mega <- filter(mega, STATE!="Guam" & STATE!="Puerto Rico" & STATE!= "Virgin Islands")

mega <- left_join(mega, groups)
mega2 <- unique(mega)
mega$OCC_GROUP <- ifelse( (is.na(mega$OCC_GROUP) & grepl("0000", mega$OCC_CODE)), "major", mega$OCC_GROUP) 
mega$OCC_GROUP <- ifelse( (is.na(mega$OCC_GROUP) & !grepl("0000", mega$OCC_CODE)), "detailed", mega$OCC_GROUP) 
#mega$OCC_TITLE <- str_to_title(mega$OCC_TITLE)

# breaking apart the dataframe to rebuild with adjusted figures

value100 <- read.csv("data/valueof100.csv", stringsAsFactors=F)
value100$per_change <- (value100$value100-100)/100
value100s <- read.csv("data/price_parities.csv", stringsAsFactors=F)
value100s$y2008_diff <- 100+((100-value100s$y2008)/value100s$y2008*100)
value100s$y2009_diff <- 100+((100-value100s$y2009)/value100s$y2009*100)
value100s$y2010_diff <- 100+((100-value100s$y2010)/value100s$y2010*100)
value100s$y2011_diff <- 100+((100-value100s$y2011)/value100s$y2011*100)
value100s$y2012_diff <- 100+((100-value100s$y2012)/value100s$y2012*100)
value100s$y2013_diff <- 100+((100-value100s$y2013)/value100s$y2013*100)
value100s$y2014_diff <- 100+((100-value100s$y2014)/value100s$y2014*100)
value100s$y2008_pc <- (value100s$y2008_diff-100)/100
value100s$y2009_pc <- (value100s$y2009_diff-100)/100
value100s$y2010_pc <- (value100s$y2010_diff-100)/100
value100s$y2011_pc <- (value100s$y2011_diff-100)/100
value100s$y2012_pc <- (value100s$y2012_diff-100)/100
value100s$y2013_pc <- (value100s$y2013_diff-100)/100
value100s$y2014_pc <- (value100s$y2014_diff-100)/100

mega <- left_join(mega, value100s)
#mega$backup <- ifelse(mega$adjusted_A_MEDIAN==-1, "blah", "nope")

mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR>=2014, mega$adjusted_A_MEDIAN*mega$y2014_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR==2013, mega$adjusted_A_MEDIAN*mega$y2013_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR==2012, mega$adjusted_A_MEDIAN*mega$y2012_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR==2011, mega$adjusted_A_MEDIAN*mega$y2011_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR==2010, mega$adjusted_A_MEDIAN*mega$y2010_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR==2009, mega$adjusted_A_MEDIAN*mega$y2009_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)
mega$adjusted_A_MEDIAN <- ifelse(mega$YEAR<=2008, mega$adjusted_A_MEDIAN*mega$y2008_pc+mega$adjusted_A_MEDIAN, mega$adjusted_A_MEDIAN)

mega$adjusted_A_MEAN <- ifelse(mega$YEAR>=2014, mega$adjusted_A_MEAN*mega$y2014_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR==2013, mega$adjusted_A_MEAN*mega$y2013_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR==2012, mega$adjusted_A_MEAN*mega$y2012_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR==2011, mega$adjusted_A_MEAN*mega$y2011_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR==2010, mega$adjusted_A_MEAN*mega$y2010_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR==2009, mega$adjusted_A_MEAN*mega$y2009_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
mega$adjusted_A_MEAN <- ifelse(mega$YEAR<=2008, mega$adjusted_A_MEAN*mega$y2008_pc+mega$adjusted_A_MEAN, mega$adjusted_A_MEAN)
  # 
# just2015 <- filter(mega, YEAR==2015)
# just2014 <- filter(mega, YEAR==2014)
# just2013 <- filter(mega, YEAR==2013)
# just2012 <- filter(mega, YEAR==2012)
# just2011 <- filter(mega, YEAR==2011)
# just2010 <- filter(mega, YEAR==2010)
# just2009 <- filter(mega, YEAR==2009)
# just2008 <- filter(mega, YEAR<=2008)
# 
# just2015 <- left_join(values)
# 
# mega <- rbind(just2008, just2009)
# mega <- rbind(mega, just2010)
# mega <- rbind(mega, just2011)
# mega <- rbind(mega, just2012)
# mega <- rbind(mega, just2013)
# mega <- rbind(mega, just2014)
# mega <- rbind(mega, just2015)

#write.table(mega, "mega_wages.csv", na="")
mega <- mega[,1:34]
write.csv(mega, "mega_wages2.csv")


flat <- readLines("mega_wages.txt")
flat <- gsub(",NA", ",-1", flat)
write(flat, "mega_wages.csv")
#  TITLES


titles <- rbind(w1999_titles, w2000_titles, w2001_titles, w2002_titles, w2003_titles,
                w2004_titles, w2005_titles, w2006_titles, w2007_titles, w2008_titles,
                w2009_titles, w2010_titles, w2011_titles, w2012_titles, w2013_titles,
                w2014_titles, w2015_titles)

titles <- subset(titles, !is.na(OCC_TITLE))
titles <- unique(titles)
titles <- titles %>% 
  group_by(OCC_CODE) %>%
  filter(row_number()==1) %>%
  filter(OCC_CODE!="occ_code")

titles$OCC_TITLE <-   str_to_lower(titles$OCC_TITLE)
titles$OCC_TITLE <- paste(toupper(substr(titles$OCC_TITLE, 1, 1)), substr(titles$OCC_TITLE, 2, nchar(titles$OCC_TITLE)), sep="")

write.csv(titles, "titles.csv")
