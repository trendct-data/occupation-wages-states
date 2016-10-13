overtime <- readRDS("data/mega_wages.rds")
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
wages2015 <- read_excel("data/state_M2015_dl.xlsx", sheet=1)
value100 <- read.csv("data/valueof100.csv", stringsAsFactors=F)
value100$per_change <- (value100$value100-100)/100
wages2015$OCC_TITLE <- str_to_lower(wages2015$OCC_TITLE)
wages2015$OCC_TITLE <- gsub(", general", "", wages2015$OCC_TITLE)

#wages2015$OCC_TITLE <- paste(toupper(substr(wages2015$OCC_TITLE, 1, 1)), substr(wages2015$OCC_TITLE, 2, nchar(wages2015$OCC_TITLE)), sep="")

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

#mega <- read.csv("mega_wages.csv", stringsAsFactors=F)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Would your job make you better or worse off in another state?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("STATE", label = h3("Select state"), 
                  unique(mega$STATE), selected = unique(mega$STATE[1])),
      selectInput("OCCUPATION", label = h3("Select occupation"), 
                  unique(mega$OCC_TITLE), selected = unique(mega$OCC_TITLE[1]))
    ),
    mainPanel(
      h3("Explore how the value of your pay varies depending on where you live"),
      h2("Interactive goes here"),
      p("Would you have more purchasing power doing your job where you currently reside or would you be better off in another state, after adjusting for the cost of living?"),

      p("What someone earns varies with factors such as industry, geography, and worker skill. Jobs with more consistent tasks, such as waiters or retail workers, have more consistent wages state to state."),
        
      p("But the larger the variation within a particular occupation, particularly with healthcare, management, and the arts, the more the pay can vary. We looked at the median annual salary for jobs in every state and adjusted it for cost of living."),
        
      p("The value of a dollar differs state to state. Prices for some things can be cheaper or more expensive depending on where someone lives. For example, $103.52 in Texas actually has the same purchasing power as $102.99 in Maine, according to the Bureau of Economic Analysis."),
        
      textOutput("text1"),
      textOutput("text2")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  state_abbr<-input$STATE
  state_occ <-input$OCCUPATION
  
  state_wages <- subset(wages2015, ST==state_abbr)
  state_name <- unique(state_wages$STATE)
  total_emp <-  subset(state_wages, OCC_TITLE=="All Occupations")
  total_emp <- as.numeric(total_emp$TOT_EMP)
  total_emp <- round(total_emp/1000000,1)
  
  state_wages_median <- state_wages %>%
    arrange(A_MEDIAN)
  state_wages_median <- subset(state_wages_median, !is.na(A_MEDIAN))
  
  state_wages_mean <- state_wages %>%
    arrange(A_MEAN)
  state_wages_mean <- subset(state_wages_mean, !is.na(A_MEAN))
  
  state_wages_median_adjusted <- state_wages %>%
    arrange(adjusted_A_MEDIAN)
  state_wages_median <- subset(state_wages_median, !is.na(adjusted_A_MEDIAN))
  
  state_wages_mean_adjusted <- state_wages %>%
    arrange(adjusted_A_MEAN)
  state_wages_mean <- subset(state_wages_mean, !is.na(adjusted_A_MEAN))
  
  min_job_mean <- head(state_wages_mean, 5)
  min_job_median <- head(state_wages_median, 5)
  
  max_job_mean <- tail(state_wages_mean, 5)
  max_job_median <- tail(state_wages_median, 5)
  
  min_job_mean_adjusted <- head(state_wages_mean_adjusted, 5)
  min_job_median_adjusted <- head(state_wages_median_adjusted, 5)
  
  max_job_mean_adjusted <- tail(state_wages_mean_adjusted, 5)
  max_job_median_adjusted <- tail(state_wages_median_adjusted, 5)
  
  number1_median <- subset(state_wages, rank_A_MEDIAN==1)
  number1_median_adjusted <- subset(state_wages, rank_adjusted_A_MEDIAN==1)
  
  dol_value1 <- state_wages$value100[1]
  state_number <- state_wages$AREA[1]
  state_number2 <- as.numeric(state_number)
  state_number2 <- ifelse(state_number2<26, state_number2+25, state_number2-25)
  state_number2 <- ifelse(nchar(state_number2)==1, paste0("0", state_number2), as.character(state_number2))
  state_number2 <- subset(wages2015, AREA==state_number2)
  dol_value2_state <- state_number2$STATE[1]
  dol_value2 <- state_number2$value100[1]
  
  
  # Calculating disparity within occupations by state
  
  occ_disparity <- wages2015 %>%
    group_by(OCC_CODE) %>%
    filter(!is.na(A_MEDIAN)) %>%
    arrange(A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_CODE, TYPE, STATE, A_MEDIAN) 
  
  occ_disparity_max <- occ_disparity %>%
    filter(TYPE=="Max") %>%
    spread(TYPE, A_MEDIAN)
  
  colnames(occ_disparity_max) <- c("OCC_CODE", "max_State", "max_Wage")
  
  
  occ_disparity <- wages2015 %>%
    group_by(OCC_CODE) %>%
    filter(!is.na(A_MEDIAN)) %>%
    arrange(A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_CODE, TYPE, STATE, A_MEDIAN) 
  
  occ_disparity_min <- occ_disparity %>%
    filter(TYPE=="Min") %>%
    spread(TYPE, A_MEDIAN)
  
  colnames(occ_disparity_min) <- c("OCC_CODE", "min_State", "min_Wage")
  
  
  
  occ_disparity_adj <- wages2015 %>%
    group_by(OCC_CODE) %>%
    filter(!is.na(adjusted_A_MEDIAN)) %>%
    arrange(adjusted_A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_CODE, TYPE, STATE, adjusted_A_MEDIAN) 
  
  occ_disparity_max_adj <- occ_disparity_adj %>%
    filter(TYPE=="Max") %>%
    spread(TYPE, adjusted_A_MEDIAN)
  
  colnames(occ_disparity_max_adj) <- c("OCC_CODE", "max_adj_State", "max_adj_Wage")
  
  
  occ_disparity_adj <- wages2015 %>%
    group_by(OCC_CODE) %>%
    filter(!is.na(adjusted_A_MEDIAN)) %>%
    arrange(adjusted_A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_CODE, TYPE, STATE, adjusted_A_MEDIAN) 
  
  occ_disparity_min_adj <- occ_disparity_adj %>%
    filter(TYPE=="Min") %>%
    spread(TYPE, adjusted_A_MEDIAN)
  
  colnames(occ_disparity_min_adj) <- c("OCC_CODE", "min_adj_State", "min_adj_Wage")
  
  occ_code_names <- wages2015[c("OCC_CODE", "OCC_TITLE")]
  occ_code_names <- unique(occ_code_names)
  
  occ_max_min <- left_join(occ_code_names, occ_disparity_max)
  occ_max_min <- left_join(occ_max_min , occ_disparity_min)
  occ_max_min <- left_join(occ_max_min , occ_disparity_max_adj)
  occ_max_min <- left_join(occ_max_min , occ_disparity_min_adj)
  occ_max_min$adj_diff <- occ_max_min$max_adj_Wage - occ_max_min$min_adj_Wage
  #write.table(occ_max_min, "occ_max_min.csv", sep=",", na="")
  #write.csv(occ_max_min, "occ_max_min.csv")
  
  
  # Calculating disparity of occupation pay within states
  
  state_disparity <- wages2015 %>%
    group_by(ST) %>%
    filter(!is.na(A_MEDIAN)) %>%
    arrange(A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_TITLE, TYPE, ST, A_MEDIAN) 
  
  state_disparity_max <- state_disparity %>%
    filter(TYPE=="Max") %>%
    spread(TYPE, A_MEDIAN)
  
  colnames(state_disparity_max) <- c("max_OCC_TITLE", "State", "max_Wage")
  
  state_disparity_min <- state_disparity %>%
    filter(TYPE=="Min") %>%
    spread(TYPE, A_MEDIAN)
  
  colnames(state_disparity_min) <- c("min_OCC_TITLE", "State", "min_Wage")
  
  
  
  state_disparity_adj <- wages2015 %>%
    group_by(ST) %>%
    filter(!is.na(adjusted_A_MEDIAN)) %>%
    arrange(adjusted_A_MEDIAN) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    mutate(TYPE=ifelse(row_number() %% 2 == 0, "Max", "Min")) %>%
    select(OCC_TITLE, TYPE, ST, adjusted_A_MEDIAN) 
  
  state_disparity_max_adj <- state_disparity_adj %>%
    filter(TYPE=="Max") %>%
    spread(TYPE, adjusted_A_MEDIAN)
  
  colnames(state_disparity_max_adj) <- c("max_adj_OCC_TITLE", "State", "max_adj_Wage")
  
  state_disparity_min_adj <- state_disparity_adj %>%
    filter(TYPE=="Min") %>%
    spread(TYPE, adjusted_A_MEDIAN)
  
  colnames(state_disparity_min_adj) <- c("min_adj_OCC_TITLE", "State", "min_adj_Wage")
  
  
  state_max_min <- left_join(state_disparity_max , state_disparity_min)
  state_max_min <- left_join(state_max_min , state_disparity_max_adj)
  state_max_min <- left_join(state_max_min , state_disparity_min_adj)
  state_max_min$max_adj_Wage <- as.numeric(state_max_min$max_adj_Wage)
  state_max_min$min_adj_Wage <- as.numeric(state_max_min$min_adj_Wage)
  
  state_max_min$adj_diff <- state_max_min$max_adj_Wage - state_max_min$min_adj_Wage
  
  #write.table(occ_max_min, "occ_max_min.csv", sep=",", na="")
  #write.csv(occ_max_min, "occ_max_min.csv")
  
  # STATE PERCENTS EMPLOYEES
  wages2015$TOT_EMP <- as.numeric(wages2015$TOT_EMP)
  
  wages2015_no_totals <- wages2015 %>%
    filter(OCC_GROUP!="total")
  
  wages2015_totals <- wages2015 %>%
    filter(OCC_GROUP=="total")
  
  states <- unique(wages2015_no_totals$STATE)
  
  for (i in 1:length(states)) {
    state_num <- states[i]
    states_subset <- subset(wages2015_no_totals, STATE==state_num)
    states_subset_total <- subset(wages2015_totals, STATE==state_num)
    states_subset_total_num <- as.numeric(states_subset_total$TOT_EMP[1])
    states_subset$per_employees <-round(states_subset$TOT_EMP/states_subset_total_num*100,2)
    states_subset_total$per_employees <- -1
    states_subset_all <- rbind(states_subset, states_subset_total)
    
    if (i == 1) {
      states_recombined <- states_subset_all
    } else {
      states_recombined <- rbind(states_recombined, states_subset_all)
    }
    
    
  }
  
  
  most_workers_detailed <- states_recombined %>%
    group_by(ST, OCC_GROUP) %>%
    filter(!is.na(TOT_EMP)) %>%
    arrange(-per_employees) %>%
    filter(row_number()==1) %>%
    filter(OCC_GROUP!="total" & OCC_GROUP=="detailed") %>%
    select(ST, OCC_TITLE, per_employees) 
  
  colnames(most_workers_detailed) <- c("group", "STATE", "detailed_title", "detailed_percent")
  most_workers_detailed$group <- NULL
  
  most_workers_major <- states_recombined %>%
    group_by(ST, OCC_GROUP) %>%
    filter(!is.na(TOT_EMP)) %>%
    arrange(-per_employees) %>%
    filter(row_number()==1) %>%
    filter(OCC_GROUP!="total" & OCC_GROUP=="major") %>%
    select(ST, OCC_TITLE, per_employees) 
  
  colnames(most_workers_major) <- c( "group", "STATE","major_title", "major_percent")
  most_workers_major$group <- NULL
  
  most_workers <- left_join(most_workers_major, most_workers_detailed)
  
  most_workers_state <- subset(most_workers, STATE==state_abbr)
  state_max_min_state <- subset(state_max_min, State==state_abbr)
    
    
  text <- paste0("

                 ##", state_name, "
                 
                 In ", state_name, ", there are about ", total_emp, " million full-time workers based on 2015 data.
                 
                 The largest major job category in the state was ", most_workers_state$major_title, " (about ", most_workers_state$major_percent," percent). Using narrower job descriptions, ", most_workers_state$detailed_title," were the largest category at ", most_workers_state$detailed_percent," percent.
                 
                 The annual median pay ranges from $", formatC(round(min(state_wages_median$A_MEDIAN)), format="d", big.mark=',')," for ", min_job_median$OCC_TITLE[1]," at the lowest end of the spectrum to  $", formatC(round(max(state_wages_median$A_MEDIAN)), format="d", big.mark=',')," for ", max_job_median$OCC_TITLE[5]," at the highest. That's a gap of $", paste0(formatC(round(max(state_max_min_state$adj_diff)), format="d", big.mark=','),  ifelse(state_abbr=="MS", ", the largest in the country.", paste0(". Mississippi has the largest gap in the country of $193,000 between anesthesiologists and psychiatric aides."))),"
                 
                 Credentials, experience, and skill contribute to the differences in pay within a given occupation.
                 
                 ## Best paid jobs
                 ", ifelse(substr(state_name, nchar(state_name), nchar(state_name))=="s", paste0(state_name, "'"), paste0(state_name, "'s"))," highest paid jobs include <span class='occ_title_em'>", paste0(max_job_median$OCC_TITLE[2], ", ", max_job_median$OCC_TITLE[3], "</span>, and <span class='occ_title_em'>", max_job_median$OCC_TITLE[4]),"</span>.
                 
                 About ", nrow(number1_median)," occupations in ", state_name," have the highest median annual pay compared to the other states in the country. Out of ", length(unique(state_wages$OCC_TITLE))," specific occupations, that's ", round(length(number1_median)/length(unique(state_wages$OCC_TITLE))*100,0)," percent of all jobs with the highest pay categorized by the Bureau of Labor Statistics.
                 
                 However, the value of a dollar differs state to state. Prices for some things can be cheaper or more expensive depending on where someone lives. For example, $", dol_value1," in ", state_name," is actually $", dol_value2," in ", dol_value2_state,", according to the Bureau of Economic Analysis.
                 
                 So after adjusting for the actual value of a dollar in a given state, ", state_name," has about ", nrow(number1_median_adjusted)," jobs that have the highest median annual salary compared to other states.
                 
                 ## Worst paid jobs
                 
                 Its lowest-paying jobs include <span class='occ_title_em'>", paste0(min_job_median$OCC_TITLE[4], "</span>, <span class='occ_title_em'>", min_job_median$OCC_TITLE[3], "</span>, and <span class='occ_title_em'>", min_job_median$OCC_TITLE[2]),"</span>. After adjusting for cost of living, those occupational groups make $", formatC(round(max(min_job_median$adjusted_A_MEAN[4])), format="d", big.mark=','),",  $", formatC(round(max(min_job_median$adjusted_A_MEAN[3])), format="d", big.mark=','),", and  $", formatC(round(max(min_job_median$adjusted_A_MEAN[2])), format="d", big.mark=','),", respectively.
                 
                 <span class='occ_title_em'>", wages_all_state_min$OCC_TITLE[1],"</span> have seen the largest decline in wages. Between ", wages_all_state_min$FIRST_YEAR[1]," and ", wages_all_state_min$LAST_YEAR[1],", the adjusted median annual salary dropped from $",  formatC(round(min(wages_all_state_min$FIRST_adjusted_A_MEDIAN[1])), format="d", big.mark=',')," to $",  formatC(round(min(wages_all_state_min$LAST_adjusted_A_MEDIAN[1])), format="d", big.mark=','),". That's a change of about ", formatC(round(wages_all_state_min$percent_change_wages[1],2), format="d", big.mark=','), " percent.
                 
                 Meanwhile, <span class='occ_title_em'>", str_to_lower(wages_all_state_max$OCC_TITLE[5]),"</span> saw the sharpest increase in wages in ", state_name,". Income went up ", wages_all_state_max$percent_change_wages[5]," percent between ", wages_all_state_max$FIRST_YEAR[1]," and ", wages_all_state_max$LAST_YEAR[1]," from $",  formatC(round(min(wages_all_state_max$FIRST_adjusted_A_MEDIAN[5])), format="d", big.mark=',')," to $",  formatC(round(min(wages_all_state_max$LAST_adjusted_A_MEDIAN[5])), format="d", big.mark=','),".
                 
                 Some occupations are markedly competitive, with fewer workers earning more. Local demand for the work and cost of living also can affect salaries.
                 
                 
                 The biggest decline for any job category in", state_name, "was among <span class='occ_title_em'>", growth_all_state_min$OCC_TITLE[1],"</span>. In ", growth_all_state_min$FIRST_YEAR[1],", there were ",  formatC(round(min(growth_all_state_min$FIRST_TOT_EMP[1])), format="d", big.mark=',')," employees. But by ", growth_all_state_min$LAST_YEAR[1],", that figure declined ", growth_all_state_min$percent_change_emp[1]*-1," percent to ",  formatC(round(min(growth_all_state_min$LAST_TOT_EMP[1])), format="d", big.mark=','),". 
                 
                 The jobs that had gained the most employees was ", str_to_lower(growth_all_state_max$OCC_TITLE[5]),". There were ", formatC(round(min(growth_all_state_max$percent_change_emp[5])), format="d", big.mark=',')," percent more workers in ", growth_all_state_max$LAST_YEAR[1]," as compared to ", growth_all_state_max$FIRST_YEAR[1],". Overall, the total number of workers grew from ",  formatC(round(min(growth_all_state_max$FIRST_TOT_EMP[5])), format="d", big.mark=',')," to ",  formatC(round(min(growth_all_state_max$LAST_TOT_EMP[5])), format="d", big.mark=',')," in ", state_name,".")
  
  
  output$text1 <- text
  output$text2 <- the_occu
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

