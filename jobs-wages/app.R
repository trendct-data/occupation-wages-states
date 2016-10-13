overtime <- readRDS("data/mega_wages.rds")
wages2015 <- readRDS("data/wages2015.rds")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
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
  
  #state_abbr<-"TX"
  #state_occ <-"Editors"
  
  state_wages <- subset(wages2015, ST==state_abbr)
  state_name <- unique(state_wages$STATE)
  total_emp <-  subset(state_wages, OCC_TITLE=="All Occupations")
  total_emp <- as.numeric(total_emp$TOT_EMP)
  total_emp <- round(total_emp/1000000,1)
  
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

