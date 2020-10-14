#####
library(shiny)
library(tidyverse)
library(readxl)

# Loading and cleaning dataset

ozlist <- read_excel("Gov50final_data/urbaninstitute_tractlevelozanalysis_update1242018.xlsx") %>% 
  mutate(urbsubrur = case_when(Metro == 1 ~ "Urban",
                               Micro == 1 ~ "Suburban",
                               TRUE ~ "Rural")) %>% 
  select(-Metro, -Micro, -NoCBSAType)

urban <- c("Urban", "Suburban", "Rural")

# I loaded a dataset from the Urban Institute that shows tract-level economic
# and demographic information for all Qualified Opportunity Zones. I believe
# these data were last updated on 12/14/2018.
# To clean up the categorization of Metropolitan and Micropolitan Statistical
# Areas, I created a new column that will be easy to transfer into a shiny app.
# The suburbrur column helps us see the urbanization of the region. Later in the
# project, I might bring in 538's urbanization index to be able to view these
# census tracts on a continuous urbanization scale, rather than broad categories
# like this.

######################################################################################
######################################################################################

# Below is the User Interface. I meant to create a dropdown where you can choose
# between Urban, Suburban, or Rural opportunity zones. The output (in server)
# should show average unemployment, home ownership, and poverty by southern
# state for the type of zone selected.

ui <- fluidPage(navbarPage(
  "Economic Conditions in QOZs by Urbanness",
  
  tabPanel(
    "Main",

    # Here is a sidebar!
    
    sidebarPanel(
      selectInput(
        inputId = "urb_choice",                 # a name for the value you choose here
        label = "Would you like to view urban, suburban, or rural Qualified Opportunity Zones?",
        choices = urban                       # your list of choices to choose from
      ),
    ),
    
    
    # And here is your "main panel" for the page.
    
    mainPanel(
      textOutput("urb_message"),              
      plotOutput("urb_UE_plot"),
      plotOutput("urb_pov_plot"),
      plotOutput("urb_own_plot")
    )
  ),
  tabPanel("About",
             h3("This is an about me! My name is James Fitz-Henley"),
           p("Here is the link to my repository: https://github.com/jamesfitzhenley/milestone_4."),
           p("The dataset used in this Shiny app comes from the Urban Institute. The data
  show census-tract level economic and demographic data for every Qualified
  Opportunity Zone in the United States. These data were last updated on Dec.
  14, 2018. Some of the included data are unemployment rates, homeownership
  rates, racial population breakdown, a flag for recent socioeconomic change,
  and designation as metro- or micropolitan statistical area."),
           p("The original data can be found at this site:
             https://www.urban.org/policy-centers/metropolitan-housing-and-communities-policy-center/projects/opportunity-zones"))
  )
)

server <- function(input, output, session) {
 
  # Here, renderText() is updating the message on the main panel based on the
  # chosen urbanness.
  
  output$urb_message <- renderText({
    paste0("Here are economic conditions for ",
           input$urb_choice,
           " qualified opportunity zones in Southern states.")
  })

  
  # This line makes our dataset reactive, so that it updates based on user
  # choice.
  
  results <- reactive({
    ozlist
  })
  
  # We use renderPlot() to show each of the three desired plots. For each graph,
  # the title changes with the user's choice. The first shows average
  # unemployment by state in opportunity zones filtered by user choice.
  
  output$urb_UE_plot <- renderPlot({
    results() %>%
      filter(urbsubrur == input$urb_choice,
             state %in% c("Alabama", "Georgia", "South Carolina",
                          "North Carolina", "Mississippi", "Tennessee")) %>%
      group_by(state) %>% 
      summarize(avg_UE = mean(unemprate, na.rm = TRUE, .groups = "keep")) %>% 
      
      # this plot is just like normal!
      ggplot(aes(x = state, y = avg_UE)) +
      geom_col() +
      labs(title = paste0("Unemployment in Southern ",
                          input$urb_choice,
                          " QOZs"),
           x = "State",
           y = "Average UE Rate") +
      theme_classic()
  })
  
  # This plot shows average poverty rates by state in opportunity zones filtered
  # by user choice.
  
  output$urb_pov_plot <- renderPlot({
    results() %>%
      filter(urbsubrur == input$urb_choice,
             state %in% c("Alabama", "Georgia", "South Carolina",
                          "North Carolina", "Mississippi", "Tennessee")) %>%
      group_by(state) %>% 
      summarize(avg_pov = mean(PovertyRate, na.rm = TRUE, .groups = "keep")) %>% 
      
      # this plot is just like normal!
      ggplot(aes(x = state, y = avg_pov)) +
      geom_col() +
      labs(title = paste0("Poverty Rates in Southern ",
                          input$urb_choice,
                          " QOZs"),
           x = "State",
           y = "Average Poverty Rate") +
      theme_classic()
  })
  
  # This plot shows average home ownership rates by state in qualified
  # opportunity zones filtered by user choice.
  
  output$urb_own_plot <- renderPlot({
    results() %>%
      filter(urbsubrur == input$urb_choice,
             state %in% c("Alabama", "Georgia", "South Carolina",
                          "North Carolina", "Mississippi", "Tennessee")) %>%
      group_by(state) %>% 
      summarize(avg_own = mean(pctown, na.rm = TRUE, .groups = "keep")) %>% 
      
      # this plot is just like normal!
      ggplot(aes(x = state, y = avg_own)) +
      geom_col() +
      labs(title = paste0("Home Ownership Rates in Southern ",
                          input$urb_choice,
                          " QOZs"),
           x = "State",
           y = "Average Home Ownership Rate") +
      theme_classic()
  })
}

shinyApp(ui, server)