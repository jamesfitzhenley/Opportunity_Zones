#####
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)

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

###############################################################################
###############################################################################

# Below is the User Interface. I meant to create a dropdown where you can choose
# between Urban, Suburban, or Rural opportunity zones. The output (in server)
# should show average unemployment, home ownership, and poverty by southern
# state for the type of zone selected.

ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
  "Economic Conditions in QOZs by Urbanness",
  
  tabPanel("Context",
    mainPanel(
      h2("Background on Opportunity Zones"),
      
      h3("What are Opportunity Zones?"),
      p("As part of the 2017 Tax Cuts and Jobs Act, the creation of Opportunity
       Zones (OZs) is the most recent federal, bipartisan economic development
       effort. OZs are census tracts chosen by governors (or territory
       executives) to offer tax benefits to private investors. If investors hold
       their capital in these designated census tracts for at least 5-10 years,
       they are eligible for tax deferrals or exemptions. (2017 Tax Cuts and Jobs
       Act)."),
      
      h3("Which census tracts are eligible to be designated as OZs?"),
      p("A census tract can be eligible through any of the following three criteria:"),
      p("I. Poverty rate equal to or greater than 20%;"),
      p("II. Median family income less than or equal to 80% of the area’s median family income; or"),
      p("III. Positioned contiguously with tracts that meet one of the above criteria, with median
        family income no more than 125% of the median family income in tracts it is contiguous with"),
      p("Meeting (I) or (II) classifies a tract as a low-income community.
        Meeting (III) qualifies a tract under the contiguous tract exemption."),
      p("Each state can designate 25% of their eligible tracts as OZs.
        No more than 5% of designated tracts can qualify under the contiguous tract exemption."),
      
      h3("How are OZs chosen from eligible census tracts?"),
      p("It varies across states. Within the states I investigated, North Carolina,
        Tennessee, and Alabama provided a detailed explanation of the factors that
        influenced their decision. The remaining states (South Carolina, Mississippi,
        and Georgia) included little to no information about the decision process."),
      p("Some common considerations that were mentioned included:"),
        p("- A goal to designate at least one tract in each county"),
        p("- Applications from developers, local governments, or community leaders"),
        p("- Alignment with state priorities (specific industries, low-income housing, etc.)"),
        p("- Proximity to supportive resources (college/universities, infrastructure, existing development plans)"),
      p("South Carolina has seen the most vocal critique of the selection process, as
        some tracts were chosen which had existing development projects or about which
        they did not receive any applications."),
      
      br(),
      p("Sources: North Carolina Department of Commerce, South Carolina Post & Courier,
        Georgia Department of Community Affairs, Clarion Ledger,
        Alabama Department of Economic and Development Affairs, Opportunity Alabama,
        Tennessee Department of Economic and Community Development")
      )
    ),
  
  tabPanel("Main",

    # Here is a sidebar!
    
    sidebarPanel(
      selectInput(
        inputId = "urb_choice",                 # a name for the value you choose here
        label = "Would you like to view urban, suburban, or rural Qualified Opportunity Zones?",
        choices = urban                       # your list of choices to choose from
      )
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
             https://www.urban.org/policy-centers/metropolitan-housing-and-communities-policy-center/projects/opportunity-zones")
          )
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