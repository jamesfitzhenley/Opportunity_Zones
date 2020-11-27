#####
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(tidycensus)
library(rstanarm)
library(ggthemes)

## Load and clean data set

# Make object for states the project will focus on

states <- c("Alabama", "North Carolina", "South Carolina",
            "Georgia", "Tennessee", "Mississippi")

# Load and clean Urban Institute data on all Qualified Opportunity Zones. Data
# last updated on 12/14/2018. Includes all eligible tracts (LIC and contiguous
# non-LIC).

ozlist <- read_excel(
  "Gov50final_data/urbaninstitute_tractlevelozanalysis_update1242018.xlsx") %>% 
  mutate(urbsubrur = case_when(Metro == 1 ~ "Urban",
                               Micro == 1 ~ "Suburban",
                               TRUE ~ "Rural"),
         GEOID = geoid) %>% 
  select(-Metro, -Micro, -NoCBSAType, - geoid) %>% 
  mutate(as.numeric(Designated)) %>% 
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% 
  mutate(as.logical(Designated)) %>% 
  filter(state %in% states)


# Make version of Urban Institute data with only eligible census tracts

elig_ozlist <- ozlist %>% 
  filter(PovertyRate >= 0.2)

# Make object with public-facing name of  variables project will investigate

desc_vars <- c("Poverty Rate", "Education", "Household Income",
               "White Population", "Unemployment")

# Read the RDS of census data that was created in clean_data.Rmd

census <- readRDS(
  file = "C:/Users/James/Desktop/Gov50Projects/milestone_4/Gov50final_data/censuspulldata.rds")

# Make function that combines ozlist and census data
  # Census data lets me work with maps, ozlist brings in OZ designation

ozcensus_combine <- function(fullstate) {
  ozlist %>% 
    filter(state == fullstate) %>% 
    select(state, GEOID, Designated) %>% 
    left_join(census, .) %>% 
    mutate(state = if_else(str_detect(NAME, fullstate),
                           fullstate,
                           "delete")) %>% 
    filter(state == fullstate) %>% 
    mutate_if(is.numeric, ~ replace(., is.na(.), 0))
}

# Make tibble to match public facing variable name, explanation, and variable
# data name
  # This will help me make graphs with an interactive aspect

var_names <- tibble(fullName = desc_vars,
                    dataName = c("PovertyRate", "HSorlower",
                                 "medhhincome2014_tract", "pctwhitealone",
                                 "unemprate"),
                    explanation = c("Poverty rate",
                                    "Percentage of adults (25+) with a high
                                    school degree or less education",
                                    "Median household income (2014)",
                                    "Percentage of population that is white",
                                    "Unemployment rate"),
                    model = c("pov_model", "edu_model", "inc_model",
                              "white_model", "ue_model"),
                    pred = c("pov_mod_output", "edu_mod_output", "inc_mod_output",
                                "white_mod_output", "ue_mod_output"))

###############################################################################
###############################################################################

# Build out the User Interface

ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
  "Economic Conditions in QOZs by Urbanness",
  
  # Make an informational tab with important context
  
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
  
  # Make a tab with maps to show where OZs are in the relevant states
  
  tabPanel("Maps",
           
           sidebarPanel(
             selectInput(
               inputId = "state_choice",         
               label = "Select a state to see its Opportunity Zones.",
               choices = states                 
             )
           ),
           
           mainPanel(
             plotOutput("oz_map"),
           )
  ),
  
  # Make a tab for the demographic and economic differences between OZs and non-OZs

  tabPanel("Descriptive",
           
           mainPanel(
             h3("How do Opportunity Zones differ from other census tracts?"),
             p("The figures below show demographic differences between OZs and
        other census tracts."),
             selectInput(
               inputId = "desc_var_choice",
               label = "Select a demographic variable across which to view differences.",
               choices = desc_vars
             ),
             plotOutput("desc_plot")
           )
  ),
  
  # Make a tab with the logistic model and its outputs
  
  tabPanel("Model",

     mainPanel(
       h3("How well do different demographic and economic conditions
          predict that a census tract will be an Opportunity Zone?"),
       p("The figures below show a logistic regression of a census tract's
         Opportunity Zone designation with various predictors."),
       selectInput(
         inputId = "mod_var_choice",
         label = "Select a demographic variable to view its prediction of
         OZ designation.",
         choices = desc_vars
       ),
       plotOutput("model_plot")
     )
  ),
  
  # Make an About tab
  
  tabPanel("About",
           h3("This is an about me! My name is James Fitz-Henley"),
           p("Here is the link to my repository: 
             https://github.com/jamesfitzhenley/Opportunity_Zones."),
           p("The dataset used in this Shiny app comes from the Urban Institute 
           and the 2018 American Community Survey. The Urban Institute data
  show census-tract level economic and demographic data for every Qualified
  Opportunity Zone in the United States. These data were last updated on Dec.
  14, 2018. Some of the included data are unemployment rates, homeownership
  rates, racial population breakdown, a flag for recent socioeconomic change,
  and designation as metro- or micropolitan statistical area."),
           p("The original Urban Instute data can be found at this site:
             https://www.urban.org/policy-centers/metropolitan-housing-and-comm
             unities-policy-center/projects/opportunity-zones")
  )
)
)

server <- function(input, output, session) {
  
  # Plot maps showing where OZs are
    # input$state_choice is full name of state
  
  output$oz_map <- renderPlot({
    ozcensus_combine(input$state_choice) %>% 
      ggplot(aes(fill = Designated)) +
      geom_sf() +
      theme_void() +
      theme(legend.position = "none") +
      labs(title = paste0("Opportunity Zones in", input$state_choice),
           caption = "Source: Urban Institue, 2018 American Community Survey",
           subtitle = "Light blue census tracts are designated Opportunity Zones")
  })
  
  # This line makes our dataset reactive, so that it updates based on user
  # choice.
  
  results <- reactive({
    ozlist
  })
  

  # Plot descriptive differences between OZs and non-OZs
    # Input$desc_var_choice is variable you want to see differences along
  
  output$desc_plot <- renderPlot({
    
    data_desc_var <- var_names$dataName[
      input$desc_var_choice == var_names$fullName]
    desc_explain <- var_names$explanation[
      input$desc_var_choice == var_names$fullName]
    
    results() %>%
      filter(state %in% states) %>%
      group_by(Designated, state) %>% 
      dplyr::summarize(unemprate = mean(unemprate,
                                        na.rm = TRUE,
                                        .groups = "keep"),
                       PovertyRate = mean(PovertyRate,
                                          na.rm = TRUE,
                                          .groups = "keep"),
                       medhhincome2014_tract = mean(medhhincome2014_tract,
                                                    na.rm = TRUE,
                                                    .groups = "keep"),
                       HSorlower = mean(HSorlower,
                                        na.rm = TRUE,
                                        .groups = "keep"),
                       pctwhitealone = mean(pctwhitealone,
                                            na.rm = TRUE,
                                            .groups = "keep")) %>%
      mutate(as.logical(Designated)) %>% 
      mutate(Designated = case_when(Designated == 1 ~ "Opportunity Zone",
                                    T ~ "Other Tracts")) %>% 
      
      # this plot is just like normal!
      
      ggplot(aes(x = Designated, y = get(data_desc_var))) +
      geom_col() +
      facet_wrap(~ state) +
      labs(title = paste0(input$desc_var_choice, " in Southern Qualified Opportunity Zones"),
           subtitle = desc_explain,
           y = input$desc_var_choice,
           caption = "Source: Urban Institute, 2014 and 2018 American Community Surveys") +
      theme_economist()
  })
  
  # Plot logistic model's output
    # Input$mod_var_choice is variable you want to see differences along
  
  output$model_plot <- renderPlot({
    
      # Create objects for variable's explanation and posterior_predict output
      # to be graphed
    
    mod_explain <- var_names$explanation[
      input$mod_var_choice == var_names$fullName]
    
    mod_predict <- var_names$pred[
      input$mod_var_choice == var_names$fullName]
      
      
        # Plot chosen variable's likelihood of designation
    
      get(mod_predict) %>% 
      ggplot(aes(x = variable, y = mean)) +
      geom_line() +
      labs(title = paste0("Likelihood of OZ Designation Based on ",
                          input$mod_var_choice),
          subtitle = mod_explain,
          x = input$mod_var_choice,
          y = "Likelihood of OZ Designation",
          caption = "Source: Urban Institute, 
          2014 and 2018 American Community Surveys") +
      theme_economist()
  })
}

shinyApp(ui, server)