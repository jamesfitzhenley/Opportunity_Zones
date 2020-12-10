#####
library(ggplot2)
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(tidycensus)
library(rstanarm)
library(ggthemes)
library(sf)
census_api_key("fcc099ffb2714e2423437589e23236c5640ba53e")

## Load and clean data set

# Make object for states the project will focus on

states <- c("Alabama", "North Carolina", "South Carolina",
            "Georgia", "Tennessee", "Mississippi")

# Load and clean Urban Institute data on all Qualified Opportunity Zones. Data
# last updated on 12/14/2018. Includes all eligible tracts (LIC and contiguous
# non-LIC).

ozlist <- readRDS(file = "Gov50final_data/ozlist.rds") %>% 
  mutate(GEOID = geoid) %>% 
  mutate(Designated = as.numeric(Designated)) %>%
  mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>%
  mutate(Designated = as.logical(Designated)) %>%
  filter(state %in% states)

    # I loaded the Urban Institute data, filtered to the states of interest, and
    # replaced the NAs in the designated column with zeroes before switching it
    # back to a logical column.

# Make object with public-facing name of variables that the project will
# investigate

desc_vars <- c("Poverty Rate", "Low Education", "Household Income",
               "White Population", "Unemployment Rate")

# Read the RDS of census data that was created in clean_data.Rmd

census <- readRDS(
  file = "Gov50final_data/censuspulldata.rds")

# Make function that combines ozlist and census data for a specific state
  # Census data lets me work with maps, ozlist brings in OZ designation

ozcensus_combine <- function(fullstate) {
  ozlist %>% 
    filter(state == fullstate) %>% 
    select(state, GEOID, Designated) %>% 
    left_join(census, ., by = "GEOID") %>% 
    mutate(state = if_else(str_detect(NAME, fullstate),
                           fullstate,
                           "delete")) %>% 
    filter(state == fullstate) %>% 
    mutate_if(is.logical, ~ replace(., is.na(.), F))
}

      # I used left_join to make sure that I still include all the tracts in
      # each state. I originally used inner_join(), but that would only include
      # the tracts in ozlist, which only includes census tracts that are
      # eligible to be designated OZs, not all the tracts in a state. After this
      # join, it is fair to turn all NAs in the Designated column to Falses,
      # since these are census tracts that are ineligible to be designated
      # Opportunity Zones.

# Make object that combines ozlist and census data for all relevant states
  # This will be faster for the shiny app to do than running ozcensus_combine()
  # 6 times or some modified version of it. The code to create this object is in
  # the clean_data.Rmd.

# Make tibble to match public facing variable name, explanation, and variable
# data name

var_names <- tibble(
              fullName = desc_vars,
              dataName = c("PovertyRate", "HSorlower",
                           "medhhincome2014_tract", "pctwhitealone",
                           "unemprate"),
              explanation = c("Poverty rate",
      "Percentage of adults (25+) with a high school degree or less education",
                              "Median household income (2014)",
                              "Percentage of population that is white",
                              "Unemployment rate"),
              model = c("pov_model", "edu_model", "inc_model",
                        "white_model", "ue_model"),
              pred = c("pov_mod_output", "edu_mod_output", "inc_mod_output",
                          "white_mod_output", "ue_mod_output"),
              sd = c("sd_pov","sd_edu","sd_inc","sd_white","sd_ue"))

        # This will help me make graphs with an interactive aspect. This way, I
        # can change the labeling of a chart and the model that it graphs while
        # using a clean, interpretable set of names for variables.

# Load in all_objects.RDS

all_objects <- readRDS(file = "Gov50final_data/all_objects.rds")
    
    # This will bring in everything that I created in clean_data.Rmd. By reading
    # this object into this .Rmd, I can be sure that the app will run whenever
    # someone clicks on the link, regardless of whether or not they can access
    # and run clean_data.

# Create objects for each of the models/tibbles in all_objects

  # Objects for each of the models

pov_model <- all_objects$pov_mod
inc_model <- all_objects$inc_mod
white_model <- all_objects$white_mod
ue_model <- all_objects$ue_mod
edu_model <- all_objects$edu_mod


  # Objects for each of the posterior_predict outputs

pov_mod_output <- all_objects$pov_output
inc_mod_output <- all_objects$inc_output
white_mod_output <- all_objects$white_output
ue_mod_output <- all_objects$ue_output
edu_mod_output <- all_objects$edu_output

  # Object for ozcensus_combined_all

ozcensus_combined_all <- all_objects$ozcensus_combined_all

      # I chose to name the objects the same as the names in the var_names
      # tibble or elsewhere in final_project.R to limit the number of changes I
      # need to make to my code. This is also easier, because I have now made it
      # so that I can use the same name to refer to the same object in
      # final_project.R and clean_data.R

###############################################################################
###############################################################################

# Build out the User Interface

ui <- fluidPage(theme = shinytheme("flatly"), navbarPage(
  "Demographic and Economic Conditions in Qualified Opportunity Zones",
  
  # Make an informational tab with important context
  
  tabPanel("Context",
       mainPanel(
         h2("Background on Opportunity Zones"),
         
         h3("What are Opportunity Zones?"),
         p("As part of the 2017 Tax Cuts and Jobs Act, the creation of 
   Opportunity Zones (OZs) is the most recent federal, bipartisan economic 
   development effort. OZs are census tracts chosen by governors (or 
   territory executives) to offer tax benefits to private investors. If 
   investors hold their capital in these designated census tracts for at 
   least 5-7 years, they are eligible for tax deferrals or exemptions. 
   (2017 Tax Cuts and Jobs Act)."),
         
         h3("Which census tracts are eligible to be designated as OZs?"),
         p("A census tract can be eligible through any of the following 
           three criteria:"),
         p("I. Poverty rate equal to or greater than 20%;"),
         p("II. Median family income less than or equal to 80% of the 
           area’s median family income; or"),
         p("III. Positioned contiguously with tracts that meet one of 
           the above criteria, with median family income no more than 
           125% of the median family income in tracts it is contiguous 
           with"),
         p("Meeting (I) or (II) classifies a tract as a low-income 
           community. Meeting (III) qualifies a tract under the 
           contiguous tract exemption."),
         p("Each state can designate 25% of their eligible tracts 
           as OZs. No more than 5% of designated tracts can qualify 
           under the contiguous tract exemption."),
         
         h3("How are OZs chosen from eligible census tracts?"),
         p("It varies across states. Within the states I investigated, 
         North Carolina, Tennessee, and Alabama provided a detailed 
         explanation of the factors that influenced their decision. 
         The remaining states (South Carolina, Mississippi, and Georgia) 
         published little to no information about the decision process."),
         p("Some common considerations that were mentioned included:"),
         p("- A goal to designate at least one tract in each county"),
         p("- Applications from developers, local governments, or 
           community leaders"),
         p("- Alignment with state priorities (specific industries, 
           low-income housing, etc.)"),
         p("- Proximity to supportive resources (college/universities, 
           trasnportation infrastructure, existing development plans)"),
         p("South Carolina has seen the most vocal critique of the 
          selection process, as some tracts were chosen which had existing 
          development projects or about which they did not receive any 
          applications."),
         
         br(),
         p("Sources: North Carolina Department of Commerce, South Carolina 
         Post & Courier, Georgia Department of Community Affairs, Clarion 
         Ledger, Alabama Department of Economic and Development Affairs, 
         Opportunity Alabama, Tennessee Department of Economic and 
         Community Development")
       )
  ),
  
  # Make a tab with maps to show where OZs are in the relevant states
  
  tabPanel("Maps",
           
           # Make a drop-down menu on the side that allows the reader to select
           # a state to view.
           
           sidebarPanel(
             selectInput(
               inputId = "state_choice",         
               label = "Select a state to see its Opportunity Zones.",
               choices = states                 
             )
           ),
           
           # Set the output to be a map, which I will make in the server below.
           
           mainPanel(
             plotOutput("oz_map"),
             p("In the data, some census tracts were missing information about
               demographic measures and their designation as an Opportunity
               Zone. I assumed that these tracts were not Opportunity Zones."),
             p("Gray areas on the map are urban areas. These population-dense 
               areas have so many census tracts close together that it is hard 
               to see each tract.")
           )
  ),
  
  # Make a tab for the demographic and economic differences between OZs and
  # non-OZs

  tabPanel("Descriptive",
           
           # Have a brief explanation in the main panel
           
           mainPanel(
             h3("How do Opportunity Zones differ from other census tracts?"),
             p("The figures below show demographic differences between OZs and
        other census tracts."),
             
          # Make a dropdown to choose a demographic variable to view the
          # differences
          
             selectInput(
               inputId = "desc_var_choice",
               label = "Select a demographic variable 
                        across which to view differences.",
               choices = desc_vars
             ),
             plotOutput("desc_plot"),
             p("The error bars show +/- 2 standard deviations."),
             p("Because of the size of the standard deviations, we see no
               significant difference between Opportunity Zones and other tracts
               in any of these six (6) states on any of these measures.
               In theory, additional research could compare Opportunity Zones to 
               other tracts across the country, controlling for state, to see if 
               a larger dataset would show any significant differences. However,
               given that income inequality is fairly high in the South
               compared to the rest of the United States, it is unlikely that
               we would see greater differences and sufficiently smaller
               error.")
           )
  ),
  
  # Make a tab with the logistic model and its outputs
  
  tabPanel("Model",

    # Have a brief explanation in the main panel
           
     mainPanel(
       h3("How do different demographic and economic conditions
          predict that a census tract will be an Opportunity Zone?"),
       p("The figures below show a logistic regression of a census tract's
         Opportunity Zone designation with various predictors."),
       
       # Make a dropdown to choose a demographic variable to view the
       # differences
       
       selectInput(
         inputId = "mod_var_choice",
         label = "Select a demographic variable to view its prediction of
         OZ designation.",
         choices = desc_vars
       ),
       plotOutput("model_plot"),
       p("Somewhat unsurprisingly, we see that census tracts with worse 
         economic and educational conditions are more likely to be designated 
         an Opportunity Zone. We also see that census tracs with a lower white 
         population are more likely to be designated an Opportunity Zone.")
     )
  ),
  
  # Make an About tab
  
  tabPanel("About",
           h3("This is an about me! My name is James Fitz-Henley"),
           p("Here is the link to my repository: 
             https://github.com/jamesfitzhenley/Opportunity_Zones."),
           p("The data used in this Shiny app come from the Urban Institute 
           and the 2018 American Community Survey. The Urban Institute data
  show census-tract level economic and demographic data for every Qualified
  Opportunity Zone in the United States. These data were last updated on Dec.
  14, 2018. Some of the included data are unemployment rates, homeownership
  rates, racial population breakdown. The 2018 ACS provided more demographic
  and economic data about each census tract. It also provided me with the
  needed information to produce state maps with census tracts marked on
  them."),
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
    state_map <- ozcensus_combine(input$state_choice) 
    
    st_crs(state_map$geometry) = 4326
    
    # The line of code above is incredibly useful. For a long time, I ran into
    # an issue where the shiny would run locally, but I would receive an error
    # instead of the maps when I published. I tried running this line of code in
    # the console multiple times, installing and reinstalling the sf package,
    # and more. With a ton of Tyler's help, I saw to use this in the actual app
    # and solved the problem that had been holding me up for almost a week.
    # Evelyn Cai also put in a fair amount of time helping me with this on the
    # class slack.
    
    state_map %>% 
      ggplot(aes(fill = Designated)) +
      geom_sf() +
      theme_void() +
      theme(legend.position = "none") +
      labs(title = paste0("Opportunity Zones in ", input$state_choice),
           caption = "Source: Urban Institue, 2018 American Community Survey",
           subtitle = 
             "Light blue census tracts are designated Opportunity Zones")
  })
  
  # Plot descriptive differences between OZs and non-OZs
    # Input$desc_var_choice is variable you want to see differences along
  
  output$desc_plot <- renderPlot({
    
    # Use the var_names tibble to extract the data name, explanation, and
    # standard deviations that align with the variable chosen in the dropdown
    
    data_desc_var <- var_names$dataName[
      input$desc_var_choice == var_names$fullName]
    
    desc_explain <- var_names$explanation[
      input$desc_var_choice == var_names$fullName]
    
    desc_sd <- var_names$sd[
      input$desc_var_choice == var_names$fullName]
    
    # Use the output of ozcensus_combine() for each state, so that the
    # comparison is between OZ and non-OZ tracts, not just between OZ and
    # eligible, non-OZ tracts
    
      # For the sake of ease, I initially tried to create another function
      # called ozcensus_combine_all(); this way, I could avoid making an object
      # for each state, then binding those 6 separate tibbles.
    
      # For the sake of speed, I instead made an object with what that function
      # would have been. I went ahead and then summarized the data in that
      # object, so that it is ready to be graphed immediately in this output.
      # This was all done in the clean.data.Rmd.
    
      ggplot(ozcensus_combined_all, 
             aes(x = Designated, y = get(data_desc_var))) +
      geom_col(aes(fill = Designated)) +
      geom_errorbar(aes(x = Designated,
                        ymin = get(data_desc_var) - 2*get(desc_sd),
                        ymax = get(data_desc_var) + 2*get(desc_sd)),
                    color = "black",
                    width = 0.4) +
      facet_wrap(~ state) +
      labs(title = paste0(input$desc_var_choice,
                          " in Southern Qualified Opportunity Zones"),
           subtitle = desc_explain,
           y = input$desc_var_choice,
           caption = 
          "Source: Urban Institute, 2014 and 2018 American Community Surveys") +
        theme_economist()
      
        # I used the summarize() function to find the mean value for each
        # variable in the state's Opportunity Zones and other tracts, then
        # compared them through a bar chart. I then used paste0() to get the
        # choice that the reader selects in the dropdown menu to show up in the
        # title of the chart.
    
  })
  
  # Plot logistic model's output
    # Input$mod_var_choice is variable you want to see differences along
  
  output$model_plot <- renderPlot({
    
      # Create objects for variable's explanation and posterior_predict output
      # to be graphed; these objects use the var_names tibble that I made at the
      # beginning, based on the input from the model page's dropdown menu.
    
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
          2014 and 2018 American Community Surveys",
          label = "State") +
      theme_economist()
  })
}

shinyApp(ui, server)