#####
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(tidycensus)
library(rstanarm)
census_api_key("fcc099ffb2714e2423437589e23236c5640ba53e")

# Loading and cleaning dataset

states <- c("Alabama", "North Carolina", "South Carolina",
            "Georgia", "Tennessee", "Mississippi")

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

desc_vars <- c("Poverty Rate", "Education", "Household Income",
               "White Population", "Unemployment")

# census <- read.csv("Gov50final_data/census_data_by_tract_11.11.csv") %>% 
#   slice(-1) %>% 
#   select(-Name.of.Area,
#          -(Summary.Level:State..FIPS.),
#          -(County.Subdivision..FIPS.:Alaska.Native.Regional.Corporation..FIPS.),
#          -(Metropolitan.and.Micropolitan.Statistical.Area:Area..Water.),
#          -(Population.Density..Per.Sq..Mile.:Area..Land..1)) %>% 
#   mutate(
#     as.numeric(
#       Population.25.Years.and.Over..Less.than.High.School,
#       Population.25.Years.and.Over..High.School.Graduate..Includes.Equivalency.,
#       Population.25.Years.and.Over.))
  # mutate(
  #   edu_rate = (Population.25.Years.and.Over..Less.than.High.School +
  #      Population.25.Years.and.Over..High.School.Graduate..Includes.Equivalency.)
  #      /Population.25.Years.and.Over.)
  #
  # ^^^^ This mutated column shows the percent of the population over age 25
  # that has a high shcool education or lower. I am holding off on calculating
  # it for now, so that I can just try to make the map.

census_pull <- function(state) {
  get_acs(geography = "tract",
                state = state,
                variables = "B19013_001",
                year = 2018,
                geometry = TRUE)  
}
alcensus <- census_pull("AL")
nccensus <- census_pull("NC")
sccensus <- census_pull("SC")
tncensus <- census_pull("TN")
gacensus <- census_pull("GA")
mscensus <- census_pull("MS")
census <- bind_rows(alcensus, nccensus, sccensus,
                    tncensus, gacensus, mscensus)

census <- read.csv("C:/Users/James/Desktop/Gov50Projects/milestone_4/Gov50final_data/censuspulldata.csv")

ozcensus_combine <- function(fullstate) {
  ozlist %>% 
    filter(state == fullstate) %>% 
    select(state, GEOID, Designated) %>% 
    inner_join(census, .)
}

  # Here, I pulled data from the census for each state, so that it is in a
  # format that can be graphed with tidycensus. I then combined the ozlist data
  # with it, so that we get the Opportunity Zone designation variable into the
  # data set, and can use that as the fill.

  # Originally, I had to do this for each state because I couldn't get it to
  # show tract-level data for the whole country at one time. I don't think I
  # actually need these objects, based on how I coded the server, but I am
  # leaving them here for now in case I need them later:
      
      # alozcensus <- ozcensus_combine("Alabama")
      # ncozcensus <- ozcensus_combine("North Carolina")
      # scozcensus <- ozcensus_combine("South Carolina")
      # tnozcensus <- ozcensus_combine("Tennessee")
      # gaozcensus <- ozcensus_combine("Georgia")
      # msozcensus <- ozcensus_combine("Mississippi")


log_model <- stan_glm(Designated ~ unemprate + PovertyRate +
                        medhhincome2014_tract + HSorlower + pctwhitealone,
               refresh = 0,
               family = binomial(link = "logit"),
               data = (ozlist %>%
                         filter(state %in% states) %>%
                         group_by(Designated, state)))

# I made the logistic regression above. I got help in doing this from Nick
# Maxwell, who learned about this from Kevin, one of the course CAs. Though the
# initial outputs of this model are not very useful, which I put it into a
# posterior predict int he server, it will create an easy-to-interpret chart
# showing the probability of being designated an opportunity zone based on the
# input(s).

var_names <- tibble(fullName = desc_vars,
                    dataName = c("PovertyRate", "HSorlower",
                                 "medhhincome2014_tract", "pctwhitealone",
                                 "unemprate"),
                    explanation = c("Poverty Rate",
                                    "Percentage of Adults (25+) with a High
                                    School Degree or Lower",
                                    "Median Household Income (2014)",
                                    "Percentage of Population that is White",
                                    "Unemployment Rate"))

# This tibble makes it so that I can reference a more public-facing definition
# of each variable and explanation for it. I can now use the dataName in code
# which references the other values in the same row as needed. This will help
# for making graphs with an interactive aspect.

# I loaded a dataset from the Urban Institute that shows tract-level economic
# and demographic information for all Qualified Opportunity Zones. I believe
# these data were last updated on 12/14/2018.


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
  
  tabPanel("Maps",
   
    sidebarPanel(
     selectInput(
       inputId = "state_choice",         # a name for the value you choose here
       label = "Select a state to see its Opportunity Zones.",
       choices = states                 # your list of choices to choose from
     )
   ),
   
   
   # And here is your "main panel" for the page.
   
    mainPanel(
     plotOutput("oz_map"),
   )
    ),
  
  tabPanel("Descriptive",
    
    # And here is your "main panel" for the page.
    
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
  
  # tabPanel("Model",
  #          
  #    mainPanel(
  #      h3("How well do different demographic and economic conditions
  #         predict that a census tract will be an Opportunity Zone?"),
  #      p("The figures below show a logistic regression of a census tract's
  #        Opportunity Zone designation with various predictors."),
  #      selectInput(
  #        inputId = "model_choice",
  #        label = "Select a demographic variable to view its prediction of
  #        OZ designation.",
  #        choices = desc_vars
  #      ),
  #      plotOutput("model_plot")
  #    )
  # ),
  
  tabPanel("About",
             h3("This is an about me! My name is James Fitz-Henley"),
           p("Here is the link to my repository: https://github.com/jamesfitzhenley/milestone_6."),
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
  
  # We use renderPlot() to show each of the three desired plots. For each graph,
  # the title changes with the user's choice. The first shows average
  # unemployment by state in opportunity zones filtered by user choice.
  
  output$desc_plot <- renderPlot({
    
    clean_desc_var <- var_names$dataName[
      input$desc_var_choice == var_names$fullName]
    
     results() %>%
      filter(state %in% states) %>%
      group_by(Designated, state) %>% 
      summarize(avg_UE = mean(unemprate, na.rm = TRUE, .groups = "keep"),
                pov_rate = mean(PovertyRate, na.rm = TRUE, .groups = "keep"),
                hh_inc = mean(medhhincome2014_tract, na.rm = TRUE, .groups = "keep"),
                low_edu = mean(HSorlower, na.rm = TRUE, .groups = "keep"),
                pct_white = mean(pctwhitealone, na.rm = TRUE, .groups = "keep")) %>%
      mutate(as.logical(Designated)) %>% 
      mutate(Designated = case_when(Designated == 1 ~ "Opportunity Zone",
                                    T ~ "Other Tracts")) %>% 
      
      # this plot is just like normal!
      ggplot(aes(x = Designated, y = get(var_names$dataName[
                    input$desc_var_choice == var_names$fullName]))) +
      geom_col(fill = "skyblue4") +
      facet_wrap(~ state) +
      labs(title = paste0(input$desc_var_choice, " in Southern Qualified Opportunity Zones"),
           y = "Average UE Rate",
           caption = "Source: Urban Institute, 2014 and 2018 American Community Surveys") +
      theme_classic()
  })
  
  # output$model_plot <- renderPlot({
  #   new_obs <-  results() %>%
  #     filter(state %in% states) %>%
  #     group_by(Designated, state) %>% 
  #     summarize(unemprate = mean(unemprate, na.rm = TRUE, .groups = "keep"),
  #               PovertyRate = mean(PovertyRate, na.rm = TRUE, .groups = "keep"),
  #               medhhincome2014_tract = mean(medhhincome2014_tract, na.rm = TRUE, .groups = "keep"),
  #               HSorlower = mean(HSorlower, na.rm = TRUE, .groups = "keep"),
  #               pctwhitealone = mean(pctwhitealone, na.rm = TRUE, .groups = "keep")) %>%
  #     select(-Designated)
  #   
  #   posterior_predict(log_model, newdata = new_obs) %>% 
  #     as_tibble() %>% 
  #     mutate_all(as.numeric) %>% 
  #     mutate(north = `1`,
  #            south = `2`) %>% 
  #     select(north, south) %>% 
  #     pivot_longer(cols = north:south,
  #                  names_to = "north",
  #                  values_to = "immigration") %>% 
  #     ggplot(aes(immigration, fill = north)) +
  #     geom_histogram(aes(y = after_stat(count/sum(count))),
  #                    position = "identity",
  #                    alpha = 0.5,
  #                    binwidth = 0.1) +
  #     labs(title = "Predicted Immigration Attitudes",
  #          subtitle = "Posterior Distributions Based on Estimates 1981-2015",
  #          x = "Immigration Conservatism",
  #          y = "Density",
  #          caption = "Source: Caughey et al. 2019") +
  #     scale_fill_manual(name = "Region",
  #                       labels = c("Northern Europe", "Rest of Europe"),
  #                       values = c("purple", "gray")) +
  #     theme_economist()
  #   
  #   # results() %>%
  #   #   filter(state %in% states) %>%
  #   #   group_by(Designated, state) %>% 
  #   #   summarize(avg_UE = mean(unemprate, na.rm = TRUE, .groups = "keep"),
  #   #             pov_rate = mean(PovertyRate, na.rm = TRUE, .groups = "keep"),
  #   #             hh_inc = mean(medhhincome2014_tract, na.rm = TRUE, .groups = "keep"),
  #   #             low_edu = mean(HSorlower, na.rm = TRUE, .groups = "keep"),
  #   #             pct_white = mean(pctwhitealone, na.rm = TRUE, .groups = "keep")) %>%
  #   #   mutate(as.logical(Designated)) %>% 
  #   #   mutate(Designated = case_when(Designated == 1 ~ "Opportunity Zone",
  #   #                                 T ~ "Other Tracts")) %>% 
  #   #   
  #   #   # this plot is just like normal!
  #   #   ggplot(aes(x = Designated, y = avg_UE)) +
  #   #   geom_col(fill = "skyblue4") +
  #   #   facet_wrap(~ state) +
  #   #   labs(title = paste0(input$desc_var_choice, " in Southern Qualified Opportunity Zones"),
  #   #        y = "Average UE Rate",
  #   #        caption = "Source: Urban Institute, 2014 and 2018 American Community Surveys") +
  #   #   theme_classic()
  # })
}

shinyApp(ui, server)