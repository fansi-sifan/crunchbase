
library(dplyr)
library(purrr)
library(shiny)
library(plotly)
library(DT)
library(tidytext)
library(visNetwork)
library(igraph)
library(reshape2)
library(rsample)

load("data/cbsa_100.rda")
load("data/cb_cbsa.rda")
cb_cbsa <- cb_cbsa %>%
  filter(!is.na(cbsa_code))

source("helper.R")

# Define UI for application that draws a histogram
ui <- navbarPage(

  # Application title
  titlePanel("metro technology complexity"),
  tabPanel(
    "complexity index",
    # Sidebar with a slider input for number of
    sidebarLayout(
      sidebarPanel(
        sliderInput("tag_min",
          "(tech_us_total) Minimum number of companies for a tag to qualify as a category:",
          min = 0,
          max = 50,
          value = 10
        ),

        sliderInput("tag_max",
          "(tech_us_total) Maximum number of companies for a tag to qualify as a category:",
          min = 1000,
          max = 4500,
          value = 4000
        ),
     
        htmlOutput("text"),
        
        br(),
        

        sliderInput("firm_n",
          "(tech_msa_total) Minimum number of companies in a metro to qualify as having relative technological advantage:",
          min = 0,
          max = 5,
          value = 3
        ),
   

        selectInput(
          inputId = "method",
          label = "Choose a parameter to measure RCA: ",
          choices = c(
            "Location quotient (lq)"  = "lq",
            # "Normalized location quotient, unweighted (slq)" = "slq",
            "Normalized location quotient, weighted (SLQ)" = "SLQ"
          ),
          selected = "SLQ"
        ),

        sliderInput("probs",
          "Choose a position in distribution to determine RCA",
          min = 0.5,
          max = 1,
          value = 0.95
        ),

        # sliderInput("ubi",
        #   "(Ubiquity) Minimum number of cities for a technology to qualify:",
        #   min = 0,
        #   max = 50,
        #   value = 5
        # ),
        #
        # sliderInput("div",
        #   "(Diversity) Minimum number of technologies for a city to qualify:",
        #   min = 0,
        #   max = 50,
        #   value = 5
        # ),
        #
        # checkboxInput("msa",
        #   "Top 100 metro only",
        #   value = F
        # ),

        h4("Metro diversity and average ubiquity of technologies"),
        plotlyOutput("Plot")
      ),

      mainPanel(
        h4("Metro startup Complexity Rank"),
        DT::dataTableOutput("table_cbsa"),

        h4("Metro - technology pairs"),
        DT::dataTableOutput("table_all")
      )
    )
  ),

  tabPanel(
    "Product Space",
    fluidRow(
      column(
        2,
        sliderInput("freq",
          "Mininum number of co-occurance",
          min = 5,
          max = 20,
          value = 10
        ),
        selectInput("msa_name",
          "Select a metro to highlight its product space",
          choices = "Select a metro"
        )
      ),
      column(
        10,
        visNetworkOutput("network", height = 800)
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  data <- reactive(
    cb_cbsa %>%
      clean_cat(min = input$tag_min, max = input$tag_max) 
  )

  data_final <- reactive({
    tmp <- data()%>%
      clean_firms(input$firm_n)%>%
      calculate_LQ() %>%
      calculate_SLQ() 
    
    tmp %>%
      left_join(get_SLLQ(tmp, "SLQ", input$probs), by = "tech_name") %>%
      rename(z_SLQ = value) %>%
      left_join(get_SLLQ(tmp, "slq", input$probs), by = "tech_name") %>%
      rename(z_slq = value) %>%
      ungroup() %>%
      calculate_tci(method = input$method)
  })

  # network
  observe({
    updateSelectInput(session, "msa_name", 
                      choices = data_final() %>% 
                        select(cbsa_name) %>% 
                        unique())
  })

  nw_data <- reactive(
    data_final() %>%
      create_network(freq = input$freq, input$msa_name)
  )


  output$network <- renderVisNetwork(
    nw_data() %>%
      Plot_network()
  )

  # data table

  output$text <- renderUI({
    str1 <- paste0("Total unique categories: ", nrow(data() %>% select(tech_name) %>% unique()))
    str2 <- paste0(nrow(cb_cbsa %>% select(permalink) %>% unique()) - nrow(data() %>% ungroup() %>% select(permalink) %>% unique()), " observations dropped")
    HTML(paste(str1, str2, sep = "<br/>"))
  })

  output$table_cbsa <- DT::renderDataTable(
    DT::datatable(create_output(data_final(), 900) %>%
      select(cbsa_name, TCI_city = div) %>%
      arrange(-TCI_city) %>%
      unique()) %>%
      formatRound(columns = "TCI_city", digits = 2)
  )

  output$table_all <- DT::renderDataTable(
    DT::datatable(create_output(data_final(), 900) %>%
      select(cbsa_name, tech_name,
        tech_msa_total = n,
        TCI_city = div, TCI_tech = ubi, tech_us_total, tech_us_share, tech_msa_share, lq, LQ, slq, SLQ
      )) %>%
      formatPercentage(columns = c("tech_us_share", "tech_msa_share"), digits = 2 )%>%
      formatRound(columns = c("TCI_city", "TCI_tech", "lq","LQ","slq", "SLQ"), digits = 2)
  )

  output$Plot <- renderPlotly({
    plot_mean_ubi(data_final())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
