
library(shiny)
library(plotly)
library(DT)

load("data/cbsa_100.rda")
load("data/complexity_cbsa.rda")

create_output <- function(df, itr) {
  # Iterate 100 times to calculate KCI
  for (i in itr) {
    tmp <- df %>%
      group_by(tech_name) %>%
      mutate(ubi = sum(div) / ubi) %>%
      ungroup() %>%
      group_by(cbsa_name, cbsa_code) %>%
      mutate(div = sum(ubi) / div) %>%
      ungroup()
    return(tmp)
  }

  KCI <- tmp %>%
    select(cbsa_code, cbsa_name, KCI = div) %>%
    unique()

  TCI <- tmp %>%
    select(tech_name, TCI = ubi) %>%
    unique()

  # merge
  output <- df %>%
    left_join(KCI, by = c("cbsa_code", "cbsa_name")) %>%
    left_join(TCI, by = "tech_name") %>%
    arrange(-KCI)

  return(output)
}


plot_mean_ubi <- function(df) {
  ggplot(
    df %>%
      group_by(cbsa_code, cbsa_name, div) %>%
      summarise(mean_ubi = sum(ubi / div)),
    aes(x = div, y = mean_ubi, label = cbsa_name)
  ) +
    geom_point(stat = "identity") +
    geom_vline(aes(xintercept = mean(div)), color = "red") +
    geom_hline(aes(yintercept = mean(mean_ubi)), color = "red")
}



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("metro technology complexity"),

  # Sidebar with a slider input for number of
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Minimum number of companies to qualify as having relative technological advantage:",
                  min = 1,
                  max = 5,
                  value = 3
      ),
      
      sliderInput("ubi",
        "(Ubiquity) Minimum number of cities for a technology to qualify:",
        min = 1,
        max = 50,
        value = 10
      ),

      sliderInput("div",
        "(Diversity) Minimum number of technologies for a city to qualify:",
        min = 1,
        max = 50,
        value = 5
      ),

      checkboxInput("msa",
        "Top 100 metro only",
        value = T
      ),

      h4("Metro diversity and average ubiquity of technologies"),
      plotlyOutput("Plot")
    ),

    mainPanel(
      h4("Metro startup Complexity Rank"),
      DT::dataTableOutput("table_cbsa"),

      h4("Metro - technology pairs (Search for metro or technology:"),
      DT::dataTableOutput("table_all")
    )
  )
)


# Define server logic
server <- function(input, output) {
  data_final <- reactive(
    complexity_cbsa %>%

      # remove technologies only claimed by one city
      filter(ubi >= input$ubi) %>%
      # remove cities with low diversity
      filter(div >= input$div) %>%
      # remove nonmetros
      # top 100 metros
      {
        if (input$msa) filter(., cbsa_code %in% cbsa_100) else filter(., !is.na(cbsa_code))
      } %>%

      ungroup() %>%
      # recalculate diversity -------
      group_by(cbsa_name, cbsa_code) %>%
      mutate(div = n()) %>%
      # recalculate ubiquity --------
      group_by(tech_name) %>%
      mutate(ubi = n()) %>%
      ungroup() %>%
      filter(n >= input$n)
  )

  output$table_cbsa <- DT::renderDataTable(
    DT::datatable(create_output(data_final(), 100) %>%
      select(cbsa_name, TCI_city = div) %>%
      arrange(-TCI_city) %>%
      unique()) %>%
      formatRound(columns = "TCI_city", digits = 2)
  )

  output$table_all <- DT::renderDataTable(
    DT::datatable(create_output(data_final(), 100)%>%
                    select(cbsa_name, tech_name, tech_msa_total = n, 
                           TCI_city = div, TCI_tech = ubi, tech_us_total, tech_us_share, tech_msa_share, lq),
                  filter = "top") %>%
      formatRound(columns = c("tech_us_share", "tech_msa_share", "lq", "TCI_city", "TCI_tech"), digits = 2)
  )

  output$Plot <- renderPlotly({
    plot_mean_ubi(data_final())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
