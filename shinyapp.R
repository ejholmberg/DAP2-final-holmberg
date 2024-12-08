library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
# APP 1: MSP enrollment over time, by county and by specific MSP program
# Load data (replace this with your file path)
directory <- "~/Mirror/2024-2025/DAP 2/final/data"
data <- readRDS(paste0(directory, "/data_full.RData"))

counties <- unique(data$county_of_beneficiary)
msp_vars <- c("All MSPs" = "total_msp_per_100k",
              "QMB" = "total_qmb_per_100k",
              "SLMB" = "total_slmb_per_100k",
              "QI" = "total_qi_per_100k")

eligibility_info <- list(
  "total_qmb_per_100k" = "Income eligibility for QMB is <190% FPL.",
  "total_slmb_per_100k" = "Income eligibility for SLMB is 190-210% FPL.",
  "total_qi_per_100k" = "Income eligibility for QI is 210-225% FPL.",
  "total_msp_per_100k" = "Income must be under 225% FPL to qualify for MSPs ($2,844/mo for an individual; $3,853/mo for a couple)."
)

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "united"),
  title = "Massachusetts Medicare Savings Programs (MSP)\nQuarterly Enrollment Rates",
  
  sidebar = sidebar(
    title = "Select a County and Program",
    selectInput(
      "county",
      "County:",
      choices = counties
    ),
    selectInput(
      "variable",
      "Program:",
      choices = msp_vars
    ),
    tags$hr(),
    tags$div(
      style = "padding: 10px 0;",
      textOutput("eligibility_text")
    )
  ),
  
  card(
    plotlyOutput("timeSeriesPlot")
  )
)

server <- function(input, output) {
  output$eligibility_text <- renderText({
    eligibility_info[[input$variable]]
  })
  output$timeSeriesPlot <- renderPlotly({
    # Filter data for selected county
    filtered_data <- data %>%
      filter(county_of_beneficiary == input$county) |> 
      arrange(date)
    
    # Get user-friendly name for plot title
    user_name <- names(msp_vars)[msp_vars == input$variable]
    
    # Create time series plot
    p <- ggplot(filtered_data, aes(x = date, y = .data[[input$variable]], group = 1,
                                   text = paste0("Date: ", format(date, "%b %Y"), "\nRate: ", 
                                                 round(.data[[input$variable]], 1)))) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0.5, 0.5))) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "Enrollment rate\n(per 100,000 Medicare beneficiaries)",
        title = paste("Enrollment Rates for", user_name, "\nin", input$county, "County")
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
}

shinyApp(ui, server)

# APP 2: demographic maps to compare w/ map of MSP enrollment change since 2020 policy implementation

# Load data (replace this with your file path)
directory <- "~/Mirror/2024-2025/DAP 2/final/data"
data <- readRDS(paste0(directory, "/data_full.RData")) |> 
  mutate(beneficiaries_prop = beneficiaries/total_pop)

enrollment_pre <- data |> 
  filter(date == "2019-12-30")

enrollment_post <- data |> 
  filter(date == "2023-09-30")

enrollment_data <- enrollment_pre |> 
  left_join(enrollment_post, join_by(FIPS_STCO), suffix = c("_pre", "_post")) |> 
  mutate(change = total_msp_per_100k_post - total_msp_per_100k_pre) |> 
  select(county_of_beneficiary_pre, geometry_pre, change)

p <- ggplot(enrollment_data) +
  geom_sf(aes(fill = change, geometry = geometry_pre)) +
  scale_fill_distiller(palette = "RdBu", direction = 1, labels = scales::comma) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  labs(fill = "Change in rate\n(per 100,000\nbeneficiaries)")

# Creating list of demographic choices to select
demographic_choices <- c("Medicare beneficiaries, 65+" = "beneficiaries",
                         "Medicare beneficiaries, 65+ (proportion of population)" = "beneficiaries_prop",
                         "Total population" = "total_pop",
                         "White population (%)" = "white_percent",
                         "Non-white population (%)" = "nonwhite_percent")

ui <- page_sidebar(
  title = "Comparison Tool for County-Level Demographics and MSP Enrollment Shift",
  sidebar = sidebar(
    selectInput("demographic", "Select Demographic Variable:",
                choices = demographic_choices,
                selected = "beneficiaries"),
    p(
      "Data sources: 5-year ACS (2015-2022) and 1-year ACS (2023), US Census Bureau; CMS Quarterly Enrollment Snapshot (06/2015-09/2023)",
      style = "font-size: 0.8em; color: #666; margin-top: 20px;"
    )
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Demographics, September 2023"),
      plotOutput("dynamic_map")
    ),
    card(
      card_header("MSP Enrollment Rate Change Post-January 2020 Expansion"),
      plotOutput("static_map")
    )
  )
)

server <- function(input, output) {
  output$static_map <- renderPlot({
    p
  })
  
  output$dynamic_map <- renderPlot({
    current_data <- data |> 
      filter(date == "2023-09-30")
    
    ggplot(current_data) +
      geom_sf(aes(fill = .data[[input$demographic]], geometry = geometry)) +
      scale_fill_distiller(palette = "Blues", direction = 1, labels = scales::comma) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(face = "bold", hjust = 0.5)
      ) +
      labs(title = names(demographic_choices)[demographic_choices == input$demographic],
           fill = "Value")
  })
}

shinyApp(ui, server)
