pacman::p_load(shiny, bslib, quantmod, tidyverse, lubridate)

library(shiny)
library(quantmod)
library(tidyverse)
library(lubridate)
library(bslib)

# Define your remove_outliers function
remove_outliers <- function(df, outlier_level = 'months', count_var = 10, remove = 'high', window = 0){
  outlier_col <- sym(outlier_level)
  df %>%
    left_join(
      df %>%
        {if(remove == 'high') arrange(., desc(PCT_change)) else arrange(., desc(PCT_change))} %>%
        slice(1:count_var) %>%
        select(!!outlier_col) %>%
        mutate(MATCHED = 1) %>%
        distinct(),
      by = as.character(outlier_col)
    ) %>%
    mutate(PCT_change = ifelse(is.na(MATCHED), PCT_change, 0))
}

# Load SP500 data and preprocess
sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo", from = as.Date("1926-01-01"), to = Sys.Date())
GSPC <- get("GSPC", envir = sp500)

GSPC_df <- GSPC %>% 
  as.data.frame() %>%
  tibble()

names(GSPC_df) <- str_remove_all(names(GSPC_df), 'GSPC.')
GSPC_df$dates <- as.Date(row.names(as.data.frame(GSPC)))

GSPC_df <- GSPC_df %>%
  select(Close, Adjusted, dates) %>%
  mutate(PCT_change = (Close - lag(Close)) / lag(Close),
         days = row_number())

cleaned_df <- GSPC_df %>%
  mutate(week = 7 * ceiling(as.numeric(days - 5 + 4)/7) + as.Date(5 - 4),
         month = floor_date(dates, 'month')) %>%
  group_by(week) %>%
  mutate(weeks = cur_group_id()) %>%
  group_by(month) %>%
  mutate(months = cur_group_id()) %>%
  ungroup()

# UI for the Shiny app
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "cerulean"),  # Choose a theme
  
  titlePanel("Clustering Best and Worst Periods of SP500 Returns"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("count_var", "Outlier Removal Count:", min = 0, max = 100, value = 0, step = 1),
      selectInput("outlier_level", "Time period removed:", choices = c("days", "weeks", "months"), selected = "days"),
      selectInput('remove_type', 'Return result type to remove', choices = c('high', 'low'), selected = 'high'),
      selectInput('graph_type', 'Select graph type', choices = c('standard', 'log scale'), selected = 'standard'),
      actionButton("update", "Create/update Graphs")
    ),
    
    mainPanel(
      plotOutput("percentChangePlot"),
      plotOutput("longitudinal_returns"),
      # plotOutput("logValuePlot"),
      verbatimTextOutput("annualReturnText")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  
  # Reactive for filtered data
  filtered_df <- reactive({
    remove_outliers(cleaned_df, 
                    outlier_level = input$outlier_level,
                    count_var = input$count_var, 
                    remove = input$remove_type)
  })
  
  # Calculate returns and annual returns
  observeEvent(input$update, {
    filtered_data <- filtered_df()
    orig_max_value <- filtered_data$Close[nrow(filtered_data)]
    initial_value <- filtered_data$Close[1]
    new_values <- initial_value * cumprod(1 + coalesce(filtered_data$PCT_change, 0))
    final_value <- new_values[length(new_values)]
    total_return <- (final_value / initial_value) - 1
    num_years <- as.numeric(difftime(max(filtered_data$days), min(filtered_data$days), units = "days")) / 365.25
    print(num_years)
    orig_annual_return <- (1 + (orig_max_value/initial_value)-1)^(1 / num_years) - 1
    annual_return <- (1 + total_return)^(1 / num_years) - 1
    
    # Outputs
    output$annualReturnText <- renderText({
      paste("Original Annual Return:", round(orig_annual_return * 100, 2), "%\n",
            "Annual Return after removing outliers:", round(annual_return * 100, 2), "%")
    })
    
    # Plot 1: Percent Change
    output$percentChangePlot <- renderPlot({
      filtered_data %>%
        mutate(tab = 'filtered', Close = new_values) %>%
        select(-MATCHED) %>%
        rbind(cleaned_df %>%
                mutate(tab = 'cleaned')) %>%
        ggplot(aes(days, PCT_change)) + 
        geom_line() + 
        facet_wrap(~tab) + 
        theme_minimal() + 
        labs(y = 'Percent change', x = 'Date', title = 'Daily Change')
    })
    
    # Plot 2: longitudinal returns
    output$longitudinal_returns <- renderPlot({
      if(input$graph_type == 'standard'){
      
        filtered_data %>%
          mutate(tab = 'Filtered', Close = new_values) %>%
          select(-MATCHED) %>%
          rbind(cleaned_df %>%
                  mutate(tab = 'Original')) %>%
          ggplot(aes(days, Close)) + 
          geom_line() + 
          facet_wrap(~tab) + 
          theme_minimal() +
          labs(y = 'SP500 value', x = 'Date', 
               title = paste('Total values removing top', input$count_var, input$outlier_level),
               subtitle = paste0('Loss of ', 
                                 round((orig_annual_return/annual_return - 1) * 100, 1),
                                 '%'))
      } else {
        filtered_data %>%
          mutate(tab = 'Filtered', Close = log(new_values)) %>%
          select(-MATCHED) %>%
          rbind(cleaned_df %>%
                  mutate(tab = 'Original', Close = log(Close))) %>%
          ggplot(aes(days, Close)) + 
          geom_line() + 
          facet_wrap(~tab, nrow = 2) + 
          theme_minimal() +
          labs(y = 'SP500 log value', x = 'Date',
               title = paste('Total values removing top', input$count_var, input$outlier_level))
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
