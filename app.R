# =============================================================================
# DETECT Shiny Dashboard
# Created: 2020-10-28
# 
# Note: that there is a difference between using tableOutput and 
# dataTableOutput. 
# One important difference is that tableOutput doesn't format dates as date 
# values by default. 
# https://stackoverflow.com/questions/21548843/r-shiny-different-output-between-rendertable-and-renderdatatable

# Note: must use DT::dataTableOutput and DT::renderDataTable instead of
# shiny::dataTableOutput and shiny::renderDataTable if your data includes date
# columns.
# https://community.rstudio.com/t/data-table-issue-while-rendering-the-shiny-page-datatables-warning-table-id-datatables-table-0-requested-unknown-parameter/44016/4
# =============================================================================

# Load packages
# -------------
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)


# Load data
# ---------
# For now, just import the data that I cleaned in analysis_01_recruitment_metrics.Rmd.
# Eventually, I will need to figure out where to store all that data cleaning 
# code and how best to run it when I refresh the data.
calls_per_day_w_scheduled <- read_rds("/Users/bradcannell/Desktop/calls_per_day_w_scheduled.rds")

# =============================================================================
# Create the UI
# =============================================================================
ui <- dashboardPage(
    dashboardHeader(title = "DETECT Dashboard"),
    
    # Inputs
    # Create an input area in the user interface to select the dates to filter on
    # Using fluidRow stacks the date selector above the data frames instead of to
    # the left of the data frames.
    dashboardSidebar(
        dateRangeInput(
            "dates",
            "What dates do you want (inclusive)?",
            # Set default start and end dates
            start = "2019-08-01",
            end = Sys.Date()
        )
    ),
    dashboardBody(
        # Make a chart from the filtered data
        fluidRow(
            plotlyOutput("plot")
        )
    )
)

# =============================================================================
# Create the server function
# =============================================================================
server <- function(input, output, session) {
    
    # Create a reactive version of the filtered data frame
    filtered_df <- reactive({
        calls_per_day_w_scheduled %>% 
            filter(Date >= input$dates[1] & Date <= input$dates[2])
    })
    
    # The left-hand side of the assignment operator (<-), output$ID, indicates 
    # that you’re providing the recipe for the Shiny output with the matching ID.
    # The right-hand side of the assignment uses a specific render function to 
    # wrap some code that you provide.
    # Add the calls/scheduled per day plot to the UI
    output$plot <- renderPlotly({
        filtered_df() %>%
            ggplot(aes(Date, n_calls)) +
                geom_line(color = "#8a8a8a") +
                geom_point(aes(color = n_scheduled_f)) +
                scale_x_date("Date", date_label = "%b-%y") +
                scale_y_continuous("Number of Calls") +
                scale_color_manual(
                    "F/U Scheduled",
                    values = c(
                        "#8a8a8a", "#F2E750", "#F2B807", "#F28705", "#C52104", 
                        "#a60303"
                    ),
                    drop = FALSE
                ) +
                theme_classic() +
                theme(legend.title = element_text(size = 8))
    })
    
}

# =============================================================================
# Render the app
# =============================================================================
shinyApp(ui, server)
