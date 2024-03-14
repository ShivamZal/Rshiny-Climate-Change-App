library(leafdown)
library(leaflet)
library(shiny)
library(dplyr)
library(leafgl)
library(sf)
library(ggplot2)
library(plotly)
library(readr)
library(shinydashboard)
library(bs4Dash)
library(ggrepel)

helper_files = list.files(path = "helper", full.names = TRUE, pattern = "*.R")
sapply(helper_files, source, encoding = "UTF-8")

# shapes and data -----------------------------------------------------------------
usa1 <- readRDS("shapes/usa1.RDS")
usa2 <- readRDS("shapes/usa2.RDS")
spdfs_list <- list(usa1, usa2, usa2)
df_stations_monthly <- get_data()


#####

server <- function(input, output) {
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  active_marker_ids <- NULL
  markers <- NULL
  
  rv_update_leafdown <- reactiveVal(0)
  rv_active_markers <- reactiveVal()
  
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv_update_leafdown(rv_update_leafdown() + 1)
  })
  
  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    active_marker_ids <<- c()
    isolate(rv_active_markers(c()))
    rv_update_leafdown(rv_update_leafdown() + 1)
  })
  
  output$leafdown <- renderLeaflet({
    rv_update_leafdown()
    update_leafdown_map(my_leafdown, input, df_stations_monthly, my_leafdown$curr_data)
  })
  
  observeEvent(input$leafdown_glify_click, {
    click_lat <- input$leafdown_glify_click$lat
    click_lng <- input$leafdown_glify_click$lng
    markers <- avg_temp_per_displ_marker(df_stations_monthly, my_leafdown$curr_data)
    new_id <- which(markers$latitude == click_lat & markers$longitude == click_lng)
    if (new_id %in% active_marker_ids) {
      active_marker_ids <<- active_marker_ids[!active_marker_ids == new_id]
    } else {
      active_marker_ids <<- c(active_marker_ids, new_id)
    }
    rv_active_markers(markers[active_marker_ids, ])
    update_markers(active_marker_ids, markers)
  }, ignoreInit = TRUE)
  
  # plots
  output$line_plot <- renderPlot({
    curr_sel_data <- my_leafdown$curr_sel_data()
    curr_map_level <- my_leafdown$curr_map_level
    active_markers <- rv_active_markers()
    create_line_plot(curr_sel_data, curr_map_level, df_stations_monthly, active_markers)
  })
  
  output$scatter_plot <- renderPlot({
    curr_sel_data <- my_leafdown$curr_sel_data()
    curr_map_level <- my_leafdown$curr_map_level
    active_markers <- rv_active_markers()
    create_scatter_plot(curr_sel_data, curr_map_level, df_stations_monthly, active_markers)
  })
  
}


####

ui  <- dashboardPage(
  dashboardHeader(),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("list-alt")),
      menuItem("Static Maps", tabName = "static", icon = icon("th")),
      menuItem("Interactive Map", tabName = "interactive", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "interactive",
              fluidRow(
                
                box(
                  tags$style(HTML(".leaflet-container {background: #343a40;}")),
                  HTML("<h2><b>Interactive maps</b></h2>"),
                  actionButton("drill_down", "Drill Down"),
                  actionButton("drill_up", "Drill Up"),
                  leafletOutput("leafdown", height = 700),
                  collapsible = FALSE,
                  width = 6,
                  headerBorder = FALSE
                ),
                column(
                  width = 6,
                  fluidRow(
                    bs4Card(
                      plotOutput("scatter_plot", height = 328),
                      width = 12,
                      collapsible = FALSE,
                      headerBorder = FALSE
                    )
                  ),
                  fluidRow(
                    box(
                      plotOutput("line_plot", height = 328),
                      width = 12,
                      collapsible = FALSE,
                      headerBorder = FALSE
                    )
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "static",
              HTML("<h2><b><center>Static Maps</center></b></h2>"),
              HTML("<p>The static maps presented assert the correctness of the prediction as a similar trend is explored in each of the global maps especially the baseline global map whereas a longitudinal trend is seen for USA</p>"),
              
              br(),
              img(src = "world_1913.png", height = "100%", width = "100%"),
              h2(""),
              h2(""),
              img(src = "world_2013.png", height = "100%", width = "100%"),
              h2(""),
              h2(""),
              img(src = "world_change.png", height = "100%", width = "100%"),
              h2(""),
              h2(""),
              img(src = "usa_change.png", height = "100%", width = "100%"),
              h2(""),
              h2(""),
              
      ),
      tabItem(tabName = "introduction",
              HTML("<h2><b><center>Introduction</center></b></h2>"),
              br(),
              p("Climate change describes global warming—the ongoing increase in global average temperature—and its effects on Earth’s climate system.
                There are many factors which include population, deforestation, industrial and agricultural practices.  Looking at the changes in global average land temperatures over more than 150 years, one can clearly see a steady upward trend.

For example, the avg land temperature increased from 8.3 in 1913 to 9.83 in 2015 i.e. an increase of 1.53 degrees C.

Average Land Temperatures in US have been increasing as well during the same time. They increased from 11.09 in 1913 to 12.1 in 2013 i.e an increase of 1.01 degrees C."
              ), 
              br(),
              HTML('<center><img src = "cc.jpg", height = "35%", width = "50%"></center>'),
              br(),
              HTML('<p>With the help of this Shiny App, I have tried to uncover more crucial factors which has not been discussed enough in the day-to-day climate change discussions. 
                These significant factors that are further analysed here are: <b>latitude and longitude</b>'),
              br(),
              HTML("<h3><b><center>QHPT </center></b></h3>"),
              HTML("<p><b>Question:</b>How is the world and USA facing climate change over the years?</p>"),
              HTML("<p><b>Hypothesis:</b>The effects of climate change differ by geographic location (region and coordinates)</p>"),
              HTML("<p><b>Prediction:</b>Increase in temperature is positively correlated to increase in latitude</p>"),
              HTML("<h3><b><center>Methods and Dataset</center></b></h3>"),
             HTML("<p>There are 2 datasets used for plotting static and interactive maps where the former is from Berkeley Earth which contains average temperature values for all countries from 1913-2013, the latter is acquired solely for the interactive map which contains average temperature and precipitation values for USA states and counties from meteostat.net</p>"),
            HTML("<p>Libraries like <b>plotly</b> and <b>ggplot</b> are used to create static maps which discuss temperature change for a period of 100 years whereas <b>leaflet</b> and <b>leafdown</b> are used for interactive map which gives user the freedom to drill up or down based on finding out the granularity of his research</p>"),
            HTML("<h3><b><center>Conclusion</center></b></h3>"),
            HTML("<p>Since the further scope and holes which were discussed during the lighting talk presentation regarding increasing granularity and adding one more factor (rainfall), the conclusion of latitude and region being an integral factor is justified as the countries near the equator have mostly faced less effects of increasing temperature than countries residing closer to north pole whereas countries near south pole have an opposite effect of climate change where the mean temperature has been seen decreasing over the years.
                 Coversely, USA states face the opposite trend where the northern states are comparatively cooler with less rainfall than southern states which is quite intriguing</p>")

    )
  )
)
)


shinyApp(ui = ui, server = server)