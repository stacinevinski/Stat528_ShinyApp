library(shiny)
library(maps)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

iowa <- map_data("county") %>% filter(region=="iowa")

if (!file.exists("monument.rda")) {
  monuments <- read.csv("https://data.iowa.gov/api/views/6394-pygx/rows.csv", stringsAsFactors = FALSE)
  monuments <- monuments %>% separate(City.Location, into=c("City2", "Location"), sep="\n") %>%
    mutate(
      Location = gsub("[()]","", Location),
      Resource.Name = gsub("  +","", Resource.Name)
    ) %>% separate(Location, into =c("lat", "long"), sep=",") %>% 
    mutate(
      lat = as.numeric(lat),
      long = as.numeric(long)
    ) %>%
    filter(
      !is.na(lat),
      !is.na(long)
    )
  save(monuments, file="monument.rda")
} else {
  load("monument.rda")
}

allmonuments <- iowa %>% ggplot(aes(x = long, y = lat)) + 
  geom_path(aes(group = group), colour ="grey50") +
  ggthemes::theme_map() + coord_equal() +
  geom_point(aes(text = Resource.Name), data = monuments, size = 1) 
  

ui <- fluidPage(
  titlePanel("Monuments in Iowa"),
  sidebarPanel(
    selectInput("monument", "Monument: ", choices=unique(monuments$Resource.Name), multiple = TRUE),
    sliderInput("long", "range in longitude", min = min(iowa$long), max = max(iowa$long), 
                value = c(min(iowa$long), max(iowa$long))),
    sliderInput("lat", "range in latitude", min = min(iowa$lat), max = max(iowa$lat), 
                value = c(min(iowa$lat), max(iowa$lat)))
  ),
  mainPanel(plotlyOutput("map"))
)

server <- function(input, output) {
  output$map <- renderPlotly({
    gp <- NULL
    if (length(input$monument) > 0) 
      gp <- geom_point(aes(text = Resource.Name),
                       data = monuments %>% filter(Resource.Name %in% input$monument), 
                       colour = "orange", size = 3)
    gg <- allmonuments + gp +
      xlim(input$long[1], input$long[2]) + 
      ylim(input$lat[1], input$lat[2])
    ggplotly(gg, tooltip = "text")
  })
}

shinyApp(ui, server)
