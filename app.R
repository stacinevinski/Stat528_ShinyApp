library(shinyjs)
require(dplyr)
library(usmap)
library(ggplot2)
theme_set(theme_bw(base_size=16))

ui <- fluidPage(
  # Application title
  titlePanel("Top US Industries by Employment"),

  # Sidebar with a slider input for the number of bins
  sidebarPanel(
    sliderInput("year",
                "Year:",
                min = 2005,
                max = 2019,
                value = 2005,
                step = 1,
                ticks = FALSE),

    #Top industry calculation
    selectInput("topInd", "Display top industry by:",
                c("# of Establishments" = "ESTAB",
                  "Annual Payroll" = "PAYANN_19DOL",
                  "First Quarter Payroll" = "PAYQTR1_19DOL",
                  "# of Employees" = "EMP")),

    #State or County
    selectInput("level", "Map display level of detail:",
                c("County (by State)" = "County",
                  "Region (by County)"="RegionCounty",
                  "Region (by State)"="RegionState",
                  "State (entire US)" = "State"),
                "State"),

    #Show states
    uiOutput("states"),

    #Show regions
    uiOutput("regions"),

    #Industries to show
    checkboxGroupInput("inds", "Industries to display:",
                       c("Accommodation and food services"="Accommodation and food services",
                         "Administrative and support and waste management and remediation services"="Administrative and support and waste management and remediation services",
                         "Agriculture, forestry, fishing and hunting"="Agriculture, forestry, fishing and hunting",
                         "Arts, entertainment, and recreation"="Arts, entertainment, and recreation",
                         "Construction"="Construction",
                         "Educational services"="Educational services",
                         "Finance and insurance"="Finance and insurance",
                         "Health care and social assistance"="Health care and social assistance",
                         "Industries not classified"="Industries not classified",
                         "Information"="Information",
                         "Management of companies and enterprises"="Management of companies and enterprises",
                         "Manufacturing"="Manufacturing",
                         "Mining, quarrying, and oil and gas extraction"="Mining, quarrying, and oil and gas extraction",
                         "Professional, scientific, and technical services"="Professional, scientific, and technical services",
                         "Real estate and rental and leasing"="Real estate and rental and leasing",
                         "Retail trade"="Retail trade",
                         "Transportation and warehousing"="Transportation and warehousing",
                         "Utilities"="Utilities",
                         "Wholesale trade"="Wholesale trade",
                         "Other services (except public administration)"="Other services (except public administration)"
                       ),selected=c("Accommodation and food services","Administrative and support and waste management and remediation services","Agriculture, forestry, fishing and hunting","Arts, entertainment, and recreation","Construction","Educational services","Finance and insurance","Health care and social assistance","Industries not classified","Information","Management of companies and enterprises","Manufacturing","Mining, quarrying, and oil and gas extraction","Other services (except public administration)","Professional, scientific, and technical services","Real estate and rental and leasing","Retail trade","Transportation and warehousing","Utilities","Wholesale trade"))
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("chlor")
  )
)

server <- function(input, output) {
  output$states <- renderUI({
    req(input$level == "County")

    selectInput("states", "State:",
                c("Alabama"="Alabama",
                  "Alaska"="Alaska",
                  "Arizona"="Arizona",
                  "Arkansas"="Arkansas",
                  "California"="California",
                  "Colorado"="Colorado",
                  "Connecticut"="Connecticut",
                  "Delaware"="Delaware",
                  "Florida"="Florida",
                  "Georgia"="Georgia",
                  "Hawaii"="Hawaii",
                  "Idaho"="Idaho",
                  "Illinois"="Illinois",
                  "Indiana"="Indiana",
                  "Iowa"="Iowa",
                  "Kansas"="Kansas",
                  "Kentucky"="Kentucky",
                  "Louisiana"="Louisiana",
                  "Maine"="Maine",
                  "Maryland"="Maryland",
                  "Massachusetts"="Massachusetts",
                  "Michigan"="Michigan",
                  "Minnesota"="Minnesota",
                  "Mississippi"="Mississippi",
                  "Missouri"="Missouri",
                  "Montana"="Montana",
                  "Nebraska"="Nebraska",
                  "Nevada"="Nevada",
                  "New Hampshire"="New Hampshire",
                  "New Jersey"="New Jersey",
                  "New Mexico"="New Mexico",
                  "New York"="New York",
                  "North Carolina"="North Carolina",
                  "North Dakota"="North Dakota",
                  "Ohio"="Ohio",
                  "Oklahoma"="Oklahoma",
                  "Oregon"="Oregon",
                  "Pennsylvania"="Pennsylvania",
                  "Rhode Island"="Rhode Island",
                  "South Carolina"="South Carolina",
                  "South Dakota"="South Dakota",
                  "Tennessee"="Tennessee",
                  "Texas"="Texas",
                  "Utah"="Utah",
                  "Vermont"="Vermont",
                  "Virginia"="Virginia",
                  "Washington"="Washington",
                  "West Virginia"="West Virginia",
                  "Wisconsin"="Wisconsin",
                  "Wyoming"="Wyoming"
                ),"Alabama")
  })

  output$regions <- renderUI({
    req(input$level == "RegionState" | input$level == "RegionCounty")

    selectInput("regions", "Region:",
                c("Midwest"="Midwest",
                  "Northeast"="Northeast",
                  "South"="South",
                  "West"="West"
                  ))
  })

  observeEvent(input$topInd,{
    if (input$topInd == "ESTAB"){
      dispInd = "Total Establishements"
    } else if (input$topInd == "PAYANN_19DOL"){
      dispInd = "Annual Payroll"
    } else if (input$topInd == "PAYQTR1_19DOL"){
      dispInd = "1st Quarter Payroll"
    } else {
      dispInd = "Total Number of Employees"
    }
  })

  observeEvent(input$level,{
    if(input$level == "County"){
      output$chlor <- renderPlot({
        indsList <- NULL
        for (i in input$inds){
          if (is.null(indsList)) {
            #indsList <- paste0('"',i,'"',sep="")
            indsList <- c(i)
          } else {
            #indsList <- paste0(indsList,',"',i,'"',sep="")
            indsList <- append(indsList,i)
          }
        }

        if (input$topInd == "ESTAB"){
          dispInd = "Total Establishements"
        } else if (input$topInd == "PAYANN_19DOL"){
          dispInd = "Annual Payroll"
        } else if (input$topInd == "PAYQTR1_19DOL"){
          dispInd = "1st Quarter Payroll"
        } else {
          dispInd = "Total Number of Employees"
        }

        if (is.null(input$states)) {
          stateChosen = "Alabama"
        } else {
          stateChosen = input$states
        }

        TopInds <- df %>% filter(YEAR==input$year, STATE==stateChosen,INDUSTRY %in% indsList) %>%
          group_by(FIPS_CTY, INDUSTRY) %>%
          summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
          filter(Total == max(Total))
        TopInds<-rename(TopInds, "fips"="FIPS_CTY")

        plot_usmap(data = TopInds, values="INDUSTRY", include=c(stateChosen), size=1) +
          labs(title = paste(input$year,"Top Industry by",dispInd,"in",stateChosen)) +
          theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
          scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                        "Administrative and support and waste management and remediation services"="blue",
                                        "Agriculture, forestry, fishing and hunting"="pink",
                                        "Arts, entertainment, and recreation"="yellow",
                                        "Construction"="brown",
                                        "Educational services"="purple",
                                        "Finance and insurance"="black",
                                        "Health care and social assistance"="green",
                                        "Industries not classified"="orange",
                                        "Information"="maroon",
                                        "Management of companies and enterprises"="limegreen",
                                        "Manufacturing"="olivedrab",
                                        "Mining, quarrying, and oil and gas extraction"="springgreen",
                                        "Professional, scientific, and technical services"="turquoise",
                                        "Real estate and rental and leasing"="aquamarine",
                                        "Retail trade"="khaki",
                                        "Transportation and warehousing"="orchid",
                                        "Utilities"="navyblue",
                                        "Wholesale trade"="honeydew",
                                        "Other services (except public administration)"="grey"
                                        ))

    })
    } else if (input$level == "State"){
      output$chlor <- renderPlot({
        indsList <- NULL
        for (i in input$inds){
          if (is.null(indsList)) {
            #indsList <- paste0('"',i,'"',sep="")
            indsList <- c(i)
          } else {
            #indsList <- paste0(indsList,',"',i,'"',sep="")
            indsList <- append(indsList,i)
          }
        }

        if (input$topInd == "ESTAB"){
          dispInd = "Total Establishements"
        } else if (input$topInd == "PAYANN_19DOL"){
          dispInd = "Annual Payroll"
        } else if (input$topInd == "PAYQTR1_19DOL"){
          dispInd = "1st Quarter Payroll"
        } else {
          dispInd = "Total Number of Employees"
        }
        TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList) %>%
          group_by(FIPS_ST, INDUSTRY) %>%
          summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
          filter(Total == max(Total))
        TopInds<-rename(TopInds, "fips"="FIPS_ST")

        plot_usmap(data = TopInds, values="INDUSTRY", size=1) +
          labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
          theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
          scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                       "Administrative and support and waste management and remediation services"="blue",
                                                       "Agriculture, forestry, fishing and hunting"="pink",
                                                       "Arts, entertainment, and recreation"="yellow",
                                                       "Construction"="brown",
                                                       "Educational services"="purple",
                                                       "Finance and insurance"="black",
                                                       "Health care and social assistance"="green",
                                                       "Industries not classified"="orange",
                                                       "Information"="maroon",
                                                       "Management of companies and enterprises"="limegreen",
                                                       "Manufacturing"="olivedrab",
                                                       "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                       "Professional, scientific, and technical services"="turquoise",
                                                       "Real estate and rental and leasing"="aquamarine",
                                                       "Retail trade"="khaki",
                                                       "Transportation and warehousing"="orchid",
                                                       "Utilities"="navyblue",
                                                       "Wholesale trade"="honeydew",
                                                       "Other services (except public administration)"="grey"
          ))
      })
    } else if (input$level == "RegionState") {
      output$chlor <- renderPlot({
        indsList <- NULL
        for (i in input$inds){
          if (is.null(indsList)) {
            #indsList <- paste0('"',i,'"',sep="")
            indsList <- c(i)
          } else {
            #indsList <- paste0(indsList,',"',i,'"',sep="")
            indsList <- append(indsList,i)
          }
        }

        if (input$topInd == "ESTAB"){
          dispInd = "Total Establishements"
        } else if (input$topInd == "PAYANN_19DOL"){
          dispInd = "Annual Payroll"
        } else if (input$topInd == "PAYQTR1_19DOL"){
          dispInd = "1st Quarter Payroll"
        } else {
          dispInd = "Total Number of Employees"
        }

        if (input$regions == "Midwest"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("North Dakota","South Dakota","Nebraska","Kansas","Minnesota","Iowa","Missouri","Wisconsin","Illinois","Michigan","Indiana","Ohio")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.midwest_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else if (input$regions == "Northeast"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Pennsylvania","New Jersey","New York","Connecticut","Rhode Island","Massachusetts","Vermont","New Hampshire","Maine")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.northeast_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else if (input$regions == "South"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Texas","Oklahoma","Arkansas","Louisiana","Mississippi","Alabama","Georgia","Florida","South Carolina","Tennessee","North Carolina","Kentucky","Virginia","West Virginia","Maryland","Delaware")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.south_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else {
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Alaska","Hawaii","Washington","Oregon","California","Montana","Idaho","Nevada","Wyoming","Utah","Colorado","Arizona","New Mexico")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.west_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        }
      })
    } else {
      output$chlor <- renderPlot({
        indsList <- NULL
        for (i in input$inds){
          if (is.null(indsList)) {
            #indsList <- paste0('"',i,'"',sep="")
            indsList <- c(i)
          } else {
            #indsList <- paste0(indsList,',"',i,'"',sep="")
            indsList <- append(indsList,i)
          }
        }

        if (input$topInd == "ESTAB"){
          dispInd = "Total Establishements"
        } else if (input$topInd == "PAYANN_19DOL"){
          dispInd = "Annual Payroll"
        } else if (input$topInd == "PAYQTR1_19DOL"){
          dispInd = "1st Quarter Payroll"
        } else {
          dispInd = "Total Number of Employees"
        }

        if (is.null(input$regions)) {
          region="Midwest"
        } else {
          region = input$regions
        }

        if (region == "Midwest"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("North Dakota","South Dakota","Nebraska","Kansas","Minnesota","Iowa","Missouri","Wisconsin","Illinois","Michigan","Indiana","Ohio")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.midwest_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else if (region == "Northeast"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Pennsylvania","New Jersey","New York","Connecticut","Rhode Island","Massachusetts","Vermont","New Hampshire","Maine")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.northeast_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else if (region == "South"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Texas","Oklahoma","Arkansas","Louisiana","Mississippi","Alabama","Georgia","Florida","South Carolina","Tennessee","North Carolina","Kentucky","Virginia","West Virginia","Maryland","Delaware")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.south_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        } else {
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Alaska","Hawaii","Washington","Oregon","California","Montana","Idaho","Nevada","Wyoming","Utah","Colorado","Arizona","New Mexico")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")

          plot_usmap(data = TopInds, values="INDUSTRY", include=.west_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(legend.position = "right",plot.title = element_text(size=25),legend.title = element_text(size=15),legend.text = element_text(size=15)) +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="yellow",
                                                         "Construction"="brown",
                                                         "Educational services"="purple",
                                                         "Finance and insurance"="black",
                                                         "Health care and social assistance"="green",
                                                         "Industries not classified"="orange",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="olivedrab",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="aquamarine",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="navyblue",
                                                         "Wholesale trade"="honeydew",
                                                         "Other services (except public administration)"="grey"
            ))
        }
      })
    }
  })
}

# Bind ui and server together
shinyApp(ui, server)
