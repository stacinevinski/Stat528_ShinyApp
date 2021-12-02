library(shiny)
library(shinyjs)
require(dplyr)
library(usmap)
library(ggplot2)
theme_set(theme_bw(base_size=16))

if(!exists("CPI")) {
  source("DataPrep.R")
}

ui <- fluidPage(
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("chlor")
  ),  
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
    tags$head(
      tags$style(HTML('
        .my_checkBox_Accommodation input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Accommodation input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: red;
            background-color: red;
            content: "X";
            font-size: smaller;
            font-color: black;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Administrative input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Administrative input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: blue;
            background-color: blue;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Agriculture input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Agriculture input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: pink;
            background-color: pink;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Arts input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Arts input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: MediumVioletRed;
            background-color: MediumVioletRed;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Construction input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Construction input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: Salmon;
            background-color: Salmon;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Educational input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Educational input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: BlueViolet;
            background-color: BlueViolet;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Finance input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Finance input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: YellowGreen;
            background-color: YellowGreen;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Health input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Health input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: SeaGreen;
            background-color: SeaGreen;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_NotClassified input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_NotClassified input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: Tan;
            background-color: Tan;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Information input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Information input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: maroon;
            background-color: maroon;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Management input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Management input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: limegreen;
            background-color: limegreen;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Manufacturing input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Manufacturing input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: DarkOliveGreen;
            background-color: DarkOliveGreen;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Mining input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Mining input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: springgreen;
            background-color: springgreen;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Professional input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Professional input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: turquoise;
            background-color: turquoise;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_RealEstate input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_RealEstate input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: SteelBlue;
            background-color: SteelBlue;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Retail input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Retail input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: khaki;
            background-color: khaki;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Transportation input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Transportation input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: orchid;
            background-color: orchid;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Utilities input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Utilities input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: DarkBlue;
            background-color: DarkBlue;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Wholesale input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Wholesale input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: RosyBrown;
            background-color: RosyBrown;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
        
        .my_checkBox_Other input[type="checkbox"]:before {
            border: 2px solid black;
            color: white;
            background-color: white;
            content: "";
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }
  
        .my_checkBox_Other input[type="checkbox"]:checked:after {
            border: 2px solid black;
            color: orange;
            background-color: orange;
            content: "X";
            font-size: smaller;
            vertical-align: middle;
            text-align: center;
            height: 15px;
            left: 0;
            position: absolute;
            top: 0;
            width: 15px;
        }

  '))
    ),
    tags$div(
      HTML(
        '<div id="inds" class="form-group shiny-input-checkboxgroup shiny-input-container">
        <label class="control-label" for="inds">Industries</label>
          <div class="my_checkBox_Accommodation">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Accommodation and food services" checked="checked"/>
                <span><span style="color: black;">Accommodation and food services</span></span>
              </label>
            </div>
          <div class="my_checkBox_Administrative">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Administrative and support and waste management and remediation services" checked="checked"/>
                <span><span style="color: black;">Administrative and support and waste management and remediation services</span></span>
              </label>
            </div>
          <div class="my_checkBox_Agriculture">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Agriculture, forestry, fishing and hunting" checked="checked"/>
                <span><span style="color: black;">Agriculture, forestry, fishing and hunting</span></span>
              </label>
            </div>
          <div class="my_checkBox_Arts">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Arts, entertainment, and recreation" checked="checked"/>
                <span><span style="color: black;">Arts, entertainment, and recreation</span></span>
              </label>
            </div>
          <div class="my_checkBox_Construction">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Construction" checked="checked"/>
                <span><span style="color: black;">Construction</span></span>
              </label>
            </div>
          <div class="my_checkBox_Educational">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Educational services" checked="checked"/>
                <span><span style="color: black;">Educational services</span></span>
              </label>
            </div>
          <div class="my_checkBox_Finance">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Finance and insurance" checked="checked"/>
                <span><span style="color: black;">Finance and insurance</span></span>
              </label>
            </div>
          <div class="my_checkBox_Health">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Health care and social assistance" checked="checked"/>
                <span><span style="color: black;">Health care and social assistance</span></span>
              </label>
            </div>
          <div class="my_checkBox_NotClassified">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Industries not classified" checked="checked"/>
                <span><span style="color: black;">Industries not classified</span></span>
              </label>
            </div>
          <div class="my_checkBox_Information">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Information" checked="checked"/>
                <span><span style="color: black;">Information</span></span>
              </label>
            </div>
          <div class="my_checkBox_Management">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Management of companies and enterprises" checked="checked"/>
                <span><span style="color: black;">Management of companies and enterprises</span></span>
              </label>
            </div>
          <div class="my_checkBox_Manufacturing">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Manufacturing" checked="checked"/>
                <span><span style="color: black;">Manufacturing</span></span>
              </label>
            </div>
          <div class="my_checkBox_Mining">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Mining, quarrying, and oil and gas extraction" checked="checked"/>
                <span><span style="color: black;">Mining, quarrying, and oil and gas extraction</span></span>
              </label>
            </div>
          <div class="my_checkBox_Professional">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Professional, scientific, and technical services" checked="checked"/>
                <span><span style="color: black;">Professional, scientific, and technical services</span></span>
              </label>
            </div>
          <div class="my_checkBox_RealEstate">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Real estate and rental and leasing" checked="checked"/>
                <span><span style="color: black;">Real estate and rental and leasing</span></span>
              </label>
            </div>
          <div class="my_checkBox_Retail">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Retail trade" checked="checked"/>
                <span><span style="color: black;">Retail trade</span></span>
              </label>
            </div>
          <div class="my_checkBox_Transportation">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Transportation and warehousing" checked="checked"/>
                <span><span style="color: black;">Transportation and warehousing</span></span>
              </label>
            </div>
          <div class="my_checkBox_Utilities">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Utilities" checked="checked"/>
                <span><span style="color: black;">Utilities</span></span>
              </label>
            </div>
          <div class="my_checkBox_Wholesale">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Wholesale trade" checked="checked"/>
                <span><span style="color: black;">Wholesale trade</span></span>
              </label>
            </div>
          <div class="my_checkBox_Other">
            <div class="checkbox">
              <label>
                <input type="checkbox" name="inds" value="Other services (except public administration)" checked="checked"/>
                <span><span style="color: black;">Other services (except public administration)</span></span>
              </label>
            </div>
              <label>
                <span><span style="color: grey;">&#9646;Not Available</span></span>
              </label>

          </div>'
      )
    )
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
      dispInd = "Total Establishments"
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
          dispInd = "Total Establishments"
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
          theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
          scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                       "Administrative and support and waste management and remediation services"="blue",
                                                       "Agriculture, forestry, fishing and hunting"="pink",
                                                       "Arts, entertainment, and recreation"="MediumVioletRed",
                                                       "Construction"="Salmon",
                                                       "Educational services"="BlueViolet",
                                                       "Finance and insurance"="YellowGreen",
                                                       "Health care and social assistance"="SeaGreen",
                                                       "Industries not classified"="Tan",
                                                       "Information"="maroon",
                                                       "Management of companies and enterprises"="limegreen",
                                                       "Manufacturing"="DarkOliveGreen",
                                                       "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                       "Professional, scientific, and technical services"="turquoise",
                                                       "Real estate and rental and leasing"="SteelBlue",
                                                       "Retail trade"="khaki",
                                                       "Transportation and warehousing"="orchid",
                                                       "Utilities"="DarkBlue",
                                                       "Wholesale trade"="RosyBrown",
                                                       "Other services (except public administration)"="orange"
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
          dispInd = "Total Establishments"
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
          theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
          scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                       "Administrative and support and waste management and remediation services"="blue",
                                                       "Agriculture, forestry, fishing and hunting"="pink",
                                                       "Arts, entertainment, and recreation"="MediumVioletRed",
                                                       "Construction"="Salmon",
                                                       "Educational services"="BlueViolet",
                                                       "Finance and insurance"="YellowGreen",
                                                       "Health care and social assistance"="SeaGreen",
                                                       "Industries not classified"="Tan",
                                                       "Information"="maroon",
                                                       "Management of companies and enterprises"="limegreen",
                                                       "Manufacturing"="DarkOliveGreen",
                                                       "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                       "Professional, scientific, and technical services"="turquoise",
                                                       "Real estate and rental and leasing"="SteelBlue",
                                                       "Retail trade"="khaki",
                                                       "Transportation and warehousing"="orchid",
                                                       "Utilities"="DarkBlue",
                                                       "Wholesale trade"="RosyBrown",
                                                       "Other services (except public administration)"="orange"
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
          dispInd = "Total Establishments"
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
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else if (input$regions == "Northeast"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Pennsylvania","New Jersey","New York","Connecticut","Rhode Island","Massachusetts","Vermont","New Hampshire","Maine")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.northeast_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else if (input$regions == "South"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Texas","Oklahoma","Arkansas","Louisiana","Mississippi","Alabama","Georgia","Florida","South Carolina","Tennessee","North Carolina","Kentucky","Virginia","West Virginia","Maryland","Delaware")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.south_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else {
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Alaska","Hawaii","Washington","Oregon","California","Montana","Idaho","Nevada","Wyoming","Utah","Colorado","Arizona","New Mexico")) %>%
            group_by(FIPS_ST, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_ST")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.west_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
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
          dispInd = "Total Establishments"
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
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else if (region == "Northeast"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Pennsylvania","New Jersey","New York","Connecticut","Rhode Island","Massachusetts","Vermont","New Hampshire","Maine")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.northeast_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else if (region == "South"){
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Texas","Oklahoma","Arkansas","Louisiana","Mississippi","Alabama","Georgia","Florida","South Carolina","Tennessee","North Carolina","Kentucky","Virginia","West Virginia","Maryland","Delaware")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.south_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        } else {
          TopInds <- df %>% filter(YEAR==input$year,INDUSTRY %in% indsList, STATE %in% c("Alaska","Hawaii","Washington","Oregon","California","Montana","Idaho","Nevada","Wyoming","Utah","Colorado","Arizona","New Mexico")) %>%
            group_by(FIPS_CTY, INDUSTRY) %>%
            summarise(Total = sum(.data[[noquote(input$topInd)]])) %>%
            filter(Total == max(Total))
          TopInds<-rename(TopInds, "fips"="FIPS_CTY")
          
          plot_usmap(data = TopInds, values="INDUSTRY", include=.west_region, size=1) +
            labs(title = paste(input$year,"Top Industry by",dispInd,"in Each State")) +
            theme(plot.title = element_text(size=25),legend.title = element_text(size=15),legend.position="none") +
            scale_fill_manual(name="Industry",values = c("Accommodation and food services"="red",
                                                         "Administrative and support and waste management and remediation services"="blue",
                                                         "Agriculture, forestry, fishing and hunting"="pink",
                                                         "Arts, entertainment, and recreation"="MediumVioletRed",
                                                         "Construction"="Salmon",
                                                         "Educational services"="BlueViolet",
                                                         "Finance and insurance"="YellowGreen",
                                                         "Health care and social assistance"="SeaGreen",
                                                         "Industries not classified"="Tan",
                                                         "Information"="maroon",
                                                         "Management of companies and enterprises"="limegreen",
                                                         "Manufacturing"="DarkOliveGreen",
                                                         "Mining, quarrying, and oil and gas extraction"="springgreen",
                                                         "Professional, scientific, and technical services"="turquoise",
                                                         "Real estate and rental and leasing"="SteelBlue",
                                                         "Retail trade"="khaki",
                                                         "Transportation and warehousing"="orchid",
                                                         "Utilities"="DarkBlue",
                                                         "Wholesale trade"="RosyBrown",
                                                         "Other services (except public administration)"="orange"
            ))
        }
      })
    }
  })
}

# Bind ui and server together
shinyApp(ui, server)