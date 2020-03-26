library(tidyverse)
library(shiny) 
library(scales)
library(shinyWidgets) # extra shiny UI options 
library(plotly) # interactive plots 
library(ggthemes)
library(shinythemes) # shiny UI themes
# library(leaflet) # interactive map
# library(rgdal) # geospatial data abstraction library functions
# library(geojsonio) # deal with json file
# library(sp) # deal with spatial data

load("Data/data.RData")
##### Functions #####
LagOutcomeByLocation <- function(location, metric, minimum = 100) {
    dfCaseThreshold <- dfFull %>% group_by((!!sym(location)), Date) %>%
        summarise(Value = sum((!!sym(metric)), na.rm=T)) %>%
        filter(Value >= minimum) %>%
        slice(1) %>%
        select((!!sym(location)), Date)
    
    dfTmp <- dfFull %>% group_by((!!sym(location)), Date) %>%
        summarise(Value = sum((!!sym(metric)), na.rm = T))
    
    left_join(dfTmp, dfCaseThreshold, by = location, suffix = c("", ".y")) %>%
        group_by((!!sym(location)), Date) %>%
        filter(Date >= Date.y) %>%
        select(-Date.y) %>%
        ungroup() %>%
        group_by((!!sym(location))) %>%
        mutate(TSE = Date - dfCaseThreshold$Date[match((!!sym(location)), dfCaseThreshold[[location]])])
}    

CreatePlotText <- function(Region, Outcome, PlotType, ScaleType, LaggedPlot, NewPlot) {
    textPlot = "ggplotly(ggplot()"
    if (PlotType == "Line") { textPlot = paste(textPlot, "+ geom_line(data = dfTmp, mapping = aes(x =", 
                                               ifelse(LaggedPlot, "TSE,", "Date,"), "y = Value, color = ", Region,
                                               "), size = 1) + geom_point(data = dfTmp, mapping = aes(x =", 
                                               ifelse(LaggedPlot, "TSE,", "Date,"), "y = Value, color =", Region, ")) + theme_minimal()")
    } else if (PlotType == "Bar") { textPlot = paste(textPlot, "+ geom_bar(data = dfTmp, mapping = aes(x =", ifelse(LaggedPlot, "TSE,", "Date,"), "y = Value, fill =", Region, "), stat = 'Identity', position = \"dodge\") + theme_minimal()")
    }
    
    if (LaggedPlot) {
        if (Outcome == "Confirmed") {textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Cases since 100th Confirmed Case\", x = \"Days Since 100th Confirmed Case\", y = \"Confirmed Cases\")")
        } else if (Outcome == "Deaths") {textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Deaths since 100th Confirmed Death\", x = \"Days Since 100th Confirmed Death\", y = \"Confirmed Deaths\")")
        } else if (Outcome == "Recovered") { textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Recoveries since 100th Recovery\", x = \"Days since 100th Recovery\", y = \"Confirmed Recoveries\")")
        }
    } else {
        if (Outcome == "Confirmed") {textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Cases\", x = \"Date\", y = \"Confirmed Cases\")")
        } else if (Outcome == "Deaths") {textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Deaths\", x = \"Date\", y = \"Confirmed Deaths\")")
        } else if (Outcome == "Recovered") { textPlot = paste(textPlot, "+ labs(title = \"Cumulative Confirmed Recoveries\", x = \"Date\", y = \"Confirmed Recoveries\")")
        }
    }
    
    if (ScaleType == "Linear") {textPlot = paste(textPlot, "+ scale_y_continuous(labels = comma)")
    } else if (ScaleType == "Log-10") {textPlot = paste(textPlot, "+ scale_y_continuous(labels = comma, trans = 'log10')")
    }
    
    if (LaggedPlot) {textPlot = paste(textPlot, "+ xlim(0, input$LaggedDaysToShow)")
    }
    
    if (NewPlot) {textPlot = str_replace_all(textPlot, "Cumulative", "Daily New")}
    
    textPlot = paste0(textPlot, ")")
    
    textPlot
}

##### Define UI for application #####
ui <- fluidPage(theme = shinytheme("yeti"),

    # Application title
    titlePanel("COVID-19 Dashboard"),
    h3(textOutput("GlobalConfirmedCount"), align = "left"),
    h4(textOutput("GlobalDeathCount"), align = "left"),
    h6(textOutput("CurrentDate"), align = "left"),
    tabsetPanel(type = "tabs",
                tabPanel("Charts", 
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 dateRangeInput("DateRange", "Date Range:", start = min(dfFull$Date), end = max(dfFull$Date)),
                                 selectizeInput("DisplayRegions", "Select Regions to Display:", 
                                                    choices = c("World" = "World", 
                                                                "Continents" = "continent", 
                                                                "Sub-Regions" = "sub_region",
                                                                "Countries" = "Country"),
                                                    selected = c("World")),
                                 
                                 pickerInput("Locations", "Locations", choices = NULL, 
                                             options = list(`actions-box` = T, `live-search` = T), multiple = T), 
                                 radioButtons("Outcome", "Outcome:", 
                                              choices = c("Confirmed Cases" = "Confirmed",
                                                          "Deaths" = "Deaths")),
                                 div(tabsetPanel(id = "InputTabsetPanel",
                                                 tabPanel("Plot Options", 
                                                          radioButtons("PlotType", "Plot Type:", choices = c("Line", "Bar")),
                                                          radioButtons("ScaleType", "Y-Axis Scale:", choices = c("Linear", "Log-10")))
                                                 ))
                             ),
                             mainPanel(
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Cumulative",
                                                      plotlyOutput("CumulativePlot", width = "100%")),
                                             tabPanel("Cumulative (Lagged)",
                                                      numericInput("LaggedDaysToShow", "Number of Days:", value = as.numeric(max(dfFull$Date) - min(dfFull$Date))),
                                                      plotlyOutput("LaggedCumulativePlot", width = "100%")),
                                             tabPanel("New Outcomes",
                                                      plotlyOutput("NewPlot", width = "100%"))
                                             )
                                 )
                             )
                )
                         # ),
                # tabPanel("World Map",
                #          sidebarLayout(
                #              sidebarPanel(
                #                  width = 3, 
                #                  dateRangeInput("DateRangeMap", "Date Range:", start = min(dfFull$Date), end = max(dfFull$Date)),
                #                  radioButtons("OutcomeMap", "Outcome:", 
                #                               choices = c("Confirmed Cases" = "Confirmed",
                #                                           "Deaths" = "Deaths", 
                #                                           "Recoveries" = "Recovered")),
                #                  radioButtons("MetricMap", "New or Cumulative:", choices = c("New", "Cumulative"))),
                #              mainPanel(
                #                  plotlyOutput("WorldMap", width = "100%"))
                #              )
                #          )
                )
    )

##### Define server logic #####
server <- function(input, output, session) {
    
    observe({
        LocationChoices <- if (input$DisplayRegions == "continent") { listContinents
        } else if (input$DisplayRegions == "sub_region") { listSubRegions
        } else if (input$DisplayRegions == "Country") { listCountries
        } else if (input$DisplayRegions == "World") { NULL }

        updatePickerInput(session, 'Locations', choices = LocationChoices)
    })
    
    output$GlobalConfirmedCount <- renderText({
        curDate <- max(dfFull$Date)
        paste0(prettyNum(sum(dfFull$Confirmed[dfFull$Date == curDate], na.rm=T), big.mark = ","), " Confirmed Cases")
    })
    
    output$GlobalDeathCount <- renderText({
        curDate <- max(dfFull$Date)
        paste0(prettyNum(sum(dfFull$Deaths[dfFull$Date == curDate], na.rm=T), big.mark = ","), " Deaths")
    })
    
    output$CurrentDate <- renderText({ paste("Data as of", format(max(dfFull$Date), "%d %B %Y")) })

    output$CumulativePlot <- renderPlotly({
        if (is.null(input$Locations) & input$DisplayRegions != "World") { return(NULL)}
        
        if (input$DisplayRegions == "World") { dfTmp = dfFull 
        } else { dfTmp = dfFull[dfFull[[input$DisplayRegions]] %in% input$Locations, ]
        }
        
        dfTmp <- dfTmp %>%
            group_by((!!sym(input$DisplayRegions)), Date) %>%
            summarise(Value = sum((!!sym(input$Outcome)), na.rm=T)) %>%
            filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
            
        PlotText = CreatePlotText(input$DisplayRegions, input$Outcome, input$PlotType, input$ScaleType, F, F)

        eval(parse(text = PlotText))
    })
    
    output$LaggedCumulativePlot <- renderPlotly({
        if (is.null(input$Locations) & input$DisplayRegions != "World") { return(NULL)}
        
        dfTmp <- LagOutcomeByLocation(location = input$DisplayRegions, metric = input$Outcome, minimum = 100)
    
        if (input$DisplayRegions != "World") { dfTmp = dfTmp[dfTmp[[input$DisplayRegions]] %in% input$Locations, ] }

        PlotText = CreatePlotText(input$DisplayRegions, input$Outcome, input$PlotType, input$ScaleType, T, F)
        
        if (nrow(dfTmp) == 0) { "Selected location does not have over 100 of the specified outcome"
        } else { eval(parse(text = PlotText))
        }
        
    })
    
    output$NewPlot <- renderPlotly({
        if (is.null(input$Locations) & input$DisplayRegions != "World") { return(NULL)}
        
        if (input$Outcome == "Confirmed") { valCol = "NewCases"
        } else if (input$Outcome == "Deaths") { valCol = "NewDeaths"
        } else if (input$Outcome == "Recovered") { valCol = "NewRecoveries"}
        
        if (input$DisplayRegions == "World") { dfTmp = dfFull 
        } else { dfTmp = dfFull[dfFull[[input$DisplayRegions]] %in% input$Locations, ]
        }
        
        dfTmp <- dfTmp %>%
            group_by((!!sym(input$DisplayRegions)), Date) %>%
            summarise(Value = sum((!!sym(valCol)), na.rm=T)) %>%
            filter(Date >= input$DateRange[1] & Date <= input$DateRange[2])
        
        PlotText = CreatePlotText(input$DisplayRegions, input$Outcome, input$PlotType, input$ScaleType, F, T)
        
        eval(parse(text = PlotText))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


