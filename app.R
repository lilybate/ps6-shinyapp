
library(shiny)
library(tidyverse)
data <- read_delim("UAH-lower-troposphere-long.csv.bz2")


ui <- fluidPage(
  titlePanel("Lily Bates, PS6"),
  tabsetPanel(
    tabPanel(
      "Introduction to the Data",
      mainPanel(
        h3("This app is looking at global tempatures deviations (celcius) 
        in different regions around the globe compares to the 1991-2020 baseline."),
        p("This dataset has", nrow(data), "observations for 27 regions"),
        em("Data collected from 1978 to 2023"),
        h3("Here is a random selection from the UAH data set"),
        dataTableOutput("sample"),
        p("I spent approx 6 hours on this problem set")
      )
    ),
    
  tabPanel(
      "Plot",
      sidebarLayout(
        sidebarPanel(
          p("You can observe and compare at the average change in tempature globally 
            in each of the 12 months."),
          checkboxGroupInput(
            "monthSelect",
            "Which month would you like to veiw?",
            choices = 1:12, 
            selected = 1
          ),
          radioButtons(
            "trendline",
            "Show data trendline?",
            choices = c("Yes", "No"),
            selected = "No"
          )
        ), mainPanel(
        plotOutput("graph"),
        textOutput("obs")
      ))
    ), 
  
tabPanel(
      "Data Table",
      sidebarLayout(
        sidebarPanel(
          p("Observe the average tempature deviations in the selected region. Choose two or more regions to 
            compare dataset averages."),
          checkboxGroupInput("region", "Choose region(s) to veiw",
                             choices = unique(data$region),
                             selected = "globe")
      ),
      mainPanel(
        textOutput("obs2"),
        dataTableOutput("table")
      )
    ))))



server <- function(input, output) {
  
output$sample <- renderDataTable({
  data %>% 
    sample_n(5)
})
                 
graphdata <- reactive({
  data %>% 
    filter(region == "globe") %>% 
    filter(month %in% input$monthSelect) 
})

output$graph<- renderPlot({
  if(input$trendline == "Yes")({
    ggplot(graphdata(), aes(x= year, y= temp, col= factor(month)))+
    geom_point()+
    ggtitle("Global Tempature Deviations from 1978-2022")+
    labs(x="Year", y="Tempature Deviation from Previous Year (C)")+
    geom_smooth(method = lm)}) else({
      ggplot(graphdata(), aes(x= year, y= temp, col= factor(month)))+
        geom_point()+
        ggtitle("Global Tempature Deviations from 1978-2022")+
        labs(x="Year", y="Tempature Deviation from Previous Year (C)")
        })
    }
)


output$obs <- renderPrint({
  data %>%
    filter(month %in% input$monthSelect) %>% 
    filter(region == "globe") %>%
    nrow() %>% 
    paste("There are", ., "non-missing observations in this graph")
})


output$table <- renderDataTable({
  data %>% 
    filter(region %in% input$region) %>% 
    group_by(region) %>% 
    summarise(mean(temp))
})

output$obs2 <- renderPrint({
  data %>%
    filter(region %in% input$region)  %>% 
    nrow() %>% 
    paste("There are", ., "temperture deviation observations for the selected region(s)")
})

}


shinyApp(ui = ui, server = server)

