
library(shiny)
library(rsconnect)
library(dplyr)
library(plotly)
library(shinythemes)
library(openintro)




df <- read.csv('./Data/cleaned-cdc-mortality-1999-2010-2.csv',header = T)
data <-  df %>% filter( df$Year == 2010 )  
data <- data %>% arrange(data$State ,data$Crude.Rate)



ui <- navbarPage('DATA-608',theme = shinytheme("united"),
        tabPanel( 'Question 1',
             headerPanel('(2010) Mortality rates by State'),
        sidebarPanel(
 
        selectInput('cause','Cause', data$ICD.Chapter,selected = T)
 
         ),
  
  mainPanel(
    plotlyOutput('plot')
    ) 
  ),
  tabPanel( 'Question 2', 
            headerPanel('Mortality rates by State vs Population Average'),
            sidebarPanel(
              
              selectInput('cause2','Cause', data$ICD.Chapter,selected = T),
              selectInput('state','State', data$State,selected = T)
              
            
                         ),
            mainPanel(
            textOutput('text'),
            plotlyOutput('plot2')
            )
            
            
              
          )

)


server <- function(input, output) {
  
  data.2 <- reactive(df %>% group_by(Year) %>% filter(ICD.Chapter == input$cause2) %>% summarise(sum(Crude.Rate) / (count = n())) %>%
    select('sum(Crude.Rate)/(count = n())')  %>% rename('population.avg' = 'sum(Crude.Rate)/(count = n())' ) %>% 
    cbind(filter(df,ICD.Chapter == input$cause2,State == input$state)) )
  
  
  
  output$plot <- renderPlotly({
    plot_ly(subset(data, data$ICD.Chapter == input$cause),x = ~State,y=~Crude.Rate , type='scatter',mode = 'lines+markers',
            line = list(color = 'orange', width = 2)) 
  })
  output$plot2 <- renderPlotly({
    plot_ly(data.2(),x = ~Year,y=~Crude.Rate , type='scatter',name = paste(abbr2state(input$state),'-Crude Rates'),mode = 'lines+markers',line = list(color = 'orange', width = 2))  %>% 
      add_trace(y = ~population.avg, name = 'Population AVG Crude Rates', mode = 'lines+markers')
  })
 output$text <- renderText({
   abbr2state(input$state)
 })
 
  
}

shinyApp(ui = ui, server = server)
