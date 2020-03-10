library(tidyr)
library(shiny)
library(rsconnect)
library(dplyr)
library(plotly)
library(shinythemes)
library(openintro)


rsconnect::setAccountInfo(name='anthony-m',
                          token='E05F98E148F47EA3F9CD950CE9235DB9',
                          secret='MvbJwHFNGN27ixLX/kB0zbqDLh2Monn/SbOzGKFg')


df <- read.csv('https://raw.githubusercontent.com/Anth350z/Data-608-module-3/master/cleaned-cdc-mortality-1999-2010-2.csv',header = T)
data <-  df %>% filter( df$Year == 2010 )  
data <- data %>% arrange(data$Crude.Rate)

data2 <- df %>% complete(ICD.Chapter,State, Year = full_seq(1999:2010, T)) %>% 
  fill(0)



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
                             
                             selectInput('cause2','Cause', df$ICD.Chapter,selected = T),
                             selectInput('state','State', df$State,selected = T)
                             
                             
                           ),
                           mainPanel(
                             textOutput('text'),
                             plotlyOutput('plot2')
                           )
                           
                           
                           
                 )
                 
)


server <- function(input, output) {
  
  data.2 <- reactive(data2 %>% group_by(Year) %>% filter(ICD.Chapter == input$cause2) %>% summarise(sum(Crude.Rate ,na.rm = T) / sum(complete.cases(Crude.Rate))) %>%
                       select('sum(Crude.Rate, na.rm = T)/sum(complete.cases(Crude.Rate))')  %>% 
                       rename('population.avg' = 'sum(Crude.Rate, na.rm = T)/sum(complete.cases(Crude.Rate))' ) %>% 
                       cbind(filter(data2,ICD.Chapter == input$cause2,State == input$state))  )
  
  
  
  output$plot <- renderPlotly({
    plot_ly(subset(data, data$ICD.Chapter == input$cause),x = ~abbr2state(State),y=~Crude.Rate , type='scatter',mode = 'lines+markers',
            line = list(color = 'orange', width = 2) ) %>% layout( xaxis = list(categoryorder = "array",
                                                                                categoryarray = data$State,tickangle=60,title="States"))
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
