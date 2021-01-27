library(ggplot2)
library(dplyr)

cdata <- read.csv("DailyCasesCovid19.csv", header = TRUE, sep=",")

# User interface
ui <- fluidPage(
  titlePanel("Simple COVID-19 Data Visualization in Malaysia"),
  sidebarPanel(img(src="logo.png", height=120, width= 320),
               h3("WHAT IS COVID-19?"), h4("Coronavirus disease (COVID-19) is an infectious disease caused by a newly
                                           discovered coronavirus. Most people who fall sick with COVID-19 will experience
                                           mild to moderate symptoms and recover without special treatment."),
               
               h3("Most common symptoms:"), h5("-Fever"), h5("Dry cough"), h5("Tiredness"),
               h3("How to prevent the spread?"), h5("-Clean or sanitize your hands often."),
               h5("Maintains 1m of social distancing."), h5("Always wear your mask."),
               img(src="wabak.png", height=120, width =300)),
  sidebarPanel(
    h3("What do you want to know?"),
    h3("Let's Explore!"),
    h4("Cumulative Cases of COVID-19 in Malaysia"),
   selectInput("sel_State", label="Select the States", choices = unique(cdata$State), multiple = TRUE, selected = "WP Kuala Lumpur"),
  h6("Last updated on Jan 28, 2021")
   ), mainPanel(plotOutput("bar_plot")),
  #sidebarPanel(img(src="transmission.png", height=440, width =380), img(src="wabak.png", height=120, width =300))

  
)


server <- function(input, output) {
  output$bar_plot <- renderPlot({
    cdata = read.csv("DailyCasesCovid19.csv", header = TRUE, sep=",")
    
    #Summarize Data and Plot Chart
    data <- reactive({
      req(input$sel_State)
      df <- cdata %>% filter(State %in% input$sel_State) %>% group_by(State)  %>% summarize(Cumulative=sum(Cumulative)) 
    })
    
    
    #plot
    #barplot(df$Cumulative)
    output$bar_plot <- renderPlot({
      g<- ggplot(data(), aes(y= Cumulative, x=State))
      g + geom_bar(stat="sum") + geom_text(aes(label=Cumulative),
                                           position=position_dodge(width=0.9), vjust=-0.8) + 
        labs(title="Interstate Comparison", 
             y="Number of Cases")
        })
    
    
  })
  
}



shinyApp(ui = ui, server = server)



