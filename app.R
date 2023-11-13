#Dynamic Plots

#Load required libraries
library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(rsconnect)

#Load data
youtube <- read_csv("~/github/users/adamcp2013/YouTube/main/data/Global YouTube StatisticsSubset.csv")
quartiles<-quantile(youtube$`video views`, probs = c(.25, .75))
IQR<-IQR(youtube$`video views`)
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
data_no_outlier<-subset(youtube,youtube$`video views`>Lower & youtube$`video views`<Upper)

quartiles<-quantile(data_no_outlier$`uploads`, probs = c(.25, .75))
IQR<-IQR(data_no_outlier$`uploads`)
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
data_no_outlier_uploads<-subset(data_no_outlier,data_no_outlier$`uploads`>Lower & data_no_outlier$`uploads`<Upper)

data_no_outlier2<-subset(data_no_outlier, data_no_outlier$video_views_for_the_last_30_days != "NaN")
quartiles<-quantile(data_no_outlier2$video_views_for_the_last_30_days, probs = c(.25, .75))
IQR<-IQR(data_no_outlier2$video_views_for_the_last_30_days)
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
data_no_outlier_30_days<-subset(data_no_outlier2,data_no_outlier2$video_views_for_the_last_30_days>Lower & data_no_outlier2$video_views_for_the_last_30_days<Upper)

# Define UI ----
ui <- fluidPage(
  titlePanel("YouTube Data 2023"),
  
  sidebarLayout(
    sidebarPanel(
        h2("User Controls"),
        
        selectInput("var", 
                    label = "Choose a variable to display",
                    choices = c("Video Views", 
                                   "Uploads",
                                   "Video Views Last 30 Days"), 
                    selected = "Video Views"),
        
        img(src = "rstudio.png", height = 50, width = 150),
        br(),
        br(),
        "Shiny is a product of ", 
        span("RStudio", style = "color:blue"),
    ),
    mainPanel(
      h2("Visualize the Data"),
      img(src="youtube.png", height = 70, width = 200),
      plotOutput("myplot")
    )
  ) 
)

# Define server logic ----
server <- function(input, output) {
  output$myplot <- renderPlot({
    data <- switch(input$var,
                   "Video Views Last 30 Days" = data_no_outlier_30_days$video_views_for_the_last_30_days,
                   "Uploads" = data_no_outlier_uploads$uploads,
                   "Video Views" = data_no_outlier$`video views`)
    dataset <- switch(input$var,
                      "Video Views Last 30 Days" = data_no_outlier_30_days,
                      "Uploads" = data_no_outlier_uploads,
                      "Video Views" = data_no_outlier)
    xvar <- switch(input$var,
                      "Video Views Last 30 Days" = data_no_outlier_30_days$category,
                      "Uploads" = data_no_outlier_uploads$category,
                      "Video Views" = data_no_outlier$category)
    ggplot(dataset, aes(x=xvar, y=data, fill=xvar)) +
      geom_boxplot() +
      ggtitle(input$var, "by Category")+
      labs(x="YouTube Channel Categories", y=input$var)+
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), legend.position="none") 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

