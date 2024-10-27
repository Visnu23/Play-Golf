#libraries
library(shiny)
library(shinythemes)
library(data.table)
library(readr)
library(randomForest)

#import csv
weather<- read_csv("D:/python 3.10.7/home work/documents/projects/R Projects/weather-weka.csv")


# Ensure all data is in the correct format
weather$temperature <- as.numeric(weather$temperature)
weather$humidity <- as.numeric(weather$humidity)
weather$outlook <- as.factor(weather$outlook)
weather$windy <- as.factor(weather$windy)
weather$play <- as.factor(weather$play)

# Check structure to confirm changes
str(weather)

#building the model
model<- randomForest(play ~ ., data=weather,ntree=500,mtry=4,importance=TRUE)

#save the model
#saveRDS(model,"model.rds")

#read in the RF model
#model<-readRDS("model.rds")

#ui component

ui<-fluidPage(theme = shinytheme("united"),
              #page header
              headerPanel('play Golf'),
              #input
              sidebarPanel(
                HTML("<h3>Input parameters</h3>"),
                selectInput("outlook",label = "Outlook:",
                            choices = list("Sunny"="sunny","Overcast"="overcast","Rainy"="rainy"),
                            selected = "Rainy"),
                sliderInput("temperature","Temperature:",
                            min=64,max=86,
                            value=70),
                sliderInput("humidity","Humidity:",
                            min=65,max=96,
                            value = 90),
                selectInput("windy",label = "Windy:",
                            choices = list("Yes"="TRUE","No"="FALSE"),
                            selected = "TRUE"),
                actionButton("submitbutton","Submit",class="btn btn-primary")
              ),
              
              mainPanel(
                tags$label(h3('Status/Output')),   #textbox
                verbatimTextOutput('contents'),
                tableOutput('tabledata')   #Prediction result table
              )
            )
# server component

server<-function(input,output,session){
  #input
  datasetInput<-reactive({
    # Create a data frame directly from the inputs
    test <- data.frame(
      outlook = factor(input$outlook, levels = c("overcast", "rainy", "sunny")),
      temperature = as.numeric(input$temperature),
      humidity = as.numeric(input$humidity),
      windy = factor(input$windy, levels = c("TRUE", "FALSE"))
    )
    
    # Ensure factor levels match those in the model's training data
    test$outlook <- factor(test$outlook, levels = levels(weather$outlook))
    test$windy <- factor(test$windy, levels = levels(weather$windy))
    
    # Prediction output with probability
    Output <- data.frame(Prediction = predict(model, test), 
                         Probability = round(predict(model, test, type = "prob"), 3))
    return(Output)
  })
  
  #Output text box
  output$contents<-renderPrint({
    if(input$submitbutton>0){
      isolate("Calculation complete.")
    }else{
        return("Server is ready for calculation.")
      }
  })
  #results
  output$tabledata<-renderTable({
    if (input$submitbutton>0){
      isolate(datasetInput())
    }
  })
}

#Shiny app component

shinyApp(ui=ui,server=server)



