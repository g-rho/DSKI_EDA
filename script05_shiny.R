# install.packages("shiny")
# library(shiny)

### exercise 1:

# open file -> new file -> shiny web app
# ...add name and chose a directory
# ...click create
# ...click run app

# Every shiny app has:
# - ...a ui where the layout of browser page is created
# - ...a server that contains the code to be executed. 
# - ln.49: shinyApp(ui = ui, server = server) starts the application.


### exercise 2:
# --> Task:
# Replace the ui (ln. 13-33) by...:

ui <- fluidPage(
  sliderInput(inputId = "Zahl",
              label = "Legen Sie eine Anzahl fest.",
              value = 500,
              min = 25,
              max = 1000),
  plotOutput("hist")
  )

# ...and replace the server (formerly ln. 36-46, now ln. 23-33) by...:
server <- function(input, output){
  output$hist <- renderPlot({ hist(runif(input$Zahl, min = 0, max = 1),
                                   xlab = "Wert",
                                   ylab = "Häufigkeit",
                                   main = paste(input$Zahl, "Zufallswerte zwischen 0 und 1")
                                   )
  })
}

# ...and reload the app!
# --> Take a look at the server function: Describe what the code of the server function does!
# --> Which roles do the objects input and out put take?
# --> Where do you find the InputId "Zahl" and "hist" in the plotOutput from the ui in the server?
# --> ...Note the double brackets ({}) which relate to a phenomeon called reactivity.

### exercise 3:
# Replace the server (ln. 23-30) by...:
server <- function(input, output){
  histdata <- reactive({
  runif(input$Zahl, min = 0, max = 1)  
  })
  
  output$hist <- renderPlot({
    hist(histdata(),
         xlab = "Wert",
         ylab = "Häufigkeit",
         main = paste(input$Zahl, "Zufallswerte zwischen 0 und 1")
         )
  })output$mean <- renderText({
  paste("Mittelwert = ", 
        round(mean(histdata()), 3)
        )
})

output$median <- renderText({
  paste("Median = ", 
        round(median(histdata()), 3)
  )
})

output$sd <- renderText({
  paste("Standardabweichung = ", 
        round(sd(histdata()), 3)
  )
})

}

# ...Note the double brackets ({}). 
# ...By reactive({})an object (histdata) is created in a 'reactive' context, 
# ...i.e. it reacts to user input (here: input$Zahl).
# Further note that renderPlot({}) as used before is also reactive.

# --> Add...
textOutput("mean"),
textOutput("median"),
textOutput("sd")
# ...to the ui and...:




### exercise 4: Create new app the does an interactive data analysis of the airquality data.
#   --> Task: Not only run the code but try to understand what happens in the different lines.

library(shiny)
library(datasets)
library(tidyr)
library(tibble)
library(ggplot2)

# remove NAs
aq.no.missing   <- drop_na(airquality)
# create table for user based selection of variables
options         <- c("Ozon (ppb, Teile pro Milliarde)" = "Ozone",
                     "Sonne (Langley" = "Solar.R",
                     "Wind (MPH)" = "Wind",
                     "Temperatur (F)" = "Temp")
# turn it into a data frame...
df.options      <- data.frame(options) 
# ...note that the names are assigned to the rownames of the df.options
# create a new column from the rownames
df.lv           <- rownames_to_column(df.options)
# assign column names to df.lv
colnames(df.lv) <- c("label", "value")  

ui <- fluidPage(
  selectInput("X", "X Variable:", options),
  selectInput("Y", "Y Variable:", options),
  plotOutput("scatter")
)

server <- function(input, output){
  selections <- reactive({
    aq.no.missing[,c(input$X, input$Y)]
  })
  
  output$scatter <- renderPlot({
    x_column    <- selections()[,1]
    y_column    <- selections()[,2]
    correlation <- cor(x_column, y_column)
    regression  <- lm(y_column ~ x_column)
    intercept   <- regression$coefficients[1]
    slope       <- regression$coefficients[2]
    x_label     <- df.lv$label[which(df.lv$value == input$X)]
    y_label     <- df.lv$label[which(df.lv$value == input$Y)]
    
    ggplot(selections(), aes(x = x_column, y = y_column)) + 
      geom_point(size = 3) + 
      labs(x = x_label, y = y_label,
           title = paste(y_label," vs. ",x_label, "\n r = ", round(correlation, 3),
                         "Y' =", round(intercept, 3), " + ", round(slope, 3), " X")
      ) + 
      theme(axis.title.x = element_text(size = 18),
            axis.text.x  = element_text(size = 17),
            axis.title.y = element_text(size = 18),
            axis.text.y  = element_text(size = 17),
            plot.title   = element_text(hjust = 0.5, size = 20)
      ) +
      geom_smooth(method = "lm", col = "blue") # add regression line to plot
    
    ## this is how the plot could be created using base R
    # plot(x = x_column, y = y_column,
    #      xlab = x_label, ylab = y_label,
    #      cex.axis = 1.5, cex.lab = 1.5, pch = 20, cex = 2, cex.main = 1.8,
    #      main = paste(y_label," vs. ",x_label, "\n r = ", round(correlation, 3),
    #                   "Y' =", round(intercept, 3), " + ", round(slope, 3), " X")
    #      )
    # abline(intercept, slope)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)






### exercise 5: 
# Create a new shiny app the visualizes the law of lure numbers:
# - In the ui the user can choose how often a dice should be rolled.
# - In the server simulate rolling a dice several times and count the number of sixes.
# - Finally visualize the relative frequency of sixes as a function of how often the dice has been rolled.  


## References:
# https://shiny.rstudio.com/gallery/
# https://shiny.rstudio.com/tutorial/
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf