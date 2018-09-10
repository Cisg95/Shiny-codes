# Clean and set up environment --------------------------------------------

rm(list = ls())

setwd("W:\\Camila Saavedra\\Beta1 EMAILS")

#install.packages("githubinstall") 

library(shiny)
library(dplyr)
library(DT)
library(sigmaNet)
library(igraph)
load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)


# Plots -------------------------------------------------------------------

from_to <- data.frame(class_mails$username_from,class_mails$username_to,class_mails$is_suspiscius)
mails.g <- graph.data.frame(d = from_to, directed = T)
fix(mails.g)

layout <- layout_with_fr(mails.g)
social_net<- sigmaFromIgraph(mails.g, layout = layout)

n_total <- nrow(class_mails)

# Define UI for application that plots features of movies
ui <- fluidPage(
  sigmaNetOutput('network', height = '600px'),
  textOutput('text')
)

server <- function(input, output) {
  output$text <- renderText({
    req(input$name)
    paste0('You clicked on: ', input$name)
  })
  
  output$network <- renderSigmaNet({
    social_net
  })
}

shinyApp(ui, server)