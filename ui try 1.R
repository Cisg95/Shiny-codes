library(shiny)
library(visNetwork)
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)

load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
nodes <- data.frame(nodos=union(unique(class_mails$username_from),unique(class_mails$username_to)))
nodes <- data.frame(id= rownames(nodes),nodes)
data <- class_mails[1:100,]

data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t,is_suspiscius)

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])
data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue") 

# SHINY APP ---------------------------------------------------------------
ui <- fluidPage(

     mainPanel(visNetworkOutput("network_proxy_nodes"))
   
)

server <- function(input, output, session) {
  
  output$network_proxy_nodes <- renderVisNetwork({
    
    nododos <- data.frame(id = as.integer(nodes$id),
                          label = nodes$nodos) 
    edgeges <- data.frame(from = data$id_f, to = data$id_t, is_s = data$is_suspiscius, color = data$color)
    visNetwork(nododos, edgeges,main = "Social Network Bank", width = "100%") %>%
      visNodes(color = list(background = "lightblue", highlight = 'pink')) %>% visOptions(highlightNearest = T,nodesIdSelection = T, manipulation = T)
    })

}

shinyApp(ui=ui, server =server)
