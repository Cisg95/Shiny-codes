library(shiny)
library(visNetwork)
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(Hmisc)

load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms

# Split the receptor ------------------------------------------------------

class_mails$To<- ifelse(class_mails$To=="","Unknown",class_mails$To)
sep_col <- function(){
  
  for (i in 1:2111){
    separated <- strsplit(class_mails$To,",")[[i]]
    n <- separated %>% NROW
    vector <- rep("NA",5)
    vector[1:n] <- strsplit(class_mails$To,",")[[i]]  
    
    if (i==1){
      df <-  vector %>% t() %>% as.data.frame
    }else{
      vector %<>% t() %>% as.data.frame
      df %<>% rbind(vector)
    }
  }
  return(df)
}
df <- sep_col()
df  %<>%  cbind(ID=class_mails$ID)

df %<>%  gather(key=var_,value = c(V1, V2, V3, V4, V5),-ID)
names(df)[3] <- "to_single"
df %<>% filter(nchar(to_single)>2)
df[2] <-  NULL
mails<- merge(x = class_mails, y = df, by = "ID")
mails <- mails[1:1200,]

nodes <- data.frame(nodos=union(unique(mails$From),unique(mails$to_single)))
nodes["Emails"] <- nodes
# Cleaning data (names) ---------------------------------------------------
nodes$nodos <- sub("@.*", "", nodes$nodos)


# Making nice ------------------------------------------------

nodes <- data.frame(id= rownames(nodes),nodos= nodes$nodos, emails= nodes$Emails)
data <- mails[1:1200,]

data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t,is_suspiscius) %>%  unique()

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])
data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue") 
# SHINY APP ---------------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    # Input(s)
    sidebarPanel(
      # Explanatory text
      HTML(paste0("Network Filter tool")),
      # Break for visual separation
      br(), br(),
      selectInput(inputId = "worker",
                  label = "Select worker:",
                  choices = nodes$nodos,
                  selected = "Paul kaufman",
                  multiple = FALSE)
    ),
    mainPanel(visNetworkOutput("network_proxy_nodes"))
  )
)

server <- function(input, output) {
  #The datas with the nodes and edges    
  nododos <- data.frame(id = as.integer(nodes$id),
                        label = nodes$nodos)
  
  edgeges <- data.frame(from = data$id_f, to = data$id_t, is_s = data$is_suspiscius, color = data$color)
  edgeges <- edgeges[order(edgeges$is_s, decreasing = TRUE), ]
  
  edges0 <- edgeges %>% group_by(from, to) %>% summarise(value=n())
  
  edges1 <- distinct(edgeges, from, to, .keep_all = T)
  
  edges3 <- as.data.frame(merge(edges1,edges0))
  
  output$network_proxy_nodes <- renderVisNetwork({
 
    #Filtering the social network by worker
    id_selected <- nododos %>%  filter(input$worker == label) %>% select(id) %>%  as.integer()
    edge <- edges3 %>%  filter(id_selected == from | id_selected == to)
    node <- data.frame(id=union(unique(edge$from),unique(edge$to)))
    node <- left_join(node,nododos)
    second_level <- node[!(node$id == id_selected),]
    second_level_edges <- filter(edges3,from %in% second_level$id)
    second_level_edges <- second_level_edges[!(second_level_edges$is_s == 0),]
    sec_edge <- rbind(second_level_edges,edge)
    node <- data.frame(id=union(unique(sec_edge$from),unique(sec_edge$to)))
    node <- left_join(node,nododos)
    
    #Creating the plot
    visNetwork(node, sec_edge, main = "Social Network Bank", width = "100%") %>%
      visNodes(color = list(background = "lightblue", highlight = 'pink')) %>% 
      visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 2, type = 'arrow', value = sec_edge$count))) %>% 
      visOptions( highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = FALSE, hover = TRUE),nodesIdSelection = T) 
  })
  
  
}

shinyApp(ui=ui, server =server)
