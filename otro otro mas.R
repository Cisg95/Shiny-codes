
# SigmaNet ----------------------------------------------------------------

library(igraph)
library(sigmaNet)
library(shiny)
library(dplyr)
library(lubridate)
load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
d <- data.frame(from = class_mails$username_from,to=class_mails$username_to)
d <- data.frame(lapply(d, gsub, pattern = ".", replacement = " ", fixed = TRUE))
d[,1] <- capitalize(as.character(d[,1]))
d[,2] <- capitalize(as.character(d[,2]))
df <- data.frame(username_from = d$from,username_to = d$to)
df.g <- graph.data.frame(d = df, directed = F)
layout <- layout_with_fr(df.g)
sig <- sigmaFromIgraph(df.g, layout = layout)
sig


# Diagrammer --------------------------------------------------------------

install.packages("Hmisc")
library(dplyr)
library(magrittr)
library(shiny)
library(lubridate)
library(DiagrammeR)
library(Hmisc)

load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
nodes <- data.frame(nodos=union(unique(class_mails$username_from),unique(class_mails$username_to)))
nodes <- data.frame(id= rownames(nodes),nodes)
data <- class_mails[1:100,]


data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t)

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])

i_graph_1 <-
  create_graph()

i_graph_2 <-
  i_graph_1 %>%
  add_nodes_from_table(
    table = nodes,
    label_col = nodos)

i_graph_2[["nodes_df"]][["id_external"]] <- as.integer(i_graph_2[["nodes_df"]][["id_external"]])

i_graph_3 <-
  i_graph_2 %>%
  add_edges_from_table(
    table =  data ,
    from_col = id_f ,
    to_col = id_t,
    from_to_map = id_external)


i_graph_3 %>%
  get_edge_df() %>% View()

j_graph <- 
  i_graph_3 %>%
  drop_node_attrs(
    node_attr = id_external)

j_graph %>% render_graph()


# VisNetwork --------------------------------------------------------------

#install.packages("visNetwork")
library(lubridate)
require(visNetwork)
library(dplyr)
library(magrittr)
library(shiny)


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

nododos <- data.frame(id = nodes$id,
                      label = nodes$nodos)
edgeges <- data.frame(from = data$id_f, to = data$id_t)
visNetwork(nododos, edgeges)

# vivagRaph ---------------------------------------------------------------
#is not working, the package not found
library(lubridate)
devtools::install_github('keeganhines/vivagRaph')
library(vivagRaphjs)
load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
nodes <- data.frame(nodos=union(unique(class_mails$username_from),unique(class_mails$username_to)))
nodes <- data.frame(id= rownames(nodes),nodes)
data <- class_mails[1:100,]


data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t)

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])
Nodedes <- data.frame(id = nodes$id,
                   label = nodes$nodos)
Edgeges <- data.frame(from = data$id_f, to = data$id_t)
vivagRaph(nodes=Nodedes,edges=Edgeges)


# Plotly ------------------------------------------------------------------
#problems in the last part
install.packages("plotly")
library(plotly)
library(igraph)
library(sigmaNet)
library(shiny)
library(dplyr)
library(lubridate)
load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
d <- data.frame(from = class_mails$username_from,to=class_mails$username_to)
d <- data.frame(lapply(d, gsub, pattern = ".", replacement = " ", fixed = TRUE))
d[,1] <- capitalize(as.character(d[,1]))
d[,2] <- capitalize(as.character(d[,2]))
df <- data.frame(username_from = d$from,username_to = d$to)
df.g <- graph.data.frame(d = df, directed = F)
g <- upgrade_graph(df.g)#graph
l <- layout.circle(g)#layout

#Create Vertices and Edges
vs <- V(g)#vertices
es <- as.data.frame(get.edgelist(g))
Ne <- length(es[1]$V1)

#Create nodes
xn <- l[,2]
yn <- l[,1]

network <- plot_ly(x = ~xn, y = ~yn, mode = "markers", text = vs, hoverinfo = "text", type = "scatter")

#Create edges
edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = xn[v0],
    y0 = yn[v0],
    x1 = xn[v1],
    y1 = yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

#Create network
axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

p <- layout(
  network,
  title = 'Social Network',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

p

# Plotly try 2 - 3D -------------------------------------------------------
install.packages("network")
install.packages("devtools")
devtools::install_github("dgrapov/networkly")
library(networkly)
library(plotly)
library(dplyr)
library(magrittr)
library(shiny)

load(file = "W:/Camila Saavedra/Beta1 EMAILS/enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails <- class_mails[1:100,]
nodes <- data.frame(nodos=union(unique(class_mails$username_from),unique(class_mails$username_to)))
nodes <- data.frame(id= rownames(nodes),nodes)
data <- class_mails[1:100,]


data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t)

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])

#net params
type<-"3d"

#create network objects
obj<-get_network(data,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))

#add legend
legend<-format_legend(obj,node.data=node.data)

net<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) 

#add layout options
layout(net,
       scene = list(showlegend=TRUE,
                    yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                    xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                    zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))
