#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(visNetwork)
library(Hmisc)

file_path <- "C:/Users/cristian.acuna/cacunas/Mindless Machine/Cristian Acuna/"
file_path %<>% paste0("pocs/SMART/data/enron_training.Rdata")

load(file = file_path)

class_mails$Date %<>% as.POSIXct(.)

# Split the receptor ------------------------------------------------------
class_mails$To <- ifelse(class_mails$To == "", "Unknown", class_mails$To)
sep_col <- function(){

  for (i in 1:2111) {
    separated <- strsplit(class_mails$To,",")[[i]]
    n <- separated %>% NROW
    vector <- rep("NA",5)
    vector[1:n] <- strsplit(class_mails$To,",")[[i]]

    if (i == 1) {
      df <-  vector %>% t() %>% as.data.frame
    }else{
      vector %<>% t() %>% as.data.frame
      df %<>% rbind(vector)
    }
  }
  return(df)
}

df <- sep_col()
df %<>% cbind(ID = class_mails$ID)

df %<>% gather(key=var_,value = c(V1, V2, V3, V4, V5),-ID)
names(df)[3] <- "to_single"
df %<>% filter(nchar(to_single)>2)
df[2] <-  NULL
mails <- merge(x = class_mails, y = df, by = "ID")
mails <- mails[1:1200,]

nodes <- data.frame(nodos=union(unique(mails$From),unique(mails$to_single)))

# Cleaning data (names) ---------------------------------------------------
nodes$nodos <- sub("@.*", "", nodes$nodos)


# Making nice ------------------------------------------------

nodes <- data.frame(id= rownames(nodes),nodes)
data <- mails[1:1200,]

data %<>% mutate(id_f=match(data$username_from,nodes$nodos))
data %<>% mutate(id_t=match(data$username_to,nodes$nodos))
data %<>% select(id_f,id_t,is_suspiscius) %>%  unique()

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])
data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$mailHist <- renderPlot(
    {
      filter_mail <- class_mails %>% filter(From == input$User)

      filter_mail$Month_Name <- month.abb[filter_mail$Month]

      if (input$range == "Among Year") {
        x <- filter_mail$Month_Name %>%
          ordered(levels = c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec"))
        y <- filter_mail %>% filter(is_suspiscius == 1)
        y <- y$Month_Name %>%  ordered(levels = c("Jan","Feb","Mar",
                              "Apr","May","Jun",
                              "Jul","Aug","Sep",
                              "Oct","Nov","Dec"))
      }

      if (input$range == "Among Month") {
        x <- filter_mail$Day %>% ordered(levels = seq(max(31)))
        y <- filter_mail %>% filter(is_suspiscius == 1)
        y <- y$Day %>% ordered(levels = seq(max(31)))
      }

      if (input$range == "Among Week") {
        x <- filter_mail$Weekday %>%
          ordered(levels = c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday",
                    "Sunday"))
        y <- filter_mail %>% filter(is_suspiscius == 1)
        y <- y$Weekday %>%
          ordered(levels = c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday",
                    "Sunday"))
      }

      if (input$range == "Among Day") {
        x <- filter_mail$Hour %>% ordered(levels = seq(max(24)))
        y <- filter_mail %>% filter(is_suspiscius == 1)
        y <- y$Hour %>% ordered(levels = seq(max(24)))
      }

      #barplot(height=table(y),
      #      #col = "#75AADB",
      #      border = "white",
      #      xlab = input$range %>% str_sub( start = 7, end = -1))

      barplot(
        height = table(x),
        col = "#75AADB",
        border = "white",
        xlab = input$range %>% str_sub( start = 7, end = -1))

  })

  #The datas with the nodes and edges
  nododos <- data.frame(id = as.integer(nodes$id),
                        label = nodes$nodos)

  edgeges <- data.frame(from = data$id_f, to = data$id_t, is_s = data$is_suspiscius, color = data$color)

  output$network_proxy_nodes <- renderVisNetwork({

    #Filtering the social network by worker
    id_selected <- nododos %>%  filter(input$worker == label) %>% select(id) %>%  as.integer()
    edge <- edgeges %<>%  filter(id_selected == from | id_selected == to)
    node <- data.frame(id=union(unique(edge$from),unique(edge$to)))
    node <- left_join(node,nododos)
    second_level <- node[!(node$id == id_selected),]
    second_level_edges <- edgeges[edgeges$from == second_level$id,]
    second_level_edges <- second_level_edges[!(second_level_edges$is_s == 0),]
    sec_edge <- rbind(second_level_edges,edge)
    node <- data.frame(id=union(unique(sec_edge$from),unique(sec_edge$to)))
    node <- left_join(node,nododos)

    #Creating the plot
    visNetwork(node, sec_edge, main = "Social Network Bank", width = "100%") %>%
      visNodes(color = list(background = "lightblue", highlight = 'pink')) %>%  visOptions(highlightNearest = T)
  })

})
