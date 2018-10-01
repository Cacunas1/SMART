#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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

df %<>% gather(key = var_,value = c(V1, V2, V3, V4, V5),-ID)
names(df)[3] <- "to_single"
df %<>% filter(nchar(to_single) > 2)
df[2] <-  NULL
mails <- merge(x = class_mails, y = df, by = "ID")
mails <- mails[1:1200,]

nodes <- data.frame(nodos = union(unique(mails$From),unique(mails$to_single)))

# Cleaning data (names) ---------------------------------------------------
nodes$nodos <- sub("@.*", "", nodes$nodos)


# Making nice ------------------------------------------------

nodes <- data.frame(id = rownames(nodes),nodes)
data <- mails[1:1200,]

data %<>% mutate(id_f = match(data$username_from,nodes$nodos))
data %<>% mutate(id_t = match(data$username_to,nodes$nodos))
data %<>% select(id_f, id_t, is_suspiscius) %>%  unique()

nodes[-1] <- lapply(nodes[-1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,2] <- capitalize(nodes[,2])
data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("SMART Viz"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "User",
        label = "Select User",
        choices = (class_mails$From) %>% unique(),
        selected = "christian.yoder@enron.com"
      ),
      selectInput(
        inputId = "range",
        label = "Range",
        choices = c("Among Year", "Among Month", "Among Week", "Among Day"),
        selected = "Among Year"
      ),
      selectInput(
        inputId = "worker",
        label = "Select worker:",
        choices = nodes$nodos,
        selected = "Greg piper",
        multiple = FALSE
      ),
      width = 4
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("mailHist"),
       visNetworkOutput("network_proxy_nodes"),
       DT::dataTableOutput(outputId = "suspiscius_table")
    )
  )
))
