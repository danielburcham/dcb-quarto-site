# Load packages
library(bslib)
library(shiny)
library(tidyverse)

# Import and wrangle data
download.file("https://raw.githubusercontent.com/danielburcham/arbdatascience/master/Mikesell_Phenology_Data.csv", "Mikesell_Phenology_Data.csv")
phenoData <- read.csv("Mikesell_Phenology_Data.csv", header = TRUE, na.strings = "999") |>
  mutate(bud_burst = as.Date(bud_burst,format="%j"),
         first_leaf = as.Date(first_leaf,format="%j"),
         full_leaf = as.Date(full_leaf,format="%j"),
         open_flowers = as.Date(open_flowers,format="%j"),
         ripe_fruit = as.Date(ripe_fruit,format="%j"),
         colored_leaves = as.Date(colored_leaves,format="%j"),
         leaves_dropped = as.Date(leaves_dropped,format="%j"))

# Define user interface
ui <- page_sidebar(
  title = "Mikesell Phenology Data",
  sidebar = radioButtons("radio","Select phenophase",
                         choices = c("Breaking leaf buds" = "bud_burst",
                                     "First leaf" = "first_leaf",
                                     "Full leaf" = "full_leaf",
                                     "Open flowers" = "open_flowers",
                                     "Ripe fruit" = "ripe_fruit",
                                     "Colored leaves" = "colored_leaves",
                                     "Leaves dropped" = "leaves_dropped"),
                         selected = "bud_burst"),
  plotOutput("phenophasePlot",width="100%")
)

# Define server function
server <- function(input, output) {
  
  dataInput <- reactive({
    switch(input$radio,
           bud_burst = phenoData[,c("year","species","bud_burst")],
           first_leaf = phenoData[,c("year","species","first_leaf")],
           full_leaf = phenoData[,c("year","species","full_leaf")],
           open_flowers = phenoData[,c("year","species","open_flowers")],
           ripe_fruit = phenoData[,c("year","species","ripe_fruit")],
           colored_leaves = phenoData[,c("year","species","colored_leaves")],
           leaves_dropped = phenoData[,c("year","species","leaves_dropped")])
  })
  
  output$phenophasePlot <- renderPlot({
    ggplot(dataInput(), aes(.data[[input$radio]],reorder(species, .data[[input$radio]], FUN = median, na.rm = TRUE))) +
      geom_violin(aes(fill = reorder(species, .data[[input$radio]], FUN = median, na.rm = TRUE)), show.legend = FALSE, na.rm = TRUE) +
      stat_summary(fun = "median", size = 3.5, geom = "point", na.rm = TRUE) +
      geom_jitter(aes(alpha = year), height = 0, width = 0.2, na.rm = TRUE) +
      labs(
        x = "Date",
        y = "Species",
        alpha = "Year"
      ) +
      scale_y_discrete(limits=rev) + 
      theme(axis.text.y = element_text(face="italic",size=12),
            axis.text.x = element_text(size=12),
            axis.title = element_text(face="bold",size=13),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13),
            panel.background = element_rect(fill="white"),
            panel.grid.major = element_line(color="gray93"))
  }, res=75)
}

# Create Shiny App
shinyApp(ui, server)
