library(lubridate)
library(readxl)
library(shiny)
library(showtext)
library(tidyverse)

# Import data ----
phenoData <- read_excel("data/Mikesell_Phenology_Data.xlsx",sheet=1,na="999")
phenoData$species <- as.factor(phenoData$species)

server <- function(input, output) {
  output$phenophasePlot <- renderPlot({
    data <- switch(input$radio,
                   "bud_burst" = phenoData[,c("year","species","bud_burst")] |>
                     mutate(bud_burst = as.Date(paste(bud_burst,year),format="%j")),
                   "first_leaf" = phenoData[,c("year","species","first_leaf")] |>
                     mutate(first_leaf = as.Date(paste(first_leaf,year),format="%j")),
                   "full_leaf" = phenoData[,c("year","species","full_leaf")] |>
                     mutate(full_leaf = as.Date(paste(full_leaf,year),format="%j")),
                   "open_flowers" = phenoData[,c("year","species","open_flowers")] |>
                     mutate(open_flowers = as.Date(paste(open_flowers,year),format="%j")),
                   "ripe_fruit" = phenoData[,c("year","species","ripe_fruit")] |>
                     mutate(ripe_fruit = as.Date(paste(ripe_fruit,year),format="%j")),
                   "colored_leaves" = phenoData[,c("year","species","colored_leaves")] |>
                     mutate(colored_leaves = as.Date(paste(colored_leaves,year),format="%j")),
                   "leaves_dropped" = phenoData[,c("year","species","leaves_dropped")] |>
                     mutate(leaves_dropped = as.Date(paste(leaves_dropped,year),format="%j")))
    
    
    ggplot(data, aes(.data[[input$radio]],reorder(species, .data[[input$radio]], FUN = median, na.rm = TRUE))) +
      geom_violin(aes(fill = species), show.legend = FALSE, na.rm = TRUE) + 
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
            legend.title = element_text(size=13))
  })
}