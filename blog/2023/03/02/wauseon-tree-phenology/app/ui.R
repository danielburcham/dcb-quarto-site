library(bslib)
library(shiny)

ui <- page_sidebar(
  title = "Mikesell Phenology Data",
  sidebar = radioButtons("radio","Select phenophase",
                         choiceNames = c("Breaking leaf buds",
                                         "First leaf",
                                         "Full leaf",
                                         "Open flowers",
                                         "Ripe fruit",
                                         "Colored leaves",
                                         "Leaves dropped"),
                         choiceValues = c("bud_burst",
                                          "first_leaf",
                                          "full_leaf",
                                          "open_flowers",
                                          "ripe_fruit",
                                          "colored_leaves",
                                          "leaves_dropped"),
                         selected = "bud_burst"),
  plotOutput("phenophasePlot",width="100%",height="600px")
)