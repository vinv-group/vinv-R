library(leaflet)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyglide)
library(bslib)

navbarPage("vinv R", id="nav",
           theme = bs_theme(version = 4, bootswatch = "minty"),
           tabPanel("Map",
                    div(class="outer",
                        useShinyjs(),
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            #includeScript("gomap.js")
                        ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput(outputId = "map", width="100%", height="100%"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = FALSE, top = 20, left = "auto", right = 20, bottom = "auto",
                          width = 330,
                          
                          fluidRow(
                            column(12, align="center",
                                   img(src='img/logo_transparent.png', align = "center", width= "75"),
                            )
                          ),
                          fluidRow(
                            column(12, align="center",
                                    htmlOutput("vinv")
                            ),
                          ),
                          fileInput(
                              "vinvfile",
                              "",
                              multiple = FALSE,
                              accept = ".vinv",
                              width = NULL,
                              buttonLabel = "Browse...",
                              placeholder = "...select .vinv file"
                          ),
                          htmlOutput("exampleFile"),
                          
                          #actionLink("sdfsf", "vinv.io"),
                          
                          
                          conditionalPanel(
                            condition = "output.vinvHasData === true",
                            hr(),
                            verbatimTextOutput("flexCoordinates"),
                            verbatimTextOutput("fixedCoordinates"),
                            fluidRow(
                              column(5, align="center",
                                h6("Circle"),
                              ),
                              column(2,
                                     materialSwitch(
                                       inputId = "useH3",
                                       status = "primary",
                                       
                                       right = TRUE
                                     )
                              ),
                              
                              column(5, align="center",
                                h6("H3"),
                              ),
                            ),
                            conditionalPanel(
                              condition = "input.useH3 == false",
                              sliderInput("radius", "Radius in meter:",
                                          min = 5, max = 500,
                                          value = 50),
                            ),
                            conditionalPanel(
                              condition = "input.useH3 == true",
                              sliderInput("h3res", "H3 Resolution:",
                                          min = 7, max = 12,
                                          value = 10),
                            ),
                            
                            textOutput("radiusInHa"),
                            hr(),
                            textOutput("plotsInRadius"),
                            textOutput("dataInRadius"),
                            
                            
                            conditionalPanel(
                              condition = "output.positionSelected == true",
                              h6('Group by'),
                              awesomeCheckbox(
                                inputId = "groupBySpecies",
                                label = "Species",
                                value = TRUE,
                              ),
                              hr(),
                              glide(
                                height = "250px",
                                screen(
                                  plotOutput("plotHeight",  width = "100%", height = "250")
                                ),
                                screen(
                                  plotOutput("plotDBH",  width = "100%", height = "250")
                                ),
                                screen(
                                  plotOutput("plotVol",  width = "100%", height = "250")
                                )
                              )
                            )
                          )
                        )
                    )
           ),

           tabPanel("Data explorer",
              # conditionalPanel(
              #   condition = "vinv.hasData == true",
              #   DT::dataTableOutput("treeTable")
              # ),
              DT::dataTableOutput("treeTable")
           ),

           conditionalPanel("false", icon("crosshair"))
)
