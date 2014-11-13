library(shiny)
library(markdown)
library(ggmap)
library(shinyIncubator)
# RENDER EQUILIBRIUM USER INTERFACE
renderFleetDynamics <- function(prefix)
{
  
  wellPanel(
    fluidRow(
      # column(6,
      # these need to be based on the life history tab
      sliderInput(paste0(prefix,"_","num_fisher"),"Number of fishers",min=5,max=50,value=10,step=1),
      sliderInput(paste0(prefix,"_","size_limit"),"Min & Max size limit (inches)",min=0,max=100,value=c(32,100),step=1),
      numericInput(paste0(prefix,"_","season"), label = "Months the fishery is open", value = 8),
      numericInput(paste0(prefix,"_","price"), label = "Price", value = 8),
      numericInput(paste0(prefix,"_","travelCost"), label = "Cost to travel one unit", value = 8),
      numericInput(paste0(prefix,"_","fishCost"), label = "Cost to fish in a timestep", value = 8),
      numericInput(paste0(prefix,"_","densCost"), label = "Increase in cost as density decreases", value = 8)      
      
    )
  )
  
}
progressInit()
# RENDER LAYOUT CONTROLS FOR TUPLIP PLOTS
renderLayoutInputs <- function()
{
  wellPanel(
    fluidRow(
      column(3,
             selectInput("icolor","Facet color",
                         c(None = ".",
                           "Scenario",
                           "Procedure",
                           "gear",
                           "area",
                           "sex",
                           "group"),
                         selected=".")
      ),
      
      column(3,
             selectInput("facet_row","Facet row",
                         c(None = ".",
                           "Scenario",
                           "Procedure",
                           "gear",
                           "area",
                           "sex",
                           "group"),
                         selected="Procedure")
      ),
      
      column(3,
             selectInput("facet_col","Facet column",
                         c(None = ".",
                           "Scenario",
                           "Procedure",
                           "gear",
                           "area",
                           "sex",
                           "group"),
                         selected="Scenario")
      ),
      
      column(3,
             selectInput('plotType',"Facet variable",
                         c( "Spawning biomass",
                            "Depletion",
                            "Catch",
                            "Sub-legal Catch",
                            "AAV in Catch",
                            "Wastage",
                            "Efficiency",
                            "Fishing mortality"),
                         selected="Spawning biomass")	       
      )
    )
  )
}


renderLifeHistory <- function()
{
  tabsetPanel(type="tabs",id="tab",
              
              # growth parameters
              tabPanel("Growth", 
                       sidebarPanel(
                         h4("Max age"),
                         column(3,
                                numericInput("MaxAge", label = "Years", value = 20)),
                         br(""),
                         br(""),
                         h4("weight at age (power)"),
                         column(4,
                                numericInput("alphaWt", label = "alpha", value = 0.0046)),
                         column(4,
                                numericInput("betaWt", label = "Beta", value = 2.63)),
                         br(""),
                         br(""),
                         
                         h4("length at age (von Bert)"),
                         column(3,
                                numericInput("Linf", label = "Linf", value = 183)),
                         column(3,
                                numericInput("growthK", label = "K", value = 0.24)),
                         column(3,
                                numericInput("growtht0", label = "t0", value = 0.44)),
                         br(""),
                         br(""),
                         h4("maturity at age (logistic)"),
                         column(4,
                                numericInput("mat50", label = "Age at 50% maturity", value = 2.8)),
                         column(4,
                                numericInput("mat95", label = "Age at 95% maturity", value = 3.5)),
                         
                         br(""),
                         br("")
                         ),
                       mainPanel(plotOutput("growth"))
              ),
              
              # Recruitment
              tabPanel("Recruitment",
                       sidebarPanel(
                         h4("Natural mortality"),
                         sliderInput("natM", label = "",min=0.02,max=1,value=.24,step=0.01),
                         h4("Recruitment parameters"),
                         numericInput("rzero", label = "Virgin recruitment", value = 20000),
                         sliderInput("steepness", label = "steepness",min=0.2,max=1,value=.7,step=0.01),
                         
                         h4("Recruitment distribution"),
                         selectInput('recDist',"",
                                     c( "Equal",
                                        "Proportional to biomass",
                                        "Proportional to habitat"),
                                     selected="Equal"),        
                         br("")
                         ),
                       mainPanel(plotOutput("recruitment"))
              ),
           
              # Movement
              tabPanel("Movement",
                       sidebarPanel(
                       numericInput("sdxaxs", label="Standard deviation of movement in the x axis", value = 1),
                       numericInput("sdyaxs", label="Standard deviation of movement in the y axis",value = 1)
              ),
                       mainPanel(plotOutput("movement"))
                       
                       
              ),
              # Habitat
              tabPanel("Habitat",
                      habdat<-as.matrix(read.csv("habitatBLZ.csv"))
                       filled.contour(z=habdat)
                      )
              
  )
}


renderSpat <- function()
{
#   fluidRow(
#     column(4,
#            wellPanel(h3("Location of fishery"),
#                      sliderInput("zoomLvl","Zoom",min=1,max=13,value=5,step=1),
#                      numericInput("Lat", label = "Latitude", value = 34.42),
#                      numericInput("Long", label = "Longitude", value = 119.71)
#            )
#     ),
#     column(8,
#             plotOutput("spatial")
# 
#     )
#   )
  
}

renderBanner <- function()
{
  wellPanel(
    # Logo image
    column(9),
    column(3,
           img(src="iphclogo.png",  height = 80, width = 80),
           img(src="iscamLogo.png", height = 80, width = 80)
    )
  )
}

# ----------------------------------------#
# MAIN USER INTERFACE FOR THE APPLICATION #
# ----------------------------------------#
shinyUI(fluidPage(navbarPage("Fishery sandbox",
                             
                             # INFORMATION INTERFACE (NEEDS TOC)
                             tabPanel("About",
                                      fluidRow(
                                        includeMarkdown("About.md")
                                      )
                                      
                             ),
                             # Defining the spatial extent 
                             tabPanel("Spatial extent",
#                                       renderSpat()
                                      sidebarLayout(
                                        sidebarPanel(
                                          sliderInput("zoomLvl","Zoom",min=1,max=13,value=7,step=1),
                                          numericInput("Lat", label = "Latitude", value = 34.42),
                                          numericInput("Long", label = "Longitude", value = -119.71),
                                          textOutput("clickcoord")
                                        ),
                                        mainPanel(plotOutput("map",clickId=c("plotclick","plotclick2")))
                                      )
                                      
                             ),
                            
                             # Defining the life history
                             tabPanel("Life History",
                                      fluidRow(
                                        renderLifeHistory()
                                )),

                            # Defining the life history
                              tabPanel("Simulation",
                                  fluidRow(
 
                                     )),
                                    
                             # EQUILIBRIUM INTERFACE
                             tabPanel("Comparison",
                                      fluidRow(
                                        column(6,
                                               h3("Comparison of management strategies")
                                        )
#                                         column(6,
#                                                selectInput('selChartType',"Model Output",
#                                                            c("Equilibrium Yield",
#                                                              "Performance Metrics at MSY",
#                                                              "Equilibrium Value",
#                                                              "Value at MSY"))
#                                         )
                                        # column(3,
                                        #   # helpText('Select an output')
                                        # )
                                      ),
                                      fluidRow(
                                        column(3,
                                               h4("Strategy A"),
                                               renderFleetDynamics("a")
                                        ),
                                        column(3,
                                               h4("Strategy B"),
                                               renderFleetDynamics("b")
                                        ),
                                        column(6,
                                               tabsetPanel(type="tabs",id="eqtab",
                                                           tabPanel("Plots",
                                                                    plotOutput("MScomp", height = "550px")
                                                           ),
                                                           tabPanel("Tables",
                                                                    tags$p("Biological sustainability"),
                                                                    tableOutput('table_biological'),
                                                                    tags$p("Fisheries sustainability at SSB-threshold. TCEY=(O26 bycatch)+(Wastage)+(FCEY)"),
                                                                    tableOutput('table_fishery'),
                                                                    tags$p("Economic performance at SSB-threshold (million $)"),
                                                                    tableOutput('table_economics'),
                                                                    tags$p("MSY-based reference points"),
                                                                    tableOutput('msytable')
                                                                    # tags$p("SPR-based reference points"),
                                                                    # tableOutput('sprtable'),
                                                                    # tags$p("U26:O26 ratios"),
                                                                    # tableOutput('u26ratio')
                                                           )
                                               )
                                        )
                                        
                                      )

                             )
                             
                             
                             
                             
                             
)))