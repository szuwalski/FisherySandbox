library(shiny)
require(animation)
require(caTools)

library(devtools)
install_github("rstudio/shiny-incubator")
library(shinyIncubator)

source("helpers.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/Master.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/lenwei.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/VisualizeMovement.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/movArray.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/InitialPop.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/Recruitment.R")
source("https://raw.githubusercontent.com/SNAPwg/SNAP/master/samplingFunc.R")

paramNames <- c("size_limit",
                "discard_mortality_rate",
                "spr_target",
                "selex_fishery",
                "selex_bycatch",
                "num_bycatch",
                "five",
                "ten",
                "twenty",
                "forty")

## ------------------------------------------------------------------------------------ ##
## Define server logic required to run scripts
## ------------------------------------------------------------------------------------ ##
shinyServer(function(input, output, session) {
  
  # Subset Dataframe based on User Interface Selection.
  data <- reactive({
    
    if(input$plotType=="Spawning biomass" || input$plotType=="Depletion")
    {
      DF <- BIO.DF
    }
    if(input$plotType=="Catch" || input$plotType=="Fishing mortality")
    {
      DF <- CAT.DF
    }
    if(   input$plotType=="Sub-legal Catch" || input$plotType=="Wastage")
    {
      DF <- SUB.DF
    }
    if(input$plotType=="AAV in Catch" )
    {
      DF <- AAV.DF
    }
    if(input$plotType=="Efficiency")
    {
      DF <- MSE.DF
    }
    
    a  <- subset(DF,
                 Year      %in% input$years[1]:input$years[2] &
                   Scenario  %in% input$scenario                &
                   Procedure %in% input$procedure
    )
    
  })
  
  
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # TULIP PLOTS FOR MSE PROCEDURES AND SCENARIOS
  output$funnelPlot <- renderPlot({
    tulip.plot(data(),input)
  })
  
  # GVIS GRAPHICS FOR COMPARING SCENARIOS DYNAMICALLY
#   output$googleVisPlot <- renderGvis({
#     motionChart(MOT.DF,input)
#   })
  
  # maps of area
  output$spatial <- renderPlot({
   showMap(input)
  })
  
  output$map <- renderPlot({
    showMap(input)
  })
  
  output$growth <- renderPlot({
    growthPlots(input)
  })

 output$recruitment <- renderPlot({
    recPlots(input)
  })

 output$movement <-renderPlot({
   movePlots(input)
 })

 output$MScomp <- renderPlot({
   
   Graphs<-F
   GraphsFish<-F
   PrintLifeHistory<-F
   Life<-read.csv("LifeHistory.csv")                          # life history characteristics
   SimCTL<-read.csv("GrandSimCtlBLZ.csv",header=F)               # simulation controls
   Fleets<-read.csv("Fleets.csv",header=F)                    # fleet characteristics  
   season<-read.csv("season.csv",header=F)                    # fishing seasons by fleet
   Samp <- read.csv("SamplingParams.csv")                     # sampling controls for management
   NoTakeZoneNULL<-read.csv("notakezoneBLZNULL.csv",header=F)   # marine protected areas (0=open access, 1=MPA, 2=TURF?)
   NoTakeZoneImp<-read.csv("notakezoneBLZ.csv",header=F)      # marine protected areas (0=open access, 1=MPA, 2=TURF?)
   habitat<-read.csv("habitatBLZ.csv",header=F)                # habitat quality (recruitment suitability)
 
   OpenAccess<-Master(Life,SimCTL,Fleets,season,Samp,NoTakeZoneNULL,NoTakeZoneImp,habitat,Graphs,GraphsFish,PrintLifeHistory)
   OAtotCatch<-apply(OpenAccess$CatchByFisher,2,sum,na.rm=T)
   OAtotCost<-apply(OpenAccess$CostByFisher,2,sum,na.rm=T)
   OAtotProfit<-apply(OpenAccess$ProfitByFisher,2,sum,na.rm=T)
   
   burnInt   <-SimCTL[grep('burn',SimCTL[,2]),1]
   simTimePlt <-SimCTL[grep('simTime',SimCTL[,2]),1]
   initManage<-SimCTL[grep('initManage',SimCTL[,2]),1]   # year in which to initiate management
   yearMark2  <-SimCTL[grep('yearMark',SimCTL[,2]),1]      # number of time steps in a year
   SpcRow    <-SimCTL[grep('SpaceR',SimCTL[,2]),1]	    	# Rows in the grid space
   SpcCol	  <-SimCTL[grep('SpaceC',SimCTL[,2]),1]  			# cols in the grid spcae
   
   par(mfrow=c(7,1),mar=c(.1,4,.1,.1))
   plot(OAtotCatch,type="b",xaxt='n',las=2,ylim=c(0,max(OAtotCatch,na.rm=T)),ylab="Total Catch",pch=16)
   abline(v=initManage-burnInt,col=2,lty=2)
   legend("topright",col=2,lty=2,"Management implemented",bty='n')
   plot(OAtotCost,lty=2,type="b",pch=16,xaxt='n',las=2,ylim=c(0,max(OAtotCost,na.rm=T)),ylab="Total cost of fishing")
   abline(v=initManage-burnInt,col=2,lty=2)
   
   plot(OAtotProfit,lty=2,type="b",pch=16,xaxt='n',las=2,ylim=c(0,max(OAtotProfit,na.rm=T)),ylab="Total profit of fishing")
   abline(v=initManage-burnInt,col=2,lty=2)
   
   plot(OpenAccess$CostOfManagement[burnInt:(simTimePlt)],pch=16,type="b",xaxt='n',las=2,ylim=c(0,max(OpenAccess$CostOfManagement,na.rm=T)),
        ylab="Cost of MPA")
   abline(v=initManage-burnInt,col=2,lty=2)
   
   plot(OpenAccess$SpawningBiomass[burnInt:simTimePlt],pch=16,type="b",xaxt='n',las=2,
        ylab="SpawningBio",ylim=c(0,max(OpenAccess$SpawningBiomass[burnInt:simTimePlt],na.rm=T)))
   abline(v=initManage-burnInt,col=2,lty=2)
   
   plot(OpenAccess$ExploitableNumbers[burnInt:simTimePlt],pch=16,type="b",xaxt='n',las=2,
        ylab="Exploitable Numbers",ylim=c(0,max(OpenAccess$ExploitableNumbers[burnInt:simTimePlt],na.rm=T)))
   
   plot(OpenAccess$OutsideMPAspbio[burnInt:simTimePlt],ylim=c(0,max(OpenAccess$OutsideMPAspbio[burnInt:simTimePlt],OpenAccess$InsideMPAspbio[burnInt:simTimePlt])),type='b',pch=16)
   lines(OpenAccess$InsideMPAspbio[burnInt:simTimePlt],type='b',pch=16,col=2)
   abline(v=initManage-burnInt,col=2,lty=2)
   
   legend("topright",col=c(1,2),pch=16,legend=c("Outside MPA","Inside MPA"),bty='n') 

 })

  # MEDIAN DEPLETION TABLE
  output$viewDepletionTable <- renderTable({
    
    cat("Depletion table \n")
    df  <- subset(mse.data$biomass.df,
                  Year      %in% input$years[1]:input$years[2] &
                    Scenario  %in% input$scenario                &
                    Procedure %in% input$procedure
    )
    mdf <- melt(df,id=c("Scenario","Procedure","Year"))
    tmp <- dcast(mdf,Procedure~Scenario,mean,na.rm=TRUE,margins="Scenario",
                 subset=.(variable=="t.Dt0.5"))
    return(tmp)
    
  })
  
  # MEDIAN CATCH TABLE
  output$viewCatchTable <- renderTable({
    
    cat("Catch table \n")
    df  <- subset(mse.data$catch.df,
                  Year      %in% input$years[1]:input$years[2] &
                    Scenario  %in% input$scenario                &
                    Procedure %in% input$procedure
    )
    mdf <- melt(df,id=c("Scenario","Procedure","Year"))
    tmp <- dcast(mdf,Procedure~Scenario,mean,na.rm=TRUE,margins="Scenario",
                 subset=.(variable=="ct50"))
    return(tmp)
    
  })
  
  # PROBABILITY OF GOING BELOW THE LIMIT REFERENCE POINT: P(SB<0.20)
  output$viewSSBlimit <- renderTable({
    
    cat("SSB Limit table \n")
    df  <- subset(mse.data$biomass.df,
                  Year      %in% input$years[1]:input$years[2] &
                    Scenario  %in% input$scenario                &
                    Procedure %in% input$procedure
    )
    mdf <- melt(df,id=c("Scenario","Procedure","Year"))
    tmp <- dcast(mdf,Procedure~Scenario,mean,na.rm=TRUE,margins="Scenario",
                 subset=.(variable=="P.SSB.0.20."))
    return(tmp)
    
  })
  
  # PROBABILITY OF GOING BELOW THE THRESHOLD REFERENCE POINT: P(SB<0.30)
  output$viewSSBthreshold <- renderTable({
    
    cat("SSB Threshold table \n")
    df  <- subset(mse.data$biomass.df,
                  Year      %in% input$years[1]:input$years[2] &
                    Scenario  %in% input$scenario                &
                    Procedure %in% input$procedure
    )
    mdf <- melt(df,id=c("Scenario","Procedure","Year"))
    tmp <- dcast(mdf,Procedure~Scenario,mean,na.rm=TRUE,margins="Scenario",
                 subset=.(variable=="P.SSB.0.30."))
    return(tmp)
    
  })
  
#   # OPERATING MODEL INTERFACE FUNCTIONS
#   output$Spat <- renderPlot({
#     cat(input$omiplotType)
#     switch(input$omiplotType,
#            "Spawning biomass"   = .plotSpawnBiomass(M),
#            "Depletion"          = .plotDepletion(M),
#            "Recruitment"        = .plotRecruitment(M),
#            "Stock Recruitment"  = .plotStockRecruit(M),
#            "Relative abundance" = .plotSurveyFit(M),
#            "Mortality"          = .plotMortality(M)
#            pngMAP_df <-  get_map(location = c(lat=16.2,lon=-88.65), source = "google", zoom = 11,maptype="satellite")
#            ggmap(pngMAP_df)
#            
#     )
#   })
  
  
  
  
  
  
  
  # EQUILIBRIUM MODEL INTERFACE 
  getParams <- function(prefix) {
    
    # input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params <- c(params,prefix=prefix)
    # print(params)
    params
  }
  
  ## Run equilibrium models
  scnA <- reactive(do.call(equilibrium_model, getParams("a")))
  scnB <- reactive(do.call(equilibrium_model, getParams("b")))
  
  
  output$a_equilPlot <- renderPlot({
    AB <- rbind(scnA(),scnB())
#     switch(input$selChartType,
#            "Equilibrium Yield"          = .plotEquilYield(AB),
#            "Performance Metrics at MSY" = .plotPerformanceMSY(AB),
#            "Equilibrium Value"          = .plotEquilValue(AB),
#            "Value at MSY"               = .plotPerformanceValue(AB)
#     )
  })
  
  output$table_biological <- renderTable({
    AB <- rbind(scnA(),scnB())
    .biologicalTable(AB)
  })
  
  output$table_fishery <- renderTable({
    AB <- rbind(scnA(),scnB())
    .fisheryTable(AB)
  })
  
  output$table_economics <- renderTable({
    AB <- rbind(scnA(),scnB())
    .economicTable(AB)
  })
  
  output$msytable <- renderTable({
    AB <- rbind(scnA(),scnB())
    .equilibriumTables(AB)
  })
  
  # output$sprtable <- renderTable({
  #   AB <- rbind(scnA(),scnB())
  #   # .sprTables(AB)
  # })
  
  # output$u26ratio <- renderTable({
  #   AB <- rbind(scnA(),scnB())
  #   # .u26Table(AB)
  # })
  
})  # End of ShinyServer
## ------------------------------------------------------------------------------------ ##

.biologicalTable <- function(Scenario)
{
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                BMSY    = Be[which.max(Ye)],
                DMSY    = depletion[which.max(Ye)],
                BTarget = Be[which.min(depletion>ssb_threshold)],
                BLimit  = Be[which.min(depletion>ssb_limit)],
                SPR     = SPR[which.min(depletion>ssb_threshold)],
                Fmsy    = fe[which.max(Ye)],
                Ftarget = fe[which.min(depletion>ssb_threshold)],
                Flimit  = fe[which.min(depletion>ssb_limit)]
  )
  colnames(test)=c("Scenario",
                   "♀ SSB_MSY",
                   "♀ Depletion",
                   "♀ SSB Threshold",
                   "♀ SSB Limit",
                   "SPR Threshold",
                   "F MSY",
                   "F Target",
                   "F Limit")
  rownames(test)=NULL
  print(test)
}

.fisheryTable <- function(Scenario)
{
  
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                "TCEY"    = TCEY[which.min(depletion>ssb_threshold)],#+O26[which.min(depletion>ssb_threshold)]+We[which.min(depletion>ssb_threshold)]+Ye[which.min(depletion>ssb_threshold)],
                "O26 Bycatch" = O26[which.min(depletion>ssb_threshold)],
                "Wastage" = We[which.min(depletion>ssb_threshold)],
                "FCEY"    = Ye[which.min(depletion>ssb_threshold)],
                "FCEY/TCEY (%)" = OE[which.min(depletion>ssb_threshold)]*100,
                "U26 Bycatch" = U26[which.min(depletion>ssb_threshold)]
                
  )
  colnames(test)[1]="Scenario"
  print(test)
}

.economicTable <- function(Scenario)
{
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                "Landed value"= YEv[which.min(depletion>ssb_threshold)],
                "Wastage value" = WEv[which.min(depletion>ssb_threshold)],
                "Bycatch value" = BYv[which.min(depletion>ssb_threshold)],
                "Total value of all mortality" = YEv[which.min(depletion>ssb_threshold)]+WEv[which.min(depletion>ssb_threshold)]+BYv[which.min(depletion>ssb_threshold)]
  )
  colnames(test)[1]="Scenario"
  print(test)
}

.u26Table <- function(Scenario)
{
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                "Fishery @ MSY"  =f26[which.max(Ye)],
                "Bycatch @ MSY"  =b26[which.max(Ye)],
                "Fishery @ SPR"  =f26[which.min(SPR>spr_target)],
                "Bycatch @ SPR"  =b26[which.min(SPR>spr_target)]
  )
  colnames(test)[1]="Scenario"
  print(test) 
}


.sprTables <- function(Scenario)
{
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                FSPR  =fe[which.min(SPR>spr_target)],
                Catch =Ye[which.min(SPR>spr_target)],
                BMSY  =Be[which.min(SPR>spr_target)],
                Depl  =depletion[which.min(SPR>spr_target)],
                DMSY  =De[which.min(SPR>spr_target)],
                WMSY  =We[which.min(SPR>spr_target)],
                EFF   =OE[which.min(SPR>spr_target)]
  )
  colnames(test)[1]="Scenario"
  print(test)
}

.equilibriumTables <- function(Scenario)
{
  test <- ddply(Scenario,.(prefix),plyr::summarize,
                Fmsy=fe[which.max(Ye)],
                MSY =Ye[which.max(Ye)],
                BMSY=Be[which.max(Ye)],
                Depl=depletion[which.max(Ye)],
                DMSY=De[which.max(Ye)],
                WMSY=We[which.max(Ye)]
  )
  colnames(test)=c("Scenario","F","MSY","Biomass (♀)",
                   "Depletion","Discards","Wastage")
  rownames(test)=NULL
  print(test)
}


.plotEquilYield <- function(Scenario)
{
  
  helpText("These are the equilibrium yield versus fishing effort.")
  
  mdf<-melt(Scenario,id.vars=1:5)
  sdf<-subset(mdf,variable %in% c("Ye","De","We","OE"))
  levels(sdf$variable)[levels(sdf$variable)=="Ye"] <- "Directed Fishery Yield (Mlb)"
  levels(sdf$variable)[levels(sdf$variable)=="De"] <- "Directed Fishery Discard (Mlb)"
  levels(sdf$variable)[levels(sdf$variable)=="We"] <- "Directed Fishery Wastage (Mlb)"
  levels(sdf$variable)[levels(sdf$variable)=="OE"] <- "Operational Efficiency (%)"
  
  
  p <- ggplot(sdf,(aes(fe,value,col=prefix))) +geom_line()
  # p <- p + geom_vline(xintercept=0.3, subset = .(variable == "Directed Fishery Yield (Mlb)"))
  # p <- p + geom_vline(data=sdg,aes(xintercept=fe[which.min("SPR">spr_target)],col="Scenario"),size=2,alpha=0.5)
  p <- p + facet_wrap(~variable,scales="free")
  p <- p + labs(x="Fishing Intensity",col="Scenario",y="")
  print(p + theme_bw(14))
  
}

.plotEquilValue <- function(Scenario)
{
  
  
  mdf<-melt(Scenario,id.vars=1:5)
  sdf<-subset(mdf,variable %in% c("YEv","DEv","BYv","WEv"))
  levels(sdf$variable)[levels(sdf$variable)=="YEv"] <- "Landed value"
  levels(sdf$variable)[levels(sdf$variable)=="DEv"] <- "Value of discards"
  levels(sdf$variable)[levels(sdf$variable)=="BYv"] <- "Value of bycatch mortality"
  levels(sdf$variable)[levels(sdf$variable)=="WEv"] <- "Value of wastage"
  
  p <- ggplot(sdf,(aes(fe,value,col=prefix))) +geom_line()
  p <- p + facet_wrap(~variable,scales="free")
  p <- p + labs(x="Fishing Intensity",col="Scenario",y="Millions of dollars")
  print(p + theme_bw(14))
  
}


.plotPerformanceMSY<- function(Scenario)
{
  x<-ddply(Scenario,.(prefix),plyr::summarize,
           "Fishing Intensity @ MSY"=fe[which.max(Ye)],
           "Maximum Fishery Yield"  =Ye[which.max(Ye)],
           "Discards"               =De[which.max(Ye)],
           "Wastage"                =We[which.max(Ye)]
  )
  
  p <- ggplot(melt(x,id.vars=1),aes(variable,value,fill=prefix))
  p <- p + geom_bar(stat="identity",position="dodge")
  p <- p + labs(x="Variable",y="Value (million lbs or fishing intensity)",fill="Scenario")
  p <- p +facet_wrap(~variable,scales="free")
  print(p + theme_bw(14))
}

.plotPerformanceValue<- function(Scenario)
{
  x<-ddply(Scenario,.(prefix),plyr::summarize,
           "Landed Value @ MSY"           =YEv[which.max(Ye)],
           "Value of Wastage"             =WEv[which.max(Ye)],
           "Total Value of all mortality" =YEv[which.max(Ye)]+WEv[which.max(Ye)]+BYv[which.max(Ye)],
           "Value of losses"              =(YEv[which.max(Ye)]+WEv[which.max(Ye)]+BYv[which.max(Ye)])-YEv[which.max(Ye)]
  )
  
  p <- ggplot(melt(x,id.vars=1),aes(variable,value,fill=prefix))
  p <- p + geom_bar(stat="identity",position="dodge")
  p <- p + labs(x="Variable",y="Value (million $)",fill="Scenario")
  p <- p +facet_wrap(~variable,scales="free")
  print(p + theme_bw(14))
}

