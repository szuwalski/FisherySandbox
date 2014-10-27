library(ggmap)
showMap<-function(input)
{
  pngMAP_df <-  get_map(location = c(lat=input$Lat,lon=input$Long), source = "google", zoom = input$zoomLvl,maptype="satellite")
  p <- ggmap(pngMAP_df)
  print(p)
}


growthPlots<-function(input)
{
  MatAge<-1/(1+exp(-log(19)*((seq(1,input$MaxAge)-input$mat50)/(input$mat95-input$mat50))))
  lenAtAge<-rep(0,input$MaxAge)
  wgtAtAge<-rep(0,input$MaxAge)
  for(k in 1:input$MaxAge)
  {
    lenAtAge[k]<-input$Linf*(1-exp(-1*input$growthK*(k-input$growtht0)))
    wgtAtAge[k]<-input$alphaWt*lenAtAge[k]^input$betaWt
  }
    par(mfrow=c(2,1),mar=c(.1,4,.1,4),oma=c(3,.1,.1,3))
    plot(lenAtAge~seq(1,input$MaxAge),xaxt='n',las=1,type='l',ylab="",xlab="")
    mtext(side=2,"Length",line=3)
    par(new=T)
    plot(wgtAtAge~seq(1,input$MaxAge),xaxt='n',yaxt='n',las=1,type='l',col=2,lty=2,ylab="",xlab="")
    axis(4,las=1)
    legend("bottomright",lty=c(1,2),col=c(1,2),legend=c("Length","Weight"),bty='n')
    mtext(side=4,"Weight",line=3)
    plot(MatAge~seq(1,input$MaxAge),las=1,type="l",ylab="",xlab="",ylim=c(0,1))
    mtext(side=1,"Age",line=2)
    mtext(side=2,"Probability of mature",line=3)
  
}


recPlots<-function(input)
{
  tempB<-rep(0,100)
  tempB[1]<-input$rzero
  for(x in 2:length(tempB))
    tempB[x]<-tempB[x-1]*exp(-input$natM)
 
  VirSpBio<-sum(tempB)
  SpawningBiomass<-seq(1,VirSpBio,VirSpBio/1000)
  
  Recruits<-(0.8*input$rzero*input$steepness*SpawningBiomass) / ((0.2*VirSpBio*(1-input$steepness))+ (input$steepness-0.2)*SpawningBiomass)

  plot(Recruits~SpawningBiomass,type="l")

}

