###################
#  Escape_4_Panel #
#   Brooke Davis  #
###################

#====================================================================
# Purpose: Plots escapement trajectories for the 2 OMs and 6 MPs
# Trimmed down original function to just show example from one stock
#=====================================================================

# Data structure: We use a "Blob" as a catch-all data structure for each model Run ( a set 
#  of simulations assuming a certain OM and MP scenario). Blob is not a built-in functions or
#  a specific object type in R, it is just the name we have given.

# Each Blob is simply a list where model options (Blob$Options), data (Blob$Data), and 
#   simulation (Blob$Sims) outputs are stored

# A blob is initalized for each run with a given set of options, these options are used to read 
#  in appropriate data. This object is then fed to a main simulation function that takes the prescribed
#  options and data, and run the sumulations. 

# After each run (of a given number of sims) a Blob is saved as an RDS, these are the objects that 
#  are seen in the DataIn folder

# These blobs are then the easiest way to feed data into a plotting function, since they include all 
#  model options, input data, and output simulations

Run <- "Expanded_Nat"
Mods2Plot_1 = c(1,2,3,4); Mods2Plot_2 = c(1,5,6); Mods2Plot_3 = c(7,8,9,10); Mods2Plot_4 = c(7,11,12)
Smax  <- T
Plot_Spawners <- T
Names <- readRDS(paste("DataIn/Scenario_Names_", Run, ".RDS", sep=""))
Esc_LeadIn <- read.csv("DataIn/Escape_LeadIn.csv")
  
  # want to extract table of median change in escapment over 10 years (2016 to 2025) and 20 years (2016 to 2035)
  # also save all blobs
  Blobs <- list()
  Escape <- list()
  Spawners <- list()
  for(mm in 1:length(Names)){
    Blobs[[mm]] <- readRDS(paste("DataIn/", Names[mm], ".rds", sep=""))
    Escape[[mm]] <- Blobs[[mm]]$Sims$Escape
    if(Plot_Spawners==T){
      Spawners[[mm]] <- Blobs[[mm]]$Sims$Spawners
    }
  } # end run loop
  
  cols <- c("#0000ff", "#b22222", "#006400", "#FF9937","#7F3DCC",  "#808080")
  #  Blue, Green, Red, Orange, Purple, grey
  TGrey <- "#80808050"
  
  
  png(filename = "4PanelEsc.png", width = 480*4, height = 480*4, 
      units = "px", pointsize = 14*2.5, bg = "white",  res = NA)
  par(mfrow=c(2,2), oma=c(2,3,2,1), mar=c(2,2,2,1))
  
  Stocks <- Blobs[[1]]$Data$StocksInfo$StockID
  Years <- Blobs[[1]]$Options$Years
  NY <- Blobs[[1]]$Data$NY
  MaxAge <- Blobs[[1]]$Data$MaxAge
  Escapement <- Blobs[[1]]$Data$Escapement
  
  StocksInfo_List <- list()
  for(mm in 1:length(Blobs)){
    StocksInfo_List[[mm]] <- Blobs[[1]]$Data$StocksInfo
  }
  
# Just do WCVI.N as example
  ss <- 6
    MyMean <- list()
    MySD <- list()
    # first store all model data so can get ylims
    for(mm in 1:length(Blobs)){
      if(Plot_Spawners==T){
        MyDat <- lapply(Spawners[[mm]], "[", ,ss  ) 
      } else {
        StockDat <- lapply(Escape[[mm]], "[", ,ss , ) 
        MyDat <- lapply(1:length(StockDat), function(x) apply(StockDat[[x]], 1, sum) )
      }
      MyMean[[mm]] <-  sapply(1:NY, function(x) mean(sapply(MyDat, "[", x) ))
      MySD[[mm]] <- sapply(1:NY, function(x) sd(sapply(MyDat, "[", x) ))
    } # end mod loop
    # remove initialization years
    Years_To_Plot <- Years[-(1:MaxAge)]
    MyMean <- lapply(MyMean, "[", which(Years %in% Years_To_Plot))
    MySD <- lapply(MySD, "[", which(Years %in% Years_To_Plot))
    # Get ylims
    Edat <- Esc_LeadIn[which(Esc_LeadIn$StockID==Stocks[ss] & Esc_LeadIn$Year < min(Years)), ]
    # get ylims
    ylims <- c( 0 , max(c( unlist(MyMean) + unlist(MySD), Edat$Escape ), na.rm=T))
    # get escapament init data
    Edat_Init <- Escapement[which(Escapement$StockID==Stocks[ss]),]
    # Plot
    #**************************************************
    # Panel 1 -- Basic Ricker with 4 fishing scenarios OR smolt release scenarios
    Cols <- c("#000000", cols[1:3])
    # start with leadin and inits
    plot(Edat$Year, Edat$Escape, col="darkgrey", type="l", ylim=ylims, xlim=c(min(Edat$Year), max(Years)), lwd=2, ann=F)
    # now add initialization data
    lines(Edat_Init$Year, Edat_Init$Abundance, col="black", lwd=2)
    # Now add model fits
    for(mm in 1:length(Mods2Plot_1)){
      lines(Years_To_Plot, MyMean[[Mods2Plot_1[mm]]], col=Cols[mm], lwd=2)
      # end mods loop
      # Add tranparent error bars
      polygon(y=c(MyMean[[Mods2Plot_1[mm]]] - MySD[[Mods2Plot_1[mm]]], rev(MyMean[[Mods2Plot_1[mm]]] + MySD[[Mods2Plot_1[mm]]])), 
              x=c(Years_To_Plot, rev(Years_To_Plot)), col= paste(Cols[mm], 50, sep=""), border=Cols[mm])
    } # end mod loop
    # add Lines for capacity, Smax
    if(Smax == T & StocksInfo_List[[mm]]$Type[which(StocksInfo_List[[mm]]$StockID==Stocks[ss])] == "Natural"){
      # extract alpha and beta
      # only works if natural stock
      alpha <- StocksInfo_List[[Mods2Plot_1[mm]]]$Ricker_A[which(StocksInfo_List[[Mods2Plot_1[mm]]]$StockID==Stocks[ss])]
      beta <- StocksInfo_List[[Mods2Plot_1[mm]]]$Ricker_B[which(StocksInfo_List[[Mods2Plot_1[mm]]]$StockID==Stocks[ss])]
      abline(h=1/beta, lty=2, col="orange", lwd=2)
      abline(h=1/beta*log(alpha), lty=3, col="orange", lwd=2)
    }
    

    legend("topleft", col=c(Cols, "white", "orange", "orange"), 
               legend=c("Current", "50% Curr.", "50% Curr. PT", "No Fishing", "", "Smax", "Capacity"), title="Fishing Scenario",
               bty="n", cex=0.8, lwd=2, lty=c(1,1,1,1,1,2,3))
     
    mtext(side=2, line=3, text="Historical Productivity")
    
    #**************************************************
    # Panel 2 -- Basic Ricker with 2 Productivity/CC scenarios OR AI bias scenarios
    Cols <- c("#000000", cols[4:5])
    # start with leadin and inits
    plot(Edat$Year, Edat$Escape, col="darkgrey", type="l", ylim=ylims, xlim=c(min(Edat$Year), max(Years)), lwd=2, ann=F)
    # now add initialization data
    lines(Edat_Init$Year, Edat_Init$Abundance, col="black", lwd=2)
    # Now add model fits
    for(mm in 1:length(Mods2Plot_2)){
      lines(Years_To_Plot, MyMean[[Mods2Plot_2[mm]]], col=Cols[mm], lwd=2)
      # end mods loop
      # Add tranparent error bars
      polygon(y=c(MyMean[[Mods2Plot_2[mm]]] - MySD[[Mods2Plot_2[mm]]], rev(MyMean[[Mods2Plot_2[mm]]] + MySD[[Mods2Plot_2[mm]]])), 
              x=c(Years_To_Plot, rev(Years_To_Plot)), col= paste(Cols[mm], 50, sep=""), border=Cols[mm])
      # This is a little more complicated for this because Smax/capacity will change
      # will use final year
      if(Smax == T & StocksInfo_List[[mm]]$Type[which(StocksInfo_List[[mm]]$StockID==Stocks[ss])] == "Natural"){
        # extract alpha and beta
        alpha <- StocksInfo_List[[Mods2Plot_2[mm]]]$Ricker_A[which(StocksInfo_List[[Mods2Plot_2[mm]]]$StockID==Stocks[ss])] * Blobs[[Mods2Plot_2[mm]]]$Data$Prod_Mults$Mult[length(Years)]
        beta <- StocksInfo_List[[Mods2Plot_2[mm]]]$Ricker_B[which(StocksInfo_List[[Mods2Plot_2[mm]]]$StockID==Stocks[ss])] / Blobs[[Mods2Plot_2[mm]]]$Data$CC_Mults$Mult[length(Years)]
        if(mm==1){
          abline(h=1/beta, col=Cols[mm], lwd=2)
        } else {
          abline(h=1/beta, lty=2, col=Cols[mm], lwd=2)
        }
        abline(h=1/beta*log(alpha), lty=3, col=Cols[mm], lwd=2)
      }
    } # end mod loop
    
    legend("topleft", col=c(Cols, "white", "grey", "grey"), legend=c("No Change", "Prod. 25%", "CC 25%", "", "Smax", "Capacity"  ), 
               title="Habitat Scenario", bty="n", cex=0.8, lwd=2, lty=c(1,1,1,1,2,3))
    
    
    #**************************************************
    # Panel 3 --  R.B with 4 fishing scenarios
    Cols <- c("#000000", cols[1:3])
    # start with leadin and inits
    plot(Edat$Year, Edat$Escape, col="darkgrey", type="l", ylim=ylims, xlim=c(min(Edat$Year), max(Years)), lwd=2, ann=F)
    # now add initialization data
    lines(Edat_Init$Year, Edat_Init$Abundance, col="black", lwd=2)
    # Now add model fits
    for(mm in 1:length(Mods2Plot_3)){
      lines(Years_To_Plot, MyMean[[Mods2Plot_3[mm]]], col=Cols[mm], lwd=2)
      # end mods loop
      # Add tranparent error bars
      polygon(y=c(MyMean[[Mods2Plot_3[mm]]] - MySD[[Mods2Plot_3[mm]]], rev(MyMean[[Mods2Plot_3[mm]]] + MySD[[Mods2Plot_3[mm]]])), 
              x=c(Years_To_Plot, rev(Years_To_Plot)), col= paste(Cols[mm], 50, sep=""), border=Cols[mm])
    } # end mod loop
    if(Smax == T  & Smax == T & StocksInfo_List[[mm]]$Type[which(StocksInfo_List[[mm]]$StockID==Stocks[ss])] == "Natural"){
      # extract alpha and beta
      alpha <- StocksInfo_List[[Mods2Plot_3[mm]]]$Ricker_A[which(StocksInfo_List[[Mods2Plot_3[mm]]]$StockID==Stocks[ss])]
      beta <- StocksInfo_List[[Mods2Plot_3[mm]]]$Ricker_B[which(StocksInfo_List[[Mods2Plot_3[mm]]]$StockID==Stocks[ss])]
      abline(h=1/beta, lty=2, col="orange", lwd=2)
      abline(h=1/beta*log(alpha), lty=3, col="orange", lwd=2)
    }
    mtext(side=2, line=3, text="Recent Productivity")
    
    #***************************************************
    # Panel 4 -- R.B with 2 productivity/CC scenarios
    Cols <- c("#000000", cols[4:5])
    # start with leadin and inits
    plot(Edat$Year, Edat$Escape, col="darkgrey", type="l", ylim=ylims, xlim=c(min(Edat$Year), max(Years)), lwd=2, ann=F)
    # now add initialization data
    lines(Edat_Init$Year, Edat_Init$Abundance, col="black", lwd=2)
    # Now add model fits
    for(mm in 1:length(Mods2Plot_4)){
      lines(Years_To_Plot, MyMean[[Mods2Plot_4[mm]]], col=Cols[mm], lwd=2)
      # end mods loop
      # Add tranparent error bars
      polygon(y=c(MyMean[[Mods2Plot_4[mm]]] - MySD[[Mods2Plot_4[mm]]], rev(MyMean[[Mods2Plot_4[mm]]] + MySD[[Mods2Plot_4[mm]]])), 
              x=c(Years_To_Plot, rev(Years_To_Plot)), col= paste(Cols[mm], 50, sep=""), border=Cols[mm])
      # add Smax/capacity lines
      if(Smax == T & StocksInfo_List[[mm]]$Type[which(StocksInfo_List[[mm]]$StockID==Stocks[ss])] == "Natural"){
        # extract alpha and beta
        alpha <- StocksInfo_List[[Mods2Plot_4[mm]]]$Ricker_A[which(StocksInfo_List[[Mods2Plot_4[mm]]]$StockID==Stocks[ss])] * Blobs[[Mods2Plot_4[mm]]]$Data$Prod_Mults$Mult[length(Years)]
        beta <- StocksInfo_List[[Mods2Plot_4[mm]]]$Ricker_B[which(StocksInfo_List[[Mods2Plot_4[mm]]]$StockID==Stocks[ss])] / Blobs[[Mods2Plot_4[mm]]]$Data$CC_Mults$Mult[length(Years)]
        # want to show both dashed lines, so make first solid
        if(mm==1){
          abline(h=1/beta, col=Cols[mm], lwd=2)
        } else {
          abline(h=1/beta, lty=2, col=Cols[mm], lwd=2)
        }
        abline(h=1/beta*log(alpha), lty=3, col=Cols[mm], lwd=2)
      }
    } # end mod loop
    
    # label stock
    mtext(side=3, text=Stocks[ss], outer=T)
    # add overall labels
    mtext(side=1, text="Year", outer=T, line=0.5)
    mtext(side=2, text = "Escapement", outer=T)
  dev.off()
  


