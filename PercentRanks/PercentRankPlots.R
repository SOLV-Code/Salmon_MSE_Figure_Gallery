
###################################################
# SCRIPT TO ILLUSTRATE PERCENT RANKS IN VARIOUS PLOTS 
# Created by Gottfried Pestal (Solv Consulting Ltd.)
# Version 1 - September 3, 2012
###################################################

# custom function to imitate Excel's percentrank() function
perc.rank<-function(x){
	rank.x<-rank(x, ties.method="min")
	perc.rank.x <- (rank.x-1)/(max(rank.x)-1)
	perc.rank.x
	}

# example
perc.rank(c(1,3,2,4,5,6,7,3,44,5,22,3,3,44,56,90,876,2,0,0,123))


# custom function to plot percent ranks as deviations from median
perc.rank.plot<-function(x,ma=NULL,yrs.lab=c(1950,2010),type="fancy"){
# x is a time series stored in an array, with year labels as dim names
# ma specifies whether to plot a moving average. if ma is a number it defines the period for the avg
	
	if(type=="spark"){barplot(perc.rank(x)-0.5, ylim=c(-0.5,0.5),col="darkblue",border="darkblue", xlab="", ylab="",axes=FALSE,axisnames=FALSE)}


	if(type=="fancy"){
		x.ticks<-barplot(perc.rank(x)-0.5, ylim=c(-0.5,0.5),col="lightblue",border="lightblue", xlab="", ylab="",axes=FALSE,axisnames=FALSE)
		abline(h=c(-0.5,0,0.5),col="gray")
		text(rep(-3,3), c(-0.5,0,0.5),adj=1,labels=c("Min","Median","Max"),xpd=NA,cex=1.4)
		if(!is.null(ma)){lines(x.ticks,filter(perc.rank(x)-0.5,filter=rep(1/ma,ma) ,sides=1),col="red",lwd=2)}
		axis(side=1,at=x.ticks[seq(1,length(x.ticks),by=10)],labels = seq(yrs.lab[1],yrs.lab[2],10),cex.axis=1.4)
	} # end type=fancy
}


# custom function to plot time series
ts.plot<-function(x,y,xlim=c(1950,2010),grid=pretty(y,n=4),grid.label=TRUE, ma=NULL, minmaxpts=TRUE){
# ma specifies whether to plot a moving average. if ma is a number it defines the period for the avg
	# set the switch between writing full numbers and using scientific notation in labels
	options(scipen=3)
	# plot the time seris
	plot(x,y, xlim=xlim,ylim=c(min(grid),max(grid)),type="l",col="blue",axes=FALSE,xlab="",ylab="",lwd=1,xpd=TRUE)
	# add gridlines
	segments(x0=rep(xlim[1]+2,length(grid)) , y0=grid, x=rep(xlim[2],length(grid)) , y1=grid  ,col="grey",lty=2,xpd=TRUE)
	if(grid.label){text(x=rep(xlim[1]-1,length(grid)),y=grid,labels=prettyNum(grid,big.mark=","),cex=1.4, adj=1,xpd=TRUE)}
	# add year axis
	axis(side=1,at=seq(xlim[1],xlim[2],10),labels = seq(xlim[1],xlim[2],10),cex.axis=1.4)

	#calc and plot moving avg
	if(!is.null(ma)){lines(x, filter(y,filter=rep(1/ma,ma) ,sides=1),col="red",lwd=2,lty=1)}
	

	if(minmaxpts==TRUE){
		# calc, plot, and label max point
		points(x[y==max(y,na.rm=TRUE)],y[y==max(y,na.rm=TRUE)],col="dark blue",pch=21,bg="green",lwd=1.2)
		text(x[y==max(y,na.rm=TRUE)],y[y==max(y,na.rm=TRUE)] ,labels=paste(prettyNum(round(y[y==max(y,na.rm=TRUE)],2),big.mark=",")," (",x[y==max(y,na.rm=TRUE)],")",sep=""),pos=2,cex=0.7)

		# calc, plot, and label min point
		points(x[y==min(y,na.rm=TRUE)],y[y==min(y,na.rm=TRUE)],col="dark blue",pch=21,bg="red",lwd=1.2)
		text(x[y==min(y,na.rm=TRUE)],y[y==min(y,na.rm=TRUE)] ,labels=paste(prettyNum(round(y[y==min(y,na.rm=TRUE)],2),big.mark=",")," (",x[y==min(y,na.rm=TRUE)],")",sep=""),pos=2,cex=0.7)
		} # end if minmax pts =TRUE
} # end ts.plot function








###################################################################
# CREATE SAMPLE DATA
# create an array and fill it with data for plotting 
# Time series for 6 populations sampled from lognormal distribution with different variabilities and different patterns in mean values.
# Populations 5 and 6 have underlying 4yr cycles as well

sample.data <- array(NA,dim=c(61,6,3), dimnames=list(1950:2010,paste("Pop",1:6,sep=""),c("obs","meanlog","sdlog")))

sample.data[,"Pop1","meanlog"]  <- 10
sample.data[,"Pop1","sdlog"] <- 1 

sample.data[,"Pop2","meanlog"]  <- c(rep(2,50),seq(2,6,length.out=11))
sample.data[,"Pop2","sdlog"]  <- c(rep(1,50),seq(1,1.2,length.out=11))

sample.data[,"Pop3","meanlog"]  <- seq(7,3,length.out=61)
sample.data[,"Pop3","sdlog"]  <- seq(2.5,1.5,length.out=61)

sample.data[,"Pop4","meanlog"]  <- c(seq(8,4,length.out=20),seq(4,8,length.out=30),seq(8,6,length.out=11))
sample.data[,"Pop4","sdlog"]  <- c(seq(1.2,0.7,length.out=20),seq(0.7,1.2,length.out=30),seq(1.2,0.5,length.out=11))


# cyclic populations
sample.data[seq(1,61,by=4),"Pop5","meanlog"]  <- seq(9,4,length.out=length(seq(1,61,by=4)))
sample.data[seq(1,61,by=4),"Pop5","sdlog"]  <- seq(1,0.5,length.out=length(seq(1,61,by=4)))
sample.data[seq(2,61,by=4),"Pop5","meanlog"]  <- seq(5,4,length.out=length(seq(2,61,by=4)))
sample.data[seq(2,61,by=4),"Pop5","sdlog"]  <- seq(1,0.5,length.out=length(seq(2,61,by=4)))
sample.data[seq(3,61,by=4),"Pop5","meanlog"]  <- seq(3,2,length.out=length(seq(3,61,by=4)))
sample.data[seq(3,61,by=4),"Pop5","sdlog"]  <- seq(0.5,0.2,length.out=length(seq(3,61,by=4)))
sample.data[seq(4,61,by=4),"Pop5","meanlog"]  <- seq(1,3,length.out=length(seq(4,61,by=4)))
sample.data[seq(4,61,by=4),"Pop5","sdlog"]  <- seq(0.2,0.7,length.out=length(seq(4,61,by=4)))


sample.data[seq(1,61,by=4),"Pop6","meanlog"]  <- seq(4,12,length.out=length(seq(1,61,by=4)))
sample.data[seq(1,61,by=4),"Pop6","sdlog"]  <- seq(0.5,1,alength.out=length(seq(1,61,by=4)))
sample.data[seq(2,61,by=4),"Pop6","meanlog"]  <- seq(3,1,length.out=length(seq(2,61,by=4)))
sample.data[seq(2,61,by=4),"Pop6","sdlog"]  <- seq(0.5,0.1,length.out=length(seq(2,61,by=4)))
sample.data[seq(3,61,by=4),"Pop6","meanlog"]  <- seq(2,0.5,length.out=length(seq(3,61,by=4)))
sample.data[seq(3,61,by=4),"Pop6","sdlog"]  <- seq(0.1,0.05,length.out=length(seq(3,61,by=4)))
sample.data[seq(4,61,by=4),"Pop6","meanlog"]  <- seq(0.1,0.005,length.out=length(seq(4,61,by=4)))
sample.data[seq(4,61,by=4),"Pop6","sdlog"]  <- seq(0.01,0.005,length.out=length(seq(4,61,by=4)))

for(pop in paste("Pop",1:6,sep="")){
		sample.data[,pop,"obs"] <- rlnorm(61,meanlog=sample.data[,pop,"meanlog"],sdlog=sample.data[,pop,"sdlog"])
		}




########################################################################
# PERCENT RANK PLOT - FINAL PLOTS
# Plots 6 time series on 1 page


# open the png file as a plotting device
png(filename = "PercRank_FinalPlots.png",
    		width = 480*4, height = 480*4, units = "px", pointsize = 14*2.5, bg = "white",  res = NA)

# specify margins 
par(omi=c(2,2,2,2) , mai=c(1.5,1.5,1.5,1.5))

# divide the plotting device into 18 panels (plus 6 extra for labeling)
layout(matrix(c(1:6),ncol=2,byrow=TRUE),widths=c(1,1),heights=c(rep(1,6)))

# Loop through the data sets
for(pop in paste("Pop",1:6,sep="")){
	perc.rank.plot(sample.data[,pop,"obs"],ma=4, yrs.lab=c(1950,2010))
	title(main=pop,cex.main=1.6)
	}
title(main="Percent Ranks \n (as deviations from median)",outer=TRUE, cex.main=1.6)


mtext("https://github.com/SOLV-Code/Graph-Gallery/SOLV/PercentRank",side=1,line=0.5, outer=TRUE, cex=0.8,col="darkblue")

# turn off plotting device and save png file
dev.off()



########################################################################
# PERCENT RANK PLOT - PROGRESSION IN SPARKLINES
# Plots 4 variations of 6 time series in 3 columns of sparklines


png(filename = "PercRank_ProgressionInSparklines.png",
    		width = 480*4, height = 480*4, units = "px", pointsize = 14*3, bg = "white",  res = NA)

# specify margins 
par(omi=c(2,1,4,2) , mai=c(1,1,0.5,0.5))


# divide the plotting device into 18 panels (plus 6 extra for labeling)
layout(matrix(c(1:30),ncol=5,byrow=FALSE))

# plot column of labels
for(pop in paste("Pop",1:6,sep="")){
		plot(1:10,1:10,type="n",bty="n", axes=FALSE,xlab="",ylab="")
		text(5,5,labels=pop,xpd=TRUE,cex=1.4)
	}


# plot first column of sparklines - raw data
for(pop in paste("Pop",1:6,sep="")){
		plot(1950:2010,sample.data[,pop,"obs"],type="l",bty="n", axes=FALSE,xlab="",ylab="",col="darkblue",lwd=3)
		if(pop=="Pop1"){title(main="Raw Data",line=3,xpd=NA,cex.main=1.2)}
	}

# plot second column of sparklines - log(data)
for(pop in paste("Pop",1:6,sep="")){
		plot(1950:2010,log(sample.data[,pop,"obs"]),type="l",bty="n", axes=FALSE,xlab="",ylab="",col="darkblue",lwd=3)
		if(pop=="Pop1"){title(main="Log(Data)",line=3,xpd=NA,cex.main=1.2)}
	}

	
# plot third column of sparklines - percrank(data)
for(pop in paste("Pop",1:6,sep="")){
		plot(1950:2010,perc.rank(sample.data[,pop,"obs"]),ylim=c(0,1),type="l",bty="n", axes=FALSE,xlab="",ylab="",col="darkblue",lwd=3)
		if(pop=="Pop1"){title(main="PercRank(Data)",line=3,xpd=NA,cex.main=1.2)}
	}

# plot fourth column of sparklines -  perc ranks barplots
for(pop in paste("Pop",1:6,sep="")){
		perc.rank.plot(sample.data[,pop,"obs"], type="spark")
		if(pop=="Pop1"){title(main="PercRank(Data)",line=3,xpd=NA,cex.main=1.2)}
	}


title(main="Progression From Raw Data to Percent Ranks in Sparklines",outer=TRUE, line=6, cex.main = 1.7)
mtext("https://github.com/SOLV-Code/Graph-Gallery/SOLV/PercentRank",side=1,line=0.5, outer=TRUE, cex=0.8,col="darkblue")


# turn off plotting device and save png file
dev.off()




