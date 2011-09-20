# Capability Analysis Functions
# 
# Author: Emilio Lopez
###############################################################################

ss.ca.yield <- function(defects = 0, rework = 0, opportunities = 1){
	Yield <- (opportunities - sum(defects)) / opportunities
	FTY <- (opportunities - sum(defects) - sum(rework)) / opportunities
	RTY <- prod((opportunities - (defects + rework)) / opportunities)
	DPU <- sum(defects)
	DPMO <- (DPU / opportunities) * 10^6 
	ss.ca.yield <- (list(Yield = Yield, FTY = FTY, 
						RTY = RTY, DPU = DPU, DPMO = DPMO))
	as.data.frame(ss.ca.yield)
} 

ss.ca.z <- function(x, LSL = NA, USL = NA, 
		LT = FALSE, f.na.rm = TRUE){
	if (is.na(LSL) & is.na(USL)) {
		stop ("No specification limits provided")
	}
	zz.m <- mean(x, na.rm = f.na.rm)
	zz.s <- sd(x, na.rm = f.na.rm)
	zul <- (USL - zz.m) / zz.s
	zll <- (zz.m - LSL) / zz.s
	
	if (is.na(zul)){
		z <- zll
	}
		else if (is.na(zll)){
			z <- zul
		}
		else {
			z <- min(zul, zll)
		}
		if (LT == FALSE){
			z <- z - 1.5
		} 
		return(as.vector(z))	
}


ss.ca.cp <- function(x, LSL = NA, USL = NA, 
		LT = FALSE, f.na.rm = TRUE, 
		ci = FALSE, alpha = 0.05){
	if (is.na(LSL) & is.na(USL)) {
		stop("No specification limits provided")
	}
	if (!is.numeric(x)){
		stop("Incorrect vector data")
	}
	cp.m <- mean(x, na.rm = f.na.rm)
	cp.s <- sd(x, na.rm = f.na.rm)
	cp.l <- (cp.m - LSL) / (3 * cp.s)
	cp.u <- (USL - cp.m) / (3 * cp.s)
	cp <- (USL - LSL) / (6 * cp.s)
	if (is.na(cp)){
		cp <- max(cp.l, cp.u, na.rm = TRUE)
	}
	if (ci == FALSE){
		return(as.numeric(cp))
	}
	else{
		return(c(
				as.numeric(cp)*sqrt((qchisq (alpha/2,length(x)-1,
											lower.tail=TRUE)/(length(x)-1))),				
				as.numeric(cp)*sqrt((qchisq (alpha/2,length(x)-1,
											lower.tail=FALSE)/(length(x)-1)))
		))							
	}
}

ss.ca.cpk <- function(x, LSL = NA, USL = NA, 
		LT = FALSE, f.na.rm = TRUE, 
		ci = FALSE, alpha = 0.05 ){
	if (is.na(LSL) & is.na(USL)) {
		stop("No specification limits provided")
	}
	if (!is.numeric(x)){
		stop("Incorrect vector data")
	}
	ss.n <- length(x[!is.na(x)])
	cpk.m <- mean(x, na.rm = f.na.rm)
	cpk.s <- sd(x, na.rm = f.na.rm)
	cpk.ul <- (USL - cpk.m) / (3 * cpk.s)
	cpk.ll <- (cpk.m - LSL) / (3 * cpk.s)
	cpk <- min(cpk.ul, cpk.ll, na.rm = TRUE)
	if (ci == FALSE){
		return(as.numeric(cpk))
	}
	else{
		return(c(
		cpk*(1-(qnorm(1-(alpha/2))*sqrt((1/(9*ss.n*cpk^2))+(1/(2*(ss.n-1)))))),
		cpk*(1+(qnorm(1-(alpha/2))*sqrt((1/(9*ss.n*cpk^2))+(1/(2*(ss.n-1))))))
		))
	}
		

}	

###############################################################################
ss.study.ca<-function (xST, xLT = NA, LSL = NA, USL = NA, 
		Target = NA, alpha = 0.05, 
		f.na.rm = TRUE,
		f.main = "Six Sigma Capability Analysis Study", 
		f.sub = ""){
	if (is.na(Target)){
		stop("Target is needed")
	}
	if (is.na(LSL) & is.na(USL)){
		stop("No specification limits provided")
	}
	require(nortest)
	#Facts
	mST = mean(xST, na.rm = f.na.rm)
	sST = sd(xST, na.rm = f.na.rm)
	nST = length(xST[!is.na(xST)])
	nLT = length(xLT[!is.na(xLT)])
	zST = ss.ca.z(xST, LSL, USL)
	cpST = ss.ca.cp(xST, LSL, USL)
	cpiST = ss.ca.cp(xST, LSL, USL, ci = TRUE, alpha = alpha)
	cpkST = ss.ca.cpk(xST, LSL, USL)
	cpkiST = ss.ca.cpk(xST, LSL, USL, ci = TRUE, alpha = alpha)
	DPMO <- (1 - pnorm(zST - 1.5)) * 10^6
	if (is.numeric(xLT)){
		mLT = mean(xLT, na.rm = f.na.rm)
		sLT = sd(xLT,na.rm = f.na.rm)
		cpLT = ss.ca.cp(xLT, LSL, USL, LT = TRUE)	
		cpiLT = ss.ca.cp(xLT, LSL, USL, LT = TRUE, ci = TRUE, alpha = alpha)
		cpkLT = ss.ca.cpk(xLT, LSL, USL, LT = TRUE)
		cpkiLT = ss.ca.cpk(xLT, LSL, USL, LT = TRUE, ci = TRUE, alpha = alpha)
		zLT = ss.ca.z(xLT, LSL, USL, LT = TRUE)
		DPMO <- (1 - pnorm(zLT)) * 10^6
	}
	else{
		mLT=NA
		sLT=NA
		cpLT=NA	
		cpiLT=NA
		cpkLT=NA
		cpkiLT=NA
		zLT<-zST-1.5
	}

######
	.ss.prepCanvas(f.main, f.sub)
#grid.rect()##########
	vp.plots<-viewport(name="plots",
			layout=grid.layout(2,2,c(0.6,0.4),c(0.6,0.4)))
	pushViewport(vp.plots)

	vp.hist <- viewport(name="hist", layout.pos.row=1, layout.pos.col=1)
	pushViewport(vp.hist)
#grid.rect()##########
	grid.text("Histogram & Density", y=1, just=c("center", "top") )

##############	

binwST <- diff(range(xST))/ sqrt(nST)
ggdata <- melt(xST)
qqp <- ggplot(ggdata, aes(x=value))
hist <- qqp + geom_histogram(aes(y = ..density..), 
				binwidth = binwST,
				fill = "steelblue", 
				stat = "bin")
if (!is.na(LSL)){
	hist <- hist +
		annotate(geom = "text", 
				x = LSL, 
				y = 0.2, 
				label = "LSL", 
				hjust = -0.1, 
				size = 5) 
} 
hist <- hist +	annotate(geom = "text",
				x = Target, 
				y = 0.4, 
				label = "Target",
				hjust = -0.1,
				size = 5)
if (!is.na(USL)){
	hist <- hist + 
		annotate(geom = "text",
				x = USL, 
				y = 0.2, 
				label = "USL",
				hjust = 1.1, 
				size = 5) 
}
	hist <- hist + xlab(NULL) + 
		ylab(NULL) + 
		opts(axis.text.y = theme_blank())
if (!is.na(LSL)){
		hist <- hist + geom_vline(xintercept = LSL,
				linetype = 2,
				size = 1) 
	}
if (!is.na(USL)){
	hist <- hist + geom_vline(xintercept = USL,
			linetype = 2,
			size = 1) 
}
	hist <- hist + geom_vline(xintercept = Target,
				linetype = 3, 
				size = 1) +
		stat_density(geom="path", 
				position="identity", 
				binwidth = binwST,
				size = 1) +
		stat_function( 
				fun = dnorm, 
				args = with(ggdata,	c(mean(value), sd(value))),
				linetype = 2, 
				size = 1
		) 

if (is.numeric(xLT)){
	binwLT <- diff(range(xLT))/ sqrt(nLT)
	ggdataLT <- melt(xLT)
	hist <- hist + 
		stat_density(geom="path",
				data = ggdataLT, 
				position = "identity", 
				binwidth = binwLT) + 
		stat_function(
				fun = dnorm, 
				args = with(ggdataLT, 
						c(mean = mean(value), sd = sd(value))),
				linetype=2
		)
} 

	print(hist, newpage=FALSE)
	
	popViewport()
	vp.norm<-viewport(name="normal",layout.pos.row=2, layout.pos.col=1,
			layout=grid.layout(2,2,c(0.6,0.4),c(0.1, 0.9)))
	pushViewport(vp.norm)
	grid.text("Check Normality", y=1,just=c("center","top"))
#grid.rect()##########
	vp.qq<-viewport(name="qqp",layout.pos.row=2,layout.pos.col=1, 
			height=unit(0.5,"npc"))
	pushViewport(vp.qq)
#grid.rect()##########

	qqp <- qplot(sample = xST, stat="qq") + 
			xlab(NULL)+ ylab(NULL)+
			opts(axis.text.x=theme_blank(),axis.text.y=theme_blank()) 
	print(qqp,newpage=FALSE)
	popViewport()
	vp.testn<-viewport(name="testn",layout.pos.row=2, layout.pos.col=2)
	pushViewport(vp.testn)
	ss.ts<-shapiro.test(xST)
	ss.tl<-lillie.test(xST)
	if (min(ss.ts$p.value, ss.tl$pvalue) < alpha){
		warning("Normality test/s failed")
	} 
	grid.text("Shapiro-Wilk Test", y=.9,just=c("center","top"), 
			gp=gpar(cex=.8))
	grid.text(paste("p-value: ",format(ss.ts$p.value,digits=4)),
			gp=gpar(cex=.8), y=.8)
	grid.text("Lilliefors (K-S) Test", gp=gpar(cex=.8))
	grid.text(paste("p-value: ", format(ss.tl$p.value,digits=4)),
			gp=gpar(cex=.8),y=.4)
	popViewport()
	grid.text("Normality accepted when p-value > 0.05",y=0.02, 
			just=c("center","bottom"), gp=gpar(cex=.8))
	popViewport()
	vpNumbers<-viewport(name="numbers", 
			layout.pos.row=c(1,2), layout.pos.col=2,
			layout=grid.layout(4,1))
	pushViewport(vpNumbers)
	
grid.rect(gp=gpar(col="#BBBBBB",lwd=2))##########
	vpLegend<-viewport(name="legend", layout.pos.row=1)
	pushViewport(vpLegend)

grid.rect(gp=gpar(col="#BBBBBB",lwd=2))##########
	grid.text(expression(bold("Density Lines Legend")), 
			y=0.95, just=c("center","top"))
	grid.lines(x=c(0.05,0.3), y=c(0.7,0.7), gp=gpar(lty=1, lwd=3))
	grid.text("Density ST", x=0.35, y=0.7,just=c("left","center"),
			gp=gpar(cex=0.8))
	
	grid.lines(x=c(0.05,0.3), y=c(0.55,0.55), gp=gpar(lty=2, lwd=3))
	grid.text("Theoretical Dens. ST", x=0.35, y=0.55,just=c("left","center"), 
			gp=gpar(cex=0.8))

if (is.numeric(xLT)){	
	grid.lines(x=c(0.05,0.3), y=c(0.40,0.40), gp=gpar(lty=1, lwd=1))
	grid.text("Density LT", x=0.35, y=0.40,just=c("left","center"), 
			gp=gpar(cex=0.8))
	
	grid.lines(x=c(0.05,0.3), y=c(0.25,0.25), gp=gpar(lty=2, lwd=1))
	grid.text("Theoretical Density LT", x=0.35, y=0.25,just=c("left","center"), 
			gp=gpar(cex=0.8))
}	
	popViewport()
	vpSpec<-viewport(name="spec", layout.pos.row=2)
	pushViewport(vpSpec)
#grid.rect()#############
	grid.text(expression(bold("Specifications")), y=.95, just=c("center","top"))
	grid.text(expression(bold("LSL: ")), 
			y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(LSL, y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("Target: ")), 
			y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(Target, y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("USL: ")), 
			y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(USL, y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	popViewport()
	vpProcess<-viewport(name="proc", layout.pos.row=3,
			layout=grid.layout(1,2))
	pushViewport(vpProcess)
#grid.rect()##############
	grid.lines(x=c(0,1),y=c(1,1), gp=gpar(col="#BBBBBB", lwd=3))
	grid.text(expression(bold("Process")), y=.95, just=c("center","top"))
	vpSTp<-viewport(layout.pos.col=1)
	pushViewport(vpSTp)
	grid.text("Short Term",x=0.05, y=.95, just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("Mean: ")), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",mST), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("SD: ")), y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",sST), y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("n: ")), y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(nST, y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold(Z[s]*": ")), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.2f",zST), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	
	popViewport()
	vpLTp<-viewport(layout.pos.col=2)
	pushViewport(vpLTp)
	grid.text("Long Term",x=.95, y=.95, just=c("right","top"), gp=gpar(cex=.8))
	grid.text(expression(bold("Mean: ")), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",mLT), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("SD: ")), y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",sLT), y=unit(.95,"npc")-unit(2.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("n: ")), y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(nLT, y=unit(.95,"npc")-unit(3.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold(Z[s]*": ")), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.2f",zLT), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("DPMO: ")), y=unit(.95,"npc")-unit(5.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(round(DPMO,1), y=unit(.95,"npc")-unit(5.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	popViewport()
	popViewport()
	vpIndices<-viewport(name="ind", layout.pos.row=4,
			layout=grid.layout(1,2))
	pushViewport(vpIndices)
#grid.rect()###############
grid.lines(x=c(0,1), y=c(1,1), gp=gpar(col="#BBBBBB",lwd=2))
	grid.text(expression(bold("Indices")),y=.95,just=c("center","top"))
	vpSTi<-viewport(layout.pos.col=1)
	pushViewport(vpSTi)
	grid.text("Short Term",x=0.05, y=.95, just=c("left","top"),gp=gpar(cex=.8))
	grid.text(expression(bold(C[p]*": ")), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",cpST), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("CI: ")), y=unit(.95,"npc")-unit(3,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.7))
	grid.text(paste("[",paste(sprintf("%.1f",cpiST[1]),sep=""),
					",",sprintf("%.1f",cpiST[2]),"]",sep=""), 
					y=unit(.95,"npc")-unit(3,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.7))
	grid.text(expression(bold(C[pk]*": ")), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",cpkST), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("CI: ")), y=unit(.95,"npc")-unit(6.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.7))
	grid.text(paste("[",paste(sprintf("%.1f",cpkiST[1]),sep=""),
					",",sprintf("%.1f",cpkiST[2]),"]",sep=""), 
			y=unit(.95,"npc")-unit(6.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.7))

	popViewport()
	vpLTi<-viewport(layout.pos.col=2)
	pushViewport(vpLTi)
	grid.text("Long Term",x=.95, y=.95, just=c("right","top"), gp=gpar(cex=.8))
	grid.text(expression(bold(P[p]*": ")), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",cpLT), y=unit(.95,"npc")-unit(1.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("CI: ")), y=unit(.95,"npc")-unit(3,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.7))
	grid.text(paste("[",paste(sprintf("%.1f",cpiLT[1]),sep=""),
					",",sprintf("%.1f",cpiLT[2]),"]",sep=""), 
			y=unit(.95,"npc")-unit(3,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.7))
	grid.text(expression(bold(P[pk]*": ")), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.8))
	grid.text(sprintf("%.4f",cpkLT), y=unit(.95,"npc")-unit(4.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.8))
	grid.text(expression(bold("CI: ")), y=unit(.95,"npc")-unit(6.5,"lines"), 
			just=c("right","top"),
			gp=gpar(cex=.7))
	grid.text(paste("[",paste(sprintf("%.1f",cpkiLT[1]),sep=""),
					",",sprintf("%.1f",cpkiLT[2]),"]",sep=""), 
			y=unit(.95,"npc")-unit(6.5,"lines"), 
			just=c("left","top"),
			gp=gpar(cex=.7))
	popViewport()
	popViewport()
	
}
#trellis.par.set(standard.theme(color=FALSE))
#ss.study.ca(x,LSL=740, USL=760, T=750, f.sub="Winery Project")
