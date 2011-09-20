# Loss Function Analysis
# 
# Author: Emilio
###############################################################################

ss.lfa <- function(lfa.data, lfa.ctq, lfa.Delta, lfa.Y0, lfa.L0, 
		lfa.size=NA, lfa.output="both", lfa.sub="Six Sigma Project"){
		lfa.k <- lfa.L0/lfa.Delta
		lfa.lf <- bquote(bold(L==.(lfa.k)%.%(Y-.(lfa.Y0))^2))
		lfa.MSD <- with(lfa.data,
				sum((get(lfa.ctq)-lfa.Y0)^2)/length(get(lfa.ctq)))
		lfa.avLoss <- lfa.k * lfa.MSD
		if (is.numeric(lfa.size)){
			lfa.Loss <- lfa.size * lfa.avLoss
		}
	
	if (lfa.output %in% c("both", "plot")){
		.ss.prepCanvas(main="Loss Function Analysis", sub=lfa.sub)
		#grid.newpage()
		##titulos, ...
		vp.function <- viewport(name="Taguchi", layout=grid.layout(2,2,
						heights=c(0.9,0.1), widths=c(0.8,0.2)))
		pushViewport(vp.function)
		vp.plot<-viewport(name="plot", layout.pos.row=1, layout.pos.col=1)
		#plot
		pushViewport(vp.plot)
		ggdata <- melt(with(lfa.data,get(lfa.ctq)))
		qqp <- ggplot(ggdata, aes(x=value))
		qqp <- qqp + stat_function(fun=function(x)eval(lfa.k)*(x-eval(lfa.Y0))^2,
				size=1.2) + ylab("Cost of Poor Quality") + xlab("Observed Value")
		qqp <- qqp + geom_vline(xintercept=eval(lfa.Y0-lfa.Delta), linetype=2)
		qqp <- qqp + geom_vline(xintercept=eval(lfa.Y0+lfa.Delta), linetype=2)
		qqp <- qqp + geom_vline(xintercept=eval(lfa.Y0), linetype=3, size=1.1)
		qqp <- qqp + geom_hline(yintercept=0, size=1)
		qqp <- qqp + annotate(geom="text", x=lfa.Y0-lfa.Delta, 
				y=lfa.avLoss, label="LSL", hjust=-0.1)
		qqp <- qqp + annotate(geom="text", x=lfa.Y0+lfa.Delta, 
				y=lfa.avLoss, label="USL", hjust=1.1)
		qqp <- qqp + annotate(geom="text", x=lfa.Y0, 
				y=ss.lf(lfa.Y0-lfa.Delta, lfa.Delta, lfa.Y0, lfa.L0), 
				label="T", hjust=1.1)
		print(qqp, newpage=FALSE)
		popViewport()
		
		#function
		vp.fun <- viewport(name="fun", layout.pos.row=2, layout.pos.col=1)
		pushViewport(vp.fun)
		grid.rect(width=0.9, gp=gpar(lty=0, fill="#DDDDDD"))####################
		grid.text(lfa.lf)
		popViewport()
		
		#data
		vp.data <- viewport(name="data", layout.pos.row=1:2, layout.pos.col=2)
		pushViewport(vp.data)
		vp.data.input <- viewport(name="input", layout.pos.row=2, 
				layout.pos.col=1)
		pushViewport(vp.data.input)
		grid.rect(y=0.95, width=0.99, just="top", height=0.7)####################
		my.margin <- 0.9
		grid.text(expression(bold(Data)), y=my.margin, just="top")
		grid.text(paste("CTQ:",eval(lfa.ctq)),
				y=unit(my.margin,"npc")-unit(2,"lines"),
				just="top")
		grid.text(bquote(Y[0]==.(lfa.Y0)),
				y=unit(my.margin,"npc")-unit(3,"lines"),
				just="top")
		grid.text(bquote(Delta==.(lfa.Delta)),
				y=unit(my.margin,"npc")-unit(4,"lines"),
				just="top")
		grid.text(bquote(L[0]==.(lfa.L0)),
				y=unit(my.margin,"npc")-unit(5,"lines"),
				just="top")
		if (is.numeric(lfa.size)){
			size.exists=1
			grid.text(bquote(Size==.(lfa.size)),
					y=unit(my.margin,"npc")-unit(6,"lines"),
					just="top")
		}
		grid.lines(y=unit(my.margin,"npc")-unit(8,"lines"))
		
		grid.text(bquote(Mean==.(round(with(lfa.data,mean(get(lfa.ctq))),digits=4))),
				y=unit(my.margin,"npc")-unit(10,"lines"),
				just="top")
		grid.text(bquote(k==.(lfa.k)),
				y=unit(my.margin,"npc")-unit(11,"lines"),
				just="top")
		grid.text(bquote(MSD==.(round(lfa.MSD,digits=4))),
				y=unit(my.margin,"npc")-unit(12,"lines"),
				just="top")
		grid.text(bquote(Av.Loss==.(round(lfa.avLoss,digits=4))),
				y=unit(my.margin,"npc")-unit(13,"lines"),
				just="top")
		if (is.numeric(lfa.size)){
		grid.text(bquote(Loss==.(round(lfa.Loss,digits=4))),
				y=unit(my.margin,"npc")-unit(14,"lines"),
				just="top")
	}
	}
	if (lfa.output %in% c("both", "text")){
		#pintar valores en output
		return(list(lfa.k=lfa.k, lfa.lf=as.expression(lfa.lf), lfa.MSD=lfa.MSD, 
						lfa.avLoss=lfa.avLoss, lfa.Loss=lfa.Loss))
	}
	else{
		invisible(list(lfa.k=lfa.k, lfa.lf=as.expression(lfa.lf), lfa.MSD=lfa.MSD, 
						lfa.avLoss=lfa.L0, lfa.Loss=lfa.Loss))
	}
}
		
##################################


ss.lf <- function(lfa.Y1, lfa.Delta, lfa.Y0, lfa.L0) {
	if (lfa.Delta <= 0){
		stop("The tolerance of the process must be greater than 0")
	}
	if (lfa.L0 <= 0){
		warning("The Cost of poor quality at tolerance limit should be greater than 0")
	}
	lfa.k <- lfa.L0/lfa.Delta
	return(lfa.k*(lfa.Y1-lfa.Y0)^2)
	
}