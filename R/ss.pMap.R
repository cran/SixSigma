ss.pMap <-
function(procs, inputs.overall, outputs.overall,
                  input.output, x.parameters, y.features,
		  main="Six Sigma Process Map", sub,
		  ss.col=c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")){
	nprocs<-length(procs)
	.ss.prepCanvas(main,sub)
	paintBox<-function(){
		x<-c(rep(0.10,4),0.30,0.70,rep(0.90,4),0.7,0.3)
		y<-c(0.10,0.37,0.64,rep(0.91,4),0.64,0.37,rep(0.1,3))
		grid.xspline(x, y,
			shape=c(1,1,1,1,1,1),open=FALSE,
			gp=gpar(fill=ss.col[4],lwd=3, col=ss.col[2])
			)
	}
#
#	grid.newpage()
#	grid.rect(gp=gpar(col=ss.col[2], lwd=2, fill=ss.col[5]))
#	vp.canvas<-viewport(width=unit(1,"npc")-unit(5,"mm"),
#		height=unit(1,"npc")-unit(5,"mm"),
#		name="canvas",
#			layout=grid.layout(3,3,
#			widths=unit(c(1,1,1), c("lines", "null", "lines")),
#			heights=unit(c(3,1,2), c("lines", "null", "lines"))
#			)
#		)
#	pushViewport(vp.canvas)
#	grid.rect(gp=gpar(col="#FFFFFF", lwd=0, fill="#FFFFFF"))
##Title
#	vp.title<-viewport(layout.pos.col=1:3, layout.pos.row=1, name="title")
#	pushViewport(vp.title)
#	grid.text(main, name="titulo",	gp=gpar(fontsize=20))
#	popViewport()
#
##Subtitle
#	vp.subtitle<-viewport(layout.pos.col=1:3, layout.pos.row=3, name="subtitle")
#	pushViewport(vp.subtitle)
#	grid.text ( sub, gp=gpar(col=ss.col[1]))
#	popViewport()

#ProcessMap container
	vp.map<-viewport(name="map",layout.pos.col=1:3, layout.pos.row=2,
		layout=grid.layout(3,nprocs, heights=c(0.2,0.6,0.2)))	##
	pushViewport(vp.map)

#overall inputs
	vp.inputs<-viewport(layout.pos.col=1,layout.pos.row=1, name="inputs")
	pushViewport(vp.inputs)
	paintBox()
	grid.text("INPUTS\nX")
	grid.move.to(x=0.5, y=0.1)
	upViewport()
	vp.inputsText<-viewport(layout.pos.col=2:4, layout.pos.row=1, name="inputst")
	pushViewport(vp.inputsText)
	for (i in 1:length(inputs.overall)){
		grid.text(x=unit(0.5,"cm"),
		y=unit(1, "npc")-unit(i,"lines"),
		paste(inputs.overall[i],"\n"),
		just=c("left","top"), name="inputst")
	}
	upViewport()

#Processes
	for (i in 1:nprocs){
		vp.proc<-viewport(layout.pos.col=i, layout.pos.row=2)
		pushViewport(vp.proc)
		pushViewport(viewport(y=1, h=0.5, just=c("center","top")))
		paintBox()
		grid.lines(x=c(0.1,0.9), y=c(0.74, 0.74), gp=gpar(lwd=3, col=ss.col[2]))
		grid.text(procs[i], y=0.85, just=c("center", "top"))
		grid.text("INPUTS", rot=90, x=0.20, y=0.20,
			just=c("left", "bottom"), gp=gpar(fontsize=8, col=ss.col[1]))
		for (j in 1:length(input.output[[i]])){
			grid.text(input.output[[i]][j], y=unit(0.7, "npc")-unit(j-1,"lines"),
				just=c("center","top"))
		}
		if (i==1){
			grid.line.to(x=0.5, y=0.91,
				arrow=arrow(angle = 30, length = unit(0.15, "inches"),
				ends = "last", type = "open"), gp=gpar(lwd=6, col=ss.col[1]))
		}
		if (i>1){
			grid.line.to(x=0.1, y=0.5,
			arrow=arrow(angle = 30, length = unit(0.15, "inches"),
			ends = "last", type = "open"), gp=gpar(lwd=6, col=ss.col[1]))
		}
		grid.move.to(x=0.9, y=0.5)
		if (i==nprocs){
			grid.move.to(x=0.7, y=0.1)
		}
		upViewport()
		pushViewport(viewport(y=0.5, h=0.5, just=c("center","top")))
		grid.text("Param.(x): ", y=unit(1,"npc")-unit(2,"mm"), gp=gpar(fontsize=8),
			x=unit(0,"npc")+unit(2,"mm"),
			just=c("left", "top"))
		for (j in 1:length(x.parameters[[i]])){
			grid.text(paste(x.parameters[[i]][[j]][1],x.parameters[[i]][[j]][2]),
				y=unit(1,"npc")-unit(2,"mm")-unit(j-1,"lines"),
				x=unit(1,"strwidth","Param.(x):   "),
				gp=gpar(fontsize=8), just=c("left", "top"))
		}
		grid.text("Featur.(y): ",
			y=unit(1,"npc")-unit(2,"mm")-unit(length(x.parameters[[i]]),"lines"),
			x=unit(0,"npc")+unit(2,"mm"),
			gp=gpar(fontsize=8),
			just=c("left", "top"))
		for (j in 1:length(y.features[[i]])){
			grid.text(y.features[[i]][[j]],
				y=unit(1,"npc")-unit(2,"mm")-unit(j-1+length(x.parameters[[i]]),
					"lines"),
				x=unit(1,"strwidth","Featur.(y):   "),
				gp=gpar(fontsize=8), just=c("left", "top"))
		}
		upViewport()
		upViewport()
	}

#overalloutputs
	vp.outputs<-viewport(layout.pos.col=nprocs, layout.pos.row=3, name="outputs")
	pushViewport(vp.outputs)
	paintBox()
	grid.text("OUTPUTS\nY")
	grid.line.to(x=0.7, y=0.91, arrow=arrow(angle = 30, length = unit(0.15, "inches"),
		ends = "last", type = "open"), gp=gpar(lwd=6, col=ss.col[1]))
	upViewport()
	vp.outputsText<-viewport(layout.pos.col=1:3, layout.pos.row=3, name="outputst")
	pushViewport(vp.outputsText)
	for (i in 1:length(outputs.overall)){
		grid.text(x=unit(1,"npc")-unit(0.5,"cm"),
			y=unit(1, "npc")-unit(i,"lines"), paste(outputs.overall[i],"\n"),
			just=c("right","top"), name="outputst")
	}
	vp.legend<-viewport(x=unit(0.2, "cm"),
		y=unit(0.2,"cm"),
		just=c("left","bottom"),
		height=unit(1,"npc")-unit(0.4, "cm"),
		width=0.3)
	pushViewport(vp.legend)
	grid.rect(gp=gpar(fill=ss.col[3]))
	grid.text("LEGEND\n\t(C)ontrollable\n\t(Cr)itical\n\t(N)oise\n\t(P)rocedure",
		y=unit(1, "npc")-unit(0.2, "cm") , x=unit(0, "npc")+unit(0.2,"cm"),
		just=c("left", "top"),
		gp=gpar(fontsize=8))
	upViewport()
	upViewport()
}
