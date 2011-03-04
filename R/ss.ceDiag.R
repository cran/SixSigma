ss.ceDiag<-function(effect, causes.gr, causes, main="Six Sigma Cause-and-effect Diagram", sub, palette=c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")){
    n.causes<-length(causes.gr)
	grid.newpage()
	grid.rect(gp=gpar(col=palette[2], lwd=2, fill=palette[5]))
	vp.canvas<-viewport(width=unit(1,"npc")-unit(5,"mm"),height=unit(1,"npc")-unit(5,"mm"),
		name="canvas",
		layout=grid.layout(3,3,
			widths=unit(c(1,1,1), c("lines", "null", "lines")),
			heights=unit(c(3,1,2), c("lines", "null", "lines"))
		))
	pushViewport(vp.canvas)
	grid.rect(gp=gpar(col="#FFFFFF", lwd=0, fill="#FFFFFF"))

#Title
	vp.title<-viewport(layout.pos.col=1:3, layout.pos.row=1, name="title")
	pushViewport(vp.title)
	grid.text (main, name="titulo", gp=gpar(fontsize=20))
	popViewport()

#Subtitle
	vp.subtitle<-viewport(layout.pos.col=1:3, layout.pos.row=3, name="subtitle")
	pushViewport(vp.subtitle)
	grid.text ( sub, gp=gpar(col=palette[1]))
	popViewport()

#ProcessMap container
	vp.diagram<-viewport(name="diagram",layout.pos.col=1:3, layout.pos.row=2)
	pushViewport(vp.diagram)

#Fish head
	w.head<-unit(1,"strwidth", effect)+unit(4,"mm")
	vp.head<-viewport(x=unit(1,"npc")-w.head,
	h=1, w=w.head, just=c("left", "center"))
	pushViewport(vp.head)

	x.hshape<-c(0.00,0.00,0.50,0.93,0.95,0.93,0.50)
	y.hshape<-c(0.44,0.56,0.55,0.52,0.50,0.48,0.45)
	grid.xspline(x.hshape, y.hshape, shape=c(0,0,-1,1,0,1,-1), open=FALSE,
		gp=gpar(col=palette[2],lwd=2, fill=palette[5]))
	grid.text(effect)
	popViewport()

#Fish tail
	vp.tail<-viewport(x=0.01, h=1, w=0.05, just=c("left","center"))
	pushViewport(vp.tail)
	grid.xspline(x=c(0,0,1), y=c(0.44,0.56,0.5), shape=c(0,0,0), open=FALSE,
		gp=gpar(col=palette[2],lwd=2, fill=palette[5]))
	popViewport()
	vp.body<-viewport(x=0.06,
		h=1, w=unit(1,"npc")-w.head-unit(0.06,"npc"), just=c("left", "center"))
	pushViewport(vp.body)
	grid.lines(x=c(0,1), y=c(0.5,0.5),
		gp=gpar(col=palette[2],lwd=4))
	room<-1/floor((n.causes+1)/2)
	up<-seq(room,1, room)
	for (i in 1:(length(up)-1)){
		grid.lines(x=c(up[i]-0.15, up[i] ), y=c(0.8,0.5),
			gp=gpar(col=palette[2],lwd=2))
		grid.text(causes.gr[i], x=up[i]-0.15, y=0.81, just=c("center", "bottom"))
		for (j in 1:length(causes[[i]])){
			grid.text(causes[[i]][j], x=unit(up[i]-0.15,"npc")+unit(j,"lines"),
				y=unit(0.80, "npc")-unit(j,"lines"), gp=gpar(fontsize=8),
				just=c("left","center"))
			}
		}
	x=as.vector(c(unit(1, "npc")-unit(room/2, "npc")-unit(0.15,"npc"),
		unit(1, "npc")-unit(room/2,"npc"))
		)
	for (i in length(up):n.causes){
		if (i==length(up)){
			grid.lines(x=c(1-room/2-0.15,1-room/2),
				y=c(0.2,0.5), 		gp=gpar(col=palette[2],lwd=2))
			grid.text(causes.gr[i], x=1-room/2-0.15, y=0.19, just=c("center", "top"))
		}
		grid.lines(x=c((1-(room+(2*(i-4)*room))/2)-0.15,1-(room+(2*(i-4)*room))/2 ),
			y=c(0.2,0.5),
			gp=gpar(col=palette[2],lwd=2))
		grid.text(causes.gr[i], x=(1-(room+(2*(i-4)*room))/2)-0.15, y=0.19, just=c("center", "top"))
		for (j in 1:length(causes[[i]])){
			grid.text(causes[[i]][j],
				x=unit((1-(room+(2*(i-4)*room))/2)-0.15, "npc")+unit(j,"lines"),
				y=unit(0.20, "npc")+unit(j,"lines"), gp=gpar(fontsize=8),
				just=c("left","center"))
		}
	}
}
