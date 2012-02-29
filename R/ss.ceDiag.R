ss.ceDiag <- function(effect, causes.gr, causes, 
		main = "Six Sigma Cause-and-effect Diagram", 
		sub, ss.col = c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")){
    
	n.causes<-length(causes.gr)
	.ss.prepCanvas(main,sub)

#Fish head
	w.head <- unit(1, "strwidth", effect) + unit(4, "mm")
	vp.head <- viewport(x = unit(1, "npc") - w.head,
		height = 1, 
		width = w.head, 
		just = c("left", "center"))
	pushViewport(vp.head)

	x.hshape <- c(0.00, 0.00, 0.50, 0.93, 0.95, 0.93, 0.50)
	y.hshape <- c(0.44, 0.56, 0.55, 0.52, 0.50, 0.48, 0.45)
	grid.xspline(x.hshape, 
		y.hshape, 
		shape = c(0, 0, -1, 1, 0, 1, -1),
		open = FALSE,
		gp = gpar(col = ss.col[2], 
			lwd = 2, fill = ss.col[5]))
	grid.text(effect)
	popViewport()

#Fish tail
	vp.tail <- viewport(x = 0.01, 
		height = 1, 
		width = 0.05, 
		just = c("left","center"))
	pushViewport(vp.tail)
	grid.xspline(x = c(0, 0, 1),
		y = c(0.44, 0.56, 0.5), 
		shape = c(0, 0, 0), open = FALSE,
		gp = gpar(col = ss.col[2], 
			lwd=2, 
			fill = ss.col[5]))
	popViewport()
	vp.body <- viewport(x = 0.06,
		height = 1, 
		width = unit(1, "npc") - w.head - unit(0.06, "npc"), 
		just = c("left", "center"))
	pushViewport(vp.body)
	grid.lines(x = c(0,1), 
		y = c(0.5, 0.5),
		gp = gpar(col = ss.col[2], lwd=4))
	
#body
	#up lines
	m <- (n.causes%/%2) + 1
	pUp <- seq(1/m, 1-(1/m), 1/m)
	for (i in 1:(length(pUp))){
		grid.lines(x = c(pUp[i] - 0.15,	pUp[i]), 
			y = c(0.8,0.5),
			gp = gpar(col = ss.col[2], lwd=2))
		grid.text(causes.gr[i], 
			x = pUp[i] - 0.15,
			y = 0.81, 
			just = c("center", "bottom"))
		for (j in 1:length(causes[[i]])){
			grid.text(causes[[i]][j], 
				x = unit(pUp[i] - 0.15, "npc") + unit(j, "lines"),
				y = unit(0.80, "npc") - unit(j, "lines"), 
				gp = gpar(fontsize = 8),
				just = c("left", "center"))
			}
		}
	#down lines
	pDown <- pUp + (1/(2*m))
	if (n.causes%%2 != 0) pDown <- c(1/(m*2), pDown)
	k <- length(pDown)
	for (i in (length(pUp)+1):n.causes){
		grid.lines(x = c((pDown[k] - 0.15), pDown[k]), 
			y = c(0.2, 0.5),
			gp = gpar(col = ss.col[2], lwd=2))
		grid.text(causes.gr[i], 
			x = pDown[k] - 0.15, 
			y = 0.19, 
			just = c("center", "top"))
		for (j in 1:length(causes[[i]])){
			grid.text(causes[[i]][j],
				x = unit((pDown[k] - 0.15), "npc") + unit(j, "lines"),
				y = unit(0.20, "npc") + unit(j, "lines"), 
				gp = gpar(fontsize = 8),
				just=c("left", "center"))
		}
		k <- (k - 1)
	}
}
