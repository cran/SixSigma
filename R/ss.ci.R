ss.ci<-function(x, sigma2 = NA, alpha = 0.05, data = NA, 
		xname = "x", approx.z = FALSE, main = "Confidence Interval for the Mean", 
		digits = 3,
		sub = "", ss.col = c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")){
	if (is.data.frame(data)){
        attach(data, warn.conflicts=FALSE)
    }
    na <- length(which(is.na(x)))
    if (na > 0) { cat(na, " missing values were ommitted\n")}
    m <- mean(x, na.rm = TRUE)
    n <- length(x) - na
    s <- ifelse(is.numeric(sigma2), 
			sqrt(sigma2),
			sd(x, na.rm = TRUE))
    if (is.numeric(sigma2) | approx.z == TRUE){
        st <- qnorm(1 - (alpha/2))
        st.dist <- c("z")
    }
    else{
		if (n < 30) {
        	warning("\nThe sample size is lower than 30. Check Normality\n\n")
		}
		st <- qt(1-(alpha/2), n-1)
        st.dist <- c("t")
    }
    dist <- st * (s/sqrt(n))

    cat("\tMean = ", round(m, digits), "; sd = ", round(s, digits), "\n", sep = "")
    cat("\t", (1-alpha)*100, "% Confidence Interval= ", 
			round(m-dist, digits), " to ", round(m+dist, digits),"\n\n", sep = "")
    ci <- c(m-dist, m+dist)
	names(ci) <- c("LL", "UL")
##Canvas-container
    .ss.prepCanvas(main,sub, ss.col)

##figures
    vp.figures <- viewport(name = "figures", 
			x = 0, 
			width = 1, 
			height = unit(8, "lines"),
			y = 1, 
			just = c("left", "top"),
			layout = grid.layout(1, 2, widths = c(0.4, 0.6))						 )
    pushViewport(vp.figures)

    vp.figures1 <- viewport(name = "figures1", 
			layout.pos.col = 1, 
			layout.pos.row = 1)
    pushViewport(vp.figures1)
    grid.roundrect(height = unit(7, "lines"), 
       width = 0.95,
       gp = gpar(fill = ss.col[5], col = ss.col[2], lwd=2))
    grid.text("Mean:\nStdDev:\nn:\nMissing:", just = "left",
		x = unit(1, "npc") - unit(5.5, "cm"), 
		gp = gpar(fontface = c("bold")))
    grid.text(paste(round(m, digits), "\n", round(s, digits), "\n", n,
					"\n", na, sep = ""), just = "right", 
			x = unit(1, "npc") - unit(1, "cm"))


    popViewport()
    vp.figures2 <- viewport(name = "figures2", layout.pos.col = 2,
			layout.pos.row = 1)
    pushViewport(vp.figures2)
    grid.roundrect(height = unit(7, "lines"), 
            width = 0.95,
            gp = gpar(fill = ss.col[5], col = ss.col[2], lwd = 2))

    grid.text(paste((1-alpha)*100, "% CI:\nP-Var:\n",
                    st.dist, ":", sep = ""),
              just = "left",
              x = unit(0,"npc") + unit(1,"cm"), 
			  gp = gpar(fontface=c("bold")))
    grid.text(paste("[", round(ci[1], digits), ", ", round(ci[2], digits),
            "]\n", ifelse(is.numeric(sigma2), sigma2, "unknown"),
			"\n", round(st, digits),
            sep = ""), just = "right", 
			x = unit(0,"npc") + unit(7.5,"cm"))
    popViewport()
    popViewport()

#graph
    vp.graph <- viewport(name = "graph", 
			y = 0, 
			width = 0.95,
            height = unit(1, "npc") - unit(8, "lines"),
            just = c("center", "bottom"),
			layout = grid.layout(1, 2, 
				widths = unit(c(1, 6), c("null", "cm"))))
    pushViewport(vp.graph)
	
    vp.test <- viewport(name = "test", layout.pos.row = 1, layout.pos.col = 2)
    pushViewport(vp.test)
    grid.rect()
    grid.roundrect(height = unit(6, "lines"),
                   width = 0.9, 
				   y = unit(1, "npc") + unit(-1, "lines"),
				   just = "top",
                   gp = gpar(fill = ss.col[5], col = ss.col[2], lwd = 2))

    grid.text("Shapiro-Wilks\nNormality Test\n", 
			y = unit(1, "npc") - unit(3, "lines"),
			gp = gpar(fontface = c("bold")))
	pval <- shapiro.test(x)[2]$p.value 
	if (pval < 0.05){
		warning("Sample data is non-normal.")
	}
    grid.text(paste(round(shapiro.test(x)[1]$statistic, digits), "\n"),
			y = unit(1, "npc") - unit(5, "lines"))
    grid.text(paste("p-value:", round(pval, digits), "\n"),
              y = unit(1, "npc") - unit(6, "lines"))
	
	vp.qq <- viewport(name="qqp", 
			x = 0.5, y=0.25,
			  height = unit(0.6,"npc"))
	pushViewport(vp.qq)
	qqp <- qplot(sample = x, stat="qq") + 
			  xlab(NULL) + ylab(NULL) +
			  opts(axis.text.x = theme_blank(),
					  axis.text.y = theme_blank(),
					  title = "Normal q-q Plot") 
	print(qqp,newpage=FALSE)  
	popViewport()
    popViewport()

    vp.hist <- viewport(name = "hist", 
			layout.pos.row = 1,
			layout.pos.col = 1)
    pushViewport(vp.hist)
    grid.rect()
	
	ggdata <- melt(x)
	binw <- diff(range(x))/ sqrt(n)
	qqp <- ggplot(ggdata, aes(x = value))
	myhist <- qqp + 
			geom_histogram(aes(y = ..density..), 
					binwidth = binw,
					fill = "white",
					col = "gray",
					stat = "bin") +
			xlab(paste("Value of", deparse(substitute(x)))) +
			opts(title = "Histogram & Density Plot") +
			stat_density(geom="path", position="identity", 
					binwidth = binw,
					size = 1) 
    print(myhist, newpage=FALSE)

    return (ci)
}
