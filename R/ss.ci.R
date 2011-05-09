ss.ci<-function(x,sigma="", alpha=0.05, data="", xname="x", approx.z=TRUE, main="Confidence Interval for the mean", sub="", ss.col=c("#666666", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE")){
    if (is.data.frame(data)){
        attach(data, warn.conflicts=FALSE)
    }
    na<-length(which(is.na(x)))
    if (na>0){cat(na," missing values were ommitted\n")}
    m<-mean(x,na.rm=TRUE)
    n<-length(x)-na
    s<-ifelse(is.numeric(sigma),sqrt(sigma),sd(x, na.rm=TRUE))
    if (is.numeric(sigma)|| n>30 || approx.z==TRUE){
        st<-qnorm(1-(alpha/2))
        st.dist<-c("z")
    }
    else{
        message("\nWarning: the sample size is less than 30. Check Normality\n\n")
        st<-qt(1-(alpha/2),n-1)
        st.dist<-c("t")
    }
    dist<-st*(s/sqrt(n))

    cat("\tMean=",round(m,4),"; sd=",round(s,4),"\n",sep="")
    cat("\t",(1-alpha)*100, "% Confidence Interval= ",round(m-dist,4)," to ", round(m+dist,4),"\n\n", sep="")
    ci<-c(m-dist, m+dist)
##Canvas-container
    .ss.prepCanvas(main,sub, ss.col)

##figures
    vp.figures<-viewport(name="figures",x=0, w=1, h=unit(8,"lines"),
                         y=1, just=c("left", "top") ,
                         layout=grid.layout(1,2))
    pushViewport(vp.figures)

    vp.figures1<-viewport(name="figures1", layout.pos.col=1, layout.pos.row=1)
    pushViewport(vp.figures1)
    grid.roundrect(height=unit(7,"lines"), x=unit(1,"npc")-unit(0.5,"cm"),
                   width=unit(4.5,"cm"),
                   gp=gpar(fill=ss.col[5], col=ss.col[2], lwd=2),
                   just=c("right","center"))

    grid.text("Mean:\nStdDev:\nn:\nMissing:",just="left",
              x=unit(1,"npc")-unit(4.5,"cm"), gp=gpar(fontface=c("bold")))
    grid.text(paste(round(m,4),"\n",round(s,4),"\n",n,
                    "\n",na,sep=""),just="right", x=unit(1,"npc")-unit(1,"cm"))


    popViewport()
    vp.figures2<-viewport(name="figures2", layout.pos.col=2, layout.pos.row=1)
    pushViewport(vp.figures2)
    grid.roundrect(height=unit(7,"lines"), x=unit(0,"npc")+unit(0.5,"cm"),
                   width=unit(6,"cm"),
                   gp=gpar(fill=ss.col[5], col=ss.col[2], lwd=2),
                   just=c("left","center"))

    grid.text(paste((1-alpha)*100,"% CI:\nP-Var:\n",
                    st.dist,":",sep=""),
              just="left",
              x=unit(0,"npc")+unit(1,"cm"), gp=gpar(fontface=c("bold")))
    grid.text(paste(round(ci[1],4)," - ",round(ci[2],4),
                    "\n",ifelse(is.numeric(sigma),sigma,"unknown"),"\n",round(st,4),
                    sep=""),just="right", x=unit(0,"npc")+unit(6,"cm"))
    popViewport()
    popViewport()

#graph
    vp.graph<-viewport(name="graph",y=0, width=0.95,
                       h=unit(1,"npc")-unit(8,"lines"),
                       just=c("center", "bottom"),
                       layout=grid.layout(1,2, widths=unit(c(1,6),c("null","cm"))))
    pushViewport(vp.graph)
    vp.test<-viewport(name="test",layout.pos.row=1,layout.pos.col=2)
    pushViewport(vp.test)
    grid.rect()
    grid.roundrect(height=unit(6,"lines"),
                   width=0.9, y=unit(0.5,"npc")+unit(1,"lines"),
                   gp=gpar(fill=ss.col[5], col=ss.col[2], lwd=2))

    grid.text("Shapiro-Wilks\nNormality Test\n",y=unit(0.5,"npc")+unit(2,"lines"),
              gp=gpar(fontface=c("bold")))
    grid.text(paste(round(shapiro.test(x)[1]$statistic,4),"\n"))
    grid.text(paste("p-value:",round(shapiro.test(x)[2]$p.value,4),"\n"),
              y=unit(0.5,"npc")-unit(1,"lines"))
    popViewport()

    vp.hist<-viewport(name="hist",layout.pos.row=1,layout.pos.col=1)
    pushViewport(vp.hist)
    grid.rect()
    myhist<-histogram(x,col=ss.col[1],
                      main=paste("Histogram of",xname),
                      xlab=xname)
    print(myhist,newpage=FALSE)

    return (c(m-dist, m+dist))
}
