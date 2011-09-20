ss.rr<-function(var, part, appr, 
		data = "stop('Data' is required for lattice graphics)", 
		main = "Six Sigma Gage R&R Study", sub = ""){

##Figures and facts
    if (is.data.frame(data)){attach(data,warn.conflicts=FALSE)}
    a<-length(levels(part))
    b<-length(levels(appr))
    n<-length(var)/(a*b)
    options(show.signif.stars=FALSE)
    model<-anova(lm(var~part+appr+part*appr))
    rownames(model)[length(rownames(model))]<-"Repeatability"
    print(model)
    varComp<-matrix(ncol=5,nrow=7)
    rownames(varComp)<-c("Total Gage R&R", "  Repeatability", "  Reproducibility",
                        paste("   ", rownames(model)[2]),
                        paste("   ",rownames(model)[3]), "Part-To-Part",
                         "Total Variation")
    colnames(varComp)<-c("VarComp","%Contrib","StdDev", "5.15*SD","%StudyVar")
    varComp[2,1]<-model[4,3]
    varComp[4,1]<-max(c((model[2,3]-model[3,3])/(a*n),0))
    varComp[5,1]<-max(c((model[3,3]-model[4,3])/n,0))
    varComp[3,1]<-varComp[4,1]+varComp[5,1]
    varComp[6,1]<-max(c((model[1,3]-model[3,3])/(b*n),0))
    varComp[1,1]<-varComp[2,1]+varComp[3,1]
    varComp[7,1]<-varComp[1,1]+varComp[6,1]
    varComp[,1]<-round(varComp[,1],7)
    varComp[,2]<-round(100*(varComp[,1]/varComp[7,1]),2)
    varComp[,3]<-sqrt(varComp[,1])
    varComp[,4]<-varComp[,3]*5.15
    varComp[,5]<-round(100*(varComp[,4]/varComp[7,4]),2)

    cat(paste("\nGage R&R\n"))
    print(varComp[,1:2])
    cat("\n")
    print(varComp[,3:5])
    ncat<-max(c(1,floor((varComp[6,3]/varComp[1,3])*1.41)))
    cat(paste("\nNumber of Distinct Categories=",ncat,"\n"))

##graph
    .ss.prepCanvas(main, sub)
    vp.plots<-viewport(name="plots",
                       layout=grid.layout(3,2))
    pushViewport(vp.plots)
    vp.bar<-viewport(name="barplot", layout.pos.row=1, layout.pos.col=1)
    pushViewport(vp.bar)
    databar<-cbind(varComp[c(1,2,3,6),2],varComp[c(1,2,3,6),5])
    rownames(databar)<-c("G.R&R","Repeat","Reprod","Part2Part")
    #Barchart
    plot<- barchart(databar, freq=FALSE, grid=TRUE,
                    par.settings=list(axis.text=list(cex=0.6),
                                      par.ylab.text=list(cex=0.8),
                                      par.main.text=list(cex=0.85)),
                    ylab=list("Percent",fontsize=8),
                    panel=function(...){
                        panel.barchart(...)
                        panel.abline(h=0)
                    },
                    auto.key=list(text=c("%Contribution","%Study Var"),
                                 cex=0.8,
                                 columns=2, space="bottom", cex=0.8,
                                 rectangles=TRUE, points=FALSE,adj=1,
                                 rep=FALSE
                                 ),
                    stack=FALSE,horizontal=FALSE,
                    main=list("Components of Variation",fontsize=14))
    print(plot, newpage=FALSE)
    popViewport()
    vp.varByPart<-viewport(name="varByPart",layout.pos.row=1, layout.pos.col=2)
    pushViewport(vp.varByPart)
    #Var by part
    plot<-stripplot(var~part,data=data, grid=TRUE,
                    par.settings=list(axis.text=list(cex=0.6),
                                   par.xlab.text=list(cex=0.8),
                                   par.ylab.text=list(cex=0.8),
                                   par.main.text=list(cex=0.9)),
                    main="Var by Part",
                    type=c("p","a"))
    print(plot,newpage=FALSE)
    popViewport()
    vp.varByAppr<-viewport(name="varByAppr",layout.pos.row=2, layout.pos.col=2)
    pushViewport(vp.varByAppr)
    #var by appraiser
    plot<-stripplot(var~appr,data=data, grid=TRUE,
                    par.settings=list(axis.text=list(cex=0.6),
                                   par.xlab.text=list(cex=0.8),
                                   par.ylab.text=list(cex=0.8),
                                   par.main.text=list(cex=0.9)),
                    main="Var by appraiser",
                    type=c("p","a"))
    print(plot,newpage=FALSE)
    popViewport()
    vp.Interact<-viewport(name="Interact",layout.pos.row=3, layout.pos.col=2)
    pushViewport(vp.Interact)

    #Interaction
    data.xbar<-aggregate(data=ss.data.rr,var~appr+part,mean)
    plot<-stripplot(var~part,groups=appr, data=data.xbar, pch=16,grid=TRUE,
                    par.settings=list(par.main.text=list(cex=0.9)),
                    main="Part*appraiser Interaction",
                    type=c("p","a"),
                    auto.key=list(text=levels(appr),
                                  columns=2, space="bottom", cex=0.5,
                                  lines=TRUE, points=FALSE,adj=1))
    print(plot,newpage=FALSE)
    popViewport()

    #Control Charts

    data.xrange<-aggregate(data=ss.data.rr,var~appr+part,function(x)max(x)-min(x))
    ar<-mean(data.xrange$var)
    #Mean
    vp.ccMean<-viewport(name="ccMean",layout.pos.row=3, layout.pos.col=1)
    pushViewport(vp.ccMean)
    plot<-xyplot(data=data.xbar, var~part|appr, pch=16,
                 par.settings=list( axis.text=list(cex=0.6),
                                   par.xlab.text=list(cex=0.8),
                                   par.ylab.text=list(cex=0.8),
                                   par.main.text=list(cex=0.9)),
                 par.strip.text=list(cex=0.6),
                 main=expression(bold(bar(x)*" Chart by appraiser")),grid=TRUE,
                 layout=c(b,1),
                 type="b",
                 panel=function(...){
                     panel.xyplot(...)
                     panel.abline(h=mean(var,na.rm=TRUE), lty=2)
                     panel.abline(h=mean(var,na.rm=TRUE)+
                                  (3/(.ss.cc.getConst(n,"d2")*sqrt(n)))*ar)
                     panel.abline(h=mean(var,na.rm=TRUE)-
                                  (3/(.ss.cc.getConst(n,"d2")*sqrt(n)))*ar)

                 }
                 )
    print(plot,newpage=FALSE)
    popViewport()
    ##Range
    vp.ccRange<-viewport(name="ccRange", layout.pos.row=2, layout.pos.col=1)
    pushViewport(vp.ccRange)


    plot<-xyplot(data=data.xrange, var~part|appr, pch=16,
                 par.settings=list(axis.text=list(cex=0.6),
                                   par.xlab.text=list(cex=0.8),
                                   par.ylab.text=list(cex=0.8),
                                   par.main.text=list(cex=0.9)),
                 par.strip.text=list(cex=0.6),
                 main="R Chart by appraiser",grid=TRUE,
                 layout=c(b,1),
                 type="b",
                 panel=function(...){
                     panel.xyplot(...)
                     panel.abline(h=ar, lty=2)
                     panel.abline(h=ar*(1+
                                  (.ss.cc.getConst(n,"d3")/(.ss.cc.getConst(n,"d2")))))
                     panel.abline(h=ar*(1-
                                  (.ss.cc.getConst(n,"d3")/(.ss.cc.getConst(n,"d2")))))

                 }
                 )
    print(plot,newpage=FALSE)
    popViewport()
	if (is.data.frame(data)){detach(data)}
	invisible(list(anovaTable=model,varComp=varComp[,1:2],
					studyVar=varComp[,3:5],ncat=ncat))
}
