pkgname <- "SixSigma"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SixSigma')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SixSigma-package")
### * SixSigma-package

flush(stderr()); flush(stdout())

### Name: SixSigma-package
### Title: Six Sigma Tools for Quality and Process Improvement
### Aliases: SixSigma, SixSigma-package
### Keywords: package quality SixSigma

### ** Examples

example(ss.ci)
example(ss.study.ca)
example(ss.rr)
example(ss.lf)
example(ss.lfa)
example(ss.ceDiag)
example(ss.pMap)
example(ss.ca.yield)
example(ss.ca.z)
example(ss.ca.cp)
example(ss.ca.cpk)
example(ss.cc)



cleanEx()
nameEx("ss.ca.yield")
### * ss.ca.yield

flush(stderr()); flush(stdout())

### Name: ss.ca.yield
### Title: Main calculations regarding The Voice of the Process in
###   SixSigma: Yield, FTY, RTY, DPMO
### Aliases: ss.ca.yield
### Keywords: DPMO FTY RTY Yield

### ** Examples

ss.ca.yield(c(3,5,12),c(1,2,4),1915)



cleanEx()
nameEx("ss.ca.z")
### * ss.ca.z

flush(stderr()); flush(stdout())

### Name: ss.ca.z
### Title: Capability Indices
### Aliases: ss.ca.cp ss.ca.cpk ss.ca.z
### Keywords: capability cp cpk

### ** Examples

ss.ca.cp(ss.data.ca$Volume,740, 760)
ss.ca.cpk(ss.data.ca$Volume,740, 760)
ss.ca.z(ss.data.ca$Volume,740,760)



cleanEx()
nameEx("ss.cc")
### * ss.cc

flush(stderr()); flush(stdout())

### Name: ss.cc
### Title: Control Charts
### Aliases: ss.cc

### ** Examples

ss.cc("mr", ss.data.pb1, CTQ = "pb.humidity")
testout <- ss.data.pb1
testout[31,] <- list(31,17)
ss.cc("mr", testout, CTQ = "pb.humidity")



cleanEx()
nameEx("ss.cc.constants")
### * ss.cc.constants

flush(stderr()); flush(stdout())

### Name: ss.cc.constants
### Title: Functions to find out constants of the relative range
###   distribution.
### Aliases: ss.cc.constants ss.cc.getc4 ss.cc.getd2 ss.cc.getd3
### Keywords: charts constants control

### ** Examples

ss.cc.getd2(20)
ss.cc.getd3(20)
ss.cc.getc4(20)



cleanEx()
nameEx("ss.ceDiag")
### * ss.ceDiag

flush(stderr()); flush(stdout())

### Name: ss.ceDiag
### Title: Cause and Effect Diagram
### Aliases: ss.ceDiag
### Keywords: cause-and-effect

### ** Examples

effect <- "Flight Time"
causes.gr <- c("Operator", "Environment", "Tools", "Design",
  "Raw.Material", "Measure.Tool")
causes <- vector(mode = "list", length = length(causes.gr))
causes[1] <- list(c("operator #1", "operator #2", "operator #3"))
causes[2] <- list(c("height", "cleaning"))
causes[3] <- list(c("scissors", "tape"))
causes[4] <- list(c("rotor.length", "rotor.width2", "paperclip"))
causes[5] <- list(c("thickness", "marks"))
causes[6] <- list(c("calibrate", "model"))
ss.ceDiag(effect, causes.gr, causes, sub = "Paper Helicopter Project")



cleanEx()
nameEx("ss.ci")
### * ss.ci

flush(stderr()); flush(stdout())

### Name: ss.ci
### Title: Confidence Interval for the mean
### Aliases: ss.ci
### Keywords: confidence interval mean normality test

### ** Examples

ss.ci(len, data=ss.data.strings, alpha = 0.05,
  sub = "Guitar Strings Test | String Length",
  xname = "Length")



cleanEx()
nameEx("ss.data.batteries")
### * ss.data.batteries

flush(stderr()); flush(stdout())

### Name: ss.data.batteries
### Title: Data for the batteries example
### Aliases: ss.data.batteries
### Keywords: data datasets msa

### ** Examples

data(ss.data.batteries)
summary(ss.data.batteries)
plot(voltage~voltmeter, data = ss.data.batteries)



cleanEx()
nameEx("ss.data.bolts")
### * ss.data.bolts

flush(stderr()); flush(stdout())

### Name: ss.data.bolts
### Title: Data for the bolts example
### Aliases: ss.data.bolts
### Keywords: data datasets lfa

### ** Examples

data(ss.data.bolts)
summary(ss.data.bolts)
hist(ss.data.bolts$diameter)



cleanEx()
nameEx("ss.data.ca")
### * ss.data.ca

flush(stderr()); flush(stdout())

### Name: ss.data.ca
### Title: Data for a filling process in a winery
### Aliases: ss.data.ca
### Keywords: capability data datasets

### ** Examples

data(ss.data.ca)
summary(ss.data.ca)
hist(ss.data.ca$Volume)



cleanEx()
nameEx("ss.data.doe1")
### * ss.data.doe1

flush(stderr()); flush(stdout())

### Name: ss.data.doe1
### Title: Pizza dough example data
### Aliases: ss.data.doe1
### Keywords: data datasets doe

### ** Examples

data(ss.data.doe1)
summary(ss.data.doe1)
bwplot(score ~ flour | salt + bakPow , data = ss.data.doe1)



cleanEx()
nameEx("ss.data.doe2")
### * ss.data.doe2

flush(stderr()); flush(stdout())

### Name: ss.data.doe2
### Title: Data for the pizza dough example (robust design)
### Aliases: ss.data.doe2
### Keywords: data datasets doe

### ** Examples

data(ss.data.doe2)
summary(ss.data.doe2)
bwplot(score ~ temp | time, data = ss.data.doe2)



cleanEx()
nameEx("ss.data.pastries")
### * ss.data.pastries

flush(stderr()); flush(stdout())

### Name: ss.data.pastries
### Title: Pastries data
### Aliases: ss.data.pastries
### Keywords: data datasets msa

### ** Examples

data(ss.data.pastries)
summary(ss.data.pastries)
xyplot(comp ~ lab | batch, data = ss.data.pastries)



cleanEx()
nameEx("ss.data.pb1")
### * ss.data.pb1

flush(stderr()); flush(stdout())

### Name: ss.data.pb1
### Title: Particle Boards Example - Individual Data
### Aliases: ss.data.pb1
### Keywords: cc data datasets

### ** Examples

data(ss.data.pb1)
summary(ss.data.pb1)
library(qcc)
pb.groups.one <- with(ss.data.pb1, qcc.groups(pb.humidity, pb.group))
pb.xbar.one <- qcc(pb.groups.one, type="xbar.one")
summary(pb.xbar.one)
plot(pb.xbar.one)



cleanEx()
nameEx("ss.data.pb2")
### * ss.data.pb2

flush(stderr()); flush(stdout())

### Name: ss.data.pb2
### Title: Particle Boards Example - by Groups
### Aliases: ss.data.pb2
### Keywords: cc data datasets

### ** Examples

data(ss.data.pb2)
summary(ss.data.pb2)
if (require(qcc)){
  pb.groups.xbar <- with(ss.data.pb2, qcc.groups(pb.humidity, pb.group))
  pb.xbar <- qcc(pb.groups.xbar, type="xbar")
  summary(pb.xbar)
} else {
message("qcc package is needed to run this example")
}



cleanEx()
nameEx("ss.data.pb3")
### * ss.data.pb3

flush(stderr()); flush(stdout())

### Name: ss.data.pb3
### Title: Particle Boards Example - Attribute data
### Aliases: ss.data.pb3
### Keywords: cc data datasets

### ** Examples

data(ss.data.pb3)
summary(ss.data.pb3)
if (require(qcc)){
with(ss.data.pb3,
		plot(qcc(stockouts, orders, type ="p"))
	)
} else {
message("qcc package is needed to run this example")
}



cleanEx()
nameEx("ss.data.pb4")
### * ss.data.pb4

flush(stderr()); flush(stdout())

### Name: ss.data.pb4
### Title: Data for Practicle Boards Example - number of defects
### Aliases: ss.data.pb4
### Keywords: cc data datasets

### ** Examples

data(ss.data.pb4)
summary(ss.data.pb4)



cleanEx()
nameEx("ss.data.pc")
### * ss.data.pc

flush(stderr()); flush(stdout())

### Name: ss.data.pc
### Title: Data set for the printer cartridge example
### Aliases: ss.data.pc
### Keywords: charts data datasets

### ** Examples

data(ss.data.pc)
summary(ss.data.pc)



cleanEx()
nameEx("ss.data.pc.big")
### * ss.data.pc.big

flush(stderr()); flush(stdout())

### Name: ss.data.pc.big
### Title: Larger data set for the printer cartridges example
### Aliases: ss.data.pc.big
### Keywords: charts data datasets

### ** Examples

data(ss.data.pc.big)
summary(ss.data.pc.big)



cleanEx()
nameEx("ss.data.pc.r")
### * ss.data.pc.r

flush(stderr()); flush(stdout())

### Name: ss.data.pc.r
### Title: Data set for the printer cartridge example, by region
### Aliases: ss.data.pc.r
### Keywords: charts data datasets

### ** Examples

data(ss.data.pc.r)
summary(ss.data.pc.r)



cleanEx()
nameEx("ss.data.rr")
### * ss.data.rr

flush(stderr()); flush(stdout())

### Name: ss.data.rr
### Title: Gage R&R data
### Aliases: ss.data.rr
### Keywords: data datasets msa

### ** Examples

data(ss.data.rr)
summary(ss.data.rr)



cleanEx()
nameEx("ss.data.strings")
### * ss.data.strings

flush(stderr()); flush(stdout())

### Name: ss.data.strings
### Title: Data set for the Guitar Strings example
### Aliases: ss.data.strings
### Keywords: data datasets msa

### ** Examples

data(ss.data.strings)
summary(ss.data.strings)



cleanEx()
nameEx("ss.heli")
### * ss.heli

flush(stderr()); flush(stdout())

### Name: ss.heli
### Title: Creates a pdf file with the design of the Paper Helicopter
### Aliases: ss.heli

### ** Examples

ss.heli()
vignette("HelicopterInstructions")



cleanEx()
nameEx("ss.lf")
### * ss.lf

flush(stderr()); flush(stdout())

### Name: ss.lf
### Title: Evaluates the Loss Function for a process.
### Aliases: ss.lf
### Keywords: function loss Taguchi

### ** Examples

#Example bolts: evaluate LF at 10.5 if Target=10, Tolerance=0.5, L_0=0.001
ss.lf(10.5, 0.5, 10, 0.001)



cleanEx()
nameEx("ss.lfa")
### * ss.lfa

flush(stderr()); flush(stdout())

### Name: ss.lfa
### Title: Loss Function Analysis
### Aliases: ss.lfa
### Keywords: function loss Taguchi

### ** Examples

ss.lfa(ss.data.bolts, "diameter", 0.5, 10, 0.001,
		lfa.sub = "10 mm. Bolts Project",
		lfa.size = 100000, lfa.output = "both")



cleanEx()
nameEx("ss.pMap")
### * ss.pMap

flush(stderr()); flush(stdout())

### Name: ss.pMap
### Title: Process Map
### Aliases: ss.pMap
### Keywords: map process

### ** Examples

inputs.overall<-c("operators", "tools", "raw material", "facilities")
outputs.overall<-c("helicopter")
steps<-c("INSPECTION", "ASSEMBLY", "TEST", "LABELING")
#Inputs of process "i" are inputs of process "i+1"
input.output<-vector(mode="list",length=length(steps))
input.output[1]<-list(c("sheets", "..."))
input.output[2]<-list(c("sheets"))
input.output[3]<-list(c("helicopter"))
input.output[4]<-list(c("helicopter"))

#Parameters of each process
x.parameters<-vector(mode="list",length=length(steps))
x.parameters[1]<-list(c(list(c("width", "NC")),list(c("operator", "C")),
list(c("Measure pattern", "P")), list(c("discard", "P"))))
x.parameters[2]<-list(c(list(c("operator", "C")),list(c("cut", "P")),
list(c("fix", "P")), list(c("rotor.width", "C")),list(c("rotor.length",
"C")), list(c("paperclip", "C")), list(c("tape", "C"))))
x.parameters[3]<-list(c(list(c("operator", "C")),list(c("throw", "P")),
list(c("discard", "P")), list(c("environment", "N"))))
x.parameters[4]<-list(c(list(c("operator", "C")),list(c("label", "P"))))
x.parameters

#Features of each process
y.features<-vector(mode="list",length=length(steps))
y.features[1]<-list(c(list(c("ok", "Cr"))))
y.features[2]<-list(c(list(c("weight", "Cr"))))
y.features[3]<-list(c(list(c("time", "Cr"))))
y.features[4]<-list(c(list(c("label", "Cr"))))
y.features

ss.pMap(steps, inputs.overall, outputs.overall,
        input.output, x.parameters, y.features,
        sub="Paper Helicopter Project")



cleanEx()
nameEx("ss.rr")
### * ss.rr

flush(stderr()); flush(stdout())

### Name: ss.rr
### Title: Gage R & R (Measure System Assessment)
### Aliases: ss.rr
### Keywords: Gauge MSA repeatability reproducibility R&R

### ** Examples

data(ss.data.rr)
ss.rr(time1, prototype, operator, data=ss.data.rr,
	sub="Six Sigma Paper Helicopter Project")



cleanEx()
nameEx("ss.study.ca")
### * ss.study.ca

flush(stderr()); flush(stdout())

### Name: ss.study.ca
### Title: Graphs and figures for a Capability Study
### Aliases: ss.study.ca
### Keywords: capability

### ** Examples

ss.study.ca(ss.data.ca$Volume, rnorm(40, 753, 3),
		LSL = 740, USL = 760, T = 750, alpha = 0.05,
 			f.sub = "Winery Project")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
