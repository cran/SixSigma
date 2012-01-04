ss.cc.getc4 <- function(n = NA){
	if (is.na(n) | n < 2 | abs(n-round(n))!=0){
		stop("Invalid sample size")
	}
	c4=sqrt(2/(n-1)) * ((gamma(n/2))/(gamma((n-1)/2)))
	names(c4)=c("c4")
	return(c4)
}

ss.cc.getd2 <- function (n = NA){
	if (is.na(n) | n < 2 | abs(n-round(n))!=0){
		stop("Invalid sample size")
	}
	f <- function(x){
		1 - ptukey(x, n, Inf)
	}
	d2 <- integrate(f, 0, Inf)
	if (d2$abs.error > 0.001) 
		warning("The absolute error after numerical integration 
						is greater than 0.001")
	d2 <- d2$value
	names(d2) <- c("d2")
	return(d2)
}

ss.cc.getd3 <- function (n = NA){
	if (is.na(n) | n < 2 | abs(n-round(n))!=0){
		stop("Invalid sample size")
	}
	f <- function (x){
		x * (1 - ptukey(x, n, Inf))
	}
	d3 <- integrate(f, 0, Inf)
	if (d3$abs.error > 0.001) 
		warning("The absolute error after numerical integration
						is greater than 0.001")
	d3 <- 2 * d3$value
	this.d2 <- ss.cc.getd2(n)
	d3 <- sqrt(d3 - this.d2^2)
	names(d3) <- c("d3")
	return(d3)
}

