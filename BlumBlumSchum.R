primeNr <- function(){
  n <- sample(2:100,1)
  i <- 1
  while(i < length(n)){
    p <- n[i]
    not.prime <- n[which(n %% p==0)]
    not.prime <- not.prime[! not.prime %in% p]
    n <- n[! n %in% not.prime]
    i <- i + 1
  }
  return(n)
}



blumNr <- function(){
	
	number <- primeNr()
	if(number %% 4 == 3)
		return(number)
	else 
		blumNr()

}

blumblum <- function()
{
	prime1 <- blumNr();
	prime2 <- blumNr();
	M <- prime1 * prime2;
	xi <- runif(1,min = 0,max = M-1)
	xi <- (xi*xi)%%M;
	xi <- as.integer(xi)
return(xi)
}



