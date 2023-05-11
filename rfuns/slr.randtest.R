
slr.randtest <- function(formula,   data=NULL, nshuffles=0,  direction = c("greater", "less", "two.sided"), 
                plt=FALSE ){
  # Description: Function to compute the null distribution when comparing two props
  # and p-value
  # Arguments: 
  # formula: like t.test  (response ~ explanatory)
  # first.level: To determine how the difference in props is computed -- corresponds to the groups in the explanatory variable.
  # yesResponse: What is considered a sucess -- should match the success name in the dataframe
  # data:  Has to be in tidy or data matrix form.  
  # nshuffles: Number of randomizations
  # direction: specify the sign in Ha
  # returnRandStats: TRUE if you want the diff in props for each "shuffle"

   
  if(length(nshuffles)==0)
    nshuffles=10
  
  if (nshuffles < 1 | nshuffles %%1 != 0) 
    stop("Error: number of reps must be positive whole number!!! --- Try again :)")
  if(!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either of 'greater',  'less', or 'two.sided' !!!!--- Try again :)")
  direction<- trimws(direction, which = c("both" ), whitespace = "[ \t\r\n]")
  
  
  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  if(  is.na(pred.varName) | is.na(resp.varName) )
    stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format.")
  
  
  resp  <- eval(parse(text = paste0("data$", resp.varName)))

  pred  <- eval(parse(text = paste0("data$", pred.varName)))

  lmfit<- lm(  resp  ~  pred ) 
  tmpsum <- summary(lmfit)
  obs.teststat <- tmpsum$coefficients[2,3]
  l2.coefs <- coef(lmfit)
  r.squared <- tmpsum$r.squared
    
  
    
    tmpfun <- function(idx,  resp, pred){
      
      resp2 <- sample(resp)
      lmfitsim=  lm(  resp2  ~  pred ) 
      lmsumfitlmfitsim <- summary(lmfitsim)
      TSobssim <- lmsumfitlmfitsim$coefficients[2,3]
      return(TSobssim)
    }

  
  randstats <- lapply(X=1:nshuffles, FUN=tmpfun, resp=resp, pred=pred  )
  randstats=sort(unlist( randstats) )
  
  aboveidx <- randstats >= obs.teststat
  howmanyAbove <- sum( aboveidx )
  greater.pval <- howmanyAbove / nshuffles # store a one-sided pvalue
  
  belowidx <- randstats <= obs.teststat
  howmanyBelow <- sum( belowidx)
  
  less.pval <- howmanyBelow/ nshuffles # store a one-sided pvalue
  
  twsd.aboveidx <- randstats >= abs(obs.teststat)
  twsd.belowidx <- randstats <= -abs(obs.teststat)
  howmanyAboveBelow <- sum(twsd.aboveidx) + sum(twsd.belowidx)
  two.sided.pval <- howmanyAboveBelow/ nshuffles # store a one-sided pvalue
 
  if(direction == "greater"){
    pvalue <- greater.pval
  }
  
  if(direction == "less"){
    pvalue <- greater.pval
  } 
  
  if(direction == "two.sided"){
    
    pvalue <- two.sided.pval
  }
  
  
  if(plt){
  
  require(lattice)
    
  htmp <- hist( randstats,   plot=FALSE)
  brks <-htmp$breaks
  
  br1<-do.breaks(c(abs(obs.teststat), max(brks)), length(brks)/3)
  br2<-do.breaks(c(min(brks), -abs(obs.teststat)), length(brks)/3)
  br3<-do.breaks(c(  -abs(obs.teststat), abs(obs.teststat)),  length(brks)/3)
  brksall <- sort(unique( c(br1, br2, br3) ))
  
  if(direction == "greater"){
    
    cat1 <- rep(NA, length(randstats))
    cat1[ aboveidx] <-  "yes"
    cat1[ !aboveidx]  <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)
    
    
  }
  
  if(direction == "less"){
    
    cat1 <- rep(NA, length(randstats))
    cat1[ belowidx] <-  "yes"
    cat1[ !belowidx]  <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)
    
    
  }
  if(direction == "two.sided"){
    
    cat1 <- rep(NA, length(randstats))
    cat1[ twsd.aboveidx] <-  "yes"
    cat1[ twsd.belowidx] <-  "yes"
    
    cat1[ is.na(cat1) ] <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)
    
  }	

  hg <- histogram( ~ randstats ,  type="count", ylab="Number of simulations", xlab="Slope estimate",
                   groups=cat2, data=cprtmpdf,  breakds=brksall, nint=length(brksall))
  print(hg)
  return(hg)
  }
  
  
  cat(" Randomization test for a linear association in SLR", "\n")
  
  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("Obs. test statistic=", obs.teststat, "       \n" )
  cat("Obs. slope estimate=", l2.coefs[2], "       \n" )
  cat("R-squared =", r.squared, "\n")
  cat("p-value =", pvalue, "\n \n")
 

  
} 


