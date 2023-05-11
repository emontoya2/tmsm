two.mean.test<- function(  formula,  data ,    first.level, welch=TRUE,
                             direction = c("two.sided", "greater", "less"),  
                           randtest=FALSE, nshuffles=NULL, returnRandStats=FALSE, printout=TRUE , printoutND=FALSE){							
  # Description: Function to compute the test statistic, null distribution, p-value, and CI
  # for a two-sample t-based method. It also allows for a randomization test using Welch's t-test test statistic.
  # Arguments: 
  # formula:  response ~ explanatory. response is replaced with the name of the the resposne (measured) 
  #    variable of interest, and explanatory is replaced x with the name of the factor or grouping
  #    variable that characterizes the different populations  or treatments
  # data: set equal to a dataframe that is in tidy form
  # first.level: A level/category from the grouping variable.  It 
  #             determines how the difference in sample means is computed.
  #             It should be consistent with the formulation of the hypothesis
  # direction: the sign in the alternative: "two.sided" (default), "greater" , or "less"
  # conf.level: confidence level for the CI (default is .95). The function will always provide
  #             a CI by default.
  # welch: Set equal to TRUE (default) for Welch's t-test. Set to FALSE for Student's t-test
  # randtest: Set equal to TRUE to carry out a randomization test
  # nshuffles: The number of randomization for the randomization test. 
 
  
  first.level<- trimws(first.level, which = c("both" ), whitespace = "[ \t\r\n]")
    direction<- trimws(direction, which = c("both" ), whitespace = "[ \t\r\n]")
 
  if(!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either of 'greater',  'less', or 'two.sided' !!!!--- Try again :)")
  
  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  if(  is.na(pred.varName) | is.na(resp.varName) )
    stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format.")
  
  
  eval(parse(text = paste0("data$", pred.varName, " = factor(data$", 
                           pred.varName, ")")))
  
  resp  <- eval(parse(text = paste0("data$", resp.varName)))
  
  pred  <- eval(parse(text = paste0("data$", pred.varName)))
  

  
  n <- dim(data)[1] # sample size
  
  tmpidx2 = which( levels(pred) == first.level) # determine primary last level
  pred <- relevel(pred, levels(pred)[tmpidx2]) # change order depending on first level
  dataOG <- data.frame( resp=resp, pred=pred)
  
  
  respord  <- dataOG$resp # eval(parse(text = paste0("dataOR$", resp.varName)))
  preord  <-  dataOG$pred #eval(parse(text = paste0("dataOR$", pred.varName)))
  
  #datax <- c(preord)-1   #   1,  2--> first grp is 0, 2nd grp is is 1
  datax <-  as.numeric(as.factor(  preord)) -1 

  n1 <- sum(datax==0)  
  n2 <- sum(datax==1)
  
  tmpa = mean( c( respord[which(datax==0)] )  )
  tmpb = mean( c( respord[which(datax==1)] )  )
  tmpasd = sd( c( respord[which(datax==0)] )  )
  tmpbsd = sd( c( respord[which(datax==1)] )  )

  #obs.diff <- tmpa - tmpb			 # obs
  
  alternative <- direction

	
  tresult   <- wilcox.test(  resp ~ pred, data=dataOG, alternative = direction  )
  
  x <- dataOG$resp 
  g<-  dataOG$pred
  ng <- table(g)
  
  if (any(ng<2)) {
    ng.to.del <- names(ng)[which(ng<2)]
    x <- x[g!=ng.to.del]
    g <- droplevels(g[g!=ng.to.del])
  }
  med <- median(x)
  cont <- table(x>med,g)
  
  
  test <- suppressWarnings(chisq.test(cont))
   
  
  obsTS<- tresult$statistic#test$statistic#$tresult$statistic
  obs.teststat <- obsTS
  
  first.level  <- levels(pred)

 
 
 
  

  dataOGsim <- dataOG

 

 # edit fun below to just shuffle data then compute t-test using t.test...
 tmpfun <- function(X, datasim, data){

	dataOGsim$resp <- sample(data$resp, size=dim(data)[1], replace = FALSE)
	#tresultrand   <- t.test(  resp ~ pred, data=dataOGsim, alternative = direction , 
	#                          var.equal=var.equal  )
	
	x <- dataOGsim$resp 
	g<-  dataOGsim$pred
	ng <- table(g)
	
	if (any(ng<2)) {
	  ng.to.del <- names(ng)[which(ng<2)]
	  x <- x[g!=ng.to.del]
	  g <- droplevels(g[g!=ng.to.del])
	}
	med <- median(x)
	cont <- table(x>med,g)
	
	
	testrand <- suppressWarnings(chisq.test(cont))
	
	tresult   <- wilcox.test(  resp ~ pred, data=dataOGsim, alternative = direction  )
	
	
	obsTSrand<- tresult$statistic#testrand$statistic
	return(obsTSrand)
  }
  
  
  randstats <- lapply(X=1:nshuffles, FUN=tmpfun,  datasim=dataOGsim , data=dataOG)
  #sort(unlist( randstats) )
  randstats=sort(unlist( randstats) )
  hist(randstats, freq=F)
  lines( seq(0, 100, len=100), dwilcox(seq(0, 100, len=100), 10, 10))  
  plot( seq(0, 100, len=100), dwilcox(seq(0, 1, len=100), 10, 10))  
  
  x <- -1:(10*10 + 1)
  fx <- dwilcox(x, 10,10)
  Fx <- pwilcox(x, 4, 6)
  
  minx=n1-5
  maxx=n1*n2+5
  
  layout(rbind(1,2), widths = 1, heights = c(3,2))
  lines(x, fx, type = "h", col = "violet",
       main =  "Probabilities (density) of Wilcoxon-Statist.(n=6, m=4)")
  
  
  aboveidx <- randstats >= obs.teststat
  howmanyAbove <- sum( aboveidx )
  greater.pval <- howmanyAbove / nshuffles # store a one-sided pvalue
  
  #belowidx <- randstats <= obs.teststat
  #howmanyBelow <- sum( belowidx)
  #less.pval <- howmanyBelow/ nshuffles # store a one-sided pvalue
  #  twsd.aboveidx <- randstats >= abs(obs.teststat)
  #twsd.belowidx <- randstats <= -abs(obs.teststat)
  #howmanyAboveBelow <- sum(twsd.aboveidx) + sum(twsd.belowidx)
  #two.sided.pval <- howmanyAboveBelow/ nshuffles # store a one-sided pvalue
  
 
    
 
  
  # add labels to the elements in the object 'temp'
   
  # first.level  <- levels(pred)

  hg <- histogram( ~ randstats ,  type="count", ylab="Number of simulations", xlab="Mood's median test statistic"  )
 
   cat("     Simulation based two-sample test for independent samples", "\n")
  cat("                             ", "\n")
  
  
 

  pvalue <- round(pvalue, 3)


  if(printout){
 
  cat("formula: ", resp.varName, "~", pred.varName, "\n")
    
    cat("sample mean of ",  first.level[1]  ," group:", tresult[[5]][1] , "\n")
    cat("sample mean of ",  first.level[2]  ," group:", tresult[[5]][2] , "\n")
    cat("sample sd of ",  first.level[1]  ," group:", tmpasd , "\n")
    cat("sample sd of ",  first.level[2]  ," group:", tmpbsd , "\n")
 
     cat("\n")
	 
  cat("difference between groups: (",  first.level[1]  ," group ) - ( ", first.level[2] ," group )"  ,"\n")
  cat("obs t-test statistic:", obsTS, "            ", "p-value =", pvalue, "\n")
  cat("df= ", df,"\n")
  cat("direction:" , direction, "\n")
   cat("\n")

 

   if(!randtest){
   cat("Confidence level: " , conf.level, "\n")
   cat("CI:( ",  lb,", ",  ub, ") \n")
   }
  }
  
    
  if(returnRandStats)
    return( randstats)
  
  if(printoutND){ 
  return(hg)
	}
  
  # returns the p-value, the test stat for each randomization, and the
  # observed test stat



  
}