two.mean.test<- function(  formula,  data ,    first.level, welch=TRUE,
                             direction = c("two.sided", "greater", "less"), conf.level= .95, 
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

  require( mosaic )
  
  var.equal = TRUE
  if(welch)
    var.equal= FALSE
  
  first.level<- trimws(first.level, which = c("both" ), whitespace = "[ \t\r\n]")
    direction<- trimws(direction, which = c("both" ), whitespace = "[ \t\r\n]")
 
  if(!(direction %in% c("greater", "less", "two.sided")))
    stop("Error: Direction must be either of 'greater',  'less', or 'two.sided' !!!!--- Try again :)")
  
  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  if(  is.na(pred.varName) | is.na(resp.varName) )
    stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format.")
  
  
  #eval(parse(text = paste0("data$", pred.varName, " = factor(data$", 
  #                         pred.varName, ")")))
  
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

	
  tresult   <- t.test(  resp ~ pred, data=dataOG, alternative = direction , 
                        conf.level = conf.level, var.equal=var.equal)
  obsTS<- tresult$statistic
  obs.teststat <- obsTS
  
  first.level  <- levels(pred)

  df=c(unlist(tresult$parameter))
  if(randtest)
    df ="N/A"
 
  

  dataOGsim <- dataOG

if(randtest){

 # edit fun below to just shuffle data then compute t-test using t.test...
 tmpfun <- function(X, datasim, data, alternative){

	dataOGsim$resp <- sample(data$resp, size=dim(data)[1], replace = FALSE)
	tresultrand   <- t.test(  resp ~ pred, data=dataOGsim, alternative = alternative , 
	                          var.equal=var.equal  )
	obsTSrand<- tresultrand$statistic
	return(obsTSrand)
  }
  
  
  randstats <- lapply(X=1:nshuffles, FUN=tmpfun,  datasim=dataOGsim , data=dataOG, alternative=direction)
  #sort(unlist( randstats) )
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
  
  
  htmp <- hist( randstats,   plot=FALSE)
  brks <-htmp$breaks
 
  br1<-do.breaks(c(abs(obs.teststat), max(brks)), length(brks)/3)
  br2<-do.breaks(c(min(brks), -abs(obs.teststat)), length(brks)/3)
  br3<-do.breaks(c(  -abs(obs.teststat), abs(obs.teststat)),  length(brks)/3)
  brksall <- sort(unique( c(br1, br2, br3) ))
  
  if(direction == "greater"){
    
    pvalue <- greater.pval
    cat1 <- rep(NA, length(randstats))
    cat1[ aboveidx] <-  "yes"
    cat1[ !aboveidx]  <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)
 
    
  }
  
  if(direction == "less"){
    
    pvalue <- less.pval
    cat1 <- rep(NA, length(randstats))
    cat1[ belowidx] <-  "yes"
    cat1[ !belowidx]  <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)

    
  }
  if(direction == "two.sided"){
    
    pvalue <- two.sided.pval
    cat1 <- rep(NA, length(randstats))
    cat1[ twsd.aboveidx] <-  "yes"
    cat1[ twsd.belowidx] <-  "yes"
    
    cat1[ is.na(cat1) ] <-  "no"
    cat2 <- as.factor(cat1)
    cprtmpdf <- data.frame( randstats, cat2)
    
  }	
  # add labels to the elements in the object 'temp'
   
  # first.level  <- levels(pred)

  hg <- histogram( ~ randstats ,  type="count", ylab="Number of simulations", xlab="Difference in means",
             groups=cat2, data=cprtmpdf,  breakds=brksall, nint=length(brksall))
 
   cat("     Simulation based two-sample test for independent samples", "\n")
  cat("                             ", "\n")
  
  
}else{


  tresultCI <- t.test(  resp ~ pred, data=dataOG, alternative = "two.sided" , conf.level = conf.level, var.equal=var.equal  )
  
   
  if(direction == "greater")    
    pvalue <- tresult$p.value#pt( tresult$statistic, df=min(n1-1, n2-1), lower.tail=FALSE)
  
  
  if(direction == "less") 
    pvalue <-  tresult$p.value#pt( tresult$statistic, df=min(n1-1, n2-1), lower.tail=TRUE)
  
  if(direction == "two.sided") 
    pvalue <-  tresult$p.value#2*pt( abs(tresult$statistic), df=min(n1-1, n2-1), lower.tail=FALSE)
  
  
  #MOE <- qt((1-conf.level)/2, df= min(n1-1, n2-1) , lower.tail=FALSE)*tresultCI$stderr
  lb <-  c(tresultCI$conf.int[1]) #obs.diff - MOE
  ub <-  c(tresultCI$conf.int[2]) #obs.diff + MOE
  
  eval(parse(text = paste0("data$", pred.varName, " = factor(data$", 
                           pred.varName, ")")))
  pred  <- eval(parse(text = paste0("data$", pred.varName)))
  #first.level  <- levels(pred)
  
 
	
  n <- dim(data)[1]
  teststat<- tresult$statistic
  if(direction == "greater"){
    hg<- plotDist('t', df=df, kind='density', xlim=c(-4.5, 4.5),  
                  panel = function(x,y,...){
                    panel.xyplot(x,y, ...); 
                    xx <- c( teststat, x[x>= teststat & x<=5], 5) 
                    yy <- c(0,   y[x>= teststat & x<=5], 0) 
                    panel.polygon(xx,yy, ..., col='blue')
                  }) 
  }
  
  if(direction == "less"){
    
    hg<- plotDist('t', df=df, kind='density', xlim=c(-4.5, 4.5),  
                  panel = function(x,y,...){
                    panel.xyplot(x,y, ...); 
                    xx <- c( -5, x[x>=  -5 & x<=teststat], teststat) 
                    yy <- c(0,   y[x>= -5 & x<=teststat], 0) 
                    panel.polygon(xx,yy, ..., col='blue')
                  }) 
    
  }
  
  
  if(direction == "two.sided"){
    
    hg<- plotDist('t', df=df, kind='density', xlim=c(-4.5, 4.5),  
                  panel = function(x,y,...){
                    panel.xyplot(x,y, ...); 
                    xx <- c( abs(teststat), x[x>= abs(teststat) & x<=5], 5) 
                    yy <- c(0,   y[x>= abs(teststat) & x<=5], 0) 
                    panel.polygon(xx,yy, ..., col='blue')
                    xx <- c( -5, x[x>=  -5 & x<=-abs(teststat)], -abs(teststat)) 
                    yy <- c(0,   y[x>= -5 & x<=-abs(teststat)], 0) 
                    panel.polygon(xx,yy, ..., col='blue')
                  }) 
    
  }
  
     cat("     Theoretical-based two-sample test for independent samples", "\n")
  cat("                             ", "\n")
 
}

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