
sfaov <- function(formula,   data, PWC = FALSE, welch= FALSE, conf.level= .95){
  # Description: To be added.
  

  
 if(welch){
  var.equal=FALSE
 }else{
   var.equal=TRUE
 }

  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  

  resp  <- eval(parse(text = paste0("data$", resp.varName)))
  # respOG  <- eval(parse(text = paste0("data$", resp.varName)))
  
  pred  <- eval(parse(text = paste0("data$", pred.varName)))
  #predOG  <- eval(parse(text = paste0("data$", pred.varName)))
  
  aovfitobj<- aov(  resp  ~  pred ) 
  aovfit<- summary(aovfitobj)
  
  #SSE <- round(aovfit$`Sum Sq`[1],3)
  #SSR <- round(aovfit$`Sum Sq`[2],3)
  #MSR <- round(aovfit$`Mean Sq`[1],3)
  #MSE <- round(aovfit$`Mean Sq`[2],3)
  pvalue<- round(aovfit[[1]]$`Pr(>F)`[1] ,3)
  obs.teststat <- round( aovfit[[1]]$`F value`[1],3)
  dfMST <-  aovfit[[1]]$Df[2]
  
  if(!var.equal){
    welchaovfit<- oneway.test( resp  ~  pred, var.equal = var.equal)
    obs.teststat <-  round(welchaovfit$statistic,3)
    dfMST <- round(welchaovfit$parameter[2],3)
    pvalue <- round(welchaovfit$p.value,3)
  }  
  
 
 
 
  
  
  n <- dim(data)[1] # sample size
  
  if(var.equal){
  cat("One-way analysis of means (assuming equal variances)", "\n")
  }else{
    cat("One-way analysis of means (not assuming equal variances)", "\n")
 
  }
  cat(" \n")
  
  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("============== \n")
  cat("ANOVA Table \n")
  
  
  aovfit2 <- aovfit# round(as.data.frame(aovfit),2)
  aovfit2[[1]]$`Pr(>F)`[1]<- c(round(pvalue,3))	
  names(aovfit2[[1]]) <- c(names(aovfit2[[1]])[1:4], "p-value")
  aovfit2[[1]]$Df[2] <- dfMST
  aovfit2[[1]]$`F value`[1] <-  obs.teststat
  

  rownames(aovfit2[[1]])[1] <-   "Treatment"#pred.varName	
  rownames(aovfit2[[1]])[2] <-  "Error"	

  #cat(pvalue, "\n")
  print(aovfit2[[1]])
  
  cat("============== \n")
  
  cat("\n")
  
  
  SumSqs <- aovfit[[1]]$`Sum Sq`
  rsqrd <- round(SumSqs[1]/SumSqs[2],3)
  
  cat("R-squared= ", rsqrd, "\n")
  cat("\n")
  
  if(welch)
    cat("Note: The Df for the Error component, F value and p-value provided are for Welch's F-test", "\n")
  #print(aovfit)
  

  
 
  if(PWC){
    

    samplemeansbylevel=model.tables(aovfitobj, "means" )
    samplemeansbylevel <-  c(round(samplemeansbylevel$tables$pred,2))
    
    
    if(var.equal){

      tukeyresult <- TukeyHSD(x=aovfitobj, conf.level=conf.level)
      

      cat("\n")
      cat("Sample group means:" , paste(names(samplemeansbylevel), "-", samplemeansbylevel, " " , sep=""), "\n")
      cat("\n")
      
      cat("Tukey multiple comparisons of means", "\n")
      print(tukeyresult$pred )
      
    }else{
            
      cat("\n")
      cat("Sample group means:" , paste(names(samplemeansbylevel), "-", samplemeansbylevel, " " , sep=""), "\n")
      cat("\n")
      
      cat("Games-Howell multiple comparisons of means", "\n")
      
      ghresult <- games.howell(grp=pred, obs=resp)
      ghresult2 <- ghresult[, c(1,2, 7,8, 6)]
      names(ghresult2)[2:5] <- c("diff", "lwr", "upr", " p adj")
      ghresult2 <- as.data.frame(ghresult2)
      print(ghresult2 )
      
      }
 

    }
    
  }
  
  
 
# source: https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096
games.howell <- function(grp, obs) {
  
  #Create combinations
  combs <- combn(unique(grp), 2)
  
  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(obs, grp, length)
  groups <- length(tapply(obs, grp, length))
  Mean <- tapply(obs, grp, mean)
  std <- tapply(obs, grp, var)
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    mean.diff <- Mean[combs[1,x]] - Mean[combs[2,x]]
    
    #t-values
    t <- abs(Mean[combs[2,x]] - Mean[combs[1,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))
    
    # Degrees of Freedom
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    #p-values
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    # Sigma standard error
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    # Upper Confidence Limit
    upper.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Lower Confidence Limit
    lower.conf <- lapply(1:ncol(combs), function(x) {
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
    })[[1]]
    
    # Group Combinations
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    # Collect all statistics into list
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
  })
  
  # Unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })
  
  # Create dataframe from flattened list
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  # Select columns set as factors that should be numeric and change with as.numeric
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
  
  # Rename data frame columns
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  
  return(results)
}