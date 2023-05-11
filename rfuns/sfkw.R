
sfkw <- function(formula,   data, PWC = FALSE ){
  # Description: Function to compute the null distribution when comparing two props
 
  require(effectsize)

  
  #stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format... of if you are trying to include more covariates this method is not applicable")
  
  resp.varName <- all.vars(formula)[1]
  pred.varName <- all.vars(formula)[2]
  

  resp  <- eval(parse(text = paste0("data$", resp.varName)))
  # respOG  <- eval(parse(text = paste0("data$", resp.varName)))
  
  pred  <- eval(parse(text = paste0("data$", pred.varName)))
  #predOG  <- eval(parse(text = paste0("data$", pred.varName)))
  
  kwtobj<- kruskal.test(  resp  ~  pred ) 
  esqd <-rank_eta_squared( resp  ~  pred)$rank_eta_squared
  
  
  obs.teststat<- round( kwtobj$statistic, 3)
  pvalue<- round( kwtobj$p.value, 3)
  dfkw<- round( kwtobj$parameter, 3)
  
 
 
  cat("Kruskal-Wallis test (single-factor ANOVA on ranks)", "\n")

  cat(" \n")
  
  cat("data: ", resp.varName, "~", pred.varName, "\n")
  cat("============== \n")
 
  cat("Kruskal-Wallis test statistic= ", obs.teststat, "\n")
  cat("p-value= ", pvalue, "\n")
  cat("Null distribition is chi-squared with df= ", dfkw, "\n")
  cat("============== \n")
  
  cat(" \n")
  
  cat("Eta-squared= ", esqd, "\n")
  
  
  
  if(PWC){
    # require(broom)
    require(FSA)
    
    samplemeds <- tapply(resp, pred, median)
    
    cat("\n")
    cat("Sample group medians:" , paste(names(samplemeds), "-", samplemeds, " " , sep=""), "\n")
    cat("\n")
    
   # cat("Dunn's test with Holm's correction", "\n")
    
    
    dtresult <- dunnTest(x=resp,g=pred, 
                 method="holm")
    print(dtresult)
    cat("\n")
    
 
    #tidy(pairwise.t.test(x=resp, g=pred, p.adj = "bonf"))
  }
  
  

  
  # returns the p-value, the test stat for each randomization, and the
  # observed test stat
}

  