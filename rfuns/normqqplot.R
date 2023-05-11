normqqplot <- function( formula , data=NULL , ylab="Sample quantiles", main=NULL){

if(length(all.vars(formula))==2){  
varName <-  all.vars(formula)[1]
gvarName <-  all.vars(formula)[2]

if(!is.null(data)){ 
  resp  <- eval(parse(text = paste0("data$", varName)))
  gvar  <- eval(parse(text = paste0("data$", gvarName)))
 
  }else{
 resp  <- eval(parse(text = paste0(varName)))
 gvar  <- eval(parse(text = paste0(gvarName)))
 
  }


qqmath( ~ resp | gvar, ylab=ylab, main=main, distribution=qnorm,
        xlab="Normal quantiles", data,
        panel = function(...) {
          panel.qqmathline(...)
          panel.qqmath(...)
        })
}else{
  
  gvar=NULL
  varName <-  all.vars(formula) 
  
  if(!is.null(data)){ 
    resp  <- eval(parse(text = paste0("data$", varName)))

  }else{
    resp  <- eval(parse(text = paste0(varName)))
  }


qqmath( ~ resp  , ylab=ylab, main=main, distribution=qnorm,
					   xlab="Normal quantiles", data,
					   panel = function(...) {
			panel.qqmathline(...)
			panel.qqmath(...)
		   })
}
}


