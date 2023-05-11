fviz_eig.psych <- function (X, choice = c("variance", "eigenvalue"), geom = c("bar",  "line"), barfill = "steelblue", barcolor = "steelblue", 
          linecolor = "black", ncp = 10, addlabels = FALSE, hjust = 0, 
          main = NULL, xlab = NULL, ylab = NULL, ggtheme = theme_minimal(), 
          ...) 
{
  
  require(ggplot2)
  X$Vaccounted[2:3,] <- X$Vaccounted[2:3,]*100
  eig <- t(X$Vaccounted[1:3,])
  eig <- eig[1:min(ncp, nrow(eig)), , drop = FALSE]
  choice <- choice[1]
  if (choice == "eigenvalue") {
    eig <- eig[, 1]
    text_labels <- round(eig, 1)
    if (is.null(ylab)) 
      ylab <- "Eigenvalue"
  }
  else if (choice == "variance") {
    eig <- eig[, 2]
    text_labels <- paste0(round(eig, 1), "%")
  }
  else stop("Allowed values for the argument choice are : 'variance' or 'eigenvalue'")
  if (length(intersect(geom, c("bar", "line"))) == 0) 
    stop("The specified value(s) for the argument geom are not allowed ")
  df.eig <- data.frame(dim = factor(1:length(eig)), eig = eig)
  extra_args <- list(...)
  bar_width <- extra_args$bar_width
  linetype <- extra_args$linetype
  if (is.null(linetype)) 
    linetype <- "solid"
  p <- ggplot(df.eig, aes(dim, eig, group = 1))
  if ("bar" %in% geom) 
    p <- p + geom_bar(stat = "identity", fill = barfill, 
                      color = barcolor, width = bar_width)
  if ("line" %in% geom) 
    p <- p + geom_line(color = linecolor, linetype = linetype) + 
    geom_point(shape = 19, color = linecolor)
  if (addlabels) 
    p <- p + geom_text(label = text_labels, vjust = -0.4, 
                       hjust = hjust)
  if (is.null(main)) 
    main <- "Scree plot"
  if (is.null(xlab)) 
    xlab <- "Dimensions"
  if (is.null(ylab)) 
    ylab <- "Percentage of explained variances"
  p <- p + labs(title = main, x = xlab, y = ylab)
  ggpubr::ggpar(p, ggtheme = ggtheme, ...)
}
