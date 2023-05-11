two.mean.test <- function(formula, data, first.level, direction = c("greater",
    "less", "two-sided"), conflevel = 0.95, randtest = FALSE, nshuffles = NULL,
    returnRandStats = FALSE, printout = TRUE) {
    # Description: Function to compute the null distribution
    # when testing a single mean (theo based) and p-value
    # Arguments: 
    # formula: response ~ explanatory
    # data: Has to be in tidy or data matrix form.  
    # nsims: number of simulations (replications of exp.)  
    # direction: specify the sign in Ha

    first.level <- trimws(first.level, which = c("both"), whitespace = "[ \t\r\n]")
    direction <- trimws(direction, which = c("both"), whitespace = "[ \t\r\n]")

    if (!(direction %in% c("greater", "less", "two-sided")))
        stop("Error: Direction must be either of 'greater',  'less', or 'two-sided' !!!!--- Try again :)")

    resp.varName <- all.vars(formula)[1]
    pred.varName <- all.vars(formula)[2]
    if (is.na(pred.varName) | is.na(resp.varName))
        stop("Wrong formula, should be of the form  y ~  x   ... or your data is not in tidy or data matrix format.")


    eval(parse(text = paste0("data$", pred.varName, " = factor(data$",
        pred.varName, ")")))

    resp <- eval(parse(text = paste0("data$", resp.varName)))

    pred <- eval(parse(text = paste0("data$", pred.varName)))



    n <- dim(data)[1]  # sample size

    tmpidx2 = which(levels(pred) == first.level)  # determine primary last level
    pred <- relevel(pred, levels(pred)[tmpidx2])  # change order depending on first level
    dataOG <- data.frame(resp = resp, pred = pred)


    respord <- dataOG$resp  # eval(parse(text = paste0('dataOR$', resp.varName)))
    preord <- dataOG$pred  #eval(parse(text = paste0('dataOR$', pred.varName)))

    # datax <- c(preord)-1 # 1, 2--> first grp is 0, 2nd grp is
    # is 1
    datax <- as.numeric(as.factor(preord)) - 1

    n1 <- sum(datax == 0)
    n2 <- sum(datax == 1)

    tmpa = mean(c(respord[which(datax == 0)]))
    tmpb = mean(c(respord[which(datax == 1)]))
    tmpasd = sd(c(respord[which(datax == 0)]))
    tmpbsd = sd(c(respord[which(datax == 1)]))

    obs.diff <- tmpa - tmpb  # obs

    tresult <- t.test(resp ~ pred, data = dataOG, alternative = direction,
        conf.level = conflevel, var.equal = TRUE)
    obsTS <- tresult$statistic
    obs.teststat <- obsTS

    first.level <- levels(pred)


    alternative <- direction
    if (direction == "two-sided")
        direction = "two.sided"


    dataOGsim <- dataOG

    if (randtest) {

        # edit fun below to just shuffle data then compute
        # t-test using t.test...
        tmpfun <- function(X, datasim, data, alternative) {
            
            dataOGsim$resp <- sample(data$resp, size = dim(data)[1],
                replace = FALSE)
            tresultrand <- t.test(resp ~ pred, data = dataOGsim,
                alternative = direction, var.equal = TRUE)
            obsTSrand <- tresultrand$statistic
            return(obsTSrand)
        }


        randstats <- lapply(X = 1:nshuffles, FUN = tmpfun, datasim = dataOGsim,
            data = dataOG, alternative = alternative)
        # sort(unlist( randstats) )
        randstats = sort(unlist(randstats))

        aboveidx <- randstats >= obs.teststat
        howmanyAbove <- sum(aboveidx)
        greater.pval <- howmanyAbove/nshuffles  # store a one-sided pvalue

        belowidx <- randstats <= obs.teststat
        howmanyBelow <- sum(belowidx)

        less.pval <- howmanyBelow/nshuffles  # store a one-sided pvalue

        twsd.aboveidx <- randstats >= abs(obs.teststat)
        twsd.belowidx <- randstats <= -abs(obs.teststat)
        howmanyAboveBelow <- sum(twsd.aboveidx) + sum(twsd.belowidx)
        two.sided.pval <- howmanyAboveBelow/nshuffles  # store a one-sided pvalue


        htmp <- hist(randstats, plot = FALSE)
        brks <- htmp$breaks

        br1 <- do.breaks(c(abs(obs.teststat), max(brks)), length(brks)/3)
        br2 <- do.breaks(c(min(brks), -abs(obs.teststat)), length(brks)/3)
        br3 <- do.breaks(c(-abs(obs.teststat), abs(obs.teststat)),
            length(brks)/3)
        brksall <- sort(unique(c(br1, br2, br3)))

        if (direction == "greater") {

            pvalue <- greater.pval
            cat1 <- rep(NA, length(randstats))
            cat1[aboveidx] <- "yes"
            cat1[!aboveidx] <- "no"
            cat2 <- as.factor(cat1)
            cprtmpdf <- data.frame(randstats, cat2)


        }

        if (direction == "less") {

            pvalue <- less.pval
            cat1 <- rep(NA, length(randstats))
            cat1[belowidx] <- "yes"
            cat1[!belowidx] <- "no"
            cat2 <- as.factor(cat1)
            cprtmpdf <- data.frame(randstats, cat2)


        }
        if (direction == "two-sided") {

            pvalue <- two.sided.pval
            cat1 <- rep(NA, length(randstats))
            cat1[twsd.aboveidx] <- "yes"
            cat1[twsd.belowidx] <- "yes"

            cat1[is.na(cat1)] <- "no"
            cat2 <- as.factor(cat1)
            cprtmpdf <- data.frame(randstats, cat2)

        }
        # add labels to the elements in the object 'temp'

        # first.level <- levels(pred)

        hg <- histogram(~randstats, type = "count", ylab = "Number of simulations",
            xlab = "Difference in means", groups = cat2, data = cprtmpdf,
            breakds = brksall, nint = length(brksall))

        cat("     Simulation based two-sample test for independent samples",
            "\n")
        cat("                             ", "\n")


    } else {


        tresultCI <- t.test(resp ~ pred, data = dataOG, alternative = "two.sided",
            conf.level = conflevel, var.equal = TRUE)


        if (direction == "greater")
            pvalue <- pt(tresult$statistic, df = min(n1 - 1, n2 -
                1), lower.tail = FALSE)


        if (direction == "less")
            pvalue <- pt(tresult$statistic, df = min(n1 - 1, n2 -
                1), lower.tail = TRUE)

        if (direction == "two.sided")
            pvalue <- 2 * pt(abs(tresult$statistic), df = min(n1 -
                1, n2 - 1), lower.tail = FALSE)


        MOE <- qt((1 - conflevel)/2, df = min(n1 - 1, n2 - 1), lower.tail = FALSE) *
            tresultCI$stderr
        lb <- obs.diff - MOE
        ub <- obs.diff + MOE

        eval(parse(text = paste0("data$", pred.varName, " = factor(data$",
            pred.varName, ")")))
        pred <- eval(parse(text = paste0("data$", pred.varName)))
        # first.level <- levels(pred)



        n <- dim(data)[1]
        teststat <- tresult$statistic
        if (direction == "greater") {
            hg <- plotDist("t", df = n - 1, kind = "density", xlim = c(-4.5,
                4.5), panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                xx <- c(teststat, x[x >= teststat & x <= 5], 5)
                yy <- c(0, y[x >= teststat & x <= 5], 0)
                panel.polygon(xx, yy, ..., col = "blue")
            })
        }

        if (direction == "less") {

            hg <- plotDist("t", df = n - 1, kind = "density", xlim = c(-4.5,
                4.5), panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                xx <- c(-5, x[x >= -5 & x <= teststat], teststat)
                yy <- c(0, y[x >= -5 & x <= teststat], 0)
                panel.polygon(xx, yy, ..., col = "blue")
            })

        }


        if (direction == "two.sided") {

            hg <- plotDist("t", df = n - 1, kind = "density", xlim = c(-4.5,
                4.5), panel = function(x, y, ...) {
                panel.xyplot(x, y, ...)
                xx <- c(abs(teststat), x[x >= abs(teststat) & x <=
                  5], 5)
                yy <- c(0, y[x >= abs(teststat) & x <= 5], 0)
                panel.polygon(xx, yy, ..., col = "blue")
                xx <- c(-5, x[x >= -5 & x <= -abs(teststat)], -abs(teststat))
                yy <- c(0, y[x >= -5 & x <= -abs(teststat)], 0)
                panel.polygon(xx, yy, ..., col = "blue")
            })

        }

        cat("     Theoretical-based two-sample test for independent samples",
            "\n")
        cat("                             ", "\n")

    }

    pvalue <- round(pvalue, 3)


    if (printout) {

        cat("formula: ", resp.varName, "~", pred.varName, "\n")

        cat("sample mean of ", first.level[1], " group:", tresult[[5]][1],
            "\n")
        cat("sample mean of ", first.level[2], " group:", tresult[[5]][2],
            "\n")
        cat("sample sd of ", first.level[1], " group:", tmpasd, "\n")
        cat("sample sd of ", first.level[2], " group:", tmpbsd, "\n")

        cat("\n")

        cat("difference between groups: (", first.level[1], " group ) - ( ",
            first.level[2], " group )", "\n")
        cat("obs t-test statistic:", obsTS, "            ", "p-value =",
            pvalue, "\n")
        cat("direction:", direction, "\n")
        cat("\n")



        if (!randtest) {
            cat("Confidence level: ", conflevel, "\n")
            cat("CI:( ", lb, ", ", ub, ") \n")
        }
    }


    if (returnRandStats)
        return(randstats)

    if (printout) {
        return(hg)
    } else {
        return(list(pvalue = pvalue))
    }

    # returns the p-value, the test stat for each
    # randomization, and the observed test stat




}
