# Run from the repository root: Rscript tests/review-regressions.R
# Regression checks for the numerical and helper corrections in the book.
source("rfuns/sfaov.R")
source("rfuns/slr.randtest.R")

surv <- read.csv("datasets/dssurv.csv")
surv$Light <- factor(surv$Light)
welch <- oneway.test(Survival ~ Light, surv)
stopifnot(abs(unname(welch$statistic) - 3.23572251) < 1e-7,
          abs(welch$p.value - .100811316) < 1e-7)

strong <- data.frame(y=c(1,2,3,11,12,13), g=factor(rep(c("A","B"),each=3)))
output <- capture.output(sfaov(y~g,strong))
reported <- as.numeric(sub(".*R-squared=\\s*", "", grep("R-squared=",output,value=TRUE)))
expected <- summary(lm(y~g,strong))$r.squared
stopifnot(abs(reported-round(expected,3)) < 1e-10, reported <= 1)

gh90 <- games.howell(surv$Light,surv$Survival,conf.level=.90)
gh95 <- games.howell(surv$Light,surv$Survival,conf.level=.95)
stopifnot(all(gh90$`lower limit` < gh90$`upper limit`),
          all(gh95$`upper limit` > gh90$`upper limit`),
          all(gh95$`lower limit` < gh90$`lower limit`))
expected_upper <- gh90$`Mean Difference` +
  qtukey(.90,nmeans=3,df=gh90$df)*gh90$`Standard Error`
stopifnot(isTRUE(all.equal(gh90$`upper limit`,expected_upper)))
output <- capture.output(sfaov(Survival~Light,surv,PWC=TRUE,welch=TRUE,conf.level=.90))
stopifnot(any(grepl("lwr",output)), any(grepl("upr",output)))

# Same shuffles: a strong negative slope must yield a small lower-tail
# p-value and a large upper-tail value; finite simulation cannot give zero.
d <- data.frame(x=1:12,y=c(12,11,9,10,8,7,5,6,4,3,1,2))
get_p <- function(direction) {
  set.seed(223)
  output <- capture.output(slr.randtest(y~x,d,nshuffles=199,direction=direction))
  as.numeric(sub("p-value =\\s*", "", grep("p-value =",output,value=TRUE)))
}
stopifnot(get_p("less") > 0, get_p("less") < .05, get_p("greater") > .95)

creativity <- read.csv("datasets/extintdata.csv")
fit <- t.test(creativity$Score[creativity$Treatment=="Intrinsic"],
              creativity$Score[creativity$Treatment=="Extrinsic"])
stopifnot(abs(unname(fit$statistic)-2.915292) < 1e-6,
          abs(fit$p.value-.005617534) < 1e-8,
          max(abs(fit$conf.int-c(1.277603,7.010803))) < 1e-6)
source("rfuns/two.mean.test.R")
balanced <- data.frame(y=rep(c(-2,-1,1,2),2),g=factor(rep(c("A","B"),each=4)))
set.seed(223)
output <- capture.output(two.mean.test(y~g,balanced,first.level="A",
                 direction="two.sided",randtest=TRUE,nshuffles=99))
stopifnot(any(grepl("p-value = 1",output,fixed=TRUE)))
cat("Statistical helper regression checks passed.\n")
