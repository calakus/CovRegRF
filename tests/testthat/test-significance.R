## Load data
data(data)
xvar.names <- colnames(data$X)
yvar.names <- colnames(data$Y)
data <- data.frame(data$X, data$Y)

formula <- as.formula(paste(paste(yvar.names, collapse="+"), ".", sep=" ~ "))

test_that("significance",{
  sig1 <- significance.test(formula,
                            data[, c(xvar.names[1:2], yvar.names)],
                            params.rfsrc = list(ntree = 50),
                            nperm = 20,
                            test.vars = NULL)
  expect_gte(sig1$pvalue,0)
  expect_lte(sig1$pvalue,1)

  sig2 <- significance.test(formula,
                            data,
                            params.rfsrc = list(ntree = 50),
                            nperm = 20,
                            test.vars = xvar.names[3])
  expect_gte(sig2$pvalue,0)
  expect_lte(sig2$pvalue,1)
})
