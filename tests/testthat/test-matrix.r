library("pbivnorm")
library("MASS")
context("Specifying x as a matrix")

nobs <- 100
rho <- 0.5
X <- mvrnorm(n = nobs,
             mu = c(0, 0),
             Sigma = matrix(c(1, rho, rho, 1), 2, 2))

test_that("Results are identical whether using vectors or a matrix", {
    expect_identical(pbivnorm(x = X, rho = rho),
                     pbivnorm(x = X[, 1], y = X[, 2], rho = rho))
    expect_identical(pbivnorm(x = X, rho = -rho),
                     pbivnorm(x = X[, 1], y = X[, 2], rho = -rho))
    expect_identical(pbivnorm(x = X, rho = 0),
                     pbivnorm(x = X[, 1], y = X[, 2], rho = 0))
})

