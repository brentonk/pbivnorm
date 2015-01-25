library("pbivnorm")
library("mnormt")
library("MASS")
context("Same results as mnormt::pmnorm()")

set.seed(2529)

pbiv_via_pmnorm <- function(x, rho)
{
    apply(x, 1, pmnorm, varcov = matrix(c(1, rho, rho, 1), 2, 2))
}

nobs <- 100
rho <- 0.5
X <- mvrnorm(n = nobs,
             mu = c(0, 0),
             Sigma = matrix(c(1, rho, rho, 1), 2, 2))

test_that("Results are equal between pbivnorm() and pmnorm()", {
    expect_equal(pbivnorm(x = X, rho = rho),
                 pbiv_via_pmnorm(x = X, rho = rho))
    expect_equal(pbivnorm(x = X, rho = -rho),
                 pbiv_via_pmnorm(x = X, rho = -rho))
    expect_equal(pbivnorm(x = X, rho = 0),
                 pbiv_via_pmnorm(x = X, rho = 0))
})
