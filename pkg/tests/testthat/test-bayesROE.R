context("bayesROE")

## function to compuote posterior probability that effect size more extreme
## than delta
postP <- function(ee, se, mu, g, delta) {
    postVar <- g/(1 + g)*se^2
    postMean <- g/(1 + g)*ee + 1/(1 + g)*mu
    p <- stats::pnorm(q = delta, mean = postMean, sd = sqrt(postVar),
                      lower.tail = ifelse(sign(ee) == 1, FALSE, TRUE))
    return(p)
}

test_that("recomputing posterior probability for positive effect estimate", {
    ee <- 2
    se <- 0.5
    delta <- c(0, 2)
    alpha <- 0.025
    nGrid <- 500
    dat <- bayesROE(ee = ee, se = se, delta = delta, alpha = alpha,
                    sdLim = c(0.001, 2*se),
                    nGrid = 500)$data
    p <- postP(ee = ee, se = se, mu = dat$mu, g = dat$sePrior^2/se^2,
               delta = dat$delta)
    expect_equal(object = p,
                 expected = rep(x = 1 - alpha, times = length(delta)*nGrid))
})

test_that("recomputing posterior probability for negative effect estimate", {
    ee <- -2
    se <- 0.5
    delta <- -c(0, 2)
    alpha <- 0.025
    nGrid <- 500
    dat <- bayesROE(ee = ee, se = se, delta = delta, alpha = alpha,
                    sdLim = c(0.001, 2*se),
                    nGrid = 500)$data
    p <- postP(ee = ee, se = se, mu = dat$mu, g = dat$sePrior^2/se^2,
               delta = dat$delta)
    expect_equal(object = p,
                 expected = rep(x = 1 - alpha, times = length(delta)*nGrid))
})
