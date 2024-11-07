
test_that("test leak", {
  N <- 200
  x1 <- rnorm(N)
  x2 <- rbinom(N, size = 1, prob = .2)
  y <- x1^3 - 0.5 * x2 + rnorm(N, 0, 1)
  y <- y * 10
  X <- cbind(x1, x2)
  X <- cbind(X, model.matrix(~ 0 + sample(letters[1:5], N, replace = T)))
  colnames(X) <- paste0("x", 1:ncol(X))
  X_copy <- cbind(X, X, X)
  colnames(X_copy) <- paste0("x", 1:ncol(X_copy))

  fit_gKRLS <- gam(y ~ 0 + s(x1, x2, x3, x4, x5, x6, x7, bs = "gKRLS"), data = data.frame(y,X))
  out <- legacy_marginal_effect(fit_gKRLS)
})