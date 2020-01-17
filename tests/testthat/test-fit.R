data(sub_all_crime)
dat1 = cleanData(sub_all_crime)
dat = formatData(dat1, verbose=FALSE)

test_that("clean data",{
  expect_true(nrow(dat1)<=nrow(sub_all_crime))
})

test_that("format data",{
  expect_true(ncol(dat)>ncol(dat1))
})

test_that("glm parameters = lr parameters",{

  lrfit = lr(Arrest ~ poly(dist_from_station, 3) + `Primary Type` + Domestic,
             data = dat)
  glmfit = glm(Arrest ~ poly(dist_from_station, 3) + `Primary Type` + Domestic,
               data = dat, family=binomial(link="logit"))

  l = lrfit$coefficients
  g = glmfit$coefficients

  expect_true(all(round(l, 3) == round(g, 3)))
})

test_that("predict from lr + metrics",{

  lrfit = lr(Arrest ~ poly(dist_from_station, 3) + `Primary Type` + Domestic + `HARDSHIP INDEX`,
             data = dat)
  pprobs = predict(lrfit, type="probs")
  ppreds = predict(lrfit, type="preds")
  pvals = predict(lrfit, type="vals")

  c = cv.lr(lrfit, metric="all", leave_out = nrow(sub_all_crime)/3, seed=2, verbose=FALSE)
  r = roc.lr(lrfit)

  expect_true(all(c(
    (pprobs >= 0 & pprobs <= 1),
    (ppreds == 0 | ppreds == 1),
    length(c==3)
  )))
})
