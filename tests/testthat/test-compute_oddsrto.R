test_that("check the estimate", {

  est <- oddsrto::compute_oddsrto(.data=ae01, aeterm='aebodsys')

  expect_equal(est$aebodsys[1], 'Cardiovascular')
  expect_equal(est$aebodsys[2], 'Respiratory')

  expect_equal(round(est$estimate[1],1), 2.1)

})


test_that("check the error", {

  ae02 <- NULL
  # debugonce(compute_oddsrto)
  # est1 <- compute_oddsrto(.data=ae01, aeterm='aebodsys')

  expect_error(oddsrto::compute_oddsrto(.data=ae02, aeterm='aebodsys'))

})

test_that("check the aeterms values", {
  # debugonce(compute_oddsrto)
  # est1 <- compute_oddsrto(.data=ae01, aeterm='aebodsys')

  expect_error(oddsrto::compute_oddsrto(.data=ae01, aeterm='aedecod'))
})
