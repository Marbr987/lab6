context("get_coordinates")

test_that("knapsack rejects errounous input", {
  expect_error(brute_force_knapsack(43.2, 5))
  expect_error(knapsack_dynamic(data.frame(a=1:2, b=1:2), 4))
  expect_error(greedy_knapsack("lksfdj"))
})

test_that("get_coordinates returns correct type", {
  expect_true(length(get_coordinates("Stockholm")) == 2)
  expect_true(typeof(get_coordinates("Lund")) == "double")
})

test_that("get_coordinates returns correct values", {
  expect_equal(get_coordinates("Stockholm"), c(longitude=18.07109, latitude=59.32512))
  expect_equal(get_coordinates("Umea"), c(longitude=20.26307, latitude=63.82566 ))
})