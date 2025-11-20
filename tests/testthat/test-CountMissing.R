test_that("Output matches direct call to dplyr", {
  small_tbl <- tribble(~group, ~var1, ~var2,
                       "A", 1, NA,
                       "A", 2, "x",
                       "B", NA, "y",
                       "C", 3, "z")

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = "drop"),
                count_all_missing_by_group(small_tbl, group)
  )

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = NULL),
                count_all_missing_by_group(small_tbl, group, NULL)
  )
})

test_that("Checking error handling for .groups input", {
  expect_error(
    count_all_missing_by_group(airquality, Month, "kep")
  )
  expect_no_error(
    count_all_missing_by_group(airquality, Month, NULL)
  )
})

test_that("Test empty cases correctly", {
  small_tbl <- tibble(group=character(), var1=numeric())

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = "drop"),
                count_all_missing_by_group(small_tbl, group)
  )

})

test_that("Use keep for .group", {
  small_tbl <- tribble(~group, ~var1, ~var2,
                       "A", 1, NA,
                       "A", 2, "x",
                       "B", NA, "y",
                       "C", 3, "z")

  expect_equal( small_tbl |> group_by(group) |>
                  summarize(across(everything(), ~sum(is.na(.x))),
                            .groups = "keep"),
                count_all_missing_by_group(small_tbl, group, "keep")
  )

})

