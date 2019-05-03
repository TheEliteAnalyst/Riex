context("check parameters value")
test_that("input values selected for period are valid", {
  prints_text(iex.chart("GM", "2m"), c("Stock Info is available in IEX", "Value incorrect. Select from valid values: 5y, 2y, 1y, ytd, 6m, 3m, 1m, 1d" ))

})
test_that("input values selected for stock symbol are valid", {
  prints_text(iex.chart("gm", "1d"), c("Stock Info is available in IEX", "Range selected is :1d" ))

})




