NOAA <- readr::read_tsv(system.file("extdata/signif.txt", package = "FooR"))
expect_that(NOAA, is_a("data.frame"))
