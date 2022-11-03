.libPaths("C:/R/win-library/")
install.packages("stringr")
install.packages("stringi")
install.packages("rex")
install.packages("rematch2")
library(rematch2)
library(devtools)
create_package("C:/MGrid")
usethis::use_git()

(x <- "alfa,bravo,charlie,delta")
strsplit(x, split = ",")
str(strsplit(x, split = ","))
unlist(strsplit(x, split = ","))
strsplit(x, split = ",")[[1]]

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

usethis::use_r("strsplit1")

load_all()
strsplit1(x, split = ",")
exists("strsplit1", where = globalenv(), inherits = FALSE)

library(Mpackage)

check()
document()
use_mit_license()
writeLines(readLines("LICENSE"))
install()


use_testthat()
use_test("strsplit1")
test()

use_package("stringr")

gitcreds::gitcreds_set()
gitcreds::gitcreds_get()
usethis::use_git()
usethis::use_github()
