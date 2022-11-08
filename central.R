.libPaths("C:/R/win-library/")
install.packages("stringr")
install.packages("stringi")
install.packages("rex")
install.packages("rematch2")
library(rematch2)
library(devtools)

# 2.5
create_package("C:/Mpackage")

# 2.6
usethis::use_git()

#2.7
(x <- "alfa,bravo,charlie,delta")
strsplit(x, split = ",")
str(strsplit(x, split = ","))
unlist(strsplit(x, split = ","))
strsplit(x, split = ",")[[1]]

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

#2.8
usethis::use_r("strsplit1")
usethis::use_r("Allocation")
usethis::use_r("CorrectLUtotal")
usethis::use_r("CreateBaseline")
usethis::use_r("CreateExoMap")
usethis::use_r("CreateSuitmap")
usethis::use_r("ReprojShift")

#2.8
load_all()
strsplit1(x, split = ",")
exists("CreateBaseline", where = globalenv(), inherits = FALSE)

library(Mpackage)
# 2.9 commit all changes from now on

#2.10
check()

#2.12
use_mit_license()
writeLines(readLines("LICENSE"))

#2.13
document()

#2.14
check()

#2.15
install()

#2.16
use_testthat()
use_test("CreateBaseline")
test()

#2.17
use_package("raster", "terra", "sp")

#2.18
#gitcreds::gitcreds_set()
#gitcreds::gitcreds_get()
usethis::use_git()
usethis::use_github()


