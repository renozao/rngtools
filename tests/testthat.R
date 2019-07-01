library(testthat)
library(rngtools)

test_check("rngtools")

# test that everything works fine when the RNG version is set on backward-compatibility mode
if( utils::compareVersion(paste0(R.version$major, ".", R.version$minor), "3.6.0") >=  0 ){
  RNGversion("3.5.0")
  test_check("rngtools")
  
}
