# Copyright: Renaud Gaujoux (2009-2021)
# 
# This file is part of the rngtools package for R. 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
###################################################################

library(testthat)
library(rngtools)

test_check("rngtools")

# test that everything works fine when the RNG version is set on backward-compatibility mode
if( utils::compareVersion(paste0(R.version$major, ".", R.version$minor), "3.6.0") >=  0 ){
  RNGversion("3.5.0")
  test_check("rngtools")
  
}
