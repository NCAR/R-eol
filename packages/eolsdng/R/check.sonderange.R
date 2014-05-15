# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsdng" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

check.sonderange <-
function(sonderange=NA, nsonde=0)
{
  msg = "sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE STA
RTING/ENDING NUMBER OF SOUNDINGS TO BE PROCESSED, THE NUMBERS CAN NOT BEYOND TOTAL NUMBER OF SOUNDINGS !!!"
  if (length(sonderange) == 1) {
    if (!is.na(sonderange)) {
      print(msg)
      sonderange = NA
    }
  }
  if (length(sonderange) > 2) {
    print(msg)
    sonderange = NA
  }
  if (length(sonderange) == 2) {
    if (sonderange[1]>sonderange[2] | sonderange[1]<1 | sonderange[2]>nsonde) {
      print(msg)
      sonderange = NA
    }
  }
  return(sonderange)
}
