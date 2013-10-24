#
#               Copyright (C) by UCAR
# 
fun.set.mar <- function()
{
    # set plot margins for multiplot figure
    mar <- par("mar")
    mfg <- par("mfg")
    if (mfg[3]==1) # only one row of plots
        par(mar=c(5.1,mar[2],4.1,mar[4]))
    else if (mfg[1]==1) # first row of multirow fig
        par(mar=c(2.1,mar[2],4.1,mar[4]))
    else if (mfg[1]==mfg[3]) # last row of multirow fig
        par(mar=c(5.1,mar[2],1.1,mar[4]))
    else # intermediate row of multirow fig
        par(mar=c(2.1,mar[2],1.1,mar[4]))

    if (mfg[4] > 1) {
        mar <- par("mar")
        mar[4] <- 1
        par(mar=mar)
    }
      
    mfg
}
