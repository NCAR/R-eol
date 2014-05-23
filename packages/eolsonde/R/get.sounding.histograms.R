# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:
#
# 2013,2014, Copyright University Corporation for Atmospheric Research
# 
# This file is part of the "eolsonde" package for the R software environment.
# The license and distribution terms for this file may be found in the
# file LICENSE in this package.

get.sounding.histograms <- function (fdir=NULL,projsonde=NULL,
    Thist=TRUE,RHhist=TRUE,Phist=TRUE,
    WSPDhist=TRUE,WDIRhist=TRUE) 
{
    flist = check.files(fdir)
    print(paste("PLEASE MAKE SURE ONLY QCed SOUNDING DATA UNDER", fdir, "!!!"))
    M = length(flist)
    pres = temp = rhum = wspd = wdir = NA
    for (i in 1:M) {
        print(flist[i])
        f = paste(fdir, flist[i], sep = "/")
        qc.sounding = readin.file("qc",f)
        time = qc.sounding[,"time"]
        ps = qc.sounding[,"P"]
        t  = qc.sounding[,"T"]
        rh = qc.sounding[,"RH"]
        w  = qc.sounding[,"Wspd"]
        wd = qc.sounding[,"Wdir"]

        maxp = max(ps,na.rm=TRUE)
        if(maxp > 1015)	print(maxp)

        pres = append(pres,ps)
        temp = append(temp,t)
        rhum = append(rhum,rh)
        wspd = append(wspd,w)
        wdir = append(wdir,wd)
    }

    ind = !is.na(pres)
    pres = pres[ind]
    temp = temp[ind]
    rhum = rhum[ind]
    wspd = wspd[ind]
    wdir = wdir[ind]

    count = 0
    for (j in 1:5) {
        if (j == 1) {
            if (!Thist)	next
            count = count + 1
        }
        if (j == 2) {
            if (!RHhist)	next
            count = count + 1
        }
        if (j == 3) {
            if (!Phist)	next
            count = count + 1
        }
        if (j == 4) {
            if (!WSPDhist)	next
            count = count + 1
        }
        if (j == 5) {
            if (!WDIRhist)	next
            count = count + 1
        }
    }
    if (count == 0)   stop("NO VARIABLE HAS BEEN CHOSEN TO PLOT !!!")

    system("rm -f histograms.ps")
    postscript(file="histograms.ps", paper="letter", horizontal=TRUE)
    xlabs = c("Temperature (C)", "RH (%)", "PRESSURE (mb)", "Wind Speed (m/s)", "Wind Direction (deg)")
    ylabs = "Probability"
    par(mfrow = c(count, 1), mar = c(5,5,2,1))
    par(xaxs = "i", yaxs = "i", tck = -0.1, lwd = 2)
    for (j in 1:5) {
        if (j == 1) {
            if (!Thist)	next
            data = temp
        }
        if (j == 2) {
            if (!RHhist)	next
            data = rhum
        }
        if (j == 3) {
            if (!Phist)	next
            data = pres
        }
        if (j == 4) {
            if (!WSPDhist)	next
            data = wspd
        }
        if (j == 5) {
            if (!WDIRhist)	next
            data = wdir
        }
        mtitle = paste(projsonde, " (Min.=", round(min(data,na.rm=TRUE),2), " Max.=", round(max(data,na.rm=TRUE),2), ")", sep = "")
        hist(data, xlab=xlabs[j], ylab=ylabs, main=mtitle, probability=TRUE, col="gray", lwd=2)
        box(lwd=1)
    }
    dev.off()
}

