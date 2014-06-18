pagepng <- function(filename)
{
    png(filename=filename,width=10.3,height=8,units="in",res=300)
}

project_plots <- function(dataDir=Sys.getenv("SONDE_DATA"),plotDir=file.path(dataDir,"postflight"))
{
    # AVAPS users don't want project dependent code in an .RData, so
    # things like this have to go in this package.
    dpar(platform="AVAPS")
    Sys.setenv(PROJECT="")
    dpar(start="2008 1 1 00:00",end="2019 1 1 00:00")

    if (!file.exists(plotDir)) dir.create(plotDir)

    # set plot suffix to trailing portion of dataDir
    plotSuffix <- dataDir
    psep <- unlist(gregexpr(.Platform$file.sep,dataDir,fixed=TRUE))
    if (tail(psep,1) == nchar(dataDir)) psep <- tail(psep,2)[1]
    else psep <- tail(psep,1)
    if (!is.null(psep)) plotSuffix <- substring(dataDir,psep+1)

    xs <- readSoundings(dir=dataDir,file="D%Y%m%d_%H%M%S_P\\.[0-9]+")
    ns <- length(xs)

    if (ns == 0)
        stop(paste("No data found between",format(dpar("start")),"and",format(dpar("end"))))

    # t1 <- positions(xs[[1]])[1]
    # plotSuffix <- format(t1,format="%Y%m%d_%H%M")

    vars <- c("T","RH","Wspd","Vz")
    col = c("black", "red", "blue", "green", "purple")

    # Make level and contour plots
    for (type in c("level")) {
        for (var in vars) {
            pagepng(filename=file.path(plotDir,paste0(var,"_",type,"_",plotSuffix,".png")))

            units <- units(xs[[1]][,var])
            scontour(xs,"P",var,contour=(type=="contour"))
            dev.off()
        }
    }

    if (FALSE) {

        # plot layout on page, nr X nc
        nr <- 1L
        if (ns < 3) nr <- 1L
        nc <- 2L
        # if (ns > 4) nc <- 4L
        np <- 0

        xlim <- list(RH=c(0,100),T=c(-80,30),Vz=c(-60,60),Wspd=c(-60,60))

        for (sname in names(xs)) {
            if (np %% (nc*nr) == 0) {
                if (np > 0) {
                    mtext(paste(plotSuffix,"Profiles"),outer=TRUE,side=3,cex=1.2)
                    dev.off()
                }
                pagepng(filename=file.path(plotDir,paste0("profiles_",np,"_",plotSuffix,".png")))
                par(mfrow=c(nr,nc))
            }
            sprofile(xs[[sname]][,c(vars,"P")],title=sname,col=col,xlim=xlim)
            np <- np + 1
        }
        mtext(paste(plotSuffix,"Profiles"),outer=TRUE,side=3,cex=1.2)
        dev.off()
    }
    NULL
}
