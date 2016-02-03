pagepng <- function(filename)
{
    png(filename=filename,width=10.3,height=8,units="in",res=300)
}

project_plots <- function(dataDir=Sys.getenv("SONDE_DATA"),plotDir=file.path(dataDir,"postflight"),
    dlim=list(RH=c(-5,105),T=c(-60,30),Vz=c(-50,0),Wspd=c(0,50)))
{
    # these defaults should match the default argument above.
    dlimx <- list(RH=c(-5,105),T=c(-60,30),Vz=c(-50,0),Wspd=c(0,50))

    dlim <- lapply(names(dlimx),function(n) {
        if (is.null(dlim[[n]])) dlimx[[n]]
        else dlim[[n]]
    })
    names(dlim) <- names(dlimx)

    # AVAPS users don't want project dependent code in an .RData, so
    # things like this have to go in this package.
    dpar(platform="AVAPS")
    dpar(start="2008 1 1 00:00",end="2019 1 1 00:00")

    if (!is.null(plotDir) && !file.exists(plotDir)) dir.create(plotDir)
    else options(device.ask.default=TRUE)

    # set plot suffix to trailing portion of dataDir
    plotSuffix <- dataDir
    psep <- unlist(gregexpr(.Platform$file.sep,dataDir,fixed=TRUE))
    while (length(psep) > 0 && tail(psep,1) == nchar(dataDir)) {
        dataDir <- substring(dataDir,1,nchar(dataDir)-1)
        psep <- psep[-length(psep)]
    }
    psep <- tail(psep,1)

    if (length(psep) == 0) psep <- 0
    plotSuffix <- substring(dataDir,psep+1)

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
            if (!is.null(plotDir))
                pagepng(filename=file.path(plotDir,paste0(var,"_",type,"_",plotSuffix,".png")))

            dl <- dlim[[var]]
            if (!is.null(dl)) {
                if (is.na(dl[1])) dl[1] <- -Inf
                if (is.na(dl[2])) dl[2] <- Inf
                clip(var,TRUE,dl[1],dl[2])
            }
            else
                clip(var,F)
            units <- units(xs[[1]][,var])
            scontour(xs,"P",var,contour=(type=="contour"))
            if (!is.null(plotDir)) dev.off()
        }
    }

    if (FALSE) {

        # plot layout on page, nr X nc
        nr <- 1L
        if (ns < 3) nr <- 1L
        nc <- 2L
        # if (ns > 4) nc <- 4L
        np <- 0

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
