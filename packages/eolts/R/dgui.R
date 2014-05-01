# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:

#
#               Copyright (C) by UCAR
# 
# Graphical user interface for dpar(), reading data from netcdf, and plotting.

# TODO:
#   further break up table of variables:
#       met, eddy, soil, rad, power, other
#       derived met, derived eddy, derived flux, etc
#   dpar("robust")
#   dataset menu is not wide enough. show dataset description
#   add option to show value at cursor
#   support for par mfrow. Set plot index to 1 after time change
#   support type="l", "b", "p"
#   after selecting dataset, indicate start and end times of the dataset

#   button for new x11() plot
#   more general support for par parameters in a text field:  "mar=x"
#   provide widget to enter smoothing value?
#   x vs y plots?
#   difference plots?
#   ability to set dpar start/end from zoom

.this <- new.env(parent=emptyenv())

thisExists <- function(name) 
{
    exists(name,envir=.this)
}

thisGet <- function(name) 
{
    get(name,envir=.this)
}

thisSet <- function(name,val) 
{
    assign(name,val,envir=.this)
}

# alternative functions for getting/setting objects in the
# enclosing environment, which is the namespace.
# To assign an object, have to use unlockBinding()
# Seems like its not a good idea to mess with the namespace.
# Leaving these functions here for reference.
thatExists <- function(name) 
{
    parenv <- parent.env(environment(NULL))
    exists(name,envir=parenv)
}

thatGet <- function(name) 
{
    parenv <- parent.env(environment(NULL))
    get(name,envir=parenv)
}

thatSet <- function(name,val) 
{
    parenv <- parent.env(environment(NULL))
    lb <- bindingIsLocked(name,parenv)
    if (lb) unlockBinding(name,parenv)
    assign(name,val,envir=parenv)
    if (lb) lockBinding(name,parenv)
}
# mytest <- 2
# thatSet("mytest",0)

# The date selector in the calendar widget can only handle this time format.
# "%Y %m %d" doesn't work.
dfmt <- "%Y-%m-%d"
infmt <- "%Y-%m-%d%H%M%OS"

thisSet("mainWidget",NULL)
thisSet("selectedVars",NULL)
thisSet("allVariables",NULL)
thisSet("allHeights",NULL)
thisSet("allStations",NULL)
thisSet("allSites",NULL)
# thisSet("var1MomLayout",NULL)
# thisSet("var2MomLayout",NULL)
# thisSet("var_group",NULL)

# save reference because we may need to change the size
thisSet("datasetCombo",NULL)
# save reference because we may need to change the size
thisSet("tzradio",NULL)

thisSet("outVarName",NULL)
thisSet(".tmpData",NULL)

thisSet("timeLengthType","")
thisSet("startDate","")
thisSet("startTime","")
thisSet("endDate","")
thisSet("endTime","")

thisSet("heightsCheckBoxWidget",NULL)
thisSet("variablesNotebookWidget",NULL)

thisSet("zoomTimes",pairlist())
thisSet("zoomWidgets",NULL)

thisSet("readDataWidget",NULL)
thisSet("readAndPlotDataWidget",NULL)
    
formatHMS <- function(tx)
{
    c(format(tx,format="%H"),format(tx,format="%M"),format(tx,format="%OS"))
}

setTimePeriodLength <- function()
{
    dt <- "second"
    dv <- dpar("lensec")

    if ((dv %% (86400L/2L)) == 0) {
        dt <- "day"
        dv <- dv / 86400
    }
    else if ((dv %% 3600) == 0) {
        dt <- "hour"
        dv <- dv / 3600
    }
    else if ((dv %% 60) == 0) {
        dt <- "minute"
        dv <- dv / 60
    }
    thisSet("timeLengthType",dt)
    thisSet("timeLengthValue",dv)
}

toggleVariableHandler <- function(h,...)
{
    # cat("var=",svalue(h$obj)," selected\n")
    sv <- thisGet("selectedVars")
    if (svalue(h$obj)) {
        # cat("var=",h$action," selected\n")
        sv <- c(sv,h$action)
    }
    else {
        # cat("var=",h$action," unselected\n")
        mx <- match(h$action,sv)
        if (!is.na(mx)) sv <- sv[-mx]
    }
    thisSet("selectedVars",sv)

    readEnable <- length(sv) > 0

    widget <- thisGet("readDataWidget")
    enabled(widget) <- readEnable
    widget <- thisGet("readAndPlotDataWidget")
    enabled(widget) <- readEnable

    NULL
}

disableZoom <- function()
{
    for (button in thisGet("zoomButtons")) enabled(button) <- FALSE
    thisSet("zoomTimes",pairlist())
}

enableZoom <- function()
{
    for (button in thisGet("zoomButtons")) enabled(button) <- TRUE
}

checkVariables <- function()
{
    notebook <- thisGet("variablesNotebookWidget")
    if (is.null(notebook)) return(NULL)

    allvars <- variables()
    if (length(allvars) > 0) allvars <- sort(allvars)

    if (identical(allvars,thisGet("allVariables"))) return(NULL)
    thisSet("allVariables",allvars)

    if (length(allvars) > 0) {
        hts <- sort(unique(heights(allvars)))
        hts <- hts[hts > 0.1]

        if (!identical(hts,thisGet("allHeights"))) {
            thisSet("allHeights",hts)

            chts <- sapply(hts,format)

            if (thisGet("debug")) cat("heights=",paste(chts,collapse=","),"\n")
            cb <- thisGet("heightsCheckBoxWidget")
            cb[] <- c("clear","all",chts)
        }

        lenfile <- dpar("lenfile")
        if (is.null(lenfile)) 
            iod <- netcdf()
        else iod <- netcdf(lenfile = lenfile)
        stns <- sort(unique(stations(iod)))
        close(iod)
        if (!identical(stns,thisGet("allStations"))) {
            thisSet("allStations",stns)

            if (thisGet("debug")) cat("allStations=",paste(stns,collapse=","),"\n")
            cb <- thisGet("stationsCheckBoxWidget")
            cb[] <- c("clear","all",sapply(stns,format))
        }

        sites <- sort(unique(sites(allvars)))
        if (!identical(sites,thisGet("allSites"))) {
            thisSet("allSites",sites)
            cb <- thisGet("sitesCheckBoxWidget")
            cb[] <- c("clear","all",sites)
        }

        w1vars <- sort(unique(words(allvars,1,1,sep=".")))

        cnts <- words(w1vars,1,1,sep="_") == "counts"
        if (any(cnts)) w1vars <- w1vars[!cnts]
        if (thisGet("debug")) cat(paste(w1vars,collapse=","),"\n")

        selected <- thisGet("selectedVars")
        # remove from selected variables any that are not in w1vars
        mx <- is.na(match(selected,w1vars))
        if (any(mx)) {
            selected <- selected[!mx]
            thisSet("selectedVars",selected)
        }

        high_moments <- grepl("'",w1vars,fixed=TRUE)

        # delete any existing tab pages
        if (length(notebook) > 0) {
            for (i in 1:length(notebook)) {
                svalue(notebook) <- i
                dispose(notebook)
            }
        }
        # cat("disposed\n")
        # g1 <- ggroup(container=notebook, horizontal=FALSE,label="variables")
        # var_group <- ggroup(horizontal=TRUE,use.scrollwindow=FALSE,container=notebook,label="variables")
        layout <- glayout(container=notebook, homogeneous=TRUE,
            label="first moments",spacing=0)
        nc <- 10
        i <- 0
        for (var in w1vars[!high_moments]) {
            ic <- (i %% nc) + 1
            ir <- (i %/% nc) + 1
            cb <- gcheckbox(text=var,action=var,checked=(var%in%selected),
                container=layout,handler=toggleVariableHandler)
            layout[ir,ic] <- cb
            if (FALSE && i == 0) {
                sz <- size(cb)
                cat("cb size =",paste(sz,collapse=","),"\n")
            }
            i <- i + 1
        }
        thisSet("var1MomLayout",layout)
        # cat("done\n")

        if (any(high_moments)) {
            layout <- glayout(container=notebook, homogeneous=TRUE,
                label="higher moments",spacing=0)
            nc <- 10
            i <- 0
            for (var in w1vars[high_moments]) {
                ic <- (i %% nc) + 1
                ir <- (i %/% nc) + 1
                cb <- gcheckbox(text=var,action=var, checked=(var%in%selected),
                    container=layout,handler=toggleVariableHandler)
                layout[ir,ic] <- cb
                if (FALSE && i == 0) {
                    sz <- size(cb)
                    cat("cb size =",paste(sz,collapse=","),"\n")
                }
                i <- i + 1
            }
            svalue(notebook) <- 1
        }
        thisSet("var2MomLayout",layout)

        if (FALSE) {
            sz <- size(layout)
            cat("layout size =",paste(sz,collapse=","),"\n")
            sz[2] <- as.integer(max(length(w1vars)/5,5) * sz[1])

            cat("setting layout size to",paste(sz,collapse=","),"\n")
            size(layout) <- c(1,50)

            sz <- size(var_group)
            cat("group size =",paste(sz,collapse=","),"\n")
            sz[2] <- as.integer(max(length(w1vars)/5,5) * sz[1])

            cat("setting group size to",paste(sz,collapse=","),"\n")
            size(var_group) <- c(1,50)
        }
        else if (FALSE) {
            sz <- size(layout)
            cat("layout size =",paste(sz,collapse=","),"\n")
            # size(layout) <- c(800,600)
        }
    }
    NULL
}

dgui <- function(visible=TRUE,debug=FALSE)
{
    if (FALSE) {
        cat("mytest exists=",thatExists("mytest"),"\n")
        cat("mytest=",thatGet("mytest"),"\n")
        thatSet("mytest",20)
        cat("mytest=",thatGet("mytest"),"\n")
    }

    thisSet("debug",debug)

    mainWidget <- thisGet("mainWidget")
    if (!is.null(mainWidget)) {
        visible(mainWidget) <- visible

        if (guiToolkit()@toolkit != "tcltk" && visible) {
            # now that the window is visible with known widget sizes,
            # enlarge gcombobox widgets
            ltsz <- tag(mainWidget,"ltsz")
            if (debug) cat("ltsz=",paste(ltsz,collapse=","),"\n")
            if (is.null(ltsz)) {
                ltw <- tag(mainWidget,"timeLengthTypeWidget")
                ltsz <- size(ltw)
                ltsz[1] <- ltsz[1] * 2L
                size(ltw) <- ltsz
                tag(mainWidget,"ltsz") <- ltsz

                sz <- size(tag(mainWidget,"startHourWidget"))
                size(tag(mainWidget,"startMinuteWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
                size(tag(mainWidget,"startSecWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
                size(tag(mainWidget,"endMinuteWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
                size(tag(mainWidget,"endSecWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))

                dc <- thisGet("datasetCombo")
                size(dc) <- c(100,25)
                tz <- thisGet("tzradio")
                size(tz) <- c(150,25)
            }
        }
        return(invisible(mainWidget))
    }

    t1 <- dpar("start")
    thisSet("startDate",format(t1,format=dfmt))
    thisSet("startTime",formatHMS(t1))

    t2 <- dpar("end")
    thisSet("endDate",format(t2,format=dfmt))
    thisSet("endTime",formatHMS(t2))

    setTimePeriodLength()

    # primary setting is the start time
    # changing start time results in new end time, using length
    # changing length results in new end time
    # changing end time results in new length
    # "previous", "next" buttons shift both start and end time.

    mainWidget <- gwindow("dgui", visible=FALSE)
    # g <- ggroup(container=w, horizontal=TRUE)
    mainContainer <- ggroup(container=mainWidget, horizontal=FALSE)

    if (exists(".datasets") && length(.datasets) > 0) {
        datasetHandler <- function(h,...) 
        {
            dset <- svalue(h$obj)
            dataset(dset)
            svalue(ncd) <- Sys.getenv("NETCDF_DIR")
            svalue(ncf) <- Sys.getenv("NETCDF_FILE")
            thisSet("allVariables",NULL)
            checkVariables()
            disableZoom()
            NULL
        }

        g1 <- gframe("Dataset",container=mainContainer, horizontal=TRUE)

        # enabled datasets
        mx <- sapply(.datasets,function(x){x$enable})
        dnames <- names(.datasets[mx])
        cur <- dataset()
        sel <- match(cur,dnames)
        if (is.na(sel)) sel <- 1
        combo <- gcombobox(dnames,container=g1,handler=datasetHandler,
            selected=sel, size=c(100,25))
        thisSet("datasetCombo",combo)
    }

    netcdfDirHandler <- function(h,...)
    {
        val <- svalue(h$obj)
        if (length(val) > 0) {
            Sys.setenv(NETCDF_DIR=val)
            clear.cache()
        }
        NULL
    }


    g1 <- gframe("NETCDF_DIR",container=mainContainer, horizontal=TRUE)
    ncd <- gedit(Sys.getenv("NETCDF_DIR"),container=g1)
    addHandlerChanged(ncd,netcdfDirHandler)
    if (guiToolkit()@toolkit != "tcltk") {
	if (debug) cat("changing size on NETCDF_DIR widget\n")
	size(ncd) <- c(400,25)
	if (debug) cat("changed size on NETCDF_DIR widget\n")
    }

    dirButtonHandler <- function(h,...)
    {
        dir <- gfile("select NETCDF_DIR",type="selectdir",
            initial.dir=Sys.getenv("NETCDF_DIR"))
        if (length(dir) > 0) {
            Sys.setenv(NETCDF_DIR=dir)
            svalue(h$action) <- dir
        }
        NULL
    }

    gbutton("Browse ...",container=g1,handler=dirButtonHandler,action=ncd)

    netcdfFileHandler <- function(h,...)
    {
        val <- svalue(h$obj)
        if (length(val) > 0) {
            Sys.setenv(NETCDF_FILE=val)
            clear.cache()
        }
        NULL
    }

    g1 <- gframe("NETCDF_FILE",container=mainContainer, horizontal=TRUE)
    ncf <- gedit(Sys.getenv("NETCDF_FILE"),container=g1)
    addHandlerChanged(ncf,netcdfFileHandler)
    if (guiToolkit()@toolkit != "tcltk") {
	if (debug) cat("changing size on NETCDF_FILE widget\n")
	size(ncf) <- c(400,25)
	if (debug) cat("changed size on NETCDF_FILE widget\n")
    }

    fileButtonHandler <- function(h,...)
    {
        file <- gfile("select NETCDF_FILE",type="save",
            initial.file=Sys.getenv("NETCDF_FILE"))
        if (length(file) > 0) {
            Sys.setenv(NETCDF_FILE=file)
            svalue(h$action) <- file
        }
        NULL
    }
    gbutton("Browse ...",container=g1,handler=fileButtonHandler,action=ncd)

    startDateHandler <- function(h,...)
    {
        # browser()
        if (debug) cat("startDateHandler, svalue(h$obj)=",svalue(h$obj,drop=FALSE),"\n")
        startDate <- svalue(h$obj,drop=TRUE)
        t1 <- utime(paste(startDate,paste(thisGet("startTime"),collapse=" ")),in.format=infmt)
        dpar(start=t1)
        thisSet("startDate",startDate)

        # adjust end time widgets to new end time
        t2 <- dpar("end")
        endDate <- format(t2,format=dfmt)
        endTime <- formatHMS(t2)

        thisSet("endDate",endDate)
        thisSet("endTime",endTime)

        blockHandlers(endDateWidget)
        blockHandlers(endHourWidget)
        blockHandlers(endMinuteWidget)
        blockHandlers(endSecWidget)

        svalue(endDateWidget) <- endDate
        svalue(endHourWidget) <- endTime[1]
        svalue(endMinuteWidget) <- endTime[2]
        svalue(endSecWidget) <- endTime[3]

        unblockHandlers(endDateWidget)
        unblockHandlers(endHourWidget)
        unblockHandlers(endMinuteWidget)
        unblockHandlers(endSecWidget)

        checkVariables()
        disableZoom()
        NULL
    }

    startTimeHandler <- function(h,...)
    {
        # browser()
        startTime <- thisGet("startTime")
        if (debug) cat("startTimeHandler, svalue(h$obj)=",svalue(h$obj),",h$action=",h$action,"\n")
        startTime[h$action] <- as.character(svalue(h$obj))
        t1 <- utime(paste(thisGet("startDate"),paste(startTime,collapse=" ")),in.format=infmt)
        dpar(start=t1)
        thisSet("startTime",startTime)

        # adjust end time widgets
        t2 <- dpar("end")
        endDate <- format(t2,format=dfmt)
        endTime <- formatHMS(t2)

        thisSet("endDate",endDate)
        thisSet("endTime",endTime)

        blockHandlers(endDateWidget)
        blockHandlers(endHourWidget)
        blockHandlers(endMinuteWidget)
        blockHandlers(endSecWidget)

        svalue(endDateWidget) <- endDate
        svalue(endHourWidget) <- endTime[1]
        svalue(endMinuteWidget) <- endTime[2]
        svalue(endSecWidget) <- endTime[3]

        unblockHandlers(endDateWidget)
        unblockHandlers(endHourWidget)
        unblockHandlers(endMinuteWidget)
        unblockHandlers(endSecWidget)

        checkVariables()
        disableZoom()
        NULL
    }

    endDateHandler <- function(h,...)
    {
        # browser()
        if (debug) cat("endDateHandler, svalue(h$obj)=",svalue(h$obj,drop=FALSE),"\n")
        endDate <- svalue(h$obj,drop=TRUE)
        t2 <- utime(paste(endDate,paste(thisGet("endTime"),collapse=" ")),
            in.format=infmt)

        thisSet("endDate",endDate)

        if (t2 <= dpar("start")) {
            # adjust start time widgets
            t1 <- t2 - dpar("lensec")
            dpar(start=t1,end=t2)
            startDate <- format(t1,format=dfmt)
            startTime <- formatHMS(t1)

            thisSet("startDate",startDate)
            thisSet("startTime",startTime)

            blockHandlers(startDateWidget)
            blockHandlers(startHourWidget)
            blockHandlers(startMinuteWidget)
            blockHandlers(startSecWidget)

            svalue(startDateWidget) <- startDate
            svalue(startHourWidget) <- startTime[1]
            svalue(startMinuteWidget) <- startTime[2]
            svalue(startSecWidget) <- startTime[3]

            unblockHandlers(startDateWidget)
            unblockHandlers(startHourWidget)
            unblockHandlers(startMinuteWidget)
            unblockHandlers(startSecWidget)
        }
        else {
            dpar(end=t2)

            setTimePeriodLength()

            blockHandlers(timeLengthTypeWidget)
            blockHandlers(timeLengthValueWidget)

            svalue(timeLengthTypeWidget) <- thisGet("timeLengthType")
            svalue(timeLengthValueWidget) <- thisGet("timeLengthValue")

            unblockHandlers(timeLengthTypeWidget)
            unblockHandlers(timeLengthValueWidget)
        }
        checkVariables()
        disableZoom()
        NULL
    }

    endTimeHandler <- function(h,...)
    {
        # browser()
        if (debug) cat("endTimeHandler, svalue(h$obj)=",svalue(h$obj),",h$action=",h$action,"\n")

        endTime <- thisGet("endTime")
        endTime[h$action] <- as.character(svalue(h$obj))
        thisSet("endTime",endTime)

        t2 <- utime(paste(thisGet("endDate"),paste(endTime,collapse=" ")),in.format=infmt)

        if (t2 <= dpar("start")) {
            # adjust start time widgets
            t1 <- t2 - dpar("lensec")
            dpar(start=t1,end=t2)
            startDate <- format(t1,format=dfmt)
            startTime <- formatHMS(t1)

            thisSet("startDate",startDate)
            thisSet("startTime",startTime)

            blockHandlers(startDateWidget)
            blockHandlers(startHourWidget)
            blockHandlers(startMinuteWidget)
            blockHandlers(startSecWidget)

            svalue(startDateWidget) <- startDate
            svalue(startHourWidget) <- startTime[1]
            svalue(startMinuteWidget) <- startTime[2]
            svalue(startSecWidget) <- startTime[3]

            unblockHandlers(startDateWidget)
            unblockHandlers(startHourWidget)
            unblockHandlers(startMinuteWidget)
            unblockHandlers(startSecWidget)
        }
        else {
            dpar(end=t2)
            setTimePeriodLength()

            blockHandlers(timeLengthTypeWidget)
            blockHandlers(timeLengthValueWidget)

            svalue(timeLengthTypeWidget) <- thisGet("timeLengthType")
            svalue(timeLengthValueWidget) <- thisGet("timeLengthValue")

            unblockHandlers(timeLengthTypeWidget)
            unblockHandlers(timeLengthValueWidget)
        }
        checkVariables()
        disableZoom()
        NULL
    }
    hrs <- sprintf("%02d",0L:23L)
    minsecs <- sprintf("%02d",seq(from=0L,to=55L,by=5L))

    g1 <- gframe("Start Time",container=mainContainer, horizontal=TRUE)

    g2 <- gframe("date",container=g1, horizontal=TRUE)
    startDateWidget <- gcalendar(text=thisGet("startDate"),format=dfmt,
        container=g2,handler=startDateHandler)
    tooltip(startDateWidget) <- "Set dpar(\"start\"). Changing the start date or time changes the end time to (start+length) and removes any zoom. Hit return after editing the date."

    g2 <- gframe("hour",container=g1, horizontal=TRUE)
    startHourWidget <- gcombobox(hrs,container=g2,action=1L,handler=startTimeHandler)
    tooltip(startHourWidget) <- "Set dpar(\"start\"). Changing the start date or time changes the end time to (start+length) and removes any zoom."

    g2 <- gframe("minute",container=g1, horizontal=TRUE)
    startMinuteWidget <- gcombobox(minsecs,container=g2,action=2L, editable=TRUE,
        handler=startTimeHandler)
    tooltip(startMinuteWidget) <- "Set dpar(\"start\"). Changing the start date or time changes the end time to (start+length) and removes any zoom."

    g2 <- gframe("sec",container=g1, horizontal=TRUE)
    startSecWidget <- gcombobox(minsecs, container=g2,action=3L, editable=TRUE,
        handler=startTimeHandler)
    tooltip(startSecWidget) <- "Set dpar(\"start\"). Changing the start date or time changes the end time to (start+length) and removes any zoom."

    g1 <- gframe("End Time",container=mainContainer, horizontal=TRUE)

    g2 <- gframe("date",container=g1, horizontal=TRUE)
    endDateWidget <- gcalendar(text=thisGet("endDate"),format=dfmt,container=g2,handler=endDateHandler)
    tooltip(endDateWidget) <- "Set dpar(\"end\"). Changing the end date or time changes the period length to (end-start) and removes any zoom. Hit return after editing the date."

    g2 <- gframe("hour",container=g1, horizontal=TRUE)
    endHourWidget <- gcombobox(hrs,container=g2,action=1L,handler=endTimeHandler)
    tooltip(endHourWidget) <- "Set dpar(\"end\"). Changing the end date or time changes the period length to (end-start) and removes any zoom."
    g2 <- gframe("minute",container=g1, horizontal=TRUE)
    endMinuteWidget <- gcombobox(minsecs, container=g2, action=2L, editable=TRUE,
        handler=endTimeHandler)
    tooltip(endMinuteWidget) <- "Set dpar(\"end\"). Changing the end date or time changes the period length to (end-start) and removes any zoom."
    g2 <- gframe("sec",container=g1, horizontal=TRUE)
    endSecWidget <- gcombobox(minsecs, container=g2, action=3L,  editable=TRUE,
        handler=endTimeHandler)
    tooltip(endSecWidget) <- "Set dpar(\"end\"). Changing the end date and time changes the period length to (end-start) and removes any zoom."

    timeLengthTypeHandler <- function(h,...)
    {
        dt <- svalue(h$obj)
        dv <- thisGet("timeLengthValue")
        if (debug) cat("timeLengthTypeHandler, timeLengthType=",dt,", timeLengthValue=",dv,"\n")
        switch(dt,
            day=dpar(lenday=dv),
            hour=dpar(lenhr=dv),
            minute=dpar(lenmin=dv),
            second=dpar(lensec=dv))

        thisSet("timeLengthType",dt)

        # by default, font is not initialized, all NULL
        if (FALSE) {
            f <- font(h$obj)
            if (debug) cat("timeLengthTypeHandler, font, weight=",f$weight,", style=",f$style,
                ", family=",f$family,",size=",f$size,",foreground=",f$foreground,
                ", background=",f$background,", scale=",f$scale,"\n")
        }

        # adjust end time widgets
        t2 <- dpar("end")
        endDate <- format(t2,format=dfmt)
        endTime <- formatHMS(t2)

        thisSet("endDate",endDate)
        thisSet("endTime",endTime)

        blockHandlers(endDateWidget)
        blockHandlers(endHourWidget)
        blockHandlers(endMinuteWidget)
        blockHandlers(endSecWidget)

        svalue(endDateWidget) <- endDate
        svalue(endHourWidget) <- endTime[1]
        svalue(endMinuteWidget) <- endTime[2]
        svalue(endSecWidget) <- endTime[3]

        unblockHandlers(endDateWidget)
        unblockHandlers(endHourWidget)
        unblockHandlers(endMinuteWidget)
        unblockHandlers(endSecWidget)

        checkVariables()
        disableZoom()
        NULL
    }

    timeLengthValueHandler <- function(h,...)
    {
        dt <- thisGet("timeLengthType")
        dv <- as.numeric(svalue(h$obj))
        if (debug) cat("timeLengthValueHandler, timeLengthType=",dt,", timeLengthValue=",dv,"\n")
        if (!is.na(dv)) {
            switch(dt,
                day=dpar(lenday=dv),
                hour=dpar(lenhr=dv),
                minute=dpar(lenmin=dv),
                second=dpar(lensec=dv))

            thisSet("timeLengthValue",dv)

            # adjust end time widgets
            t2 <- dpar("end")
            endDate <- format(t2,format=dfmt)
            endTime <- formatHMS(t2)

            thisSet("endDate",endDate)
            thisSet("endTime",endTime)

            blockHandlers(endDateWidget)
            blockHandlers(endHourWidget)
            blockHandlers(endMinuteWidget)
            blockHandlers(endSecWidget)

            svalue(endDateWidget) <- endDate
            svalue(endHourWidget) <- endTime[1]
            svalue(endMinuteWidget) <- endTime[2]
            svalue(endSecWidget) <- endTime[3]

            unblockHandlers(endDateWidget)
            unblockHandlers(endHourWidget)
            unblockHandlers(endMinuteWidget)
            unblockHandlers(endSecWidget)
            checkVariables()
            disableZoom()
        }
        NULL
    }

    g1 <- gframe("Time",container=mainContainer, horizontal=TRUE)
    g2 <- gframe("period length",container=g1, horizontal=TRUE)

    types <- c("day","hour","minute","second")
    selected <- match(thisGet("timeLengthType"),types,nomatch=1)
    timeLengthTypeWidget <- gcombobox(types,selected=selected,container=g2,
        hander=timeLengthTypeHandler)
    tooltip(timeLengthTypeWidget) <- "Set dpar(\"lenday\",\"lenhr\",...) Changing the time period length changes the end time to (start+length) and removes any zoom."

    # size(timeLengthTypeWidget) is 1,1 at this point
    addHandlerChanged(timeLengthTypeWidget,timeLengthTypeHandler)

    timeLengthValueWidget <- gedit(as.character(thisGet("timeLengthValue")),width=6,container=g2)
    tooltip(timeLengthValueWidget) <- "Set dpar(\"lenday\",\"lenhr\",...) Changing the time period length changes the end time to (start+length) and removes any zoom."
    # Had to use this method, rather than handler= in the constructor
    addHandlerChanged(timeLengthValueWidget,timeLengthValueHandler)

    backForwardHandler <- function(h,...)
    {
        if (h$action == 1L)
            dpar.next()
        else
            dpar.prev()

        # adjust start time widgets
        t1 <- dpar("start")
        startDate <- format(t1,format=dfmt)
        startTime <- formatHMS(t1)

        thisSet("startDate",startDate)
        thisSet("startTime",startTime)

        blockHandlers(startDateWidget)
        blockHandlers(startHourWidget)
        blockHandlers(startMinuteWidget)
        blockHandlers(startSecWidget)

        svalue(startDateWidget) <- startDate
        svalue(startHourWidget) <- startTime[1]
        svalue(startMinuteWidget) <- startTime[2]
        svalue(startSecWidget) <- startTime[3]

        unblockHandlers(startDateWidget)
        unblockHandlers(startHourWidget)
        unblockHandlers(startMinuteWidget)
        unblockHandlers(startSecWidget)

        # adjust end time widgets
        t2 <- dpar("end")
        endDate <- format(t2,format=dfmt)
        endTime <- formatHMS(t2)

        thisSet("endDate",endDate)
        thisSet("endTime",endTime)

        blockHandlers(endDateWidget)
        blockHandlers(endHourWidget)
        blockHandlers(endMinuteWidget)
        blockHandlers(endSecWidget)

        svalue(endDateWidget) <- endDate
        svalue(endHourWidget) <- endTime[1]
        svalue(endMinuteWidget) <- endTime[2]
        svalue(endSecWidget) <- endTime[3]

        unblockHandlers(endDateWidget)
        unblockHandlers(endHourWidget)
        unblockHandlers(endMinuteWidget)
        unblockHandlers(endSecWidget)
        checkVariables()
        disableZoom()
        NULL
    }

    g2 <- gframe("shift",container=g1, horizontal=TRUE)
    widget <- gbutton("previous",container=g2,handler=backForwardHandler,action=-1L)
    tooltip(widget) <- "shift start and end time earlier by the period length"

    widget <- gbutton("next",container=g2,handler=backForwardHandler,action=1L)
    tooltip(widget) <- "shift start and end time later by the period length"

    timeZoneHandler <- function(h,...)
    {
        if (svalue(h$obj) == "local")
            options(time.zone="")
        else
            options(time.zone=svalue(h$obj))

        t1  <- utime(paste(thisGet("startDate"),paste(thisGet("startTime"),collapse=" ")),
                in.format=infmt)
        t2 <- utime(paste(thisGet("endDate"),paste(thisGet("endTime"),collapse=" ")),
            in.format=infmt)

        dpar(start=t1,end=t2)

        setTimePeriodLength()

        blockHandlers(timeLengthTypeWidget)
        blockHandlers(timeLengthValueWidget)

        svalue(timeLengthTypeWidget) <- thisGet("timeLengthType")
        svalue(timeLengthValueWidget) <- thisGet("timeLengthValue")

        unblockHandlers(timeLengthTypeWidget)
        unblockHandlers(timeLengthValueWidget)

        disableZoom()
        NULL
    }

    localtz <- options("time.zone")[[1]]
    if (nchar(localtz) == 0) {
        localtz <- attr(as.POSIXlt(Sys.time()),"tzone")
        if (nchar(localtz[1]) > 0) localtz <- localtz[1]
        else localtz <- Sys.getenv("TZ")
    }
    if (nchar(localtz) == 0) localtz <- "local"

    g2 <- gframe("zone",container=g1, horizontal=TRUE)
    radio <- gradio(c(localtz,"UTC"),container=g2,horizontal=TRUE,handler=timeZoneHandler)
    thisSet("tzradio",radio)

    gb1 <- ggroup(container=mainContainer, horizontal=TRUE)
    gn1 <- ggroup(container=mainContainer, horizontal=TRUE,use.scrollbar=TRUE)

    notebook <- gnotebook(container=gn1, tab.pos=3)
    thisSet("variablesNotebookWidget",notebook)
    # size(gn1) <- c(800,600)

    if (FALSE) {
        widget <- gbutton("show variables",container=gb1,handler=showVariablesHandler,
            action=notebook)
        tooltip(widget) <- "Read variable names from NetCDF file and display"
    }

    g1 <- gframe("Heights (m)",container=mainContainer, horizontal=TRUE)

    if (FALSE) {
        hts <- thisGet("allHeights")
        if (is.null(hts)) hts <- 0
    }
    heightsHandler <- function(h,...)
    {
        hts <- svalue(h$obj)
        if (debug) cat("heightsHandler, hts=",paste(hts,collapse=","),"\n")
        if ("all" %in% hts) {
            hts <- thisGet("allHeights")
            chts <- sapply(hts,format)
            svalue(h$obj) <- chts
            dpar(hts=NULL)
        }
        else if ("clear" %in% hts) {
            hts <- thisGet("allHeights")
            svalue(h$obj) <- rep(FALSE,length(hts)+2)
            dpar(hts=NULL)
        }
        else {
            hts <- as.numeric(hts)
            dpar(hts=hts)
        }
        NULL
    }
    if (FALSE) {
        chts <- c("clear","all",sapply(hts,format))
        names(chts) <- chts
    }
    else chts <- c("clear","all")

    heightsCheckBoxWidget <- gcheckboxgroup(chts,checked=rep(FALSE,length(chts)),
        horizontal=TRUE,handler=heightsHandler,container=g1)
    thisSet("heightsCheckBoxWidget",heightsCheckBoxWidget)

    g1 <- gframe("Stations",container=mainContainer, horizontal=TRUE)

    if (FALSE) {
        stns <- thisGet("allStations")
    }

    stationsHandler <- function(h,...)
    {
        stns <- svalue(h$obj)
        if (debug) cat("stationHandler, stns=",paste(stns,collapse=","),"\n")
        if ("all" %in% stns) {
            stns <- thisGet("allStations")
            cstns <- sapply(stns,format)
            svalue(h$obj) <- cstns
            dpar(stns=NULL)
        }
        else if ("clear" %in% stns) {
            stns <- thisGet("allStations")
            svalue(h$obj) <- rep(FALSE,length(stns)+2)
            dpar(stns=NULL)
        }
        else {
            stns <- as.numeric(stns)
            dpar(stns=stns)
        }
        NULL
    }

    if (FALSE) {
        cstns <- c("clear","all",sapply(stns,format))
    }
    else cstns <- c("clear","all")

    stationsCheckBoxWidget <- gcheckboxgroup(cstns,checked=rep(FALSE,length(cstns)),
        horizontal=TRUE,handler=stationsHandler,container=g1)
    thisSet("stationsCheckBoxWidget",stationsCheckBoxWidget)

    g1 <- gframe("Sites",container=mainContainer, horizontal=TRUE)

    if (FALSE) {
        sites <- thisGet("allSites")
    }
    sitesHandler <- function(h,...)
    {
        sites <- svalue(h$obj)
        if (debug) cat("sitesHandler, sites=",paste(sites,collapse=","),"\n")
        if ("all" %in% sites) {
            sites <- thisGet("allSites")
            svalue(h$obj) <- sites
            dpar(sites=NULL)
        }
        else if ("clear" %in% sites) {
            sites <- thisGet("allSites")
            svalue(h$obj) <- rep(FALSE,length(sites)+2)
            dpar(sites=NULL)
        }
        else {
            dpar(sites=sites)
        }
        NULL
    }
    if (FALSE) {
        csites <- c("clear","all",sites)
    }
    else csites <- c("clear","all")

    sitesCheckBoxWidget <- gcheckboxgroup(csites,checked=rep(FALSE,length(csites)),
        horizontal=TRUE,handler=sitesHandler,container=g1)
    thisSet("sitesCheckBoxWidget",sitesCheckBoxWidget)

    checkVariables()

    readDataHandler <- function(h,...)
    {
        vars <- thisGet("selectedVars")
        if (debug) cat("selected vars=",paste(vars,collapse=","),"\n")

        if (FALSE) {
            allvars <- thisGet("allVariables")
            w1vars <- words(allvars,1,1,sep=".")
            mx <- match(w1vars,vars)
            if (any(!is.na(mx))) vars <- allvars[!is.na(mx)]
            else vars <- NULL
        }
        if (length(vars) == 0) return(NULL)

        if (debug) cat("reading ",paste(vars,collapse=","),"\n")

        # warn=1, print warnings as they occur.
        # Under the default warn=0, warnings are printed
        # when the top-level function returns, which, in the
        # case of dgui, is just before exiting R.
        wl <- options(warn=1)

        if (FALSE) {
            lenfile <- dpar("lenfile")
            if (is.null(lenfile)) 
                iod <- netcdf()
            else iod <- netcdf(lenfile = lenfile)
            x <- readts(iod, variables = vars)
            close(iod)
        }
        else {
            x <- dat(vars,derived=FALSE,smooth=TRUE)
        }

        options(wl)

        ovar <- thisGet("outVarName")
        if (!is.null(ovar)) assign(ovar,x,envir=globalenv())
        else thisSet(".tmpData",x)

        NULL
    }
    g1 <- ggroup(container=mainContainer, horizontal=TRUE)
    outputVariableHandler <- function(h,...)
    {
        var <- as.character(svalue(h$obj))
        if (var == "<none>") thisSet("outVarName",NULL)
        else thisSet("outVarName",var)
        NULL
    }

    plotIt <- function(tz=NULL)
    {
        ovar <- thisGet("outVarName")
        x <- NULL
        if (!is.null(ovar)) {
            if (exists(ovar,envir=globalenv())) x <- get(ovar,envir=globalenv())
            else {
                cat("output object",ovar,"not found.\n")
                return(NULL)
            }
        }
        else x <- thisGet(".tmpData")

        if (!is.null(x)) {
            wl <- options(warn=1)
            if (is.null(tz)) tryCatch(plot(x))
            else tryCatch(plot(x[tz,]))
            options(wl)
        }
        else cat("output object",ovar,"is NULL.\n")
        NULL
    }
    zoomInHandler <- function(h,...)
    {
        # cat("zoomInHandler\n")
        tz <- tlocator(2)
        if (!is.null(tz)) {
            if (tz[2] <= tz[1]) {
                cat("bad time selection: ",format(tz[1],format="%Y %b %d %H:%M:%OS"),
                    "-", format(tz[2],format="%Y %b %d %H:%M:%OS"),"\n")
            }
            else {
                zoom <- thisGet("zoomTimes")
                zoom[[length(zoom)+1]] <- tz
                thisSet("zoomTimes",zoom)
                cat("zoom level=",length(zoom),"\n")
                plotIt(tz)
            }
        }
        NULL

    }
    zoomOutHandler <- function(h,...)
    {
        # cat("zoomOutHandler\n")
        zoom <- thisGet("zoomTimes")
        # cat("zoomInHandler\n")
        if (length(zoom) > 0) {
            zoom[[length(zoom)]] <- NULL
            thisSet("zoomTimes",zoom)
        }
        cat("zoom level=",length(zoom),"\n")

        if (length(zoom) == 0) plotIt(NULL)
        else plotIt(zoom[[length(zoom)]])
        NULL
    }
    noZoomHandler <- function(h,...)
    {
        # cat("noZoomHandler\n")
        zoom <- thisGet("zoomTimes")
        length(zoom) <- 0
        thisSet("zoomTimes",zoom)

        cat("zoom level=",length(zoom),"\n")

        plotIt(NULL)
        NULL
    }

    readAndPlotHandler <- function(h,...)
    {
        readDataHandler()

        zoom <- thisGet("zoomTimes")
        if (length(zoom) > 0) plotIt(zoom[[length(zoom)]])
        else {
            plotIt()
            enableZoom()
        }
        NULL
    }

    widget <- gbutton("read data",container=g1,handler=readDataHandler)
    tooltip(widget) <- "Read data from selected variables between start and end time into output object"
    enabled(widget) <- FALSE
    thisSet("readDataWidget",widget)

    widget <- gbutton("read and plot",container=g1,handler=readAndPlotHandler,action=g1)
    tooltip(widget) <- "Read data from selected variables between start and end time into output object and plot the resulting time series at the current zoom setting."
    enabled(widget) <- FALSE
    thisSet("readAndPlotDataWidget",widget)

    glabel("output to:",container=g1)
    widget <- gcombobox(c("<none>","x","x1","x2","y","y1","y2"),
        container=g1, editable=TRUE, handler=outputVariableHandler)

    tooltip(widget) <- "Data read from NetCDF files will be placed in this time series object in .GlobalEnv at the top of the R search list, where it is available from the R command line."

    zoomButtons <- list()
    widget <- gbutton("zoom in",container=g1,handler=zoomInHandler)
    tooltip(widget) <- "In graphics window, after cursor changes to a +, click with the left mouse button to select plot start and end times"
    enabled(widget) <- FALSE
    zoomButtons[1] <- widget

    widget <- gbutton("zoom out",container=g1,handler=zoomOutHandler)
    tooltip(widget) <- "back out one level of zoom, and re-plot"
    enabled(widget) <- FALSE
    zoomButtons[2] <- widget

    widget <- gbutton("no zoom",container=g1,handler=noZoomHandler)
    enabled(widget) <- FALSE
    zoomButtons[3] <- widget
    thisSet("zoomButtons",zoomButtons)

    thisSet("mainWidget",mainWidget)

    visible(mainWidget) <- visible

    tag(mainWidget,"ltsz") <- NULL
    tag(mainWidget,"timeLengthTypeWidget") <- timeLengthTypeWidget
    tag(mainWidget,"startHourWidget") <- startHourWidget
    tag(mainWidget,"startMinuteWidget") <- startMinuteWidget
    tag(mainWidget,"startSecWidget") <- startSecWidget
    tag(mainWidget,"endMinuteWidget") <- endMinuteWidget
    tag(mainWidget,"endSecWidget") <- endSecWidget

    if (guiToolkit()@toolkit != "tcltk" && visible) {
        # now that the window is visible with known widget sizes,
        # enlarge gcombobox widgets
        ltsz <- tag(mainWidget,"ltsz")
        if (debug) cat("ltsz=",paste(ltsz,collapse=","),"\n")
        if (is.null(ltsz)) {
            ltw <- tag(mainWidget,"timeLengthTypeWidget")
            ltsz <- size(ltw)
            ltsz[1] <- ltsz[1] * 2L
            if (debug) cat("ltsz=",paste(ltsz,collapse=","),"\n")
            size(ltw) <- ltsz
            tag(mainWidget,"ltsz") <- ltsz

            sz <- size(tag(mainWidget,"startHourWidget"))
            size(tag(mainWidget,"startMinuteWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(mainWidget,"startSecWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(mainWidget,"endMinuteWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(mainWidget,"endSecWidget")) <- as.integer(c(sz[1]*1.3,sz[2]))

            dc <- thisGet("datasetCombo")
            size(dc) <- c(100,25)
            tz <- thisGet("tzradio")
            size(tz) <- c(150,25)
        }
    }

    invisible(mainWidget)
}

dgpar <- function(visible=TRUE,debug=FALSE)
{
    stop("dgpar is now called dgui")
}
