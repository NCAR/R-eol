# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:

# stations gcheckboxgroup, with an "all" button.
#   dpar("all.stations") set in project.init?  Or read from netcdf?
#   read from netcdf. Augment with all.stations?
# sites radiobox
#   dpar("all.sites") read from netcdf.
# heights gcheckboxgroup:
#   Gather from variables.

.this <- new.env(parent=emptyenv())

mytest <- 2

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

that_exists <- function(name) 
{
    parenv <- parent.env(environment(NULL))
    exists(name,envir=parenv)
}

that_get <- function(name) 
{
    parenv <- parent.env(environment(NULL))
    get(name,envir=parenv)
}

that_put <- function(name,val) 
{
    # enclosing environment, the variables in the namespace scope
    parenv <- parent.env(environment(NULL))
    lb <- bindingIsLocked(name,parenv)
    if (lb) unlockBinding(name,parenv)
    assign(name,val,envir=parenv)
    if (lb) lockBinding(name,parenv)
}

# The date selector in the calendar widget can only handle this time format.
# "%Y %m %d" doesn't work.
dfmt <- "%Y-%m-%d"
infmt <- "%Y-%m-%d%H%M%OS"

that_put("mytest",0)

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

thisSet("outVarName","x")

thisSet("timeLengthType","")
thisSet("startDate","")
thisSet("startTime","")
thisSet("endDate","")
thisSet("endTime","")

thisSet("heightsCheckBoxWidget",NULL)
    
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

dgui <- function(visible=TRUE,debug=FALSE)
{
    cat("mytest exists=",that_exists("mytest"),"\n")
    cat("mytest=",that_get("mytest"),"\n")
    that_put("mytest",20)
    cat("mytest=",that_get("mytest"),"\n")

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
            NULL
        }

        g1 <- gframe("Dataset",container=mainContainer, horizontal=TRUE)
        combo <- gcombobox(names(.datasets),container=g1,handler=datasetHandler,
            size=c(100,25))
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
    size(ncd) <- c(400,25)

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
    size(ncf) <- c(400,25)

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
        NULL
    }

    endDateHandler <- function(h,...)
    {
        # browser()
        if (debug) cat("endDateHandler, svalue(h$obj)=",svalue(h$obj,drop=FALSE),"\n")
        endDate <- svalue(h$obj,drop=TRUE)
        t2 <- utime(paste(endDate,paste(thisGet("endTime"),collapse=" ")),in.format=infmt)

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
        NULL
    }
    hrs <- sprintf("%02d",0L:23L)
    minsecs <- sprintf("%02d",seq(from=0L,to=55L,by=5L))

    g1 <- gframe("Start Time",container=mainContainer, horizontal=TRUE)

    g2 <- gframe("date",container=g1, horizontal=TRUE)
    startDateWidget <- gcalendar(text=thisGet("startDate"),format=dfmt,
        container=g2,handler=startDateHandler)

    g2 <- gframe("hour",container=g1, horizontal=TRUE)
    startHourWidget <- gcombobox(hrs,container=g2,action=1L,handler=startTimeHandler)
    g2 <- gframe("minute",container=g1, horizontal=TRUE)
    startMinuteWidget <- gcombobox(minsecs,container=g2,action=2L, editable=TRUE,
        handler=startTimeHandler)
    g2 <- gframe("sec",container=g1, horizontal=TRUE)
    startSecWidget <- gcombobox(minsecs, container=g2,action=3L, editable=TRUE,
        handler=startTimeHandler)

    g1 <- gframe("End Time",container=mainContainer, horizontal=TRUE)

    g2 <- gframe("date",container=g1, horizontal=TRUE)
    endDateWidget <- gcalendar(text=thisGet("endDate"),format=dfmt,container=g2,handler=endDateHandler)

    g2 <- gframe("hour",container=g1, horizontal=TRUE)
    endHourWidget <- gcombobox(hrs,container=g2,action=1L,handler=endTimeHandler)
    g2 <- gframe("minute",container=g1, horizontal=TRUE)
    endMinuteWidget <- gcombobox(minsecs, container=g2, action=2L, editable=TRUE,
        handler=endTimeHandler)
    g2 <- gframe("sec",container=g1, horizontal=TRUE)
    endSecWidget <- gcombobox(minsecs, container=g2, action=3L,  editable=TRUE,
        handler=endTimeHandler)

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
        }
        NULL
    }

    g1 <- gframe("Time",container=mainContainer, horizontal=TRUE)
    g2 <- gframe("period length",container=g1, horizontal=TRUE)

    types <- c("day","hour","minute","second")
    selected <- match(thisGet("timeLengthType"),types,nomatch=1)
    timeLengthTypeWidget <- gcombobox(types,selected=selected,container=g2,
        hander=timeLengthTypeHandler)

    # size(timeLengthTypeWidget) is 1,1 at this point
    addHandlerChanged(timeLengthTypeWidget,timeLengthTypeHandler)

    timeLengthValueWidget <- gedit(as.character(thisGet("timeLengthValue")),width=6,container=g2)
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
        NULL
    }

    g2 <- gframe("time shift",container=g1, horizontal=TRUE)
    gbutton("previous",container=g2,handler=backForwardHandler,action=-1L)
    gbutton("next",container=g2,handler=backForwardHandler,action=1L)

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
        NULL
    }

    localtz <- options("time.zone")[[1]]
    if (nchar(localtz) == 0) {
        localtz <- attr(as.POSIXlt(Sys.time()),"tzone")
        if (nchar(localtz[1]) > 0) localtz <- localtz[1]
        else localtz <- Sys.getenv("TZ")
    }
    if (nchar(localtz) == 0) localtz <- "local"

    g2 <- gframe("time zone",container=g1, horizontal=TRUE)
    radio <- gradio(c(localtz,"UTC"),container=g2,horizontal=TRUE,handler=timeZoneHandler)
    thisSet("tzradio",radio)

    toggleVariableHandler <- function(h,...)
    {
        # cat("var=",svalue(h$obj)," selected\n")
        sv <- thisGet("selectedVars")
        if (svalue(h$obj)) {
            cat("var=",h$action," selected\n")
            thisSet("selectedVars",c(sv,h$action))
        }
        else {
            cat("var=",h$action," unselected\n")
            mx <- match(h$action,sv)
            if (!is.na(mx)) thisSet("selectedVars", sv[-mx])
        }
        NULL
    }

    showVariablesHandler <- function(h,...)
    {
        thisSet("selectedVars",NULL)
        allvars <- variables()
        thisSet("allVariables",allvars)
        if (length(allvars) > 0) {
            hts <- sort(unique(heights(allvars)))
            hts <- hts[hts > 0.1]
            thisSet("allHeights",hts)

            chts <- sapply(hts,format)

            if (debug) cat("heights=",paste(chts,collapse=","),"\n")
            cb <- thisGet("heightsCheckBoxWidget")
            cb[] <- c("clear","all",chts)

            lenfile <- dpar("lenfile")
            if (is.null(lenfile)) 
                iod <- netcdf()
            else iod <- netcdf(lenfile = lenfile)
            stns <- sort(unique(stations(iod)))
            close(iod)
            thisSet("allStations",stns)

            if (debug) cat("allStations=",paste(stns,collapse=","),"\n")
            cb <- thisGet("stationsCheckBoxWidget")
            cb[] <- c("clear","all",sapply(stns,format))

            sites <- sort(unique(sites(allvars)))
            thisSet("allSites",sites)
            cb <- thisGet("sitesCheckBoxWidget")
            cb[] <- c("clear","all",sites)

            w1vars <- sort(unique(words(allvars,1,1,sep=".")))

            cnts <- words(w1vars,1,1,sep="_") == "counts"
            if (any(cnts)) w1vars <- w1vars[!cnts]
            cat(paste(w1vars,collapse=","),"\n")

            high_moments <- grepl("'",w1vars,fixed=TRUE)

            # delete any existing tab pages
            if (length(h$action) > 0) {
                for (i in 1:length(h$action)) {
                    svalue(h$action) <- i
                    dispose(h$action)
                }
            }
            # cat("disposed\n")
            # g1 <- ggroup(container=h$action, horizontal=FALSE,label="variables")
            # var_group <- ggroup(horizontal=TRUE,use.scrollwindow=FALSE,container=h$action,label="variables")
            layout <- glayout(container=h$action, homogeneous=TRUE,
                label="first moments",spacing=0)
            nc <- 10
            i <- 0
            for (var in w1vars[!high_moments]) {
                ic <- (i %% nc) + 1
                ir <- (i %/% nc) + 1
                cb <- gcheckbox(text=var,action=var,
                    handler=toggleVariableHandler)
                layout[ir,ic] <- cb
                if (i == 0) {
                    sz <- size(cb)
                    cat("cb size =",paste(sz,collapse=","),"\n")
                }
                i <- i + 1
            }
            thisSet("var1MomLayout",layout)
            # cat("done\n")

            if (any(high_moments)) {
                layout <- glayout(container=h$action, homogeneous=TRUE,
                    label="higher moments",spacing=0)
                nc <- 10
                i <- 0
                for (var in w1vars[high_moments]) {
                    ic <- (i %% nc) + 1
                    ir <- (i %/% nc) + 1
                    cb <- gcheckbox(text=var,action=var,
                        handler=toggleVariableHandler)
                    layout[ir,ic] <- cb
                    if (i == 0) {
                        sz <- size(cb)
                        cat("cb size =",paste(sz,collapse=","),"\n")
                    }
                    i <- i + 1
                }
                svalue(h$action) <- i
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
            else {
                sz <- size(layout)
                cat("layout size =",paste(sz,collapse=","),"\n")
                # size(layout) <- c(800,600)
            }
        }
        NULL
    }

    gb1 <- ggroup(container=mainContainer, horizontal=TRUE)
    gn1 <- ggroup(container=mainContainer, horizontal=TRUE,use.scrollbar=TRUE)

    g2 <- gnotebook(container=gn1, tab.pos=3)
    # size(gn1) <- c(800,600)

    gbutton("show variables",container=gb1,handler=showVariablesHandler,action=g2)

    g1 <- gframe("Heights (m)",container=mainContainer, horizontal=TRUE)

    hts <- thisGet("allHeights")
    if (is.null(hts)) hts <- 0
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
    chts <- c("clear","all",sapply(hts,format))
    names(chts) <- chts

    heightsCheckBoxWidget <- gcheckboxgroup(chts,checked=rep(FALSE,length(chts)),
        horizontal=TRUE,handler=heightsHandler,container=g1)
    thisSet("heightsCheckBoxWidget",heightsCheckBoxWidget)

    g1 <- gframe("Stations",container=mainContainer, horizontal=TRUE)

    stns <- thisGet("allStations")
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
    cstns <- c("clear","all",sapply(stns,format))

    stationsCheckBoxWidget <- gcheckboxgroup(cstns,checked=rep(FALSE,length(cstns)),
        horizontal=TRUE,handler=stationsHandler,container=g1)
    thisSet("stationsCheckBoxWidget",stationsCheckBoxWidget)

    g1 <- gframe("Sites",container=mainContainer, horizontal=TRUE)

    sites <- thisGet("allSites")
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
    csites <- c("clear","all",sites)

    sitesCheckBoxWidget <- gcheckboxgroup(csites,checked=rep(FALSE,length(csites)),
        horizontal=TRUE,handler=sitesHandler,container=g1)
    thisSet("sitesCheckBoxWidget",sitesCheckBoxWidget)

    readVariablesHandler <- function(h,...)
    {
        sv <- thisGet("selectedVars")
        cat("reading ",paste(sv,collapse=","),"\n")
        allvars <- thisGet("allVariables")
        w1vars <- words(allvars,1,1,sep=".")
        mx <- match(w1vars,sv)
        if (any(!is.na(mx))) {
            vars <- allvars[!is.na(mx)]

            if (FALSE) {
                lenfile <- dpar("lenfile")
                if (is.null(lenfile)) 
                    iod <- netcdf()
                else iod <- netcdf(lenfile = lenfile)
                x <- readts(iod, variables = vars)
                close(iod)
            }
            else {
                x <- dat(vars,derived=FALSE)
            }
            assign(thisGet("outVarName"),x,envir=globalenv())
        }

        # x <- dat(thisGet("selectedVars"))
        NULL
    }
    g1 <- ggroup(container=mainContainer, horizontal=TRUE)
    gbutton("read variables into",container=g1,handler=readVariablesHandler)

    outputVariableHandler <- function(h,...)
    {
        thisSet("outVarName",as.character(svalue(h$obj)))
        NULL
    }

    gcombobox(c("x","x1","x2","y","y1","y2"),
        container=g1,action=2L, editable=TRUE, handler=outputVariableHandler)

    plotHandler <- function(h,...)
    {
        ov <- thisGet("outVarName")
        tryCatch(plot(get(ov,envir=globalenv())))
        NULL
    }

    gbutton("plot",container=g1,handler=plotHandler)

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
