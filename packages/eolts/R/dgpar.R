# -*- mode: C++; indent-tabs-mode: nil; c-basic-offset: 4; tab-width: 4; -*-
# vim: set shiftwidth=4 softtabstop=4 expandtab:

# stations gcheckboxgroup, with an "all" button.
#   dpar("all.stations") set in project.init?  Or read from netcdf?
#   read from netcdf. Augment with all.stations?
# sites radiobox
#   dpar("all.sites") read from netcdf.
# heights gcheckboxgroup:
#   Gather from variables.
# variables:
#

dgpar <- function(visible=TRUE,debug=FALSE)
{
    if (!exists(".dgpar.w",envir=eolts:::.eoltsEnv)) {
        format_hms <- function(tx)
        {
            c(format(tx,format="%H"),format(tx,format="%M"),format(tx,format="%OS"))
        }

        set_dlen <- function()
        {
            dlen_type <<- "second"
            dlen_value <<- dpar("lensec")

            if ((dlen_value %% (86400L/2L)) == 0) {
                dlen_type <<- "day"
                dlen_value <<- dlen_value / 86400
            }
            else if ((dlen_value %% 3600) == 0) {
                dlen_type <<- "hour"
                dlen_value <<- dlen_value / 3600
            }
            else if ((dlen_value %% 60) == 0) {
                dlen_type <<- "minute"
                dlen_value <<- dlen_value / 60
            }
        }

        # The underlying date selector can only handle this time format
        # "%Y %m %d" doesn't work
        dfmt <- "%Y-%m-%d"

        infmt <- "%Y-%m-%d%H%M%OS"

        t1 <- dpar("start")
        dstart <- format(t1,format=dfmt)
        tstart <- format_hms(t1)

        t2 <- dpar("end")
        dend <- format(t2,format=dfmt)
        tend <- format_hms(t2)

        dlen_type <- ""
        dlen_value <- 0

        set_dlen()

        # primary setting is the start time
        # changing start time results in new end time, using length
        # changing length results in new end time
        # changing end time results in new length
        # "previous", "next" buttons shift both start and end time.

        w <- gwindow("dgpar", visible=FALSE)
        # g <- ggroup(cont=w, horizontal=TRUE)
        g <- ggroup(cont=w, horizontal=FALSE)

        dstarthandler <- function(h,...)
        {
            # browser()
            if (debug) cat("dstart, svalue(h$obj)=",svalue(h$obj,drop=FALSE),"\n")
            dstart <<- svalue(h$obj,drop=TRUE)
            dpar(start=utime(paste(dstart,paste(tstart,collapse=" ")),in.format=infmt))

            # adjust end time widgets
            t2 <- dpar("end")
            dend <<- format(t2,format=dfmt)
            tend <<- format_hms(t2)

            blockHandlers(dend_widget)
            blockHandlers(tend_hr)
            blockHandlers(tend_min)
            blockHandlers(tend_sec)

            svalue(dend_widget) <- dend
            svalue(tend_hr) <- tend[1]
            svalue(tend_min) <- tend[2]
            svalue(tend_sec) <- tend[3]

            unblockHandlers(dend_widget)
            unblockHandlers(tend_hr)
            unblockHandlers(tend_min)
            unblockHandlers(tend_sec)
            NULL
        }

        tstarthandler <- function(h,...)
        {
            # browser()
            if (debug) cat("tstart, svalue(h$obj)=",svalue(h$obj),",h$action=",h$action,"\n")
            tstart[h$action] <<- as.character(svalue(h$obj))
            # if (debug) cat("dstart=",dstart,"\n")
            # if (debug) cat("tstart=",paste(tstart,collapse=" "),"\n")
            # if (debug) cat("start=",paste(dstart,paste(tstart,collapse=" ")),"\n")
            dpar(start=utime(paste(dstart,paste(tstart,collapse=" ")),in.format=infmt))

            # adjust end time widgets
            t2 <- dpar("end")
            dend <<- format(t2,format=dfmt)
            tend <<- format_hms(t2)

            blockHandlers(dend_widget)
            blockHandlers(tend_hr)
            blockHandlers(tend_min)
            blockHandlers(tend_sec)

            svalue(dend_widget) <- dend
            svalue(tend_hr) <- tend[1]
            svalue(tend_min) <- tend[2]
            svalue(tend_sec) <- tend[3]

            unblockHandlers(dend_widget)
            unblockHandlers(tend_hr)
            unblockHandlers(tend_min)
            unblockHandlers(tend_sec)
            NULL
        }

        dendhandler <- function(h,...)
        {
            # browser()
            if (debug) cat("dend, svalue(h$obj)=",svalue(h$obj,drop=FALSE),"\n")
            dend <<- svalue(h$obj,drop=TRUE)
            t2 <- utime(paste(dend,paste(tend,collapse=" ")),in.format=infmt)

            if (t2 <= dpar("start")) {
                # adjust start time widgets
                t1 <- t2 - dpar("lensec")
                dpar(start=t1,end=t2)
                dstart <<- format(t1,format=dfmt)
                tstart <<- format_hms(t1)

                blockHandlers(dstart_widget)
                blockHandlers(tstart_hr)
                blockHandlers(tstart_min)
                blockHandlers(tstart_sec)

                svalue(dstart_widget) <- dstart
                svalue(tstart_hr) <- tstart[1]
                svalue(tstart_min) <- tstart[2]
                svalue(tstart_sec) <- tstart[3]

                unblockHandlers(dstart_widget)
                unblockHandlers(tstart_hr)
                unblockHandlers(tstart_min)
                unblockHandlers(tstart_sec)
            }
            else {
                dpar(end=t2)
                set_dlen()

                blockHandlers(len_type_widget)
                blockHandlers(len_val_widget)

                svalue(len_type_widget) <- dlen_type
                svalue(len_val_widget) <- dlen_value

                unblockHandlers(len_type_widget)
                unblockHandlers(len_val_widget)
            }
            NULL
        }

        tendhandler <- function(h,...)
        {
            # browser()
            if (debug) cat("tend, svalue(h$obj)=",svalue(h$obj),",h$action=",h$action,"\n")
            tend[h$action] <<- as.character(svalue(h$obj))
            # if (debug) cat("tend=",paste(dend,paste(tend,collapse=" ")),"\n")
            t2 <- utime(paste(dend,paste(tend,collapse=" ")),in.format=infmt)

            if (t2 <= dpar("start")) {
                # adjust start time widgets
                t1 <- t2 - dpar("lensec")
                dpar(start=t1,end=t2)
                dstart <<- format(t1,format=dfmt)
                tstart <<- format_hms(t1)

                blockHandlers(dstart_widget)
                blockHandlers(tstart_hr)
                blockHandlers(tstart_min)
                blockHandlers(tstart_sec)

                svalue(dstart_widget) <- dstart
                svalue(tstart_hr) <- tstart[1]
                svalue(tstart_min) <- tstart[2]
                svalue(tstart_sec) <- tstart[3]

                unblockHandlers(dstart_widget)
                unblockHandlers(tstart_hr)
                unblockHandlers(tstart_min)
                unblockHandlers(tstart_sec)
            }
            else {
                dpar(end=t2)
                set_dlen()

                blockHandlers(len_type_widget)
                blockHandlers(len_val_widget)

                svalue(len_type_widget) <- dlen_type
                svalue(len_val_widget) <- dlen_value

                unblockHandlers(len_type_widget)
                unblockHandlers(len_val_widget)
            }
            NULL
        }
        hrs <- sprintf("%02d",0L:23L)
        minsecs <- sprintf("%02d",seq(from=0L,to=55L,by=5L))

        g1 <- gframe("data start",cont=g, horizontal=TRUE)

        g2 <- gframe("date",cont=g1, horizontal=TRUE)
        dstart_widget <- gcalendar(text=dstart,format=dfmt,cont=g2,handler=dstarthandler)

        g2 <- gframe("hour",cont=g1, horizontal=TRUE)
        tstart_hr <- gcombobox(hrs,cont=g2,action=1L,handler=tstarthandler)
        g2 <- gframe("minute",cont=g1, horizontal=TRUE)
        tstart_min <- gcombobox(minsecs,cont=g2,action=2L, editable=TRUE,
            handler=tstarthandler)
        g2 <- gframe("sec",cont=g1, horizontal=TRUE)
        tstart_sec <- gcombobox(minsecs, cont=g2,action=3L, editable=TRUE,
            handler=tstarthandler)

        g1 <- gframe("data end",cont=g, horizontal=TRUE)

        g2 <- gframe("date",cont=g1, horizontal=TRUE)
        dend_widget <- gcalendar(text=dend,format=dfmt,cont=g2,handler=dendhandler)

        g2 <- gframe("hour",cont=g1, horizontal=TRUE)
        tend_hr <- gcombobox(hrs,cont=g2,action=1L,handler=tendhandler)
        g2 <- gframe("minute",cont=g1, horizontal=TRUE)
        tend_min <- gcombobox(minsecs, cont=g2, action=2L, editable=TRUE,
            handler=tendhandler)
        g2 <- gframe("sec",cont=g1, horizontal=TRUE)
        tend_sec <- gcombobox(minsecs, cont=g2, action=3L,  editable=TRUE,
            handler=tendhandler)

        len_type_handler <- function(h,...)
        {
            dlen_type <<- svalue(h$obj)
            if (debug) cat("type handler, dlen_type=",dlen_type,", dlen_value=",dlen_value,"\n")
            switch(dlen_type,
                day=dpar(lenday=dlen_value),
                hour=dpar(lenhr=dlen_value),
                minute=dpar(lenmin=dlen_value),
                second=dpar(lensec=dlen_value))

            # by default, font is not initialized, all NULL
            if (FALSE) {
                f <- font(h$obj)
                if (debug) cat("type handler, font, weight=",f$weight,", style=",f$style,
                    ", family=",f$family,",size=",f$size,",foreground=",f$foreground,
                    ", background=",f$background,", scale=",f$scale,"\n")
            }

            # adjust end time widgets
            t2 <- dpar("end")
            dend <<- format(t2,format=dfmt)
            tend <<- format_hms(t2)

            blockHandlers(dend_widget)
            blockHandlers(tend_hr)
            blockHandlers(tend_min)
            blockHandlers(tend_sec)

            svalue(dend_widget) <- dend
            svalue(tend_hr) <- tend[1]
            svalue(tend_min) <- tend[2]
            svalue(tend_sec) <- tend[3]

            unblockHandlers(dend_widget)
            unblockHandlers(tend_hr)
            unblockHandlers(tend_min)
            unblockHandlers(tend_sec)
            NULL
        }

        len_val_handler <- function(h,...)
        {
            dlen_value <<- as.numeric(svalue(h$obj))
            if (debug) cat("val handler, dlen_type=",dlen_type,", dlen_value=",dlen_value,"\n")
            if (!is.na(dlen_value)) {
                switch(dlen_type,
                    day=dpar(lenday=dlen_value),
                    hour=dpar(lenhr=dlen_value),
                    minute=dpar(lenmin=dlen_value),
                    second=dpar(lensec=dlen_value))

                # adjust end time widgets
                t2 <- dpar("end")
                dend <<- format(t2,format=dfmt)
                tend <<- format_hms(t2)

                blockHandlers(dend_widget)
                blockHandlers(tend_hr)
                blockHandlers(tend_min)
                blockHandlers(tend_sec)

                svalue(dend_widget) <- dend
                svalue(tend_hr) <- tend[1]
                svalue(tend_min) <- tend[2]
                svalue(tend_sec) <- tend[3]

                unblockHandlers(dend_widget)
                unblockHandlers(tend_hr)
                unblockHandlers(tend_min)
                unblockHandlers(tend_sec)
            }
            NULL
        }

        g1 <- gframe("",cont=g, horizontal=TRUE)
        g2 <- gframe("time length",cont=g1, horizontal=TRUE)

        types <- c("day","hour","minute","second")
        selected <- match(dlen_type,types,nomatch=1)
        len_type_widget <- gcombobox(types,selected=selected,cont=g2,
            hander=len_type_handler)

        # size(len_type_widget) is 1,1 at this point
        addHandlerChanged(len_type_widget,len_type_handler)

        len_val_widget <- gedit(as.character(dlen_value),width=6,cont=g2)
        # Had to use this method, rather than handler= in the constructor
        addHandlerChanged(len_val_widget,len_val_handler)

        back_forward_handler <- function(h,...)
        {
            if (h$action == 1L)
                dpar.next()
            else
                dpar.prev()

            # adjust start time widgets
            t1 <- dpar("start")
            dstart <<- format(t1,format=dfmt)
            tstart <<- format_hms(t1)

            blockHandlers(dstart_widget)
            blockHandlers(tstart_hr)
            blockHandlers(tstart_min)
            blockHandlers(tstart_sec)

            svalue(dstart_widget) <- dstart
            svalue(tstart_hr) <- tstart[1]
            svalue(tstart_min) <- tstart[2]
            svalue(tstart_sec) <- tstart[3]

            unblockHandlers(dstart_widget)
            unblockHandlers(tstart_hr)
            unblockHandlers(tstart_min)
            unblockHandlers(tstart_sec)

            # adjust end time widgets
            t2 <- dpar("end")
            dend <<- format(t2,format=dfmt)
            tend <<- format_hms(t2)

            blockHandlers(dend_widget)
            blockHandlers(tend_hr)
            blockHandlers(tend_min)
            blockHandlers(tend_sec)

            svalue(dend_widget) <- dend
            svalue(tend_hr) <- tend[1]
            svalue(tend_min) <- tend[2]
            svalue(tend_sec) <- tend[3]

            unblockHandlers(dend_widget)
            unblockHandlers(tend_hr)
            unblockHandlers(tend_min)
            unblockHandlers(tend_sec)
            NULL
        }

        g2 <- gframe("time shift",cont=g1, horizontal=TRUE)
        gbutton("previous",cont=g2,handler=back_forward_handler,action=-1L)
        gbutton("next",cont=g2,handler=back_forward_handler,action=1L)

        time_zone_handler <- function(h,...)
        {
            if (svalue(h$obj) == "local")
                options(time.zone="")
            else
                options(time.zone=svalue(h$obj))
            dpar(start=utime(paste(dstart,paste(tstart,collapse=" ")),in.format=infmt),
                 end=utime(paste(dend,paste(tend,collapse=" ")),in.format=infmt))

            set_dlen()

            blockHandlers(len_type_widget)
            blockHandlers(len_val_widget)

            svalue(len_type_widget) <- dlen_type
            svalue(len_val_widget) <- dlen_value

            unblockHandlers(len_type_widget)
            unblockHandlers(len_val_widget)
            NULL
        }

        localtz <- options("time.zone")[[1]]
        if (nchar(localtz) == 0) {
            localtz <- attr(as.POSIXlt(Sys.time()),"tzone")
            if (nchar(localtz[1]) > 0) localtz <- localtz[1]
            else localtz <- Sys.getenv("TZ")
        }
        if (nchar(localtz) == 0) localtz <- "local"

        g2 <- gframe("time zone",cont=g1, horizontal=TRUE)
        gradio(c(localtz,"UTC"),cont=g2,horizontal=TRUE,handler=time_zone_handler)

        assign(".dgpar.w",w,envir=eolts:::.eoltsEnv)

        tag(w,"ltsz") <- NULL
        tag(w,"len_type_widget") <- len_type_widget
        tag(w,"tstart_hr") <- tstart_hr
        tag(w,"tstart_min") <- tstart_min
        tag(w,"tstart_sec") <- tstart_sec
        tag(w,"tend_min") <- tend_min
        tag(w,"tend_sec") <- tend_sec
    }
    else {
        w <- get(".dgpar.w",envir=eolts:::.eoltsEnv)
    }
    visible(w) <- visible

    if (guiToolkit()@toolkit != "tcltk" && visible) {
        # now that the window is visible with known widget sizes,
        # enlarge gcombobox widgets
        ltsz <- tag(w,"ltsz")
        if (debug) cat("ltsz=",paste(ltsz,collapse=","),"\n")
        if (is.null(ltsz)) {
            ltw <- tag(w,"len_type_widget")
            ltsz <- size(ltw)
            ltsz[1] <- ltsz[1] * 2L
            size(ltw) <- ltsz
            tag(w,"ltsz") <- ltsz

            sz <- size(tag(w,"tstart_hr"))
            size(tag(w,"tstart_min")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(w,"tstart_sec")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(w,"tend_min")) <- as.integer(c(sz[1]*1.3,sz[2]))
            size(tag(w,"tend_sec")) <- as.integer(c(sz[1]*1.3,sz[2]))
        }
    }

    invisible(w)
}

