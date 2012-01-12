#!/usr/bin/Rscript
format.difftime <- function (x,format=NULL,...) {
     # Look for a "format" attribute, if null then return basics
     if (is.null(format)){
         if (is.null(attr(x,"format")))
             return(paste(format(unclass(x),...), units(x)))
         else
             format <- rep(attr(x,"format"),length(x))
     } else {
         format <- rep(format,length(x))
     }

     units(x)<-'secs'

     rem <- unclass(x)

     w <- d <- h <- m <- s <- array(0,length(x))
     lunit <- ""
     if (length(grep('%W',format,fixed=TRUE)) > 0 ){
         w     <- rem %/% (7 * 86400)
         rem   <- rem - w * 7 * 86400
         lunit <- "weeks"
     }
     if (length(grep('%d',format,fixed=TRUE)) > 0){
         d     <- rem %/% 86400
         rem   <- rem - d * 86400
         lunit <- "days"
     }
     if (length(grep('%H',format,fixed=TRUE)) > 0){
         h     <- rem %/% 3600
         rem   <- rem - h * 3600
         lunit <- "hours"
     }
     if (length(grep('%M',format,fixed=TRUE)) > 0){
         m     <- rem %/% 60
         rem   <- rem  - m *  60
         lunit <- "mins"
     }
     if (length(grep('%S',format,fixed=TRUE)) > 0){
         s     <- rem
         rem   <- rem - s
         lunit <- "secs"
     }

     # Find precision formatting
     digits <-
         ifelse(is.null(getOption("digits")),
             0,
             as.integer(getOption("digits"))
         )
     digits.secs <-
         ifelse(is.null(getOption("digits.secs")),
             0,
             as.integer(getOption("digits.secs"))
         )

     # Place remainder in last unit we saw.
     # Also set formatting.
     wf <- df <- hf <- mf <- sf <- "%02.f"
     if (lunit != "") {
         if (lunit == "weeks") {
             w <- w + rem / (7 * 86400)
             wf <- paste("%02.",digits,"f",sep='')
         } else if (lunit == "days") {
             d <- d + rem / 86400
             df <- paste("%02.",digits,"f",sep='')
         } else if (lunit == "hours") {
             h <- h + rem / 3600
             hf <- paste("%02.",digits,"f",sep='')
         } else if (lunit == "mins") {
             m <- m + rem / 60
             mf <- paste("%02.",digits,"f",sep='')
         } else if (lunit == "secs") {
             sf <- paste("%02.",digits.secs,"f",sep='')
         }
     }


     # Do substitution
     for (i in 1:length(format)){
         format[i] <- gsub('%W',sprintf(wf,w[i]),format[i],fixed=TRUE)
         format[i] <- gsub('%d',sprintf(df,d[i]),format[i],fixed=TRUE)
         format[i] <- gsub('%H',sprintf(hf,h[i]),format[i],fixed=TRUE)
         format[i] <- gsub('%M',sprintf(mf,m[i]),format[i],fixed=TRUE)
         format[i] <- gsub('%S',sprintf(sf,s[i]),format[i],fixed=TRUE)
     }

     format
}

readFields = function(filename) {
    header = readLines(filename, n=1)
    head = textConnection(header)
    x = read.csv(head, header=F)
    close(head)
    v = as.vector(x[,])
    if (length(v) < 2) stop('Insufficient number of fields')
    headers = c()
    for (d in v) {
        headers = c(headers, as.character(d))
    }
    return (headers)
}

read.sar.file = function(sarfile, test.name=basename(dirname(sarfile)), rundate=Sys.Date(), elapsed=FALSE) {
    sar = read.csv(sarfile)
    sar$times = create_ts(times=sar$Time, date=rundate)
    sar$test.name = factor(test.name)
    if (elapsed) {
        td = sar$times - min(sar$times)
        epoch = '1970-01-01'
        times = as.POSIXct(epoch, tz='', origin=epoch) + as.integer(td)
        sar$times = times
    }
    sar$Time = sar$times
    sar$times = NULL
    names(sar) = c(readFields(sarfile), 'test.name')
    return (sar)
}

findShift = function(values) {
    values = as.numeric(values)
    if (length(values) <= 1) return (0)
    if (length(values) == 2) {
        if (values[1] > values[2]) return (2)
        return (0)
    }
    lower = 1
    upper = length(values)
    while (lower < upper) {
        mid = as.integer((lower + upper) / 2)
        midp1 = mid + 1
        if (values[mid] > values[midp1]) return (midp1)
        if (values[lower] > values[mid])  {
            upper = mid
        }
        else 
            lower = midp1
    }
    return (0)
}

create_ts = function(date, times) {
    date=as.Date(date)
    ts = as.POSIXct(paste(as.character(date), times), format="%Y-%m-%d %I:%M:%S %p")
    shift = findShift(ts)
    if (shift != 0) {
            ndate = date + 1
            nts = as.POSIXct(paste(as.character(ndate), times[shift:length(ts)]), format="%Y-%m-%d %I:%M:%S %p")
            nnts = c(ts[1:shift-1], nts)
            return (nnts)
    }
    return (ts)
}

read.network.packets = function(network) {
    select = c('rxpck.s', 'txpck.s', 'IFACE')
    out = c('ReceviedPackets', 'TransferedPackets', 'IFACE')
    drilled = drill(network, select, out)
    return (drilled)
}

read.network.bytes = function(network) {
    select = c('rxbyt.s', 'txbyt.s', 'IFACE')
    out = c('BytesReceived', 'BytesTransfered', 'IFACE')
    drilled = drill(network, select, out)
    return (drilled)
}


read.network.compressed = function(network) {
    select = c('rxcmp.s', 'txcmp.s', 'IFACE')
    out = c('BytesReceived', 'BytesTransfered', 'IFACE')
    drilled = drill(network, select, out)
    return (drilled)
}

read.nfs.client = function(nfs) {
    #select = c('call.s','retrans.s','read.s','write.s','access.s','getatt.s')
    select = c('call.s','read.s','write.s')
    return (drill(nfs, headers=select, out.headers=select))
}

read.cpuusage = function(cpufile) {
    select = c('X.user', 'X.system', 'CPU')
    out = c('User', 'System', 'CPU')
    drilled = drill(cpufile, select, out)
    drilled$Busy = 100.0 - as.numeric(cpufile$X.idle)
    return (drilled)
}

ccd = function(d) strptime(d, format="%Y%m%d-%H%M%S")

search.sar.files = function(location) {
    sar.files = list()
    sar.files$cpu = list.files(location, pattern='.*\\.sar\\..*cpu$', full.names=TRUE, ignore.case=TRUE)
    date.file = list.files(location, pattern='.*\\.sar\\.date$', full.names=TRUE, ignore.case=TRUE)
    time.file = list.files(location, pattern='^time__.*', full.names=TRUE, ignore.case=TRUE)
    trange = readLines(time.file, n=2)
    sar.files$ttime = format.difftime(ccd(trange[2]) - ccd(trange[1]), format="%H:%M:%S")
    sar.files$rundate = readLines(date.file, n=1)
    sar.files$iostat = list.files(location, pattern='.*\\.sar\\..*iostat*', full.names=TRUE, ignore.case=TRUE)
    sar.files$memory = list.files(location, pattern='.*\\.sar\\..*memory*', full.names=TRUE, ignore.case=TRUE)
    sar.files$block = list.files(location, pattern='.*\\.sar\\..*block*', full.names=TRUE, ignore.case=TRUE)
    sar.files$network = list.files(location, pattern='.*\\.sar\\..*network$', full.names=TRUE, ignore.case=TRUE)
    sar.files$nfs = list.files(location, pattern='.*\\.sar\\..*nfs$', full.names=TRUE, ignore.case=TRUE)
	sar.files$paging = list.files(location, pattern='.*\\.sar\\..*paging$', full.names=TRUE, ignore.case=TRUE)
    return (sar.files)
}

read.sar = function(sar.file, test.name, rundate=Sys.Date(), elapsed=FALSE) {
    sar.data = list()
    sar.data$ttime = sar.file$ttime
    sar.data$block = read.sar.file(sar.file$block, test.name, rundate, elapsed)
    sar.data$nfs = read.sar.file(sar.file$nfs, test.name, rundate, elapsed)
    sar.data$network = read.sar.file(sar.file$network, test.name, rundate, elapsed)
    sar.data$iostat = read.sar.file(sar.file$iostat, test.name, rundate, elapsed)
    sar.data$memory = read.sar.file(sar.file$memory, test.name, rundate, elapsed)
    sar.data$cpu = read.sar.file(sar.file$cpu, test.name, rundate, elapsed)
	sar.data$paging = read.sar.file(sar.file$paging, test.name, rundate, elapsed)
    return (sar.data)
}

drill = function(data, headers, out.headers) {
    select = c('times', headers)
    print("Names")
    print(names(data))
    drilled = data[select]
    names(drilled)  = c('Time', out.headers)
    return (drilled)
}

read.memory.usage = function(cpufile) {
    drilled = drill(cpufile, 'X.memused', 'Memused')
    return (drilled)
}

read.iostat.usage = function(cpufile) {
    select = c('rd_sec.s', 'wr_sec.s', 'DEV')
    out = c('Read', 'Write', 'DEV')
    drilled = drill(cpufile, select, out)
    drilled$Read = as.numeric(drilled$Read)
    drilled$Write = as.numeric(drilled$Write)
    return (drilled)
}

read.iostat.queue = function(iostat) {
    select = c('avgrq.sz', 'DEV')
    out = c('AvgQueSz', 'DEV')
    drilled = drill(iostat, select, out)
    drilled$AvgQueSz = as.numeric(drilled$AvgQueSz)
    return (drilled)
}

read.iostat.svctm = function(iostat) {
    select = c('svctm', 'DEV')
    out = c('ServiceTime', 'DEV')
    drilled = drill(iostat, select, out)
    drilled$ServiceTime = as.numeric(drilled$ServiceTime)
    return (drilled)
}
read.rdwr.requests = function(block.file) {
    select = c('rtps', 'wtps')
    out = c('Read', 'Write')
    drilled = drill(block.file, select, out)
    return (melt(drilled, id=c('Time')))
}

read.block.usage = function(block.file) {
    select = c('bread.s','bwrtn.s', 'test.name')
    out = c('Read', 'Write', 'test.name')
    drilled = drill(block.file, select, out)
    return (melt(drilled, id=c('Time', 'test.name')))
}

read.paging.usage = function(page.file) {
	select = c('pgpgin.s','pgpgout.s')
	out    = c('PageIn','PageOut')
	drilled = drill(page.file, select, out)
    return (melt(drilled, id=c('Time')))
}

read.page.majfault.usage = function(page.file) {
        select = c('majflt.s')
        out    = c('MajorPageFaults')
        drilled = drill(page.file, select, out)
    return (melt(drilled, id=c('Time')))
}
read.page_fault.usage = function(page.file) {
        select = c('fault.s')
        out    = c('PageFault')
        drilled = drill(page.file, select, out)
    return (melt(drilled, id=c('Time')))
}

read.all = function(base.dir, elapsed=F) {
    sarfile = search.sar.files(location=base.dir)
    test.name = basename(base.dir)
    sar = read.sar(sarfile, test.name, sarfile$rundate, elapsed=elapsed)
}
