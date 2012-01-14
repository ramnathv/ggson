#!/usr/bin/Rscript
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
