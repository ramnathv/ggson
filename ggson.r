#!/usr/bin/Rscript
timeseries = function(data, factor_col) {
    if (is.null(factor_col)) factor_col = '.'

    p = ggplot(data, aes(Time, value)) + geom_point() + geom_smooth()
    if (length(levels(data$variable)) == 1)
        p = p + geom_facets(facets=paste(factor_col, 'test.name', sep='~'))
    else if (factor_col != '.' & length(levels(data$test.name)) == 1)
        p = p + geom_facets(facets=paste('variable', factor_col, sep='~'))
    else if (factor_col != '.')
        p = p + geom_facets(facets=paste(paste('variable+', factor_col, sep=''), 'test.name', sep='~'))
    else
        p = p + geom_facets(facets=paste('variable', 'test.name', sep='~'))
    return (p)
}

open_device  = function(gdesc, fileprefix) {
    file.name = paste(gdesc, fileprefix, '.png', sep='')
    fl = paste(out.location, file.name, sep='/')
    png(fl, height=height, width=width)
}

process.plot = function(gdesc, data, factor_col, fileprefix, Title) {
    f = get(gdesc)
    open_device(gdesc, fileprefix)
    plt = f(data, factor_col)
    plt + opt(title='Title')
    print(plt)
    dev.off()
}

build_condition(x, values sep='==', condition='|') {
    paste(paste(x, values, sep=sep), sep=condition)
}

do.trans = function(d, data) {
    x = data[[d$variable]]
    x = eval(parse(text=d$trans))
}

modify = function(data,group,trans) {
    trans = c(trans, rep('x', times=length(group) - length(trans))
    d = data.frame(variable=group, trans=trans, stringsAsFactors=F) 
    transfrmed = ddply(d, .(variable), .fun=do.trans, data)
}

process.graphs = function(pdesc, factor_col, data) {
    basic = c('Time', factor_col, 'test.name')
    sel = c(basic, pdesc$group)
    groups = build_condition('names(data)',  sel, sep='==', condition='|')
    data = data[, eval(groups)]
    data = modify(data, pdesc$group, pdesc$trans)
    if (!is.null(pdesc$alias) & length(pdesc$alias) == length(pdesc$group))
        names(data) = c(basic, pdesc$alias)
    data = melt(data, id=basic)
    l_ply(pdesc$graphs, process.plot, data, factor_col, pdesc$fileprefix, pdesc$Title)
}

data.processor = function(ddesc, path) {
    files = list.files(path=path, ddesc$pattern)
    data = ldply(files, read.data)
    ddesc$category
    if (!is.null(ddesc$exclude) & length(ddesc$exclude) != 0) {
        excludes = build_condition(paste('data', ddesc$category, sep='$'), ddesc$exclude, sep='!=')
        data = data[eval(excludes),] 
    }
    nms = names(ddesc)
    processors = nms != 'category' & nms != 'pattern' & nms != 'exclude'
    l_aply(ddesc[processor], process.graphs, data)
}
