#!/usr/bin/Rscript
library('stringr')
library('plyr')
library('ggplot2')
library('reshape')
source('reader.sar.r')
generate.facet = function(data, factor_col, reverse=F, mncr=4) {
    if (is.null(factor_col)) factor_col = '.'
    if (length(levels(data$variable)) == 1)
        facets = paste(factor_col, 'test.name', sep='~')
    else if (factor_col != '.' & length(levels(data$test.name)) == 1)
        facets = paste('variable', factor_col, sep='~')
    else if (factor_col != '.')
        facets = paste('variable', paste('test.name+', factor_col, sep=''), sep='~')
    else
        facets = paste('variable', 'test.name', sep='~')

    parts = str_split(facets, '~')[[1]]
    if (reverse) {
        facets = paste(parts[2], parts[1], sep='~')
    }

    if (length(grep(parts, pattern='^\\.$')) == 0) 
        return (facet_grid(facets))

    parts = str_split(facets, '~')[[1]]
    if (parts[1] == '.') {
        mncr = as.integer(length(levels(data[[parts[2]]])) / 2)
        f = facet_wrap(parts[2], ncol=mncr)
    } else {
        mncr = as.integer(length(levels(data[[parts[1]]])) / 2)
        f = facet_wrap(parts[1], nrow=mncr)
    }
}

lesser.first = function(data, ftrs) {
    l1 = levels(data[[ftrs[1]]])
    l2 = levels(data[[ftrs[2]]])
    if (length(l2) == 1 && length(l1) == 1) {
        return (NULL)
    }
    if (length(l1) == 1) return (c('.', ftrs[2]))
    if (length(l2) == 1) return (c('.', ftrs[1]))
    if (length(l1) > length(l2)) return (ftrs[c(2, 1)])
    ftrs
}

gen.fcts = function(data, xfactor) {
    x = logical(0)
    for (i in 1:ncol(data))
        x = c(x, is.factor(data[, i]))
    ftrs  = names(data)[x]
    ftrs = ftrs[ ftrs != xfactor ]
    if (length(ftrs) == 1) ftrs = c('.', ftrs)
    else ftrs = lesser.first(data, ftrs)
    if (is.null(ftrs)) return (NULL)
    facets = paste(ftrs, collapse='~')
    parts = str_split(facets, '~')[[1]]
    if (length(grep(parts, pattern='^\\.$')) == 0)
        return (facet_grid(facets))

    parts = str_split(facets, '~')[[1]]
    if (parts[1] == '.') {
        mncr = as.integer(length(levels(data[[parts[2]]])) / 2)
        if (length(levels(data[[parts[2]]])) == 1)  return (NULL)
        if (mncr == 1) mncr = 2
        f = facet_wrap(parts[2], ncol=mncr)
    } else {
        mncr = as.integer(length(levels(data[[parts[1]]])) / 2)
        if (length(levels(data[[parts[1]]])) == 1)  return (NULL)
        if (mncr == 1) mncr = 2
        f = facet_wrap(parts[1], nrow=mncr)
    }
    return (f)
}

boxplt = function(data, pdesc) {
    if (is.null(pdesc$category)) {
        pdesc$category = 'test.name'
    }

    p = ggplot(data) + geom_boxplot(aes_string(x=pdesc$category, y='value'))
    if (is.facetable(data, pdesc$category))
        p = p + gen.fcts(data, pdesc$category)
    if (!is.null(pdesc$yscale)) {
        scale = paste('scale_y', pdesc$yscale, sep='_')
        f = get(scale)
        p = p + f()
    }
    
    return (p)

}

boxpltflp = function(data, pdesc) {
    if (is.null(pdesc$category))
        pdesc$category = 'test.name'
    p = ggplot(data) + geom_boxplot(aes_string(x=pdesc$category, y='value'))
    if (is.facetable(data, pdesc$category))
        p = p + gen.fcts(data, pdesc$category)
}

jit = function(data, pdesc) {
    if (is.null(pdesc$category)) {
        pdesc$category = 'test.name'
    }

    if (is.null(pdesc$alpha)) pdesc$alpha = 1
    p = ggplot(data) + geom_jitter(aes_string(x=pdesc$category, y='value'), alpha=I(1/pdesc$alpha))
    if (is.facetable(data, pdesc$category))
        p = p + gen.fcts(data, pdesc$category) 
    if (!is.null(pdesc$yscale)) {
        scale = paste('scale_y', pdesc$yscale, sep='_')
        f = get(scale)
        p = p + f()
    }
    return (p)
}

is.facetable = function(data, category) {
    length(levels(data[['variable']])) > 1 ||length(levels(data[[category]])) > 1 || length(levels(data[['test.name']])) > 1
}

open_device  = function(gdesc, fileprefix, height, width) {
    if (is.null(height)) height = 600
    if (is.null(width)) width = 800
    file.name = paste(fileprefix, gdesc, '.png', sep='')
    fl = paste(out.location, file.name, sep='/')
    png(fl, height=height, width=width)
}

mysummary = function(x, ...) {
    r = summary(as.numeric(x), ...)
    if (length(grep("NA's", names(r), ignore.case=T)) == 0)
        r[["NA's"]] = 0
    r2 = as.data.frame(r) 
    names(r2) = names(r)
    return (r2)
}

process.plot = function(gdesc, pdesc) {
    f = get(gdesc)
    open_device(gdesc, pdesc$fileprefix, pdesc$height, pdesc$width)
    plt = f(pdesc$data, pdesc)
    if (!is.null(pdesc$Title))
        plt = plt + opts(title=pdesc$Title)
    print(plt)
    dev.off()
}

build_condition = function(x, values, sep='==', condition='|') {
    paste(paste(x, values, sep=sep), collapse=condition)
}

process.graphs = function(pdesc, ddesc) {
    basic = c('Time', ddesc$category, 'test.name')
    sel = c(basic, pdesc$group)
    pdesc$data = ddesc$data[, sel]
    
    pdesc$data = modify(pdesc$data, pdesc$group, pdesc$trans)
    if (!is.null(pdesc$alias)) {
        if (length(pdesc$alias) < length(pdesc$group))
            pdesc$alias = c(pdesc$alias, pdesc$group[length(pdesc$alias)+1:length(pdesc$group)])
        names(pdesc$data) = c(basic, pdesc$alias[!is.na(pdesc$alias)])
    }
    pdesc$data = melt(pdesc$data, id=basic)
    pdesc$category = ddesc$category
    l_ply(pdesc$graphs, process.plot, .progress='text', pdesc)
}

jitbox = function(data, pdesc) {

    if (is.null(pdesc$category)) {
        pdesc$category = 'test.name'
    }

    if (is.null(pdesc$alpha)) pdesc$alpha = 1
    p = ggplot(data, aes_string(x=pdesc$category, y='value')) + geom_jitter(alpha=I(1/pdesc$alpha)) + geom_boxplot() 
    if (is.facetable(data, pdesc$category)) {
            facet = gen.fcts(data, pdesc$category)
            p = p + facet + opts(axis.text.x=theme_text(angle=90))
    }
    if (!is.null(pdesc$yscale)) {
        scale = paste('scale_y', pdesc$yscale, sep='_')
        f = get(scale)
        p = p + f()
    }
    return (p)
}

boxjit = function(data, pdesc) {
    if (is.null(pdesc$category)) {
        pdesc$category = 'test.name'
    }

    if (is.null(pdesc$alpha)) pdesc$alpha = 1
    p = ggplot(data, aes_string(x=pdesc$category, y='value')) + geom_boxplot() + geom_jitter(alpha=I(1/pdesc$alpha)) 
    if (is.facetable(data, pdesc$category)) {
            facet = gen.fcts(data, pdesc$category)
            p = p + facet + opts(axis.text.x=theme_text(angle=90))
    }
    if (!is.null(pdesc$yscale)) {
        scale = paste('scale_y', pdesc$yscale, sep='_')
        f = get(scale)
        p = p + f()
    }
    return (p)
}

timeseries = function(data, pdesc) {
    if (is.null(pdesc$alpha)) pdesc$alpha = 1
    if (is.null(pdesc$category)) pdesc$category = '.'
    if (!is.null(pdesc$color))
        p = ggplot(data, aes(Time, value)) + geom_point(aes_string(color=pdesc$color), alpha=I(1/pdesc$alpha)) + geom_smooth(aes_string(color=pdesc$color))
    else
        p = ggplot(data, aes(Time, value)) + geom_point(alpha=I(1/pdesc$alpha)) + geom_smooth()
    if (length(levels(data[['variable']])) > 1 ||length(levels(data[[pdesc$category]])) > 1 || length(levels(data[['test.name']])) > 1)
        p = p + generate.facet(data, pdesc$category) 
    p + scale_x_datetime(format="%H:%M:%S") + opts(axis.text.x=theme_text(angle=90))
}


modify = function(data,group,trans) {
    trans = c(trans, rep('x', times=length(group) - length(trans)))
    d = data.frame(variable=group, trans=trans, stringsAsFactors=F) 
    #transfrmed = ddply(d, .(variable), .fun=do.trans, data)
    for (i in 1:nrow(d)) {
        x = data[[d[i,1]]]
        data[[as.character(d[i, 1])]] = eval(parse(text=as.character(d[i, 2])))
    }
    return (data)
}

read.data = function(file) {
    test.name = basename(dirname(file))
    data = read.sar.file(file, elapsed=T)
    data$test.name = factor(test.name)
    return (data)
}

data.processor = function(ddesc, path) {
    files = list.files(path=path, ddesc$pattern, recursive=T, full.names=T)
    ddesc$data = ldply(files, read.data)
    if (!is.null(ddesc$exclude) & length(ddesc$exclude) != 0) {
        ddesc$exclude = paste("'", ddesc$exclude, "'", sep='')
        excludes = build_condition(paste('ddesc$data', ddesc$category, sep='$'), ddesc$exclude, sep='!=', condition='&')
        ddesc$data = ddesc$data[eval(parse(text=excludes)),] 
        ddesc$data[[ddesc$category]] = factor(ddesc$data[[ddesc$category]])
    }
    nms = names(ddesc)
    processors = nms != 'category' & nms != 'pattern' & nms != 'exclude' & nms != 'data'
    sq = 1:length(ddesc)
    l_ply(ddesc[ sq[processors] ], process.graphs, .progress='text', ddesc)
}

