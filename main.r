#!/usr/bin/Rscript
library("optparse")
library("plyr")
library("rjson")
source("ggson.r")

option_list <- list(
    make_option(c("-s", "--sourced"), help="Source directory to find sar output files",
        type='character', action="store"),
    make_option(c("-j", "--json"), help="JSON file describing plots",
        type='character', action="store"),
    make_option(c("-d", "--destination"), help="Destination location to store plots",
        type='character', action="store")
                )
parser = OptionParser(option_list=option_list, usage="%prog -s /path/to/sar/csv/outpur -j plot/spec.json -d /path/to/store/plots")
arguments <- parse_args(parser, positional_arguments=T)
opt = arguments$options
if (length(arguments$options) < 3) {
    cat("Insufficient number of arguments")
    print_help(parser)
    stop()
}

if (!file.exists(opt$source)) {
    stop('specified source directory doesn\'t exists')
}
if (!file.exists(opt$json)) {
    stop('specified json doesn\'t exists')
}

dir.create(opt$destination, recursive=T, showWarnings=F)

desc = fromJSON(file=opt$json)
out.location = opt$destination
l_ply(desc, data.processor, opt$sourced)

