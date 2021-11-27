multhistogram <- function (x, beside = TRUE, freq = NULL, probability = !freq,plot.it = TRUE,percentage=TRUE, ...) 
{
    hist.args <- formals(hist.default)
    args <- list(...)
    hargs <- names(args)[names(args) %in% names(hist.args)]
    hist.args[hargs] <- args[hargs]
    barplot.args <- formals(barplot.default)
    bargs <- names(args)[names(args) %in% names(barplot.args)]
    barplot.args[bargs] <- args[bargs]
    barplot.args$beside <- beside
    barplot.args$... <- barplot.args$inside <- NULL
    allhist <- hist(unlist(x), breaks = hist.args$breaks, plot = FALSE)
    if (!"names.arg" %in% bargs) {
        barplot.args$names.arg <- signif(allhist$mids, 2)
    }
    if (is.null(freq)) {
        freq <- if (!missing(probability)) 
            !as.logical(probability)
        else TRUE
    }
    if (freq) 
        comp <- "counts"
    else comp <- "density"
    if(percentage){
        freq=FALSE
    }        
    combhist <- t(sapply(x, function(z) hist(z, breaks = allhist$breaks, 
        plot = FALSE)[[comp]]))
    if(percentage){
        combhist <- combhist*100/apply(combhist,1,sum)
        barplot.args$ylab = "percentage(%)"
    }
    if (plot.it) 
        do.call("barplot", c(list(combhist), barplot.args))
    invisible(list(breaks = allhist$breaks, out = combhist))
}


if(FALSE){

dist_dis<-c()
dist_dis$early <- rnorm(100,10,2)
dist_dis$late <- rbinom(100,20,0.2)
multhistogram(dist_dis,freq=TRUE)

}
