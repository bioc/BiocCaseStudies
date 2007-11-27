 ## split long strings
insertLinefeed <- function(str, n=57, tex=TRUE){
    ss <- strsplit(str, " ")
    out <- NULL
    for(i in seq(along=ss)){
        for(j in seq(along=ss[[i]])){
            ins <- ss[[i]][j]
            if(i==1 && j==1){
                out <- c(out, substr(ins,0,n-6))
                ins <- substr(ins,n-5,nchar(ins))
            }
            while(nchar(ins)>n){
                out <- c(out, substr(ins,0,n))
                ins <- substr(ins,n+1,nchar(ins))
            }
        }
        ss[[i]] <- c(out, ins)
    }
    if(tex)
        ret <- paste(unlist(ss), "|", collapse="\\\\\n\\verb|", sep="")
    else
        ret <- paste(unlist(ss), collapse="\n", sep="")
    return(substr(ret, 0, nchar(ret)-1))
}

mySessionInfo <- function(ref=TRUE){
                     
    ## force floats and wrap in minpage
    cat("\\subsubsection*{}")
    cat("\\FloatBarrier\n")
    cat("\\hrulefill\\\\\n")
    #cat("\\begin{minipage}{\\textwidth}\n")
    cat("The version number of R and the packages and their versions",
        "that were used to generate this document are listed below:\n")
    se <- sessionInfo()
    se$locale <- insertLinefeed(se$locale)
    cat(myToLatex(se))
    if(ref){
        cat("\\bibliographystyle{abbrvnat}\n")
        cat("\\bibliography{../Common/useR}\n")
    }
    #cat("\\end{minipage}\n")
}


myToLatex <- function (object, ...) 
{
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("\\begin{itemize}", paste("  \\item ",
                                     object$R.version$version.string, 
                                     ", \\\\\\verb|",object$R.version$platform,
                                     "|", sep = ""), 
           paste("  \\item Locale: \\verb|", object$locale, 
                 "|", sep = ""), strwrap(paste("\\item Base packages:", 
                                               paste(sort(object$basePkgs),
                                                     collapse = ", ")), 
                                         indent = 2, exdent = 4))
    if (length(opkgver)) {
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z, strwrap(paste("  \\item Other packages: ", 
                                paste(names(opkgver), opkgver, sep = "~",
                                      collapse = ", ")), 
                          indent = 2, exdent = 4))
        }
    if (length(nspkgver)) {
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z, strwrap(paste("  \\item Loaded via a namespace ",
                                "(and not attached): ", 
                                paste(names(nspkgver), nspkgver,
                                      sep = "~", collapse = ", ")), 
                          indent = 2, exdent = 4))
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "Latex"
    z
}

