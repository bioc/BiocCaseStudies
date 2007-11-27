## a function to resample 'nrep' times 6*(1:6) samples from each class
## of the ExpressionSet x and plot the number of differentially expressed
## genes as calculated by function 'selfun'.
## This code is used for the differential expression lab in section 8.
resample <- function(x, selfun, groupsize = 6*(1:6), nrep = 25) {
  twoGroups <- split(1:nrow(pData(x)), x$mol.biol)
  stopifnot(length(twoGroups)==2)
  n <- matrix(nrow = nrep, ncol = length(groupsize))
  for (i in seq(along = groupsize)) {
    for (rep in 1:nrep) {
      samp <- c(sample(twoGroups[[1]], groupsize[i]), 
                sample(twoGroups[[2]], groupsize[i]))
      n[rep, i] <- do.call(selfun, args=list(x = x[, samp]))
    }
    cat(".")
  }
  cat("\n")
  mns <- apply(n, 2, mean)
  stds <- apply(n, 2, sd)
  plot(groupsize, mns, pch = 16, col = "#c03030", 
    ylim = c(min(mns-stds) - 5, max(mns + stds) + 5), xlab = "groupsize",
    ylab = "selected no. of diff. exp. genes", main = selfun)
  segments(groupsize, mns - stds, groupsize, mns + stds, col = "red")
  return(invisible(NULL))
}
