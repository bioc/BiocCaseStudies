allSets <- function(type=c("neg", "af4"), step=c("subset", "unspec", "de"),
                    varCut=0.5){
  step <- match.arg(step)
  type <- match.arg(type)
  require(ALL)
  require(Category)

  neg <- function(step){
    data(ALL)
    ## susetting
    bcell <- grep("^B", as.character(ALL$BT))
    moltyp <- which(as.character(ALL$mol.biol) %in% c("NEG", "BCR/ABL"))
    ALL_bcrneg <- ALL[, intersect(bcell, moltyp)]
    ALL_bcrneg$mol.biol <- factor(ALL_bcrneg$mol.biol)
    if(step=="subset") return(ALL_bcrneg)
    ## unspecific filtering
    filt_bcrneg <- nsFilter(ALL_bcrneg,require.entrez = TRUE, 
                            require.GOBP = TRUE, remove.dupEntrez = TRUE,
                            var.func = IQR, var.cutoff = varCut)
    ALL_bcrneg <- filt_bcrneg$eset
    if(step=="unspec") return(ALL_bcrneg)
    ## differential expression
    tt_bcrneg = rowttests(ALL_bcrneg, "mol.biol")
    pAdjust = mt.rawp2adjp(tt_bcrneg$p.value, proc="BH")
    ALL_bcrneg <- ALL_bcrneg[pAdjust$adjp[,1]<0.05,]
    return(ALL_bcrneg)
  }
  af4 <- function(step){
    data(ALL)
    ## susetting
    bcell <- grep("^B", as.character(ALL$BT))
    types <- c("ALL1/AF4", "BCR/ABL")
    ALL_af4bcr <- ALL[, intersect(bcell, which(ALL$mol.biol %in% types))]
    ALL_af4bcr$mol.biol <- factor(ALL_af4bcr$mol.biol)
     if(step=="subset") return(ALL_af4bcr)
    ## unspecific filtering
    filt_af4bcr = nsFilter(ALL_af4bcr,require.entrez = TRUE, 
      require.GOBP = TRUE, remove.dupEntrez = FALSE,
      var.func = IQR, var.cutoff = varCut)
    ALL_af4bcr <- filt_af4bcr$eset
    if(step=="unspec") return(ALL_af4bcr)
    ## differential expression
    tt_af4bcr = rowttests(ALL_af4bcr, "mol.biol")
    pAdjust = mt.rawp2adjp(tt_af4bcr$p.value, proc="BH")
    ALL_af4bcr <- ALL_af4bcr[pAdjust$adjp[,1]<0.05,]
    return(ALL_af4bcr)
  }
  return(switch(type, neg=neg(step), af4=af4(step)))
}
