#' Decomposes sigmaW sigmaB and estimates acov matrices as well
#'
#'
#' @param myData description tbd
#' @param clusterVar description tbd
#' @param varNames description tbd
#'
#' @return sigmaw, sigmab, two acov matrices
#'
#' @examples main(myData, clusterVar, varNames)
#'
#' @export
#' @importFrom dplyr group_by_
#' @importFrom magrittr '%>%'
# ------------------------------------------------------------------------------




main <- function(myData, clusterVar, varNames) {

  # Sort rows by cluster
  myData <- myData[order(myData[clusterVar]),]
  # Sort columns by varNames
  myData <- myData[, c(clusterVar, varNames)]


  # ------------------
  # some preable info
  # ------------------
  nt=nrow(myData)


  #-----------------------------------------------------------
  #counting the number of clusters or the level-2 sample size;
  #-----------------------------------------------------------
  NG  <- length(unique(myData[,clusterVar]))


  # -----------------------------------------
  # generating cluster sizes
  # -----------------------------------------
  nlevel1 <- myData %>% group_by_(clusterVar) %>% count() %>% as.matrix()


  # --------------------------
  # Prepping some matrices
  # --------------------------
  # subset the variables of interest
  ymat <- as.matrix(myData[,varNames])
  p    <- ncol(ymat)
  # print(paste0("number of variables = ", p))

  # number of unique elements in a pxp cov matrix
  ps   <- p*(p+1)/2;

  # compute the average of every row --------------------------
  # this was originally done more in line with SAS code as:
  #    (matrix(nrow = 1, ncol = nt, 1)%*%ymat/nt)
  # but tested with microbenchmarking and the 'colsums' solution was much faster
  ybar <- t(as.matrix(colSums(ymat)/nt))



  # sample covariance matrix
  smat = cov(ymat)

  # vectorize
  vsmat <- vech(smat)

  # putting all "estimates" in one vector. Mu first (with initial estimates at ybar)
  # --followed by sigmaW, followed by SigmaB
  beta00 = matrix(ncol  = 1,
                  byrow = FALSE,
                  data  = c(t(ybar), (vsmat/2), (vsmat/2)))
  rownames(beta00) <- c(paste0(varNames, "mn"), # naming ybar betas
                        paste0("sw.", rownames(vsmat)), # naming sw entries
                        paste0("sb.", rownames(vsmat))) # naming sb entries
  beta0 <- beta00


  # some other housekeeping---------------------------

  # average cluster size
  nbar=nt/NG

  # make duplication matrix (need to review this step)
  dup <- yb2007_DP(p)

  # --------------------------------------------------
  # **the data subroutine prepare for the data
  # and calculate sample mean and covariances;
  # --------------------------------------------------
  r       <- yb2007_processData(ymat = ymat, p = p, nlevel1 = nlevel1, NG = NG)
  smatw   <- r["smatw"][[1]]
  ymean   <- r["ymean"][[1]] # group means
  vsmatL1 <- r["vsmatL1"][[1]]




  # --------------------------
  # Main part of gauss-newton
  # --------------------------
  #
  r    <- yb2007_minq1(dup,nlevel1,NG,beta0, smatw,ymean,vsmatL1, varNames)
  # updating values
  err  <- r["err"][[1]]
  stdi <- r["stdi"][[1]]
  beta0 = r["beta0"][[1]]
  mu    = r["mu"][[1]]
  sigb  = r["sigb"][[1]]
  vsigb = r["vsigw"][[1]]
  sigw  = r["sigw"][[1]]
  vsigw = r["vsigw"][[1]]


  # print("START IF ERR=0")
  if (err == 0) {

    results <- yb2007_ascov(beta0,p,dup, nlevel1,NG,smatw,ymean,vsmatL1, varNames)
    Amat    <- results["Amat"][[1]]
    Gamma   <- results["Gamma"][[1]]

    Gamma[(p+1),(p+1)]

    Gamma11 <- (nbar-1) * Gamma[(p+1):(ps+p),(p+1):(ps+p)]
    Gamma22 <- Gamma[(ps+p+1):(2*ps+p),(ps+p+1):(2*ps+p)]
    Gamma12 <- sqrt(nbar-1) * Gamma[(p+1):(ps+p),(ps+p+1):(2*ps+p)]

    results <- yb2007_mdm1(p, beta0, varNames)
    mu    = results["mu"   ][[1]]
    sigb  = results["sigb" ][[1]]
    vsigb = results["vsigb"][[1]]
    sigw  = results["sigw" ][[1]]
    vsigw = results["vsigw"][[1]]

    permu <- yb2007_permuteGamma(p)

    # print "---------------------------------------------------------------------------------------------";
    nw    = nt-NG
    #print(paste0("the sample size equivalent number N-J for analyzing level-1 alone=", nw))
    # sbigw = sigw
    # vsw   = vsigw
    #print("hat_Sigma_1=")
    #print(sbigw)

    # print "---------------------------------------------------------------------------------------------";
    Gamma11_p=permu %*% Gamma11 %*% t(permu) #*This line is not necessary if the weight matrix needs to be in the order of vech(\hat\Sigma);
    # print("hat_Gamma11=")
    # print(Gamma11)

    # print "---------------------------------------------------------------------------------------------";
    sbigb = NG*sigb/(NG-1)
    # vsb   = vsigb
    # print("hat_Sigma_2=")
    # print(sbigb)

    # print "---------------------------------------------------------------------------------------------";
    Gamma22_p = permu %*% Gamma22 %*% t(permu)# *This line is not necessary if the weight matrix needs to be in the order of vech(\hat\Sigma);
    # print("hat_Gamma22=")
    # print(Gamma22)

    # name the colums of gamma11 and gamma22
    l <- vector(length = ps)
    counter = 1
    for (i in 1:length(varNames)) {
      for (ii in i:length(varNames)) {
        l[counter] <- paste0(varNames[i],"...", varNames[ii])
        counter = counter+1
      }
    }
    rownames(Gamma11) <- l
    colnames(Gamma11) <- l
    rownames(Gamma22) <- l
    colnames(Gamma22) <- l

  }
  return(list(sigma1   = sigw,
              sigma2   = sbigb,
              gamma11   = Gamma11,
              gamma11_p = Gamma11_p,
              gamma22   = Gamma22,
              gamma22_p = Gamma22_p,
              amat      = Amat,
              gamma    = Gamma,
              SS_total = nt,
              SS_l2    = NG,
              SS_l1_nt.ng    = nw))
}



