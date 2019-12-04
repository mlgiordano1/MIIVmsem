#' **Maximizing the log likelihood function for the 2-level saturated model;
#'
#'
#' @param dup     description tbd
#' @param nlevel1 description tbd
#' @param NG      description tbd
#' @param beta0   description tbd
#' @param smatw   description tbd
#' @param ymean   description tbd
#' @param vsmatL1 description tbd
#'
#' @return list
#'
#' @examples yb2007_minq1(a, b, c, d, e, f, g)
#'
#' @export
# ------------------------------------------------------------------------------


yb2007_minq1 <- function(dup, nlevel1, NG, beta0, smatw, ymean, vsmatL1, varNames) {

  # ----------------------
  # saving a few variables
  # ----------------------

  # total sample size
  nt <- sum(nlevel1[,"n"])
  # variables
  p <- nrow(smatw)
  # unique elements
  ps=p*(p+1)/2
  # setting a threshold
  ep=.00001
  # ?
  vsmatw <- vech(smatw)
  # err = error; starting with no error (will switch to err=1 if probs)
  err <- 0




  # --------------------
  # Start Gauss-Newton
  # --------------------
  iitera <- 0
  dt = .05 #(arbitrary- this will start first loop and update in that loop)

  while (dt>ep) {

    # break out of the loop after 50 iterations
    if (iitera>50) {
      err = 1
      print(paste0("iterations = ", iitera))
      break()
    }

    iitera <- iitera+1
    # print(paste0("While loop iteration: ", iitera))

    # subsettting both sigw and sigb vector
    sigwb0 <- beta0[(p+1):(2*ps+p),]
    # mdm1 - takes beta0 and makes mu, sigw, and sigb vectors.
    results_r2 <- yb2007_mdm1(p,beta0, varNames)
    # pulling out resulst and saving them
    mu    <- results_r2["mu"][[1]]
    sigb  <- results_r2["sigb"][[1]]
    sigw  <- results_r2["sigw"][[1]]
    vsigw <- results_r2["vsigw"][[1]]
    # vsigb <- results_r2["vsigb"][[1]]



    siginw=solve(sigw)

    # weight given by normal theory
    weightw <- 0.5 * t(dup) %*% (siginw %x% siginw) %*% dup;

    ssiginj = matrix(nrow = p,  ncol = p,  0)
    ssymj   = matrix(nrow = p,  ncol = 1,  0)
    gt2     = matrix(nrow = ps, ncol = 1,  0);
    gt3     = matrix(nrow = ps, ncol = 1,  0);
    ddljj   = matrix(nrow = ps, ncol = ps, 0);
    ddljw   = matrix(nrow = ps, ncol = ps, 0);
    ddlww   = matrix(nrow = ps, ncol = ps, 0);

    for (j in 1:NG) {

      # label each of these?
      nj      = nlevel1[j,"n"]
      sigj    = sigb + sigw/nj
      vsigj   = vech(sigj)
      siginj  = solve(sigj)
      weightj = 0.5*t(dup) %*% (siginj %x% siginj) %*% dup
      ssiginj = ssiginj+siginj
      ymj     = ymean[j,]
      ssymj   = ssymj + siginj %*% ymj
      cymj    = ymj-mu
      Rj      = cymj %*% t(cymj)
      vrj     = vech(Rj)
      cvrj    = vrj - vsigj
      wcvrj   = weightj %*% cvrj
      gt2     = gt2 + wcvrj/nj
      gt3     = gt3 + wcvrj
      ddljj   = ddljj + weightj
      ddlwj   = ddljw + weightj / nj
      ddlww   = ddlww + weightj / (nj * nj)

    }


    mu1    = solve(ssiginj) %*% ssymj

    gt2    = (nt-NG) * weightw %*% (vsmatw-vsigw) + gt2
    gta    = rbind(gt2,gt3)
    ddljw  = t(ddlwj)
    ddlww  = (nt-NG) * weightw + ddlww

    ddl    = rbind(cbind(ddlww,ddlwj),
                   cbind(ddljw,ddljj))
    stdi   = solve(ddl)
    delt   = stdi %*% gta
    sigwb1 = as.matrix(sigwb0, ncol = 1)+delt
    beta0  = rbind(mu1,sigwb1)
    dt     = sum(delt^2)/sum(sigwb1^2);

  } # end while

  # ---------------------------------------------
  # saving final updated matrices from Above loop
  # ---------------------------------------------
  results_r2 <- yb2007_mdm1(p,beta0, varNames)
  mu         <- results_r2["mu"][[1]]
  sigb       <- results_r2["sigb"][[1]]
  sigw       <- results_r2["sigw"][[1]]
  vsigw      <- results_r2["vsigw"][[1]]
  vsigb      <- results_r2["vsigb"][[1]]


  return(list(stdi  = stdi,
              err   = err,
              beta0 = beta0,
              mu    = mu,
              sigb  = sigb,
              vsigb = vsigb,
              sigw  = sigw,
              vsigw = vsigw))

}
