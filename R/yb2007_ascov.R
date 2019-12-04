#' CalculatiNG the asymptotic covariance matrix of the saturated model;
#' UsiNG only the level2 units as buildiNG blocks;
#'
#' @param beta description tbd
#' @param p description tbd
#' @param dup description tbd
#' @param nlevel1 description tbd
#' @param NG description tbd
#' @param smatw description tbd
#' @param ymean description tbd
#' @param vsmatL1 description tbd
#'
#'
#' @return list of amat and gamma
#'
#' @examples yb2007_ascov(df, 2018, 08)
#'
#' @export
# ------------------------------------------------------------------------------



yb2007_ascov <- function(beta, p,dup,nlevel1,NG,smatw,ymean,vsmatL1, varNames) {
  p  = nrow(smatw)
  ps = p*(p+1)/2
  Nt = sum(nlevel1[,"n"])


  results_r2 <- yb2007_mdm1(p,beta, varNames)
  # pulliNG out resulst and saviNG them in this env
  mu    <- results_r2["mu"][[1]]
  sigb  <- results_r2["sigb"][[1]]
  vsigb <- results_r2["mvsigb"][[1]]
  sigw  <- results_r2["sigw"][[1]]
  vsigw <- results_r2["vsigw"][[1]]


  siginw = solve(sigw)
  weightw = 0.5 * t(dup) %*% (siginw%x%siginw) %*% dup;

  ddluu = matrix(nrow = p,  ncol = p,  0)
  ddljj = matrix(nrow = ps, ncol = ps, 0)
  ddljw = matrix(nrow = ps, ncol = ps, 0)
  ddlww = matrix(nrow = ps, ncol = ps, 0)

  Bmat  = matrix(nrow = (p+2*ps),ncol = (p+2*ps), 0)

  for (jj in 1:NG) {

    nj     = nlevel1[jj, "n"]
    Sigj   = sigb + sigw / nj
    siginj = solve(Sigj)
    vsigj  = vech(Sigj)

    # weight given by normal theory ;
    weightj = 0.5 * t(dup) %*% (siginj%x%siginj) %*% dup

    ddluu  = ddluu + siginj
    ddljj  = ddljj + weightj
    ddljw  = ddljw + weightj / nj
    ddlww  = ddlww + weightj / (nj*nj)


    ymj    = ymean[jj,]    # might need to edit this line
    cymj   = ymj - mu
    gj1    = siginj %*% cymj

    Rj     = cymj %*% t(cymj)
    vrj    = vech(Rj)
    cvrj   = vrj - vsigj
    wcvrj  = weightj %*% cvrj
    gj3    = wcvrj


    wcvswj = weightw %*% (vsmatL1[jj,] - (nj-1) * vsigw) # check this line


    gj2    = wcvrj / nj + wcvswj
    gj     = rbind(gj1, gj2, gj3)
    Bmat   = Bmat + gj %*% t(gj)

  }  # end; *(jj);

  ddlwj = t(ddljw)

  ddlww = (Nt-NG) * weightw + ddlww

  Amat=rbind(cbind(ddluu,                          matrix(nrow = p,ncol = ps,0), matrix(nrow = p, ncol = ps,0)),
             cbind(matrix(nrow = ps, ncol = p,0),  ddlww,                        ddlwj),
             cbind(matrix(nrow = ps, ncol = p,0),  ddljw,                        ddljj)
             )

  # amat and stdi check out with SAS output
  Amat  = Amat / NG
  stdi  = solve(Amat)

  Bmat  = Bmat / NG
  Gamma = stdi %*% Bmat %*% stdi




  return(list(Amat = Amat, Gamma = Gamma))


}
