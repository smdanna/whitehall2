
#### Sensitivity analysis 1: dichotomized depressive score ####

# Recode depressive symptoms as binary: low dep symptoms/CESD<16(0), high dep symptoms/CESD>=16(1)

w2.r$MDPN.r <- ifelse(w2.r$MDPN_SUM < 16, 0, 1)
View(w2.r)

### Univariate and multivariate regressions

# Function created by Dr. Lawrence Joseph
# http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/EPIB-621/regression.with.ci.txt

regression.with.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from an lm                   #
  #  (linear model) command in R and provides not                #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for intercept and slope.          #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  t.quantile <- qt(1-(1-level)/2, df=regress.out$df)
  intercept.ci <- summary(regress.out)$coefficients[1] + c(-1, 1) * t.quantile * summary(regress.out)$coefficients[3]
  slope.ci <- summary(regress.out)$coefficients[2] + c(-1, 1) * t.quantile * summary(regress.out)$coefficients[4]
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci, slope.ci = slope.ci)
  return(output)
}

# Function created by Dr. Lawrence Joseph
# http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/EPIB-621/multiple.regression.with.ci.txt

multiple.regression.with.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from an lm                   #
  #  (linear model) command in R and provides not                #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for intercept and slope.          #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  t.quantile <- qt(1-(1-level)/2, df=regress.out$df)
  number.vars <- length(regress.out$coefficients)
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * t.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci)
  return(output)
}

# Univariate linear regression with binary depressive score

uni.JCog.MDPN <- lm(w2.s$JCog.z ~ w2.s$MDPN.r)
regression.with.ci(uni.JCog.MDPN)

# Multivarite linear regression with binary depressive score

multi.JCog <- lm(w2.s$JCog.z ~ w2.s$MDM.do + w2.s$MDPN.r 
                 + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                 + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                 + w2.s$MCog.z)
multiple.regression.with.ci(multi.JCog)


# Multivariate linear regression with diabetes status * binary depressive score interaction term

# Create interaction term

w2.s$MDM.do_MDPN <- w2.s$MDM.do*w2.s$MDPN.r


multi.JCog_int.do <- lm(w2.s$JCog.z ~ w2.s$MDM.do + w2.s$MDPN.r 
                        + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                        + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                        + w2.s$MCog.z + w2.s$MDM.do_MDPN)
multiple.regression.with.ci(multi.JCog_int.do)




#### Sensitivity analysis 2: Expanded diabetes group ####

# Recode MDM.r into normal(0), prediabetes and diabetes(1), missing and non-partic(NA)
w2.r$MDM.pd <- w2.r$MDM.r
w2.r$MDM.pd[w2.r$MDM.r == 1] <- 0
w2.r$MDM.pd[w2.r$MDM.r == 2] <- 1
w2.r$MDM.pd[w2.r$MDM.r == 3] <- 1


### Univariate and multivariate regressions

# Univariate linear regression with expanded diabetes group 

uni.JCog.MDM.pd <- lm(w2.s$JCog.z ~ w2.s$MDM.pd)
regression.with.ci(uni.JCog.MDM.pd)

# Multivariate linear regression with expanded diabetes group
multi.JCog <- lm(w2.s$JCog.z ~ w2.s$MDM.pd + w2.s$MDPN_SUM 
                 + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                 + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                 + w2.s$MCog.z)
multiple.regression.with.ci(multi.JCog)

### Multivariate linear regression with expanded diabetes * depressive symptoms interaction term

# Create interaction term: expanded diabetes group * continuous depressive symptoms

w2.s$MDM.pd_MDPNc <- w2.s$MDM.pd*w2.s$MDPN_SUM

multi.JCog_int.pd <- lm(w2.s$JCog.z ~ w2.s$MDM.pd + w2.s$MDPN_SUM 
                        + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                        + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                        + w2.s$MCog.z + w2.s$MDM.pd_MDPNc)
multiple.regression.with.ci(multi.JCog_int.pd)

