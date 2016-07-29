
#### Descriptive stats ####

# Table 1: characteristics of sample used for current analysis (w2.s)

summary(as.factor(w2.s$SEX))
summary(as.factor(w2.s$ETHN_DS))
summary(as.factor(w2.s$MDM.r))
summary(as.factor(w2.s$MDPN.r))
summary(as.factor(w2.s$MPAWHO_S))
summary(as.factor(w2.s$XSCLASS))
summary(w2.s$MAGE_C)
sd(w2.s$MAGE_C,na.rm = TRUE)
summary(w2.s$MDPN_SUM)
sd(w2.s$MDPN_SUM,na.rm = TRUE)
summary(w2.s$TEDTOTYR)
sd(w2.s$TEDTOTYR,na.rm = TRUE)
summary(w2.s$MCog.z)
sd(w2.s$MCog.z,na.rm = TRUE)
summary(w2.s$JCog.z)
sd(w2.s$JCog.z,na.rm = TRUE)

# Table 1, characteristics of full whitehall sample (w2.r)

summary(as.factor(w2.r$SEX))
summary(as.factor(w2.r$ETHN_DS))
summary(as.factor(w2.r$MDM.r))
summary(as.factor(w2.r$MDPN.r))
summary(as.factor(w2.r$MPAWHO_S))
summary(as.factor(w2.r$XSCLASS))
summary(w2.r$MAGE_C)
sd(w2.r$MAGE_C,na.rm = TRUE)
summary(w2.r$MDPN_SUM)
sd(w2.r$MDPN_SUM,na.rm = TRUE)
summary(w2.r$TEDTOTYR)
sd(w2.r$TEDTOTYR,na.rm = TRUE)
summary(w2.r$MCog.z)
sd(w2.r$MCog.z,na.rm = TRUE)
summary(w2.r$JCog.z)
sd(w2.r$JCog.z,na.rm = TRUE)

# means of composite cognitive z-score at phases 7 and 9

mean(w2.s$MCog.z,na.rm = TRUE)
mean(w2.s$JCog.z,na.rm = TRUE)

# creation of six diabetes-depressive sypmtom groups (diabetes, prediabetes, normal; low, high symptoms)

NoDiab.NoDep <- w2.s[which(w2.s$MDM.r==1 & w2.s$MDPN.r==0),]
View(NoDiab.NoDep)
NoDiab.Dep   <- w2.s[which(w2.s$MDM.r==1 & w2.s$MDPN.r==1),]
View(NoDiab.Dep)
PrDiab.NoDep <- w2.s[which(w2.s$MDM.r==2 & w2.s$MDPN.r==0),]
View(PrDiab.NoDep)
PrDiab.Dep   <- w2.s[which(w2.s$MDM.r==2 & w2.s$MDPN.r==1),]
View(PrDiab.Dep)
Diab.NoDep   <- w2.s[which(w2.s$MDM.r==3 & w2.s$MDPN.r==0),]
View(Diab.NoDep)
Diab.Dep     <- w2.s[which(w2.s$MDM.r==3 & w2.s$MDPN.r==1),]
View(Diab.Dep)

# means of the six groups

mean(NoDiab.NoDep$JCog.z,na.rm = TRUE)  
mean(NoDiab.Dep$JCog.z,na.rm = TRUE)    
mean(PrDiab.NoDep$JCog.z,na.rm = TRUE)  
mean(PrDiab.Dep$JCog.z,na.rm = TRUE)    
mean(Diab.NoDep$JCog.z,na.rm = TRUE)    
mean(Diab.Dep$JCog.z,na.rm = TRUE)      

# plot of means of six diabetes-depressive symptoms groups (unadjusted)

diab = c("1norm",     "1norm",    "2prediab",  "2prediab",  "3diab",   "3diab") 
dep =  c("0no dep",   "1dep",     "0no dep",   "1dep",      "0no dep", "1dep") 
mean = c(0.1192711,   -0.655472,  -0.03658314, -0.5918611,  -1.462304, -3.53261) 
lCI = c(-0.001994503, -1.0127375, -0.2232579,  -1.20270723, -1.882958, -4.721045)
uCI = c(0.240536614,  -0.2982066,  0.1500916,   0.01898508, -1.041651, -2.344175)
JCog.means = data.frame(dep, diab, mean, lCI, uCI)   
View(JCog.means)

library(ggplot2)

ggplot(JCog.means, aes(x=diab, y=mean, colour=dep, group=dep)) + 
  geom_errorbar(aes(ymin=lCI, ymax=uCI), width=.1, position=position_dodge(0.05)) +
  geom_line(position=position_dodge(0.05)) +
  geom_point(position=position_dodge(0.05),size=3,shape=21, fill="white") +
  xlab("Diabetes status") +
  ylab("Summary cognition score") +
  scale_colour_hue(name="Depression status",    # Legend label, use darker colors
                   breaks=c("0no dep", "1dep"),
                   labels=c("Low symptoms", "High symptoms"),
                   l=40) +                    # Use darker colors, lightness=40
  # ggtitle("Change in Cognition among Exposure Groups (Phase 7 - Phase 9)") +
  theme_bw()+
  scale_x_discrete(breaks=c("1norm", "2prediab", "3diab"),
                   labels=c("Normal", "Prediabetes", "Diabetes")) +
  theme(plot.title = element_text(size = rel(1.75)))+
  theme(axis.title.y = element_text(size = rel(1.5), angle = 90), axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size = rel(1.5)), axis.text.x = element_text(size=12))+
  theme(legend.text = element_text(size = 15))+
  theme(legend.title = element_text(size=15))+ 
  geom_hline(yintercept = 0)




#### Univariate and multivariate regressions ####

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

# Perform simple linear regression for each independent variable, compare with full model

# diabetes status
uni.JCog.MDM.do <- lm(w2.s$JCog.z ~ w2.s$MDM.do)
regression.with.ci(uni.JCog.MDM.do)

# depression status
uni.JCog.MDPNc <- lm(w2.s$JCog.z ~ w2.s$MDPN_SUM)
regression.with.ci(uni.JCog.MDPNc)

# age
uni.JCog.MAGE <- lm(w2.s$JCog.z ~ w2.s$MAGE_C)
regression.with.ci(uni.JCog.MAGE)

# sex
uni.JCog.SEX <- lm(w2.s$JCog.z ~ w2.s$SEX)
regression.with.ci(uni.JCog.SEX)

# ethnicity
uni.JCog.ETH <- lm(w2.s$JCog.z ~ w2.s$ETHN_DS)
regression.with.ci(uni.JCog.ETH)

# years of educ
uni.JCog.TED <- lm(w2.s$JCog.z ~ w2.s$TEDTOTYR)
regression.with.ci(uni.JCog.TED)

# social class
uni.JCog.XSCL <- lm(w2.s$JCog.z ~ w2.s$XSCLASS)
regression.with.ci(uni.JCog.XSCL)

# physical activity
uni.JCog.WHO <- lm(w2.s$JCog.z ~ w2.s$MPAWHO_S)
regression.with.ci(uni.JCog.WHO)

# MCog (cog at wave 7)
uni.JCog.MCog <- lm(w2.s$JCog.z ~ w2.s$MCog.z)
regression.with.ci(uni.JCog.MCog)

# full regression
multi.JCog <- lm(w2.s$JCog.z ~ w2.s$MDM.do + w2.s$MDPN_SUM 
                 + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                 + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                 + w2.s$MCog.z)
multiple.regression.with.ci(multi.JCog)



#### Interaction term ####

# create interaction term

w2.s$MDM.do_MDPNc <- w2.s$MDM.do*w2.s$MDPN_SUM

View(w2.s)
summary(w2.s$MDM.do_MDPNc)

### regression with interaction term

multi.JCog_int.do <- lm(w2.s$JCog.z ~
                          + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                        + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                        + w2.s$MCog.z)
multiple.regression.with.ci(multi.JCog_int.do)

multi.JCog_int.do <- lm(w2.s$JCog.z ~ w2.s$MDM.do + w2.s$MDPN_SUM 
                        + w2.s$MAGE_C + w2.s$SEX + w2.s$ETHN_DS 
                        + w2.s$TEDTOTYR + w2.s$XSCLASS + w2.s$MPAWHO_S 
                        + w2.s$MCog.z + w2.s$MDM.do_MDPNc)
multiple.regression.with.ci(multi.JCog_int.do)

### visualizing interaction

library(ggplot2)

w2.s$MDM.do.f <- !is.na(w2.s$MDM.do)
summary(w2.s$MDM.do.f)

int.df <- w2.s[!is.na(w2.s$MDM.do),]
View(int.df)

int.graph <- ggplot(int.df, aes(y=int.df$JCog.z, x=int.df$MDPN_SUM, 
                                colour=factor(int.df$MDM.do)))
int.graph + stat_smooth(method=lm) +
  xlab("CES-D score") +
  ylab("Composite cognition score") +
  scale_colour_hue(name="Diabetes status",    # Legend label, use darker colors
                   breaks=c("0", "1"),
                   labels=c("Normal", "Diabetes"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Interaction between diabetes and depression score") +
  theme_bw()+
  theme(plot.title = element_text(size = rel(1.75)))+
  theme(axis.title.y = element_text(size = rel(1.5), angle = 90), axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size = rel(1.5)), axis.text.x = element_text(size=12))+
  theme(legend.text = element_text(size = 15))+
  theme(legend.title = element_text(size=15))+ 
  geom_hline(yintercept = 0)