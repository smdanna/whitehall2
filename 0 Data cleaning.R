
# Data cleaning and preparation

w2  # Whitehall II dataset as received from University College London

w2.r <- w2[,c("MDMWHOTO",    # diabetes status    phase 7
              "MDPN_SUM",    # CES-D              phase 7 
              "MAGE_C",      # age                phase 7
              "JAGE_C",      # age                phase 9
              "SEX",         # sex                baseline
              "ETHN_DS",     # ethnicity          baseline
              "TEDTOTYR",    # education          phase 5
              "XSCLASS",     # employment grade   phase 3
              "MPAWHO_S")]   # physical activity  phase 7
View(w2.r)

#### Recode diabetes varible ####

# recode diabetes variable into normal(1), prediabetes(2), type 2 diabetes(3), missing and non-partic(NA)
w2.r$MDM.r <- w2.r$MDMWHOTO
w2.r$MDM.r[w2.r$MDM.r == 3] <- 2
w2.r$MDM.r[w2.r$MDM.r == 4] <- 3
w2.r$MDM.r[w2.r$MDM.r == 5] <- 3
w2.r$MDM.r[w2.r$MDM.r == 6] <- NA
w2.r$MDM.r[w2.r$MDM.r == 7] <- NA

## recode MDM.r into normal and prediabetes(0), type 2 diabetes(1), missing and non-partic(NA)
w2.r$MDM.do <- w2.r$MDM.r
w2.r$MDM.do[w2.r$MDM.r == 1] <- 0
w2.r$MDM.do[w2.r$MDM.r == 2] <- 0
w2.r$MDM.do[w2.r$MDM.r == 3] <- 1

#### Recode depressive symptoms variable ####

# dichotomize CES-D into low [0-15] and high symptoms [16-60]
w2.r$MDPN.r <- ifelse(w2.r$MDPN_SUM < 16, 0, 1)
View(w2.r)

#### Creation of cognition z-scores ####

# phase 7 MAH4   MMEM   MMH   MANIMALS   MSWORDS
# phase 9 JAH4   JMEM   JMH   JANIMALS   JSWORDS

### 1: Compute z-scores for phase 7

# MAH4.z
mean(w2$MAH4,na.rm = TRUE)  # 43.55407
sd(w2$MAH4, na.rm = TRUE)   # 11.32621

w2.r$MAH4.z <- (w2$MAH4-43.55407)/11.32621
mean(w2.r$MAH4.z,na.rm = TRUE)
sd(w2.r$MAH4.z,na.rm = TRUE)
View(w2.r$MAH4.z)

# MMEM.z
w2.r$MMEM.z <- (w2$MMEM-(mean(w2$MMEM,na.rm = TRUE)))/sd(w2$MMEM,na.rm = TRUE)
mean(w2.r$MMEM.z,na.rm = TRUE)
sd(w2.r$MMEM.z,na.rm = TRUE)

# MMH.z
w2.r$MMH.z <- (w2$MMH-(mean(w2$MMH,na.rm = TRUE)))/sd(w2$MMH,na.rm = TRUE)
mean(w2.r$MMH.z,na.rm = TRUE)
sd(w2.r$MMH.z,na.rm = TRUE)

# MANIMALS.z
w2.r$MANIMALS.z <- (w2$MANIMALS-(mean(w2$MANIMALS,na.rm = TRUE)))/sd(w2$MANIMALS,na.rm = TRUE)
mean(w2.r$MANIMALS.z,na.rm = TRUE)
sd(w2.r$MANIMALS.z,na.rm = TRUE)

# MSWORDS.z
w2.r$MSWORDS.z <- (w2$MSWORDS-(mean(w2$MSWORDS,na.rm = TRUE)))/sd(w2$MSWORDS,na.rm = TRUE)
mean(w2.r$MSWORDS.z,na.rm = TRUE)
sd(w2.r$MSWORDS.z,na.rm = TRUE)

### 2: Compute a phase 7 summary score 

w2.r$MCog.z <- w2.r$MAH4.z + w2.r$MMEM.z + w2.r$MMH.z + w2.r$MANIMALS.z + w2.r$MSWORDS.z
View(w2.r)

### 3: Compute z-scores for phase 9 using phase 7 means and SD

# JAH4
w2.r$JAH4.z <- (w2$JAH4-(mean(w2$MAH4,na.rm = TRUE)))/sd(w2$MAH4,na.rm = TRUE)
mean(w2.r$JAH4.z,na.rm = TRUE)
sd(w2.r$JAH4.z,na.rm = TRUE)

# JMEM
w2.r$JMEM.z <- (as.numeric(w2$JMEM)-(mean(w2$MMEM,na.rm = TRUE)))/sd(w2$MMEM,na.rm = TRUE)
mean(w2.r$JMEM.z,na.rm = TRUE)
sd(w2.r$JMEM.z,na.rm = TRUE)

# JMH
w2.r$JMH.z <- (as.numeric(w2$JMH)-(mean(w2$MMH,na.rm = TRUE)))/sd(w2$MMH,na.rm = TRUE)
mean(w2.r$JMH.z,na.rm = TRUE)
sd(w2.r$JMH.z,na.rm = TRUE)

# JANIMALS   
w2.r$JANIMALS.z <- (as.numeric(w2$JANIMALS)-(mean(w2$MANIMALS,na.rm = TRUE)))/sd(w2$MANIMALS,na.rm = TRUE)
mean(w2.r$JANIMALS.z,na.rm = TRUE)
sd(w2.r$JANIMALS.z,na.rm = TRUE)

# JSWORDS
w2.r$JSWORDS.z <- (as.numeric(w2$JSWORDS)-(mean(w2$MSWORDS,na.rm = TRUE)))/sd(w2$MSWORDS,na.rm = TRUE)
mean(w2.r$JSWORDS.z,na.rm = TRUE)
sd(w2.r$JSWORDS.z,na.rm = TRUE)

# If there is change in cognition, JAH4.z will be different from 0

### 4: Compute a phase 9 summary score

w2.r$JCog.z <- w2.r$JAH4.z + w2.r$JMEM.z + w2.r$JMH.z + w2.r$JANIMALS.z + w2.r$JSWORDS.z
View(w2.r)                    

#### Creation of dataframe using complete observations of JCog.z ####

w2.s <- w2.r[ which(!is.na(w2.r$JCog.z)), ]

View(w2.s)
View(w2.r)

