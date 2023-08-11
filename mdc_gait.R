## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ICC)
library(gamlss)
library(table1)
library(kableExtra)
library(ggpubr)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Primary dataset
dat <- read.table(file = "data/MotorScores.tab", fill = TRUE, header = TRUE, sep = "\t")

# Height/weight data
height.weight <- read.table(file = "data/HeightWeight.tab", fill = TRUE, header = TRUE, sep = "\t")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make assessment/reassessment wide instead of long
dat.wide <- dat %>% pivot_wider(id_cols = login,
                                values_from = c(LO4MT_UsualPaceScore),
                                names_from = c(assmnt),
                                names_prefix = "speed.")


# Harvest BMI values
height.weight %<>% mutate(bmi = 703 * weight / height^2)
height.weight <- height.weight %>% dplyr::select(login, bmi)



# Harvest demographic data
dat.dem <- dat %>% dplyr::select(login, ethncty: Race_White, eth_group, lang) %>%
                   group_by(login) %>%
                   slice(1L)

dat.dem %<>% mutate(sex = as.factor(case_when(gender == 2 ~ "Female",
                                              gender == 1 ~ "Male")))

# Merge in height/weight and demographic data into wide dataset
dat.dem <- left_join(dat.dem, height.weight, by = "login")


# Merge in demographic data to wide dataframe
dat.wide <- left_join(dat.wide, dat.dem, by = "login")


# Create change variables
dat.wide %<>% mutate(change = speed.2 - speed.1)


# Employ selection criteria

dat.wide <- dat.wide %>% filter(!is.na(change))
dat.wide <- dat.wide %>% filter(age >=18)




## -------------------------------------------------------------------------------------------------------------------------------------------------------------

table1(~ age + sex + bmi + speed.1 + change,
       caption = "Table 1. Demographics of participants in usual gait speed dataset",
       data = dat.wide)

# histogram
meanspeed<-mean(dat.wide$speed.1)
his.line<-data.frame(mean=c(mean(dat.wide$speed.1),mean(dat.wide$speed.1)),
                     count = c(0,20))

his.speed<-ggplot(data = dat.wide)+
  geom_histogram(aes(x = speed.1), fill = "lightblue", color = "black")+
  geom_line(aes(x=mean, y=count), data=his.line, linetype=2, size=2, color="black")+
  
  #ggtitle(label = "Distribution of change scores")+
  xlab(label = "Walking Speed (m/s); Initial Test")+
  ylab(label = "Count")+
  annotate(geom = 'text', x = meanspeed, y = Inf, hjust = 1.1, vjust = 3,
           label = paste("Mean: ",
                         round(meanspeed, digits = 2),
                         " m/s",
                         sep = ""), color="black")+
  
  theme_bw()

his.speed
## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(dat.wide$change)




# histogram
ggplot(data = dat.wide)+
  geom_histogram(aes(x = change), fill = "dodgerblue", color = "black")+
  
  ggtitle(label = "Distribution of change scores")+
  
  theme_bw()


# Relationship between baseline speed and change
ggplot(data = dat.wide, aes(x = speed.1, y = change))+
  geom_point()+
  
  geom_hline(yintercept = 0)+
  
  ggtitle(label = "Usual gait speed change by speed.1")+
  
  theme_bw()
  
                  


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Pivot dat.wide to long
dat.icc <- pivot_longer(dat.wide,
                        cols = c(speed.1, speed.2),
                        values_to = "speed")

# Calculate ICC
icc <- ICC::ICCbare(x = login, y = speed, data = dat.icc)

# Calculate SEM with ICC
sem <- sd(dat.icc$speed) * sqrt(1 - icc)

#calculate MDC
mdc90 <- sem * 1.645  * sqrt(2)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Calculate overall precision
mdc.prec <- mdc90 * 2



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -- Examine Improvement Threshold ---

# Calculate overall coverage for improvement
ncount.mdc.imp= nrow(subset(dat.wide, change >= mdc90))
tcount.mdc.imp= nrow(dat.wide)
mdc.imp<-binom.test(ncount.mdc.imp, tcount.mdc.imp, p=0.05)



# Calculate coverage for improvement with baseline values below median
dat.1 <- dat.wide %>% filter(speed.1 < median(dat.wide$speed.1))

ncount.mdc.imp1= nrow(subset(dat.1, change >= mdc90))
tcount.mdc.imp1= nrow(dat.1)
mdc.imp1<-binom.test(ncount.mdc.imp1, tcount.mdc.imp1, p=0.05)


# Calculate coverage for improvement with baseline values above median
dat.2 <- dat.wide %>% filter(speed.1 > median(dat.wide$speed.1))

ncount.mdc.imp2= nrow(subset(dat.2, change >= mdc90))
tcount.mdc.imp2= nrow(dat.2)
mdc.imp2<-binom.test(ncount.mdc.imp2, tcount.mdc.imp2, p=0.05)




# --- Examine Decline Threshold ---

# Calculate overall coverage for improvement

ncount.mdc.dec= nrow(subset(dat.wide, change <= -mdc90))
tcount.mdc.dec= nrow(dat.wide)
mdc.dec<-binom.test(ncount.mdc.dec, tcount.mdc.dec, p=0.05)




# Calculate coverage for improvement with baseline values below median
dat.1 <- dat.wide %>% filter(speed.1 < median(dat.wide$speed.1))

ncount.mdc.dec1= nrow(subset(dat.1, change <= -mdc90))
tcount.mdc.dec1= nrow(dat.1)
mdc.dec1<-binom.test(ncount.mdc.dec1, tcount.mdc.dec1, p=0.05)



# Calculate coverage for improvement with baseline values above median
dat.2 <- dat.wide %>% filter(speed.1 > median(dat.wide$speed.1))

ncount.mdc.dec2= nrow(subset(dat.2, change <= -mdc90))
tcount.mdc.dec2= nrow(dat.2)
mdc.dec2<-binom.test(ncount.mdc.dec2, tcount.mdc.dec2, p=0.05)





## -------------------------------------------------------------------------------------------------------------------------------------------------------------
mdc.plot <-
  
ggplot(data = dat.wide, aes(x = speed.1, y = change))+
  
  
  geom_point()+
  
  geom_ribbon(aes(ymin = mdc90, ymax = Inf, fill = "detectable improvement"), alpha = 0.2)+
  geom_ribbon(aes(ymax = -mdc90, ymin = -Inf, fill = "detectable decline"), alpha = 0.2)+
  
  
  #geom_hline(yintercept = 0, linetype = 3, size = 1)+
  geom_line(aes(y = mdc90, linetype = "detectable change threshold"), size = 0.7)+
  geom_line(aes(y = -mdc90, linetype = "detectable change threshold"), size = 0.7)+
  
  annotate(geom = 'text', x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Mean precision: ",
                         round(mdc.prec, digits = 2),
                         " m/s",
                         sep = ""))+
  
  xlab(label = "Walking Speed (m/s); Initial Test")+
  ylab(label = "Test-Retest Difference (m/s)")+
  
  
  coord_cartesian(xlim = c(min(dat.wide$speed.1), max(dat.wide$speed.1)),
                  ylim = c(min(dat.wide$change - 0.1), max(dat.wide$change + 0.1)),
                  expand = FALSE)+
 
  scale_linetype_manual(limits = "detectable change threshold",
                        values = 2,
                        name = "")+
  
  scale_fill_manual(limits = c("detectable improvement", "detectable decline"), 
                    values = c("green3", "red3"),
                    name = "")+
  
  
  
  ggtitle(label = "MDC Trad")+
  
  theme_bw()



# view plot
mdc.plot



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create model
m.slr <- lm(change ~ speed.1, dat = dat.wide)
summary(m.slr)

# examine residuals

plot(m.slr)

ggplot()+
  geom_histogram(aes(x = residuals(m.slr)), fill = "dodgerblue", color = "black")+
  ggtitle(label = "Distribution of SLR residuals")+
  theme_bw()



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Create predictions
slrmin<-min(dat.wide$speed.1)
slrmax<-max(dat.wide$speed.1)
slr.data<-data.frame(speed.1=seq(from=slrmin, to=slrmax, by=0.01))

slr.pred1 <- data.frame(predict(m.slr, newdata = slr.data, interval = "predict", level = 0.90))
slr.pred1$prec<-slr.pred1$upr-slr.pred1$lwr
slr_mn_prec<-mean(slr.pred1$prec)
sd(slr.pred1$prec)
slr.pred1$speed.1 <- slr.data$speed.1

#write.csv(slr.pred1, file="slr_080123.csv")



slr.pred1$speed.1 <- slr.data$speed.1



slr.pred <- data.frame(predict(m.slr, newdata = dat.wide, interval = "predict", level = 0.90))
slr.pred$login <- dat.wide$login

# Merge back in
dat.wide <- left_join(dat.wide, slr.pred, by = "login")

dat.wide %<>% rename(slr.pred = fit,
                     slr.lwr = lwr,
                     slr.upr = upr) %>%
              mutate(slr.prec = slr.upr - slr.lwr)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Overall precision
slr.prec <- mean(dat.wide$slr.upr - dat.wide$slr.lwr, na.rm = TRUE)

## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Overall coverage for improvement

ncount.slr.imp= nrow(subset(dat.wide, change >= slr.upr))
tcount.slr.imp= nrow(dat.wide)
slr.imp<-binom.test(ncount.slr.imp, tcount.slr.imp, p=0.05)


# Coverage for improvement - Values below median
dat.1 <- dat.wide %>% filter(speed.1 < median(dat.wide$speed.1))

ncount.slr.imp1= nrow(subset(dat.1, change >= slr.upr))
tcount.slr.imp1= nrow(dat.1)
slr.imp1<-binom.test(ncount.slr.imp1, tcount.slr.imp1, p=0.05)



# Coverage for improvement - Values above median
dat.2 <- dat.wide %>% filter(speed.1 > median(dat.wide$speed.1))

ncount.slr.imp2= nrow(subset(dat.2, change >= slr.upr))
tcount.slr.imp2= nrow(dat.2)
slr.imp2<-binom.test(ncount.slr.imp2, tcount.slr.imp2, p=0.05)




# Overall coverage for decline
ncount.slr.dec= nrow(subset(dat.wide, change <= slr.lwr))
tcount.slr.dec= nrow(dat.wide)
slr.dec<-binom.test(ncount.slr.dec, tcount.slr.dec, p=0.05)




# Coverage for decline - Values below median
dat.1 <- dat.wide %>% filter(speed.1 < median(dat.wide$speed.1))

ncount.slr.dec1= nrow(subset(dat.1, change <= slr.lwr))
tcount.slr.dec1= nrow(dat.1)
slr.dec1<-binom.test(ncount.slr.dec1, tcount.slr.dec1, p=0.05)




# Coverage for decline- Values above median
dat.2 <- dat.wide %>% filter(speed.1 > median(dat.wide$speed.1))

ncount.slr.dec2= nrow(subset(dat.2, change <= slr.lwr))
tcount.slr.dec2= nrow(dat.2)
slr.dec2<-binom.test(ncount.slr.dec2, tcount.slr.dec2, p=0.05)





## -------------------------------------------------------------------------------------------------------------------------------------------------------------
slr.plot <-

ggplot(data = dat.wide, aes(x = speed.1, y = change))+
  
  geom_point()+
  
  geom_ribbon(aes(ymax = Inf, ymin = slr.upr, fill = "detectable improvement"), alpha = 0.2)+
  geom_ribbon(aes(ymax = slr.lwr, ymin = -Inf, fill = "detectable decline"), alpha = 0.2)+
  
  
  #geom_hline(yintercept = 0, linetype = 3, size = 1)+
  geom_line(aes(y = slr.upr, linetype = "detectable change threshold"), size = 0.7)+
  geom_line(aes(y = slr.lwr, linetype = "detectable change threshold"),
            size = 0.7)+
  
  annotate(geom = 'text', x = Inf, y = Inf, hjust = 1, vjust = 1, 
           label = paste("Mean precision: ",
                         round(slr_mn_prec, digits = 2),
                         " m/s",
                         sep = ""))+
  
  xlab(label = "Walking Speed (m/s); Initial Test")+
  ylab(label = "Test-Retest Difference (m/s)")+
  
  coord_cartesian(xlim = c(min(dat.wide$speed.1), max(dat.wide$speed.1)),
                  ylim = c(min(dat.wide$change - 0.1), max(dat.wide$change + 0.1)),
                  expand = FALSE)+
  
  scale_linetype_manual(limits = "detectable change threshold",
                        values = 2,
                        name = "")+
  
  scale_fill_manual(limits = c("detectable improvement", "detectable decline"), 
                    values = c("green3", "red3"),
                    name = "")+
  
  ggtitle(label = "MDC SLR")+
  
  theme_bw()

# View plot
slr.plot




## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# m.gamlss <- quote(gamlss(change + 100 ~cs(speed.1, df=p[1]),
#                          sigma.formula = ~cs(speed.1, df=p[2]),
#                          #nu.formula = ~cs(speed.1, df = p[3]),
#                          data=na.omit(dat.wide),
#                          family=BCT, n.cyc=500))
# 
# 
# 
# op <- find.hyper(model=m.gamlss, par=c(1,1), lower=c(0, 0), steps=c(0.5,0.5), k=5)
# 
# op$par



## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Model fit
m.gamlss <- gamlss(change + 100 ~(speed.1),
                   sigma.formula = ~(speed.1),
                   #nu.formula = ~speed.1,
                   data=na.omit(dat.wide),
                   family=BCCG, n.cyc=500)

summary(m.gamlss)

# Calculate centiles (i.e., variance)
gamlss.centiles <- centiles.pred(obj = m.gamlss,
                                 type = "centiles",
                                 cent = c(5, 10, 25, 50, 75, 90, 95),
                                 xvalues = c(dat.wide$speed.1),
                                 xname = c("speed.1"),
                                 legend = FALSE,
                                 plot = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Merge in actual observed values
gamlss.centiles <- gamlss.centiles %>% mutate(speed.1 = x,
                                              gamlss.pred = `50` - 100,  
                                              gamlss.upr = `95` - 100,
                                              gamlss.lwr = `5` - 100,
                                              gamlss.prec = gamlss.upr  - gamlss.lwr) %>%
                                      dplyr::select(speed.1, gamlss.upr, gamlss.lwr, gamlss.prec, gamlss.pred)




dat.final <- left_join(dat.wide, gamlss.centiles, by = "speed.1")

dat.final <- distinct(dat.final)


minx<-min(dat.wide$speed.1)
maxx<-max(dat.wide$speed.1)
gamlss.centiles1 <- centiles.pred(obj = m.gamlss,
                                  type = "centiles",
                                  cent = c(5, 10, 25, 50, 75, 90, 95),
                                  xvalues = seq(from=minx, to=maxx, by=0.01),
                                  xname = c("speed.1"),
                                  legend = FALSE,
                                  plot = FALSE)




gamlss.centiles1$prec<-gamlss.centiles1$`95`-gamlss.centiles1$`5`
gamlss_mn_prec<-mean(gamlss.centiles1$prec)

gamlss.centiles2<-gamlss.centiles1-100

gamlss.centiles2$speed.1<-gamlss.centiles2$x+100

#write.csv(gamlss.centiles2, file="gamlss_080123.csv")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------

# Overall GAMLSS precision
gamlss.prec <- mean(dat.final$gamlss.upr - dat.final$gamlss.lwr, na.rm = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------


# Overall coverage for improvement
ncount.gamlss.imp= nrow(subset(dat.final, change >= gamlss.upr))
tcount.gamlss.imp= nrow(dat.final)
gamlss.imp<-binom.test(ncount.gamlss.imp, tcount.gamlss.imp, p=0.05)



# Coverage for improvement - Values below median
dat.1 <- dat.final %>% filter(speed.1 < median(dat.final$speed.1))

ncount.gamlss.imp1= nrow(subset(dat.1, change >= gamlss.upr))
tcount.gamlss.imp1= nrow(dat.1)
gamlss.imp1<-binom.test(ncount.gamlss.imp1, tcount.gamlss.imp1, p=0.05)



# Coverage for improvement - Values above median
dat.2 <- dat.final %>% filter(speed.1 > median(dat.final$speed.1))

ncount.gamlss.imp2= nrow(subset(dat.2, change >= gamlss.upr))
tcount.gamlss.imp2= nrow(dat.2)
gamlss.imp2<-binom.test(ncount.gamlss.imp2, tcount.gamlss.imp2, p=0.05)




# Overall coverage for decline
ncount.gamlss.dec= nrow(subset(dat.final, change <= gamlss.lwr))
tcount.gamlss.dec= nrow(dat.final)
gamlss.dec<-binom.test(ncount.gamlss.dec, tcount.gamlss.dec, p=0.05)

#


# Coverage for decline - Values below median
dat.1 <- dat.final %>% filter(speed.1 < median(dat.final$speed.1))

ncount.gamlss.dec1= nrow(subset(dat.1, change <= gamlss.lwr))
tcount.gamlss.dec1= nrow(dat.1)
gamlss.dec1<-binom.test(ncount.gamlss.dec1, tcount.gamlss.dec1, p=0.05)



# Coverage for decline- Values above median
dat.2 <- dat.final %>% filter(speed.1 > median(dat.final$speed.1))

ncount.gamlss.dec2= nrow(subset(dat.2, change <= gamlss.lwr))
tcount.gamlss.dec2= nrow(dat.2)
gamlss.dec2<-binom.test(ncount.gamlss.dec2, tcount.gamlss.dec2, p=0.05)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot

gamlss.plot <-
  
ggplot(data = dat.final, aes(x = speed.1))+
  
  geom_ribbon(aes(ymin = gamlss.upr, ymax = Inf, fill = "detectable improvement"), alpha = 0.2)+
  geom_ribbon(aes(ymin = -Inf, ymax = gamlss.lwr, fill = "detectable decline"), alpha = 0.2)+
  
  geom_point(aes(x = speed.1, y = change))+
  
#  geom_hline(yintercept = 0, linetype = 2)+
  geom_line(aes(y = gamlss.upr, linetype = "detectable change threshold"), size = 0.7)+
  geom_line(aes(y = gamlss.lwr, linetype = "detectable change threshold"), size = 0.7)+
  
  annotate(geom = 'text', x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste("Mean precision: ",
                         "0.70",
                         " m/s",
                         sep = ""))+
  
  xlab(label = "Walking Speed (m/s); Initial Test")+
  ylab(label = "Test-Retest Difference (m/s)")+
  
  coord_cartesian(xlim = c(min(dat.wide$speed.1), max(dat.wide$speed.1)),
                  ylim = c(min(dat.wide$change - 0.1), max(dat.wide$change + 0.1)),
                  expand = FALSE)+
  
  scale_linetype_manual(limits = "detectable change threshold",
                        values = 2,
                        name = "")+
  
  scale_fill_manual(limits = c("detectable improvement", "detectable decline"), 
                    values = c("green3", "red3"),
                    name = "")+

  ggtitle(label = "MDC GAMLSS")+

  theme_bw()



# View plot
gamlss.plot




## -------------------------------------------------------------------------------------------------------------------------------------------------------------

plot.combined.speed <- ggarrange(mdc.plot, slr.plot, gamlss.plot,ncol = 3, nrow=1, common.legend = TRUE, legend="bottom")

plot.combined.speed


## -------------------------------------------------------------------------------------------------------------------------------------------------------------


table.cov <- tribble(~Metric, ~Traditional, ~SLR, ~GAMLSS,
                     
                     "Overall coverage: Improvement Threshold",
                     
                     paste(round(mdc.imp$estimate * 100, digits = 1), "% (", round((mdc.imp$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.imp$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.imp$estimate * 100, digits = 1), "% (", round((slr.imp$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.imp$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.imp$estimate * 100, digits = 1), "% (", round((gamlss.imp$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.imp$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     
                     "Overall coverage: Decline Threshold",
                     
                     paste(round(mdc.dec$estimate * 100, digits = 1), "% (", round((mdc.dec$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.dec$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.dec$estimate * 100, digits = 1), "% (", round((slr.dec$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.dec$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.dec$estimate * 100, digits = 1), "% (", round((gamlss.dec$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.dec$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     
                     "Below median coverage: Improvement Threshold",
                     
                     paste(round(mdc.imp1$estimate * 100, digits = 1), "% (", round((mdc.imp1$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.imp1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.imp1$estimate * 100, digits = 1), "% (", round((slr.imp1$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.imp1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.imp1$estimate * 100, digits = 1), "% (", round((gamlss.imp1$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.imp1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     
                     "Below median coverage: Decline Threshold",
                     
                     paste(round(mdc.dec1$estimate * 100, digits = 1), "% (", round((mdc.dec1$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.dec1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.dec1$estimate * 100, digits = 1), "% (", round((slr.dec1$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.dec1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.dec1$estimate * 100, digits = 1), "% (", round((gamlss.dec1$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.dec1$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     
                     "Above median coverage: Improvement Threshold",
                     
                     paste(round(mdc.imp2$estimate * 100, digits = 1), "% (", round((mdc.imp2$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.imp2$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.imp2$estimate * 100, digits = 1), "% (", round((slr.imp2$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.imp2$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.imp2$estimate * 100, digits = 1), "% (", round((gamlss.imp2$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.imp2$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     
                     
                     "Above median coverage: Decline Threshold",
                     
                     paste(round(mdc.dec2$estimate * 100, digits = 1), "% (", round((mdc.dec2$conf.int[1]) * 100, digits = 1), ", ",
                           round((mdc.dec2$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(slr.dec2$estimate * 100, digits = 1), "% (", round((slr.dec2$conf.int[1]) * 100, digits = 1), ", ",
                           round((slr.dec2$conf.int[2]) * 100, digits = 1), ")", sep = ""),
                     
                     paste(round(gamlss.dec2$estimate * 100, digits = 1), "% (", round((gamlss.dec2$conf.int[1]) * 100, digits = 1), ", ",
                           round((gamlss.dec2$conf.int[2]) * 100, digits = 1), ")", sep = ""))


                    

table.cov<- kbl(table.cov, caption = "Comparing coverage between MDC approaches (Ideal value = 5%)") %>% 
              kable_styling(bootstrap_options = c("striped"))

table.cov



