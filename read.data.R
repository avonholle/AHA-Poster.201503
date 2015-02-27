# read.data.R
# took this program from C:\Users\vonholle\Documents\grad_school\classes\epid.718\aha_abstract\programs\Descriptive.R

#install.packages("memisc")
#install.packages("knitr")
#install.packages("pastecs")
#install.packages("reporttools","tables")
#install.packages("pander")
#install.packages("pROC")
#install.packages("ModelGood")
#install.packages("tables")
#install.packages("FactoMineR")
#install.packages("GPArotation")

library(xtable)
library(foreign)
library(memisc)
#library(stargazer)
library(knitr)
library(pastecs)
library(ggplot2)
library(gridExtra)
library(xtable)
library(plyr)
#library(tables)
library(reshape)
#library(reporttools)
#library(rreport)
library(Hmisc)
#library(pander)
library(rms)

#library(pROC)
#library(Epi)
#library(ModelGood)
library(tables)
#library(FactoMineR)
library(psych)
#library(GPArotation)

## @knitr read1

setwd("C:/Users/vonholle/Documents/grad_school/classes/epid.718/data")

mydata <- read.spss("VonHolle_Chile_longitudinal_08.25.2014.sav")

head(mydata)
#summary(mydata)

# get labels
# See http://www.r-bloggers.com/migrating-from-spss-to-r-rstats/
# ###################################################################

# load data base as list
#efc.spss <- read.spss("VonHolle_Chile_longitudinal_08.04.2014.sav", to.data.frame=FALSE, use.value.labels=FALSE)
efc.spss <- read.spss("VonHolle_Chile_longitudinal_08.25.2014.sav", to.data.frame=FALSE, use.value.labels=FALSE) # updated sent 8/25/2014

# convert list to data frame
efc <- as.data.frame(efc.spss)
efc <- efc[!(is.na(efc$A_HDLc_alt_Cook)),] # take out any missing HDL values to make final data set. IMPORTANT!
nrow(efc)

# get rid of any underscores in variable names
names(efc) <- make.names(names(efc), unique=T, allow_=F)

# copy all variable labels in separated list
efcvars <- attr(efc.spss, "variable.labels"); class(efcvars); head(efcvars)
# get rid of any underscores in variable names
names(efcvars) <- make.names(names(efcvars), unique=T, allow_=F)
# get rid of any underscores in variable label or stargazer will not work with html files
sapply(efcvars, function(x) gsub("&", ".", x))
sapply(efcvars, function(x) gsub("_", ".", x))


# convert dates. 
# #################################################################
# BACKGROUND INFO: SPSS uses October 14, 1582 as the origin thereby
# representing datetimes as seconds since the beginning
# of the Gregorian calendar
# see http://r.789695.n4.nabble.com/Importing-dates-from-SPSS-file-td3260293.html
# and http://cran.r-project.org/doc/Rnews/Rnews_2004-1.pdf
# ##################################################################
names(efc)
dates <- c("f.dob", "T1156", "T1157", "A.last.period.date", "f.dom5y" )
#dates
efc[dates] <- apply(efc[dates], 2, function(x) { as.POSIXlt(x, origin="1582/10/14") })
head(efc[dates])

# see http://stackoverflow.com/questions/2254986/how-to-subtract-days-in-r
efc$maternal.dob <- as.numeric(efc$f.dob$year - efc$T1156$year)

# add label to factor variable for dichotomous outcome
levels(factor(efc$A.HDLc.alt.Cook))
efc$hdl.cook.2 <- factor(efc$A.HDLc.alt.Cook, labels=c(">40 mg/dL", "<=40 mg/dL"))
efcvars$hdl.cook.2 <- "HDL Cook threshold. factor"

mean(efc$maternal.dob, na.rm=T)

class(efc$maternal.dob)
mean(efc$maternal.dob)
efcvars$maternal.dob <- "Estimated age of mom at birth from maternal dob and date of birth of infant"

# derive maternal bmi before pregnancy
median(efc$T1137/(efc$T1131/100)^2, na.rm=T)
summary(efc$T1137)
summary(efc$T1131)

efc$mom.bmi.before = efc$T1137/(efc$T1131/100)^2
quantile(efc$mom.bmi.before, c(0.01,0.02, 0.03, 0.05, 0.92, 0.925, 0.93, 0.94, 0.95, 0.97, 0.98, 0.99), na.rm=T)
efc$mom.bmi.before[efc$mom.bmi.before>50|efc$mom.bmi.before<15] <- NA # set biologically implausible values to missing

#check
quantile(efc$mom.bmi.before, c(0.01,0.02, 0.03, 0.05, 0.92, 0.925, 0.93, 0.94, 0.95, 0.97, 0.98, 0.99), na.rm=T)
describe(efc$mom.bmi.before)

# and change variable labels for ses vars
#        "actua.ses.q33", "actua.ses.q34", "actua.ses.q38",
# "actua.ses.q31", "actua.ses.q32"

efcvars$actua.ses.q31 <- "sesq31. Schooling father last year approved"
efcvars$actua.ses.q32 <- "sesq32. Total years of schooling father"

efcvars$actua.ses.q33 <- "sesq33. Schooling mother last year approved"
efcvars$actua.ses.q34 <- "sesq34. Total years of schooling mother"
efcvars$actua.ses.q38 <- "sesq38. Mothers education"

efcvars$mom.bmi.before = "Maternal BMI before pregnancy"

# copy all value labels as separated list
efclabel <- attr(efc.spss, "label.table") # note, no labels

head(efcvars)
names(efcvars)
head(efclabel)
efcvars[1:20]
rownames(efcvars)

# test code to get characters in a string (for variable labels)

test <- as.character(cat(paste(shQuote(efcvars[1:5], type="cmd"), collapse=", ")))
test

test2 <- dput(as.character(efcvars[1:5]))
test2

# now add variable names to each string
efcvars.both <- paste(names(efcvars),efcvars,sep=": ")
efcvars.both
head(efcvars.both)

# see http://stackoverflow.com/questions/6347356/creating-a-comma-separated-vectors
string1 <- dput(as.character(efcvars[1:5]))
string1

efcvars.both <- paste(names(efcvars), efcvars, sep=": ")
varlabs <- dput(as.character(efcvars.both))
nrow(as.data.frame(varlabs))

# to keep the columns of data that are just headers, re-do data so that the missing are all one value,
# such as -9999

# select the subset of variables to be used in analysis
vars.more <- c("f.agemos5", "f.bmiz5y", "f.bmi5y", "A.age", "A.sex", "A.body.weight.average",
               "f.height5y", "v8",
               "A.diast.BP.average", "A.syst.BP.average", "A.insulin", "A.TG", "A.glucose",
               "A.LDLc", "A.HDLc",
               "v.momage.i", "f.dob", "T1156",
               "A.height.average", "A.waist.average",
               "f.dob", "v.wt0",
               "R.momsmoke", "R.pasmoke",
               "sex", "sex.n",
               "actua.ses.q33", "actua.ses.q34", "actua.ses.q38",
               "actua.ses.q31", "actua.ses.q32", "A.HDLc.alt.Cook",
               "actua.ses.q26",
               "hdl.cook.2",
               "actua.ses.q08",
               "T1137", "T1131", 'mom.bmi.before',
               "ID.v1",
               "actua.ses.q08", "actua.ses.q16", "actua.ses.q21", "actua.ses.q29",
               "actua.ses.q37", "actua.ses.q40", "actua.ses.q41", "actua.ses.q42", 
               "actua.ses.q43", "actua.ses.q44", "actua.ses.q45", "actua.ses.q49", 
               "actua.ses.q59") # Note: last 13 variables are graffar
vars.more2 <- vars.more[!(vars.more %in% dates)] # exclude any converted date variables from table
vars.more2

# Need to convert the original names to the names as modified above (to select out)
# efc.final <- efc
# nrow(efc.final)
# 
# names(efc.final) <- make.names(names(efc), unique=T, allow_=F)
# names(efc.final[vars.more])
# 
# test <- efc.final[vars.more]
# sapply(test, is.numeric)
# names(test)
# sapply(test[,c(14:17)], function(x) max(x, na.rm=T) )
# sapply(test[,c(14:17)], function(x) min(x, na.rm=T) )
# 
# efc.final[vars.more][1:10,c(14:18)]

# note you cannot get variables aligned by subgroup in stargazer. 
# will have to print them off separately.
# stargazer(efc.final[vars.more], 
#           type = "text", 
#           title="Descriptive statistics", digits=2)

# income: umich_chile.actua_ses.q26
# maternal education: umich_chile.actua_ses.q33
#                     umich_chile.actua_ses.q34, 
#                     umich_chile.actua_ses.q38, 
# paternal education: umich_chile.actua_ses.q31,
#                      umich_chile.actua_ses.q32

# need to convert the original names to the names as modified above (to select out)
efc.final.prep <- efc
efcvars.final.prep <- efcvars
names(efc.final.prep) <- make.names(names(efc), unique=T, allow_=F)
names(efcvars.final.prep) <- make.names(names(efcvars), unique=T, allow_=F)


# Make an index based on Graffar
# ###################################

graffar.vars <- c("actua.ses.q08", "actua.ses.q16", "actua.ses.q21", "actua.ses.q29",
                  "actua.ses.q37", "actua.ses.q40", "actua.ses.q41", "actua.ses.q42", 
                  "actua.ses.q44", "actua.ses.q45", "actua.ses.q49", 
                  "actua.ses.q59",
                  "actua.ses.q26") # took out "actua.ses.q43" because it has all 1's. it's the toilet variable? 
                                    # and q26 is hh income

summary(efc.final.prep[graffar.vars])

efc.final.prep$graffar.index = apply(efc.final.prep[graffar.vars], 1, sum)
head(efc.final.prep$graffar.index)
summary(efc.final.prep$graffar.index)

# PCA
# ###############################
# result.pca <- PCA(efc.final.prep[graffar.vars]) # this has a nice factor map

# based on factor map the first dimension indicates a range from high levels at
# q42, q21, q41, q59 (kitchen, household activity, type of house, property) to 
# low levels at q26, which is household income
# so a higher score on the first factor indicates higher household standard of living and a 
# lower score indicates a lower income
# the factor map demonstrates clustering at the first component indicating not a lot of variability in the sample
# -- people tend to answer similarly.

# pca from the psych package
results.pca.2 <- principal(efc.final.prep[graffar.vars], nfactors=3, rotate="varimax")
results.pca.2 # print results 
biplot(results.pca.2)
# names(results.pca.2)
# head(results.pca.2$scores)
# summary(results.pca.2$scores)
# cov(results.pca.2$scores[complete.cases(results.pca.2$scores),])

graffar <- as.matrix(efc.final.prep[complete.cases(efc.final.prep[graffar.vars]),graffar.vars])
head(graffar)
summary(names(graffar))

# see www.statmethods.net/advstats/factor.html
# results.pca.3 <- princomp(graffar, cor=T)
# summary(results.pca.3)
# plot(results.pca.3,type="lines") # scree plot 
# head(results.pca.3$scores)
# biplot(results.pca.3)

# Fix up the final data set
# ######################################################
efc.final <- efc.final.prep[vars.more]
efcvars.final <- efcvars.final.prep[vars.more]

# add first pca score to data set
efc.final$graffar.pca.score1 <- results.pca.2$scores[,1]
efcvars.final$graffar.pca.score1 <- "Graffar PCA score 1"


efcvars.both.final <- paste(names(efc.final), efcvars.final, sep=": ") # add variable labels to row header
efcvars.both.final

varlabs.final <- dput(as.character(efcvars.both.final)) # make a vector of these new names to put in table as row header
length(varlabs.final)

# stargazer(efc.final, 
#           type = "text", 
#           title="Descriptive statistics for subset of variables to use in EPID 718", digits=2, 
#           covariate.labels=varlabs.final )

names(efc.final)

mean(efc.final$f.dob)
date.test <- as.POSIXlt(efc.final$f.dob, origin="1582/10/14") 
head(date.test)

table(efc$A.Smet.Cook)
table(efc$A.HDLc.alt.Cook)
table(efc$A.HDLc.alt)

nrow(efc.final)



## @knitr section2


# ###################################################
# Produce tables from reporttools. Will refine later.
# ###################################################

# first, extract out the numeric variables

# need to extract out only the variables that are numeric
# see https://stat.ethz.ch/pipermail/r-help/2008-December/182825.html
# issues with coercing into some sort of 'supertype'. confusing.
# need to use sapply

head(names(efc))
#num.names <- sapply(efc[-ID.v1], is.numeric) # do not include participant ID
#head(num.names)
#table(num.names)
#num.names = names(num.names[num.names==T]) # columns with numeric variables
#length(num.names)

num.names.sub1 <- sapply(efc[vars.more], is.numeric) # subset of variables most likely to be in analysis
num.names.sub1 <- names(num.names.sub1[num.names.sub1==T])

fac.names <- sapply(efc, is.factor)
#fac.names <- fac.names[, -which(names(fac.names) %in% c("actua.ses.q17"))]
fac.names = names(fac.names[fac.names==T ]) # columns with numeric variables

#fac.names = fac.names[!actua.ses.q17]
length(fac.names) #check
head(fac.names)


#table.cont <- tableContinuous(vars=efc[num.names.sub1], group=efc$A.HDLc.alt.Cook)

#setwd("C:/Users/Ann/Documents/grad_school/classes/epid.718/aha_abstract/results")
# 
# tableContinuous(vars=efc[num.names.sub1], group=efc$A.HDLc.alt.Cook)
# warnings()
# 
# tableNominal(vars=efc[fac.names], group=efc$A.HDLc.alt.Cook)

efc.hdl.1 <- efc[efc$A.HDLc.alt.Cook==1,]
nrow(efc.hdl.1)
efc.hdl.0 <- efc[efc$A.HDLc.alt.Cook==0,]
nrow(efc.hdl.0)

# stargazer(efc.final[efc$A.HDLc.alt.Cook==1,], efc.final[efc$A.HDLc.alt.Cook==0,],
#           out.header=T,
#           column.labels=c("a","b"),
#           type = "text", 
#           title="Descriptive statistics for subset of variables to use in EPID 718", digits=2, 
#           covariate.labels=varlabs.final )

sub <- efc.final.prep[vars.more2]; ncol(sub)
names(sub)

levels(factor(sub$A.HDLc.alt.Cook))
hdl.cook <- factor(sub$A.HDLc.alt.Cook, labels=c(">40 mg/dL", "<=40 mg/dL"))
table(hdl.cook)

s1 <- summary( hdl.cook~., data=sub, method="reverse")
obj.1 <- print(s1)
class(obj.1)

colnames(obj.1) <- c("1", "2", "3")
head(obj.1)

# Add labels to variables
efc.final.2 <- efc.final.prep[vars.more2]; ncol(efc.final.2)
efcvars.final.2 <- efcvars.final.prep[vars.more2]
efcvars.both.final.2 <- paste(names(efc.final.2), efcvars.final.2, sep=": ") # add variable labels to row header
efcvars.both.final.2

varlabs.final.2 <- dput(as.character(efcvars.both.final.2)) # make a vector of these new names to put in table as row header
length(varlabs.final.2) #checking that the variable labels will be correct
ncol(sub)

sub.2 <- sub
colnames(sub.2) <- varlabs.final.2
levels(factor(sub.2$A.HDLc.alt.Cook))
hdl.cook.2 <- factor(sub.2$A.HDLc.alt.Cook, labels=c(">40 mg/dL", "<=40 mg/dL"))

s2 <- summary( hdl.cook~., data=sub.2, method="reverse")
print(s2)

# EXPOSURE VARIABLE, bmi quartiles (determined at start of project 2 9/2014)
# Exposure (BMI). Have higher bmi groups as the first ones listed
quart <- quantile(efc.final$f.bmi5y, c(0, 0.25, 0.5, 0.75, 1))
efc.final$exposure <- with(efc.final, {
  cut(efc.final$f.bmi5y, breaks=quart)})
table(efc.final$exposure)
levels(efc.final$exposure)
efc.final$exp = as.numeric(efc.final$exposure)
table(efc.final$exp)
levels(factor(efc.final$exp))
efc.final$exp = factor(efc.final$exp, labels=c("(18.1,28.6]",
                                         "(16.6,18.1]",
                                         "(15.5,16.6]",
                                         "(13.4,15.5]"),
                    levels=c(4,3,2,1))
table(efc.final$exp)

# export the efc.final data out to a permanant data frame to pull in future R programs
setwd("C:/Users/vonholle/Documents/grad_school/classes/epid.718/projects/part1/programs/R")
save(efc.final, file="efc.final.Rdata")

