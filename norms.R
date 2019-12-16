#
# Code for a psycholinguistic study about conceptual modality, part of P. Bernabeu's
# MPhil thesis. 'Modality exclusivity norms for 747 properties and concepts in Dutch:
# a replication of English'. More info at: https://goo.gl/Je4JGO. 
# Contact: pcbernabeu@gmail.com


# READ-ME
# Ctrl+f may be used to search for specific parts of this code (for instance, PCA).
# The 'all.csv' file, created outside of R, in Excel, compiles all individual ratings.
# Dutch and English data are described in separate columns. All analyses separate for 
# properties and concepts, except for a translation check. 
# Stat tests (specifying treatment of English and Dutch norms): reliability analysis
# (only Dutch norms), Pearson?s correlation (norms independent and paired), one-sample 
# t-test (norms independent), Principal Components Analysis (norms independent), ANOVA 
# (norms paired), and multiple regression (norms independent).
# The code is extensively annotated, but some clarifications are in order. Subsetting is
# done throughout the code, and is essential due to the different norms (see 'normed'
# column: English, Dutch, or both). Subsetting is often done on the basis of variables
# that are unique to either norms, especially, 'Exclusivity' and 'English_Exclusivity_Lynott_Connell_2009_2013'.
# At first, the code must be run right from the top, as different objects bear the 
# same name. In its entirety, it takes ~20 mins. Note the annotations for theoretical 
# matters. Long variables are never presented entirely, but rather in sections or via
# summaries. Yet, the reader is invited to edit and present them entirely.
# Written on R version 3.2.2 (2015-08-14). This script markdown presents each code chunk  
# followed by the results. 


# INDEX
# Libraries: please ensure every library loads, and otherwise install it via 
# 	install.packages("")
# Preprocessing
# Translation-dependent results
# Critical results
# 	Modality
#     Sound-symbolism




setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality norms')

install.packages("gdata")
install.packages("GPArotation")
install.packages("psych")
install.packages("ggplot2")
install.packages("car")
install.packages("Rmisc")
install.packages("corpcor")
install.packages('contrast')
install.packages('doBy')
install.packages('ltm')
install.packages('MASS')
install.packages('QuantPsyc')
install.packages('qpcR')
install.packages('corpcor')
install.packages('lattice')
install.packages('car')
install.packages('pastecs')
install.packages('scales')
install.packages('reshape')
install.packages('arules')
install.packages('plyr')
install.packages('RColorBrewer')
install.packages('dplyr')
install.packages('gdata')
install.packages('gtools')
install.packages('Hmisc')
install.packages('png')
install.packages('ggrepel')
install.packages('irr')
install.packages('tibble')

library(ltm)
library(lattice)
library(psych)
library(car)
library(doBy)
library(contrast)
library(pastecs)
library(scales)
library(ggplot2)
library(reshape)
library(arules)
library(plyr)
library(RColorBrewer)
library(Rmisc)
library(corpcor)
library(GPArotation)
library(gdata)
library(QuantPsyc)
library(MASS)
library(qpcR)
library(dplyr)
library(gtools)
library(Hmisc)
library(png)
library(ggrepel)
library(irr)
library(tibble)


# Calculate average percentange of unresponded items, i.e., unknown. Since there are 
# three ratings per word, and indeed the three were left blank whereever participants
# ignored some word, the calculation includes a division by 3 (besides overall mean,
# see specific percentage per file).

file1 <- read.csv('file1_gral.csv')
file2 <- read.csv('file2_gral.csv')
file3 <- read.csv('file3_gral.csv')
file4 <- read.csv('file4_gral.csv')
file5 <- read.csv('file5_gral.csv')
file6 <- read.csv('file6_gral.csv')



# What percentage of modality ratings was not provided by respondents? Missing divided by total:

 ( sum(is.na(file1)) + sum(is.na(file2)) + sum(is.na(file3)) +
   sum(is.na(file4[,-c(1:4)])) +   # first participant removed because they completed only first half survey
   sum(is.na(file5)) + sum(is.na(file6)) ) /

 ( ncol(file1[,-1]) * nrow(file1[,-1]) +
   ncol(file2[,-1]) * nrow(file2[,-1]) +
   ncol(file3[,-1]) * nrow(file3[,-1]) +
   ncol(file4[,-c(1:4)]) * nrow(file4[,-c(1:4)]) +	# first participant removed as above
   ncol(file5[,-1]) * nrow(file5[,-1]) +
   ncol(file6[,-1]) * nrow(file6[,-1]) ) *

 100	 # percentage

# 3.82% missing ratings




#_________________________________________________________________________________



# Preprocessing:
# There were 9 files with different items (mostly unrepeated) for concepts and 10
# files for properties. They were completed in different proportions, with an
# average of eight participants per file. 

# RELIABILITY ANALYSIS: In putting together the ratings from each respondent, this 
# analysis allows to calculate the fit among those. In other words, is the mean 
# realistic or forced? Two measures are provided. First, interitem consistency 
# provides the fit among items independently of raters. Second, interrater 
# reliability measures the fit among raters, independently of items. Interrater
# reliability is first calculated on the modality scores, and then at the level 
# of dominant modalities.

# Concepts

all <- read.csv('all.csv')
concs <- all[all$cat == 'Concept',]
a_concs <- concs[, c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10')]
h_concs <- concs[, c('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'a10')]
v_concs <- concs[, c('v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9', 'a10')]

# Interitem consistency

psych::alpha(a_concs)
psych::alpha(h_concs)
psych::alpha(v_concs)
# a: .74
# h: .72
# v: .70

# Interrater reliability (Koo & Li, 2016)

a_concs <- concs[, c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9')]
h_concs <- concs[, c('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9')]
v_concs <- concs[, c('v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9')]

icc(a_concs, "oneway", "consistency")
icc(h_concs, "oneway", "consistency")
icc(v_concs, "oneway", "consistency")



# Now, look at the inter-rater agreement on the basis of the highest modality score for each word, namely
# the dominant modality. Wherever a tie occurs among several modalities, one of them is randomly selected.

all.Dutch = all[!is.na(all$word),]

rater = NA
id = NA  # i.e., word ID
result = NA
results = data.frame(rater, id, result)


# Safer version of sample(), for use below
resample <- function(x, ...) x[sample.int(length(x), ...)]


getMain =
function(rater, i.col){

 for(i.row in 1:nrow(all.Dutch)){

    # For cases in which no rating was provided to any modality, assign NA
    if( length(which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)]))) == 0) {
	result = NA
    
    # For tied modalities, randomly select one of them
    } else if( !length(which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)]))) > 1) {
	result = ifelse( sample(which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)])), size=1) == 1, 'Auditory',
	  ifelse( sample(which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)])), size=1) == 2, 'Haptic',
	    'Visual' ) )
    
    # For plain cases with one dominant modality, assign that modality
    } else{
	result = ifelse( which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)])) == 1, 'Auditory',
	  ifelse( which(all.Dutch[i.row, c(i.col)]==max(all.Dutch[i.row, c(i.col)])) == 2, 'Haptic',
	    'Visual' ) )
    }

  results = rbind(results, c(rater, as.character(all.Dutch[i.row, 'id']), result))
 }
 results <<- results[!is.na(results$rater),]
}


getMain(1, c('a1','h1','v1'))
getMain(2, c('a2','h2','v2'))
getMain(3, c('a3','h3','v3'))
getMain(4, c('a4','h4','v4'))
getMain(5, c('a5','h5','v5'))
getMain(6, c('a6','h6','v6'))
getMain(7, c('a7','h7','v7'))
getMain(8, c('a8','h8','v8'))
getMain(9, c('a9','h9','v9'))
getMain(10, c('a10','h10','v10'))

all = merge(all, results[results$rater==1, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main1'

all = merge(all, results[results$rater==2, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main2'

all = merge(all, results[results$rater==3, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main3'

all = merge(all, results[results$rater==4, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main4'

all = merge(all, results[results$rater==5, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main5'

all = merge(all, results[results$rater==6, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main6'

all = merge(all, results[results$rater==7, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main7'

all = merge(all, results[results$rater==8, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main8'

all = merge(all, results[results$rater==9, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main9'

all = merge(all, results[results$rater==10, c('id', 'result')], by='id')
colnames(all)[colnames(all)=="result"] = 'main10'

# Order columns
names(all)
all = all[,c(1:58, 67:76, 59:66)]

# Fleiss' Kappa (Fleiss, 1971)
kappam.fleiss(all[,c('main1', 'main2', 'main3', 'main4', 'main5', 'main6', 'main7', 
   'main8', 'main9', 'main10')], detail=TRUE)




# Properties

# Interitem consistency

props <- all[all$cat == 'Property',]
a_props <- props[, c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'a10')]
h_props <- props[, c('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'h10')]
v_props <- props[, c('v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9', 'v10')]

psych::alpha(a_props)
psych::alpha(h_props)
psych::alpha(v_props)
# a: .78
# h: .70
# v: .85

# Interrater reliability (Koo & Li, 2016)

a_props <- props[, c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8')]
h_props <- props[, c('h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8')]
v_props <- props[, c('v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8')]

icc(a_props, "oneway", "consistency")
icc(h_props, "oneway", "consistency")
icc(v_props, "oneway", "consistency")


# _______________________________________________________________________________





# TRANSLATION-DEPENDENT RESULTS

# Read in the general norms file, 'all'

all <- read.csv('all.csv')

# PROPERTIES
props <- all[all$cat=='Property',]
nrow(props)  # 336 Dutch + a few items from Lynott and Connell (2009) that weren't translated but were kept in to have all items from the three modalities in the PCA reanalysis.

# CONCEPTS 
concs <- all[all$cat=='Concept',]
nrow(concs)  # 411 Dutch + a few items from Lynott and Connell (2013) that weren't translated but were kept in to have all items from the three modalities in the PCA reanalysis.



# Descriptives: perceptual strength
# Dutch
summaryBy(Perceptualstrength ~ cat, FUN=stat.desc, data=all)

# English
summaryBy(English_Perceptualstrength_Lynott_Connell_2009_2013 ~ cat, FUN=stat.desc, 
data=all[!is.na(all$English_Perceptualstrength_Lynott_Connell_2009_2013),])

# The differences make sense considering the sampling method applied in the different
# norms. The difference between the English and the Dutch norms is a bit larger for 
# the concept norms because these norms were sampled differently. While the concepts
# tested in the English norms were compiled regardless of their potential modalities,
# the Dutch norms were entirely compiled  with a view to potential modality.



# Correlations

# PROPERTIES
# Modalities
rcor.test(props[, c('Auditory', 'English_Auditory_Lynott_Connell_2009_2013')], use = 'complete.obs')
rcor.test(props[, c('Haptic', 'English_Haptic_Lynott_Connell_2009_2013')], use = 'complete.obs')
rcor.test(props[, c('Visual', 'English_Visual_Lynott_Connell_2009_2013')], use = 'complete.obs')
# Significant, large correlations ranging from .69 to .80

# Exclusivity 
rcor.test(props[, c('Exclusivity', 'English_Exclusivity_Lynott_Connell_2009_2013')], use = 'complete.obs')
# Medium-sized corr Eng-Dutch


# CONCEPTS
# Modalities
rcor.test(concs[, c('Auditory', 'English_Auditory_Lynott_Connell_2009_2013')], use = 'complete.obs')
rcor.test(concs[, c('Haptic', 'English_Haptic_Lynott_Connell_2009_2013')], use = 'complete.obs')
rcor.test(concs[, c('Visual', 'English_Visual_Lynott_Connell_2009_2013')], use = 'complete.obs')
# Significant, large correlations ranging from .63 to .69

# Exclusivity 
rcor.test(concs[, c('Exclusivity', 'English_Exclusivity_Lynott_Connell_2009_2013')], use = 'complete.obs')
# Medium-sized corr Eng-Dutch


# Descriptives: M, SD, SE...
# English
psych::describe(props$English_Auditory_Lynott_Connell_2009_2013)
psych::describe(props$English_Haptic_Lynott_Connell_2009_2013)
psych::describe(props$English_Visual_Lynott_Connell_2009_2013)
psych::describe(concs$English_Auditory_Lynott_Connell_2009_2013)
psych::describe(concs$English_Haptic_Lynott_Connell_2009_2013)
psych::describe(concs$English_Visual_Lynott_Connell_2009_2013)

stat.desc(props$English_Auditory_Lynott_Connell_2009_2013)
stat.desc(props$English_Haptic_Lynott_Connell_2009_2013)
stat.desc(props$English_Visual_Lynott_Connell_2009_2013)
stat.desc(concs$English_Auditory_Lynott_Connell_2009_2013)
stat.desc(concs$English_Haptic_Lynott_Connell_2009_2013)
stat.desc(concs$English_Visual_Lynott_Connell_2009_2013)


# Dutch
psych::describe(props$Auditory)
psych::describe(props$Haptic)
psych::describe(props$Visual)
psych::describe(concs$Auditory)
psych::describe(concs$Haptic)
psych::describe(concs$Visual)

stat.desc(props$Auditory)
stat.desc(props$Haptic)
stat.desc(props$Visual)
stat.desc(concs$Auditory)
stat.desc(concs$Haptic)
stat.desc(concs$Visual)


# Sample sizes for English and Dutch
nrow(props[!is.na(props$English_Exclusivity_Lynott_Connell_2009_2013),]) # total properties w/ English norms (Lynott & Connell, 2013) = 343
nrow(concs[!is.na(concs$English_Exclusivity_Lynott_Connell_2009_2013),]) # total concepts w/ English norms (Lynott & Connell, 2013) = 392

nrow(props[!is.na(props$Exclusivity),]) # total properties w/ Dutch norms = 336
nrow(props[props$main=='Auditory' & !is.na(props$Exclusivity),])
nrow(props[props$main=='Haptic' & !is.na(props$Exclusivity),])
nrow(props[props$main=='Visual' & !is.na(props$Exclusivity),])

nrow(props[!is.na(concs$Exclusivity),]) # total concepts w/ Dutch norms = 411
nrow(props[concs$main=='Auditory' & !is.na(concs$Exclusivity),])
nrow(props[concs$main=='Haptic' & !is.na(concs$Exclusivity),])
nrow(props[concs$main=='Visual' & !is.na(concs$Exclusivity),])
# _______________________________________________________________




# CRITICAL RESULTS: not translation-influenced

# Relation between modality strength, dominant modalities, and mod exclusivitY
# ENGLISH
# properties
summaryBy(English_Auditory_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=props, FUN=mean)
summaryBy(English_Haptic_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=props, FUN=mean)
summaryBy(English_Visual_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=props, FUN=mean)
summaryBy(English_Exclusivity_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=props, FUN=mean)

# concepts
summaryBy(English_Auditory_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=concs, FUN=mean)
summaryBy(English_Haptic_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=concs, FUN=mean)
summaryBy(English_Visual_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=concs, FUN=mean)
summaryBy(English_Exclusivity_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013, data=concs, FUN=mean)


# DUTCH 
# properties
summaryBy(Auditory ~ main, data=props, FUN=mean)
summaryBy(Haptic ~ main, data=props, FUN=mean)
summaryBy(Visual ~ main, data=props, FUN=mean)
summaryBy(Exclusivity ~ main, data=props, FUN=mean)

# concepts
summaryBy(Auditory ~ main, data=concs, FUN=mean)
summaryBy(Haptic ~ main, data=concs, FUN=mean)
summaryBy(Visual ~ main, data=concs, FUN=mean)
summaryBy(Exclusivity ~ main, data=concs, FUN=mean)


# RESULTS: both languages strongly related on individual modalities and on 
# exclusivity. Correlations among modalities replicate previous norms, with visual 
# and haptic items related, and auditory ones independent.

# Yet, there is clearly a greater exclusivity in the English norms.
# Properties
psych::describe(props$English_Exclusivity_Lynott_Connell_2009_2013)      # M = 0.48
psych::describe(props$Exclusivity)  # M = 0.40

# Concepts
psych::describe(concs$English_Exclusivity_Lynott_Connell_2009_2013)      # M = 0.40
psych::describe(concs$Exclusivity)  # M = 0.29

# Indeed lower exclusivity and higher SD for Dutch items >> Check significance
# Because the English and the Dutch norms are paired, the difference has to be 
# checked through a one-sample t-test, checking the mean of one language against 
# the other language (see further below).



# Correlations among modalities within each category and language:

# ENGLISH
# PROPERTIES
rcor.test(props[, c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013', 'English_Exclusivity_Lynott_Connell_2009_2013')], use = 
'complete.obs')
corr3 = rcor.test(props[, c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013', 'English_Exclusivity_Lynott_Connell_2009_2013')], 
use = 'complete.obs')
#write.csv(corr3$cor.mat, file = "corr3.csv",na="") # saved for manuscript

# CONCEPTS
rcor.test(concs[, c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013', 'English_Exclusivity_Lynott_Connell_2009_2013')], use = 'complete.obs')
corr4 = rcor.test(concs[, c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013', 'English_Exclusivity_Lynott_Connell_2009_2013')], use = 
'complete.obs')
#write.csv(corr4$cor.mat, file = "corr4.csv",na="") # saved for manuscript


# DUTCH
# PROPERTIES
rcor.test(props[, c('Auditory', 'Haptic', 'Visual', 'Exclusivity')], use = 
'complete.obs')
corr1 = rcor.test(props[, c('Auditory', 'Haptic', 'Visual', 'Exclusivity')], 
use = 'complete.obs')
#write.csv(corr1$cor.mat, file = "corr1.csv",na="") # saved for manuscript

# CONCEPTS
rcor.test(concs[, c('Auditory', 'Haptic', 'Visual', 'Exclusivity')], use = 
'complete.obs')
corr2 = rcor.test(concs[, c('Auditory', 'Haptic', 'Visual', 'Exclusivity')], use = 
'complete.obs')
#write.csv(corr2$cor.mat, file = "corr2.csv",na="") # saved for manuscript


# Statistical tests for those differences
# Yet the same again, but now with a statistical significance test

# ENGLISH
# Setting contrasts based on means
contrasts(all$English_Main_Lynott_Connell_2009_2013) <- cbind(c(2,0,-2), c(-1,2,-1))
# (1) Aud vs Vis; (2) Hap vs Aud-&-Vis
contrasts(all$English_Main_Lynott_Connell_2009_2013)

fitt <- aov(English_Exclusivity_Lynott_Connell_2009_2013 ~ English_Main_Lynott_Connell_2009_2013 * cat, data=all)
plot(fitt)
summary(fitt)
drop1(fitt,~.,test="F")

Anova(fitt)
Anova(fitt, type = "II")
Anova(fitt, type = "III")
summary.lm(fitt)

# RESULTS: English properties with more exclusivity than concepts(***)
# Contrasts: (1) Aud vs Vis (*)
# (2) Haptic words show less exclusivity than auditory and visual ones within 
# properties but not within concepts (*)


# DUTCH
# Setting contrasts based on means
contrasts(all$main) <- cbind(c(2,0,-2), c(-1,2,-1))
# (1) Aud vs Vis; (2) Hap vs Aud-&-Vis
contrasts(all$main)

fitt <- aov(Exclusivity ~ main * cat, data=all)
plot(fitt)  # must click over the plot several times in order to continue
summary(fitt)
drop1(fitt,~.,test="F")

Anova(fitt)
Anova(fitt, type = "II")
Anova(fitt, type = "III")
summary.lm(fitt)

# RESULTS: Dutch properties with more exclusivity than concepts(***)
# Contrasts: (1) Aud vs Vis (non-sig)
# (2) Haptic words show less exclusivity than auditory and visual ones within 
# properties but not within concepts (**)

# Overall, these results stem from the nature of human perception. What exclusivity 
# seems to be indexing is the degree to which percepts will naturally co-occur. Thus,
# visual and auditory words have relatively higher exclusivities because what we 
# see and hear often stands on its own. We can often see thing but not hear or touch 
# them. By the same token, we often hear things that we cannot see or touch. Now, in
# contrast, if we can touch something, we likely can see and hear it too--hence the 
# low exclusivity of haptic items.



# SAME PLOT-WISE:
# Barplot of exclusivity percentiles within modalities for Dutch items (as in 
# Van Dantzig et al., 2011, but separately for properties and concepts)

all<-read.csv('all.csv')

allNL = all[!all$main == '',]
allNL$main = levels(droplevels(allNL$main))

concs <- allNL[allNL$cat == 'Concept' & !allNL$normed == 'English' & !allNL$main == '',]
props <- allNL[allNL$cat == 'Property' & !allNL$normed == 'English' & !allNL$main == '',]

concs$main = levels(droplevels(as.factor(concs$main)))
props$main = levels(droplevels(as.factor(props$main)))
concs$main = as.factor(concs$main)
props$main = as.factor(props$main)

allNL$catmain <- with(allNL, interaction(cat,  main))
str(allNL$catmain)

allNL$section = floor(allNL$Exclusivity * 4)  
table(allNL$section)
str(allNL$section)
table(allNL$section)  # order = 01234
allNL$section = as.factor(allNL$section)
revalue(allNL$section, c("0"="0-20%", "1"="20-40%", "2"="40-60%", "3"="60-80%", 
"4"="80-100%"))
allNL$section = mapvalues(allNL$section, from = c(0, 1, 2, 3, 4), to = c("0-20%", 
"20-40%", "40-60%", "60-80%", "80-100%"))
table(allNL$section)
str(allNL$section)

counts <- table(allNL$section, allNL$catmain)
counts
counts = prop.table(counts, 2)

# see plot:
barplot(counts, width=10, main = 'Modality exclusivity of Dutch properties and concepts   
per dominant modality (Y axis = n)      ', legend = rownames(counts), xlim=c(0,100), 
axes=FALSE, args.legend = list(x = "topright", bty = "n", inset=c(.1, .2)))

# ! THE PLOT IS SHOWN BADLY ON HERE. PLEASE SEE THE SAVED PLOT. 

# Below, run first line, then return and keep running:
png(file="stacked_exc.png", units="in", width=6, height=6, res=1000)
par(mar=c(2,-.3,3,-.3)+.4)  # run twice, if necessary 
barplot(counts, width=10, main = 'Modality exclusivity of Dutch properties and concepts   
per dominant modality (Y axis = n)      ', legend = rownames(counts), xlim=c(0,100), 
axes=FALSE, args.legend = list(x = "topright", bty = "n", inset=c(.1, .2)), col = gray.colors(5, rev=TRUE))
dev.off()


# Same plot for the English items of Lynott and Connell (of course w/out gustatory 
# or olfactory)

allENG = all[!all$English_Main_Lynott_Connell_2009_2013 == '',]
allENG$English_Main_Lynott_Connell_2009_2013 = levels(droplevels(allENG$English_Main_Lynott_Connell_2009_2013))

allENG$catmain <- with(allENG, interaction(cat,  English_Main_Lynott_Connell_2009_2013))
str(allENG$catmain)

allENG$section = floor(allENG$English_Exclusivity_Lynott_Connell_2009_2013 * 5)
table(allENG$section)
str(allENG$section)
table(allENG$section)  # order = 01234
allENG$section = as.factor(allENG$section)
revalue(allENG$section, c("0"="0-20%", "1"="20-40%", "2"="40-60%", "3"="60-80%", 
"4"="80-100%"))
allENG$section = mapvalues(allENG$section, from = c(0, 1, 2, 3, 4), to = c("0-20%", 
"20-40%", "40-60%", "60-80%", "80-100%"))
table(allENG$section)
str(allENG$section)

counts <- table(allENG$section, allENG$catmain)
counts
counts = prop.table(counts, 2)

# below, run first line, then return and keep running:
png(file="stacked_English_Exclusivity_Lynott_Connell_2009_2013.png", units="in", width=6, height=6, res=1000)
par(mar=c(2,-.3,3,-.3)+.4)  # run twice, if necessary 
barplot(counts, width=10, main = 'Modality exclusivity of English properties and concepts      
per dominant modality (Y axis = n)      ', legend = rownames(counts), xlim=c(0,100), 
axes=FALSE, args.legend = list(x = "topright", bty = "n", inset=c(.1, .2)), col = gray.colors(5, rev=TRUE))
dev.off()
# See in folder and compare.


# Comparison English Dutch on exclusivity
# Properties
t.test(props$English_Exclusivity_Lynott_Connell_2009_2013, mu = 0.40)
# The difference is considerable, t(734) = 18.8, p < .001
# dz = t/vn = 0.47

# Concepts
t.test(concs$English_Exclusivity_Lynott_Connell_2009_2013, mu = 0.29)
# The difference is considerable, t(734) = 18.8, p < .001
# dz = t/vn = 0.83
# ___________________________________________________________________________









# RELATION AMONG MODALITIES

# Below is a Principal Components Analysis (PCA) with plots. Firstly it is performed 
# on the Dutch norms, and then on Lynott and Connell's (2009, 2013) English norms 
# (leaving out gustatory and olfactory scores and words). 

all <- read.csv('all.csv')
nrow(all)   # 747 used in Dutch norms + 12 from English not used

# ON ENGLISH NORMS
# PCA plotting on the English norms, as based on Lynott and Connell's supplementary materials 
# (http://www.lancaster.ac.uk/people/connelll/lab/norms.html). 


# ENGLISH PROPERTIES

# check conditions for a PCA
# matrix

eng_prop <- all[all$cat == 'Property', c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013')]
nrow(eng_prop)
eng_prop_matrix <- cor(eng_prop, use = 'complete.obs')
eng_prop_matrix
round(eng_prop_matrix, 2)
# OK: correlations good for a PCA, with enough < .3

# now on the raw vars:
nrow(eng_prop)
cortest.bartlett(eng_prop)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
KMO(eng_prop_matrix)
# Result: .56 = mediocre. PCA not strongly recommended. But we still do it
# because the purpose is graphical only.

# check determinant
det(eng_prop_matrix)
# GOOD: > 0.00001

# start off with unrotated PCA
pc1_eng_prop <- psych::principal(eng_prop, nfactors = 3, rotate = "none")
pc1_eng_prop
# RESULT: Extract either one PC, acc to Kaiser's criterion, or two RCs, acc to 
# Joliffe's (Field, Miles, & Field, 2012)

# Unrotated: scree plot
plot(pc1_eng_prop$values, type = "b")
# Result: again one or two RCs should be extracted

# Now with varimax rotation, Kaiser-normalized (by default)
pc2_eng_prop <- psych::principal(eng_prop, nfactors = 2, rotate = "varimax", 
scores = TRUE)
pc2_eng_prop
pc2_eng_prop$loadings
# two components are good, as they both have eigenvalues over 1

pc2_eng_prop$residual
pc2_eng_prop$fit
pc2_eng_prop$communality
# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation
# (by default in psych::stats). Residuals bad: more than 50% have absolute 
# values > 0.05. Model fit good, > .90. Communalities good, all > .7. 

# subset and add PCs
eng_props <- all[all$cat == 'Property', ]
nrow(eng_props)
eng_props <- cbind(eng_props, pc2_eng_prop$scores)
nrow(eng_props)

head(eng_props)

# Finally, plot

# Set sample words to show on plot (first word in each modality)
auditory_w = as.character(sort(eng_props[eng_props$English_Main_Lynott_Connell_2009_2013=='Auditory', 'English_word_Lynott_Connell_2009_2013'])[1])
haptic_w = as.character(sort(eng_props[eng_props$English_Main_Lynott_Connell_2009_2013=='Haptic', 'English_word_Lynott_Connell_2009_2013'])[1])
visual_w = as.character(sort(eng_props[eng_props$English_Main_Lynott_Connell_2009_2013=='Visual', 'English_word_Lynott_Connell_2009_2013'])[1])
w_set = c(auditory_w, haptic_w, visual_w)

Engprops <- ggplot(eng_props,
  aes(RC1, RC2, label = as.character(English_Main_Lynott_Connell_2009_2013))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set, 12, 7),
	fontface = ifelse(eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set, 'bold', 'plain')) +
  geom_point(data=eng_props[eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  labs(subtitle='(Data from Lynott & Connell, 2009)', x = "Varimax-rotated Principal Component 1", 
	y = "Varimax-rotated Principal Component 2") +	theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = eng_props[eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set,], aes(label = English_word_Lynott_Connell_2009_2013), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )

plot(Engprops)  # ! THE PLOT IS SHOWN BADLY ON HERE. PLEASE SEE THE SAVED PLOTS

# Now to save, run first line below and return to keep running. See your folder.
#png(file="Engprops_highres.png", units="in", width=13, height=13, res=900)
#plot(Engprops)
#dev.off()

# Adjust for combined plots:

Engprops4 <- ggplot(eng_props,
  aes(RC1, RC2, label = as.character(English_Main_Lynott_Connell_2009_2013))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set, 12, 7),
	fontface = ifelse(eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set, 'bold', 'plain')) +
  geom_point(data=eng_props[eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('English properties') + 
  labs(subtitle='(Data from Lynott & Connell, 2009)', x = "", 
	y = "Varimax-rotated Principal Component 2") +	theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = eng_props[eng_props$English_word_Lynott_Connell_2009_2013 %in% w_set,], aes(label = English_word_Lynott_Connell_2009_2013), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )




# ENGLISH CONCEPTS

# check conditions for a PCA
# matrix
eng_conc <- all[all$cat == 'Concept', c('English_Auditory_Lynott_Connell_2009_2013', 'English_Haptic_Lynott_Connell_2009_2013', 'English_Visual_Lynott_Connell_2009_2013')]
nrow(eng_conc)
eng_conc_matrix <- cor(eng_conc, use = 'complete.obs')
eng_conc_matrix
round(eng_conc_matrix, 2)
# POOR: correlations not apt for a PCA, with too many below .3

# now on the raw data:
nrow(eng_conc)
cortest.bartlett(eng_conc)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
KMO(eng_conc_matrix)
# Result: .48 = poor. PCA not strongly recommended. But we still do it
# because the purpose is graphical really.

# check determinant
det(eng_conc_matrix)
# GOOD: > 0.00001

# start off with unrotated PCA
pc1_eng_conc <- psych::principal(eng_conc, nfactors = 3, rotate = "none")
pc1_eng_conc
# RESULT: Extract either one PC, acc to Kaiser's criterion, or two RCs, acc to 
# Joliffe's (Field, Miles, & Field, 2012)

# Unrotated: scree plot
plot(pc1_eng_conc$values, type = "b")
# Result: two PCs obtain.

# Now with varimax rotation, Kaiser-normalized (by default):
# always preferable because it captures explained variance best. 
pc2_eng_conc <- psych::principal(eng_conc, nfactors = 2, rotate = "varimax", 
scores = TRUE)
pc2_eng_conc
pc2_eng_conc$loadings


pc2_eng_conc$residual
pc2_eng_conc$fit
pc2_eng_conc$communality
# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation 
# (by default in psych::stats). Residuals bad: over 50% have absolute 
# values > 0.05. Model fit good, > .90. Communalities good, all > .7.

# subset and add PCs
eng_concs <- all[all$cat == 'Concept', ]
nrow(eng_concs)
eng_concs <- cbind(eng_concs, pc2_eng_conc$scores)
summary(eng_concs$RC1, eng_concs$RC2)
eng_concs <- eng_concs[eng_concs$normed == 'Dut_Eng' | eng_concs$normed == 
'English',]
nrow(eng_concs)
summary(eng_concs$RC1, eng_concs$RC2)


# Finally, plot

# Set sample words to show on plot (first word in each modality)
auditory_w = as.character(sort(eng_concs[eng_concs$English_Main_Lynott_Connell_2009_2013=='Auditory', 'English_word_Lynott_Connell_2009_2013'])[1])
haptic_w = as.character(sort(eng_concs[eng_concs$English_Main_Lynott_Connell_2009_2013=='Haptic', 'English_word_Lynott_Connell_2009_2013'])[1])
visual_w = as.character(sort(eng_concs[eng_concs$English_Main_Lynott_Connell_2009_2013=='Visual', 'English_word_Lynott_Connell_2009_2013'])[1])
w_set = c(auditory_w, haptic_w, visual_w)

Engconcs <- ggplot(eng_concs,
  aes(RC1, RC2, label = as.character(English_Main_Lynott_Connell_2009_2013))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(eng_concs$English_word_Lynott_Connell_2009_2013 %in% w_set, 12, 7),
	fontface = ifelse(eng_concs$English_word_Lynott_Connell_2009_2013 %in% w_set, 'bold', 'plain')) +
  geom_point(data=eng_concs[eng_concs$English_word_Lynott_Connell_2009_2013 %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('English concepts') + 
  labs(subtitle='(Data from Lynott & Connell, 2013)', x = "Varimax-rotated Principal Component 1", 
	y = "Varimax-rotated Principal Component 2") +	theme_bw() +
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = eng_concs[eng_concs$English_word_Lynott_Connell_2009_2013 %in% w_set,], aes(label = English_word_Lynott_Connell_2009_2013), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )

Engconcs  # ! THE PLOT IS SHOWN BADLY ON HERE. PLEASE SEE THE SAVED PLOTS

# Now to save, run first line below and return to keep running. See your folder.
#png(file="Engconcs_highres.png", units="in", width=13, height=13, res=900)
#plot(Engconcs)
#dev.off()




# ON DUTCH NORMS

# Properties
# check conditions for a PCA

# matrix
Property <- all[all$cat == 'Property' & !is.na(all$word), c('Auditory', 'Haptic', 'Visual')]
nrow(Property)
prop_matrix <- cor(Property, use = 'complete.obs')
prop_matrix
round(prop_matrix, 2)
# POOR: correlations not apt for a PCA, with too many below .3

# now on the raw vars:
nrow(Property)
cortest.bartlett(Property)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
KMO(prop_matrix)
# Result: .56 = mediocre. PCA not strongly recommended. But we still do it
# because the purpose is graphical only.

# check determinant
det(prop_matrix)
# GOOD: > 0.00001

# start off with unrotated PCA
pc1_prop <- psych::principal(Property, nfactors = 3, rotate = "none")
pc1_prop
# RESULT: Only PC1, with eigenvalue > 1, should be extracted, 
# acc to Kaiser's criterion (Jolliffe's threshold of 0.7 way too lax; 
# Field, Miles, & Field, 2012)

# Unrotated: scree plot
plot(pc1_prop$values, type = "b")
# Result: one or two RCs should be extracted, converging with eigenvalues

# Now with varimax rotation, Kaiser-normalized (by default). 
# Always preferable because it captures explained variance best. 
# Compare eigenvalues w/ 1 & 2 factors

pc2_prop <- psych::principal(Property, nfactors = 2, rotate = "varimax", scores = TRUE)
pc2_prop
pc2_prop$loadings
# good to extract 2 factors, as they both explain quite the same variance,
# and both surpass 1 eigenvalue


pc2_prop$residual
pc2_prop$fit
pc2_prop$communality
# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation
# (by default in psych::stats). Residuals OK: fewer than 50% have absolute 
# values > 0.05 (exactly 50% do).Model fit good, > .90. 
# Communalities good, all > .7 (av = .83). 

# subset and add PCs
props <- all[all$cat == 'Property' & !is.na(all$word), ]
nrow(props)
props <- cbind(props, pc2_prop$scores)
nrow(props)

# Finally, plot: letters+density (cf. Lynott & Connell, 2009, 2013)

# Set sample words to show on plot (first word in each modality)
auditory_w = as.character(sort(props[props$main=='Auditory', 'word'])[1])
haptic_w = as.character(sort(props[props$main=='Haptic', 'word'])[1])
visual_w = as.character(sort(props[props$main=='Visual', 'word'])[1])
w_set = c(auditory_w, haptic_w, visual_w)

NLprops <- ggplot(props,
  aes(RC1, RC2, label = as.character(main))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(props$word %in% w_set, 12, 7),
	fontface = ifelse(props$word %in% w_set, 'bold', 'plain')) +
  geom_point(data=props[props$word %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('Dutch properties') +
  labs(subtitle='', x = "Varimax-rotated Principal Component 1", 
	y = "Varimax-rotated Principal Component 2") +	theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = props[props$word %in% w_set,], aes(label = word), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )

NLprops  # ! THE PLOT IS SHOWN BADLY ON HERE. PLEASE SEE THE SAVED PLOTS


# Now to save, run first line below and return to keep running. See your folder.
#png(file="NLprops_highres.png", units="in", width=13, height=13, res=900)
#plot(NLprops)
#dev.off()


# Adjust for combined plots:

NLprops2 <- ggplot(props,
  aes(RC1, RC2, label = as.character(main))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(props$word %in% w_set, 12, 7),
	fontface = ifelse(props$word %in% w_set, 'bold', 'plain')) +
  geom_point(data=props[props$word %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('Dutch properties') +  labs(subtitle='', x = "", y = "") +  theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = props[props$word %in% w_set,], aes(label = word), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )


# Next:

NLprops4 <- ggplot(props,
  aes(RC1, RC2, label = as.character(main))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(props$word %in% w_set, 12, 7),
	fontface = ifelse(props$word %in% w_set, 'bold', 'plain')) +
  geom_point(data=props[props$word %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('Dutch properties') +  labs(subtitle='', x = "", y = "") +  theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = props[props$word %in% w_set,], aes(label = word), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )




# CONCEPTS

# check conditions for a PCA
# matrix
Concept <- all[all$cat == 'Concept' & !is.na(all$word), c('Auditory', 'Haptic', 'Visual')]
nrow(Concept)
conc_matrix <- cor(Concept, use = 'complete.obs')
conc_matrix
round(conc_matrix, 2)
# POOR: correlations not apt for a PCA, with too many below .3

# now on the raw data:
nrow(Concept)
cortest.bartlett(Concept)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
KMO(conc_matrix)
# Result: .49 = poor. PCA not strongly recommended. But we still do it
# because the purpose is graphical really.

# check determinant
det(conc_matrix)
# GOOD: > 0.00001

# start off with unrotated PCA
pc1_conc <- psych::principal(Concept, nfactors = 3, rotate = "none")
pc1_conc
# RESULT good: PC1 and PC2, with eigenvalue > 1, should be extracted, 
# acc to Kaiser's criterion (Jolliffe's threshold of 0.7 way too lax; 
# Field, Miles, & Field, 2012)

# Unrotated: scree plot
plot(pc1_conc$values, type = "b")
# Result: with no point of inflexion along the y axis, two PCs would obtain.

# Now with varimax rotation, Kaiser-normalized (by default):
# Always preferable because it captures explained variance best. 
# Compare eigenvalues w/ 1 & 2 Principal Components

pc2_conc <- psych::principal(Concept, nfactors = 2, rotate = "varimax", scores = TRUE)
pc2_conc
pc2_conc$loadings

# good to extract 2 Principal Components, as they both explain quite the same variance, 
# and both surpass 1 eigenvalue

pc2_conc$residual
pc2_conc$fit
pc2_conc$communality
# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation
# (by default in psych::stats). Residuals bad: over 50% have absolute 
# values > 0.05. Model fit good, > .90. Communalities good, all > .7 (av = .82). 

# subset and add PCs
concs <- all[all$cat == 'Concept' & !is.na(all$word), ]
nrow(concs)
concs <- cbind(concs, pc2_conc$scores)
nrow(concs)

# Finally, plot

# Set sample words to show on plot (first word in each modality)
auditory_w = as.character(sort(concs[concs$main=='Auditory', 'word'])[1])
haptic_w = as.character(sort(concs[concs$main=='Haptic', 'word'])[1])
visual_w = as.character(sort(concs[concs$main=='Visual', 'word'])[1])
w_set = c(auditory_w, haptic_w, visual_w)

NLconcs <- ggplot(concs,
  aes(RC1, RC2, label = as.character(main))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(concs$word %in% w_set, 12, 7),
	fontface = ifelse(concs$word %in% w_set, 'bold', 'plain')) +
  geom_point(data=concs[concs$word %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('Dutch concepts') +
  labs(subtitle='', x = "Varimax-rotated Principal Component 1", y = "") +  theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = concs[concs$word %in% w_set,], aes(label = word), size = 8, 
	alpha = 0.77, color = 'black', box.padding = 1.5 )

NLconcs  # ! THE PLOT IS SHOWN BADLY ON HERE. PLEASE SEE THE SAVED PLOTS

# Now to save, run first line below and return to keep running. See your folder.
#png(file="NLconcs_highres.png", units="in", width=13, height=13, res=900)
#plot(NLconcs)
#dev.off()


# Adjust for combined plots:

NLconcs2 <- ggplot(concs,
  aes(RC1, RC2, label = as.character(main))) + stat_density2d (color = "gray87") +
  geom_text(size = ifelse(concs$word %in% w_set, 12, 7),
	fontface = ifelse(concs$word %in% w_set, 'bold', 'plain')) +
  geom_point(data=concs[concs$word %in% w_set,], pch=21, fill=NA, size=14, stroke=2, alpha=.6) +
  ggtitle('Dutch concepts') +
  labs(subtitle='', x = "Varimax-rotated Principal Component 1", y = "") +  theme_bw() +   
  theme( plot.background = element_blank(), panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(), panel.border = element_blank(),
  	axis.line = element_line(color = 'black'),
	axis.title.x = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.title.y = element_text(colour = 'black', size = 23, margin=margin(15,15,15,15)),
	axis.text.x = element_text(size=16), axis.text.y  = element_text(size=16),
	plot.title = element_text(hjust = 0.5, size = 32, face = "bold", margin=margin(15,15,15,15)),
	plot.subtitle = element_text(hjust = 0.5, size = 20, margin=margin(2,15,15,15)) ) +
  geom_label_repel(data = concs[concs$word %in% w_set,], aes(label = word), size = 8,
	alpha = 0.77, color = 'black', box.padding = 1.5 )




# Combined plots:

# Below, run first line, get back and run next.  
# High resolution (may be changed at 'res='). Beware of high memory usage.

png(file="allfour_highres.png", units="in", width=18, height=18, res=1000)
multiplot(Engprops4, Engconcs, NLprops4, NLconcs2, cols = 2)
dev.off()

png(file="allfour_lowres.png", units="in", width=18, height=18, res=200)
multiplot(Engprops4, Engconcs, NLprops4, NLconcs2, cols = 2)
dev.off()

png(file="proppair_highres.png", units="in", width=18, height=9, res=1000)
multiplot(Engprops, NLprops2, cols = 2)
dev.off()

png(file="concpair_highres.png", units="in", width=18, height=9, res=1000)
multiplot(Engconcs, NLconcs2, cols = 2)
dev.off()

# Find all plots in your working directory

# With a naked eye, one can see the different relationships. The significance of 
# these comparisons is notable. First, it demonstrates visually the difference 
# between modality exclusivity and each of the modality strengths (which of course 
# is only natural considering how modality exclusivity was calculated). The two 
# variables then must be different indeed because in the exclusivity analysis, the 
# visual and the auditory modalities were the most similar ones, with their higher 
# exclusivities. In contrast, in the independent strengths analysis, the visual and 
# the haptic modalities show a clear interlock, which leaves the auditory experience 
# rather on its own.
# _____________________________________________________________________________________







# ICONICITY

# Last tests: iconicity/sound symbolism on concepts and properties separately.
# Regressions include same lexical vars (DVs) as Lynott and Connell, plus 
# concreteness and age of acquisition.

# Note that the selection is based on p-value thresholds, as in L&C, but also on
# AIC, which is a bayesian, relative method more appropriate with such a large
# sample. Importantly, AIC and F/p-value criteria resulted in the same inclusions 
# and exclusions for every regression.

# For both props and concs, we start with PCA with all lexical variables in order
# to isolate them, because they are intercorrelated (see Table 5 in Lynott & Connell,
# 2013)

all <- read.csv('all.csv')
nrow(all)
# Length is 759 but only 747 are from these norms. Rest are from Lynott and Connell 
# (2009, 2013) for comparative analyses. These extra items do not have an id number 
# in the file. 

# ----------------------------------------------------------------------------------


# Iconicity within properties alone, as in Lynott and Connell (2013). As a novelty, 
# the iconicity analysis is hereby performed also on the Dutch properties, in 
# addition to the concepts.

props <- subset(all, subset = cat == 'Property')
nrow(props)

# There aren't lexical data for every single word.
# Number of properties per lexical variable (from the Dutch items only of course)
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$phonemes_DUTCHPOND))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$phon_neighbours_DUTCHPOND))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$orth_neighbours_DUTCHPOND))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$freq_lg10CD_SUBTLEXNL))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$freq_lg10WF_SUBTLEXNL))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$freq_CELEX_lem))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$AoA_Brysbaertetal2014))
describe(complete.cases(props[complete.cases(props$Exclusivity),]
$concrete_Brysbaertetal2014))

# M, SD
stat.desc(props$letters)
stat.desc(props$phonemes_DUTCHPOND)
stat.desc(props$phon_neighbours_DUTCHPOND)
stat.desc(props$orth_neighbours_DUTCHPOND)
stat.desc(props$freq_lg10CD_SUBTLEXNL)
stat.desc(props$freq_lg10WF_SUBTLEXNL)
stat.desc(props$freq_CELEX_lem)
stat.desc(props$AoA_Brysbaertetal2014)
stat.desc(props$concrete_Brysbaertetal2014)


# See and print correlation of all lexical variables:

mat_lexicals_props <- as.matrix(props[c('letters', 'phonemes_DUTCHPOND', 
'orth_neighbours_DUTCHPOND', 'phon_neighbours_DUTCHPOND', 'freq_lg10CD_SUBTLEXNL', 
'freq_lg10WF_SUBTLEXNL', 'freq_CELEX_lem', 'AoA_Brysbaertetal2014', 
'concrete_Brysbaertetal2014')])

rcor.test(mat_lexicals_props, use='complete.obs')
corrs_props = rcor.test(mat_lexicals_props, use='complete.obs')
#write.csv(corrs_props$cor.mat, file = "corrs_props.csv",na="") # find table in folder
# (saved just for the manuscript)


# Go on to PCA. This PCA does not include age of acquisition or concreteness, to allow a 
# better comparison with the English data, and because no correlations > .7 (i.e. half 
# of variance explained)

lexicals_props <- props[c('letters', 'phonemes_DUTCHPOND', 'orth_neighbours_DUTCHPOND', 
'phon_neighbours_DUTCHPOND', 'freq_lg10CD_SUBTLEXNL', 'freq_lg10WF_SUBTLEXNL', 
'freq_CELEX_lem')]

str(lexicals_props)

# start with PCA for lexical variables, done as in Lynott and Connell (2013)
# Check conditions for a PCA
# Correlations

cor(lexicals_props, use = 'complete.obs')

# Result: all variables fit for PCA, as they have few scores below .3 
# The correlations broadly replicate Lynott and Connell. 

# now on the raw vars:
cortest.bartlett(lexicals_props)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
lexicals_props_matrix <- cor(lexicals_props, use = 'complete.obs')
KMO(lexicals_props_matrix)
# Result: .78 = good.

# determinant
det(lexicals_props_matrix)
# GOOD: above 0.00001

# start off with unrotated PCA

PCA_lexicals_props <- psych::principal(lexicals_props, nfactors = 7, scores = TRUE)
PCA_lexicals_props
# By all standards, extract 3 components


# scree analysis
plot(PCA_lexicals_props$values, type = "b")
# result: again, extract 3 components


PCA_lexicals_props <- psych::principal(lexicals_props, nfactors = 3, rotate = 
"varimax", scores = TRUE)

PCA_lexicals_props  # eigenvalues and exp variances good
PCA_lexicals_props$loadings

# The PCA replicates Lynott and Connell. Standdized correlation coeffs
# between each PC and its corresponding set of variables are all above .89,
# while the rest of coefficients are all below .33. 

PCA_lexicals_props
# RC1 = length // RC2 = frequency // RC3 = distinctiveness

PCA_lexicals_props$residual
PCA_lexicals_props$fit
# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation
# (by default in psych::stats pack). Residuals good: less than half w/ absolute 
# values > 0.05. Model fit good, > .90. Communalities (h2) good, all well > .7

props <- cbind(props, PCA_lexicals_props$scores)



# REGRESSION

# standardize (mean-center and scale)
props$s_Auditory <- scale(props$Auditory)
props$s_Haptic <- scale(props$Haptic)
props$s_Visual <- scale(props$Visual)
props$s_freq_lg10CD_SUBTLEXNL <- scale(props$freq_lg10CD_SUBTLEXNL)
props$s_freq_lg10WF_SUBTLEXNL <- scale(props$freq_lg10WF_SUBTLEXNL)
props$s_freq_CELEX_lem <- scale(props$freq_CELEX_lem)
props$s_AoA_Brysbaertetal2014 <- scale(props$AoA_Brysbaertetal2014)
props$s_concrete_Brysbaertetal2014 <- scale(props$concrete_Brysbaertetal2014)
props$s_letters <- scale(props$letters)
props$s_phonemes_DUTCHPOND <- scale(props$phonemes_DUTCHPOND)
props$s_orth_neighbours_DUTCHPOND <- scale(props$orth_neighbours_DUTCHPOND)
props$s_phon_neighbours_DUTCHPOND <- scale(props$phon_neighbours_DUTCHPOND)
props$s_RC1_lexicals <- scale(props$RC1)
props$s_RC2_lexicals <- scale(props$RC2) 
props$s_RC3_lexicals <- scale(props$RC3)

# length: letters
fit_letters_props <- lm(props$s_letters ~ props$s_Auditory + props$s_Haptic + 
props$s_Visual, data = props)
stat.desc(fit_letters_props$residuals, norm = TRUE)

# residuals distribution: kurtose. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_letters)
props$log_s_letters <- log(3 + props$s_letters)

fit_letters_props <- lm(props$log_s_letters ~ props$s_Auditory + props$s_Haptic + 
props$s_Visual, data = props)

# check residuals again
stat.desc(fit_letters_props$residuals, norm = TRUE)
# same; go back
fit_letters_props <- lm(props$s_letters ~ props$s_Auditory + props$s_Haptic + 
props$s_Visual, data = props)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_letters_props)
mean(vif(fit_letters_props))
1/vif(fit_letters_props)
# RESULTS: all good

step_letters_props_AIC <- stepAIC(fit_letters_props, direction="both")
step_letters_props_F <- stepAIC(fit_letters_props, direction="both", test="F")
summary(fit_letters_props)


# length: phonemes_DUTCHPOND
fit_phonemes_DUTCHPOND_props <- lm(props$s_phonemes_DUTCHPOND ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_phonemes_DUTCHPOND_props$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_phonemes_DUTCHPOND)
props$log_s_phonemes_DUTCHPOND <- log(3 + props$s_phonemes_DUTCHPOND)

fit_phonemes_DUTCHPOND_props <- lm(props$log_s_phonemes_DUTCHPOND ~ props$s_Auditory
 + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_phonemes_DUTCHPOND_props$residuals, norm = TRUE)
# worse; back
fit_phonemes_DUTCHPOND_props <- lm(props$s_phonemes_DUTCHPOND ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_phonemes_DUTCHPOND_props)
mean(vif(fit_phonemes_DUTCHPOND_props))
1/vif(fit_phonemes_DUTCHPOND_props)
# RESULTS: all good

step_phonemes_DUTCHPOND_props_AIC <- stepAIC(fit_phonemes_DUTCHPOND_props, 
direction="both")
step_phonemes_DUTCHPOND_props_F <- stepAIC(fit_phonemes_DUTCHPOND_props, 
direction="both", test="F")
summary(fit_phonemes_DUTCHPOND_props)


# distinctiveness: orth neigh size
fit_orth_neighbours_DUTCHPOND_props <- lm(props$s_orth_neighbours_DUTCHPOND ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_orth_neighbours_DUTCHPOND_props$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_orth_neighbours_DUTCHPOND)
props$log_s_orth_neighbours_DUTCHPOND <- log(2 + props$s_orth_neighbours_DUTCHPOND)

fit_orth_neighbours_DUTCHPOND_props <- lm(props$log_s_orth_neighbours_DUTCHPOND ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_orth_neighbours_DUTCHPOND_props$residuals, norm = TRUE)
# quite better

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_orth_neighbours_DUTCHPOND_props)
mean(vif(fit_orth_neighbours_DUTCHPOND_props))
1/vif(fit_orth_neighbours_DUTCHPOND_props)
# RESULTS: all good

step_orth_neighbours_DUTCHPOND_props_AIC <- 
stepAIC(fit_orth_neighbours_DUTCHPOND_props, direction="both")

step_orth_neighbours_DUTCHPOND_props_F <- 
stepAIC(fit_orth_neighbours_DUTCHPOND_props, direction="both", test="F")

summary(fit_orth_neighbours_DUTCHPOND_props)


# distinctiveness: phon neigh size
fit_phon_neighbours_DUTCHPOND_props <- lm(props$s_phon_neighbours_DUTCHPOND ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_phon_neighbours_DUTCHPOND_props$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_phon_neighbours_DUTCHPOND)
props$log_s_phon_neighbours_DUTCHPOND <- log(2 + props$s_phon_neighbours_DUTCHPOND)

fit_phon_neighbours_DUTCHPOND_props <- lm(props$log_s_phon_neighbours_DUTCHPOND ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_phon_neighbours_DUTCHPOND_props$residuals, norm = TRUE)
# quite better

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_phon_neighbours_DUTCHPOND_props)
mean(vif(fit_phon_neighbours_DUTCHPOND_props))
1/vif(fit_phon_neighbours_DUTCHPOND_props)
# RESULTS: all good

step_phon_neighbours_DUTCHPOND_props_AIC <- 
stepAIC(fit_phon_neighbours_DUTCHPOND_props, direction="both")
step_phon_neighbours_DUTCHPOND_props_F <-
 stepAIC(fit_phon_neighbours_DUTCHPOND_props, direction="both", test="F")
summary(fit_phon_neighbours_DUTCHPOND_props)


# freq: SUBTLEX-NL log-10 CD

fit_freq_lg10CD_SUBTLEXNL_props <- lm(props$s_freq_lg10CD_SUBTLEXNL ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_freq_lg10CD_SUBTLEXNL_props$residuals, norm = TRUE)

# residuals distribution: skew and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_freq_lg10CD_SUBTLEXNL)
props$log_s_freq_lg10CD_SUBTLEXNL <- log(3 + props$s_freq_lg10CD_SUBTLEXNL)

fit_freq_lg10CD_SUBTLEXNL_props <- lm(props$log_s_freq_lg10CD_SUBTLEXNL ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_freq_lg10CD_SUBTLEXNL_props$residuals, norm = TRUE)
# quite better

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_lg10CD_SUBTLEXNL_props)
mean(vif(fit_freq_lg10CD_SUBTLEXNL_props))
1/vif(fit_freq_lg10CD_SUBTLEXNL_props)
# RESULTS: all good

step_freq_lg10CD_SUBTLEXNL_props_AIC <- stepAIC(fit_freq_lg10CD_SUBTLEXNL_props, 
direction="both")
step_freq_lg10CD_SUBTLEXNL__propsF <- stepAIC(fit_freq_lg10CD_SUBTLEXNL_props, 
direction="both", test="F")
summary(fit_freq_lg10CD_SUBTLEXNL_props)


# freq: SUBTLEX-NL log-10 WF
fit_freq_lg10WF_SUBTLEXNL_props <- lm(props$s_freq_lg10WF_SUBTLEXNL ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_freq_lg10WF_SUBTLEXNL_props$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_freq_lg10WF_SUBTLEXNL)
props$log_s_freq_lg10WF_SUBTLEXNL <- log(3 + props$s_freq_lg10WF_SUBTLEXNL)

fit_freq_lg10WF_SUBTLEXNL_props <- lm(props$log_s_freq_lg10WF_SUBTLEXNL ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_freq_lg10WF_SUBTLEXNL_props$residuals, norm = TRUE)
# quite better

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_lg10WF_SUBTLEXNL_props)
mean(vif(fit_freq_lg10WF_SUBTLEXNL_props))
1/vif(fit_freq_lg10WF_SUBTLEXNL_props)
# RESULTS: all good

step_freq_lg10WF_SUBTLEXNL_props_AIC <- stepAIC(fit_freq_lg10WF_SUBTLEXNL_props, 
direction="both")
step_freq_lg10WF_SUBTLEXNL_props_F <- stepAIC(fit_freq_lg10WF_SUBTLEXNL_props, 
direction="both", test="F")
summary(fit_freq_lg10WF_SUBTLEXNL_props)


# freq: CELEX log-10 lemma WF
fit_freq_CELEX_lem_props <- lm(props$s_freq_CELEX_lem ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_freq_CELEX_lem_props$residuals, norm = TRUE)

# residuals distribution: skew and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_freq_CELEX_lem)
props$log_s_freq_CELEX_lem <- log(3 + props$s_freq_CELEX_lem)

fit_freq_CELEX_lem_props <- lm(props$log_s_freq_CELEX_lem ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_freq_CELEX_lem_props$residuals, norm = TRUE)
# same; go back
fit_freq_CELEX_lem_props <- lm(props$s_freq_CELEX_lem ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_CELEX_lem_props)
mean(vif(fit_freq_CELEX_lem_props))
1/vif(fit_freq_CELEX_lem_props)
# RESULTS: all good

step_freq_CELEX_lem_props_AIC <- stepAIC(fit_freq_CELEX_lem_props, direction="both")
step_freq_CELEX_lem_props_F <- stepAIC(fit_freq_CELEX_lem_props, direction="both", 
test="F")
summary(fit_freq_CELEX_lem_props)


# length: RC1 lexicals
fit_RC1_lexicals_props <- lm(props$s_RC1_lexicals ~ props$s_Auditory + props$s_Haptic 
+ props$s_Visual, data = props)
stat.desc(fit_RC1_lexicals_props$residuals, norm = TRUE)

# residuals distribution: skewed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_RC1_lexicals)
props$log_s_RC1_lexicals_props <- log(4 + props$s_RC1_lexicals)

fit_RC1_lexicals_props <- lm(props$log_s_RC1_lexicals ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_RC1_lexicals_props$residuals, norm = TRUE)
# good!

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC1_lexicals_props)
mean(vif(fit_RC1_lexicals_props))
1/vif(fit_RC1_lexicals_props)
# RESULTS: all good

step_RC1_lexicals_props_AIC <- stepAIC(fit_RC1_lexicals_props, direction="both")
step_RC1_lexicals_props_F <- stepAIC(fit_RC1_lexicals_props, direction="both", 
test="F")
summary(fit_RC1_lexicals_props)


# distinctiveness: RC3 lexicals
fit_RC3_lexicals_props <- lm(props$s_RC3_lexicals ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_RC3_lexicals_props$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_RC3_lexicals)
props$log_s_RC3_lexicals <- log(3 + props$s_RC3_lexicals)

fit_RC3_lexicals_props <- lm(props$log_s_RC3_lexicals ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_RC3_lexicals_props$residuals, norm = TRUE)
# quite better

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC3_lexicals_props)
mean(vif(fit_RC3_lexicals_props))
1/vif(fit_RC3_lexicals_props)
# RESULTS: all good

step_RC3_lexicals_props_AIC <- stepAIC(fit_RC3_lexicals_props, direction="both")
step_RC3_lexicals_props_F <- stepAIC(fit_RC3_lexicals_props, direction="both", 
test="F")
summary(fit_RC3_lexicals_props)


# freq: RC2 lexicals
fit_RC2_lexicals_props <- lm(props$s_RC2_lexicals ~ props$s_Auditory + props$s_Haptic
 + props$s_Visual, data = props)
stat.desc(fit_RC2_lexicals_props$residuals, norm = TRUE)

# residuals distribution: kurtosed. Raw scores/2.SE < 1
# have to log-transform DV and re-run regression

psych::describe(props$s_RC2_lexicals)
props$log_s_RC2_lexicals <- log(3 + props$s_RC2_lexicals)

fit_RC2_lexicals_props <- lm(props$log_s_RC2_lexicals ~ props$s_Auditory + 
props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_RC2_lexicals_props$residuals, norm = TRUE)
# worse; back
fit_RC2_lexicals_props <- lm(props$s_RC2_lexicals ~ props$s_Auditory + props$s_Haptic
 + props$s_Visual, data = props)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC2_lexicals_props)
mean(vif(fit_RC2_lexicals_props))
1/vif(fit_RC2_lexicals_props)
# RESULTS: all good

step_RC2_lexicals_props_AIC <- stepAIC(fit_RC2_lexicals_props, direction="both")
step_RC2_lexicals_props_F <- stepAIC(fit_RC2_lexicals_props, direction="both", 
test="F")
summary(fit_RC2_lexicals_props)


# additional var: age of acquisition
fit_AoA_Brysbaertetal2014_props <- lm(props$s_AoA_Brysbaertetal2014 ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_AoA_Brysbaertetal2014_props$residuals, norm = TRUE)
# residuals distribution: good

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_AoA_Brysbaertetal2014_props)
mean(vif(fit_AoA_Brysbaertetal2014_props))
1/vif(fit_AoA_Brysbaertetal2014_props)
# RESULTS: all good

step_AoA_Brysbaertetal2014_props_AIC <- stepAIC(fit_AoA_Brysbaertetal2014_props,
direction="both")
step_AoA_Brysbaertetal2014_props_F <- stepAIC(fit_AoA_Brysbaertetal2014_props, 
direction="both", test="F")
summary(fit_AoA_Brysbaertetal2014_props)


# additional var: concreteness
fit_concrete_Brysbaertetal2014_props <- lm(props$s_concrete_Brysbaertetal2014 ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)
stat.desc(fit_concrete_Brysbaertetal2014_props$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(props$s_concrete_Brysbaertetal2014)
props$log_s_concrete_Brysbaertetal2014 <- log(4 + props$s_concrete_Brysbaertetal2014)

fit_concrete_Brysbaertetal2014_props <- lm(props$log_s_concrete_Brysbaertetal2014 ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# check residuals again
stat.desc(fit_concrete_Brysbaertetal2014_props$residuals, norm = TRUE)
# worse; back
fit_concrete_Brysbaertetal2014_props <- lm(props$s_concrete_Brysbaertetal2014 ~ 
props$s_Auditory + props$s_Haptic + props$s_Visual, data = props)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_concrete_Brysbaertetal2014_props)
mean(vif(fit_concrete_Brysbaertetal2014_props))
1/vif(fit_concrete_Brysbaertetal2014_props)
# RESULTS: all good

step_concrete_Brysbaertetal2014_props_AIC <- 
stepAIC(fit_concrete_Brysbaertetal2014_props, direction="both")
step_concrete_Brysbaertetal2014_props_F <- 
stepAIC(fit_concrete_Brysbaertetal2014_props, direction="both", test="F")
summary(fit_concrete_Brysbaertetal2014_props)

# RESULTS: iconicity properties: 
# Auditory strength either was the strongest predictor or presented an opposite 
# polarity from the main predictor. This held for all lexical DVs except age of 
# acquisition.



# __________________________________________________________________________

# Iconicity within concepts alone, as in Lynott and Connell (2013)

concs <- all[all$cat == 'Concept' & c(all$normed == 'Dutch' | all$normed == 'Dut_Eng'),]
nrow(concs)

# There aren't lexical data for every single word.
# Percentage of concepts per lexical variable (from items w/ Dutch norms)
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$phonemes_DUTCHPOND))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$phon_neighbours_DUTCHPOND))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$orth_neighbours_DUTCHPOND))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$freq_lg10CD_SUBTLEXNL))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$freq_lg10WF_SUBTLEXNL))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$freq_CELEX_lem))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$AoA_Brysbaertetal2014))
describe(complete.cases(concs[complete.cases(concs$Exclusivity),]
$concrete_Brysbaertetal2014))

# M, SD
stat.desc(concs$letters)
stat.desc(concs$phonemes_DUTCHPOND)
stat.desc(concs$phon_neighbours_DUTCHPOND)
stat.desc(concs$orth_neighbours_DUTCHPOND)
stat.desc(concs$freq_lg10CD_SUBTLEXNL)
stat.desc(concs$freq_lg10WF_SUBTLEXNL)
stat.desc(concs$freq_CELEX_lem)
stat.desc(concs$AoA_Brysbaertetal2014)
stat.desc(concs$concrete_Brysbaertetal2014)


# See and print correlation of all lexical variables:

mat_lexicals_concs <- as.matrix(concs[c('letters', 'phonemes_DUTCHPOND', 
'orth_neighbours_DUTCHPOND', 'phon_neighbours_DUTCHPOND', 'freq_lg10CD_SUBTLEXNL', 
'freq_lg10WF_SUBTLEXNL', 'freq_CELEX_lem', 'AoA_Brysbaertetal2014', 
'concrete_Brysbaertetal2014')])

rcor.test(mat_lexicals_concs, use='complete.obs')
corrs_concs = rcor.test(mat_lexicals_concs, use='complete.obs')
#write.csv(corrs_concs$cor.mat, file = "corrs_concs.csv",na="") # find table in folder


# Go on to PCA. This PCA does not include age of acquisition or concreteness, to allow a 
# better comparison with the English data, and because no correlations > .7 (i.e. half 
# of variance explained)

lexicals_concs <- concs[c('letters', 'phonemes_DUTCHPOND', 'orth_neighbours_DUTCHPOND', 
'phon_neighbours_DUTCHPOND', 'freq_lg10CD_SUBTLEXNL', 'freq_lg10WF_SUBTLEXNL', 
'freq_CELEX_lem')]

nrow(lexicals_concs)

# start with PCA for lexical variables, done as in Lynott and Connell (2013)
# Check conditions for a PCA
# Correlations

cor(lexicals_concs, use = 'complete.obs')

# Result: all variables fit for PCA, as they have few scores below .3 
# The correlations broadly replicate Lynott and Connell. 

# now on the raw vars:
cortest.bartlett(lexicals_concs)
# GOOD: Bartlett's test significant 

# KMO: Kaiser-Meyer-Olkin Measure of Sampling Adequacy
lexicals_concs_matrix <- cor(lexicals_concs, use = 'complete.obs')
KMO(lexicals_concs_matrix)
# Result: .71 = good.

# determinant
det(lexicals_concs_matrix)
# GOOD: above 0.00001

# start off with unrotated PCA

PCA_lexicals_concs <- psych::principal(lexicals_concs, nfactors = 7, scores = TRUE)
PCA_lexicals_concs
# by Kaiser's and Joliffe's standard, extract 3 RCs

# scree analysis
plot(PCA_lexicals_concs$values, type = "b")
# result: again, extract 3 components

PCA_lexicals_concs <- psych::principal(lexicals_concs, nfactors = 3, rotate = 
"varimax", scores = TRUE)

PCA_lexicals_concs #-> check explained variance along components
PCA_lexicals_concs$loadings

# The PCA replicates Lynott and Connell. Standdized correlation coefficients
# between each PC and its corresponding set of variables are all above .89,
# while the rest of coefficients are all below .33. 

PCA_lexicals_concs
# RC1 = length // RC2 = frequency // RC3 = distinctiveness

PCA_lexicals_concs$residual
PCA_lexicals_concs$fit
PCA_lexicals_concs$communality

# Results based on a Kaiser-normalizalized orthogonal (varimax) rotation
# (by default in psych::stats pack). Residuals good: less than half w/ absolute 
# values > 0.05. Model fit good, > .90. Communalities (h2) good, all well > .7

concs <- cbind(concs, PCA_lexicals_concs$scores)


# REGRESSION

# standardize (mean-center and scale)
concs$s_Auditory <- scale(concs$Auditory)
concs$s_Haptic <- scale(concs$Haptic)
concs$s_Visual <- scale(concs$Visual)
concs$s_freq_lg10CD_SUBTLEXNL <- scale(concs$freq_lg10CD_SUBTLEXNL)
concs$s_freq_lg10WF_SUBTLEXNL <- scale(concs$freq_lg10WF_SUBTLEXNL)
concs$s_freq_CELEX_lem <- scale(concs$freq_CELEX_lem)
concs$s_AoA_Brysbaertetal2014 <- scale(concs$AoA_Brysbaertetal2014)
concs$s_concrete_Brysbaertetal2014 <- scale(concs$concrete_Brysbaertetal2014)
concs$s_letters <- scale(concs$letters)
concs$s_phonemes_DUTCHPOND <- scale(concs$phonemes_DUTCHPOND)
concs$s_orth_neighbours_DUTCHPOND <- scale(concs$orth_neighbours_DUTCHPOND)
concs$s_phon_neighbours_DUTCHPOND <- scale(concs$phon_neighbours_DUTCHPOND)
concs$s_RC1_lexicals <- scale(concs$RC1)
concs$s_RC2_lexicals <- scale(concs$RC2) 
concs$s_RC3_lexicals <- scale(concs$RC3)

# length: letters
fit_letters_concs <- lm(concs$s_letters ~ concs$s_Auditory + concs$s_Haptic + 
concs$s_Visual, data = concs)
stat.desc(fit_letters_concs$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_letters)
concs$log_s_letters <- log(3 + concs$s_letters)

fit_letters_concs <- lm(concs$log_s_letters ~ concs$s_Auditory + concs$s_Haptic + 
concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_letters_concs$residuals, norm = TRUE)
# better though still skew/kurtose

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), 
# and tolerance (pref. > 0.2)
vif(fit_letters_concs)
mean(vif(fit_letters_concs))
1/vif(fit_letters_concs)
# RESULTS: all good

step_letters_concs_AIC <- stepAIC(fit_letters_concs, direction="both")
step_letters_concs_F <- stepAIC(fit_letters_concs, direction="both", test="F")
summary(fit_letters_concs)


# length: phonemes_DUTCHPOND
fit_phonemes_DUTCHPOND_concs <- lm(concs$s_phonemes_DUTCHPOND ~ concs$s_Auditory + 
concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_phonemes_DUTCHPOND_concs$residuals, norm = TRUE)

# residuals distribution: skew and kurtose. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_phonemes_DUTCHPOND)
concs$log_s_phonemes_DUTCHPOND <- log(3 + concs$s_phonemes_DUTCHPOND)

fit_phonemes_DUTCHPOND_concs <- lm(concs$log_s_phonemes_DUTCHPOND ~ concs$s_Auditory
 + concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_phonemes_DUTCHPOND_concs$residuals, norm = TRUE)
# good

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_phonemes_DUTCHPOND_concs)
mean(vif(fit_phonemes_DUTCHPOND_concs))
1/vif(fit_phonemes_DUTCHPOND_concs)
# RESULTS: all good

step_phonemes_DUTCHPOND_concs_AIC <- stepAIC(fit_phonemes_DUTCHPOND_concs, 
direction="both")
step_phonemes_DUTCHPOND_concs_F <- stepAIC(fit_phonemes_DUTCHPOND_concs, 
direction="both", test="F")
summary(fit_phonemes_DUTCHPOND_concs)


# distinctiveness: orth neigh size
fit_orth_neighbours_DUTCHPOND_concs <- lm(concs$s_orth_neighbours_DUTCHPOND ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_orth_neighbours_DUTCHPOND_concs$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_orth_neighbours_DUTCHPOND)
concs$log_s_orth_neighbours_DUTCHPOND <- log(2 + concs$s_orth_neighbours_DUTCHPOND)

fit_orth_neighbours_DUTCHPOND_concs <- lm(concs$log_s_orth_neighbours_DUTCHPOND ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_orth_neighbours_DUTCHPOND_concs$residuals, norm = TRUE)
# better though still skew/kurtose

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_orth_neighbours_DUTCHPOND_concs)
mean(vif(fit_orth_neighbours_DUTCHPOND_concs))
1/vif(fit_orth_neighbours_DUTCHPOND_concs)
# RESULTS: all good

step_orth_neighbours_DUTCHPOND_concs_AIC <- 
stepAIC(fit_orth_neighbours_DUTCHPOND_concs, direction="both")
step_orth_neighbours_DUTCHPOND_concs_F <- 
stepAIC(fit_orth_neighbours_DUTCHPOND_concs, direction="both", test="F")
summary(fit_orth_neighbours_DUTCHPOND_concs)


# distinctiveness: phon neigh size
fit_phon_neighbours_DUTCHPOND_concs <- lm(concs$s_phon_neighbours_DUTCHPOND ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_phon_neighbours_DUTCHPOND_concs$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_phon_neighbours_DUTCHPOND)
concs$log_s_phon_neighbours_DUTCHPOND <- log(2 + concs$s_phon_neighbours_DUTCHPOND)

fit_phon_neighbours_DUTCHPOND_concs <- lm(concs$log_s_phon_neighbours_DUTCHPOND ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_phon_neighbours_DUTCHPOND_concs$residuals, norm = TRUE)
# better but not perfect

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_phon_neighbours_DUTCHPOND_concs)
mean(vif(fit_phon_neighbours_DUTCHPOND_concs))
1/vif(fit_phon_neighbours_DUTCHPOND_concs)
# RESULTS: all good

step_phon_neighbours_DUTCHPOND_concs_AIC <- 
stepAIC(fit_phon_neighbours_DUTCHPOND_concs, direction="both")
step_phon_neighbours_DUTCHPOND_concs_F <- stepAIC(fit_phon_neighbours_DUTCHPOND_concs, 
direction="both", test="F")
summary(fit_phon_neighbours_DUTCHPOND_concs)


# freq: SUBTLEX-NL log-10 CD

fit_freq_lg10CD_SUBTLEXNL_concs <- lm(concs$s_freq_lg10CD_SUBTLEXNL ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_freq_lg10CD_SUBTLEXNL_concs$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_freq_lg10CD_SUBTLEXNL)
concs$log_s_freq_lg10CD_SUBTLEXNL <- log(5 + concs$s_freq_lg10CD_SUBTLEXNL)

fit_freq_lg10CD_SUBTLEXNL_concs <- lm(concs$log_s_freq_lg10CD_SUBTLEXNL ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_freq_lg10CD_SUBTLEXNL_concs$residuals, norm = TRUE)
# worse! back 
fit_freq_lg10CD_SUBTLEXNL_concs <- lm(concs$s_freq_lg10CD_SUBTLEXNL ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_lg10CD_SUBTLEXNL_concs)
mean(vif(fit_freq_lg10CD_SUBTLEXNL_concs))
1/vif(fit_freq_lg10CD_SUBTLEXNL_concs)
# RESULTS: all good

step_freq_lg10CD_SUBTLEXNL_concs_AIC <- stepAIC(fit_freq_lg10CD_SUBTLEXNL_concs, 
direction="both")
step_freq_lg10CD_SUBTLEXNL__concsF <- stepAIC(fit_freq_lg10CD_SUBTLEXNL_concs, 
direction="both", test="F")
summary(fit_freq_lg10CD_SUBTLEXNL_concs)


# freq: SUBTLEX-NL log-10 WF
fit_freq_lg10WF_SUBTLEXNL_concs <- 
lm(concs$s_freq_lg10WF_SUBTLEXNL ~ concs$s_Auditory + concs$s_Haptic + concs$s_Visual,
 data = concs)
stat.desc(fit_freq_lg10WF_SUBTLEXNL_concs$residuals, norm = TRUE)
# residuals distribution: good. Raw scores/2.SE < 1

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_lg10WF_SUBTLEXNL_concs)
mean(vif(fit_freq_lg10WF_SUBTLEXNL_concs))
1/vif(fit_freq_lg10WF_SUBTLEXNL_concs)
# RESULTS: all good

step_freq_lg10WF_SUBTLEXNL_concs_AIC <- stepAIC(fit_freq_lg10WF_SUBTLEXNL_concs, 
direction="both")
step_freq_lg10WF_SUBTLEXNL_concs_F <- stepAIC(fit_freq_lg10WF_SUBTLEXNL_concs, 
direction="both", test="F")
summary(fit_freq_lg10WF_SUBTLEXNL_concs)


# freq: CELEX log-10 lemma WF
fit_freq_CELEX_lem_concs <- lm(concs$s_freq_CELEX_lem ~ concs$s_Auditory + 
concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_freq_CELEX_lem_concs$residuals, norm = TRUE)

# residuals distribution: good. Raw scores/2.SE < 1

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_freq_CELEX_lem_concs)
mean(vif(fit_freq_CELEX_lem_concs))
1/vif(fit_freq_CELEX_lem_concs)
# RESULTS: all good

step_freq_CELEX_lem_concs_AIC <- stepAIC(fit_freq_CELEX_lem_concs, direction="both")
step_freq_CELEX_lem_concs_F <- stepAIC(fit_freq_CELEX_lem_concs, direction="both", 
test="F")
summary(fit_freq_CELEX_lem_concs)


# length: RC1 lexicals
fit_RC1_lexicals_concs <- lm(concs$s_RC1_lexicals ~ concs$s_Auditory + concs$s_Haptic 
+ concs$s_Visual, data = concs)
stat.desc(fit_RC1_lexicals_concs$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_RC1_lexicals)
concs$log_s_RC1_lexicals_concs <- log(3 + concs$s_RC1_lexicals)

fit_RC1_lexicals_concs <- lm(concs$log_s_RC1_lexicals ~ concs$s_Auditory + 
concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_RC1_lexicals_concs$residuals, norm = TRUE)
# good

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC1_lexicals_concs)
mean(vif(fit_RC1_lexicals_concs))
1/vif(fit_RC1_lexicals_concs)
# RESULTS: all good

step_RC1_lexicals_concs_AIC <- stepAIC(fit_RC1_lexicals_concs, direction="both")
step_RC1_lexicals_concs_F <- stepAIC(fit_RC1_lexicals_concs, direction="both", 
test="F")
summary(fit_RC1_lexicals_concs)


# distinctiveness: RC3 lexicals
fit_RC3_lexicals_concs <- lm(concs$s_RC3_lexicals ~ concs$s_Auditory + concs$s_Haptic 
+ concs$s_Visual, data = concs)
stat.desc(fit_RC3_lexicals_concs$residuals, norm = TRUE)

# residuals distribution: skewed and kurtosed. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_RC3_lexicals)
concs$log_s_RC3_lexicals <- log(3 + concs$s_RC3_lexicals)

fit_RC3_lexicals_concs <- lm(concs$log_s_RC3_lexicals ~ concs$s_Auditory + 
concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_RC3_lexicals_concs$residuals, norm = TRUE)
# better though still non-normal

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC3_lexicals_concs)
mean(vif(fit_RC3_lexicals_concs))
1/vif(fit_RC3_lexicals_concs)
# RESULTS: all good

step_RC3_lexicals_concs_AIC <- stepAIC(fit_RC3_lexicals_concs, direction="both")
step_RC3_lexicals_concs_F <- stepAIC(fit_RC3_lexicals_concs, direction="both", 
test="F")
summary(fit_RC3_lexicals_concs)


# freq: RC2 lexicals
fit_RC2_lexicals_concs <- lm(concs$s_RC2_lexicals ~ concs$s_Auditory + concs$s_Haptic
 + concs$s_Visual, data = concs)
stat.desc(fit_RC2_lexicals_concs$residuals, norm = TRUE)
# residuals distribution: good. Raw scores/2.SE < 1

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_RC2_lexicals_concs)
mean(vif(fit_RC2_lexicals_concs))
1/vif(fit_RC2_lexicals_concs)
# RESULTS: all good

step_RC2_lexicals_concs_AIC <- stepAIC(fit_RC2_lexicals_concs, direction="both")
step_RC2_lexicals_concs_F <- stepAIC(fit_RC2_lexicals_concs, direction="both", 
test="F")
summary(fit_RC2_lexicals_concs)


# additional var: age of acquisition
fit_AoA_Brysbaertetal2014_concs <- lm(concs$s_AoA_Brysbaertetal2014 ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_AoA_Brysbaertetal2014_concs$residuals, norm = TRUE)
# residuals distribution: good. Raw scores/2.SE < 1

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_AoA_Brysbaertetal2014_concs)
mean(vif(fit_AoA_Brysbaertetal2014_concs))
1/vif(fit_AoA_Brysbaertetal2014_concs)
# RESULTS: all good

step_AoA_Brysbaertetal2014_concs_AIC <- stepAIC(fit_AoA_Brysbaertetal2014_concs,
direction="both")
step_AoA_Brysbaertetal2014_concs_F <- stepAIC(fit_AoA_Brysbaertetal2014_concs, 
direction="both", test="F")
summary(fit_AoA_Brysbaertetal2014_concs)


# additional var: concreteness
fit_concrete_Brysbaertetal2014_concs <- lm(concs$s_concrete_Brysbaertetal2014 ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)
stat.desc(fit_concrete_Brysbaertetal2014_concs$residuals, norm = TRUE)

# residuals distribution: skew. Raw scores/2.SE > 1
# have to log-transform DV and re-run regression

psych::describe(concs$s_concrete_Brysbaertetal2014)
concs$log_s_concrete_Brysbaertetal2014 <- log(3 + concs$s_concrete_Brysbaertetal2014)

fit_concrete_Brysbaertetal2014_concs <- lm(concs$log_s_concrete_Brysbaertetal2014 ~ 
concs$s_Auditory + concs$s_Haptic + concs$s_Visual, data = concs)

# check residuals again
stat.desc(fit_concrete_Brysbaertetal2014_concs$residuals, norm = TRUE)
# good

# Check multicollinearity: largest VIF (pref. < 10), mean VIF (pref. around 1), and 
# tolerance (pref. > 0.2)
vif(fit_concrete_Brysbaertetal2014_concs)
mean(vif(fit_concrete_Brysbaertetal2014_concs))
1/vif(fit_concrete_Brysbaertetal2014_concs)
# RESULTS: all good

step_concrete_Brysbaertetal2014_concs_AIC <- 
stepAIC(fit_concrete_Brysbaertetal2014_concs, direction="both")
step_concrete_Brysbaertetal2014_concs_F <- 
stepAIC(fit_concrete_Brysbaertetal2014_concs, direction="both", test="F")
summary(fit_concrete_Brysbaertetal2014_concs)


# Results: Iconicity of concepts and comparison with properties: 
# The properties sample was characterized by smaller advantages for Auditory 
# predictor, compared to the concepts sample. The tendency of either larger or 
# opposite scores for the Auditory strength was less evident, even though it was 
# still marginally present. This raw-figure difference was not statistically tested.


# END
