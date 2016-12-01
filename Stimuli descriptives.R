setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment')

# Descriptives of the stimuli. They're mainly useful to measure how
# imageable the stimuli were.

library(doBy)
library(pastecs)

stimuli = read.csv('stimulicsv.csv')
str(stimuli)

# After word length and frequency, which are minutely matched across 
# conditions, the most relevant variable may be modality strength.
# Note that this measure outperforms concreteness, as shown in: 
# Connell, L., & Lynott, D. (2012). Strength of perceptual experience 
# predicts word processing performance better than concreteness or 
# imageability. Cognition, 125, 452-465. 

# General modality strength (scores from 0 to 5)
stat.desc(stimuli$mean_mod_strength)   
# RESULT: 2.21. That's below half of maximum

# & for target words per condition:
summaryBy(prop_mod_strength ~ Condition, FUN=stat.desc, 
data=stimuli[stimuli$position=='target',])


# Now general concreteness (scores from 1 to 5; 
# see http://crr.ugent.be/archives/1602)
stat.desc(stimuli$mean_concreteness)

# & for target words per condition:
summaryBy(prop_concreteness ~ Condition, FUN=stat.desc, 
data=stimuli[stimuli$position=='target',])


# Now modality exclusivity (scores from 0 to 1)
stat.desc(stimuli$mean_exclusivity)

# & for target words per condition:
summaryBy(prop_exclusivity ~ Condition, FUN=stat.desc, 
data=stimuli[stimuli$position=='target',])

# In conclusion, the overall imageability of the stimuli is about 
# average, and across conditions it is nearly identical.
#