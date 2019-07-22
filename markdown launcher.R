
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality norms')

install.packages('arules')
install.packages('car')
install.packages('contrast')
install.packages('corpcor')
install.packages('doBy')
install.packages('dplyr')
install.packages('flexdashboard')
install.packages('formattable')
install.packages('gdata')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('GPArotation')
install.packages('grid')
install.packages('gridExtra')
install.packages('gtools')
install.packages('Hmisc')
install.packages('irr')
install.packages('kableExtra')
install.packages('knitr')
install.packages('lattice')
install.packages('leaflet')
install.packages('ltm')
install.packages('MASS')
install.packages('pander')
install.packages('pastecs')
install.packages('plotly')
install.packages('plyr')
install.packages('png')
install.packages("processx")
install.packages('psych')
install.packages('qpcR')
install.packages('QuantPsyc')
install.packages('RColorBrewer')
install.packages('RCurl')
install.packages('reshape')
install.packages('Rmisc')
install.packages('rsconnect')
install.packages('scales')
install.packages('shiny')
install.packages('stringr')
install.packages('tibble')
install.packages('tidyr')

library(arules)
library(car)
library(contrast)
library(corpcor)
library(doBy)
library(dplyr)
library(flexdashboard)
library(formattable)
library(gdata)
library(ggplot2)
library(ggrepel)
library(GPArotation)
library(grid)
library(gridExtra)
library(gtools)
library(Hmisc)
library(irr)
library(knitr)
library(lattice)
library(leaflet)
library(ltm)
library(MASS)
library(pander)
library(pastecs)
library(plotly)
library(plyr)
library(png)
library(processx)
library(psych)
library(qpcR)
library(QuantPsyc)
library(RColorBrewer)
library(RCurl)
library(reshape)
library(Rmisc)
library(rsconnect)
library(scales)
library(shiny)
library(stringr)
library(tibble)
library(tidyr)



# Launch dashboard

rmarkdown::run('Dutch-modality-exclusivity-norms.rmd')

rsconnect::deployApp('Dutch-modality-exclusivity-norms.rmd')


# Launch log
showLogs(appPath = getwd(), appFile = NULL, appName = NULL, account = NULL, entries = 100, streaming = FALSE)





# Launch basic notebook with code and output

rmarkdown::render("norms.R", c("word_document", "pdf_document"), output_file = 'Code with output')

# rendering takes a few minutes
# at the end, find norms pdf in your working directory

# In the Word document, it is recommended to widen the horizontal margins.



