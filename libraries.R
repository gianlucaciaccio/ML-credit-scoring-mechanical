library(plyr)
library(readxl) # Import/export excel file
library(caret) # for modeling
library(MASS) # for lda model
library(factoextra) #PCA plot
library(biotools) # Box's M-test for homogeneity of covariance matrices
library(MVN) # Multivariate Normality test (Mardia)
library(MLmetrics)
library(tidyverse) # tools for tidy data
library(cowplot)
library(themis)
library(ROSE)
#### To avoid conflicts among packages ####
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("mutate", "dplyr")
conflicted::conflict_prefer("count", "dplyr")
conflicted::conflict_prefer("summarize", "dplyr")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("arrange", "dplyr")
conflicted::conflict_prefer("rename", "dplyr")
conflicted::conflict_prefer("id", "dplyr")
conflicted::conflict_prefer("lift", "caret")
conflicted::conflict_prefer("compact", "purrr")
conflicted::conflict_prefer("slice", "dplyr")
