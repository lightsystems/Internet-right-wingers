#==============================================================================
# 09-Hypothesis3.R
# Purpose: Analyse texts of tweets by each ideological group
# and create Figure 6
# Author: Keisuke Idemitsu
#==============================================================================

library(quanteda)
library(topicmodels)
library(dplyr)
library(tidyr)
library(ggplot2)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data/"  ## designate data store folder


############ Step1: Data preparation ############
load(paste0(data.folder, "rt_tw2.rdata"))
load(paste0(data.folder, "ct_tw2.rdata"))
load(paste0(data.folder, "mt_tw2.rdata"))

## merge data
targettw <- rbind(rt_tw2[,-3], ct_tw2[,-3], mt_tw2[,-3])

## tokenise texts
tokenstw <- corpus(targettw) %>%
  tokens(remove_punct=TRUE, remove_numbers=TRUE, remove_twitter=TRUE) %>%
  tokens_remove(pattern=c("t.co", "https", "rt", "amp", "http", "t.c", "can")) %>%
  tokens_select(min_nchar = 2)

## create a dfm (document frequency matrix)
dfmtw <- dfm(tokenstw) %>%
  dfm_select('^[０-９ァ-ヶー一-龠]+$', valuetype = 'regex') %>%
  dfm_trim(min_termfreq = 10)


############ Step2: Term frequency ############
source("functions.R")
tstat_key <- textstat_keyness(dfmtw, 
                              target = docvars(dfmtw, "category")=="Liberals")
textplot_keynessJP(tstat_key, n=15, labelsize=6) ## save it manually by size 850*650

tstat_key <- textstat_keyness(dfmtw, 
                              target = docvars(dfmtw, "category")=="Conservatives")
textplot_keynessJP(tstat_key, n=15, labelsize=6) ## save it manually by size 850*650

tstat_key <- textstat_keyness(dfmtw, 
                              target = docvars(dfmtw, "category")=="Netto uyoku")
textplot_keynessJP(tstat_key, n=15, labelsize=6) ## save it manually by size 850*650

