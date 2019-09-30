#==============================================================================
# 07-Hypothesis1.R
# Purpose: Check Hypothesis1 by comparing follow relationship and quoted URLs
# and create Figure 3 and Table 1
# Author: Keisuke Idemitsu
#==============================================================================

library(dplyr)
library(ggplot2)
library(DBI)
library(stringr)
library(tidyr)
library(texreg)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data/"  ## designate data store folder


############ Step1: Check follow relationship and create Figure 3 ############

load(paste0(data.folder, "all_follow.rdata"))

## Liberal followers
lib_rates <- data.frame(
  category=c("Matome site","Netto-uyoku","Conservative","Media"),
  rate=c(nrow(all_follow[all_follow$matome==0 & all_follow$category=="Liberals",])/nrow(all_follow[all_follow$category=="Liberals",]),
         nrow(all_follow[all_follow$right_wing==0 & all_follow$category=="Liberals",])/nrow(all_follow[all_follow$category=="Liberals",]),
         nrow(all_follow[all_follow$conservative==0 & all_follow$category=="Liberals",])/nrow(all_follow[all_follow$category=="Liberals",]),
         nrow(all_follow[all_follow$media==0 & all_follow$category=="Liberals",])/nrow(all_follow[all_follow$category=="Liberals",])
  )
)
lib_rates$category <- factor(lib_rates$category, levels=unique(lib_rates$category))
lib_rates$percentage <- paste0(round(lib_rates$rate * 100, 1) %>% as.character(), "%")

# create a bar chart
ggplot(lib_rates) + 
  geom_bar(aes(category, rate, fill=category), stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c("#32CD32","#FF1493","Orange","#9ACD32"), guide=FALSE) +
  geom_text(aes(category, rate, label=percentage)) +
  labs(title="Liberal followers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1 ))
ggsave(paste0(figures.folder, "Figure 3-1 Liberals_zero.pdf"), width = 2.5, height = 4)



## Conservative followers
con_rates <- data.frame(
  category=c("Matome site","Media","Liberal","Netto-uyoku"),
  rate=c(nrow(all_follow[all_follow$matome==0 & all_follow$category=="Conservatives",])/nrow(all_follow[all_follow$category=="Conservatives",]),
         nrow(all_follow[all_follow$media==0 & all_follow$category=="Conservatives",])/nrow(all_follow[all_follow$category=="Conservatives",]),
         nrow(all_follow[all_follow$liberal==0 & all_follow$category=="Conservatives",])/nrow(all_follow[all_follow$category=="Conservatives",]),
         nrow(all_follow[all_follow$right_wing==0 & all_follow$category=="Conservatives",])/nrow(all_follow[all_follow$category=="Conservatives",])
  )
)
con_rates$category <- factor(con_rates$category, levels=unique(con_rates$category))
con_rates$percentage <- paste0(round(con_rates$rate * 100, 1) %>% as.character(), "%")

## create a bar chart
ggplot(con_rates) + 
  geom_bar(aes(category, rate, fill=category), stat="identity") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values=c("#32CD32","#9ACD32","#00CED1","#FF1493"), guide=FALSE) +
  geom_text(aes(category, rate, label=percentage)) +
  labs(title="Conservative followers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1 ))
ggsave(paste0(figures.folder, "Figure 3-2 Conservatives_zero.pdf"), width = 2.5, height = 4)



## Netto-uyoku followers
net_rates <- data.frame(
  category=c("Media","Liberal","Matome site","Conservative"),
  rate=c(nrow(all_follow[all_follow$media==0 & all_follow$category=="Netto uyoku",])/nrow(all_follow[all_follow$category=="Netto uyoku",]),
         nrow(all_follow[all_follow$liberal==0 & all_follow$category=="Netto uyoku",])/nrow(all_follow[all_follow$category=="Netto uyoku",]),
         nrow(all_follow[all_follow$matome==0 & all_follow$category=="Netto uyoku",])/nrow(all_follow[all_follow$category=="Netto uyoku",]),
         nrow(all_follow[all_follow$conservative==0 & all_follow$category=="Netto uyoku",])/nrow(all_follow[all_follow$category=="Netto uyoku",])
  )
)
net_rates$category <- factor(net_rates$category, levels=unique(net_rates$category))
net_rates$percentage <- paste0(round(net_rates$rate * 100, 1) %>% as.character(), "%")

## create a bar chart
ggplot(net_rates) + 
  geom_bar(aes(category, rate, fill=category), stat="identity") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_fill_manual(values=c("#9ACD32","#00CED1","#32CD32","Orange"), guide=FALSE) +
  geom_text(aes(category, rate, label=percentage)) +
  labs(title="Netto-uyoku followers") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1 ))
ggsave(paste0(figures.folder, "Figure 3-3 Netto-uyoku_zero.pdf"), width = 2.5, height = 4)

## check statistical significance
all_follow$liberal_0 <- ifelse(all_follow$liberal==0,T,F)
all_follow$conservative_0 <- ifelse(all_follow$conservative==0,T,F)
all_follow$right_wing_0 <- ifelse(all_follow$right_wing==0,T,F)
all_follow$media_0 <- ifelse(all_follow$media==0,T,F)
all_follow$matome_0 <- ifelse(all_follow$matome==0,T,F)

lm1 <- lm(liberal_0 ~ category, data=all_follow[all_follow$category!="Media",])
lm2 <- lm(conservative_0 ~ category, data=all_follow[all_follow$category!="Media",])
lm3 <- lm(right_wing_0 ~ category, data=all_follow[all_follow$category!="Media",])
lm4 <- lm(media_0 ~ category, data=all_follow[all_follow$category!="Media",])
lm5 <- lm(matome_0 ~ category, data=all_follow[all_follow$category!="Media",])

## create table
htmlreg(list(lm1,lm2,lm3,lm4,lm5), file=paste0(figures.folder, "Table lm_follow.doc"),
          digits=2, custom.model.names = c("Liberal","Conservative","Netto-uyoku","Media","Matome"))


############ Step2: Prepare data ############

## pick up data from SQLite database
SQL.path <- paste0(data.folder, "tl_followers.sqlite")
db <- dbConnect(RSQLite::SQLite(), SQL.path)
tw2 <- dbGetQuery(db, "
 SELECT text, screen_name, user_id_str,
 in_reply_to_screen_name, in_reply_to_status_id_str, in_reply_to_user_id_str,
 expanded_url
                  FROM tl_followers")

## merge category data
load(paste0(data.folder, "all_follow.rdata"))
tw2 <- tw2 %>% left_join(all_follow[,c("id_str","category")], by=c("user_id_str"="id_str"))

save(tw2, file=paste0(data.folder, "tw2.rdata"))


############ Step3: Check quoted URLs and create Table 1 ############

## extract row numbers of quote URLs
rows <- setdiff(grep("https://", tw2$expanded_url), grep("https://twitter.com/", tw2$expanded_url))

## add domain names of quoted URLs
tw2$cite_others <- rep(NA, nrow(tw2))
tw2$cite_others[rows] <- sapply(strsplit(tw2$expanded_url[rows], "/"), "[", 3)

## create a new dataframe which contain only tweets quoting URLs
ct_others2 <- tw2[is.na(tw2$cite_others)==F, c("category","cite_others")]

## create a frequancy table for each category and merge them
ct_others2_c <- list()
n <- length(unique(ct_others2$category))  ## number of categories
for (i in 1:n){
  ## create a frequency table of domains
  tmp <- table(ct_others2$cite_others[ct_others2$category==unique(ct_others2$category)[i]])
  tmp <- tmp[order(tmp, decreasing = T)]  ## order by frequency
  ct_others2_c[[i]] <- tmp[1:20] %>% data.frame(stringsAsFactors = F)
}
ct_others2_c <- do.call(cbind, ct_others2_c) %>% data.frame() 

write.csv(ct_others2_c, paste0(figures.folder, "Table 1 URLtable.csv"))
