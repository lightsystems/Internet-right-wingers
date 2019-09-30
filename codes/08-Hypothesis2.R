#==============================================================================
# 08-Hypothesis2.R
# Purpose: Check Hypothesis2 by comparing the amount of actions
# and create Figure 4 and 5
# Author: Keisuke Idemitsu
#==============================================================================

library(stringr)
library(tidyr)
library(dplyr)
library(circlize)

figures.folder <- "./figures/"  ## designate figures store folder
data.folder <- "./data/"  ## designate data store folder


load(paste0(data.folder, "tw2.rdata"))
load(paste0(data.folder, "freq_accounts.rdata"))


############ Step1-1: Check interactions (retweet) ############

## extract row numbers of retweets
rows <- grep("^RT @[0-9_A-Za-z]+", tw2$text)

## add screen names of retweets destination
tw2$rt_screen_name <- rep(NA, nrow(tw2))
tw2$rt_screen_name[rows] <- str_extract(tw2$text[rows], '^RT @[0-9_A-Za-z]+') %>%
  gsub("^RT @", "", .)

## create a new dataframe which contain only retweets toward opinion leaders
rt_tw2 <- tw2[tolower(tw2$rt_screen_name) %in% tolower(freq_accounts$screen_name),
              c("screen_name","category","rt_screen_name","text")]
## add cluster data
rt_tw2 <- rt_tw2 %>%
  left_join(freq_accounts[,c("screen_name","cluster")], by=c("rt_screen_name"="screen_name"))
rt_tw2 <- na.omit(rt_tw2)

save(rt_tw2, file=paste0(data.folder, "rt_tw2.rdata"))

## spread data for each follower
rt_table <- spread(count(rt_tw2[,c("screen_name","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)
## merge category data
rt_table <- rt_table %>%
  left_join(rt_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")


############ Step1-2: Check interactions (quote retweet) ############

## extract row numbers of quote retweets
rows <- grep("https://twitter.com/", tw2$expanded_url)

## add screen names of quote retweets destination (in lower case)
tw2$cite_twitter <- rep(NA, nrow(tw2))
tw2$cite_twitter[rows] <- sapply(strsplit(tw2$expanded_url[rows], "/"), "[", 4)

## create a new dataframe which contain only quote retweets toward opinion leaders
ct_tw2 <- tw2[tw2$cite_twitter %in% tolower(freq_accounts$screen_name),
              c("category","screen_name","cite_twitter","text")]

## add cluster data
freq_accounts$merge <- tolower(freq_accounts$screen_name)
ct_tw2 <- ct_tw2 %>%
  left_join(freq_accounts[,c("merge","cluster")], by=c("cite_twitter"="merge"))

save(ct_tw2, file=paste0(data.folder, "ct_tw2.rdata"))

## spread data for each follower
ct_table <- spread(count(ct_tw2[,c("screen_name","category","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)

## merge category data
ct_table <- ct_table %>%
  left_join(ct_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")


############ Step1-3: Check interactions (reply) ############

## extract row numbers of replies
rows <- grep("^@[0-9_A-Za-z]+", tw2$text)

## add screen names of replies destination
tw2$mt_screen_name <- rep(NA, nrow(tw2))
tw2$mt_screen_name[rows] <- str_extract(tw2$text[rows], '@[0-9_A-Za-z]+')
tw2$mt_screen_name <- gsub("@", "", tw2$mt_screen_name)

## create a new dataframe which contain only replies toward opinion leaders
mt_tw2 <- tw2[tolower(tw2$mt_screen_name) %in% tolower(freq_accounts$screen_name),
              c("category","screen_name","mt_screen_name","text")]
mt_tw2 <- mt_tw2 %>%
  left_join(freq_accounts[,c("screen_name","cluster")], by=c("mt_screen_name"="screen_name"))

save(mt_tw2, file=paste0(data.folder, "mt_tw2.rdata"))

## spread data for each follower
mt_table <- spread(count(mt_tw2[,c("screen_name","category","cluster")], 
                         screen_name, cluster), cluster, n, fill = 0)

## merge category data
mt_table <- mt_table %>%
  left_join(mt_tw2[,c("screen_name","category")] %>% unique(), by="screen_name")


############ Step2: Compare and create figures ############

## caluculate rate of Liberal followers who do not react
lib_actions <- data.frame(
  followers="Liberal",
  leaders=rep(c("Media","Liberal","Conservative","Netto-uyoku","Matome"),3),
  actions=c(rep("retweet",5),rep("quote retweet",5),rep("reply",5)),
  rate=c(
    nrow(rt_table[rt_table$category=="Liberals" & rt_table$`4`==0,])/nrow(rt_table[rt_table$category=="Liberals",]),
    nrow(rt_table[rt_table$category=="Liberals" & rt_table$`1`==0,])/nrow(rt_table[rt_table$category=="Liberals",]),
    nrow(rt_table[rt_table$category=="Liberals" & rt_table$`2`==0,])/nrow(rt_table[rt_table$category=="Liberals",]),
    nrow(rt_table[rt_table$category=="Liberals" & rt_table$`3`==0,])/nrow(rt_table[rt_table$category=="Liberals",]),
    nrow(rt_table[rt_table$category=="Liberals" & rt_table$`5`==0,])/nrow(rt_table[rt_table$category=="Liberals",]),
    nrow(ct_table[ct_table$category=="Liberals" & ct_table$`4`==0,])/nrow(ct_table[ct_table$category=="Liberals",]),
    nrow(ct_table[ct_table$category=="Liberals" & ct_table$`1`==0,])/nrow(ct_table[ct_table$category=="Liberals",]),
    nrow(ct_table[ct_table$category=="Liberals" & ct_table$`2`==0,])/nrow(ct_table[ct_table$category=="Liberals",]),
    nrow(ct_table[ct_table$category=="Liberals" & ct_table$`3`==0,])/nrow(ct_table[ct_table$category=="Liberals",]),
    nrow(ct_table[ct_table$category=="Liberals" & ct_table$`5`==0,])/nrow(ct_table[ct_table$category=="Liberals",]),
    nrow(mt_table[mt_table$category=="Liberals" & mt_table$`4`==0,])/nrow(mt_table[mt_table$category=="Liberals",]),
    nrow(mt_table[mt_table$category=="Liberals" & mt_table$`1`==0,])/nrow(mt_table[mt_table$category=="Liberals",]),
    nrow(mt_table[mt_table$category=="Liberals" & mt_table$`2`==0,])/nrow(mt_table[mt_table$category=="Liberals",]),
    nrow(mt_table[mt_table$category=="Liberals" & mt_table$`3`==0,])/nrow(mt_table[mt_table$category=="Liberals",]),
    nrow(mt_table[mt_table$category=="Liberals" & mt_table$`5`==0,])/nrow(mt_table[mt_table$category=="Liberals",])
  )
)


## caluculate rate of Conservative followers who do not react
con_actions <- data.frame(
  followers="Conservative",
  leaders=rep(c("Media","Liberal","Conservative","Netto-uyoku","Matome"),3),
  actions=c(rep("retweet",5),rep("quote retweet",5),rep("reply",5)),
  rate=c(
    nrow(rt_table[rt_table$category=="Conservatives" & rt_table$`4`==0,])/nrow(rt_table[rt_table$category=="Conservatives",]),
    nrow(rt_table[rt_table$category=="Conservatives" & rt_table$`1`==0,])/nrow(rt_table[rt_table$category=="Conservatives",]),
    nrow(rt_table[rt_table$category=="Conservatives" & rt_table$`2`==0,])/nrow(rt_table[rt_table$category=="Conservatives",]),
    nrow(rt_table[rt_table$category=="Conservatives" & rt_table$`3`==0,])/nrow(rt_table[rt_table$category=="Conservatives",]),
    nrow(rt_table[rt_table$category=="Conservatives" & rt_table$`5`==0,])/nrow(rt_table[rt_table$category=="Conservatives",]),
    nrow(ct_table[ct_table$category=="Conservatives" & ct_table$`4`==0,])/nrow(ct_table[ct_table$category=="Conservatives",]),
    nrow(ct_table[ct_table$category=="Conservatives" & ct_table$`1`==0,])/nrow(ct_table[ct_table$category=="Conservatives",]),
    nrow(ct_table[ct_table$category=="Conservatives" & ct_table$`2`==0,])/nrow(ct_table[ct_table$category=="Conservatives",]),
    nrow(ct_table[ct_table$category=="Conservatives" & ct_table$`3`==0,])/nrow(ct_table[ct_table$category=="Conservatives",]),
    nrow(ct_table[ct_table$category=="Conservatives" & ct_table$`5`==0,])/nrow(ct_table[ct_table$category=="Conservatives",]),
    nrow(mt_table[mt_table$category=="Conservatives" & mt_table$`4`==0,])/nrow(mt_table[mt_table$category=="Conservatives",]),
    nrow(mt_table[mt_table$category=="Conservatives" & mt_table$`1`==0,])/nrow(mt_table[mt_table$category=="Conservatives",]),
    nrow(mt_table[mt_table$category=="Conservatives" & mt_table$`2`==0,])/nrow(mt_table[mt_table$category=="Conservatives",]),
    nrow(mt_table[mt_table$category=="Conservatives" & mt_table$`3`==0,])/nrow(mt_table[mt_table$category=="Conservatives",]),
    nrow(mt_table[mt_table$category=="Conservatives" & mt_table$`5`==0,])/nrow(mt_table[mt_table$category=="Conservatives",])
  )
)


## caluculate rate of Netto-uyoku followers who do not react
net_actions <- data.frame(
  followers="Netto-uyoku",
  leaders=rep(c("Media","Liberal","Conservative","Netto-uyoku","Matome"),3),
  actions=c(rep("retweet",5),rep("quote retweet",5),rep("reply",5)),
  rate=c(
    nrow(rt_table[rt_table$category=="Netto uyoku" & rt_table$`4`==0,])/nrow(rt_table[rt_table$category=="Netto uyoku",]),
    nrow(rt_table[rt_table$category=="Netto uyoku" & rt_table$`1`==0,])/nrow(rt_table[rt_table$category=="Netto uyoku",]),
    nrow(rt_table[rt_table$category=="Netto uyoku" & rt_table$`2`==0,])/nrow(rt_table[rt_table$category=="Netto uyoku",]),
    nrow(rt_table[rt_table$category=="Netto uyoku" & rt_table$`3`==0,])/nrow(rt_table[rt_table$category=="Netto uyoku",]),
    nrow(rt_table[rt_table$category=="Netto uyoku" & rt_table$`5`==0,])/nrow(rt_table[rt_table$category=="Netto uyoku",]),
    nrow(ct_table[ct_table$category=="Netto uyoku" & ct_table$`4`==0,])/nrow(ct_table[ct_table$category=="Netto uyoku",]),
    nrow(ct_table[ct_table$category=="Netto uyoku" & ct_table$`1`==0,])/nrow(ct_table[ct_table$category=="Netto uyoku",]),
    nrow(ct_table[ct_table$category=="Netto uyoku" & ct_table$`2`==0,])/nrow(ct_table[ct_table$category=="Netto uyoku",]),
    nrow(ct_table[ct_table$category=="Netto uyoku" & ct_table$`3`==0,])/nrow(ct_table[ct_table$category=="Netto uyoku",]),
    nrow(ct_table[ct_table$category=="Netto uyoku" & ct_table$`5`==0,])/nrow(ct_table[ct_table$category=="Netto uyoku",]),
    nrow(mt_table[mt_table$category=="Netto uyoku" & mt_table$`4`==0,])/nrow(mt_table[mt_table$category=="Netto uyoku",]),
    nrow(mt_table[mt_table$category=="Netto uyoku" & mt_table$`1`==0,])/nrow(mt_table[mt_table$category=="Netto uyoku",]),
    nrow(mt_table[mt_table$category=="Netto uyoku" & mt_table$`2`==0,])/nrow(mt_table[mt_table$category=="Netto uyoku",]),
    nrow(mt_table[mt_table$category=="Netto uyoku" & mt_table$`3`==0,])/nrow(mt_table[mt_table$category=="Netto uyoku",]),
    nrow(mt_table[mt_table$category=="Netto uyoku" & mt_table$`5`==0,])/nrow(mt_table[mt_table$category=="Netto uyoku",])
  )
)


## create a graph
### Liberal followers
lib_actions$leaders <- factor(lib_actions$leaders, levels=unique(lib_actions$leaders)) ## order
lib_actions$actions <- factor(lib_actions$actions, levels=unique(lib_actions$actions)) ## order

ggplot(lib_actions)+
  geom_tile(aes(leaders, actions, fill = rate), color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Rate of accounts\nwho do not react",
                       labels = scales::percent) +
  geom_text(aes(leaders, actions, label = paste0(round(rate*100,0), "%")),
            color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )+
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  labs(title="Liberal followers") +
  coord_fixed()
ggsave(paste0(figures.folder, "Figure 4-1 liberal-act.pdf"), width = 6, height = 3)


### Conservative followers
con_actions$leaders <- factor(con_actions$leaders, levels=unique(con_actions$leaders)) ## order
con_actions$actions <- factor(con_actions$actions, levels=unique(con_actions$actions)) ## order

ggplot(con_actions)+
  geom_tile(aes(leaders, actions, fill = rate), color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Rate of accounts\nwho do not react",
                       labels = scales::percent) +
  geom_text(aes(leaders, actions, label = paste0(round(rate*100,0), "%")),
            color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )+
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  labs(title="Conservative followers") +
  coord_fixed()
ggsave(paste0(figures.folder, "Figure 4-2 Conservative-act.pdf"), width = 6, height = 3)


### Netto-uyoku followers
net_actions$leaders <- factor(net_actions$leaders, levels=unique(net_actions$leaders)) ## order
net_actions$actions <- factor(net_actions$actions, levels=unique(net_actions$actions)) ## order

ggplot(net_actions)+
  geom_tile(aes(leaders, actions, fill = rate), color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(0,1), space = "Lab", 
                       name="Rate of accounts\nwho do not react",
                       labels = scales::percent) +
  geom_text(aes(leaders, actions, label = paste0(round(rate*100,0), "%")),
            color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  )+
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  labs(title="Netto-uyoku followers") +
  coord_fixed()
ggsave(paste0(figures.folder, "Figure 4-3 Netto-uyoku-act.pdf"), width = 6, height = 3)




############ Step3-1: Retweet interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "rt_tw2.rdata"))
rt_tw2$cluster <- as.factor(rt_tw2$cluster)  ## convert to factor

## write a table 
write.csv(table(rt_tw2$category, rt_tw2$cluster), file=paste0(figures.folder, "Table 1-1 rt_cluster.csv"))

## spread data to create a plot
rt_cluster2 <- rt_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
rt_cluster2$cluster <- as.factor(rt_cluster2$cluster)
levels(rt_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto-uyoku*", "Media*", "Matome*")
levels(rt_cluster2$category) <- c("Liberal\nfollowers","Media\nfollowers","Conservative\nfollowers",
                                  "Netto-uyoku\nfollowers")

## create a palette to visualise
grid.col2 = c("Liberal\nfollowers"="#00CED1", "Media\nfollowers"="#9ACD32",
              "Conservative\nfollowers"="Orange", "Netto-uyoku\nfollowers"="#FF1493",
              "Liberal*"="#00CED1", "Conservatives*"="Orange", "Netto-uyoku*"="#FF1493",
              "Media*"="#9ACD32", "Matome*"="#32CD32")

pdf(paste0(figures.folder, "Figure 5-1 rt-interactions.pdf"), width = 4, height = 4.5)
chordDiagram(rt_cluster2[rt_cluster2$category != "Media\nfollowers",], 
             grid.col = grid.col2, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(rt_cluster2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
dev.off()



############ Step3-2: Quote retweets interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "ct_tw2.rdata"))
ct_tw2$cluster <- as.factor(ct_tw2$cluster)  ## convert to factor

## write a table 
write.csv(table(ct_tw2$category, ct_tw2$cluster), file=paste0(figures.folder, "Table 1-2 qt_cluster.csv"))

## spread data to create a plot
ct_cluster2 <- ct_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
ct_cluster2$cluster <- as.factor(ct_cluster2$cluster)
levels(ct_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto-uyoku*", "Media*", "Matome*")
levels(ct_cluster2$category) <- c("Liberal\nfollowers","Media\nfollowers","Conservative\nfollowers",
                                  "Netto-uyoku\nfollowers")

pdf(paste0(figures.folder, "Figure 5-2 qt-interactions.pdf"), width = 4, height = 4.5)
chordDiagram(ct_cluster2[ct_cluster2$category != "Media\nfollowers",],
             grid.col = grid.col2, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(ct_cluster2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
dev.off()


############ Step3-3: Replies interaction ############

## load dataframe from Hypothesis 1
load(paste0(data.folder, "mt_tw2.rdata"))
mt_tw2$cluster <- as.factor(mt_tw2$cluster)  ## convert to factor

## write a table
write.csv(table(mt_tw2$category, mt_tw2$cluster), file=paste0(figures.folder, "Table 1-3 rp_cluster.csv"))

## spread data to create a plot
mt_cluster2 <- mt_tw2[,c("category","cluster")] %>% group_by(category, cluster) %>%
  summarise(count=n())
## convert to factor and set names of factor levels
mt_cluster2$cluster <- as.factor(mt_cluster2$cluster)
levels(mt_cluster2$cluster) <- c("Liberal*", "Conservatives*", "Netto-uyoku*", "Media*", "Matome*")
levels(mt_cluster2$category) <- c("Liberal\nfollowers","Media\nfollowers","Conservative\nfollowers",
                                  "Netto-uyoku\nfollowers")

pdf(paste0(figures.folder, "Figure 5-3 rp-interactions.pdf"), width = 4, height = 4.5)
chordDiagram(mt_cluster2[mt_cluster2$category != "Media\nfollowers",],
             grid.col = grid.col2, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(mt_cluster2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)
dev.off()



