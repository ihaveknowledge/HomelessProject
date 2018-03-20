#testbed script


head(homelessDataRawDF)
names(homelessDataRawDF)

explore <- homelessDataRawDF[c('hcode', 'period', 'quarter', 'appreason', 'HomelessReason')]


table(explore$appreason)
table(explore$HomelessReason) %>% sort()



explore$reason <- NA

for (i in 1:nrow(explore)){
  if(explore$appreason[i] == "#N/A") {
    explore$reason[i] <- explore$HomelessReason[i]
  } else {
    explore$reason[i] <- explore$appreason[i]
  }
}

explore$reason <- gsub("[.]", "", explore$reason)
explore$reason <- gsub("OTHER REASONS", "OTHER", explore$reason)
explore$reason <- gsub("#N/A", "No Reason Recorded", explore$reason)
explore$reason <- gsub("TERM OF AST", "TERMINATION OF AST", explore$reason)

table(explore$reason) %>% sort()


nrow(explore)
unique(explore) %>% nrow()


head(homelessDataRawDF)
glimpse(homelessDataRawDF)





#outcome by gender

library(dplyr)
library(ggplot2)
library(infer)
library(ggthemes)


homelessDataRawDF$reason <- NA

for (i in 1:nrow(homelessDataRawDF)){
  if(homelessDataRawDF$appreason[i] == "#N/A") {
    homelessDataRawDF$reason[i] <- homelessDataRawDF$HomelessReason[i]
  } else {
    homelessDataRawDF$reason[i] <- homelessDataRawDF$appreason[i]
  }
}

homelessDataRawDF$reason <- gsub("[.]", "", homelessDataRawDF$reason)
homelessDataRawDF$reason <- gsub("OTHER REASONS", "OTHER", homelessDataRawDF$reason)
homelessDataRawDF$reason <- gsub("#N/A", "No Reason Recorded", homelessDataRawDF$reason)
homelessDataRawDF$reason <- gsub("TERM OF AST", "TERMINATION OF AST", homelessDataRawDF$reason)

homeless_stat <- homelessDataRawDF

homeless_stat <- homeless_stat %>% filter(appsname == persname & appfname == perfnames) %>% 
select(hcode, persex, reason, Hhtype) %>% filter(reason != "#N/A") %>% unique()

?rep_sample_n()



homeless_stat <- homeless_stat %>% filter(persex %in% c("MALE", "FEMALE"))

table(homeless_stat$reason) %>% sort()

#Reasons Term of AST
homeless_stat2 <- homeless_stat %>% 
  rep_sample_n(size=nrow(homeless_stat), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "TERMINATION OF AST"),
            prop_reason = mean(reason == "TERMINATION OF AST")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))

homeless_stat2 %>% summarise(q0.05 = quantile(diff_perm, p=0.05),
                             q0.95 = quantile(diff_perm, p=0.95))

homeless_stat2 %>% summarise(mean(diff_perm <= diff_orig))


#with(homeless_stat2,boxplot(diff_perm))

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red') +
  geom_vline(aes(xintercept = quantile(diff_perm, p=0.05)), col = 'blue') +
  geom_vline(aes(xintercept = quantile(diff_perm, p=0.95)), col = 'blue')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red') +
  geom_vline(aes(xintercept = quantile(diff_perm, p=0.05)), col = 'blue') +
  geom_vline(aes(xintercept = quantile(diff_perm, p=0.95)), col = 'blue')
  
homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))


table(homeless_stat$reason) %>% sort()


#Reasons OTHERS CANT ACCOMM-----------------------------

homeless_stat2 <- homeless_stat %>% 
  rep_sample_n(size=nrow(homeless_stat), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "OTHERS CANT ACCOMM"),
            prop_reason = mean(reason == "OTHERS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))



#with(homeless_stat2,boxplot(diff_perm))

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')

homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))










#Reasons PARENTS CANT ACCOMM-----------------------------

homeless_stat2 <- homeless_stat %>% 
  rep_sample_n(size=nrow(homeless_stat), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "PARENTS CANT ACCOMM"),
            prop_reason = mean(reason == "PARENTS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))





#with(homeless_stat2,boxplot(diff_perm))

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))







homeless_stat2 <- homeless_stat %>% filter(Hhtype != "#N/A" & persex != "NOT KNOWN")


lone_parent <- homeless_stat2 %>% filter(Hhtype %in% c("LONE PARENT FEM APP", "LONE PARENT MALE APP"))
with(lone_parent, table(Hhtype, persex))

single_person <- homeless_stat2 %>% filter(Hhtype %in% c("ONE PERSON FEM APP", "ONE PERSON MALE APP"))

with(single_person, table(Hhtype, persex))

table(homeless_stat2$Hhtype) %>% sort()

#lone parent------------------------

homeless_stat2 <- lone_parent %>% 
  rep_sample_n(size=nrow(lone_parent), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "OTHERS CANT ACCOMM"),
            prop_reason = mean(reason == "OTHERS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))






homeless_stat2 <- lone_parent %>% 
  rep_sample_n(size=nrow(lone_parent), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "PARENTS CANT ACCOMM"),
            prop_reason = mean(reason == "PARENTS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))







homeless_stat2 <- lone_parent %>% 
  rep_sample_n(size=nrow(lone_parent), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "TERMINATION OF AST"),
            prop_reason = mean(reason == "TERMINATION OF AST")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))







#single person---------------------


homeless_stat2 <- single_person %>% 
  rep_sample_n(size=nrow(single_person), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "OTHERS CANT ACCOMM"),
            prop_reason = mean(reason == "OTHERS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))






homeless_stat2 <- single_person %>% 
  rep_sample_n(size=nrow(single_person), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "PARENTS CANT ACCOMM"),
            prop_reason = mean(reason == "PARENTS CANT ACCOMM")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))







homeless_stat2 <- single_person %>% 
  rep_sample_n(size=nrow(single_person), reps=1000) %>% 
  mutate(reason_perm = sample(reason)) %>% 
  #group_by(persex) %>% 
  group_by(replicate, persex) %>% 
  summarise(prop_reason_perm = mean(reason_perm == "TERMINATION OF AST"),
            prop_reason = mean(reason == "TERMINATION OF AST")) %>% 
  summarise(diff_perm = diff(prop_reason_perm),
            diff_orig = diff(prop_reason))


ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_dotplot(binwidth=.001) + geom_vline(aes(xintercept = diff_orig), col='red')

ggplot(homeless_stat2, aes(x=diff_perm)) +
  geom_density() + geom_vline(aes(xintercept = diff_orig), col='red')


homeless_stat2 %>% 
  summarise(count = sum(diff_orig <= diff_perm),
            proportion = mean(diff_orig <= diff_perm))


with(single_person, table(persex, reason))
with(lone_parent, table(persex, reason))



