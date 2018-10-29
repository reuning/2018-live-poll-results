setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")

rm(list=ls())

files <- dir("data/")
files <- grep("csv", files, value=T)
setwd("data/")
df <- NULL
for(ii in 1:length(files)){
  
  tmp <- read.csv(files[ii], stringsAsFactors = F)
  
  tmp$surv_num <- as.numeric(strsplit(files[ii], "-|\\.")[[1]][4])
  stdist <- strsplit(files[ii], "-|\\.")[[1]][3]
  tmp$stdist <- stdist 
  tmp$st <- substr(stdist, 1,2)
  tmp$dist <- substr(stdist, 3, 4)
  
  df <- plyr::rbind.fill(df, tmp)
}

library(survey)

svy.df <- svydesign(~1, weights=~final_weight, data=df)



df$voted <- df$likely=="Already voted"
df$voted[df$likely=="[DO NOT READ] Don't know/Refused"] <- NA
df$voted <- df$voted*1


tmp <- by(df$voted, df$stdist, sum, na.rm=T)
df <- df[df$stdist %in% names(tmp)[which(tmp > 5)], ]





# library(lme4)
# mod <- glmer(voted~(1|ager) + (1|educ) + (1|race_gender) + (1|response) +(1|stdist), data=df, weights = final_weight, 
#              family=binomial(link="logit"), verbose = T)
# summary(mod)
# 
# dotplot.ranef.mer(ranef(mod, condVar=T))

library(rstanarm)

table(df$age_combined)
df$age_combined[df$age_combined=="[DO NOT READ] Don't know/Refused"] <- NA
table(df$gender_combined)
table(df$race_eth)
df$race_eth[df$race_eth=="[DO NOT READ] Don't know/Refused"] <- NA
table(df$educ)
df$educ[df$educ=="[DO NOT READ] Refused"] <- NA
df$educ[df$educ=="[DO NOT READ] Don't know/Refused"] <- NA
table(df$partyid)
df$partyid[df$partyid=="[DO NOT READ] Don't know/Refused"] <- NA
df$partyid[df$partyid=="[DO NOT READ] Refused"] <- NA
df$partyid[df$partyid=="or as a member of another political party"] <- NA


df$race_gender <- paste(df$race_eth, df$gender_combined, sep="_")
df$race_gender[grepl("NA", df$race_gender)] <- NA

table(df$partyid, df$response)

mod <- stan_glmer(voted~(1|age_combined) + (1|educ) + (1|race_gender) +
                    (1|partyid) +(1|stdist), data=df, weights = final_weight, 
                  family=binomial(link="logit"), cores=2, chains=2, adapt_delta = .95)


coef <- names(mod$coefficients)
plot(mod, pars=coef[34:55])
