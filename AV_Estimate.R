setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")
library(rstanarm)
library(extrafont)
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
df <- df[df$stdist %in% names(tmp)[which(tmp > 10)], ]
# df <- df[!(df$voted==1 & df$response=="Und"),]

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
df$response[df$response %in% c("3", "4", "5", "6")] <- NA

df$race_gender <- paste(df$race_eth, df$gender_combined, sep="_")
df$race_gender[grepl("NA", df$race_gender)] <- NA

table(df$partyid, df$voted)
table(df$response, df$voted)

mod <- stan_glmer(voted~(1 + partyid|age_combined/educ/race_gender)  +
                    (1|partyid/stdist), data=df, weights = final_weight, 
                  family=binomial(link="logit"), cores=2, chains=2, adapt_delta = .95)


coef <- names(mod$coefficients)
plot(mod, pars=coef[34:55])

pred.df <- df[,c("age_combined", "educ", "race_gender", "partyid")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
pred.df$age_combined <- "New"
# pred.df$response <- "New"

pred.df <-unique(pred.df)


out <- replicate(100, apply(posterior_predict(mod, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$race_gender <- pred.df$race_gender 
plot.df$choice <- pred.df$response
colnames(plot.df)[1:3] <- c("Low", "Median", "High")
library(ggplot2)

setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")

extrafont::loadfonts()
quartzFonts(futura=rep("FuturaBT-Heavy", 4))
quartzFonts(mstthin=rep("Montserrat-Thin", 4))
quartzFonts(mstreg=rep("Montserrat-Regular", 4))
plot.df$race_gender <- gsub("_", " ", plot.df$race_gender)


png("Prob_Voted_Race.png", height=8, width=6,
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=race_gender,  color=choice ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Already Having Voted") +xlab("") + 
  scale_colour_manual(values = c( "#124073", "#B71D1A", "#A8BF14"), 
                      name="Type") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()







pred.df <- df[,c("age_combined", "educ", "race_gender", "response")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
# pred.df$age_combined <- "New"
pred.df$race_gender <- "New"
# pred.df$response <- "New"

pred.df <-unique(pred.df)


out <- replicate(100, apply(posterior_predict(mod, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$age <- pred.df$age_combined 
plot.df$choice <- pred.df$response
colnames(plot.df)[1:3] <- c("Low", "Median", "High")
library(ggplot2)

setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")



png("Prob_Voted_Age.png", height=8, width=6, 
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=age,  color=choice ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Already Having Voted") +xlab("") + 
  scale_colour_manual(values = c( "#124073", "#B71D1A", "#A8BF14"), 
                      name="Type") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()

