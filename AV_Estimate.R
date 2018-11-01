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
df$partyid[df$partyid=="Independent (No party)"] <- "Independent"
df$response[df$response %in% c("3", "4", "5", "6")] <- NA

df$race_gender <- paste(df$race_eth, df$gender_combined, sep="_")
df$race_gender[grepl("NA", df$race_gender)] <- NA

table(df$partyid, df$voted)
table(df$response, df$voted)

mod <- stan_glmer(voted~(1 + partyid|age_combined) + (1 + partyid|educ) + 
                    (1+partyid|race_gender)  +
                    (1|partyid) + (1|stdist), data=df, weights = final_weight, 
                  family=binomial(link="logit"), cores=2, chains=2, adapt_delta = .99)


coef <- names(mod$coefficients)
plot(mod, pars=coef[34:89])

pred.df <- df[,c("age_combined", "educ", "race_gender", "partyid", "final_weight")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
pred.df$age_combined <- "New"
# pred.df$response <- "New"

pct <- by(pred.df$final_weight, pred.df$race_gender, sum, na.rm=T)/sum(pred.df$final_weight)*100
pred.df$final_weight <- NULL 
pred.df <-unique(pred.df)
pred.df$pct <- pct[pred.df$race_gender]

out <- replicate(100, apply(posterior_predict(mod, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$race_gender <- pred.df$race_gender 
plot.df$partyid <- pred.df$partyid
colnames(plot.df)[1:3] <- c("Low", "Median", "High")
library(ggplot2)

setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")

extrafont::loadfonts()
quartzFonts(futura=rep("FuturaBT-Heavy", 4))
quartzFonts(mstthin=rep("Montserrat-Thin", 4))
quartzFonts(mstreg=rep("Montserrat-Regular", 4))
plot.df$race_gender <- gsub("_", " ", plot.df$race_gender)
plot.df$race_gender <- paste(plot.df$race_gender, "\n(", round(pred.df$pct, 2), "%)", sep="")

png("Prob_Voted_Race.png", height=8, width=6,
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=race_gender,  color=partyid ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Having Already Voted") +xlab("") + 
  ggtitle(toupper("Democrats and Independents\nare voting early")) + 
  scale_colour_manual(values = c( "#124073", "#A8BF14", "#B71D1A"), 
                      name="Partisanship") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()







pred.df <- df[,c("age_combined", "educ", "race_gender",  "partyid", "final_weight")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
# pred.df$age_combined <- "New"
pred.df$race_gender <- "New"
# pred.df$response <- "New"

pct <- by(pred.df$final_weight, pred.df$age_combined, sum, na.rm=T)/sum(pred.df$final_weight)*100
pred.df$final_weight <- NULL 
pred.df <-unique(pred.df)
pred.df$pct <- pct[pred.df$age_combined]

pred.df <-unique(pred.df)


out <- replicate(100, apply(posterior_predict(mod, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$age <- pred.df$age_combined 
plot.df$partyid <- pred.df$partyid

plot.df$age <- paste(plot.df$age, "\n(", round(pred.df$pct, 2), "%)", sep="")

colnames(plot.df)[1:3] <- c("Low", "Median", "High")
library(ggplot2)

setwd("/Users/kevinreuning/Dropbox/Projects/2018-live-poll-results/")



png("Prob_Voted_Age.png", height=8, width=6, 
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=age,  color=partyid ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Having Already Voted") +xlab("") + 
  ggtitle(toupper("older voters are most\nlikely to have already voted")) + 
  scale_colour_manual(values = c( "#124073", "#A8BF14", "#B71D1A"), 
                      name="Partisanship") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()




mod2 <- stan_glmer(voted~(1 + response|age_combined) + (1 + response|educ) + 
                    (1+response|race_gender)  +
                    (1|response) + (1|stdist), data=df, weights = final_weight, 
                  family=binomial(link="logit"), cores=2, chains=2, adapt_delta = .99)


coef <- names(mod2$coefficients)
plot(mod2, pars=coef[34:89])

pred.df <- df[,c("age_combined", "educ", "race_gender", "response", "final_weight")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
pred.df$age_combined <- "New"
# pred.df$response <- "New"

pct <- by(pred.df$final_weight, pred.df$race_gender, sum, na.rm=T)/sum(pred.df$final_weight)*100
pred.df$final_weight <- NULL 
pred.df <-unique(pred.df)
pred.df$pct <- pct[pred.df$race_gender]

pred.df <-unique(pred.df)


out <- replicate(100, apply(posterior_predict(mod2, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$race_gender <- pred.df$race_gender 
plot.df$response <- pred.df$response
colnames(plot.df)[1:3] <- c("Low", "Median", "High")
library(ggplot2)

plot.df$race_gender <- gsub("_", " ", plot.df$race_gender)
plot.df$race_gender <- paste(plot.df$race_gender, "\n(", round(pred.df$pct, 2), "%)", sep="")


png("Prob_Voted_Race_Response.png", height=8, width=6,
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=race_gender,  color=response ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Having Already Voted") +xlab("") + 
  ggtitle(toupper("Dem supporters generally most\nlikely to have already voted")) + 
  scale_colour_manual(values = c( "#124073", "#B71D1A", "#A8BF14"), 
                      name="Vote Choice") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()







pred.df <- df[,c("age_combined", "educ", "race_gender",  "response", "final_weight")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
# pred.df$age_combined <- "New"
pred.df$race_gender <- "New"
# pred.df$response <- "New"

pct <- by(pred.df$final_weight, pred.df$age_combined, sum, na.rm=T)/sum(pred.df$final_weight)*100
pred.df$final_weight <- NULL 
pred.df <-unique(pred.df)
pred.df$pct <- pct[pred.df$age_combined]


pred.df <-unique(pred.df)


out <- replicate(100, apply(posterior_predict(mod2, newdata=pred.df), 2, mean))

out <- apply(out, 1, quantile, p=c(.05, .5, .95))
plot.df <- as.data.frame(t(out))
# plot.df$type <-  paste(pred.df$race_gender, pred.df$partyid)
plot.df$age <- pred.df$age_combined 
plot.df$response <- pred.df$response
colnames(plot.df)[1:3] <- c("Low", "Median", "High")

plot.df$age <- paste(plot.df$age, "\n(", round(pred.df$pct, 2), "%)", sep="")


png("Prob_Voted_Age_Response.png", height=8, width=6, 
    units="in", res=200)
ggplot(plot.df) + geom_pointrange(aes(ymin=Low, ymax=High, y=Median, 
                                      x=age,  color=response ), 
                                  position=position_dodge(width=.5)) +
  coord_flip() + ylab("Probability of Having Already Voted") +xlab("") + 
  ggtitle(toupper("young democratic voters are\nnot necessarily more motivated to vote")) + 
  scale_colour_manual(values = c( "#124073","#B71D1A", "#A8BF14"), 
                      name="Vote Choice") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))
dev.off()






df$response <- ordered(df$response, c("Dem", "Und", "Rep"))


library(brms)

mod3 <- brm(response~(1 + partyid|age_combined) + (1 + partyid|educ) + 
                        (1+partyid|race_gender)  +
                        (1|partyid) + (1|stdist), data=df[df$voted==1,],
            family=categorical(), chains=2, cores=2, control=list("adapt_delta"=.99, "max_treedepth"=15))





pred.df <- df[df$voted==1,c("age_combined", "educ", "race_gender",  "partyid")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
pred.df$age_combined <- "New"
pred.df$race_gender <- "New"
# pred.df$response <- "New"

pred.df <-unique(pred.df)



out3 <- replicate(100, apply(predict(mod3, newdata=pred.df,
                                     allow_new_levels=T, summary=F, 
                                     nsamples = 100), 2, function(x){
                                       c(sum(x==1), sum(x==2), sum(x==3))
                                       }))

library(abind)


tmp <- apply(out3, c(1,2), median)
lo <- apply(out3, c(1,2), quantile, p=0.05)
lo <- c(lo)
up <- apply(out3, c(1,2), quantile, p=0.95)
up <- c(up)

colnames(tmp) <- c("Republicans", "Independents", "Democrats")

png("Indep_Vote.png", height=8, width=8, 
    units="in", res=200)
par(family="mstreg")
barplot(tmp, beside = T, col=scales::alpha(c( "#124073","#A8BF14", "#B71D1A"), .6), 
        yaxt="n", xlim=c(1,15), ylim=c(0,100), border=NA)
axis(2, family="mstthin")
legend("right", legend=c("Democrat", "Undecided", "Republican"), 
       fill=scales::alpha(c( "#124073","#A8BF14", "#B71D1A"), .6), 
       title="Vote Choice", bty="n", border=NA)
title(main=toupper("Independent Early Voters are\nBreaking towards Democrats"), family="futura")
title(ylab="Percentage", xlab="")
for(ii in 1:length(up)){
  lines(x=c(ii, ii)+(.5 + ((ii -1) %/% 3)), y=c(lo[ii], up[ii]), 
        lwd=2, col=c("#B71D1A", "#124073","#A8BF14")[((ii %% 3) + 1)])
}

dev.off()



mod4 <- brm(response~(1 + file_party|age_combined) + (1 + file_party|educ) + 
              (1+file_party|race_gender)  +
              (1|file_party) + (1|stdist), data=df[df$voted==1,],
            family=categorical(), chains=2, cores=2, control=list("adapt_delta"=.99,  "max_treedepth"=15))





pred.df <- df[df$voted==1,c("age_combined", "educ", "race_gender",  "file_party")]
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- unique(pred.df)
pred.df$stdist <- "New"
pred.df$educ <- "New"
pred.df$age_combined <- "New"
pred.df$race_gender <- "New"
# pred.df$response <- "New"

pred.df <-unique(pred.df)



out4 <- replicate(100, apply(predict(mod4, newdata=pred.df,
                                    allow_new_levels=T, summary=F, 
                                    nsamples = 100), 2, function(x){
                                      c(sum(x==1), sum(x==2), sum(x==3))
                                    }))



tmp <- apply(out4, c(1,2), median)
lo <- apply(out4, c(1,2), quantile, p=0.05)
lo <- c(lo)
up <- apply(out4, c(1,2), quantile, p=0.95)
up <- c(up)

pct <- by(df$final_weight[df$voted==1], df$file_party[df$voted==1], sum)/sum(df$final_weight[df$voted==1], na.rm=T)

colnames(tmp) <- paste(c("Republicans", "Other", "Democrats"),
                       "\n",round(pct[c("Republican", "Other", "Democratic")]*100,2),"%", sep="")

png("Indep_Vote_Reg.png", height=8, width=8, 
    units="in", res=200)
par(family="mstreg")
barplot(tmp, beside = T, col=scales::alpha(c( "#124073","#A8BF14", "#B71D1A"), .6), 
        yaxt="n", xlim=c(1,15), ylim=c(0,100), border=NA)
axis(2, family="mstthin")
legend("right", legend=c("Democrat", "Undecided", "Republican"), 
       fill=scales::alpha(c( "#124073","#A8BF14", "#B71D1A"), .6), 
       title="Vote Choice", bty="n", border=NA)
title(main=toupper("Independent Early Voters are\nBreaking towards Democrats"), family="futura")
title(ylab="Percentage", xlab="Voter File")
for(ii in 1:length(up)){
  lines(x=c(ii, ii)+(.5 + ((ii -1) %/% 3)), y=c(lo[ii], up[ii]), 
        lwd=2, col=c("#B71D1A", "#124073","#A8BF14")[((ii %% 3) + 1)])
}

dev.off()


library(abind)


tmp <- apply(out4 - out3, c(1,2), median)
tmp <- c(tmp)
lo <- apply(out4 - out3, c(1,2), quantile, p=0.05)
lo <- c(lo)
lo.mid <- apply(out4 - out3, c(1,2), quantile, p=0.10)
lo.mid <- c(lo.mid)

up <- apply(out4 - out3, c(1,2), quantile, p=0.95)
up <- c(up)
up.mid <- apply(out4 - out3, c(1,2), quantile, p=0.90)
up.mid <- c(up.mid)

plot.df <- data.frame(median=tmp, lo=lo, up=up,up.mid=up.mid, lo.mid=lo.mid,
                      group=c(rep("Republicans", 3),
                              rep("Other", 3), rep("Democrat", 3)), 
                      choice=rep(c("Democrat", "Undecided", "Republican"),3))
  
png("Vote_Compare.png", height=8, width=8, 
    units="in", res=200)


ggplot(plot.df) + geom_pointrange(aes(ymin=lo.mid, ymax=up.mid, y=median, 
                                      x=group,  color=choice ), 
                                  position=position_dodge(width=.5)) +
  geom_linerange(aes(ymin=lo, ymax=up, x=group,  color=choice), 
                 position=position_dodge(width=.5), lwd=.2) + 
  coord_flip() + ylab("Difference in Model Predictions") +xlab("") + 
  annotate("text", label=paste(expression(""%<-%"Under Reporting")), x=.5, y=-8,  parse=T) + 
  annotate("text", label=paste(expression("Over Reporting"%->%"")), x=.5, y=8,  parse=T) + 
  ggtitle(toupper("voter file partisanship under-predicts support")) + 
  scale_colour_manual(values = c( "#124073","#B71D1A", "#A8BF14"), 
                      name="Vote Choice") + theme_classic(base_size=14) +
  theme(plot.title=element_text(family="futura", hjust=.5), 
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA), 
        axis.text.x = element_text(family="mstthin", color="black"))

dev.off()

pp4 <- posterior_predict(mod4)
pp3 <- posterior_predict(mod3)

pp3.est <- apply(pp3, 1, function(x) sum(diag(table(x, as.numeric(mod3$data$response)))))/length(mod3$data$response)
pp4.est <- apply(pp4, 1, function(x) sum(diag(table(x, as.numeric(mod4$data$response)))))/length(mod4$data$response)

save.image("Out.RData")
