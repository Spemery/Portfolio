library(readxl)
old <- read_excel("~/Desktop/DraftData.xlsx", 
                  sheet = "20-23")
View(old)

str(old)
old$pos<-factor(old$pos)
old$lvl<-factor(old$lvl)
old$vorp<-as.numeric(old$vorp)
old$`WS/48`<-as.numeric(old$`WS/48`)
old$PER<-as.numeric(old$PER)
str(old)
old$total<-old$vorp + old$`WS/48` + old$PER
view(old)
oldmod<-select(old, -name, -ft, -'in', -'wng ft', -'wng in', -vorp, -'WS/48', -PER)
vorpmod<-select(old, -name, -ft, -'in', -'wng ft', -'wng in', -total, -'WS/48', -PER)
wsmod<-select(old, -name, -ft, -'in', -'wng ft', -'wng in', -total, -vorp, -PER)
permod<-select(old, -name, -ft, -'in', -'wng ft', -'wng in', -vorp, -'WS/48', -total)
head(oldmod)

mod1<-lm(total~., data = oldmod)
summary(mod1)

vorplm<-lm(vorp~., data = vorpmod)
summary(vorplm)

wslm<-lm(`WS/48`~., data = wsmod)
summary(wslm)

perlm<-lm(PER~., data = permod)
summary(perlm)

mod1<-lm(total~. ,  data = oldmod)
summary(mod1)


library(readxl)
thisyr <- read_excel("~/Desktop/DraftData.xlsx", 
                     sheet = "2024 Class")
View(thisyr)

thisyr$pos<-factor(thisyr$pos)
thisyr$lvl<-factor(thisyr$lvl)
str(thisyr)

newmod<-select(thisyr, -name, -ft, -'in', -'wng ft', -'wng in')
preds<-predict(mod1, newmod)
view(preds)

thisyr$preds<-preds
view(thisyr)

write.xlsx(thisyr, "2024DraftPreds.xlsx")


###per, ts%, usg% update

library(readxl)
d1 <- read_excel("~/Desktop/DraftData.xlsx", 
                 sheet = "20-23 Updated")
View(d1)

d1$pos<-factor(d1$pos)
d1$lvl<-factor(d1$lvl)
d1$vorp<-as.numeric(d1$vorp)
d1$`WS/48`<-as.numeric(d1$`WS/48`)
d1$PER<-as.numeric(d1$PER)
str(d1)
d1$total<-d1$vorp + d1$`WS/48` + d1$PER
view(d1)

d1mod<-select(d1, -name, -ft, -'in', -'wng ft', -'wng in', -vorp, -'WS/48', -PER)
mod1all<-lm(total~.+I(ht^2), data = d1mod)
summary(mod1)

mod1ref<-lm(total~.+I(ht^2) -wt -wng -rebs -ast -pos -`usg%` -pts, data = d1mod)
summary(mod1ref)

library(readxl)
now <- read_excel("~/Desktop/DraftData.xlsx", 
                     sheet = "2024 Class")

now$pos<-factor(now$pos)
now$lvl<-factor(now$lvl)
str(now)

newmod<-select(now, -name, -ft, -'in', -'wng ft', -'wng in')
preds<-predict(mod1ref, newmod)
#view(preds)

now$preds<-preds
view(now)

write.xlsx(now, "2024DraftPredsRevised.xlsx")
