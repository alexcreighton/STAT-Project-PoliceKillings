###STAT235 Final project
#Group 2
#Alex Creighton, Brenna Harris, Noah Stracqualursi


library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(readxl)
library(dplyr)
options(dplyr.summarise.inform = FALSE) 
library(summarytools)
install.packages("waffle")
library(waffle)


#################################PART 1
#### Descriptive stats
shootings <- read_excel("/Users/alexcreighton/Desktop/UVM/MATH/STAT235/shootings.xlsx")

n<-count(shootings)

killings<-data.frame(shootings)

#####manner of death
shootings %>% 
  filter(!is.na(manner_of_death)) %>% 
  group_by(manner_of_death) %>%
  count()

deathmanner <- c(`Shot: (4647) `=4647, `Shot and Tasered: (248) `=248)

waffle(deathmanner/10, rows=20, size=0.6,  
       colors=c("red", "black"),  
       title="Manner of Death",  
       xlab="1 square = 10 persons")


#####armed
shootings %>% 
  filter(!is.na(armed)) %>% 
  group_by(armed) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


victimarmed <- c(`Gun: (2755) `=2755, `Knife: (708) `=708,
                 `Unknown: (418) `=418, `Unarmed: (348) `=348,
                 `Toy weapon: (171) `=171, `Vehicle: (120) `=120,
                 `Machete: (39) `=39, `Taser: (24) `=24,
                 `Sword: (22) `=22, `Ax: (21) `=21)

waffle(victimarmed/10, rows=24, size=0.6,  
       colors=c("red", "black", "blue", "green", "orange", "gray", "gold", "slateblue", "orchid", "lavender"),  
       title="Top 10 Armed",  
       xlab="1 square = 10 persons")


#####race
shootings %>% 
  filter(!is.na(race)) %>% 
  group_by(race) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

victimrace <- c(`White: (2476) `=2476, `Black: (1298) `=1298,
                 `Hispanic: (902) `=902, `Asian: (93) `=93,
                 `Native: (78) `=78, `Other: (48) `=48)

waffle(victimrace/10, rows=20, size=0.6,  
       colors=c("red", "black", "blue", "green", "orange", "gray"),  
       title="Race",  
       xlab="1 square = 10 persons")


#####state
shootings %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

shotstate <- c(`CA: (701) `=701, `TX: (426) `=426,
                `FL: (324) `=324, `AZ: (222) `=222,
                `CO: (168) `=168, `GA: (161) `=161,
                `OK: (151) `=151, `NC: (148) `=148,
                `OH: (146) `=146, `WA: (126) `=126)

waffle(shotstate/10, rows=16, size=0.6,  
       colors=c("red", "black", "blue", "green", "orange", "gray", "gold", "slateblue", "orchid", "lavender"),  
       title="Top 10 State",  
       xlab="1 square = 10 persons")


#####threat level
shootings %>% 
  filter(!is.na(threat_level)) %>% 
  group_by(threat_level) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

threat <- c(`Attack: (3160) `=3160, `Other: (1528) `=1528,
                `Undetermined: (207) `=207)

waffle(threat/10, rows=20, size=0.6,  
       colors=c("red", "black", "blue"),  
       title="Threat Level",  
       xlab="1 square = 10 persons")


#####body camera
shootings %>% 
  filter(!is.na(body_camera)) %>% 
  group_by(body_camera) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

bodycam <- c(`FALSE: (4317) `=4317, `TRUE: (578) `=578)

waffle(bodycam/10, rows=22, size=0.6,  
       colors=c("red", "black"),  
       title="Body Camera",  
       xlab="1 square = 10 persons")


#################################PART 2
#### Results - body cameras and race

dat1 <- read.csv("shootings.csv", sep = ",", skipNul = TRUE)
dat1 <- na.omit(dat1)
tab1 <- table(dat1$body_camera, dat1$race)
tab1
summary(tab1)
barplot(tab1, ylab="Deadly Shootings", xlab = "Race")
GK.gamma(tab1) # p-value = 0.000126
chisq.test(tab1)


#################################PART 3
#### Results - location and race
## Looking at Percent of Shootings on Body Cam 

dat1 <- shootings %>%
  group_by(race, body_camera) %>%
  summarise(count = n(), .groups = 'drop')

bodycam.race<-ggplot(dat1, aes(race, count, fill = body_camera))+
  geom_bar(stat = "identity", colour = "black", position="fill", width = 1, alpha = 0.9)

bodycam.race+ggtitle("Percent of Shootings on Camera")+
  xlab("Race")+ylab("Percent")+labs(fill="Body Camera Turned On (T/F)")


###########################################################
###########################################################

## Create DF of shootings by race 

s.race<-as.data.frame(table(shootings$race)) %>%arrange(Freq)
names(s.race)<-c('Race','Freq')

## create the plot 

ggplot(s.race,aes(x=Race,y=Freq/sum(Freq),fill=Race))+
  geom_bar(aes(y=Freq/sum(Freq)),stat="identity", colour="black")+
  ggtitle("Percent of Shootings by Race")+
  xlab("Race")+ylab("Percent")+labs(fill="Race")

###########################################################
###########################################################

## Fit the Multi-Logit Baseline Model (Uses first Group)

levels(s.race$Race) ## First Group or Baseline is ASIAN

s_r_nnet <- multinom(Freq~Race, data=s.race)
summary(s_r_nnet)
coef(s_r_nnet)

## MEaningless because the Frequency 


###########################################################
###########################################################

## Looking at the AGE and Race 

ggplot(data=shootings,aes(x=age))+
  facet_grid(gender~race)

ggplot(data=shootings,aes(x=age,colour=race))+
  geom_density()

## GLM By age and RACE (YOUNG/OLD)

dat4 <-shootings %>%mutate(elder= if_else(age >50, 1,0))


dat4<- dat4 %>% mutate(young= if_else(age<30, 1,0))

logit_age1 <- glm(elder~race, family=binomial, data=dat4)
summary(logit_age1)

logit_age2 <- glm(young~race, family=binomial, data=dat4)
summary(logit_age2)

###########################################################
###########################################################

## Looking at where the shootings took place



dat5<-as.data.frame(table(shootings$state, shootings$body_camera)) %>%arrange(Freq)
names(dat5)<-c('State','Body_Cam', 'Freq')


plot<-ggplot(dat5,aes(x=State,y=Freq,fill=Body_Cam))+
  geom_bar(aes(y=Freq), stat = "identity", position="stack")+
  ggtitle("Percent of Shootings by State")+
  xlab("State")+ylab("Number of Shootings")+labs(fill="Body Camera On T/F")
plot

## PErcent by State

plot2<-ggplot(dat5,aes(x=State,y=Freq,fill=Body_Cam))+
  geom_bar(aes(y=Freq/sum(Freq)), stat = "identity", position="stack" )+
  ggtitle("Percent of Shootings by State")+
  xlab("State")+ylab("Percent of Shootings")+labs(fill="Body Camera On T/F")
plot2+scale_fill_brewer(palette="Set2")

## Percent By state without Body Cam 

plot3<-ggplot(dat5,aes(x=State,y=Freq))+
  geom_bar(aes(y=Freq/sum(Freq)), stat = "identity", position="stack", fill="firebrick4")+
  ggtitle("Percent of Shootings by State")+
  xlab("State")+ylab("Percent of Shootings")

plot3

###########################################################
###########################################################

## How many in each state?

s.state<-as.data.frame(table(shootings$state)) %>%arrange(Freq)
names(s.state)<-c('State','Freq')

ggplot(s.state,aes(x=State,y=Freq))+
  geom_bar(aes(y=Freq), stat = "identity", fill= "cornflower blue")+
  ggtitle("Shootings by State")+
  xlab("State")+ylab("Shootings")
sum(s.state$Freq)

## Looking at total Shootings with Cam on 
cam.on<-filter(dat5, Body_Cam ==TRUE)
cam.on
sum(cam.on$Freq)

## 578

## Looking at total Shootings with Cam off
cam.off<-filter(dat5, Body_Cam ==FALSE)
cam.off
sum(cam.off$Freq)

cam.off<-cam.off %>%mutate(Body_Cam_Off= if_else(Body_Cam==TRUE, 1,0))
cam.on<-cam.on %>%mutate(Body_Cam_on= if_else(Body_Cam==TRUE, 1,0))
## 4317

## Total percent of shootings on Camera
578/4895

## 11.8 Percent of the Shootings were on Camera

## CA 

103/701

sum(cam.on$Freq)

## NV Camera on 30.59%
## VT Camera on 37.5%

###########################################################
###########################################################

## Creating a linear model of the data 
## Freq~ State

## Mutate so Body Cam is Binary 
## TRUE= 1 
dat5 <-dat5 %>%mutate(Body_Cam= if_else(Body_Cam==TRUE, 1,0))

s.state<- dat5%>%mutate(Body_Cam=ifelse(Body_Cam==TRUE,1,0))


dat6<-lm(Freq~State+Body_Cam, data = dat5)

dat6
summary(dat6)


###########################################################
###########################################################

## Create Region Variable 
state <- stringr::str_split(dat5$State, ",")
dat7 <- data.frame(id, state)
regions <- list(
  W = c("WA", "OR", "CA", "NV", "AZ", "ID", "MT", "WY",
        "CO", "NM", "UT"),
  S = c("TX", "OK", "AR", "LA", "MS", "AL", "TN", "KY",
        "GA", "FL", "SC", "NC", "VA", "WV"),
  MW = c("KS", "NE", "SD", "ND", "MN", "MO", "IA", "IL",
         "IN", "MI", "WI", "OH"),
  NE = c("ME", "NH", "NY", "MA", "RI", "VT", "PA", 
         "NJ", "CT", "DE", "MD", "DC"))
dat$westYN <- ifelse(is.element(stateList, west), 1, 0)
dat$southYN <- ifelse(is.element(stateList, south), 1, 0)
dat$midwestYN <- ifelse(is.element(stateList, midwest), 1, 0)
dat$northeastYN <- ifelse(is.element(stateList, northeast), 1, 0)

s.state$RegionW <- sapply(state, function(x) any(x %in% W))
s.state$RegionS <- sapply(state, function(x) any(x %in% S))
s.state$RegionMW <- sapply(state, function(x) any(x %in% MW))
s.state$RegionNE <- sapply(state, function(x) any(x %in% NE))

