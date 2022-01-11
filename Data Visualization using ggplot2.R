#The following script displays the use of various data visualization graphs using the package "ggplot2".
#Author: Lydia Menassie

setwd ("/Users/Lydia/Desktop/R code")
anes20 <- read.csv("anes20.csv")

#Requires ggplot2 package.
install.packages("ggplot2")
library(ggplot2)


#Coefficient plot that displays regression results (with 95% confidence interval). 
#Regression results taken from Table A1 in “Social Identities and Democratic Representation" by Adam Howat.

treatment3 <- c("Inparty, inparty value", "Inparty, outparty value", "Outparty, no value", "Outparty, inparty value", "Outparty, outparty value", "Inparty, inparty value", "Inparty, outparty value", "Outparty, no value", "Outparty, inparty value", "Outparty, outparty value")
coef3 <- c(.47, -.25, -1.15, -.57, -1.41, .27, -.42, -1.29, -.52, -1.53)
se3 <- c(.24, .24, .23, .24, .23, .26, .25, .24, .26, .25)
outcome <- c("Support","Support","Support","Support","Support","Represent Self", "Represent Self", "Represent Self", "Represent Self", "Represent Self")

partisan <- data.frame(treatment3, coef3, se3, outcome) 

partisan$treatment3=factor(partisan$treatment3, levels=c("Outparty, outparty value", "Outparty, inparty value","Outparty, no value","Inparty, outparty value","Inparty, inparty value"))
partisan$outcome=factor(partisan$outcome, levels=c("Support", "Represent Self"))

ggplot(partisan, aes(x=treatment3, y=coef3, ymin=coef3-1.984*se3, ymax=coef3+1.984*se3, point=outcome, color=outcome, shape=outcome)) +
  geom_pointrange(size=1, position=position_dodge(width=.4)) +
  geom_hline(yintercept=0, size=1, linetype="dashed", color="black") +
  theme_minimal() +
  labs(x="Treatment", y="Coefficient with 95% CI") +
  guides(point=guide_legend(title=""), shape=guide_legend(title=""), color=guide_legend(title="")) +
  coord_flip()


#Density curve displaying egalitarian values of the American public by income. Data from ANES 2020.

#Requires car package for recode command.
install.packages("car")
library(car)

anes$fairly <- recode(anes$egal_fairly, "1=5; 2=4; 3=3; 4=2; 5=1")
anes$opp <- recode(anes$egal_opp, "1=5; 2=4; 3=3; 4=2; 5=1")
anes$egal <- rowMeans(anes[,c("egal_chance","fairly", "opp", "egal_worry")])

#Requires car package for recode factor command.
install.packages("dplyr")
library(dplyr)

anes$income <- recode_factor(anes$income, '1'='under $30,000', '2'='$30,000-$69,999', '3'='$70,000-$99,999', '4'='$100,000-$249,999', '5'='$250,000 or more')

ggplot(subset(anes, !is.na(income)), aes(x=egal, color=income)) +
  geom_density() +
  labs(y="Frequency", x="Measure of Egalitarianism (where higher = more egalitarian)", title="Egalitarian Values in U.S.")+
  theme_minimal()


#Line Graph displaying an individual's self-identified ideology along side their opinion on government-run health insurance. 
#Shaded areas show standard errors. Data from ANES 2020.

mean(anes20$med_insurance[anes20$ideology==1], na.rm=TRUE)
mean(anes20$med_insurance[anes20$ideology==2], na.rm=TRUE) 
mean(anes20$med_insurance[anes20$ideology==3], na.rm=TRUE)
mean(anes20$med_insurance[anes20$ideology==4], na.rm=TRUE)
mean(anes20$med_insurance[anes20$ideology==5], na.rm=TRUE)
mean(anes20$med_insurance[anes20$ideology==6], na.rm=TRUE)
mean(anes20$med_insurance[anes20$ideology==7], na.rm=TRUE)

install.packages("plotrix")
library(plotrix)

extlib <- subset(anes20, ideology==1)
lib <- subset(anes20, ideology==2)
slight.lib <- subset(anes20, ideology==3) 
mod <- subset(anes20, ideology==4) 
slight.con <- subset(anes20, ideology==5) 
con <- subset(anes20, ideology==6)
extcon <- subset(anes20, ideology==7)

std.error(extlib$med_insurance) 
std.error(lib$med_insurance) 
std.error(slight.lib$med_insurance) 
std.error(mod$med_insurance) 
std.error(slight.con$med_insurance) 
std.error(con$med_insurance)
std.error(extcon$med_insurance)

ideo <- c(1, 2, 3, 4, 5, 6, 7)
ins_avg <- c(1.71, 2.23, 2.78, 3.46, 4.55, 5.54, 6.05) 
se <- c(.078, .042, .051, .045, .066, .041, .073)
ins_ideo <- data.frame(ideo, ins_avg)

ggplot(anes20, aes(x=ideology, y=med_insurance)) + geom_point(alpha=.01, shape = 17)+
  labs(x="Ideology (higher = more conservative)", y="Insurance
Public vs. Private (higher = private)", title= "Insurance Plan Preference by Ideology") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7)) + scale_y_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7)) + stat_smooth(method=loess, level=.99)


#Bar graph displaying feeling thermometer of police by party by race. 
#Error bars added (with 95% confidence intervals). Data from ANES 2020.

dem.w <- subset(anes20, pid3==1 & nonwhite==0)
dem.n <- subset(anes20, pid3==1 & nonwhite==1)
ind.w <- subset(anes20, pid3==2 & nonwhite==0)
ind.n <- subset(anes20, pid3==2 & nonwhite==1)
rep.w <- subset(anes20, pid3==3 & nonwhite==0)
rep.n <- subset(anes20, pid3==3 & nonwhite==1)

mean(dem.w$ft_police, na.rm=TRUE) 
mean(dem.n$ft_police, na.rm=TRUE) 
mean(ind.w$ft_police, na.rm=TRUE) 
mean(ind.n$ft_police, na.rm=TRUE) 
mean(rep.w$ft_police, na.rm=TRUE) 
mean(rep.n$ft_police, na.rm=TRUE)

#Requires plotrix package to produce standard error.
install.packages("plotrix")
library(plotrix)

std.error(dem.w$ft_police)
std.error(dem.n$ft_police)
std.error(ind.w$ft_police)
std.error(ind.n$ft_police)
std.error(rep.w$ft_police)
std.error(rep.n$ft_police)

pid3 <- c("Dem", "Dem", "Ind", "Ind", "Rep", "Rep")
race <- c("White", "Nonwhite", "White", "Nonwhite", "White", "Nonwhite")
ft_police2 <- c(64.52, 56.06, 70.62, 59.71, 85.84, 80.41)
se2 <- c( .592, .869, .584, 1.067, .358, 1.348 )

pid_police <- data.frame(pid3, race, ft_police2, se2)

ggplot(data=pid_police, aes(x=pid3, y=ft_police2, fill=race)) +
  geom_col(position = "dodge", width = 0.7) +
  guides(fill=guide_legend(title="")) +
  theme_minimal() +
  labs(y="Mean Police FT", x="Party ID", title="Feeling Thermometer of Police by Party by Race") +
  geom_errorbar(aes(ymin=ft_police2-1.96*se2, ymax=ft_police2+1.96*se2), position=position_dodge(.7), width=.2) +
  geom_text(aes(label = ft_police2), position=position_dodge(.7), vjust = 2.5, size= 3.5, color= "white") +
  scale_fill_manual(values=c("#F5B041", "#5DADE2")) 


#Bar graph displaying ideological gaps between political parties. Data from ANES 2020.

mean(anes20$ideology[anes20$pid3==1], na.rm=TRUE) 
mean(anes20$ideology[anes20$pid3==2], na.rm=TRUE) 
mean(anes20$ideology[anes20$pid3==3], na.rm=TRUE) 

pid <- c("Democrat", "Independent", "Republican")
ideo_mean <- c(2.78, 3.98, 5.60)

pid_ideo <- data.frame(pid, ideo_mean)
pid_ideo

ggplot(data=pid_ideo, aes(x=pid, y=ideo_mean, fill=pid)) +
  geom_col(width = 0.5) +
  labs(y="Mean of Ideology (higher = more conservative)", x="Party ID", title="Party Ideology Mean by Party") + 
  scale_fill_manual(values=c("#1181df", "#999999", "#e20000"), guide = "none") +
  geom_text(aes(label = ideo_mean), vjust = 1.5, size= 5, color= "white") +
  theme_classic() 

