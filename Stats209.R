rm(list = ls())

library(MatchIt)
library(dplyr)
library(ggplot2)
library(haven)
library(stargazer)
library(tableone)
library(Hmisc)
library(cem)
library(Matching)
library(car)
library(RColorBrewer)
library(arm)
library(plyr)
library(rcbalance)
library(RItools)
library(MatchIt)
library(DOS2)
library(optmatch)
library(reshape2)

summarize.match <- function(dat, ms, ps.name="prop", keep.mset=FALSE) {
  adat <- dat
  adat$mset <- ms
  adat <- adat[!is.na(adat$mset),]
  adat.treat <- adat[adat$PriorOral==1, ]
  adat.ctrl <- adat[adat$PriorOral==0, ]
  
  adat.m <- merge(adat.treat, adat.ctrl, by="mset", suffixes=c(".1", ".0"))
  
  if(!keep.mset) {
    adat.m <- adat.m[, -which(names(adat.m) %in% c("PriorOral.1", "PriorOral.0", "mset"))]
  } else {
    adat.m <- adat.m[, -which(names(adat.m) %in% c("PriorOral.1", "PriorOral.0"))]        
  }
  adat.m <- adat.m[, sort(names(adat.m), index.return=TRUE)$ix]
  
  p0.name <- paste0(ps.name,".", 0)
  p1.name <- paste0(ps.name,".",1)
  
  adat.m.tmp.1 <- adat.m[, -which(names(adat.m) %in% c(p0.name, p1.name))]
  adat.m.tmp.2 <- adat.m[, c(p0.name, p1.name)]
  
  adat.m <- cbind(adat.m.tmp.1, adat.m.tmp.2)
  
  return(adat.m)
}

data = read_dta("Attorney.dta")
data$hysLaw = car::recode(data$SchoolLawyerArgue,"'Harvard'=1;'Yale'=1;'Stanford'=1;
                          'Columbia'=1;'Chicago'=1;else=0")
data$demPres = car::recode(data$SGOrals,"'McCree'=1;'Lee'=0;'(Acting) Fried'=0;
                           'Fried'=0;'(Acting) Bryson'=0;'Starr'=0;'Days'=1;
                           '(Acting) Dellinger'=1;'Waxman'=1;'(Acting) Underwood'=0;
                           'Olson'=0;'(Acting) Clement'=0;'Clement'=0;'Garre'=0;
                           '(Acting) Kneedler'=1;'Kagan'=1;'(Acting) Katyal'=1;
                           'Verrilli'=1;'(Acting) Gershengorn'=1;'(Acting) Francisco'=1;
                           '(Acting) Wall'=1; 'Francisco'=1")

#Bryson was acting for both Bush 1 and Reagan
data$demPres[data$SGOrals=="(Acting) Bryson" & data$term=="1992"] = 1
data$SGOralsCollapseF = as.factor(data$SGOralsCollapse)
data$justiceNameF = as.factor(data$justiceName)
data$ActingSGOrals = as.numeric(factor(data$ActingSGOrals, ordered=F))-1
data$TopLawSchool = as.numeric(factor(data$TopLawSchool, ordered=F))-1
data$DCFirm = as.numeric(factor(data$DCFirm, ordered=F))-1
data$ClerkDummy = as.numeric(factor(data$ClerkDummy, ordered=F))-1
data$OppPetOrResp = as.numeric(factor(data$OppPetOrResp, ordered=F))-1
data$CriminalDummy = as.numeric(factor(data$CriminalDummy, ordered=F))-1
data$CivLibDummy = as.numeric(factor(data$CivLibDummy, ordered=F))-1
data$EconDummy = as.numeric(factor(data$EconDummy, ordered=F))-1
data$Acela = as.numeric(factor(data$Acela, ordered=F))-1
data$LawyerArgueOSG = as.numeric(factor(data$LawyerArgueOSG, ordered=F))-1
data$GenderLawyerArgue = as.numeric(factor(data$GenderLawyerArgue, ordered=F))-1
data$RaceLawyerArgue = as.numeric(factor(data$RaceLawyerArgue, ordered=F))-1
data$IdeologyOppCounsel = as.numeric(factor(data$IdeologyOppCounsel, ordered=F))-1
ctLevel = subset(data, ct_level==1)
summary(data)

#Create Data Sets

#Justice-Level
xvars = c("SGOralsCollapseF","ActingSGOrals","hysLaw","DCFirm","ClerkDummy",
          "Rehnquist","Roberts","IdeologyOppCounsel","OppPetOrResp","demPres",
          "justiceNameF")
data_nomiss = data %>%  
  dplyr::select(PriorOral, JVote, one_of(xvars)) %>%
  na.omit()
data_nomiss = data.frame(data_nomiss)

pretab1 = CreateTableOne(vars = xvars, strata = "PriorOral", 
                        data = data_nomiss, test = FALSE)
print(pretab1, smd = TRUE)

#Case-Level
xvars_case = c("SGOralsCollapseF","ActingSGOrals","hysLaw","DCFirm","ClerkDummy",
               "Rehnquist","Roberts","IdeologyOppCounsel","OppPetOrResp","demPres")
data_case_nomiss = ctLevel %>% 
  dplyr::select(PriorOral, WhoWon, minWinning, unan, nearUnan, one_of(xvars_case)) %>%
  na.omit()
data_case_nomiss = data.frame(data_case_nomiss)

pretab1 = CreateTableOne(vars = xvars_case, strata = "PriorOral", 
                        data = data_case_nomiss, test = FALSE)
print(pretab1, smd = TRUE)

#Figure 1 Plot Section 

#Justice Vote Proportions Calculations
no_experience = length(data$WhoWon[data$PriorOral == 0])
no_experience
experience = length(data$WhoWon[data$PriorOral == 1])
experience

prop_ne_1 = sum(data$JVote[data$PriorOral == 0])/no_experience
prop_ne_1
prop_ne_0 = 1 - prop_ne_1

prop_e_1 = sum(data$JVote[data$PriorOral == 1])/experience
prop_e_1
prop_e_0 = 1 - prop_e_1

prop_data = data.frame(
  Outcome = c("Lost", "Won", "Lost", "Won"),
  Experience = factor(rep(c("No Experience", "Experience"), each = 2)),
  PriorOral = rep(c("0", "1"), times = 2),
  Proportion = c(prop_ne_0, prop_ne_1, prop_e_0, prop_e_1)
)

#Plot the Proportions for Justice Vote
ggplot(prop_data, aes(x = Outcome, y = Proportion, fill = Experience)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Outcome", y = "Proportion", fill = "Experience Level") +
  scale_fill_manual(values = c("No Experience" = "red", "Experience" = "black")) +
  ggtitle("Justice Vote Opposing Counsel Result Distribution") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Court Case Proportions Calculations
no_experience = length(ctLevel$WhoWon[ctLevel$PriorOral == 0])
no_experience
experience = length(ctLevel$WhoWon[ctLevel$PriorOral == 1])
experience

prop_ne_1 = sum(ctLevel$WhoWon[ctLevel$PriorOral == 0])/no_experience
prop_ne_1
prop_ne_0 = 1 - prop_ne_1

prop_e_1 = sum(ctLevel$WhoWon[ctLevel$PriorOral == 1])/experience
prop_e_1
prop_e_0 = 1 - prop_e_1

prop_data = data.frame(
  Outcome = c("Lost", "Won", "Lost", "Won"),
  Experience = factor(rep(c("No Experience", "Experience"), each = 2)),
  PriorOral = rep(c("0", "1"), times = 2),
  Proportion = c(prop_ne_0, prop_ne_1, prop_e_0, prop_e_1)
)

#Plot the Proportions for Court Cases
ggplot(prop_data, aes(x = Outcome, y = Proportion, fill = Experience)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Outcome", y = "Proportion", fill = "Experience Level") +
  scale_fill_manual(values = c("No Experience" = "red", "Experience" = "black")) +
  ggtitle("Court Case Opposing Counsel Result Distribution") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Replication Section

#CEM Matching
match_justice_cem = matchit(PriorOral ~ SGOralsCollapseF + ActingSGOrals + hysLaw +
                              DCFirm + ClerkDummy + IdeologyOppCounsel + demPres +
                              Rehnquist + Roberts + OppPetOrResp + justiceNameF, 
                            method = "cem", data = data_nomiss)
#summary(match_justice_cem)
dta_justice_cem = match.data(match_justice_cem)

plot(xBalance(JVote~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                Roberts + OppPetOrResp + IdeologyOppCounsel + demPres - 1, 
              strata = list(unstrat = NULL, cem_justice_match = ~match_justice_cem$treat),
              data = data_nomiss)) 

legend(
  "topright",
  legend = c("unstrat", "cem_justice_match"),
  inset = 0.01,
  pch = c(15, 16),
  bg = "white"
)

match_case_cem = matchit(PriorOral ~ SGOralsCollapseF + ActingSGOrals + hysLaw +
                           DCFirm + ClerkDummy + IdeologyOppCounsel + demPres +
                           Rehnquist + Roberts + OppPetOrResp, method = "cem", 
                         data = data_case_nomiss)
#summary(match_case_cem)
dta_case_cem = match.data(match_case_cem)

plot(xBalance(WhoWon~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                Roberts + OppPetOrResp + IdeologyOppCounsel + demPres - 1, 
              strata = list(unstrat = NULL, cem_case_match = ~match_case_cem$treat),
              data = data_case_nomiss))

legend(
  "topright",
  legend = c("unstrat", "cem_case_match"),
  inset = 0.01,
  pch = c(15, 16),
  bg = "white"
)

#Table Distribution
table(data_nomiss$PriorOral)
table(dta_justice_cem$PriorOral)
table(data_case_nomiss$PriorOral)
table(dta_case_cem$PriorOral)

#Run Models

#Justice-Level
full_justice = glm(JVote~PriorOral, family = "binomial", data = data); summary(full_justice)
cem_justice = glm(JVote~PriorOral, family = "binomial", data = dta_justice_cem, 
                  weights = dta_justice_cem$weights); summary(cem_justice)
full_justice_m = glm(JVote~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist +
                       Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + 
                       justiceNameF, family = "binomial", data = data); summary(full_justice_m)
cem_justice_m = glm(JVote~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                      Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + 
                      justiceNameF, family = "binomial", data = dta_justice_cem,
                    weights = dta_justice_cem$weights); summary(cem_justice_m)

#stargazer(full_justice, full_justice_m, cem_justice, cem_justice_m, type = "text", digits = 2)

#Case-Level
full_case = glm(WhoWon~PriorOral, family = "binomial", data = ctLevel); summary(full_case)
cem_case = glm(WhoWon~PriorOral, family = "binomial", data = dta_case_cem,
               weights = dta_case_cem$weights); summary(cem_case)
full_case_m = glm(WhoWon~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                    Roberts + OppPetOrResp + IdeologyOppCounsel + demPres, 
                  family="binomial", data = ctLevel); summary(full_case_m)
cem_case_m = glm(WhoWon~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                   Roberts + OppPetOrResp + IdeologyOppCounsel + demPres, 
                 family="binomial", data = dta_case_cem, weights=dta_case_cem$weights); summary(cem_case_m)

#stargazer(full_case, full_case_m, cem_case, cem_case_m, type="text", digits = 2)

#Simulate 
set.seed(1111)

#Justice-Level
aa = sim(full_justice)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,1] + acoef[,2]*1)
untreated = invlogit(acoef[,1] + acoef[,2]*0)
j_n = treated - untreated

dd = data
aa = sim(full_justice_m)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*1 + 
                      acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                      acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                      acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                      acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                      acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                      acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0  + 
                      acoef[,"justiceNameFJPStevens"]*1)
untreated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*0 + 
                        acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                        acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                        acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                        acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                        acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                        acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0 + 
                        acoef[,"justiceNameFJPStevens"]*1)
j_m = treated - untreated

aa = sim(cem_justice)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,1] + acoef[,2]*1)
untreated = invlogit(acoef[,1] + acoef[,2]*0)
j_n_match = treated - untreated

dd = match.data(match_justice_cem)
aa = sim(cem_justice_m)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*1 + 
                      acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                      acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                      acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                      acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                      acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) + 
                      acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0  + 
                      acoef[,"justiceNameFJPStevens"]*1)
untreated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*0 + 
                        acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                        acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                        acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                        acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                        acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                        acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0  +
                        acoef[,"justiceNameFJPStevens"]*1)
j_m_match = treated - untreated

##Case-level
aa  = sim(full_case)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,1] + acoef[,2]*1)
untreated = invlogit(acoef[,1] + acoef[,2]*0)
c_n = treated - untreated

dd = ctLevel
aa = sim(full_case_m)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*1 + 
                      acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                      acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                      acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                      acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                      acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                      acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0 )
untreated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*0 + 
                        acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                        acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                        acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                        acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                        acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                        acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0 )
c_m = treated - untreated

aa = sim(cem_case)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,1] + acoef[,2]*1)
untreated = invlogit(acoef[,1] + acoef[,2]*0)
c_n_match = treated-untreated

dd = match.data(match_case_cem)
aa = sim(cem_case_m)
acoef = coef(aa)
head(acoef)
treated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*1 + 
                      acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                      acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                      acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                      acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                      acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                      acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0 )
untreated = invlogit(acoef[,"(Intercept)"] + acoef[,"PriorOral"]*0 + 
                        acoef[,"hysLaw"]*median(dd$hysLaw,na.omit=T) + 
                        acoef[,"DCFirm"]*median(dd$DCFirm,na.omit=T) + 
                        acoef[,"ClerkDummy"]*median(dd$ClerkDummy,na.omit=T) + 
                        acoef[,"Rehnquist"]*1 + acoef[,"Roberts"]*0 + 
                        acoef[,"OppPetOrResp"]*median(dd$OppPetOrResp,na.omit=T) +
                        acoef[,"IdeologyOppCounsel"]*1  + acoef[,"demPres"]*0)
c_m_match = treated - untreated

#Figure 5

pdf("MainResults.pdf", width = 15, height = 6.6, paper = 'special') 
par(mar = c(5.1,2, 4.1, 2.1), mfrow = c(1,2), oma = c(0,5,0,0))
#Justice-Level Figure
plot(NA, xlim=c(-.1,.3), ylim=c(.5,2.5), ylab = "", xlab = "ATT", 
     main="Justice-Level Data", yaxt = "n")
axis(2, at = c(2,1),labels = c("Naive", "Multivariate"), las = 2)
points(y = c(2.1,1.1), x = c(mean(j_n), mean(j_m)), pch = 19, 
       cex = 1.5,col = "gray70")
points(y = c(1.9,.9), x = c(mean(j_n_match), mean(j_m_match)), pch = 18, cex = 1.75)
segments(y0 = 2.1, y1 = 2.1, x0 = quantile(j_n,.025), x1 = quantile(j_n,.975),
         lwd = 2, col = "gray70")
segments(y0 = 1.1, y1 = 1.1, x0 = quantile(j_m,.025), x1 = quantile(j_m,.975), 
         lwd = 2, col = "gray70")
segments(y0 = 1.9, y1 = 1.9, x0 = quantile(j_n_match,.025), 
         x1 = quantile(j_n_match,.975), lwd = 2)
segments(y0 = .9, y1 = .9, x0 = quantile(j_m_match,.025), 
         x1 = quantile(j_m_match,.975), lwd = 2)
abline(v = 0, lwd = 3, col = "gray60")
#Case-Level Figure
plot(NA, xlim = c(-.1,.3), ylim = c(.5,2.5), ylab = "", xlab = "ATT", 
     main = "Court-Level Data", yaxt = "n")
points(y = c(2.1,1.1), x = c(mean(c_n), mean(c_m)), pch = 19, cex = 1.5, col = "gray70")
points(y = c(1.9,.9), x = c(mean(c_n_match), mean(c_m_match)), pch = 18, cex = 1.75)
segments(y0 = 2.1, y1 = 2.1, x0 = quantile(c_n,.025), x1 = quantile(c_n,.975),
         lwd = 2, col = "gray70")
segments(y0 = 1.1, y1 = 1.1, x0 = quantile(c_m,.025), x1 = quantile(c_m,.975),
         lwd = 2, col = "gray70")
segments(y0 = 1.9, y1 = 1.9, x0 = quantile(c_n_match,.025), x1 = quantile(c_n_match,.975),
         lwd = 2)
segments(y0 = .9, y1 = .9, x0 = quantile(c_m_match,.025), x1 = quantile(c_m_match,.975),
         lwd = 2)
abline(v = 0, lwd = 3, col = "gray60")
legend("bottomright", c("Full Dataset", "Matched Dataset"), col = c("gray70", "black"), 
       pch = c(19,18), pt.cex = c(1,1.3))
par(mar = c(5.1,4.1, 4.1, 2.1), oma = c(0,0,0,0))
par(mfrow = c(1,1))
dev.off()

#Figure 4 (Covariates Coefficients)

pdf("Coef.pdf", width = 15, height = 9, paper = 'special') 
par(mar = c(5.1,2, 4.1, 2.1), mfrow = c(1,2), oma = c(0,10,0,0))
#Justice-Level
plot(NA, xlim = c(-.75,1.25), ylim = c(.5,9.5), ylab = "", 
     xlab = "Logistic Regression Coefficient", main = "Justice-Level Data", yaxt = "n")
axis(2, at = c(9,8,7,6,5,4,3,2,1), labels = 
       c("Experienced Attorney","Educational Quality","Worked in DC Firm","Was a Supreme Court Clerk","Rehnquist Court","Roberts Court","Represented Petitioner","Represented Liberal Side","SG Appointed by Democrat"), 
     las = 2)
abline(v = 0, lwd = 3, col = "gray60")
points(y = c(9,8,7,6,5,4,3,2,1)+.1, x = coef(full_justice_m)[2:10], pch = 19,
       cex = 1.5, col = "gray70")
points(y = c(9,8,7,6,5,4,3,2,1)-.1, x = coef(cem_justice_m)[2:10], pch = 18, 
       cex = 1.75)
for(i in 1:9){
  segments(y0 = 10-i+.1, y1 = 10-i+.1, x0 = coef(full_justice_m)[i+1]+1.96*se.coef(full_justice_m)[i+1],
           x1 = coef(full_justice_m)[i+1]-1.96*se.coef(full_justice_m)[i+1],
           lwd = 2, col = "gray70")
  segments(y0 = 10-i-.1,y1=10-i-.1, x0 = coef(cem_justice_m)[i+1]+1.96*se.coef(cem_justice_m)[i+1],
           x1 = coef(cem_justice_m)[i+1]-1.96*se.coef(cem_justice_m)[i+1], 
           lwd = 2)
}
#Case-Level
plot(NA, xlim = c(-.75,1.25), ylim = c(.5,9.5), ylab = "", 
     xlab = "Logistic Regression Coefficient", main = "Case-Level Data", yaxt = "n")
abline(v = 0, lwd = 3, col = "gray60")
points(y = c(9,8,7,6,5,4,3,2,1)+.1, x = coef(full_case_m)[2:10], pch = 19,
       cex = 1.5,col = "gray70")
points(y = c(9,8,7,6,5,4,3,2,1)-.1, x = coef(cem_case_m)[2:10], pch = 18,
       cex = 1.75)
for(i in 1:9){
  segments(y0 = 10-i+.1,y1=10-i+.1, x0 = coef(full_case_m)[i+1]+1.96*se.coef(full_case_m)[i+1],
           x1 = coef(full_case_m)[i+1]-1.96*se.coef(full_case_m)[i+1],
           lwd = 2, col = "gray70")
  segments(y0 = 10-i-.1,y1=10-i-.1, x0 = coef(cem_case_m)[i+1]+1.96*se.coef(cem_case_m)[i+1],
           x1 = coef(cem_case_m)[i+1]-1.96*se.coef(cem_case_m)[i+1],
           lwd = 2)
}
legend("bottomright", c("Full Dataset", "Matched Dataset"), 
       col = c("gray70", "black"), pch = c(19,18), cex = .8, pt.cex = c(1,1.3))
dev.off()

#New Experiment (Our Analysis)

#New Covariates

#Justice-Level
xvars = c("SGOralsCollapseF","ActingSGOrals","hysLaw","DCFirm","ClerkDummy",
          "Rehnquist","Roberts","IdeologyOppCounsel","OppPetOrResp","demPres",
          "GenderLawyerArgue", "RaceLawyerArgue", "justiceNameF")
data_nomiss = data %>%  
  dplyr::select(PriorOral, JVote, one_of(xvars)) %>%
  na.omit()
data_nomiss = data.frame(data_nomiss)

data_nomiss$RaceLawyerArgue = ifelse(data_nomiss$RaceLawyerArgue == 0, 0, 1)

#Case-Level
xvars_case = c("SGOralsCollapseF","ActingSGOrals","hysLaw","DCFirm","ClerkDummy",
               "Rehnquist","Roberts","IdeologyOppCounsel","OppPetOrResp","demPres",
               "GenderLawyerArgue", "RaceLawyerArgue")
data_case_nomiss = ctLevel %>% 
  dplyr::select(PriorOral, WhoWon, minWinning, unan, nearUnan, one_of(xvars_case)) %>%
  na.omit()

data_case_nomiss$RaceLawyerArgue = ifelse(data_case_nomiss$RaceLawyerArgue == 0, 0, 1)

#Analysis

#Justice-Level
data_nomiss$prop = glm(PriorOral ~ hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                          Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + 
                         GenderLawyerArgue + RaceLawyerArgue, data = data_nomiss)$fitted.values

PriorOral = as.vector(data_nomiss$PriorOral)
justice_match = smahal(PriorOral, data_nomiss[ ,c("ActingSGOrals", "hysLaw", "DCFirm",
                                                  "ClerkDummy", "IdeologyOppCounsel",
                                                  "demPres", "OppPetOrResp", 
                                                  "GenderLawyerArgue", "RaceLawyerArgue")])

justice_match_1 = addcaliper(justice_match, z = PriorOral, p = data_nomiss$prop, caliper = .1)
justice_matching_1 = pairmatch(justice_match_1, data = data_nomiss)

dta_justice = summarize.match(data_nomiss, justice_matching_1)

plot(xBalance(JVote~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + GenderLawyerArgue + RaceLawyerArgue,
              strata = list(unstrat = NULL, justice_match =~ justice_matching_1), 
              data = data_nomiss))
legend(
  "topright",
  legend = c("unstrat", "justice_match"),
  inset = 0.01,
  pch = c(15, 16),
  bg = "white"
)

#Case-Level

data_case_nomiss$prop = glm(PriorOral ~ hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                               Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + 
                               GenderLawyerArgue + RaceLawyerArgue, data = data_case_nomiss)$fitted.values

PriorOral2 = as.vector(data_case_nomiss$PriorOral)
case_match = smahal(PriorOral2, data_case_nomiss[ ,c("ActingSGOrals", "hysLaw", 
                                                     "DCFirm", "ClerkDummy", 
                                                     "IdeologyOppCounsel", "demPres",
                                                     "OppPetOrResp", "GenderLawyerArgue",
                                                     "RaceLawyerArgue")])

case_match_1 = addcaliper(case_match, z = PriorOral2, p = data_case_nomiss$prop, caliper = .1)
case_matching_1 = pairmatch(case_match_1, data = data_case_nomiss)

dta_case = summarize.match(data_case_nomiss, case_matching_1)

plot(xBalance(WhoWon~PriorOral + hysLaw + DCFirm + ClerkDummy + Rehnquist + 
                Roberts + OppPetOrResp + IdeologyOppCounsel + demPres + GenderLawyerArgue + RaceLawyerArgue, 
              strata = list(unstrat = NULL, case_match =~ case_matching_1), 
              data = data_case_nomiss))
legend(
  "topright",
  legend = c("unstrat", "case_match"),
  inset = 0.01,
  pch = c(15, 16),
  bg = "white"
)

#Continue Analysis (p-value from a FRT of Fisher’s sharp null from the 1:1 matched data set)

#Justice-Level 

#Matched Data
n_comparisons = length(dta_justice$JVote.0)
permutations = 1000

tau_obs = sum(dta_justice$JVote.1 - dta_justice$JVote.0) / n_comparisons
tau_obs

sim_taus = numeric(permutations) 
set.seed(10)

for (i in 1:permutations) {
  # Randomly shuffle 50% of the rows
  shuffle_indices = sample(1:n_comparisons, n_comparisons / 2)
  
  # Create a shuffled version of org_ms.0$y.0 and org_ms.0$y.1
  shuffled_J0 = dta_justice$JVote.0
  shuffled_J1 = dta_justice$JVote.1
  
  shuffled_J0[shuffle_indices] = dta_justice$JVote.1[shuffle_indices]
  shuffled_J1[shuffle_indices] = dta_justice$JVote.0[shuffle_indices]
  
  tau_p = sum(shuffled_J1 - shuffled_J0) / n_comparisons
  
  sim_taus[i] = tau_p
}

p_value = mean(sim_taus >= tau_obs)
p_value

#Case-Level 
n_comparisons = length(dta_case$WhoWon.0)
permutations = 1000

tau_obs = sum(dta_case$WhoWon.1 - dta_case$WhoWon.0) / n_comparisons
tau_obs

sim_taus = numeric(permutations) 
set.seed(10)

for (i in 1:permutations) {
  # Randomly shuffle 50% of the rows
  shuffle_indices = sample(1:n_comparisons, n_comparisons / 2)
  
  # Create a shuffled version of org_ms.0$y.0 and org_ms.0$y.1
  shuffled_W0 = dta_case$WhoWon.0
  shuffled_W1 = dta_case$WhoWon.1
  
  shuffled_W0[shuffle_indices] = dta_case$WhoWon.1[shuffle_indices]
  shuffled_W1[shuffle_indices] = dta_case$WhoWon.0[shuffle_indices]
  
  tau_p = sum(shuffled_W1 - shuffled_W0) / n_comparisons
  
  sim_taus[i] = tau_p
}

p_value = mean(sim_taus >= tau_obs)
p_value

#Robustness Check

#Justice-Level
mu_0_model = lm(JVote.0 ~ hysLaw.0 + DCFirm.0 + ClerkDummy.0 + Rehnquist.0 + 
                  Roberts.0 + OppPetOrResp.0 + IdeologyOppCounsel.0 + demPres.0 
                + GenderLawyerArgue.0 + RaceLawyerArgue.0, data = dta_justice)
mu_1_model = lm(JVote.1 ~ hysLaw.1 + DCFirm.1 + ClerkDummy.1 + Rehnquist.1 + 
                  Roberts.1 + OppPetOrResp.1 + IdeologyOppCounsel.1 + demPres.1 
                + GenderLawyerArgue.1 + RaceLawyerArgue.1, data = dta_justice)

dta_justice_0 = dta_justice[ , c("hysLaw.0", "DCFirm.0", "ClerkDummy.0", "Rehnquist.0", 
                                "Roberts.0", "OppPetOrResp.0", "IdeologyOppCounsel.0", 
                                "demPres.0", "GenderLawyerArgue.0", "RaceLawyerArgue.0")]
head(dta_justice_0)
dta_justice_1 = dta_justice[ , c("hysLaw.1", "DCFirm.1", "ClerkDummy.1", "Rehnquist.1", 
                                "Roberts.1", "OppPetOrResp.1", "IdeologyOppCounsel.1", 
                                "demPres.1", "GenderLawyerArgue.1", "RaceLawyerArgue.1")]

dta_justice$mu_0_0 = predict(mu_0_model, newdata = dta_justice_0)
dta_justice$mu_0_1 = predict(mu_0_model, transmute(dta_justice, 
                                                   hysLaw.0 = hysLaw.1,
                                                   DCFirm.0 = DCFirm.1,
                                                   ClerkDummy.0 = ClerkDummy.1,
                                                   Rehnquist.0 = Rehnquist.1,
                                                   Roberts.0 = Roberts.1,
                                                   OppPetOrResp.0 = OppPetOrResp.1,
                                                   IdeologyOppCounsel.0 = IdeologyOppCounsel.1,
                                                   demPres.0 = demPres.1, 
                                                   GenderLawyerArgue.0 = GenderLawyerArgue.1,
                                                   RaceLawyerArgue.0 = RaceLawyerArgue.1))
dta_justice$mu_1_1 = predict(mu_1_model, newdata = dta_justice_1)

n_comparisons = length(dta_justice$JVote.0)
bias = sum(dta_justice$mu_0_1 - dta_justice$mu_0_0) / n_comparisons
bias

tau_pred = sum(dta_justice$JVote.1 - dta_justice$JVote.0) / n_comparisons
tau_pred

tau_corr = tau_pred - bias
tau_corr

sim_taus = numeric(permutations) 
set.seed(10)

for (i in 1:permutations) {
  # Randomly shuffle 50% of the rows
  shuffle_indices = sample(1:n_comparisons, n_comparisons / 2)
  
  # Create a shuffled version of org_ms.0$y.0 and org_ms.0$y.1
  shuffled_J0 = dta_justice$JVote.0
  shuffled_J1 = dta_justice$JVote.1
  
  shuffled_J0[shuffle_indices] = dta_justice$JVote.1[shuffle_indices]
  shuffled_J1[shuffle_indices] = dta_justice$JVote.0[shuffle_indices]
  
  tau_p = sum(shuffled_J1 - shuffled_J0) / n_comparisons
  
  sim_taus[i] = tau_p
}

p_value = mean(sim_taus >= tau_corr)
p_value

#Sensitivity Analysis

V_pred = (1 / n_comparisons**2) * sum(((dta_justice$JVote.1 - dta_justice$mu_1_1)**2) +
                                        ((dta_justice$JVote.0 - dta_justice$mu_0_0)**2))
V_pred

#Case-Level

mu_0_model = lm(WhoWon.0 ~ hysLaw.0 + DCFirm.0 + ClerkDummy.0 + Rehnquist.0 + 
                  Roberts.0 + OppPetOrResp.0 + IdeologyOppCounsel.0 + demPres.0 
                + GenderLawyerArgue.0 + RaceLawyerArgue.0, data = dta_case)
mu_1_model = lm(WhoWon.0 ~ hysLaw.1 + DCFirm.1 + ClerkDummy.1 + Rehnquist.1 + 
                  Roberts.1 + OppPetOrResp.1 + IdeologyOppCounsel.1 + demPres.1 
                + GenderLawyerArgue.1 + RaceLawyerArgue.1, data = dta_case)

dta_case_0 = dta_case[ , c("hysLaw.0", "DCFirm.0", "ClerkDummy.0", "Rehnquist.0", 
                                 "Roberts.0", "OppPetOrResp.0", "IdeologyOppCounsel.0", 
                                 "demPres.0", "GenderLawyerArgue.0", "RaceLawyerArgue.0")]
head(dta_justice_0)
dta_case_1 = dta_case[ , c("hysLaw.1", "DCFirm.1", "ClerkDummy.1", "Rehnquist.1", 
                                 "Roberts.1", "OppPetOrResp.1", "IdeologyOppCounsel.1", 
                                 "demPres.1", "GenderLawyerArgue.1", "RaceLawyerArgue.1")]

dta_case$mu_0_0 = predict(mu_0_model, newdata = dta_case_0)
dta_case$mu_0_1 = predict(mu_0_model, transmute(dta_case, 
                                                   hysLaw.0 = hysLaw.1,
                                                   DCFirm.0 = DCFirm.1,
                                                   ClerkDummy.0 = ClerkDummy.1,
                                                   Rehnquist.0 = Rehnquist.1,
                                                   Roberts.0 = Roberts.1,
                                                   OppPetOrResp.0 = OppPetOrResp.1,
                                                   IdeologyOppCounsel.0 = IdeologyOppCounsel.1,
                                                   demPres.0 = demPres.1, 
                                                   GenderLawyerArgue.0 = GenderLawyerArgue.1,
                                                   RaceLawyerArgue.0 = RaceLawyerArgue.1))
dta_case$mu_1_1 = predict(mu_1_model, newdata = dta_case_1)

n_comparisons = length(dta_case$WhoWon.0)
bias = sum(dta_case$mu_0_1 - dta_case$mu_0_0) / n_comparisons
bias

tau_pred = sum(dta_case$WhoWon.1 - dta_case$WhoWon.0) / n_comparisons
tau_pred

tau_corr = tau_pred - bias
tau_corr

sim_taus = numeric(permutations) 
set.seed(10)

for (i in 1:permutations) {
  # Randomly shuffle 50% of the rows
  shuffle_indices = sample(1:n_comparisons, n_comparisons / 2)
  
  # Create a shuffled version of org_ms.0$y.0 and org_ms.0$y.1
  shuffled_W0 = dta_case$WhoWon.0
  shuffled_W1 = dta_case$WhoWon.1
  
  shuffled_W0[shuffle_indices] = dta_case$WhoWon.1[shuffle_indices]
  shuffled_W1[shuffle_indices] = dta_case$WhoWon.0[shuffle_indices]
  
  tau_p = sum(shuffled_W1 - shuffled_W0) / n_comparisons
  
  sim_taus[i] = tau_p
}

p_value = mean(sim_taus >= tau_corr)
p_value

#Sensitivity Analysis

V_pred = (1 / n_comparisons**2) * sum(((dta_case$WhoWon.1 - dta_case$mu_1_1)**2) +
                                        ((dta_case$WhoWon.0 - dta_case$mu_0_0)**2))
V_pred




