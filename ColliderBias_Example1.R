set.seed(7788)

# height distribution of american males - better sources did not show up right away
# https://www.cabarrus.k12.nc.us/cms/lib09/NC01910456/Centricity/Domain/4633/AP%20Stats%20Chapter%202%20Day%203.pdf
NPop <- 1e6

# inches
Height <- rnorm(n=NPop,mean = 69,sd=2.5)

# Talent
Talent <- rbeta(n=NPop,shape1 = 1,shape2=1e3)
SkillScore <- as.numeric(0.6*scale(Talent) + 0.4*scale(Height))

# Define NBA Membership
NBA_LO_1 <- SkillScore - 7
NBA_LO_2 <- SkillScore - 8 + ifelse(scale(Height) >= 2,3.5,0)
NBA_Prob_1 <- exp(NBA_LO_1) / (1 + exp(NBA_LO_1))
NBA_Prob_2 <- exp(NBA_LO_2) / (1 + exp(NBA_LO_2))
NBA_Res_1 <- rbinom(n=NPop,size=1,prob=NBA_Prob_1)
NBA_Res_2 <- rbinom(n=NPop,size=1,prob=NBA_Prob_2)

NBA_Res <- NBA_Res_2

# finally, generate the response variable
# skill in points contest


## take all of the NBA players, and an
# equally sized random sample of the others 

whichNBA_1 <- which(NBA_Res_1 == 1)
whichAmateur_1 <- which(NBA_Res_1 == 0)
NumNBA_1 <- length(whichNBA_1)

amateurIndsToUse_1 <- sample(whichAmateur_1,size=NumNBA_1,replace=FALSE)

analysisDF_1 <- data.frame(SkillScore = c(SkillScore[whichNBA_1],
                                        SkillScore[amateurIndsToUse_1]),
                         Status = rep(c("Pro","Amateur"),each=NumNBA_1),
                         Height = scale(Height)[c(whichNBA_1,amateurIndsToUse_1)],
                         Talent = scale(Talent)[c(whichNBA_1,amateurIndsToUse_1)])
NumAnalysis_1 <- dim(analysisDF_1)[1]


whichNBA_2 <- which(NBA_Res_2 == 1)
whichAmateur_2 <- which(NBA_Res_2 == 0)
NumNBA_2 <- length(whichNBA_2)

amateurIndsToUse_2 <- sample(whichAmateur_2,size=NumNBA_2,replace=FALSE)

analysisDF_2 <- data.frame(SkillScore = c(SkillScore[whichNBA_2],
                                          SkillScore[amateurIndsToUse_2]),
                           Status = rep(c("Pro","Amateur"),each=NumNBA_2),
                           Height = scale(Height)[c(whichNBA_2,amateurIndsToUse_2)],
                           Talent = scale(Talent)[c(whichNBA_2,amateurIndsToUse_2)])
NumAnalysis_2 <- dim(analysisDF_2)[1]

# just randomly sample the population
randomInds_1 <- sample(1:NPop,size=NumAnalysis_1,replace=FALSE)
randomSampleDF_1 <- data.frame(SkillScore = SkillScore[randomInds_1],
                             Status = NBA_Res_1[randomInds_1],
                             Height = scale(Height)[randomInds_1],
                             Talent = scale(Talent)[randomInds_1])

randomInds_2 <- sample(1:NPop,size=NumAnalysis_2,replace=FALSE)
randomSampleDF_2 <- data.frame(SkillScore = SkillScore[randomInds_2],
                               Status = NBA_Res_2[randomInds_2],
                               Height = scale(Height)[randomInds_2],
                               Talent = scale(Talent)[randomInds_2])

## collider bias idea:
# including NBA status as a covariate will bias
#   the causal estimate of height on skill
# in what direction? - I predict it will be smaller than it should

mSmall_Random_1 <- lm(SkillScore ~ Height,data = randomSampleDF_1)
mSmall_1 <- lm(SkillScore ~ Height,data = analysisDF_1)
mFull_1 <- lm(SkillScore ~ Height + Status,data=analysisDF_1)

mSmall_Random_2 <- lm(SkillScore ~ Height,data = randomSampleDF_2)
mSmall_2 <- lm(SkillScore ~ Height,data = analysisDF_2)
mFull_2 <- lm(SkillScore ~ Height + Status,data=analysisDF_2)

### summary graphics
require(ggplot2)
require(ggthemes)
require(scales)

LowerVec <- c(confint(mSmall_Random_1)[2,1],
              confint(mSmall_1)[2,1],
              confint(mFull_1)[2,1],
              confint(mSmall_Random_2)[2,1],
              confint(mSmall_2)[2,1],
              confint(mFull_2)[2,1])
UpperVec <- c(confint(mSmall_Random_1)[2,2],
              confint(mSmall_1)[2,2],
              confint(mFull_1)[2,2],
              confint(mSmall_Random_2)[2,2],
              confint(mSmall_2)[2,2],
              confint(mFull_2)[2,2])
ScenarioVec <- rep(c("Scenario1\nNo Collider Bias","Scenario2\nCollider Bias"),
                   each = 3)
ModelVec <- rep(c("Random,Small","Equal-Sample,Small","Equal-Sample,Full"),
                times = 2)

plotDF <- data.frame(Lower = LowerVec,
                     Upper = UpperVec,
                     Scenario = ScenarioVec,
                     Model = ModelVec)

plot1 <- ggplot(data = plotDF)
plot1 <- plot1 + geom_segment(aes(x = Lower,xend=Upper,
                                  y=Model,yend=Model),size=2)
plot1 <- plot1 + facet_wrap(~ Scenario,nrow=2)
plot1 <- plot1 + geom_vline(xintercept = 0.4,lty=2,color="red",size=2)
plot1 <- plot1 + theme_tufte()
plot1 <- plot1 + labs(x = "Causal Effect Estimate of Height on Skill",
                      title = "95% CIs for the Causal Effect of Height on Skill\nVertical Line Represents True Effect Size")
plot1 <- plot1 + theme(axis.text.x = element_text(size=12),
                       axis.title.x = element_text(size=14),
                       axis.text.y = element_text(size=12),
                       axis.title.y = element_text(size=14),
                       title = element_text(size=16),
                       strip.text = element_text(size=14),
                       panel.background = element_rect(fill = "#f8f8f8"),
                       strip.background = element_rect(fill = "gray"))
ggsave(plot1,file = "heightSkillColliderExample.png",
       units = "in",width=12,height=8)