#Julie Zill

########Import data and install packages########
setwd("~/Grad School/Thesis/Chapter 3 Mesocosm experiment/Datasheets")
rm(list=ls()) #removes all objects
#options(repos=structure(c(CRAN="http://cran.r-project.org"))) #fixes knitting error about CRAN mirror
#load("~/Grad School/Thesis/Chapter 3 Mesocosm experiment/Datasheets/workspace.RData") #fixes knitting problem with missing objects

#install.packages("vctrs")
library("vctrs")
#install.packages("tidyr")
library("tidyr")
#install.packages("dplyr")
library("dplyr")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("tidyverse", dependencies=TRUE)
library("tidyverse")

#Spreadsheet of background experimental conditions associated with each video. 
#In the master excel datasheet, this is sheet "VideoMetadataOnly".
#vdata <- read.csv("VideoMetadataOnly.csv", header=TRUE, sep=",", na.strings = c("NA","N/A")) #imports worksheet data as a dataframe
metadata <- read_csv("VideoMetadataOnly.csv") #imports worksheet data as a dataframe using tidyverse function
#head(vdata) #checks that it was imported correctly
sapply(metadata, class) #tells the data class/type of each column in the datasheet

#Data that is pulled from video events, being entered continuously in the Google drive spreadsheet opened in Sheets: https://drive.google.com/file/d/1_yyjQ-Jskd04zF6Rje33B89JKl1a9Z6J/view?usp=sharing
#In the master excel datasheet, this is sheet "DataEntry_ByEvent_[date of last data transfer from online sheet of the same name to master datasheet]".
#Event data is transferred from the online worksheet 
#edata <- read.csv("DataEntry_ByEvent_12-07-20.csv", header=TRUE, sep=",", na.strings = c("NA","N/A")) #imports worksheet data as a dataframe
edata <- read_csv("DataEntry_ByEvent_12-07-20.csv") #imports worksheet data as a dataframe using tidyverse function
#head(edata) #checks that it was imported correctly
sapply(edata, class) #tells the data class/type of each column in the datasheet
View(edata)


#data<-join(edata, vdata, by = "VidNum", type = "left", match = "all")
#data<-join(edata, vdata, by = c("VidNum"), type = "left", match = "all")
#data <- merge(edata, vdata, by.x=c("VidNum"), by.y=c("VidNum"),, all=TRUE,sort=FALSE) 
#data <- merge(x=edata, y=vdata, by.x=edata$VidNum, by.y=vdata$VidNum, all.y=TRUE)
#data <- semi_join(edata, vdata, by = c("VidNum"))
data <- left_join(edata, metadata, by =c("VidNum"), copy = FALSE)
#data <- anti_join(edata, vdata, by = c("VidNum"), copy = FALSE)

write.csv(data, file="MergedData.csv")
#
#add column for DayNight
data <- mutate(data,DayNight = ifelse((TimeOfDay_Daytime123Nighttime123 %in% c("Daytime1","Daytime2","Daytime3")),"Day","Night"))
data <- rename(data,ExptDay = ExperimentDay_34567or8, TimeofDay = TimeOfDay_Daytime123Nighttime123,Trtmt = `TTTName`,ShelterType = ShelterType_SmallorLarge)
#reorder the levels of factors so they are in a sensible order (small before large, etc)
data$ShelterType <- factor(data$ShelterType,levels=c("Small","Large"))
data$Trtmt <- factor(data$Trtmt,levels=c("Control","SmallEel","BigEel"))

head(data)

#create vdata, which is the video data in horizonatl form (one row per video)
vdata <-metadata

#Calculate AllSmallManinisTimeInsideThisShelter
#test this out
data %>% 
  filter(CreatureID_EelManini %in% c("manini","Manini"),
         Size_SmallBig %in% c("small","Small"),
         is.na(TorpedoStatus_BeforeDuringAfter)) %>%
  group_by(VidNum) %>% 
  summarise(AllSmallManinisTimeInsideThisShelter = sum(UsageDuration_Secs))

#All small manini time in shelter
vdata <- left_join(vdata, 
                   data %>% 
                     filter(CreatureID_EelManini == "Manini",Size_SmallBig %in% c("Small","small")) %>%
                     group_by(VidNum) %>% 
                     summarise(AllSmallManinisTimeInsideThisShelter = sum(UsageDuration_Secs)),
                   by="VidNum")
#All large manini time in shelter
vdata <- left_join(vdata, 
                   data %>% 
                     filter(CreatureID_EelManini == "Manini",Size_SmallBig %in% c("Big","big")) %>%
                     group_by(VidNum) %>% 
                     summarise(AllBigManinisTimeInsideThisShelter = sum(UsageDuration_Secs)),
                   by="VidNum")
#Add another column here!


#once you have your video data by row, then - for analysis - you need to change it to vertical form with one row by unique video x sizee x creature 







########################Megan's Code########################
#
jdata <- read_csv("R/EelMesocosm_v2_VideoAnalysis_Zill_GoogleDriveSpreadsheet_11-09-20_ForPrelimRPlots.csv")
#View(jdata)
#add column for DayNight
jdata <- mutate(jdata,DayNight = ifelse((TimeOfDay_Daytime123Nighttime123 %in% c("Daytime1","Daytime2","Daytime3")),"Day","Night"))
jdata <- rename(jdata,Expt = ExperimentDay_34567or8, TimeofDay = TimeOfDay_Daytime123Nighttime123,Trtmt = `TTTName_Control,TwoSmallEels,TwoBigEels,OneBigOneSmall`,ShelterType = ShelterType_SmallorLarge)
#reorder the levels of factors so they are in a sensible order (small before large, etc)
jdata$ShelterType <- factor(jdata$ShelterType,levels=c("Small","Large"))
jdata$Trtmt <- factor(jdata$Trtmt,levels=c("Control","SmallEel","BigEel"))

#rearrange data so that TotalTime, TotalUsages, MaxN for both regular and torpedo treatments are coded by fish size
by.size <- bind_rows(  #this will stack the two following sets of data on top of each other, matching column headings
  jdata %>%  #select "key" column (VidNum) from jdata along w columns associated with small manini
    select(VidNum,
           ShelterTime = AllSmallManinisTimeInsideThisShelter_ImplicitlyOnCam,
           MaxN = MaxN_SmallManiniInside_ImplicitlyOnCam,
           Usage = TotalUsages_SmallManini,
           ShelterTime_BeforeT = `30sBeforeTorpedo_AllSmallManinisTimeInsideThisShelter_ImplicitlyOnCam`,
           MaxN_BeforeT = `30sBeforeTorpedo_MaxN_SmallManiniInside_ImplicitlyOnCam`,
           ShelterTime_DuringT = `30sDuringTorpedo_AllSmallManinisTimeInsideThisShelter_ImplicitlyOnCam`,
           MaxN_DuringT = `30sDuringTorpedo_MaxN_SmallManiniInside_ImplicitlyOnCam`,
           ShelterTime_AfterT = `30sAfterTorpedo_AllSmallManinisTimeInsideThisShelter_ImplicitlyOnCam`)%>% 
    add_column(ManiniSize="Small"),  #add column indicating small
  jdata %>%   #select "kay" column and columns associated with big manini
    select(VidNum,
           ShelterTime=AllBigManinisTimeInsideThisShelter_ImplicitlyOnCam,
           MaxN=MaxN_BigManiniInside_ImplicitlyOnCam,
           Usage=TotalUsages_BigManini,
           ShelterTime_BeforeT = `30sBeforeTorpedo_AllBigManinisTimeInsideThisShelter_ImplicitlyOnCam`,
           MaxN_BeforeT = `30sBeforeTorpedo_MaxN_BigManiniInside_ImplicitlyOnCam`,
           ShelterTime_DuringT = `30sDuringTorpedo_AllBigManinisTimeInsideThisShelter_ImplicitlyOnCam`,
           MaxN_DuringT = `30sDuring_MaxN_BigManiniInside_ImplicitlyOnCam`,
           ShelterTime_AfterT = `30sAfterTorpedo_AllBigManinisTimeInsideThisShelter_ImplicitlyOnCam`) %>% 
    add_column(ManiniSize="Big"))  #add a column indicating big

#select out the data that is not manini-size dependent.
by.video <- jdata %>% select(VidNum:`NumSmallEelsInTank_NoEelsOnDays3,4`,ManiniAggressionEvents_TotalNumofChases:ReliabilityNotes,TorpedoManiniAggressionEvents_TotalNumofChases:DayNight)

#this is the dataset with ManiniSize as a column and new variables
#  ShelterTime, MaxN, Usage, ShelterTime_BeforeT, MaxN_BeforeT, ShelterTime_DuringT, MaxN_DuringT, ShelterTime_AfterT
jdata.vert <- left_join(by.size,by.video)
jdata.vert$ManiniSize <- factor(jdata.vert$ManiniSize,levels=c("Small","Big"))

#First simple plot for small manini durign the day
jdata.vert %>% 
  filter(Expt==6, DayNight=="Day",ManiniSize=="Small") %>%
  group_by(Trtmt, ShelterType) %>% 
  summarise(AvgTime = mean(ShelterTime)+0.01) %>%   #I added 0.01 so we could see the zeros
  ggplot(.,aes(fill=ShelterType,x=Trtmt,y=AvgTime)) + 
  geom_bar(position="dodge", stat="identity")

#Panel plots!

jdata.vert %>% 
  filter(Expt==6) %>% 
  group_by(DayNight, ManiniSize,Trtmt,ShelterType) %>% 
  summarise(AvgTime = mean(ShelterTime)+0.5) %>%      #adding 0.5 here, so the zeros show up
  ggplot(aes(fill=ShelterType,x=Trtmt,y=AvgTime)) +
  facet_grid(rows=vars(DayNight),cols=vars(ManiniSize))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Average Shelter Time")

#same plot but with log10-transformed time so it is easier to see
jdata.vert %>% 
  filter(Expt==6) %>% 
  group_by(DayNight, ManiniSize,Trtmt,ShelterType) %>% 
  summarise(AvgLog10Time = mean(log10(ShelterTime+1))+0.01) %>%   #adding 0.01 so that zeros show up
  ggplot(aes(fill=ShelterType,x=Trtmt,y=AvgLog10Time)) +
  facet_grid(rows=vars(DayNight),cols=vars(ManiniSize))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Average Log10(Shelter Time)")

#plotting the three replicates separatly, on a log10 scale.
jdata.vert %>% 
  filter(Expt==6) %>% 
  group_by(DayNight, ManiniSize,Trtmt,ShelterType,ReplicateNum) %>% 
  summarise(log10ShelterTime = mean(log10(ShelterTime+1))+0.01) %>% 
  ggplot(aes(fill=ShelterType,x=Trtmt,y=log10ShelterTime)) +
  facet_grid(rows=vars(DayNight),cols=vars(ManiniSize))+
  geom_point(aes(color=ShelterType), stat="identity")+
  ylab("Log_10 Time in Shelter")


#how about the torpedo response
jdata.vert %>% 
  filter(Expt==8,DayNight=="Day") %>% 
  mutate(IncreaseInUse = ShelterTime_DuringT-ShelterTime_BeforeT) %>% 
  group_by(ManiniSize,Trtmt,ShelterType,ReplicateNum) %>%
  summarise(AvgIncreaseInUse = mean(IncreaseInUse)+0.1) %>%   #adding 0.1 to visualize zeros
  ggplot(aes(fill=ShelterType,x=Trtmt,y=AvgIncreaseInUse)) +
  facet_grid(cols=vars(ManiniSize))+
  geom_bar(position="dodge", stat="identity")+
  ylab("Average Increase in Shelter Use Time when exposed to Torpedo")
















###########################

##############Subsetting data by eel size x shelter presence##############
TotalNumEelsInsideThisShelter_AtBeginning
TotalNumEelsInTank
NumBigEelsInsideShelter_AtBeginning
NumBigEelsInTank_NoEelsOnDays3,4
NumSmallEelsInsideShelter_AtBeginning
NumSmallEelsInTank_NoEelsOnDays3,4




Datasubset1 <- subset(Data,TotalNumEelsInsideThisShelter_AtBeginning == 1&TotalNumEelsInTank == 1) #Any size of eel is inside this shelter
write.csv(Datasubset1, file="NewData1.csv")

Datasubset1 <- subset(Data,TotalNumEelsInsideThisShelter_AtBeginning == 0&TotalNumEelsInTank == 1) #eel in tank, but not this shelter, Any size of eel is inside this shelter
write.csv(Datasubset1, file="NewData1.csv")

###########################

###Prove treatments by first showing how much time eels spent in each shelter (6 columns x TTT)


###In general, plot each (replicate's) average as points (not bar). Color code by replicate, then by time of day (to look and see if colors have a pattern, which would indicate that that random effect is meaningful/significant)


###If I want to associate the scratch pad data with the full datasheet, add a column (VidNum), and then use function leftjoin to add 

##############Component 1a: Shelter usage during the daytime##############
#Do maninis use shelter less in the daytime when eels are present (but stationary)? Is it size dependent? If more shelter unavailable, does competition for safe space increase?

##Total manini time (or usage) by TTTname

##Q1: do maninis use shleter less (by mins or usage) in the presence of eels (by TTTname)
##TotalTimeAllManinisInside ~ TTTName

in order to concatenate, use bindrows or join. You're basically taking all of the rows and pasting it underneath (while retaining identity [large small] of each row)
use the simple analysis to detect overall large effects, and then use the specific context to explain

##Big maniniTotal manini time (or usage) by TTTname Size is on the right hand side of the equation
##Was the behavior different in the day, when TTTName interacting with manini size
##Select Daytime=1 (true). Number of secs in shelter as a function of eel presence in shelter 
##ask questions serially

##########################
hist(Data$TotalTimeAllManinisInsideThisShelter_ImplicitlyOnCam)
par(bg = 'seashell', mfrow = c(1, 1)) #1 row, 1 columns
Means_TotalTimeAllManinisInsideThisShelter_By_TotalNumEelsInsideThisShelter = tapply(Data$TotalTimeAllManinisInsideThisShelter_ImplicitlyOnCam, Data$TotalNumEelsInsideThisShelter_AtBeginning,  mean)
barplot(Means_TotalTimeAllManinisInsideThisShelter_By_TotalNumEelsInsideThisShelter,  pch = 19,   xlab    = 'Total Eels Inside This Shelter',   ylab    = 'Total Time All Maninis Inside This Shelter (sec)',    main    = 'Average time of all maninis spent inside shelter x any eel presence',   col = 'lightseagreen',    las =   1, ylim    = c(0,80))

Datasubset1 <- subset(Data.df,Eel_NonEel == 1&ObservedBaitedInsitu == 1)
write.csv(Datasubset1, file="NewData1.csv")



























Datasubset2 <- subset(Data.df,Eel_NonEel == 0)
write.csv(Datasubset2, file="NewData2.csv")

#For Datasubset1 - sum of "Biomass_g" and "Eels_NonEels=1" by site

NewData1<-read.csv("~/Grad School/Thesis/Datasheets/NewData1.csv", header = TRUE,na.strings = "NA",sep=",",blank.lines.skip = TRUE, as.is = TRUE, dec=".")
names(NewData1)
is.factor(NewData1$SurveyID)

#if False, Transform to factor
NewData1$SurveyID <- factor(NewData1$SurveyID)
Biomass<-aggregate(NewData1$Biomass_g, by=list(SurveyID=NewData1$SurveyID), FUN=sum)
Numbers<-aggregate(NewData1$Eel_NonEel, by=list(SurveyID=NewData1$SurveyID), FUN=sum)
Merge1 <- merge(Biomass, Numbers, by=c("SurveyID"), all=TRUE,sort=FALSE) 
Merge1 <- setNames(Merge1, c("SurveyID","Total_biomass","Total_Numbers "))
##########################  



##############Component 1b: Shelter usage during the night time##############
#Night: Maninis take a more stationary position at night (usually just outside the shelter entry), while eels become active. Does the eel activity reduce or interrupt herbivore shelter usage?
##########################






##########################


#############Experiment component #2: Herbivory#############
#Does the presence of an eel on a "coral head" reduce algae consumption by herbivores? Is it size dependent?
  
#Response variable:
#g of algae consumed on each shelter after 3 hrs (before/after eels are present in tank)

#Predictions of interest:
#More algae consumed on eel-free shelters
#Big maninis might perceive their body size refuge and feed on shelters occupied by small eels 
##########################



#############Experiment component #3: Refuge from transient predators#############
#What if the NEED to use shelter is increased?
  
#Under immediate threat from a transient predator, will maninis choose to share a shelter with an eel, or remain exposed?
  
#Response variables (for the 30 secs before, during, and after threat):
#Max N of manini inside each shelter
#total seconds big or small maninis spend inside each shelter
#Num manini intraspecific aggression events

#With increasing need for shelter, intraspecific aggression events will increase.
#(Before threat aggression << During aggression for all TTTs)

#Shelter usage predictions are the same as that for component #1, except amplified.

#When eels are present in the tank, maninis may be quicker to leave shelters after the threat.
##########################


##########################

##########################






##########################





##########################

##########################

##########################

##########################
par(bg = 'aliceblue')
plot(Data$sole_presence	~ Data$salinity,	pch	= 19,	ylim	= c(0,1),	xlab	= 'Salinity',	ylab	
     = 'Presence (1) or Absence (0) of sole',	main	= 'Raw data plot: Sole Presence vs Salinity',	col	= 'black',	las	=	1)


##Code from lecture9_GLMs4.pdf
max(Data$salinity) - min(Data$salinity) #find the range of data so we can choose a factor by which to divide it (32/8 = 4)
breaks	= with(Data,	seq(min(salinity),	max(salinity),	length	= 9))
breaks
#make	a	binning	factor	from	salinity
cut.salinity	= cut(Data$salinity,	breaks	= breaks)
#calculate	the	proportion	of	presence	by	bin
means	= with(Data,	tapply(sole_presence,	cut.salinity,	mean))
#function	for	the	standard	error	of	a	binary	variable
binomial.SE	= function(x)	sqrt((mean(x)*(1-mean(x)))/length(x))
#calculate	the	standard	error	for	each	bin
SEforeachbin	= with(Data,	tapply(sole_presence,	cut.salinity,	binomial.SE))
#calculate	the	midpoint	of	each	bin
bin.midpoints	= (breaks[1:8]	+ breaks[2:9])/2
bin.midpoints
#plot	the	bin	means
plot(means	~ bin.midpoints,	pch	= 19,	ylim	= c(0,1),	xlab	= 'Salinity',	ylab	
     = 'Proportion	 present',	main	= 'Sole Presence vs. 	Salinity',	col	= 'darkblue',	las	=	1, cex=1.5)
#plot	the	bin	SEs
segments(bin.midpoints,	means+SEforeachbin,	bin.midpoints,	means-SEforeachbin, col	= 'darkgoldenrod1')
#plot the raw data back over the graph of binned proportions
with(Data,	points(sole_presence	~ salinity, col	= 'black'))
```

<font color="008A7B">
  1. contd. Fit a binomial glm to test whether presence/absence of sole is driven by salinity.
Use curve() to plot the fitted logistic relationship on the same plot as the raw data</font> (and the mean binned proportions, I'm assuming[?])

Answer 1 contd:
```{r}
model<-glm(sole_presence ~ salinity, data=Data, family=binomial(link ="logit"))
summary(model)



#Function for logistic curve (from lecture8_2019_binomial.pdf notes)
logistic = function(x) exp(x)/(1+exp(x))

##"A probability ranges from 0 to 1, so we need to model the probability with a function that has an unrestricted range on the x-axis, but varies between 0 and 1 on the y-axis: the logistic function."
#Plot the fitted curve
par(bg = 'aliceblue')
plot(means	~ bin.midpoints,	pch	= 19,	ylim	= c(0,1),	xlab	= 'Salinity',	ylab	
= 'Proportion	 present',	main	= 'Sole Presence vs. 	Salinity',	col	= 'darkblue',	las	=	1, cex=1.5)
segments(bin.midpoints,	means+SEforeachbin,	bin.midpoints,	means-SEforeachbin, col	= 'darkgoldenrod1')
with(Data,	points(sole_presence	~ salinity, col	= 'black'))
curve(logistic(coef(model)[1]+coef(model)[2]*x), add = T, col = 'cyan3', lwd=3)
```

<font color="008A7B">
1. contd. What is the effect of salinity on sole presence/absence? Would you consider it a strong effect or a weak effect? Why?
Do a likelihood ratio test to see whether the effect of salinity is significant. 
Examine a plot of residuals (deviance residuals or pearson residuals) vs. predicted values. How would you interpret this plot?
</font>

Answer 1 contd:
```{r}
library("effects")
##It's easier to interpret the coefficients of a logistic model on a logit scale: logit = inverse of the logistic function
                                                                                              #The y-axis of the effects plot is on the logit scale in order to show the effects as linear (but if the original graph is too zoomed in, it may appear linear regardless).
                                                                                              plot(allEffects(model))
                                                                                              
                                                                                              ##Likelihood ratio test to see if salinity is significant
                                                                                              library("car")
                                                                                              Anova(model)
                                                                                              
                                                                                              par(mfrow=c(2,2)) # the next 4 plots will be arranged in 2 rows and 2 columns
                                                                                              plot(model) #first plot shows Residuals vs. Predicted values
                                                                                              par(mfrow = c(1, 1)) # Put plotting arrangement back to its original state
                                                                                              ```
                                                                                              
                                                                                              <b>Salinity has a negative effect on the presence of sole (coefficient = -0.12985). I consider this negative effect to be fairly strong, because as salinity increases by 1, the log odds decrease by 0.12985 (the odds are multiplied by e^-0.12985, which is 0.87). The likelihood test shows that the effect of salinity is highly significant (p=1.355e-05). Sole are more likely to be present in areas of low salinity.
                                                                                              The Residuals vs. Predicted values plot shows that there is a strong pattern in the residuals (as expected with binary data), but the <u>mean</u> of the residuals does not show a strong pattern (which is what we want).</b>
                                                                                                
                                                                                                ***
                                                                                                ***
                                                                                                
                                                                                                
                                                                                                <font color="008A7B">
                                                                                                2. (15 pts)
                                                                                              The file "heat_shock_subset.csv" includes a subset of the data from the silverleaf whitefly heat shock experiment that you looked at in homework 2 (and the original paper was attached to that assignment). Each row of the dataset is an experimental replicate. In each replicate, 10 flies were
                                                                                              subjected to a heat shock, where they were acclimated to 40?C for 1 hour, then reduced to 25?C for one hour, then shocked at 45?C for 1 hour. The number of survivors at the end is recorded in the column 'Survival', and the proportion of survivors is in 'proportion'. This experiment has a somewhat complex structure, which we'll analyze fully when we get to mixed models. For now let's imagine a simpler world where the only important factors are 'sex' and 'region'. The experiment used flies from two regions in Colombia, the Caribbean region (sea level, uniformly hot) and the Southwest region (in the Andes, more variable temperature). The researchers hypothesized that the flies from these two regions might be locally adapted to thermal conditions, leading to different heat shock tolerances. Experimental replicates were performed on males and females separately, to quantify any effect of sex on heat shock tolerance.
                                                                                              Make an exploratory plot of proportion of survivors vs. sex*region.
                                                                                              Fit a binomial glm where you test for effects of sex and region on the probability of surviving, as well as an interaction between sex and region. Remember there are two different ways to fit a binomial glm where n > 1. They are equivalent. Plot the fitted effects. How do you interpret the results?
                                                                                                Do a likelihood ratio test for the interaction between sex and region.
                                                                                              </font>
                                                                                                
                                                                                                Answer 2:
                                                                                                ```{r}
                                                                                              Data <- read.csv("heat_shock_subset.csv", header=TRUE, sep=",", na.strings = c("NA","N/A")) #imports worksheet data as a dataframe
                                                                                              #head(Data) #checks that it was imported correctly
                                                                                              sapply(Data, class) #tells the data class/type of each column in the datasheet
                                                                                              
                                                                                              par(bg = 'seashell', mfrow = c(1, 2)) #1 row, 2 columns
                                                                                              plot(proportion	~ sex,	pch	= 19,	ylim	= c(0,1),	xlab	= 'Sex',	ylab	
                                                                                                   = 'Proportion of survivors',	main	= 'Exploratory plot: prop by sex',	col	= 'lightseagreen',	las	=	1, data=Data)
                                                                                              plot(proportion	~ region,	pch	= 19,	ylim	= c(0,1),	xlab	= 'Region',	ylab	
                                                                                                   = 'Proportion of survivors',	main	= 'Exploratory plot: prop by region',	col	= 'deeppink4',	las	=	1, data=Data)
                                                                                              par(mfrow = c(1, 1)) # Put plotting arrangement back to its original state
                                                                                              
                                                                                              #Fitting a model of proportion by sex, region, and interaction of sex*region, with number of flies sampled per row = n
                                                                                              model <- glm(proportion ~ sex	+ region + sex*region, weights	= n,	data	= Data,	family	= binomial)
                                                                                              plot(allEffects(model)) #visualize effects
                                                                                              Anova(model) #Likelihood test for effects' significance
                                                                                              
                                                                                              ```
                                                                                              
                                                                                              <b>There appears to be an interaction between sex*region, because the relationship (slope) between Females and Males changes depending on region. Males seem to be less tolerant to heat shock than females, and Southwest flies generally survive heat shock better than Caribbean flies, but Caribbean males seem to be especially vulnerable to heat shock. The likelihood tatio test showed sex, region, and the interaction term to all have significant effects on survivorship (p= 4.177e-09, 1.203e-09, 0.007515 respectively).</b>
                                                                                                
                                                                                                <font color="008A7B">
                                                                                                2. contd. Oops, we didn't consider overdispersion. Fit the same model using the quasibinomial method. 
                                                                                              How big is the dispersion parameter?
                                                                                                Test the interaction with the quasibinomial model, using the F-test that accounts for
                                                                                              the dispersion parameter. How do you interpret this result, compared to the result
                                                                                              from the first model?
                                                                                                Test the main effects of the model (sex and region), using F-tests where the
                                                                                              sex*region interaction is no longer in the 'full' model. What's your final take on
                                                                                              these results? Are the results consistent with the researchers' hypothesis of local
                                                                                              adaptation?
                                                                                                </font>
                                                                                                
                                                                                                
                                                                                                Answer 2. contd.:
                                                                                                ```{r}
                                                                                              #Fitting a quasibinomial model of proportion by sex, region, and interaction of sex*region, with number of flies sampled per row = n
                                                                                              model_quasibinomial <- glm(proportion ~ sex	+ region + sex*region, weights	= n,	data	= Data,	family	= quasibinomial)
                                                                                              summary(model_quasibinomial) #Dispersion parameter phi = 2.285886, which is high (greater than 1.5 is problematic)
                                                                                              
                                                                                              Anova(model_quasibinomial,	test	= 'F') #"F-test that is derived from the likelihood ratio corrected with the dispersion factor"
                                                                                              plot(allEffects(model_quasibinomial)) #visualize effects.
                                                                                              
                                                                                              #Interaction term no longer significant. Remove.
                                                                                              model_quasibinomial_MainEffects <- glm(proportion ~ sex	+ region, weights	= n,	data	= Data,	family	= quasibinomial)
                                                                                              summary(model_quasibinomial_MainEffects) #Dispersion parameter phi = 2.293669, which is high (greater than 1.5 is problematic)
                                                                                              
                                                                                              Anova(model_quasibinomial_MainEffects,	test	= 'F') #"F-test that is derived from the likelihood ratio corrected with the dispersion factor"
                                                                                              plot(allEffects(model_quasibinomial_MainEffects)) #visualize effects
                                                                                              
                                                                                              ```
                                                                                              
                                                                                              <b>For the full quasibinomial model with the interaction term, the dispersion parameter phi was 2.285886, which is high (greater than 1.5 is problematic).
                                                                                              Changing the binomial model to quasibinomial corrected for overdispersion, which caused p values and confidence intervals to increase. The interaction term sex*region was no longer significant in the quasibinomial model (p = 0.0774451). After removing the interaction term, the main effects sex and region were shown to have a significant effect on survivorship (p = 0.000113, 6.535e-05). Males were much less tolerant than female flies, and Southwest flies had a higher proportion of survivors than the Caribbean. The results support the researchers' hypothesis that the flies in the stable Caribbean (consistently ~32C year round) would be more vulnerable to heat shock of 45C than those in the more temperature-variable Andes. Although no hypothesis about sex was made, perhaps there could be some ecological difference or physiological dimorphism that allows female flies to better survive heat shock than male flies.</b>


***
***
***
```{r}
save.image("~/Grad School/OCN 683 Adv Statistics in R/Homework/workspace.RData") #fixes knitting problem with missing objects
```

