require(plyr)
library(ggplot2)
library(ggsignif)
library(dplyr)
library(reshape2)
library(scales)
library(lubridate)
library(tibble)
library(tidyverse)
library(polycor)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(grid)
library(patchwork)
library(cowplot)


setwd("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance")

#read in source file, format columns
allsamples <- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\MultiModalSurveillance_Results.csv", stringsAsFactors = FALSE)

####CLEANING DATA#####################
#set blanks to NA
allsamples[allsamples=='']=NA

#set CollectionDate as  date format
allsamples <- allsamples %>%
  mutate(CollectionDate=mdy(CollectionDate)) %>%
  mutate(stack.n = as.numeric(stack.n))

allsamples <- allsamples %>%  
  mutate(Day = as.numeric(Day))

allsamples$Result.clean= "Negative"
allsamples$Result.clean[allsamples$Result=="Positive (2/3)"] <- "Positive"
allsamples$Result.clean[allsamples$Result=="Positive (3/3)"] <- "Positive"
allsamples <- allsamples %>% filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp')


#REMOVING NON POSITIVE FLOOR AND LOBBY SURFACE SAMPLES FROM ALLSAMPLES FOR ALL FUTURE ANALYSIS - XCX FILTER AT BEGINNING EITHER WAY
allsamples <- allsamples %>%
  filter(Location != '7th Floor Elevator Call Button') %>%
  filter(Location != '7th Floor S Hallway Door Handle') %>%
  filter(Location != 'Traced Floor Elevator Railing') %>%  
  filter(Location != 'Traced Floor Elevator Buttons')
  #filter(Location != 'Ground Floor Stairway Door Handle')
  #filter(Location != 'W. Entrance Door Handle Exterior') %>%
  #filter(Location != 'W. Entrance Door Handle Interior') %>%
  #filter(Location != 'Lobby Call Elevator Button')
  
cor.test(allsamples$ct[which(allsamples$Room.Type == 'Indoors')],
         allsamples$wing.n[which(allsamples$Room.Type == 'Indoors')])

#Make roof df and fill occupied column#####
roof <- allsamples[!grepl('Shop', allsamples$Room.Type), ]


###################BOXPLOT Stack######################

roof$occupied <- ifelse(roof$stack.n > 0, "Occupied", "Unoccupied")

roof %>%drop_na(ct)%>% 
  drop_na(occupied) %>%
  filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp') %>%
  filter(Room.Type == 'Roof') %>%
  ggplot(aes(x = occupied, y = ct, )) +
  geom_violin(aes(fill = occupied), show.legend = FALSE)+
  geom_boxplot(color = 'darkgrey', width = .2, outlier.shape = NA, aes(alpha = 0.5), show.legend = FALSE) +
  geom_signif(comparisons = list(c("Unoccupied", "Occupied")),
              map_signif_level = TRUE, 
              test = t.test, test.args=list(alternative = "less", var.equal = FALSE, paired=FALSE), textsize = 7 , y_position = -19,  show.legend = FALSE) +
  labs(title = "T-Test Comparison of Roof Stack Exhaust Air Samples by Occupancy", x = '', y = 'Ct Value') +
  theme(legend.position = "NONE")+
  scale_y_continuous(trans = 'reverse') +
  theme_minimal() +
  scale_fill_manual(values = c( "#FFCC66","lightblue")) +
  theme(text = element_text(size = 13))



####OCCUPIED PERCPOSITIVE###################
#No significance, not including in manuscript#


roof$resultbinary <- ifelse(roof$Result.clean == 'Positive', 1,0)

roofgroup <- roof %>%
  drop_na(Result.clean) %>%
  group_by(Day, Room.Type, CollectionMethod, occupied) %>%
  summarize(percpos=mean(NA.rm = TRUE, ifelse(Result.clean =='Positive', 100, 0))) 


roofgroup %>% 
  drop_na(occupied) %>%
  filter(Room.Type == 'Roof') %>%
  ggplot(aes(x = occupied, y = percpos, )) +
  geom_violin(aes(fill = occupied), show.legend = FALSE) +
  geom_jitter() +
  geom_boxplot(color = 'darkgrey', width = .2, outlier.shape = NA, aes(alpha = 0.5), show.legend = FALSE) +
  geom_signif(comparisons = list(c('Unoccupied', 'Occupied')),
              map_signif_level = FALSE, 
              test = t.test, test.args=list(alternative = NULL, var.equal = FALSE, paired=FALSE), textsize = 7 ,  show.legend = FALSE) +
  labs(title = "T-Test Comparison of Roof Stack Exhaust Air Samples by Occupancy", x = '', y = 'Daily Percent of Positive Tests') +
  theme(legend.position = "NONE")+
  theme_minimal() +
  scale_fill_manual(values = c("#FFCC66","lightblue")) +
  theme(text = element_text(size = 13))




#########VS T.test / violin plot##########################
violin = data.frame()
violin <- allsamples


violin$Room.Type[violin$Room.Type == 'Indoors'] <- 'High Touch Surfaces'
violin$Room.Type[violin$Room.Type == 'Port'] <- 'Exhaust Duct Surfaces'
violin$Room.Type[violin$Room.Type == 'Roof'] <- 'Exhaust Aerosol'

fig4 <- ggplot(violin, aes(x = Room.Type, y = ct, group = Room.Type)) +
  geom_violin(aes(fill = Room.Type), alpha = 0.8, show.legend = FALSE) +
  geom_boxplot(width = 0.15, color = 'darkgrey', outlier.shape = NA) +
  geom_signif(comparisons = list(c('High Touch Surfaces', 'Exhaust Aerosol')),map_signif_level = TRUE, test = t.test, show.legend = FALSE, color = 'red', y_position = -16, textsize = 7) +
  geom_signif(comparisons = list(c('Exhaust Aerosol', 'Exhaust Duct Surfaces')),map_signif_level = TRUE, test = t.test, show.legend = FALSE, color = 4, textsize = 7) +
  geom_signif(comparisons = list(c('Exhaust Duct Surfaces', 'High Touch Surfaces')),map_signif_level = TRUE, test = t.test, show.legend = FALSE, color = "darkgreen", textsize = 7) +
  scale_y_continuous(trans = 'reverse', breaks = c(40, 35, 30, 25, 20), limits = c(40, 14)) +
  labs(title ="Welch's T-Test Sampling Method Comparison", y = 'Ct Value', x = '', y_position = 20) +
  scale_color_manual(values = c("#00BFC4", "#00BE6C", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#00BE6C", "#F8766D")) +
  theme_minimal() +
  theme(text = element_text(size = 13))

fig4

#calculate mean for fig4
aggregate(violin$ct, list(violin$Room.Type), FUN = mean)


#T.Test results from fig4
# air to duct 0.0015
# air to surface 0.041
# duct to surface 6.8e-06



##########STACK POINT PLOT########
stackgroup <- ddply(roof, .(stack.n, Location, CollectionMethod, Room.Type), summarise, ctavg = mean(ct))

ggplot(stackgroup, aes(x = stack.n, y = ctavg, color = Room.Type)) +
  geom_point(alpha = .5,) +
  geom_smooth(se=TRUE, method = , show.legend = ) +
  labs(title = 'Linear Regression Model - Averaged', subtitle = 'Roof P = 0.1555', x = "Stack Census", y = "Ct Average", caption = "Occupants / datapoints
                  0 = 100
                  1 = 56  
                  2 = 69  
                  3 = 11  
                  4 = 4    
                  5 = 3    ") +
  scale_y_continuous(trans = "reverse")

results <- lm(ctavg ~ stack.n, data = stackgroup[which (stackgroup$Room.Type== 'Roof'),])
summary(results)



#####WING POINT PLOT####
winggroup <- ddply(roof, .(wing.n, Location, CollectionMethod, Room.Type), summarise, ctavg = mean(ct))

ggplot(winggroup, aes(x = wing.n, y =ctavg, color = Room.Type)) +
    geom_point(alpha = .5) +
    geom_smooth(se=TRUE, method = ) +
    labs(title = "North Wing Linear Regression - Averaged", subtitle = ) +
  scale_y_continuous(trans = "reverse") +
  theme(axis.title = element_text(face = 'bold')) +
  labs(x = 'North Wing census', y = 'Ct Average') +
  scale_x_continuous(breaks = c(1,5,10,15))
  


###AVERAGED CT VALUE LINEAR REGRESSION WINGROUP P-VALUES####
#ROOF = <2e-16
#
#

results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Roof'),])
summary(results)
results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Port'),])
summary(results)
results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Indoor'),])
summary(results)




#BUILDING POINT PLOT####
buildinggroup <- ddply(roof, .(building.n, Location, CollectionMethod, Room.Type), summarise, ctavg = mean(ct))

ggplot(buildinggroup, aes(x = building.n, y = ctavg, color = Room.Type)) +
  geom_point(alpha = .5) +
  geom_smooth(se=TRUE, method = ) +
  labs(title = "Building Linear Model", subtitle = 'No significant P Aside from anomalous port', x = "Building Census", y = "Ct") +
  scale_x_continuous(breaks = function(x) 
                          seq(from = min(0), 
                          to = max(x), 
                          by = 5,)) +
  scale_y_continuous(trans = "reverse")

#LM Tests
results <- lm(ctavg ~ building.n, data = buildinggroup[which (buildinggroup$Room.Type== 'Indoors'),])
summary(results)

results <- lm(ctavg ~ building.n, data = buildinggroup[which (buildinggroup$Room.Type== 'Port'),])
summary(results)

results <- lm(ctavg ~ building.n, data = buildinggroup[which (buildinggroup$Room.Type== 'Roof'),])
summary(results) 

#STACK Entry/Positive####

stackentry <- roof %>%
  filter(Room.Type != 'Indoors' ) %>%
  ggplot( aes(x = CollectionDate, y = stack.n, alpha = 0.5)) +
  geom_point(aes(color = Result.clean), alpha = .7, size = 2,) +
  facet_wrap(~Location) +
  labs(x = "Entry #", y = "Stack Census", legend = 'test', color = 'Result') +
  scale_color_manual(values = c('cadetblue', "orangered")) +
  scale_x_date(date_breaks = '7 days', date_labels = "%m/%e", date_minor_breaks = '1 day')

stackentry

#PERCENT POS/DATE - PLOT######
#group by date and calculate percent positive for each day

winggroup <- allsamples %>% 
  filter(Room.Type != 'Shop') %>%
  drop_na(Result.clean) %>%
  group_by(CollectionDate, Room.Type, CollectionMethod, wing.n) %>%
  summarize(percpos=mean(NA.rm = TRUE, ifelse(Result.clean =='Positive', 1, 0)))



#PLOT % POSITIVE BY OCCUPANCY - PLOTY#####
#wing occ
ggplot(winggroup, aes( x = wing.n, y = percpos, group = Room.Type, color = Room.Type)) +
  geom_point(alpha = .5) +
  geom_smooth(se=TRUE, method = ) +
  scale_x_continuous(breaks = function(x) seq(from = min(0),
                                              max(x),
                                              by = 2), labels = label_number(accuracy = 1))




#building occ##
buildinggroup <- allsamples %>%
  filter(Room.Type != 'Shop') %>%
  drop_na(Result.clean) %>%
  group_by(CollectionDate, Room.Type, CollectionMethod, building.n) %>%
  summarize(percpos=mean(NA.rm = TRUE, ifelse(Result.clean =='Positive',1, 0)))

ggplot(buildinggroup, aes( x= building.n, y = percpos, group = Room.Type, color = Room.Type)) +
  geom_point(alpha = .5) +
  geom_smooth(se=TRUE, method = ) +
  labs(title = "Percent Positive by Building Census", x = "Building Census", y = "Percent of Positive Tests") +
  scale_x_continuous(breaks = function(x) seq(from = min(0),
                                              max(x),
                                              by = 5), labels = label_number(accuracy = 1))




####################################################################################
#####   TIMELINE PERCENT POSITIVE    ##################################################
####################################################################################

supptimeline <- allsamples %>%
  drop_na(Result.clean) %>%
  group_by(Day, Room.Type, CollectionMethod) %>%
  summarize(percpos=mean(NA.rm = TRUE, ifelse(Result.clean =='Positive', 100, 0))) 

supptimeline$Room.Type[supptimeline$Room.Type == 'Indoors'] <- 'High Touch Surfaces'
supptimeline$Room.Type[supptimeline$Room.Type == 'Port'] <- 'Exhaust Duct Surface'
supptimeline$Room.Type[supptimeline$Room.Type == 'Roof'] <- 'Exhaust Aerosol'

supptimeline <- supptimeline %>% 
  ggplot(aes(x = Day, y = percpos, group = Room.Type, color = Room.Type)) +
  geom_point(size = 1.5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, size = 1.5, show.legend = NA) +
  labs(x = 'Study Day', y = "Percent of Positive Tests", color = NULL, ) +
  scale_y_continuous(limits = c(-3,103), minor_breaks = seq(0,100, 5)) +
  scale_x_continuous(breaks = seq(0,29, 7), minor_breaks = seq(0,29,1)) +
  theme_minimal()+
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) +
  theme(plot.subtitle=element_text(size=9)) +
  scale_color_manual(values = c("#00BFC4", "#00BE6C", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#00BE6C", "#F8766D")) +
  theme(text = element_text(size = 13))

supptimeline




##WASTEWATER########## READ IN AND PLOT NORMAL AND TRANSPARENT VERSIONS


#NORMAL - Read in OSU Data, change Date to date data type, plot
wwin <- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Wastewater\\UOR_aggregated_data.csv")

wwin <- wwin %>%
  mutate(Date=mdy(Date))


##############WASTEWATER LINEAR REGRESSION#################
ggplot(wwin, aes(x = wing.n, y = LogCopiesPerL)) +
  geom_point(color = 'brown', alpha = 0.7, size = 1.7) +
  geom_smooth(se=TRUE, method = , color = 'blue') +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  labs(title = 'Wastewater Linear Regression', subtitle = 'Wing Census P = 0.009021
Building Census P = 0.01765', x = 'North Wing Census', y = 'Log Copies/Liter',)

results <- lm(LogCopiesPerL ~ wing.n, data = wwin)
summary(results)



####WASTERWATER TIMELINE################
fig3.3 <- ggplot(wwin, aes(x = Day, y = LogCopiesPerL, group = SampleType)) +
  geom_point(aes(color = 'darkblue'), alpha = .5, size = 1.5) +
  geom_smooth(aes(color = 'darkblue'), se=TRUE, method = "loess", span = 0.9, size = 1.5) +
  labs(title = '', subtitle = '', size = 30, x = 'Study Day', y = 'Log10 GC/L', tag = "C") +
  scale_x_continuous(breaks = seq(43,71, 7), minor_breaks = seq(43,71, 1)) +
  theme_minimal() +
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = c('darkblue'), labels = 'Wastewater') +
  theme(plot.subtitle=element_text(size=10))

  
  
fig3.3





#NEW GRAPHS#
###################################################################################################################
###################################################################################################################
###################################################################################################################

wingn<- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\R_Data\\mms north wing census2.csv", stringsAsFactors = FALSE)

wingn <- wingn %>%
  mutate(?..date=mdy(?..date))



wingn$type[wingn$type == 'traced'] <- 'Contact Traced'
wingn$type[wingn$type == 'presumed'] <- 'Likely Positive'
wingn$type[wingn$type == 'positive'] <- 'Confirmed Positive'

wingn$type <- factor(wingn$type, levels = c('Contact Traced', 'Likely Positive', 'Confirmed Positive'))


fig3.1 <- ggplot(wingn, aes(fill = type, x = Day, y = count)) +
  geom_bar(position = position_stack(reverse = FALSE), stat = 'identity', alpha = .7) +
  labs(subtitle = '', y ='North Wing Census', x = 'Study Day', color = NULL, tag = "A") +
  scale_fill_manual(values = c('goldenrod1', 'darkorange1', 'red4'),) +
  scale_x_continuous(breaks = seq(43,71, 7), minor_breaks = seq(43,71, 1)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(plot.subtitle=element_text(size=10)) +
  theme(text = element_text(size = 13)) +
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1))
  
  

fig3.1

plot_grid(supptimeline, fig3.1, ncol = 1, align = 'v')



####   TIMELINE CT      ############################################################################################################

cttimeline <- allsamples %>%
  filter(Room.Type != 'Shop') %>%
  filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp') %>%
  drop_na(ct) 
 

cttimeline <- ddply(cttimeline, .(CollectionDate, Room.Type, CollectionMethod, wing.n, Day), summarise, ctavg = mean(ct))
  
cor.test(cttimeline$ct[which(cttimeline$Room.Type == 'Indoors')],
         cttimeline$wing.n[which(cttimeline$Room.Type == 'Indoors')])


roof <- allsamples[!grepl('Shop', allsamples$Room.Type), ]

#PEARSONS VALUES NORTH WING AND LOBBY SURFACE AVERAGED
#Roof - Wing# -0.2020 stack# -0.176
#WW - Wing# 0.521 same as below because can't average 1 daily value
#Port - wing# 0.332
#Surface - wing# -0.252

cor.test(allsamples$ct[which(allsamples$Room.Type == 'Port')],
         allsamples$wing.n[which(allsamples$Room.Type == 'Port')])

#Roof - Wing# -0.067   Stack# -0.176
#PEARSONS VALUES NOT AVERAGED
#WW - Wing# 0.521
#Port - wing# 0.274
#Surface - wing# -0.221

## CT LINE #####
cttimeplot <- cttimeline %>% filter(Room.Type != 'Shop') %>%
  filter(Room.Type != '') %>%
  ggplot(aes(x = CollectionDate, y = ct, group = Room.Type, color = Room.Type)) +
  geom_smooth() +
  geom_point() +
  labs(title = "Multimodal Timeline - Indoor Surfaces vs. Roof Exhaust", x = '', y = "Average Ct", color = NULL, ) +
  scale_x_date(date_breaks = '7 days', date_labels = "%m/%e", date_minor_breaks = '1 day') +
  scale_y_continuous(trans = 'reverse')


cttimeplot


##CT SMOOTH ####
#MY TAKEAWAY FROM THESE GRAPHS IS THAT CT IS SIMILARLY TRACKS FROM EACH OF THE METHODS OVER THE TIMELINE. IS THIS RIGHT? THERE IS A RELATIONSHIP HERE, JUST PERHAPS NOT A LINEAR ONE?
#QUESTION: LINEAR REGRESSIONS LOOK AT FUNCTION OF Y TO X RIGHT? IF SO THEN THIS METHOD OF ANALYSIS IS ASKING IF CHANGE IN DATE(X) WILL IMPACT LOG/L OR CT, BUT IT SHOULD BE CENSUS NOT DATE.

cttimeline$Room.Type[cttimeline$Room.Type == 'Indoors'] <- 'High Touch Surfaces'
cttimeline$Room.Type[cttimeline$Room.Type == 'Port'] <- 'Exhaust Duct Surface'
cttimeline$Room.Type[cttimeline$Room.Type == 'Roof'] <- 'Exhaust Aerosol'

fig3.2 <- cttimeline %>% filter(Room.Type != 'Shop') %>%
  filter(Room.Type != '') %>%
  ggplot(aes(x = Day, y = ctavg, group = Room.Type, color = Room.Type)) +
  geom_point(size = 1.5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, size = 1.5, show.legend = NA) +
  labs(title = "", x = 'Study Day', y = "Average Ct", color = NULL,  tag = "B") +
  scale_y_continuous(trans = 'reverse', limits = c(40, 25), breaks = c(40, 35, 30, 25), minor_breaks = seq(25,40, 1) ) +
  scale_x_continuous(breaks = seq(43,71, 7), minor_breaks = seq(43,71, 1)) +
  theme_minimal()+
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) +
  theme(plot.subtitle=element_text(size=9)) +
  scale_color_manual(values = c("#00BFC4", "#00BE6C", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#00BE6C", "#F8766D")) +
  theme(text = element_text(size = 13)) +
  theme(axis.text.x = )



fig3.2



####NO AVG CT TIMELINE###########################################

mainplot = data.frame()
mainplot <- allsamples

mainplot$Room.Type[mainplot$Room.Type == 'Indoors'] <- 'High Touch Surfaces'
mainplot$Room.Type[mainplot$Room.Type == 'Port'] <- 'Exhaust Duct Surface'
mainplot$Room.Type[mainplot$Room.Type == 'Roof'] <- 'Exhaust Aerosol'


NOAVGfig3.2 <- mainplot %>%
  filter(Room.Type != 'Shop') %>%
  ggplot(aes( x = Day, y = ct, group = Room.Type, color = Room.Type)) +
  geom_jitter(alpha = .5, size = 1.5, width = 0.17, aes(fill = Room.Type, color = Room.Type)) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, size = 1.5, show.legend = NA) +
  labs(title = "", subtitle = '', y = 'Ct Value', x = 'Study Daye') +
  scale_y_continuous(trans = 'reverse', limits = c(40, 25), breaks = c(40, 35, 30, 25), minor_breaks = seq(25,40, 1) ) +
  scale_x_continuous(breaks = seq(0,29, 7), minor_breaks = seq(0,29, 1)) +
  theme_minimal()+
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) + 
  theme(plot.subtitle=element_text(size=9)) +
  scale_color_manual(values = c("#00BFC4", "#00BE6C", "#F8766D" )) +
  scale_fill_manual(values = c("#00BFC4", "#00BE6C", "#F8766D")) +
  theme(text = element_text(size = 10, face = 'bold')) 
    

NOAVGfig3.2

plot_grid(fig3.1, fig3.2, fig3.3, ncol = 1, align = 'v')


#what is this next line?

mainplot %>% mutate(CollectionDate=as.numeric(CollectionDate))
  

results <- lm(ct ~ wing.n, data = allsamples[which (allsamples$Room.Type== 'Roof'),])
summary(results)

results <- lm(ct ~ building.n, data = allsamples[which (allsamples$Room.Type== 'Port'),])

results <- lm(ct ~ stack.n, data = allsamples[which (allsamples$Room.Type== 'Roof'),])
summary(results) 

#LM P-value Notes
# Roof ~ stack  =  0.01166
# Indoors ~ wing = 0.00615
# Port ~ wing & building = 0.0001345 & 7.09e-12 - WRONG DIRECTION - we were probably 'cleaning' the port


##PEARSONS R, NEED TO CLEAN INDOORS SURFACE SAMPLES AND RUN, EXAMINE MORE THOROUGHLY IN GENERAL

cor.test(wwin$LogCopiesPerL, wwin$wing.n)


cor.test(allsamples$ct[which(allsamples$Room.Type == 'Indoors')],
         allsamples$wing.n[which(allsamples$Room.Type == 'Indoors')])


#PEARSONS VALUES NOT AVERAGED, LOBBY SURFACES + 2nd floor
#Roof - Wing# -0.067   Stack# -0.176
#WW - Wing# 0.521
#Port - wing# 0.274
#Surface - wing# -0.221


###########################################KEVIN REQUESTS################################################################################################################################################
############################################################################################################################################################################

#READ ALLSAMPLES BACK In

allsamples2 <- read.csv("C:\\Users\\ano\\Dropbox (University of Oregon)\\Projects\\Multimodal_Surveillance\\Data\\MultiModalSurveillance_Results.csv", stringsAsFactors = FALSE)

####CLEANING DATA#####################
#set blanks to NA
allsamples2[allsamples2=='']=NA

#set CollectionDate as  date format
allsamples2 <- allsamples2 %>%
  mutate(CollectionDate=mdy(CollectionDate)) %>%
  mutate(stack.n = as.numeric(stack.n))

allsamples2 <- allsamples2 %>%  
  mutate(Day = as.numeric(Day))

allsamples2$Result.clean= "Negative"
allsamples2$Result.clean[allsamples2$Result=="Positive (2/3)"] <- "Positive"
allsamples2$Result.clean[allsamples2$Result=="Positive (3/3)"] <- "Positive"
allsamples2 <- allsamples2 %>% filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp')

allsamples2$floor[pvt$floor == 'lobby'] <- 'Lobby'
allsamples2$floor[pvt$floor == 'positive'] <- 'Positive Wing'
allsamples2$floor[pvt$floor == 'traced'] <- 'Contact Traced Wing'


pvt <- allsamples2

pvt %>% 
  filter(Room.Type == 'Indoors')%>%
  filter(Location != 'Positive Floor Elevator Common Buttons') %>%
  filter(Location != '2nd Floor Stairway Door Handle') %>%
  #filter(Location != 'Ground Floor Stairway Door Handle') %>%
  #filter(Location != 'W. Entrance Door Handle Exterior') %>%
  #filter(Location != 'W. Entrance Door Handle Interior') %>%
  #filter(Location != 'Lobby Call Elevator Button') %>%
  ggplot(aes(x = floor, y = ct, )) +
  geom_violin(aes(fill = floor, alpha = 0.5), show.legend = FALSE) +
  geom_boxplot(color = 'darkgrey', width = .2, outlier.shape = NA, aes(alpha = 0.5), show.legend = FALSE) +
  geom_signif(comparisons = list(c("Positive Wing", "Contact Traced Wing")),
              map_signif_level = TRUE, 
              test = t.test, test.args=list(alternative = NULL, var.equal = FALSE, paired=FALSE), textsize = 6, y_position = -19,  show.legend = FALSE) +
  geom_signif(comparisons = list(c("Positive Wing", "Lobby")),
              map_signif_level = TRUE, 
              test = t.test, test.args=list(alternative = NULL, var.equal = FALSE, paired=FALSE),textsize = 6,show.legend = FALSE) +
  geom_signif(comparisons = list(c("Lobby", "Contact Traced Wing")),
              map_signif_level = TRUE, 
              test = t.test, test.args=list(alternative = NULL, var.equal = FALSE, paired=FALSE), show.legend = FALSE) +
  labs(title = "T-Test Comparison of Surface Swabs by Building Location", x = '', y = 'Ct Value') +
  theme(legend.position = "NONE")+
  scale_y_continuous(trans = 'reverse') +
  theme_minimal() +
  scale_fill_manual(values = c("goldenrod1", "grey", 'red4')) +
  theme(text = element_text(size =13))




##################################################################################################
#AGGREGATE ROOF CT CENSUS CORRELATION ANALYSIS
#JUNE 3rd 2023

roof_additive <- ddply(roof, .(wing.n, Room.Type, CollectionMethod), summarise, ctsum = -1*(sum(ct)))


ggplot(roof_additive, aes(x = wing.n, y = ctsum, color = Room.Type)) +
  geom_point(color = 'brown', alpha = 0.7, size = 1.7) +
  geom_smooth(se=TRUE, method =) +
  scale_x_continuous(breaks = c(1:20)) 
# +
#   labs(title = 'Wastewater Linear Regression', subtitle = 'Wing Census P = 0.009021
# Building Census P = 0.01765', x = 'North Wing Census', y = 'Log Copies/Liter',)


roof_split <- roof_additive %>% filter(Room.Type == "Roof")
results <- lm(ctsum ~ wing.n, data = roof_split)
summary(results)


roof_split <- roof_additive %>% filter(Room.Type == "Port")
results <- lm(ctsum ~ wing.n, data = roof_split)
summary(results)


roof_split <- roof_additive %>% filter(Room.Type == "Indoors")
results <- lm(ctsum ~ wing.n, data = roof_split)
summary(results)

#Additive Wing Pvalues
#Roof Pvalue = 0.00254
#Port Pvalue = 0.00796
#Indoor pvalu = .0053


#Averaged wing Pvalues
#roof p <2e-16
# port p < 2e-16
# indoor p <2e-16 ***


results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Roof'),])
summary(results)
results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Port'),])
summary(results)
results <- lm(ctavg ~ wing.n, data = winggroup[which (winggroup$Room.Type== 'Indoors'),])
summary(results)

