library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(ggsignif)
library(grid)
library(gridExtra)
library(patchwork)
library(cowplot)
library(lubridate)
library(polycor)
require(plyr)
require(dplyr)
library(ggpubr)
library("stringr")
library(multcomp)


setwd("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\R_Data")

bhsamples <- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\R_Data\\MMS_BHdata_lobby_roof_hallways_R.csv")

#MAKE DATA EASY TO WORK WITH#####
bhsamples[bhsamples == ' ']=NA

bhsamples <- bhsamples %>%
  drop_na(Ct) %>%
  mutate(Date=mdy(Date)) %>%
  filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp') %>%
  filter(Location != 'Negative Swab') %>%
  filter(Date != '2021-01-24')


bhsamples$Result[(bhsamples$Result == 'Positive (2/3)') | (bhsamples$Result == 'Positive (3/3)')] <- 'Positive'
bhsamples$Date <- as.Date(bhsamples$Date, format = "%m/%d/%y")

############CT/DATE ALL########

bhsamples %>% 
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  filter(Date != '2021-01-24') %>%
  ggplot(aes(x = Date, y = Ct, group = Room.., color = Room..)) +
  geom_jitter(alpha = .5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, show.legend = NA) +
  labs(title = 'Timeline of DOE3 Samples By Ct',) + 
  scale_y_continuous(trans = 'reverse', limits = c(40,25)) +
  scale_x_date(date_breaks = '7 days', date_labels = "%m/%e", date_minor_breaks = '1 day')


############GETTING TIME ELAPSED BETWEEN SAMPLER ON OFF#############
bhsamples$runtime <- difftime(bhsamples$SamplerStart, bhsamples$SamplerEnd, units = "secs")



#PEARSONS - NO AVERAGE 
#-----------------------------
corcleaned <- bhsamples %>% 
  filter(CollectionMethod != 'PUF') %>%
  filter(CollectionMethod != 'Settling Plate')
  
cor.test(corcleaned$Ct[which(corcleaned$Room.. == 'Roof')],
         corcleaned$buildingcensus[which(corcleaned$Room.. == 'Roof')])

# UN-AVERAGED CT vs. BUILDING TOTAL CENSUS
# LOBBY Air only  R = --0.09785789 , plates only R = 0.1891049 ,  both air & plates R = -0.1087252
# POS FLOoR       R = -0.4449465
# ROOF AIR  only  R = 0.4857858     w/ port swabs -0.447086   , port only R = -0.3412696
# TRACED FLOOR    R = - 0.2847877

# UN-AVERAGED CT vs. POSITIVE ONLY CENSUS
# LOBBY         R = -0.08523252  w/ plates R = -0.1019951 plates only R = 0-1936317
# POS FLOOR     R = -0.2134458
# ROOF AIR      R = -0.4436083
# TRACED FLOOR  R = -0.1102002


#PEARSONS - AVERAGED
#----------------------------
fig1 <- ddply(bhsamples, .(Date, Day, Room.., CollectionMethod), summarise, ctavg = mean(Ct))

corfig1 <-  fig1 %>%
  filter(CollectionMethod == 'PUF') %>%
  filter(CollectionMethod != 'Settling Plate')


cor.test(corfig1$ctavg[which(corfig1$Room.. == 'Lobby')],
         corfig1$buildingcensus[which(corfig1$Room.. == 'Lobby')])


# AVERAGED CT vs. BUILDING N ******THIS IS PROPER METHOD*******
# LOBBY air only   R = -0.2227865 , plates only R = -0.5267562 , both air & plates R = -0.2329902 
# POS FLOoR        R = -0.4197613 
# ROOF AIR         R = -0.669434 , port only R = -0.3574467 
# TRACED FLOOR     R = -0.1247648

#AVERAGED CT vs. POSITIVE CENSUS ONLY
# LOBBY  air   R = -0.2154304 
# POS FLOOR    R = -0.166463 
# ROOF AIR     R = -0.6455641 , port swabs R = -0.4025297 , both R = -0.5247517 
# TRACED FLOOR R = -0.007230145 


################EXPERIMENT 2 FIGURE 1 - LOBBY Grouped by 4 hr for pos & traced#########################
fig1all = bhsamples

fig1all$Room..[fig1all$Room.. == '2 Hall North'] <- 'Positve Student Floor Hallway'
fig1all$Room..[fig1all$Room.. == '7 Hall North'] <- 'Traced Student Floor Hallway'
fig1all$Room..[fig1all$Room.. == 'Lobby'] <- 'Lobby Active Air Sampler'
fig1all$Room..[fig1all$Room.. == 'Roof'] <- 'Roof Air Stack'

fig1all <- fig1all %>%
  mutate(blocksegment = case_when(str_detect(segment, 'posrec|between') ~ 'Positive Rec 0900-1300',
                                  str_detect(segment, 'tracerec|aftertrac') ~ 'Traced Rec 1300-1700',
                                  str_detect(segment, 'overnight') ~ 'Unoccupied 1700-0900'
  ))

plotfig1all <- fig1all %>% 
  filter(Date != '2021-01-24') %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  filter(Room.. == 'Lobby Active Air Sampler') %>%
  ggplot(aes(x = Day, y = Ct, group = Room.., color = blocksegment)) +
  geom_point() +
  geom_smooth(se = TRUE, method = 'loess', span = 0.9, show.legend = NA, color = 'seagreen') +
  scale_y_continuous(trans = 'reverse', limits = c(40,30), breaks = seq(30, 40, 2)) +
  scale_x_continuous(breaks = seq(0, 44, 7), minor_breaks = seq(0,44, 1)) +
  labs(title = "", tag = 'A', y = 'Ct Value', x = 'Study Day') +
  theme_minimal(0) +
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) + 
  theme(plot.subtitle=element_text(size=9)) +
  theme(axis.text.x = ) +
  theme(text = element_text(size=13),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title=element_blank(),
      plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"))+    
  theme(plot.tag = element_text(face = "bold", size = 13))


plotfig1all

##############LOBBY GROUPED######################################################################################







fig1$Room..[fig1$Room.. == '2 Hall North'] <- 'Positve Student Floor Hallway'
fig1$Room..[fig1$Room.. == '7 Hall North'] <- 'Traced Student Floor Hallway'
fig1$Room..[fig1$Room.. == 'Lobby'] <- 'Lobby Active Air Sampler'
fig1$Room..[fig1$Room.. == 'Roof'] <- 'Roof Air Stack'


fig1.0 <- fig1 %>% 
  filter(Date != '2021-01-24') %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  filter(Room.. == 'Lobby Active Air Sampler') %>%
  ggplot(aes(x = Day, y = ctavg, group = Room.., color = Room..)) +
  geom_point() +
  geom_smooth(se = TRUE, method = 'loess', span = 0.9, show.legend = NA) +
  scale_y_continuous(trans = 'reverse', limits = c(40,30), breaks = seq(30, 40, 2)) +
  scale_x_continuous(breaks = seq(0, 44, 7), minor_breaks = seq(0,44, 1)) +
  labs(title = "", tag = 'A', y = 'Ct Value', x = 'Study Day') +
  theme_minimal(0) +
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) + 
  theme(axis.text.x = ) +
  scale_color_manual(values = c('seagreen',  "red4","#00BFC4", "goldenrod1" )) +
  scale_fill_manual(values = c("seagreen", "red4", "#00BFC4", "goldenrod1")) +
  theme(text = element_text(size=13),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title=element_blank(),
      plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"))+    
  theme(plot.tag = element_text(face = "bold", size = 13))
   
        
fig1.0


###### SUPPLEMENTAL - Fig 1 but with all locations
supfig1 <- fig1 %>% 
  filter(Date != '2021-01-24') %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  #filter(Room.. == 'Lobby Active Air Sampler') %>%
  ggplot(aes(x = Day, y = ctavg, group = Room.., color = Room..)) +
  geom_point() +
  geom_smooth(se = TRUE, method = 'loess', span = 0.9, show.legend = NA) +
  scale_y_continuous(trans = 'reverse', limits = c(41,25), breaks = seq(25, 40, 5), minor_breaks = seq(25,40,1)) +
  scale_x_continuous(breaks = seq(0, 44, 7), minor_breaks = seq(0,44, 1)) +
  labs(title = "", subtitle = '', y = 'Ct Value', x = 'Study Day') +
  theme_minimal(0) +
  theme(legend.title=element_blank(), panel.grid.major.y  = element_line(size = 1)) + 
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) + 
  theme(plot.subtitle=element_text(size=9)) +
  theme(axis.text.x = ) +
  scale_color_manual(values = c('seagreen',  "red4","#00BFC4", "goldenrod1" )) +
  scale_fill_manual(values = c("seagreen", "red4", "#00BFC4", "goldenrod1")) +
  theme(text = element_text(size = 13))


supfig1


detach("package:plyr", unload = TRUE)


#####census Plot##### FIX THIS
bhn <- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\R_Data\\MMS_BHdata_lobby_roof_hallways_R_CENSUS.csv")

bhn[bhn == ' ']=NA

bhn <- bhn %>%
  drop_na(Ct) %>%
  mutate(Date=mdy(Date)) %>%
  filter(MS2 != 'No Amp' | S !='No Amp' | ORF1ab !='No Amp') %>%
  filter(Location != 'Negative Swab') %>%
  filter(Date != '2021-01-24')


bhn$Result[(bhn$Result == 'Positive (2/3)') | (bhn$Result == 'Positive (3/3)')] <- 'Positive'
bhn$Date <- as.Date(bhn$Date, format = "%m/%d/%y")

#calculte median, I dont use this for any of the plots
median_calc <- bhn %>% drop_na(Ct) %>%
  group_by(Day) %>% 
  slice(which.max(buildingcensus)) %>%
  filter(Day != '0') %>%
  summary(bhn$buildingcensus)

median_calc


bhn <- bhn %>% drop_na(Ct) %>%
  group_by(Day) %>% 
  slice(which.max(buildingcensus)) %>%
  summarise(count=n(),
            'Confirmed Positive' = mean(isobuild),
            'Contact Traced' = mean(quarbuild)) %>%
  filter(Day != '0') %>%
  gather("key", 'value', -c(Day, count)) %>%
  ggplot(aes(x = Day, y = value, group = key, fill = key)) +
  geom_bar(stat = 'identity', position = position_stack(reverse = TRUE), alpha = .7) +
  labs(tag = 'B', y ='Building Census', x = 'Study Day', color = NULL) +
  scale_fill_manual(values = c('red4', 'goldenrod'),) +
  scale_x_continuous(breaks = seq(0, 44, 7), minor_breaks = seq(0,44, 1))+
  scale_y_continuous(minor_breaks  = seq(0,125,25)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(plot.subtitle=element_text(size=10)) +
  theme(legend.title=element_blank(), panel.grid.major.x  = element_line(size = 1)) +
  theme(text = element_text(size=13),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title=element_blank(),
      plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"))+    
  theme(plot.tag = element_text(face = "bold", size = 13))

  
bhn


plot_grid(fig1.0, bhn, ncol = 1, align = 'v')


plot_grid(plotfig1all, bhn, ncol = 1, align = 'v')


#########VIOLIN############# 31 v 179

fig4 = data.frame()
fig4 <- bhsamples

fig4$Room..[fig4$Room.. == '2 Hall North'] <- 'Positive Floor Hallway'
fig4$Room..[fig4$Room.. == 'Lobby'] <- 'Communal Lobby'


fig4 <- fig4 %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  filter(Room.. != '7 Hall North') %>%
  filter(Room.. != 'Roof') %>%
  ggplot(aes(x = Room.., y = Ct, group = Room..)) +
  geom_violin(aes(fill = Room.., color = Room..), alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.15, color = 'darkgrey') +
  geom_signif(comparisons = list(c('Positive Floor Hallway', 'Communal Lobby')),map_signif_level = TRUE, test = t.test, show.legend = FALSE, y_position = -23, textsize = 7) +
  scale_y_continuous(trans = 'reverse', breaks = c(40, 35, 30, 25), limits = c(40,21)) +
  scale_color_manual(values = c('red4', "seagreen")) +
  scale_fill_manual(values = c('red4', "seagreen")) +
  labs(title ="", y = 'Ct Value', x = '', y_position = 20) +
  theme_minimal() +
  theme(text = element_text(size = 14)) 



fig4



#PLOT JUST HALLWAY CT BY DATE####
ggplot(bhsamples[which(bhsamples$Room..=='2 Hall North'|(bhsamples$Room..=='7 Hall North')),], aes(x = Date, y = Ct, group = Room.., color = Room..)) +
  geom_jitter(alpha = .5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, show.legend = NA) +
  scale_y_continuous(trans = 'reverse', limits = c(41, 25)) +
  labs(title = 'DOE3 Hallways Timeline by Ct')


#####################
  #Census to Ct##
  censusplot <-ggplot(bhsamples, aes( x = buildingcensus , y = Ct, group = Room.., color = Room..)) +
    geom_jitter(alpha = .25, size = 4, aes(fill = Room.., color = Room..)) +
    geom_smooth(se=TRUE, method = lm, span = 0.9, size = 2, show.legend = NA) +
    labs(title = "North Wing Infectivity Timeline", y = 'Ct Value', x = '') +
    scale_y_continuous(trans = 'reverse') +
    theme_minimal()
  
censusplot
  

results <- lm(Ct ~ buildingcensus, data = bhsamples[which (bhsamples$Room..== 'Lobby'),])
summary(results)


##Stack Occupied t.test############

bhsamples$occupied <- ifelse(bhsamples$stacktotal > 0, 'yes', 'no')

stacktest <- bhsamples %>%
    filter(Room.. == 'Roof') %>%
    filter(CollectionMethod == 'PUF') %>%
    ggplot(aes(x = occupied, y = Ct,)) +
    geom_boxplot(aes(fill = stacktotal), outlier.shape = NA) +
    geom_signif(comparisons = list(c("no", "yes")),
                map_signif_level = FALSE,
               test = t.test, test.args = list(alternative = 'less', var.equal = FALSE, paired=FALSE) )
stacktest

####PERCENT POSITIVE GRAPHS############ IF WE"RE GOING TO USE I NEED TO CHECK MATH & METHODS###########
#calculate % positive - CHECK MATH

dategroup <- bhsamples %>%
  group_by(Date, Room.., CollectionMethod) %>%
  summarize(percpos=mean(NA.rm = TRUE, ifelse(Result == 'Positive', 1,0)))


#remove settling plates
airgroup <- dategroup %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(CollectionMethod != 'Swab') %>%
  filter(Date != '2021-01-24')

ggplot(airgroup, aes(x = Date, y = percpos, group = Room.., color = Room..)) +
  geom_point(alpha = .5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, show.legend = NA)
  
#PLOR JUST HALLWAY PERCENT POSITIVE##
halls <- airgroup %>%
  filter(Room.. == '2 Hall North' | Room.. == '7 Hall North')


ggplot(halls, aes(x = Date, y = percpos, group = Room.., color = Room..)) +
  geom_point(alpha = .5) +
  geom_smooth(se=TRUE, method = "loess", span = 0.9, show.legend = NA) +
  labs(title = 'Hallway Air Percent Positive', y = "Percent Positive")





#######LOBBY TIME SEGMENTS##############

timebreaks <- bhsamples

timebreaks$segment[timebreaks$SamplerEnd<timebreaks$SamplerStart] <- 'overnight'


segplot <- timebreaks %>%
  filter(CollectionMethod !='swab') %>%
  filter(CollectionMethod != 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  ggplot(aes(x = Date, y = Ct,  group = segment, color = segment)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  scale_y_continuous(trans = 'reverse')


segplot

timebreaks$segment[timebreaks$segment == 'posrec'] <- 'Positive Rec 0900-1100'
timebreaks$segment[timebreaks$segment == 'between'] <- 'Unoccupied 1100-1300'
timebreaks$segment[timebreaks$segment == 'tracerec'] <- 'Traced Rec 1300-1500'
timebreaks$segment[timebreaks$segment == 'aftertrac'] <- 'Unoccupied 1500-1700'
timebreaks$segment[timebreaks$segment == 'overnight'] <- 'Unoccupied 1700-0900'

timebreaks$segment <- factor(timebreaks$segment, levels = c( 'Positive Rec 0900-1100','Unoccupied 1100-1300', 'Traced Rec 1300-1500','Unoccupied 1500-1700', 'Unoccupied 1700-0900'))


segcount <- timebreaks %>% 
  filter(CollectionMethod!= 'swab') %>%
  filter(CollectionMethod!= 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  filter(Day %in% (2:20)) %>%
  group_by(segment) %>%
  summarise(n=n()) %>%
  as.data.frame()

fig2 <- timebreaks %>%
  filter(CollectionMethod!= 'swab') %>%
  filter(CollectionMethod!= 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  filter(Day %in% (2:20)) %>%
  ggplot(aes(x = segment, y = Ct)) +
  geom_violin(aes(fill = segment, color = segment), alpha = 0.5, show.legend = FALSE) +  
  geom_boxplot(width = 0.15, color = 'darkgrey', outlier.shape = NA) +  
  geom_jitter(size = 2, alpha = 0.3, width = 0.08) +
  geom_signif(comparisons = list(c('Unoccupied 1100-1300', 'Positive Rec 0900-1100')),map_signif_level = TRUE, test = "wilcox.test", test.args = list(alternative = 'less', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position =, color = "red4") +
  geom_signif(comparisons = list(c('Traced Rec 1300-1500', 'Positive Rec 0900-1100')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'less', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -23, color = "red4") +
  geom_signif(comparisons = list(c('Unoccupied 1500-1700', 'Positive Rec 0900-1100')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'less', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -22, color = "red4") +
  geom_signif(comparisons = list(c('Unoccupied 1700-0900', 'Positive Rec 0900-1100')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'less', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -21,color = "red4") +
  geom_signif(comparisons = list(c('Unoccupied 1100-1300', 'Traced Rec 1300-1500')),map_signif_level = TRUE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position =) +
  geom_signif(comparisons = list(c('Traced Rec 1300-1500', 'Unoccupied 1500-1700')),map_signif_level = TRUE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position =) +
  geom_signif(comparisons = list(c('Unoccupied 1500-1700', 'Unoccupied 1700-0900')),map_signif_level = TRUE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position =) +
  scale_y_continuous(trans = 'reverse', breaks = c(40, 35, 30, 25)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_color_manual(values = c('red4',"tan",'goldenrod','khaki3', 'steelblue')) +
  scale_fill_manual(values = c('red4', "tan",'goldenrod','khaki3', 'steelblue')) +
  labs(title ="T-Test Comparison of Lobby Active Air Time Blocks", y = 'Ct Value', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 13))
  


fig2  +
  geom_text(data=segcount,aes(y = 40.35, label=paste("n = ", n)),size = 3.5, color = 'grey30')


#T-test values
#Positive to Between 0.067
#Positive to Contact 0.027
#Positive to Overnight 0.031


#make new column in 4 hour time blocks
timebreaks <- timebreaks %>%
  mutate(blocksegment = case_when(str_detect(segment, 'Positive Rec 0900-1100|Unoccupied 1100-1300') ~ 'Positive Rec 0900-1300',
                                  str_detect(segment, 'Traced Rec 1300-1500|Unoccupied 1500-1700') ~ 'Traced Rec 1300-1500',
                                  str_detect(segment, 'Unoccupied 1700-0900') ~ 'Unoccupied 1700-0900'
  ))

#Count number of samples for each blocked segment so we can show n on graph later
blocksegcount <- timebreaks %>% 
  filter(CollectionMethod!= 'swab') %>%
  filter(CollectionMethod!= 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  filter(Day %in% (2:20)) %>%
  group_by(blocksegment) %>%
  summarise(n=n()) %>%
  as.data.frame()

#SAME plot as above but switched to 4 hr time blocks per kevin request
fig2.1 <- timebreaks %>%
  filter(CollectionMethod!= 'swab') %>%
  filter(CollectionMethod!= 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  filter(Day %in% (2:20)) %>%
  ggplot(aes(x = blocksegment, y = Ct)) +
  geom_violin(aes(fill = blocksegment, color = blocksegment), alpha = 0.5, show.legend = FALSE) +  
  geom_boxplot(width = 0.15, color = 'darkgrey', outlier.shape = NA) +  
  geom_jitter(size = 2, alpha = 0.3, width = 0.08) +
  geom_signif(comparisons = list(c('Positive Rec 0900-1300', 'Traced Rec 1300-1500')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -23, color = "red4", annotations = c("< 2e-16")) +
  geom_signif(comparisons = list(c('Traced Rec 1300-1500', 'Unoccupied 1700-0900')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -23, annotations = c("0.15")) +
  geom_signif(comparisons = list(c('Positive Rec 0900-1300', 'Unoccupied 1700-0900')),map_signif_level = FALSE, test = "wilcox.test", test.args = list(alternative = 'greater', var.equal = FALSE, paired=FALSE, show.legend = FALSE), y_position = -22, color = "red4", annotations = c("0.0314")) +
  scale_y_continuous(trans = 'reverse', breaks = c(40, 35, 30, 25)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_color_manual(values = c('red4','goldenrod', 'steelblue')) +
  scale_fill_manual(values = c('red4', 'goldenrod', 'steelblue')) +
  labs(title ="ANOVA Comparison of Lobby Active Air Time Blocks", y = 'Ct Value', x = '') +
  theme_minimal() +
  theme(text = element_text(size = 13))

fig2.1 + geom_text(data=blocksegcount,aes(y = 40.35, label=paste("n = ", n)),size = 3.5, color = 'grey30')


#filter data to check for normal dist
aovfilt <- timebreaks %>%
  filter(CollectionMethod!= 'swab') %>%
  filter(CollectionMethod!= 'Settling Plate') %>%
  filter(Room.. == 'Lobby') %>%
  filter(Day %in% (2:20))

#check if data is normally distributed. Not normally distributed but that is to be expected
ggqqplot(aovfilt$Ct)

#log2 transform Cts and run anova
aovfilt = aovfilt %>% mutate(log2ct = log2(Ct))

aovtest <- aov(log2ct ~ blocksegment, data = aovfilt)
summary.lm(aovtest)

# positive vs. traced <2e-16
# positive vs. unoccupied 0.0314
# traced vs. unnocupied 0.15



#Create model to do multiple pairwaise comparisons... DRE STUCK ON THIS
segmod = aovfilt %>% mutate(groupalt = factor(segment))

contrasts(segmod$groupalt)

c_mat = matrix(data = c(-1, 1,0,-1,0,1,0,1,-1), nrow = 5, ncol = 10)

rownames(c_mat) = c("A","B","C", "D", "E")
colnames(c_mat) = c("AvsB", "AvsC", "AvsD", "AvsE", "BvsC", "BvsD","BvsE","CvsD","CvsE","DvsE")

contrasts(segmod$groupalt) = c_mat[,1:7]

contrasts(segmod$groupalt)

#create second anova for contrasts analysis
anova_mod2 = aov(log2ct ~ groupalt, data = segmod)
summary.lm(anova_mod2)



contrasts(penguins$groupalt) = c_mat[,2:3]
anova_mod2 = aov(log2ct ~ groupalt, data = segmod)
summary.lm(anova_mod2)

