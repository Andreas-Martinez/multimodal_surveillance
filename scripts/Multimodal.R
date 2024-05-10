library(tidyverse)
library(ggsignif)
library(lubridate)
library(Hmisc)
library(reshape)
library(ggpmisc)
library(ggpubr)
library(readbitmap)
library(lmerTest)
library(report)
library(effsize)
library(gmm)
library(propagate)
library(errors)
library(scales)


load("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\hooman_script\\Oct52022")

# Creating two columns for start and end date 
# Start date
allsamples$Start = str_c(allsamples$Start.Date, " ", allsamples$SamplerStart)
allsamples$Start_Date_final = str_c(allsamples$Start_Date, " ",hour(allsamples$Start_hour), ":", minute(allsamples$Start_hour))
allsamples$Start_Date_final = parse_date_time(allsamples$Start_Date_final, orders = "y-m-d H:M", tz = "America/Los_Angeles")
allsamples = allsamples %>%
 relocate(Start_Date_final, .after = Start.Date)
allsamples = allsamples %>%
  relocate(Start.Date, .after = floor)
allsamples = allsamples %>%
  relocate(SamplerStart, .after = floor)

# End date
allsamples$End = parse_date_time(allsamples$CollectionDate, orders = "m/d/y")
allsamples$End_time = parse_date_time(allsamples$SamplerEnd, orders = "HM")
allsamples$End_Date_final = str_c(allsamples$End, " ",hour(allsamples$End_time), ":", minute(allsamples$End_time))
allsamples$End_Date_final = parse_date_time(allsamples$End_Date_final, orders = "y-m-d H:M", tz = "America/Los_Angeles")
allsamples = allsamples %>%
  relocate(End_Date_final, .after = Start_Date_final)
allsamples = allsamples %>%
  relocate(CollectionDate,SamplerEnd, .after = End_time)


# Working on air samples
allsamples_PUFs = allsamples %>%
  filter(CollectionMethod %in% "PUF")

#Importing UO nasal sample data
UO <- read.csv("C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder\\Multimodal_Surveillance\\Data\\De-ID_Quarantine_History.csv", stringsAsFactors = FALSE)
#Fixing their time 
UO$Start = parse_date_time(UO$Temp.Start.Date, orders = "mdy HM", tz = "America/Los_Angeles")
UO$End = parse_date_time(UO$Temp.End.Date, orders = "mdy HM", tz = "America/Los_Angeles")

#Fixing UO dataframe room ID
UO$room  = as.numeric(substr(UO$Temp.Room, 10, 12))
UO = UO %>%
  relocate(room, .after = Temp.Reason)
UO = UO %>%
    relocate(Temp.Room, .after = End)
UO = UO %>%
  relocate(Temp.Start.Date, Temp.End.Date, .after = End)
UO = UO %>%
    relocate(Start, End, .after = room)


# Matching UO dataset with allsamples 



#Number of subjects per each exhaust 
for (i in allsamples$SampleID) {
  for (j in UO$ID) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[UO$ID == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[UO$ID == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[UO$ID == j]) %in% TRUE) {
      allsamples$V1[allsamples$SampleID == i] = 1 + allsamples$V1[allsamples$SampleID == i]
    } 
  }
}
#V1 with row
allsamples$V1_R = 0
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V1_R[allsamples$SampleID == i] = 1 + allsamples$V1_R[allsamples$SampleID == i]
    } 
  }
}
#V1 without suspects
allsamples$V1_R_Nosus = 0
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE && UO$Temp.Reason[as.numeric(row.names(UO)) == j] %nin% "Suspect" ) {
      allsamples$V1_R_Nosus[allsamples$SampleID == i] = 1 + allsamples$V1_R_Nosus[allsamples$SampleID == i]
    } 
  }
}
# V1 intermittent 
allsamples$V1_R_int = 0
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V1_R_int[allsamples$SampleID == i] = 1 + allsamples$V1_R_int[allsamples$SampleID == i]
    } else if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] > UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE &&  isTRUE(allsamples$Start_Date_final[allsamples$SampleID == i] < UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V1_R_int[allsamples$SampleID == i] = 1 + allsamples$V1_R_int[allsamples$SampleID == i]
    }  else if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE && isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] < UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE ){
      allsamples$V1_R_int[allsamples$SampleID == i] = 1 + allsamples$V1_R_int[allsamples$SampleID == i]
    }
  }
}
#Total number of subject in building during sample collection period 
for (i in allsamples$SampleID) {
  for (j in UO$ID) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[UO$ID == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[UO$ID == j]) %in% TRUE) {
      allsamples$V2[allsamples$SampleID == i] = 1 + allsamples$V2[allsamples$SampleID == i]
    } 
  }
}
#V3 incorporates all people including suspects and did it by UO Sample ID
for (i in allsamples$SampleID) {
  for (j in UO$ID) {
    if ( isTRUE(date(allsamples$End_Date_final[allsamples$SampleID == i]) >= date(UO$Start[UO$ID == j])) %in% TRUE  &&  isTRUE(date(allsamples$End_Date_final[allsamples$SampleID == i]) <= date(UO$End[UO$ID == j])) %in% TRUE) {
      allsamples$V3[allsamples$SampleID == i] = 1 + allsamples$V3[allsamples$SampleID == i]
    } 
  }
}
#V4 incorporates all people including suspects and did it by UO rows
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V4[allsamples$SampleID == i] = 1 + allsamples$V4[allsamples$SampleID == i]
    } 
  }
}
#V4 intermittent 
allsamples$V4_R_int = 0
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V4_R_int[allsamples$SampleID == i] = 1 + allsamples$V4_R_int[allsamples$SampleID == i]
    } else if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] > UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE &&  isTRUE(allsamples$Start_Date_final[allsamples$SampleID == i] < UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$V4_R_int[allsamples$SampleID == i] = 1 + allsamples$V4_R_int[allsamples$SampleID == i]
    }  else if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] < UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE ){
      allsamples$V4_R_int[allsamples$SampleID == i] = 1 + allsamples$V4_R_int[allsamples$SampleID == i]
    }
  }
}

#V4 without suspects
allsamples$V4_Nosus = 0
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE && UO$Temp.Reason[as.numeric(row.names(UO)) == j] %nin% "Suspect" ) {
      allsamples$V4_Nosus[allsamples$SampleID == i] = 1 + allsamples$V4_Nosus[allsamples$SampleID == i]
    } 
  }
}



# Average Ct value of total subjects (those reported)
for (i in allsamples$SampleID) {
  for (j in UO$ID) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[UO$ID == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[UO$ID == j]) %in% TRUE && isTRUE(UO$Average.SARS.CoV.2.Ct_round[UO$ID == j] > 1) %in% TRUE) {
      allsamples$SubCt[allsamples$SampleID == i] = str_c(allsamples$SubCt[allsamples$SampleID == i], ",", UO$Average.SARS.CoV.2.Ct_round[UO$ID == j])
    } 
  }
}

for (i in allsamples$SampleID) {
  allsamples$Ct_all_people[allsamples$SampleID == i] = substr(allsamples$SubCt[allsamples$SampleID == i], start = 3, stop = str_count(allsamples$SubCt[allsamples$SampleID == i]))
  
}

allsamples = allsamples %>%
  relocate(Ct_all_people, .after = SubCt)

allsamples = allsamples %>%
  relocate(SubCt, .after = SamplerEnd)



# Average Ct value of subjects per each exhaust (those reported)
for (i in allsamples$SampleID) {
  for (j in UO$ID) {
    if ( isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[UO$ID == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[UO$ID == j]) %in% TRUE && isTRUE(UO$Average.SARS.CoV.2.Ct_round[UO$ID == j] > 1) %in% TRUE && str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[UO$ID == j])) %in% TRUE) {
      allsamples$SubCt_Ex[allsamples$SampleID == i] = str_c(allsamples$SubCt_Ex[allsamples$SampleID == i], ",", UO$Average.SARS.CoV.2.Ct_round[UO$ID == j])
    } 
  }
}



for (i in allsamples$SampleID) {
  allsamples$Ct_people_shaft[allsamples$SampleID == i] = substr(allsamples$SubCt_Ex[allsamples$SampleID == i], start = 3, stop = str_count(allsamples$SubCt_Ex[allsamples$SampleID == i]))
  
}

allsamples = allsamples %>%
  relocate(Ct_people_shaft, .after = V1)


#Average Ct value of each shaft 
for (i in allsamples$SampleID) {
  allsamples$Average_Ct_shaft[allsamples$SampleID ==i] = mean(as.numeric(unlist(strsplit(allsamples$Ct_people_shaft[allsamples$SampleID ==i], split = ","))))
}
allsamples = allsamples %>%
  relocate(Average_Ct_shaft, .after = Ct_people_shaft)

# First regression 
PUFs$Average_Ct_shaft = as.numeric(PUFs$Average_Ct_shaft)

PUFs_withCt = PUFs %>%
  filter(Average_Ct_shaft > 1)


#Statistically significant correlation between Ct value of roof and number of people per exhaust 
summary(lm(formula = PUFs$ct ~ PUFs$V1 + PUFs$Location))

#Barnhart fit model
Barnhart_Fit_nose = lmer(temp.location$Ct[temp.location$Location %in% "Shallow Nasal Swab"]~as.numeric(temp.location$PosDay[temp.location$Location %in% "Shallow Nasal Swab"])+(1|temp.location$SubjectID[temp.location$Location %in% "Shallow Nasal Swab"]))
summary(Barnhart_Fit_nose)


# Kim et al (2021)  fit model
summary(lmer(Patient_data$Ct~as.numeric(Patient_data$Day) + (1|Patient_data$Patient)))

# Estimating Ct value of nose during quarantine period based on Kim et al (2021)
uo_x_2 = uo_x
uo_x_2$repnumber = as.numeric(ceiling((difftime(uo_x_2$End, uo_x_2$Start, units = "days"))))
# Creating a data frame to expand participants day
uo_x_3  = as.data.frame(lapply(uo_x_2, rep, uo_x_2$repnumber+1))
uo_x_3$Sampdate_OR = parse_date_time(uo_x_3$qPCR.run.date, orders = "m/d/y", tz = "America/Los_Angeles")
uo_x_3$Currentdate = 

  # Calculating days post COVID test (Samdate) 
  
 


    
uo_x_3$Diseasestatus = as.numeric(date(uo_x_3$Sampdate) - date(uo_x_3$Sampdate_OR))

#Those without MAPtest will be given a value
for (i in as.numeric(row.names(uo_x_3))) {
  if (uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] %in% "tbt") {
    uo_x_3$Sampdate_OR[as.numeric(row.names(uo_x_3)) == i] = uo_x_3$Start[as.numeric(row.names(uo_x_3)) == i]
  }
}


#Ct value of day 0 for those without MAP test = 23.8279
for (i in as.numeric(row.names(uo_x_3))) {
  if (uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] %in% "tbt" & uo_x_3$Diseasestatus[as.numeric(row.names(uo_x_3)) == i] == 0) {
    uo_x_3$ct_estimate_phase2[as.numeric(row.names(uo_x_3)) == i] = 23.8279
  }
}

# Ct value of consecutive days for those without MAP test

for (i in as.numeric(row.names(uo_x_3))) {
  if (uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] %in% "tbt") {
    uo_x_3$Average.SARS.CoV.2.Ct[as.numeric(row.names(uo_x_3)) == i] = 23.8279
  }
}

for (i in as.numeric(row.names(uo_x_3))) {
  if (uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] %in% "tbt" & uo_x_3$Diseasestatus[as.numeric(row.names(uo_x_3)) == i] %nin% 0) {
    uo_x_3$ct_estimate_phase2[as.numeric(row.names(uo_x_3)) == i] = 0.6663 * uo_x_3$Diseasestatus[as.numeric(row.names(uo_x_3)) == i] + uo_x_3$Average.SARS.CoV.2.Ct[as.numeric(row.names(uo_x_3)) == i]
  }
}



# Estimating the Ct value based in regression model
for (i in as.numeric(row.names(uo_x_3))) {
  if (isTRUE(uo_x_3$Sampdate[as.numeric(row.names(uo_x_3)) == i] == uo_x_3$Sampdate_OR[as.numeric(row.names(uo_x_3)) == i])%in% TRUE) {
    uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] = uo_x_3$Average.SARS.CoV.2.Ct[as.numeric(row.names(uo_x_3)) == i]
  }
}

for (i in as.numeric(row.names(uo_x_3))) {
  if (isTRUE(uo_x_3$Average.SARS.CoV.2.Ct[as.numeric(row.names(uo_x_3)) == i] > 0) %in% TRUE) {
    uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == i] = 0.6663 * uo_x_3$Diseasestatus[as.numeric(row.names(uo_x_3)) == i] + uo_x_3$Average.SARS.CoV.2.Ct[as.numeric(row.names(uo_x_3)) == i]
  }
}



#Estimated Ct value incorporated per each exhaust 
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(uo_x_3))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(uo_x_3$room[as.numeric(row.names(uo_x_3)) == j])) %in% TRUE && isTRUE(date(allsamples$End_Date_final[allsamples$SampleID == i]) == date(uo_x_3$Sampdate[as.numeric(row.names(uo_x_3)) == j])) %in% TRUE ) {
      allsamples$Ct_estimate[allsamples$SampleID == i] = str_c(allsamples$Ct_estimate[allsamples$SampleID == i], ",", uo_x_3$ct_estimate[as.numeric(row.names(uo_x_3)) == j] )
    }
  }
}

allsamples$Ct_estimate_notbt = gsub("tbt,", x = allsamples$Ct_estimate, replacement = "")
allsamples$Ct_estimate_notbt2 = gsub(",tbt", x = allsamples$Ct_estimate_notbt, replacement = "")
allsamples$Ct_estimate_notbt3 = gsub("A,", x = allsamples$Ct_estimate_notbt2, replacement = "")




#Estimated Ct value incorporated per each exhaust Phase 2
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(uo_x_3))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(uo_x_3$room[as.numeric(row.names(uo_x_3)) == j])) %in% TRUE && isTRUE(date(allsamples$End_Date_final[allsamples$SampleID == i]) == date(uo_x_3$Sampdate[as.numeric(row.names(uo_x_3)) == j])) %in% TRUE ) {
      allsamples$Ct_estimate_phase2[allsamples$SampleID == i] = str_c(allsamples$Ct_estimate_phase2[allsamples$SampleID == i], ",", uo_x_3$ct_estimate_phase2[as.numeric(row.names(uo_x_3)) == j] )
    }
  }
}
allsamples$Ct_estimate_phase2_average = gsub("C,", x = allsamples$Ct_estimate_phase2, replacement = "")
allsamples$Ct_estimate_phase2_average = gsub("C", x = allsamples$Ct_estimate_phase2_average, replacement = "")


#Calculating census differently (Superceeded)
for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(UO$room[as.numeric(row.names(UO)) == j])) %in% TRUE &&isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(allsamples$End_Date_final[allsamples$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      allsamples$Testcensus[allsamples$SampleID == i] = 1 + allsamples$Testcensus[allsamples$SampleID == i]
    } 
  }
}
for (i in allsamples$SampleID)
 {
  allsamples$Average_Ct_Phase2[allsamples$SampleID ==i] = mean(as.numeric(unlist(strsplit(allsamples$Ct_estimate_phase2_average[allsamples$SampleID ==i], split = ","))))
}
allsamples = allsamples %>%
  relocate(Average_Ct_Phase2, .after = V1)

# Getting the min Ct of multiple values for phase 2
allsamples$minct = "tbt"
#Taking min of Ct
for (i in allsamples$SampleID)
{
  allsamples$minct[allsamples$SampleID ==i] = min(as.numeric(unlist(strsplit(allsamples$Ct_estimate_phase2_average[allsamples$SampleID ==i], split = ","))))
}
#Taking positive of Ct
for (i in allsamples$SampleID)
{
  allsamples$sumct[allsamples$SampleID ==i] = sum(as.numeric(unlist(strsplit(allsamples$Ct_estimate_phase2_average[allsamples$SampleID ==i], split = ","))))
}

#Keep this for now 
summary(lm(allsamples$ct[allsamples$CollectionMethod %in% "PUF"] ~ allsamples$sumct[ allsamples$CollectionMethod %in% "PUF"] + allsamples$Location[ allsamples$CollectionMethod %in% "PUF"]))

# Ct estimation formula 
#Y = 23.8279 + 0.6663 * x
#y = 0.6663 * (pos day) + base Ct value

# Finding repeated IDs 
UO_Unique = UO
UO_Unique$Tick[duplicated(UO_Unique$ID) %in% TRUE] = "yes"

UO_Unique = UO_Unique %>%
  filter(UO_Unique$Temp.Reason %nin% "Suspect")

UO_Unique$Tick_2[duplicated(UO_Unique$ID) %in% TRUE] = "yes"

UO_Unique = UO_Unique %>%
  filter(UO_Unique$Temp.Reason %nin% "Suspect")

# Statistical test
summary(lm(allsamples$ct[allsamples$Ct_people_shaft >0 & allsamples$V1 > 0] ~ allsamples$Ct_people_shaft[allsamples$Ct_people_shaft >0 & allsamples$V1 > 0] + allsamples$Location[allsamples$Ct_people_shaft >0 & allsamples$V1 > 0]))
summary(lmer(allsamples$ct[allsamples$Average_Ct_AUG23 %nin% "NaN" & allsamples$V1 > 0] ~ allsamples$Average_Ct_AUG23[allsamples$Average_Ct_AUG23 %nin% "NaN" & allsamples$V1 > 0] + (1|allsamples$Location[allsamples$Average_Ct_AUG23 %nin% "NaN" & allsamples$V1 > 0])))


# Let's give tbts estimation value
uo_x_3$ct_estimate_phase2 = uo_x_3$ct_estimate

#Incorporating airflow data
allsamples$closedAH = "tbt"
allsamples$openAH = "tbt"

for (i in allsamples$SampleID) {
  
  for (j in as.numeric(row.names(Airflow1))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(Airflow1$`Room NNN`[as.numeric(row.names(Airflow1)) == j])) %in% TRUE) {
      allsamples$closedAH[allsamples$SampleID ==i] = str_c(  allsamples$closedAH[allsamples$SampleID ==i], ",", Airflow1$`Measured window closed, bath door closed, hall door closed (ACH)`[as.numeric(row.names(Airflow1)) == j])
    }
  } 
}


for (i in allsamples$SampleID) {
  
  for (j in as.numeric(row.names(Airflow1))) {
    if (str_detect(allsamples$source.rooms[allsamples$SampleID == i], as.character(Airflow1$`Room NNN`[as.numeric(row.names(Airflow1)) == j])) %in% TRUE) {
      allsamples$openAH[allsamples$SampleID ==i] = str_c(  allsamples$openAH[allsamples$SampleID ==i], ",", Airflow1$`Measured window open, bath door open, hall door closed (ACH)`[as.numeric(row.names(Airflow1)) == j])
    }
  } 
}

allsamples$openAH = gsub("C,", x = allsamples$openAH, replacement = "")
allsamples$openAH = gsub("C", x = allsamples$openAH, replacement = "")
allsamples$closedAH = gsub("C,", x = allsamples$closedAH, replacement = "")
allsamples$closedAH = gsub("C", x = allsamples$closedAH, replacement = "")

for (i in allsamples$SampleID)
{
  allsamples$Average_ACH_Closed[allsamples$SampleID ==i] = mean(as.numeric(unlist(strsplit(allsamples$closedAH[allsamples$SampleID ==i], split = ","))))
}
for (i in allsamples$SampleID)
{
  allsamples$Average_ACH_Open[allsamples$SampleID ==i] = mean(as.numeric(unlist(strsplit(allsamples$openAH[allsamples$SampleID ==i], split = ","))))
}

for (i in allsamples$SampleID)
{
  allsamples$Sum_ACH_Closed[allsamples$SampleID ==i] = sum(as.numeric(unlist(strsplit(allsamples$closedAH[allsamples$SampleID ==i], split = ","))))
}
for (i in allsamples$SampleID)
{
  allsamples$Sum_ACH_Open[allsamples$SampleID ==i] = sum(as.numeric(unlist(strsplit(allsamples$openAH[allsamples$SampleID ==i], split = ","))))
}


#Calculate average Ct of all people present in Barnhart each day (Ct phase 3) 

for (i in allsamples$SampleID) {
  for (j in as.numeric(row.names(uo_x_3))) {
    if ( isTRUE(date(allsamples$End_Date_final[allsamples$SampleID == i]) == date(uo_x_3$Sampdate[as.numeric(row.names(uo_x_3)) == j])) %in% TRUE ) {
      allsamples$Ct_estimate_phase3[allsamples$SampleID == i] = str_c(allsamples$Ct_estimate_phase3[allsamples$SampleID == i], ",", uo_x_3$ct_estimate_phase2[as.numeric(row.names(uo_x_3)) == j] )
    }
  }
}


allsamples$Ct_estimate_phase3_average = gsub("C,", x = allsamples$Ct_estimate_phase3, replacement = "")
allsamples$Ct_estimate_phase3_average = gsub("C", x = allsamples$Ct_estimate_phase3_average, replacement = "")

for (i in allsamples$SampleID)
{
  allsamples$Average_Ct_all_Phase3[allsamples$SampleID ==i] = mean(as.numeric(unlist(strsplit(allsamples$Ct_estimate_phase3_average[allsamples$SampleID ==i], split = ","))))
}


#Exhaust 
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #26" & allsamples$sumct >0 ]~allsamples$sumct[allsamples$Location %in% "Exhaust #26"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #37"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #37"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #33"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #33"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #29"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #29"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #32"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #32"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #40"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #40"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #30"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #30"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #34"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #34"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #27"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #27"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #38 & #39"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Exhaust #38 & #39"& allsamples$sumct >0]))


summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #26" & allsamples$minct %nin% "Inf" ]~allsamples$minct[allsamples$Location %in% "Exhaust #26" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #37" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #37" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #33" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #33" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #29" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #29" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #32" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #32" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #40" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #40" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #30" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #30" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #34" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #34" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #27" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #27" & allsamples$minct %nin% "Inf"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #38 & #39" & allsamples$minct %nin% "Inf"]~allsamples$minct[allsamples$Location %in% "Exhaust #38 & #39" & allsamples$minct %nin% "Inf"]))

summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #26" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #26" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #37" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #37" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #33" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #33" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #29" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #29" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #32" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #32" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #40" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #40" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #30" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #30" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #34" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #34" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #27" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #27" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))
summary(lm(allsamples$ct[allsamples$Location %in% "Exhaust #38 & #39" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Exhaust #38 & #39" & allsamples$Average_Ct_Phase2 %nin% "NaN"]))

#Port
summary(lm(allsamples$ct[allsamples$Location %in% "Port #26" & allsamples$sumct >0 ]~allsamples$sumct[allsamples$Location %in% "Port #26"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #37"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #37"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #33"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #33"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #29"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #29"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #32"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #32"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #40"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #40"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #30"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #30"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #34"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #34"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #27"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #27"& allsamples$sumct >0]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #38 & #39"& allsamples$sumct >0]~allsamples$sumct[allsamples$Location %in% "Port #38 & #39"& allsamples$sumct >0]))


summary(lm(allsamples$ct[allsamples$Location %in% "Port #26" & allsamples$Average_Ct_Phase2 %nin% "NaN"  ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #26" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #37" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #37" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #33" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #33" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #29" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #29" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #32" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #32" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #40" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #40" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #30" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #30" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #34" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #34" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #27" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #27" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #38 & #39" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #38 & #39" & allsamples$Average_Ct_Phase2 %nin% "NaN" ]))


summary(lm(allsamples$ct[allsamples$Location %in% "Port #26" & allsamples$Average_Ct_Phase2 %nin% "NaN" & str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #26"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #37"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #37"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #33"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #33"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #29"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #29"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #32"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #32"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #40"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #40"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #30"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #30"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #34"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #34"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #27"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #27"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))
summary(lm(allsamples$ct[allsamples$Location %in% "Port #38 & #39"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]~allsamples$Average_Ct_Phase2[allsamples$Location %in% "Port #38 & #39"& allsamples$Average_Ct_Phase2 %nin% "NaN"& str_detect(allsamples$Ct_estimate, "tbt") %in% FALSE]))


# Individual port and exhaust Vs. Number of people 
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #26"  ]~allsamples$ct[allsamples$Location %in% "Exhaust #26"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #37"]~allsamples$ct[allsamples$Location %in% "Exhaust #37"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #33"]~allsamples$ct[allsamples$Location %in% "Exhaust #33"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #29"]~allsamples$ct[allsamples$Location %in% "Exhaust #29"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #32"]~allsamples$ct[allsamples$Location %in% "Exhaust #32"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #40"]~allsamples$ct[allsamples$Location %in% "Exhaust #40"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #30"]~allsamples$ct[allsamples$Location %in% "Exhaust #30"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #34"]~allsamples$ct[allsamples$Location %in% "Exhaust #34"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #27"]~allsamples$ct[allsamples$Location %in% "Exhaust #27"]))
summary(lm(allsamples$V1[allsamples$Location %in% "Exhaust #38 & #39"]~allsamples$ct[allsamples$Location %in% "Exhaust #38 & #39"]))


#################
#################
#################
#################
#################
#################
#################
#################
#################
#################
#################
#################
allsamples$Start = str_c(allsamples$Start.Date, " ", allsamples$SamplerStart)
allsamples$Start_Date_final = str_c(allsamples$Start_Date, " ",hour(allsamples$Start_hour), ":", minute(allsamples$Start_hour))
allsamples$Start_Date_final = parse_date_time(allsamples$Start_Date_final, orders = "y-m-d H:M", tz = "America/Los_Angeles")
#Importing Barnhart_result data 

#Duplicated sample ID
for (i in as.numeric(row.names(Barnhart_Results))) {
  
  if (Barnhart_Results$SampleID[as.numeric(row.names(Barnhart_Results)) == i] %in% NA && as.numeric(Barnhart_Results$SampleID[as.numeric(row.names(Barnhart_Results)) == i+1]) - as.numeric(Barnhart_Results$SampleID[as.numeric(row.names(Barnhart_Results)) == i-1]) == 2) {
    Barnhart_Results$SampleID[as.numeric(row.names(Barnhart_Results)) == i] = 1+ Barnhart_Results$SampleID[as.numeric(row.names(Barnhart_Results)) == i - 1]
  }
  
  
}

#Barnhart data set does not have duplicated sample IDs period. 

#Fixing dates
for (i in Barnhart$SampleID) {
  if (str_count(Barnhart$SamplerEnd[Barnhart$SampleID == i]) %in% 3){
    Barnhart$SamplerEnd_modified[Barnhart$SampleID == i] = str_c(0, Barnhart$SamplerEnd[Barnhart$SampleID == i])
  } else {
    Barnhart$SamplerEnd_modified[Barnhart$SampleID == i] =   Barnhart$SamplerEnd[Barnhart$SampleID == i]
  }
}
Barnhart$End_h = parse_date_time(Barnhart$SamplerEnd_modified, orders = "HM", tz = "America/Los_Angeles")

for (i in Barnhart$SampleID) {
  if (str_count(Barnhart$SamplerStart[Barnhart$SampleID == i]) %in% 3){
    Barnhart$SamplerStart_modified[Barnhart$SampleID == i] = str_c(0, Barnhart$SamplerStart[Barnhart$SampleID == i])
  } else {
    Barnhart$SamplerStart_modified[Barnhart$SampleID == i] =   Barnhart$SamplerStart[Barnhart$SampleID == i]
  }
}
Barnhart$Start_h = parse_date_time(Barnhart$SamplerStart_modified, orders = "HM", tz = "America/Los_Angeles")



for (i in  Barnhart$SampleID) {
  
  if (isTRUE(Barnhart$Start_h[Barnhart$SampleID == i] %nin% NA) %in% TRUE && isTRUE(Barnhart$Start_h[Barnhart$SampleID == i] > Barnhart$End_h[Barnhart$SampleID == i])) {
    Barnhart$End[Barnhart$SampleID == i] = str_c(Barnhart$Date[Barnhart$SampleID == i] + days(1), " ", hour(Barnhart$End_h[Barnhart$SampleID == i]), ":", minute(Barnhart$End_h[Barnhart$SampleID == i]))
  } else  {
    Barnhart$End[Barnhart$SampleID == i] = str_c(Barnhart$Date[Barnhart$SampleID == i], " ", hour(Barnhart$End_h[Barnhart$SampleID == i]), ":", minute(Barnhart$End_h[Barnhart$SampleID == i]))
  }
  
}



Barnhart = Barnhart %>%
  relocate(End, .after = SamplerEnd)

for (i in Barnhart$SampleID) {
  Barnhart$Start[Barnhart$SampleID == i] = str_c(Barnhart$Date[Barnhart$SampleID == i], " ", hour(Barnhart$Start_h[Barnhart$SampleID == i]), ":", minute(Barnhart$Start_h[Barnhart$SampleID == i]))
}
Barnhart$Start = parse_date_time(Barnhart$Start, orders = "y-m-d H:M", tz = "America/Los_Angeles")

Barnhart = Barnhart %>%
  relocate(Start, .after = SamplerEnd)

Barnhart = Barnhart %>%
  relocate(c(Start_h, Date, End_h, SamplerStart, SamplerEnd), .after = SamplerStart_modified)


#Working on Lobby 

lobby = Barnhart %>%
  filter(`Room #` %in% "Lobby")

for (i in lobby$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( isTRUE(lobby$Start[lobby$SampleID == i] %nin% NA) %in% TRUE  && isTRUE(lobby$End[lobby$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(lobby$End[lobby$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE) {
      lobby$V2[lobby$SampleID == i] = 1 + lobby$V2[lobby$SampleID == i]
    } 
  }
}

#V_Isolated 
for (i in lobby$SampleID) {
  for (j in as.numeric(row.names(UO))) {
    if ( isTRUE(lobby$Start[lobby$SampleID == i] %nin% NA) %in% TRUE  && isTRUE(lobby$End[lobby$SampleID == i] >= UO$Start[as.numeric(row.names(UO)) == j]) %in% TRUE  &&  isTRUE(lobby$End[lobby$SampleID == i] <= UO$End[as.numeric(row.names(UO)) == j]) %in% TRUE && isTRUE(UO$Temp.Reason[as.numeric(row.names(UO)) == j] %in% "Isolation") %in% TRUE) {
      lobby$V2_Iso[lobby$SampleID == i] = 1 + lobby$V2_Iso[lobby$SampleID == i]
    } 
  }
}

#Block times 
lobby$discrete = "TBT"
for (i in lobby$SampleID) {
  if (isTRUE(hour(lobby$Start[lobby$SampleID == i]) > 8) %in% TRUE && isTRUE(hour(lobby$End[lobby$SampleID == i]) < 12) %in% TRUE){
    lobby$discrete = "Positives"
  }
}

#Doing a paired ttest between rec (9-11) and suspect (11-1)
lobby_paired = data.frame(SampleID_rec = lobby$SampleID[hour(lobby$Start) > 8 & hour(lobby$End) < 12 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)], ct_rec = lobby$ct[hour(lobby$Start) > 8 & hour(lobby$End) < 12 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)],
                          Start_rec = lobby$Start[hour(lobby$Start) > 8 & hour(lobby$End) < 12 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)],
                            End_rec = lobby$End[hour(lobby$Start) > 8 & hour(lobby$End) < 12 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)]
                          )


t.test(lobby$ct[hour(lobby$Start) > 8 & hour(lobby$End) < 12 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)], lobby$ct[hour(lobby$Start) > 10 & hour(lobby$End) < 14 & lobby$Start %nin% NA & date(lobby$Start) == date(lobby$End)])

for (i in lobby_paired$SampleID_rec) {
  for (j in lobby$SampleID) {
    if (isTRUE(date(lobby_paired$End_rec[lobby_paired$SampleID_rec == i]) == date(lobby$End[lobby$SampleID == j])) %in% TRUE &&
        isTRUE(hour(lobby$Start[lobby$SampleID == j]) > 10 & hour(lobby$End[lobby$SampleID == j]) < 14 & lobby$Start[lobby$SampleID == j] %nin% NA & date(lobby$Start[lobby$SampleID == j]) == date(lobby$End[lobby$SampleID == j]))
        ){
      lobby_paired$SampleID_traced[lobby_paired$SampleID_rec == i] = lobby$SampleID[lobby$SampleID == j] 
      lobby_paired$End_traced[lobby_paired$SampleID_rec == i] = lobby$End[lobby$SampleID == j] 
      lobby_paired$Start_traced[lobby_paired$SampleID_rec == i] = lobby$Start[lobby$SampleID == j] 
      lobby_paired$ct_traced[lobby_paired$SampleID_rec == i] = lobby$ct[lobby$SampleID == j] 
      
      
    }
  }
}


for (i in lobby_paired$SampleID_rec) {
  for (j in lobby$SampleID) {
    if (isTRUE(date(lobby_paired$End_rec[lobby_paired$SampleID_rec == i]) == date(lobby$End[lobby$SampleID == j])) %in% TRUE &&
        isTRUE(hour(lobby$Start[lobby$SampleID == j]) > 10 & hour(lobby$End[lobby$SampleID == j]) < 14 & lobby$Start[lobby$SampleID == j] %nin% NA & date(lobby$Start[lobby$SampleID == j]) == date(lobby$End[lobby$SampleID == j]))
    ){
      lobby_paired$SampleID_traced[lobby_paired$SampleID_rec == i] = lobby$SampleID[lobby$SampleID == j] 
      lobby_paired$End_traced[lobby_paired$SampleID_rec == i] = lobby$End[lobby$SampleID == j] 
      lobby_paired$Start_traced[lobby_paired$SampleID_rec == i] = lobby$Start[lobby$SampleID == j] 
      lobby_paired$ct_traced[lobby_paired$SampleID_rec == i] = lobby$ct[lobby$SampleID == j] 
      
      
    }
  }
}

for (i in lobby_paired$SampleID_rec) {
  for (j in lobby$SampleID) {
    if (isTRUE(date(lobby_paired$End_rec[lobby_paired$SampleID_rec == i]) == date(lobby$End[lobby$SampleID == j])) %in% TRUE &&
        isTRUE(hour(lobby$Start[lobby$SampleID == j]) > 12 & hour(lobby$End[lobby$SampleID == j]) < 16 & lobby$Start[lobby$SampleID == j] %nin% NA & date(lobby$Start[lobby$SampleID == j]) == date(lobby$End[lobby$SampleID == j]))
    ){
      lobby_paired$SampleID_traced_present[lobby_paired$SampleID_rec == i] = lobby$SampleID[lobby$SampleID == j] 
      lobby_paired$End_traced_present[lobby_paired$SampleID_rec == i] = lobby$End[lobby$SampleID == j] 
      lobby_paired$Start_traced_present[lobby_paired$SampleID_rec == i] = lobby$Start[lobby$SampleID == j] 
      lobby_paired$ct_traced_present[lobby_paired$SampleID_rec == i] = lobby$ct[lobby$SampleID == j] 
      
      
    }
  }
}

t.test(lobby_paired$ct_rec[lobby_paired$ct_traced_present %nin% NA], lobby_paired$ct_traced_present[lobby_paired$ct_traced_present %nin% NA], paired = TRUE, alternative = "less")



for (i in lobby_paired$SampleID_rec) {
  for (j in lobby$SampleID) {
    if (isTRUE(date(lobby_paired$End_rec[lobby_paired$SampleID_rec == i]) == date(lobby$End[lobby$SampleID == j])) %in% TRUE &&
        isTRUE(hour(lobby$Start[lobby$SampleID == j]) > 14 & hour(lobby$End[lobby$SampleID == j]) < 18 & lobby$Start[lobby$SampleID == j] %nin% NA & date(lobby$Start[lobby$SampleID == j]) == date(lobby$End[lobby$SampleID == j]))
    ){
      lobby_paired$SampleID_emty1[lobby_paired$SampleID_rec == i] = lobby$SampleID[lobby$SampleID == j] 
      lobby_paired$End__emty1[lobby_paired$SampleID_rec == i] = lobby$End[lobby$SampleID == j] 
      lobby_paired$Start__emty1[lobby_paired$SampleID_rec == i] = lobby$Start[lobby$SampleID == j] 
      lobby_paired$ct__emty1[lobby_paired$SampleID_rec == i] = lobby$ct[lobby$SampleID == j] 
      
      
    }
  }
}



Table = tibble(Item = c("Total samples", " Positive samples & patients > 0", " Positive samples & patients = 0",  "Negative samples & patients > 0",  "Negative samples & patients = 0", "% of the time detected", "% of the time not detected"), 
               air_p = c(0,0,0,0,0, 0, 0), 
               surface_p = c(0,0,0,0,0, 0, 0),
               wastewater_p =c(0,0,0,0,0, 0, 0),
               hightouch_p =c(0,0,0,0,0, 0, 0)
)







#Positive people positive samples
#Air
length(allsamples$SampleID[allsamples$ct < 35 & str_detect(allsamples$Location, "Exhaust") %in% TRUE & allsamples$V2 > 0])
#Surface 
length(allsamples$SampleID[allsamples$ct < 35 & str_detect(allsamples$Location, "Port") & allsamples$V2 > 0])
#Watwater 
length(allsamples$SampleID[allsamples$LogCopiesPerL > 0 & allsamples$LogCopiesPerL %nin% NA  &allsamples$V2 > 0])
#High touched surface
length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$V2 > 0 & allsamples$ct < 35])


#No positive people positive sample
#Air
length(allsamples$SampleID[allsamples$ct < 35 & str_detect(allsamples$Location, "Exhaust") & allsamples$V2 == 0])
#Surface 
length(allsamples$SampleID[allsamples$ct < 35 & str_detect(allsamples$Location, "Port") & allsamples$V2 == 0])
#Watwater 
length(allsamples$SampleID[allsamples$LogCopiesPerL > 0 & allsamples$LogCopiesPerL %nin% NA  &allsamples$V2 == 0])
#High tiuched surface
length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$V2 == 0 & allsamples$ct < 35])


#Positive people negative samples
#Air
length(allsamples$SampleID[allsamples$ct >= 35 & str_detect(allsamples$Location, "Exhaust") & allsamples$V2 > 0])
#Surface 
length(allsamples$SampleID[allsamples$ct >= 35 & str_detect(allsamples$Location, "Port") & allsamples$V2 > 0])
#Watwater 
length(allsamples$SampleID[allsamples$LogCopiesPerL > 0 & allsamples$LogCopiesPerL %nin% NA  &allsamples$V2 > 0])
#High touched surface 
length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$V2 > 0 & allsamples$ct >= 35])


#No positive people negative sample
#Air
length(allsamples$SampleID[allsamples$ct >= 35 & str_detect(allsamples$Location, "Exhaust") & allsamples$V2 == 0])
#Surface 
length(allsamples$SampleID[allsamples$ct >= 35 & str_detect(allsamples$Location, "Port") & allsamples$V2 == 0])
#Watwater 
length(allsamples$SampleID[allsamples$LogCopiesPerL > 0 & allsamples$LogCopiesPerL %nin% NA  &allsamples$V2 == 0])
#HIgh touched surface
length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$V2 == 0 & allsamples$ct >= 35])






Table$air_p[1] = length(allsamples$SampleID[str_detect(allsamples$Location, "Exhaust") ])
Table$air_p[2] = length(allsamples$SampleID[str_detect(allsamples$Location, "Exhaust") & allsamples$ct < 35 & allsamples$V1 > 0 ])
Table$air_p[3] = length(allsamples$SampleID[str_detect(allsamples$Location, "Exhaust") & allsamples$ct < 35 & allsamples$V1 == 0 ])
Table$air_p[4] = length(allsamples$SampleID[str_detect(allsamples$Location, "Exhaust") & allsamples$ct >= 35 & allsamples$V1 > 0 ])
Table$air_p[5] = length(allsamples$SampleID[str_detect(allsamples$Location, "Exhaust") & allsamples$ct >= 35 & allsamples$V1 == 0 ])

Table$surface_p[1] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") ])
Table$surface_p[2] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") & allsamples$ct < 35 & allsamples$V1 > 0 ])
Table$surface_p[3] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") & allsamples$ct < 35 & allsamples$V1 == 0 ])
Table$surface_p[4] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") & allsamples$ct >= 35 & allsamples$V1 > 0 ])
Table$surface_p[5] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") & allsamples$ct >= 35 & allsamples$V1 == 0 ])

Table$wastewater_p[1] = length(allsamples$SampleID[str_detect(allsamples$Location, "North") ])
Table$wastewater_p[2] = length(allsamples$SampleID[str_detect(allsamples$Location, "North") & allsamples$LogCopiesPerL > 0  & allsamples$V4_Nosus > 0 ])
Table$wastewater_p[3] = length(allsamples$SampleID[str_detect(allsamples$Location, "North") & allsamples$LogCopiesPerL > 0  & allsamples$V4_Nosus == 0 ])
Table$wastewater_p[4] = "NA"
Table$wastewater_p[5] = "NA"

Table$hightouch_p[1] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" ])
Table$hightouch_p[2] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$ct < 35 & allsamples$V2 > 0 ])
Table$hightouch_p[3] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$ct < 35 & allsamples$V2 == 0 & allsamples$ct %nin% NA])
Table$hightouch_p[4] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$ct >= 35 & allsamples$V2 > 0 & allsamples$ct %nin% NA])
Table$hightouch_p[5] = length(allsamples$SampleID[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" & allsamples$ct >= 35 & allsamples$V2 == 0 & allsamples$ct %nin% NA])


#################### Ct estimation objects ############################# ############################# ############################# ############################# ############################# ############################# #############################

#Check UO_X_3 that is built upon UO map data (UO)


# Check allsamples object. Column V1_R shows the cencus for each stack and V2 is total number of people ,, all calcualted based upon UO map data



#################### Stat ############################# ############################# ############################# ############################# ############################# ############################# #############################



summary(lmer(allsamples$ct[ str_detect(allsamples$Location, "Port") & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$V1_R[str_detect(allsamples$Location, "Port")& allsamples$Average_Ct_Phase2 %nin% "NaN" ]  + (allsamples$Average_Ct_Phase2[ str_detect(allsamples$Location, "Port")& allsamples$Average_Ct_Phase2 %nin% "NaN" ]|allsamples$Location[ str_detect(allsamples$Location, "Port")& allsamples$Average_Ct_Phase2 %nin% "NaN" ])  + (allsamples$Average_ACH_Closed[str_detect(allsamples$Location, "Port")& allsamples$Average_Ct_Phase2 %nin% "NaN" ]|allsamples$Location[ str_detect(allsamples$Location, "Port")& allsamples$Average_Ct_Phase2 %nin% "NaN" ])))
summary(lmer(allsamples$ct[ allsamples$CollectionMethod %in% "PUF" & allsamples$Average_Ct_Phase2 %nin% "NaN"]~allsamples$V1_R[allsamples$CollectionMethod %in% "PUF"& allsamples$Average_Ct_Phase2 %nin% "NaN" ]  + (allsamples$Average_Ct_Phase2[ allsamples$CollectionMethod %in% "PUF"& allsamples$Average_Ct_Phase2 %nin% "NaN" ]|allsamples$Location[ allsamples$CollectionMethod %in% "PUF"& allsamples$Average_Ct_Phase2 %nin% "NaN" ])))
summary(lm(allsamples$LogCopiesPerL[ allsamples$Room.Type %in% "Shop"  ]~allsamples$V4_R[allsamples$Room.Type %in% "Shop"] + allsamples$Average_Ct_all_Phase3[allsamples$Room.Type %in% "Shop"]))
summary(lm(allsamples$ct[ str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" ]~allsamples$V2[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" ] + allsamples$Average_Ct_all_Phase3[str_detect(allsamples$Location, "Port") %in% FALSE & allsamples$CollectionMethod %in% "Swab" ] ))





#################### Figure ############################# ############################# ############################# ############################# ############################# ############################# #############################

Fig_air = ggplot(subset(allsamples, allsamples$CollectionMethod %in% "PUF" & allsamples$Average_Ct_Phase2 %nin% "NaN" ), aes(as.numeric(V1_R), ct, group = V1_R)) +
  geom_boxplot( alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21, size = 2, alpha = 0.5, aes(fill = Location)) + 
  geom_smooth(method = 'glm', color = "black", aes(group=1)) +
  theme_classic() +
  labs(x= "Census data grouped by exhaust location ", tag = "A", y = expression(paste("Aerosol C"["T"])),  subtitle = paste("P < 0.05")) +
  theme(text = element_text(size=15),
        axis.text = element_text(size=12),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"),
        plot.tag = element_text(size = 15, face = "bold")) +
  scale_y_continuous(trans = 'reverse')


Fig_air

Fig_port = ggplot(subset(allsamples, str_detect(allsamples$Location, "Port") %in% TRUE & allsamples$Average_Ct_Phase2 %nin% "NaN"), aes(as.numeric(V1_R), ct, group = V1_R))+
  geom_boxplot( alpha = 0.5, outlier.shape = NA) +
  geom_jitter(shape = 21,size = 2,  aes(fill = Location), alpha = 0.5) +
  geom_smooth(method = 'glm', color = "black", aes(group=1)) +
  labs(x= "Census data grouped by exhaust location", tag = "B", y = expression(paste("Exhaust Surface C"["T"])), subtitle = paste("Not significant"))+
  theme_classic()+
  theme(text = element_text(size=15),
        axis.text = element_text(size=12),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"))+
  theme(plot.tag = element_text(size = 15, face = "bold")) +
  scale_y_continuous(trans = 'reverse')




Fig_wastewater = ggplot(subset(allsamples, allsamples$Room.Type %in% "Shop"), aes(x = wing.n, y = LogCopiesPerL, color = LogCopiesPerL)) +
  geom_point(size = 2) +
  geom_smooth(method = 'glm', color = "black") +
  labs(x= "Building census data", y = "Wastewater viral load", tag = "C", subtitle = paste("P < 0.05")) +
  theme_classic() +
  scale_color_continuous(trans = 'reverse') +
  scale_x_continuous(breaks = seq(0, max(allsamples$wing.n, na.rm = TRUE), by = 2))  +
  theme(text = element_text(size=15),
        axis.text = element_text(size=12),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.9, vjust = -5, face = "italic"))+    
  theme(plot.tag = element_text(size = 15, face = "bold"))



Fig_wastewater

ggarrange(Fig_air, Fig_port, Fig_wastewater, nrow = 3, ncol = 1)+
  ggsave(filename = "Figure 1.png", width = 12, height = 12, units = "in" ,device = "png", path ="C:\\Users\\Andreas\\OneDrive\\Desktop\\Multimodal_Surveillance Projects Folder" ,dpi = 600, limitsize = FALSE)








