# Final version of my thesis is here: https://www.academia.edu/105451701/Analyse_Des_Facteurs_Sociaux_Influençant_La_Qualité_Du_Sommeil
# I couldn't add all source code yet because of the duties about my transition to master's
# Recently I've been practicing DS with Python so, excuses.


library(tidyverse)
library (readxl)
library(tibble)
library(tidyr)
#library(meta)
library(lme4)
library(forestplot)
library(ggrepel)
library(gridExtra)
library(ggthemes)
library(scales)

#This code is not optimal, I learned along the way
#I sometimes used old versions or alternatives, 
#and sometimes I coded too verbose
#I yet to learn dplyr, tidying went messy



#Extract functions

##Extract Worker Data
extract_worker_PC <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  label_text <-  as.character(chunk[2, 7])
  group_size <- as.numeric(chunk[2, 8])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  psqicut_Good <- ifelse(!is.na(psqi_number <- as.numeric(chunk[3,8])), psqi_number, NA)
  psqicut_Bad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 8])), psqi_number, NA)
  psqicut_Badratio <- round((psqicut_Bad / group_size), 2)
  women_psqicutBad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 10])), psqi_number, NA)
  women_psqicutGood<- ifelse(!is.na(psqi_number <- as.numeric(chunk[3, 10])), psqi_number, NA)
  men_psqicutBad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 9])), psqi_number, NA)
  men_psqicutGood <- ifelse(!is.na(psqi_number <- as.numeric(chunk[3, 9])), psqi_number, NA)
  women_psqicutBadratio <- round(women_psqicutBad /(women_psqicutBad+women_psqicutGood), 2)
  men_psqicutBadratio <- round(men_psqicutBad /(men_psqicutBad+men_psqicutGood), 2)
  women_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 10]) / group_size), as.numeric(chunk[2, 10]) / group_size, 0), 2)
  men_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), 
                            as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 0), 2)
  data.frame(study_group = label_text, psqicut_Good = psqicut_Good, psqicut_Bad = psqicut_Bad, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqicutBad = women_psqicutBad, men_psqicutBad = men_psqicutBad, men_psqicutGood = men_psqicutGood, women_psqicutGood = women_psqicutGood, group_size = group_size, psqicut_Badratio = psqicut_Badratio, City = city, Year = year, Author = Author, Age_Mean = Age_Mean, Age_SD = Age_SD, women_psqicutBadratio=women_psqicutBadratio, men_psqicutBadratio=men_psqicutBadratio ) }

extract_worker_PM <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  label_text <-  as.character(chunk[2, 7])
  mean_value <- as.numeric(chunk[3, 8])
  sd_value <- as.numeric(chunk[4, 8])
  group_size <- as.numeric(chunk[2, 8])
  women_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 10])), psqimean, 0)
  women_psqi_sd  <- ifelse(!is.na(psqisd <- as.numeric(chunk[4, 10])), psqisd, NA)
  men_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 9])), psqimean, 0)
  men_psqi_sd <- ifelse(!is.na(psqisd <- as.numeric(chunk[4, 9])), psqisd, NA)
  women_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8])), as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 0), 2)
  men_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), 
                            as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 0), 2)
  lower_limit <- max(mean_value - (1.96 * sd_value), 0)
  upper_limit <- mean_value + (1.96 * sd_value)
  lower_Womenlimit <- max(women_psqimean - (1.96 * women_psqi_sd), 0)
  upper_Womenlimit <- women_psqimean + (1.96 * women_psqi_sd)
  lower_Menlimit <- max(men_psqimean - (1.96 * men_psqi_sd), 0)
  upper_Menlimit <- men_psqimean + (1.96 * men_psqi_sd)
  data.frame(study_group = label_text, PSQI_Mean = mean_value, PSQI_SD = sd_value, lower = lower_limit, upper = upper_limit, SD = sd_value, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqimean = women_psqimean, men_psqimean = men_psqimean, group_size = group_size, City = city, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author,women_psqi_sd = women_psqi_sd, men_psqi_sd = men_psqi_sd,lower_Womenlimit = lower_Womenlimit, upper_Womenlimit= upper_Womenlimit, lower_Menlimit = lower_Menlimit, upper_Menlimit= upper_Menlimit  )}

##Extract Shiftworker Data

extract_shiftworker_PM <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  label_text <-  as.character(chunk[2, 7])
  mean_value <- as.numeric(chunk[3, 8])
  sd_value <- as.numeric(chunk[4, 8])
  group_size <- as.numeric(chunk[2, 8])
  shiftwork_yes <- as.numeric(chunk[2,19])
  shiftwork_no <- as.numeric(chunk[2,20])
  shiftyes_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 19])), psqimean, 0)
  shiftyes_psqi_sd <- ifelse(!is.na(psqisd <- as.numeric(chunk[4, 19])), psqisd, 0)
  shiftno_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 20])), psqimean, 0)
  shiftno_psqi_sd <- ifelse(!is.na(psqisd <- as.numeric(chunk[4, 20])), psqisd, 0)
  shiftworkyes_ratio <- round(as.numeric(chunk[2, 19]) / as.numeric(chunk[2, 8]), 2)
  lower_limit <- max(mean_value - (1.96 * sd_value), 0)
  upper_limit <- mean_value + (1.96 * sd_value)
  lower_shiftyeslimit <- max(shiftyes_psqimean - (1.96 * shiftyes_psqi_sd), 0)
  upper_shiftyeslimit <- shiftyes_psqimean + (1.96 * shiftyes_psqi_sd)
  lower_shiftnolimit <- max(shiftno_psqimean - (1.96 * shiftno_psqi_sd), 0)
  upper_shiftnolimit <- shiftno_psqimean + (1.96 * shiftno_psqi_sd)
  
  women_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 10])), psqimean, 0)
  men_psqimean <- ifelse(!is.na(psqimean <- as.numeric(chunk[3, 9])), psqimean, 0)
  women_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8])), as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 0), 2)
  men_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 0), 2)
  data.frame(study_group = label_text, PSQI_Mean = mean_value, PSQI_SD = sd_value, lower = lower_limit, upper = upper_limit,  Women_Ratio = women_ratio, men_psqimean = men_psqimean, group_size = group_size, City = city, shiftyes = shiftwork_yes, shiftno=shiftwork_no, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author, lower_shiftyeslimit = lower_shiftyeslimit, upper_shiftyeslimit= upper_shiftyeslimit, lower_shiftnolimit = lower_shiftnolimit, upper_shiftnolimit= upper_shiftnolimit  , shiftyes_psqimean = shiftyes_psqimean, shiftno_psqimean = shiftno_psqimean, shiftyes_psqi_sd = shiftyes_psqi_sd, shiftno_psqi_sd = shiftno_psqi_sd, Men_Ratio = men_ratio, women_psqimean = women_psqimean, shiftworkyes_ratio=shiftworkyes_ratio )}


extract_shiftworker_PC <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  label_text <-  as.character(chunk[2, 7])
  group_size <- as.numeric(chunk[2, 8])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  shiftyes <- as.numeric(chunk[2, 19])
  shiftno <- as.numeric(chunk[2, 20])
  shiftyes_psqicutBad <-  as.numeric(chunk[4, 19])
  shiftyes_psqicutGood<- as.numeric(chunk[3, 19])
  shiftno_psqicutBad <-  as.numeric(chunk[4, 20])
  shiftno_psqicutGood <-  as.numeric(chunk[3, 20])
  shiftno_psqicutBadRatio <- round((shiftno_psqicutBad/shiftno),2)
  shiftyes_psqicutBadRatio <- round((shiftyes_psqicutBad/shiftyes),2)
  psqicut_Good <- as.numeric(chunk[3,8])
  psqicut_Bad <- as.numeric(chunk[4, 8])
  psqicut_Badratio <- round((psqicut_Bad / group_size), 2)
  women_psqicutBad <-  as.numeric(chunk[4, 10])
  women_psqicutGood<- as.numeric(chunk[3, 10])
  men_psqicutBad <-  as.numeric(chunk[4, 9])
  men_psqicutGood <-  as.numeric(chunk[3, 9])
  women_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 10]) / group_size), as.numeric(chunk[2, 10]) / group_size, 0), 2)
  men_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), 
                            as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 0), 2)
  data.frame(study_group = label_text, shiftyes=shiftyes,shiftno=shiftno, psqicut_Good = psqicut_Good, psqicut_Bad = psqicut_Bad, shiftyes_psqicutBad=shiftyes_psqicutBad,shiftno_psqicutBad=shiftno_psqicutBad,shiftno_psqicutGood=shiftno_psqicutGood, shiftyes_psqicutGood=shiftyes_psqicutGood, shiftno_psqicutBadRatio=shiftno_psqicutBadRatio,shiftyes_psqicutBadRatio=shiftyes_psqicutBadRatio, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqicutBad = women_psqicutBad, men_psqicutBad = men_psqicutBad, men_psqicutGood = men_psqicutGood, women_psqicutGood = women_psqicutGood, group_size = group_size, psqicut_Badratio = psqicut_Badratio, City = city, Year = year, Author = Author, Age_Mean = Age_Mean, Age_SD = Age_SD ) }


#Extract Genders from Worker Data, 
#this is for additional data frame, 
#rest use Worker Extract
extract_workergender_PC_Comp <- function(chunk){
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  label_text <-  as.character(chunk[2, 7])
  Age_SD <- as.numeric(chunk[2,12])
  Age_Moyen000Écart <- paste(Age_Mean, "-", Age_SD)
  Auteur000Date <- paste(Author, " ", "(", year,")")
  group_size <- as.numeric(chunk[2,8])
  women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 2)
  women_psqiGood <- as.numeric(chunk[3, 10])
  men_psqiGood <- as.numeric(chunk[3,9])
  women_psqiBad <- as.numeric(chunk[4, 10])
  men_psqiBad <- as.numeric(chunk[4,9])
  women_psqiBadratio <- round((women_psqiBad / (women_psqiGood + women_psqiBad)),2)
  men_psqiBadratio <- round((men_psqiBad / (men_psqiGood + men_psqiBad)),2)
  data.frame(Auteur000Date = Auteur000Date, Type_Échantillon = label_text, Ville = city, Homme_Mauvais001Ratio = men_psqiBadratio, Femme_Mauvais001Ratio=women_psqiBadratio, Proportion_Femme = women_ratio, Age_Moyen000Écart = Age_Moyen000Écart, Taille_Échantillon = group_size)}

extract_worker_PC <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  label_text <-  as.character(chunk[2, 7])
  group_size <- as.numeric(chunk[2, 8])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  psqicut_Good <- ifelse(!is.na(psqi_number <- as.numeric(chunk[3,8])), psqi_number, NA)
  psqicut_Bad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 8])), psqi_number, NA)
  psqicut_Badratio <- round((psqicut_Bad / group_size), 2)
  women_psqicutBad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 10])), psqi_number, NA)
  women_psqicutGood<- ifelse(!is.na(psqi_number <- as.numeric(chunk[3, 10])), psqi_number, NA)
  men_psqicutBad <- ifelse(!is.na(psqi_number <- as.numeric(chunk[4, 9])), psqi_number, NA)
  men_psqicutGood <- ifelse(!is.na(psqi_number <- as.numeric(chunk[3, 9])), psqi_number, NA)
  women_psqicutBadratio <- round(women_psqicutBad /(women_psqicutBad+women_psqicutGood), 2)
  men_psqicutBadratio <- round(men_psqicutBad /(men_psqicutBad+men_psqicutGood), 2)
  women_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 10]) / group_size), as.numeric(chunk[2, 10]) / group_size, 0), 2)
  men_ratio <- round(ifelse(!is.na(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), 
                            as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 0), 2)
  data.frame(study_group = label_text, psqicut_Good = psqicut_Good, psqicut_Bad = psqicut_Bad, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqicutBad = women_psqicutBad, men_psqicutBad = men_psqicutBad, men_psqicutGood = men_psqicutGood, women_psqicutGood = women_psqicutGood, group_size = group_size, psqicut_Badratio = psqicut_Badratio, City = city, Year = year, Author = Author, Age_Mean = Age_Mean, Age_SD = Age_SD, women_psqicutBadratio=women_psqicutBadratio, men_psqicutBadratio=men_psqicutBadratio ) }

##Extract Student Data > use Worker Extract Function

##Extract Socio economic data
extract_soceco_PM <- function(chunk) {
  Author <- as.character(chunk[2,5])
  Age_Mean <- as.numeric(chunk[2,11])
  Age_SD <- as.numeric(chunk[2,12])
  year <- as.character(chunk[2,1])
  city <- as.character(chunk[2,3])
  label_text <-  as.character(chunk[2, 7])
  mean_value <- as.numeric(chunk[3, 8])
  sd_value <- as.numeric(chunk[4, 8])
  group_size <- as.numeric(chunk[2, 8])
  socecoL_size <- as.numeric(chunk[2, 45])
  socecoM_size <- as.numeric(chunk[2, 46])
  socecoH_size <- as.numeric(chunk[2, 47])
  socecoL_psqimean <- as.numeric(chunk[3, 45])
  socecoM_psqimean  <-  as.numeric(chunk[3, 46])
  socecoH_psqimean <- as.numeric(chunk[3, 47])
  socecoL_psqisd <- as.numeric(chunk[4, 45])
  socecoM_psqisd <- as.numeric(chunk[4, 46])
  socecoH_psqisd <- as.numeric(chunk[4, 47])
  lowerecoL_limit <- max(socecoL_psqimean - (1.96 * socecoL_psqisd), 0)
  upperecoL_limit <- socecoL_psqimean + (1.96 * socecoL_psqisd)
  lowerecoM_limit <- max(socecoM_psqimean - (1.96 * socecoM_psqisd), 0)
  upperecoM_limit <- socecoM_psqimean + (1.96 * socecoM_psqisd)
  lowerecoH_limit <- max(socecoH_psqimean - (1.96 * socecoH_psqisd), 0)
  upperecoH_limit <- socecoH_psqimean + (1.96 * socecoH_psqisd)
  women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 2)
  men_ratio <- round(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]),2)}
  
extract_soceco_PC <- function(chunk) {
    Author <- as.character(chunk[2,5])
    Age_Mean <- as.numeric(chunk[2,11])
    Age_SD <- as.numeric(chunk[2,12])
    year <- as.character(chunk[2,1])
    city <- as.character(chunk[2,3])
    label_text <-  as.character(chunk[2, 7])
    psqi_good <- as.numeric(chunk[3, 8])
    psqi_bad <- as.numeric(chunk[4, 8])
    group_size <- as.numeric(chunk[2, 8])
    socecoL_size <- as.numeric(chunk[2, 45])
    socecoM_size <- as.numeric(chunk[2, 46])
    socecoH_size <- as.numeric(chunk[2, 47])
    socecoL_psqigood <- as.numeric(chunk[3, 45])
    socecoM_psqigood  <- as.numeric(chunk[3, 46])
    socecoH_psqigood <- as.numeric(chunk[3, 47])
    socecoL_psqibad <- as.numeric(chunk[4, 45])
    socecoM_psqibad <- as.numeric(chunk[4, 46])
    socecoH_psqibad <-  as.numeric(chunk[4, 47])
    socecoL_psqibadRatio <- round((as.numeric(chunk[4, 45])/as.numeric(socecoL_psqigood+socecoL_psqibad)),2)
    socecoM_psqibadRatio <- round((as.numeric(chunk[4, 46])/as.numeric(socecoM_psqigood+socecoM_psqibad)),2)
    socecoH_psqibadRatio <-  round((as.numeric(chunk[4, 47])/as.numeric(socecoH_psqigood+socecoH_psqibad)),2)
women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]),2)
    men_ratio <- round(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]),2)
    data.frame(study_group = label_text, psqi_good = psqi_good,psqi_bad=psqi_bad, socecoL_size = socecoL_size,socecoM_size = socecoM_size, socecoH_size = socecoH_size, socecoL_psqigood = socecoL_psqigood, socecoM_psqigood = socecoM_psqigood, socecoH_psqigood = socecoH_psqigood, socecoL_psqibad = socecoL_psqibad, socecoM_psqibad = socecoM_psqibad, Women_Ratio = women_ratio, Men_Ratio = men_ratio,  group_size = group_size, City = city, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author, socecoL_psqibadRatio= socecoL_psqibadRatio, socecoM_psqibadRatio=socecoM_psqibadRatio, socecoH_psqibadRatio=socecoH_psqibadRatio)}  

  
##Extract Health Data - Actually Same with worker
extract_patient_PM <- function(chunk) {
    Author <- as.character(chunk[2,5])
    Age_Mean <- as.numeric(chunk[2,11])
    Age_SD <- as.numeric(chunk[2,12])
    year <- as.character(chunk[2,1])
    city <- as.character(chunk[2,3])
    label_text <-  as.character(chunk[2, 7])
    mean_value <- as.numeric(chunk[3, 8])
    sd_value <- as.numeric(chunk[4, 8])
    group_size <- as.numeric(chunk[2, 8])
    women_psqimean <-  as.numeric(chunk[3, 10])
    women_psqi_sd  <- as.numeric(chunk[4, 10])
    men_psqimean <- as.numeric(chunk[3, 9])
    men_psqi_sd <- as.numeric(chunk[4, 9])
    women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 2)
    men_ratio <- round(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]), 2)
    lower_limit <- max(mean_value - (1.96 * sd_value), 0)
    upper_limit <- mean_value + (1.96 * sd_value)
    lower_Womenlimit <- max(women_psqimean - (1.96 * women_psqi_sd), 0)
    upper_Womenlimit <- women_psqimean + (1.96 * women_psqi_sd)
    lower_Menlimit <- max(men_psqimean - (1.96 * men_psqi_sd), 0)
    upper_Menlimit <- men_psqimean + (1.96 * men_psqi_sd)
    data.frame(study_group = label_text, PSQI_Mean = mean_value, PSQI_SD = sd_value, lower = lower_limit, upper = upper_limit, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqimean = women_psqimean, men_psqimean = men_psqimean, group_size = group_size, City = city, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author,women_psqi_sd = women_psqi_sd, men_psqi_sd = men_psqi_sd,lower_Womenlimit = lower_Womenlimit, upper_Womenlimit= upper_Womenlimit, lower_Menlimit = lower_Menlimit, upper_Menlimit= upper_Menlimit  )}
  
extract_patient_PC <- function(chunk) {
    Author <- as.character(chunk[2,5])
    Age_Mean <- as.numeric(chunk[2,11])
    Age_SD <- as.numeric(chunk[2,12])
    label_text <-  as.character(chunk[2, 7])
    group_size <- as.numeric(chunk[2, 8])
    year <- as.character(chunk[2,1])
    city <- as.character(chunk[2,3])
    psqicut_Good <-  as.numeric(chunk[3,8])
    psqicut_Bad <-  as.numeric(chunk[4, 8])
    psqicut_Badratio <- round((psqicut_Bad / group_size), 2)
    women_psqicutBad <- as.numeric(chunk[4, 10])
    women_psqicutGood<- as.numeric(chunk[3, 10])
    men_psqicutBad <- as.numeric(chunk[4, 9])
    men_psqicutGood <-  as.numeric(chunk[3, 9])
    women_psqicutBadratio <- round(as.numeric(women_psqicutBad) /as.numeric((women_psqicutBad+women_psqicutGood)), 2)
    men_psqicutBadratio <- round(as.numeric(men_psqicutBad) /as.numeric((men_psqicutBad+men_psqicutGood)), 2)
    women_ratio <- round((as.numeric(chunk[2, 10]) / group_size), 2)
    men_ratio <- round((as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8])), 2)
    data.frame(study_group = label_text, psqicut_Good = psqicut_Good, psqicut_Bad = psqicut_Bad, Women_Ratio = women_ratio, Men_Ratio = men_ratio, women_psqicutBad = women_psqicutBad, men_psqicutBad = men_psqicutBad, men_psqicutGood = men_psqicutGood, women_psqicutGood = women_psqicutGood, group_size = group_size, psqicut_Badratio = psqicut_Badratio, City = city, Year = year, Author = Author, Age_Mean = Age_Mean, Age_SD = Age_SD, women_psqicutBadratio=women_psqicutBadratio, men_psqicutBadratio=men_psqicutBadratio ) }
  
#Extract Marriage Data
  
extract_marital_PM <- function(chunk) {
    Author <- as.character(chunk[2,5])
    Age_Mean <- as.numeric(chunk[2,11])
    Age_SD <- as.numeric(chunk[2,12])
    year <- as.character(chunk[2,1])
    city <- as.character(chunk[2,3])
    label_text <-  as.character(chunk[2, 7])
    mean_value <- as.numeric(chunk[3, 8])
    sd_value <- as.numeric(chunk[4, 8])
    group_size <- as.numeric(chunk[2, 8])
    married_size <- as.numeric(chunk[2, 40])
    single_size <- as.numeric(chunk[2, 41])
    divorced_size <- as.numeric(chunk[2, 42])
    married_psqimean <- as.numeric(chunk[3, 40])
    single_psqimean  <-  as.numeric(chunk[3, 41])
    divorced_psqimean <- as.numeric(chunk[3, 42])
    married_psqisd <- as.numeric(chunk[4, 40])
    single_psqisd <- as.numeric(chunk[4, 41])
    divorced_psqisd <- as.numeric(chunk[4, 42])
    lowermarried_limit <- max(married_psqimean - (1.96 * married_psqisd), 0)
    uppermarried_limit <- married_psqimean + (1.96 * married_psqisd)
    lowersingle_limit <- max(single_psqimean - (1.96 * single_psqisd), 0)
    uppersingle_limit <- single_psqimean + (1.96 * single_psqisd)
    lowerdivorced_limit <- max(divorced_psqimean - (1.96 * divorced_psqisd), 0)
    upperdivorced_limit <- divorced_psqimean + (1.96 * divorced_psqisd)
    women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]), 2)
    men_ratio <- round(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]),2)
    married_ratio <- round(as.numeric(married_size)/as.numeric(group_size), 2)
    single_ratio <- round(as.numeric(single_size)/as.numeric(group_size),2)
    divorced_ratio <- round(as.numeric(divorced_size)/as.numeric(group_size),2)
    data.frame(study_group = label_text, married_size = married_size,single_size = single_size, PSQI_Mean = mean_value, PSQI_SD =sd_value, divorced_size = divorced_size, married_psqimean = married_psqimean,lowermarried_limit=lowermarried_limit, lowersingle_limit=lowersingle_limit, lowerdivorced_limit=lowerdivorced_limit,uppermarried_limit=uppermarried_limit,uppersingle_limit=uppersingle_limit,upperdivorced_limit=upperdivorced_limit, single_psqimean = single_psqimean, divorced_psqimean = divorced_psqimean, married_psqisd = married_psqisd, single_psqisd = single_psqisd, Women_Ratio = women_ratio, Men_Ratio = men_ratio, divorced_psqisd = divorced_psqisd, group_size = group_size, City = city, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author, married_ratio=married_ratio,single_ratio=single_ratio, divorced_ratio=divorced_ratio )}
  
  
extract_marital_PC <- function(chunk) {
    Author <- as.character(chunk[2,5])
    Age_Mean <- as.numeric(chunk[2,11])
    Age_SD <- as.numeric(chunk[2,12])
    year <- as.character(chunk[2,1])
    city <- as.character(chunk[2,3])
    label_text <-  as.character(chunk[2, 7])
    psqi_good <- as.numeric(chunk[3, 8])
    psqi_bad <- as.numeric(chunk[4, 8])
    psqicut_Badratio <- round(as.numeric(psqi_bad)/as.numeric(psqi_bad+psqi_good), 2)
    group_size <- as.numeric(chunk[2, 8])
    married_size <- as.numeric(chunk[2, 40])
    single_size <- as.numeric(chunk[2, 41])
    divorced_size <- as.numeric(chunk[2, 42])
    married_psqigood <- as.numeric(chunk[3, 40])
    single_psqigood  <- as.numeric(chunk[3, 41])
    divorced_psqigood <- as.numeric(chunk[3, 42])
    married_psqibad <- as.numeric(chunk[4, 40])
    single_psqibad <- as.numeric(chunk[4, 41])
    divorced_psqibad <-  as.numeric(chunk[4, 42])
    married_psqibadRatio <- round((as.numeric(chunk[4, 40])/as.numeric(married_psqigood+married_psqibad)),2)
    single_psqibadRatio <- round((as.numeric(chunk[4, 41])/as.numeric(single_psqigood+single_psqibad)),2)
    divorced_psqibadRatio <-  round((as.numeric(chunk[4, 42])/as.numeric(divorced_psqigood+divorced_psqibad)),2)
    
    
    women_ratio <- round(as.numeric(chunk[2, 10]) / as.numeric(chunk[2, 8]),2)
    men_ratio <- round(as.numeric(chunk[2, 9]) / as.numeric(chunk[2, 8]),2)
    married_ratio <- round(as.numeric(married_size)/as.numeric(group_size),2)
    single_ratio <- round(as.numeric(single_size)/as.numeric(group_size),2)
    divorced_ratio <- round(as.numeric(divorced_size)/as.numeric(group_size),2)
    data.frame(study_group = label_text, psqi_good = psqi_good,psqi_bad=psqi_bad, married_size = married_size,single_size = single_size, divorced_size = divorced_size, married_psqigood = married_psqigood, single_psqigood = single_psqigood, divorced_psqigood = divorced_psqigood, married_psqibad = married_psqibad, single_psqibad = single_psqibad, Women_Ratio = women_ratio, Men_Ratio = men_ratio,  group_size = group_size, City = city, Year=year, Age_Mean = Age_Mean, Age_SD = Age_SD, Author = Author, married_psqibadRatio= married_psqibadRatio, single_psqibadRatio=single_psqibadRatio, divorced_psqibadRatio=divorced_psqibadRatio, married_ratio=married_ratio, single_ratio=single_ratio, divorced_ratio=divorced_ratio, divorced_psqibad=divorced_psqibad,psqicut_Badratio=psqicut_Badratio )}
  
  
#Calling all data into R

worker_PSQI_M <- read_excel(col_names = FALSE, "PSQI_M_Worker.xlsx")
  worker_PM_chunks <- split(worker_PSQI_M, rep(1:ceiling(nrow(worker_PSQI_M)/4), each = 4, length.out = nrow(worker_PSQI_M)))
  
worker_PM_nested <- tibble(chunk_id = names(worker_PM_chunks), data = worker_PM_chunks)
  
  worker_PM_list <- purrr::map_df(worker_PM_nested$data, extract_worker_PM)
  

worker_PSQI_C <- read_excel(col_names = FALSE, "PSQI_C_Worker.xlsx")
  
worker_PC_chunks <- split(worker_PSQI_C, rep(1:ceiling(nrow(worker_PSQI_C)/4), each = 4, length.out = nrow(worker_PSQI_C)))
  
worker_PC_nested <- tibble(chunk_id = names(worker_PC_chunks), data = worker_PC_chunks)
  
  
worker_PC_list <- purrr::map_df(worker_PC_nested$data, extract_worker_PC)  
  
##ShiftWorker
shiftworker_PSQI_M <- read_excel(col_names = FALSE, "PSQI_M_Shiftwork.xlsx")

shiftworker_PM_chunks <- split(shiftworker_PSQI_M, rep(1:ceiling(nrow(shiftworker_PSQI_M)/4), each = 4, length.out = nrow(shiftworker_PSQI_M)))

shiftworker_PM_nested <- tibble(chunk_id = names(shiftworker_PM_chunks), data = shiftworker_PM_chunks)

shiftworker_PM_list <- purrr::map_df(shiftworker_PM_nested$data, extract_shiftworker_PM)

shiftworker_PSQI_C <- read_excel(col_names = FALSE, "PSQI_C_Shiftwork.xlsx")

shiftworker_PC_chunks <- split(shiftworker_PSQI_C, rep(1:ceiling(nrow(shiftworker_PSQI_C)/4), each = 4, length.out = nrow(shiftworker_PSQI_C)))

shiftworker_PC_nested <- tibble(chunk_id = names(shiftworker_PC_chunks), data = shiftworker_PC_chunks)

shiftworker_PC_list <- purrr::map_df(shiftworker_PC_nested$data, extract_shiftworker_PC)

##Genre


worker_PM_listWomen <- worker_PM_list[order(worker_PM_list$Women_Ratio), ]

worker_PC_listWomen <- worker_PC_list[order(worker_PC_list$Women_Ratio), ]


##Marriage
  marital_PSQI_M <- read_excel(col_names = FALSE, "PSQI_M_Marital.xlsx")
  
  marital_PM_chunks <- split(marital_PSQI_M, rep(1:ceiling(nrow(marital_PSQI_M)/4), each = 4, length.out = nrow(marital_PSQI_M)))
  
  marital_PM_nested <- tibble(chunk_id = names(marital_PM_chunks), data = marital_PM_chunks)
  
  marital_PM_list <- purrr::map_df(marital_PM_nested$data, extract_marital_PM)
  

  marital_PSQI_C <- read_excel(col_names = FALSE, "PSQI_C_Marital.xlsx")
  
  marital_PC_chunks <- split(marital_PSQI_C, rep(1:ceiling(nrow(marital_PSQI_C)/4), each = 4, length.out = nrow(marital_PSQI_C)))
  
  marital_PC_nested <- tibble(chunk_id = names(marital_PC_chunks), data = marital_PC_chunks)
  
  marital_PC_list <- purrr::map_df(marital_PC_nested$data, extract_marital_PC)

#Worker Tables and Graphics

# Tables

##Worker
  worker_PM_list$Age_Moyen.Ecart <- paste(worker_PM_list$Age_Mean, "-", worker_PM_list$Age_SD)
  worker_PM_list$Auteur_Date <- paste(worker_PM_list$Author, " ", "(", worker_PM_list$Year,")")
  worker_PM_list$PSQI_MSD <- paste(worker_PM_list$PSQI_Mean, "-", worker_PM_list$PSQI_SD)
  
  
  worker_PM_Sim <- data.frame(Auteur000Date = worker_PM_list$Auteur_Date, Type_Échantillon = worker_PM_list$study_group, Ville = worker_PM_list$City, PSQI_Moyen000Écart = worker_PM_list$PSQI_MSD, Taille_Échantillon = worker_PM_list$group_size, Âge_Moyen000Écart = worker_PM_list$Age_Moyen.Ecart)
  
  names(worker_PM_Sim) <- gsub("_", "\n", names(worker_PM_Sim), fixed = TRUE)
  names(worker_PM_Sim) <- gsub("000", "-", names(worker_PM_Sim), fixed = TRUE)
  names(worker_PM_Sim) <- gsub("001", " ", names(worker_PM_Sim), fixed = TRUE)
  
  worker_PM_Sim <- worker_PM_Sim %>% mutate_all(~ str_replace_all(., "\\.", ","))
  
  
  png("00worker_PM_Sim.png", height = 60*nrow(worker_PM_Sim), width = 113*ncol(worker_PM_Sim))
  grid.table(worker_PM_Sim)
  dev.off()
  

worker_PC_list$Age_Moyen.Ecart <- paste(worker_PC_list$Age_Mean, "-", worker_PC_list$Age_SD)
worker_PC_list$Auteur_Date <- paste(worker_PC_list$Author, " ", "(", worker_PC_list$Year,")")
  
  
  
worker_PC_Sim <- data.frame(Auteur000Date = worker_PC_list$Auteur_Date, Type_Échantillon = worker_PC_list$study_group, Ville = worker_PC_list$City, PSQI001Seuil_Mauvais_Ratio = worker_PC_list$psqicut_Badratio, Taille_Group = worker_PC_list$group_size, Age_Moyen000Écart = worker_PC_list$Age_Moyen.Ecart)
  
  
  
  names(worker_PC_Sim) <- gsub("_", "\n", names(worker_PC_Sim), fixed = TRUE)
  names(worker_PC_Sim) <- gsub("000", "-", names(worker_PC_Sim), fixed = TRUE)
  names(worker_PC_Sim) <- gsub("001", " ", names(worker_PC_Sim), fixed = TRUE)
  
  
  worker_PC_Sim <- worker_PC_Sim %>% mutate_all(~ str_replace_all(., "\\.", ","))
  
  png("00worker_PC_Sim.png", height = 60*nrow(worker_PC_Sim), width = 113*ncol(worker_PC_Sim))
  grid.table(worker_PC_Sim)
  dev.off()

#Graphics

##Worker_PM Bar Plot PSQI_M vs Group Size
worker_PM_Bar <- ggplot(worker_PM_list, aes(x = reorder(study_group, PSQI_Mean), y = PSQI_Mean)) +
  geom_bar(stat='identity', position=position_dodge(0.1), linewidth =0.05, color ="black", fill="gray") + 
  geom_errorbar(aes(ymin=ifelse((PSQI_Mean - PSQI_SD)>0,(PSQI_Mean - PSQI_SD),0), ymax=(PSQI_Mean + PSQI_SD)))+
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 10, by = 0.5), labels=gsub("\\.", ",", seq(0,10, by=.5)))+
  geom_hline(yintercept = 5, linewidth=1.5, linetype = "dashed")+
  xlab("Type d'Échantillon") +
  ylab("PSQI Valeur Moyenne") +
  theme_bw(base_size = 28)+
  coord_flip()

print(worker_PM_Bar)

##worker_PC Bad Sleep Ratio 

worker_PC_Hist <- ggplot(worker_PC_list, aes(y=reorder(study_group,psqicut_Badratio), x=psqicut_Badratio)) +
  geom_bar(stat="identity", position ="dodge",color ="black", fill="gray") + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),labels=gsub("\\.", ",", seq(0,1, by=.1)))+ 
  #geom_text_repel(aes(label = group_size), size=6, max.overlaps = Inf,show.legend = FALSE)+ 
  theme_bw(base_size = 28)+ 
  xlab("PSQI >5 Ratio") +
  ylab("Type d'Échantillon") 

print(worker_PC_Hist)


##Shiftworker Graphs

###Reorganize data for subgrouped forest plot

shiftworkyes_PMdata <- shiftworker_PM_list[, c("Auteur_Date", "study_group", "City", "Age_Moyen.Ecart","group_size", "PSQI_Mean", "PSQI_SD", "shiftyes_psqimean", "shiftyes_psqi_sd", "lower_shiftyeslimit", "upper_shiftyeslimit", "Women_Ratio", "shiftworkyes_ratio")]
shiftworkno_PMdata <- shiftworker_PM_list[, c("Auteur_Date", "study_group", "City", "Age_Moyen.Ecart","group_size","PSQI_Mean", "PSQI_SD", "shiftno_psqimean", "shiftno_psqi_sd", "lower_shiftnolimit", "upper_shiftnolimit", "Women_Ratio", "shiftworkyes_ratio")]
shiftworkyes_PMdata$group <- "Shiftwork_Yes"
shiftworkno_PMdata$group <- "Shiftwork_No" 

colnames(shiftworkyes_PMdata) <- c("Auteur_Date", "study_group", "City", "Age_Moyen.Ecart","group_size","PSQI_Mean", "PSQI_SD", "shiftworker_psqimean", "shiftworker_psqi_sd", "lower", "upper", "Women_Ratio", "shiftworkyes_ratio","Work_Group")
colnames(shiftworkno_PMdata) <- c("Auteur_Date", "study_group", "City", "Age_Moyen.Ecart","group_size","PSQI_Mean", "PSQI_SD", "shiftworker_psqimean", "shiftworker_psqi_sd", "lower", "upper", "Women_Ratio", "shiftworkyes_ratio","Work_Group")
combined_shift_PM <- rbind(shiftworkyes_PMdata, shiftworkno_PMdata)

grouped_shift_PM <- combined_shift_PM %>% group_by(Work_Group)


##Forest plot with Shift Yes-No subgroups
shiftworker_PM_forest <- forestplot(grouped_shift_PM,
                                    legend = c("Travailleur Posté", "Travailleur Non-Posté"),
                                    labeltext = c(Auteur_Date, study_group, Age_Moyen.Ecart, group_size),
                                    boxsize = 0.2,
                                    vertices = TRUE,
                                    shapes_gp = fpShapesGp(default = gpar(lwd = 1.5)),
                                    mean = shiftworker_psqimean,
                                    ci.lb = grouped_shift_PM$lower, 
                                    ci.ub = grouped_shift_PM$upper,
                                    xlab = "PSQI Valeur Moyenne",
                                    colgap = unit(8,"mm"),
                                    grid = structure(c(5),
                                                     gp = gpar(col = "steelblue", lty = 2)
                                    ),
                                    line.margin = .1,
                                    is.summary = c(F))|> 
  fp_add_header(Auteur_Date = c("", "Auteur-Date"),
                study_group = c("", "Type\nd'Échantillon"),
                Age_Moyen.Ecart = c("", "Âge\nMoyen-Écart"),
                group_size = c("", "Taille\nd'Échantillon"))|> 
  fp_set_style(txt_gp= fpTxtGp(summary=gpar(cex=1), legend = gpar(cex=1.2)) ,box = c("gray", "black"))|> 
  fp_set_zebra_style("#EFEFEF")
print(shiftworker_PM_forest)
ggsave("0shiftworker_PM_forest.png", width = 40, height = 25, units = "cm", plot = last_plot(), dpi = 820)



##Group Shiftwork_PC same for dodged bar chart.

shiftworker_PC_histList <- shiftworker_PC_list %>%
  select(study_group, shiftno_psqicutBadRatio, shiftyes_psqicutBadRatio) %>%
  tidyr::gather(type, psqicutbadratio, -study_group)
shiftworker_PC_histList <- mutate_all(shiftworker_PC_histList, ~replace_na(.,0))

##Histogram of Shiftwork PSQI-Cut Yes/No Groups by ratio
shiftworker_PC_Hist<- ggplot(shiftworker_PC_histList, aes(x = study_group, y = psqicutbadratio, group = type, fill = type)) + 
  scale_x_discrete(drop=F)+
  scale_fill_grey(name = "Temps de Travail:", labels=c(' Travailleurs Non Postés', 'Travailleurs Postés'))+
  geom_bar(stat = 'identity', position = 'dodge', color="black") +  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+ 
  xlab("Type d'Échantillon") + ylab("PSQI >5 Ratio")+
  theme_bw(base_size=24) +
  theme(legend.spacing = unit(5, 'cm'),legend.position="top" )
  

print(shiftworker_PC_Hist)
ggsave("00shiftworker_PC_Hist.png", width = 45, height = 20, units = "cm", plot = last_plot(), dpi = 820)

