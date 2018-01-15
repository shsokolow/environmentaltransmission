##############################################
# Murray et al. data cleaning code for SNAPP:
# Andy MacDonald; updated 12/4/17
##############################################

rm(list=ls())

require(readxl)
require(plyr)
require(doBy)
require(data.table)
require(tidyr)
require(dplyr)

getwd()
setwd("/Users/shsokolow/Documents/Github/environmentaltransmission")
setwd("/Users/shsokolow/Documents/rdataforenvtrans")

########################
###### Murray data:
########################
Murray <- read.csv("ptrsb_murray_data_with05.csv", header=T)
head(Murray)
tail(Murray)
nrow(Murray)

########################
##### big dataset:
########################
######### Yellow Fever
YF05 <- read.csv("IHME_GBD_2010_RESULTS_2005_YELLOW_FEVER_Y2013M11D15.csv", header=T)

YF90 <- read.csv("IHME_GBD_2010_RESULTS_1990_YELLOW_FEVER_Y2013M11D15.csv", header=T)

YF10 <- read.csv("IHME_GBD_2010_RESULTS_2010_YELLOW_FEVER_Y2013M11D15.csv", header=T)

YF = rbind(YF90,YF05,YF10)
YF$Envtrans <-'1'
YF$Big3 <-'no'
head(YF)
tail(YF)
YF$causelevel1 <- NULL #remove unnecessary columns
YF$causelevel2 <- NULL
YF$causelevel3 <- NULL
YF$causelevel5 <- NULL

YF_subset <- YF[which(YF$age_name=='Under 5 years'|YF$age_name=='5-14 years'|YF$age_name=='15-49 years'|YF$age_name=='50-69 years'| YF$age_name=='70+ years' & YF$sex=='Both sexes' & YF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(YF_subset)
nrow(YF_subset)

######### Schisto
Schisto05 <- read.csv("IHME_GBD_2010_RESULTS_2005_SCHISTOSOMIASIS_Y2013M11D15.csv", header=T)

Schisto90 <- read.csv("IHME_GBD_2010_RESULTS_1990_SCHISTOSOMIASIS_Y2013M11D15.csv", header=T)

Schisto10 <- read.csv("IHME_GBD_2010_RESULTS_2010_SCHISTOSOMIASIS_Y2013M11D15.csv", header=T)

Schisto = rbind(Schisto90,Schisto05,Schisto10)

head(Schisto)
tail(Schisto)
Schisto$Envtrans <-'1'
Schisto$Big3 <-'no'
Schisto$causelevel1 <- NULL #remove unnecessary columns
Schisto$causelevel2 <- NULL
Schisto$causelevel3 <- NULL
Schisto$causelevel5 <- NULL
Schisto_subset <- Schisto[which(Schisto$age_name=='Under 5 years'|Schisto$age_name=='5-14 years'|Schisto$age_name=='15-49 years'|Schisto$age_name=='50-69 years'| Schisto$age_name=='70+ years' & Schisto$sex=='Both sexes' & Schisto$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Schisto_subset)
nrow(Schisto_subset)

######### Rabies
Rabies05 <- read.csv("IHME_GBD_2010_RESULTS_2005_RABIES_Y2013M11D15.csv", header=T)

Rabies90 <- read.csv("IHME_GBD_2010_RESULTS_1990_RABIES_Y2013M11D15.csv", header=T)

Rabies10 <- read.csv("IHME_GBD_2010_RESULTS_2010_RABIES_Y2013M11D15.csv", header=T)

Rabies = rbind(Rabies90,Rabies05,Rabies10)

head(Rabies)
tail(Rabies)
Rabies$Envtrans <-'1'
Rabies$Big3 <-'no'
Rabies$causelevel1 <- NULL #remove unnecessary columns
Rabies$causelevel2 <- NULL
Rabies$causelevel3 <- NULL
Rabies$causelevel5 <- NULL
Rabies_subset <- Rabies[which(Rabies$age_name=='Under 5 years'|Rabies$age_name=='5-14 years'|Rabies$age_name=='15-49 years'|Rabies$age_name=='50-69 years'| Rabies$age_name=='70+ years' & Rabies$sex=='Both sexes' & Rabies$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Rabies_subset)
nrow(Rabies_subset)


######### Onchocerciasis
Onch05 <- read.csv("IHME_GBD_2010_RESULTS_2005_ONCHOCERCIASIS_Y2013M11D15.csv", header=T)

Onch90 <- read.csv("IHME_GBD_2010_RESULTS_1990_ONCHOCERCIASIS_Y2013M11D15.csv", header=T)

Onch10 <- read.csv("IHME_GBD_2010_RESULTS_2010_ONCHOCERCIASIS_Y2013M11D15.csv", header=T)

Onch = rbind(Onch90,Onch05,Onch10)

head(Onch)
tail(Onch)
Onch$Envtrans <-'1'
Onch$Big3 <-'no'
Onch$causelevel1 <- NULL #remove unnecessary columns
Onch$causelevel2 <- NULL
Onch$causelevel3 <- NULL
Onch$causelevel5 <- NULL
Onch_subset <- Onch[which(Onch$age_name=='Under 5 years'|Onch$age_name=='5-14 years'|Onch$age_name=='15-49 years'|Onch$age_name=='50-69 years'| Onch$age_name=='70+ years' & Onch$sex=='Both sexes' & Onch$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Onch_subset)
nrow(Onch_subset)


######### malaria
Mal05 <- read.csv("IHME_GBD_2010_RESULTS_2005_MALARIA_Y2013M11D15.csv", header=T)

Mal90 <- read.csv("IHME_GBD_2010_RESULTS_1990_MALARIA_Y2013M11D15.csv", header=T)

Mal10 <- read.csv("IHME_GBD_2010_RESULTS_2010_MALARIA_Y2013M11D15.csv", header=T)

Mal = rbind(Mal90,Mal05,Mal10)

head(Mal)
tail(Mal)
Mal$Envtrans <-'1'
Mal$Big3 <-'yes'
Mal$causelevel1 <- NULL #remove unnecessary columns
Mal$causelevel2 <- NULL
Mal$causelevel3 <- NULL
Mal$causelevel5 <- NULL
Mal_subset <- Mal[which(Mal$age_name=='Under 5 years'|Mal$age_name=='5-14 years'|Mal$age_name=='15-49 years'|Mal$age_name=='50-69 years'| Mal$age_name=='70+ years' & Mal$sex=='Both sexes' & Mal$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Mal_subset)
nrow(Mal_subset)

######### lymphatic filariasis
LF05 <- read.csv("IHME_GBD_2010_RESULTS_2005_LYMPHATIC_FILARIASIS_Y2013M11D15.csv", header=T)

LF90 <- read.csv("IHME_GBD_2010_RESULTS_1990_LYMPHATIC_FILARIASIS_Y2013M11D15.csv", header=T)

LF10 <- read.csv("IHME_GBD_2010_RESULTS_2010_LYMPHATIC_FILARIASIS_Y2013M11D15.csv", header=T)

LF = rbind(LF90,LF05,LF10)

head(LF)
tail(LF)
LF$Envtrans <-'1'
LF$Big3 <-'no'
LF$causelevel1 <- NULL #remove unnecessary columns
LF$causelevel2 <- NULL
LF$causelevel3 <- NULL
LF$causelevel5 <- NULL
LF_subset <- LF[which(LF$age_name=='Under 5 years'|LF$age_name=='5-14 years'|LF$age_name=='15-49 years'|LF$age_name=='50-69 years'| LF$age_name=='70+ years' & LF$sex=='Both sexes' & LF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(LF_subset)
nrow(LF_subset)



######### leishmaniasis
Leish05 <- read.csv("IHME_GBD_2010_RESULTS_2005_LEISHMANIASIS_Y2013M11D15.csv", header=T)

Leish90 <- read.csv("IHME_GBD_2010_RESULTS_1990_LEISHMANIASIS_Y2013M11D15.csv", header=T)

Leish10 <- read.csv("IHME_GBD_2010_RESULTS_2010_LEISHMANIASIS_Y2013M11D15.csv", header=T)

Leish = rbind(Leish90,Leish05,Leish10)

head(Leish)
tail(Leish)
Leish$Envtrans <-'0'
Leish$Big3 <-'no'
Leish$causelevel1 <- NULL #remove unnecessary columns
Leish$causelevel2 <- NULL
Leish$causelevel3 <- NULL
Leish$causelevel5 <- NULL
Leish_subset <- Leish[which(Leish$age_name=='Under 5 years'|Leish$age_name=='5-14 years'|Leish$age_name=='15-49 years'|Leish$age_name=='50-69 years'| Leish$age_name=='70+ years' & Leish$sex=='Both sexes' & Leish$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Leish_subset)
nrow(Leish_subset)


######### hookworm
HW05 <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

HW90 <- read.csv("IHME_GBD_2010_RESULTS_1990_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

HW10 <- read.csv("IHME_GBD_2010_RESULTS_2010_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

HW = rbind(HW90,HW05,HW10)

head(HW)
tail(HW)
HW$Envtrans <-'1'
HW$Big3 <-'no'
HW$causelevel1 <- NULL #remove unnecessary columns
HW$causelevel2 <- NULL
HW$causelevel3 <- NULL
HW$causelevel5 <- NULL
HW_subset <- HW[which(HW$age_name=='Under 5 years'|HW$age_name=='5-14 years'|HW$age_name=='15-49 years'|HW$age_name=='50-69 years'| HW$age_name=='70+ years' & HW$sex=='Both sexes' & HW$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HW_subset)
nrow(HW_subset)


######### trichuriasis
Trich05 <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Trich90 <- read.csv("IHME_GBD_2010_RESULTS_1990_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Trich10 <- read.csv("IHME_GBD_2010_RESULTS_2010_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Trich = rbind(Trich90,Trich05,Trich10)

head(Trich)
tail(Trich)
Trich$Envtrans <-'1'
Trich$Big3 <-'no'
Trich$causelevel1 <- NULL #remove unnecessary columns
Trich$causelevel2 <- NULL
Trich$causelevel3 <- NULL
Trich$causelevel5 <- NULL
Trich_subset <- Trich[which(Trich$age_name=='Under 5 years'|Trich$age_name=='5-14 years'|Trich$age_name=='15-49 years'|Trich$age_name=='50-69 years'| Trich$age_name=='70+ years' & Trich$sex=='Both sexes' & Trich$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trich_subset)
nrow(Trich_subset)


######### Ascariasis
Asc05 <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Asc90 <- read.csv("IHME_GBD_2010_RESULTS_1990_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Asc10 <- read.csv("IHME_GBD_2010_RESULTS_2010_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)

Asc = rbind(Asc90,Asc05,Asc10)

head(Asc)
tail(Asc)
Asc$Envtrans <-'1'
Asc$Big3 <-'no'
Asc$causelevel1 <- NULL #remove unnecessary columns
Asc$causelevel2 <- NULL
Asc$causelevel3 <- NULL
Asc$causelevel5 <- NULL
Asc_subset <- Asc[which(Asc$age_name=='Under 5 years'|Asc$age_name=='5-14 years'|Asc$age_name=='15-49 years'|Asc$age_name=='50-69 years'| Asc$age_name=='70+ years' & Asc$sex=='Both sexes' & Asc$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Asc_subset)
nrow(Asc_subset)


######### food-borne trematodiasis
FBT05 <- read.csv("IHME_GBD_2010_RESULTS_2005_FOOD-BORNE_TREMATODIASES_Y2013M11D15.csv", header=T)

FBT90 <- read.csv("IHME_GBD_2010_RESULTS_1990_FOOD-BORNE_TREMATODIASES_Y2013M11D15.csv", header=T)

FBT10 <- read.csv("IHME_GBD_2010_RESULTS_2010_FOOD-BORNE_TREMATODIASES_Y2013M11D15.csv", header=T)

FBT = rbind(FBT90,FBT05,FBT10)

head(FBT)
tail(FBT)
FBT$Envtrans <-'1'
FBT$Big3 <-'no'
FBT$causelevel1 <- NULL #remove unnecessary columns
FBT$causelevel2 <- NULL
FBT$causelevel3 <- NULL
FBT$causelevel5 <- NULL
FBT_subset <- FBT[which(FBT$age_name=='Under 5 years'|FBT$age_name=='5-14 years'|FBT$age_name=='15-49 years'|FBT$age_name=='50-69 years'| FBT$age_name=='70+ years' & FBT$sex=='Both sexes' & FBT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(FBT_subset)
nrow(FBT_subset)



######### echinococcus
Ech05 <- read.csv("IHME_GBD_2010_RESULTS_2005_ECHINOCOCCOSIS_Y2013M11D15.csv", header=T)

Ech90 <- read.csv("IHME_GBD_2010_RESULTS_1990_ECHINOCOCCOSIS_Y2013M11D15.csv", header=T)

Ech10 <- read.csv("IHME_GBD_2010_RESULTS_2010_ECHINOCOCCOSIS_Y2013M11D15.csv", header=T)

Ech = rbind(Ech90,Ech05,Ech10)

head(Ech)
tail(Ech)
Ech$Envtrans <-'1'
Ech$Big3 <-'no'
Ech$causelevel1 <- NULL #remove unnecessary columns
Ech$causelevel2 <- NULL
Ech$causelevel3 <- NULL
Ech$causelevel5 <- NULL
Ech_subset <- Ech[which(Ech$age_name=='Under 5 years'|Ech$age_name=='5-14 years'|Ech$age_name=='15-49 years'|Ech$age_name=='50-69 years'| Ech$age_name=='70+ years' & Ech$sex=='Both sexes' & Ech$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Ech_subset)
nrow(Ech_subset)



######### dengue
Den05 <- read.csv("IHME_GBD_2010_RESULTS_2005_DENGUE_Y2013M11D15.csv", header=T)

Den90 <- read.csv("IHME_GBD_2010_RESULTS_1990_DENGUE_Y2013M11D15.csv", header=T)

Den10 <- read.csv("IHME_GBD_2010_RESULTS_2010_DENGUE_Y2013M11D15.csv", header=T)

Den = rbind(Den90,Den05,Den10)

head(Den)
tail(Den)
Den$Envtrans <-'1'
Den$Big3 <-'no'
Den$causelevel1 <- NULL #remove unnecessary columns
Den$causelevel2 <- NULL
Den$causelevel3 <- NULL
Den$causelevel5 <- NULL
Den_subset <- Den[which(Den$age_name=='Under 5 years'|Den$age_name=='5-14 years'|Den$age_name=='15-49 years'|Den$age_name=='50-69 years'| Den$age_name=='70+ years' & Den$sex=='Both sexes' & Den$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Den_subset)
nrow(Den_subset)

######### cysticercosis
Cyst05 <- read.csv("IHME_GBD_2010_RESULTS_2005_CYSTICERCOSIS_Y2013M11D15.csv", header=T)

Cyst90 <- read.csv("IHME_GBD_2010_RESULTS_1990_CYSTICERCOSIS_Y2013M11D15.csv", header=T)

Cyst10 <- read.csv("IHME_GBD_2010_RESULTS_2010_CYSTICERCOSIS_Y2013M11D15.csv", header=T)

Cyst = rbind(Cyst90,Cyst05,Cyst10)

head(Cyst)
tail(Cyst)
Cyst$Envtrans <-'1'
Cyst$Big3 <-'no'
Cyst$causelevel1 <- NULL #remove unnecessary columns
Cyst$causelevel2 <- NULL
Cyst$causelevel3 <- NULL
Cyst$causelevel5 <- NULL
Cyst_subset <- Cyst[which(Cyst$age_name=='Under 5 years'|Cyst$age_name=='5-14 years'|Cyst$age_name=='15-49 years'|Cyst$age_name=='50-69 years'| Cyst$age_name=='70+ years' & Cyst$sex=='Both sexes' & Cyst$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Cyst_subset)
nrow(Cyst_subset)



######### chagas
Chagas05 <- read.csv("IHME_GBD_2010_RESULTS_2005_CHAGAS_DISEASE_Y2013M11D15.csv", header=T)

Chagas90 <- read.csv("IHME_GBD_2010_RESULTS_1990_CHAGAS_DISEASE_Y2013M11D15.csv", header=T)

Chagas10 <- read.csv("IHME_GBD_2010_RESULTS_2010_CHAGAS_DISEASE_Y2013M11D15.csv", header=T)

Chagas = rbind(Chagas90,Chagas05,Chagas10)

head(Chagas)
tail(Chagas)
Chagas$Envtrans <-'1'
Chagas$Big3 <-'no'
Chagas$causelevel1 <- NULL #remove unnecessary columns
Chagas$causelevel2 <- NULL
Chagas$causelevel3 <- NULL
Chagas$causelevel5 <- NULL
Chagas_subset <- Chagas[which(Chagas$age_name=='Under 5 years'|Chagas$age_name=='5-14 years'|Chagas$age_name=='15-49 years'|Chagas$age_name=='50-69 years'| Chagas$age_name=='70+ years' & Chagas$sex=='Both sexes' & Chagas$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Chagas_subset)
nrow(Chagas_subset)

######### african trypanosomiasis
AT05 <- read.csv("IHME_GBD_2010_RESULTS_2005_AFRICAN_TRYPANOSOMIASIS_Y2013M11D15.csv", header=T)

AT90 <- read.csv("IHME_GBD_2010_RESULTS_1990_AFRICAN_TRYPANOSOMIASIS_Y2013M11D15.csv", header=T)

AT10 <- read.csv("IHME_GBD_2010_RESULTS_2010_AFRICAN_TRYPANOSOMIASIS_Y2013M11D15.csv", header=T)

AT = rbind(AT90,AT05,AT10)

head(AT)
tail(AT)
AT$Envtrans <-'1'
AT$Big3 <-'no'
AT$causelevel1 <- NULL #remove unnecessary columns
AT$causelevel2 <- NULL
AT$causelevel3 <- NULL
AT$causelevel5 <- NULL
AT_subset <- AT[which(AT$age_name=='Under 5 years'|AT$age_name=='5-14 years'|AT$age_name=='15-49 years'|AT$age_name=='50-69 years'| AT$age_name=='70+ years' & AT$sex=='Both sexes' & AT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(AT_subset)
nrow(AT_subset)

######### Hep A
HepA05 <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)

HepA90 <- read.csv("IHME_GBD_2010_RESULTS_1990_HEPATITIS_Y2013M11D15.csv", header=T)

HepA10 <- read.csv("IHME_GBD_2010_RESULTS_2010_HEPATITIS_Y2013M11D15.csv", header=T)

HepA = rbind(HepA90,HepA05,HepA10)

head(HepA)
tail(HepA)
HepA$Envtrans <-'0'
HepA$Big3 <-'no'
HepA$causelevel1 <- NULL #remove unnecessary columns
HepA$causelevel2 <- NULL
HepA$causelevel3 <- NULL
HepA$causelevel5 <- NULL
HepA_subset <- HepA[which(HepA$age_name=='Under 5 years'|HepA$age_name=='5-14 years'|HepA$age_name=='15-49 years'|HepA$age_name=='50-69 years'| HepA$age_name=='70+ years' & HepA$sex=='Both sexes' & HepA$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepA_subset)
nrow(HepA_subset)

######### Hep B
HepB05 <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)

HepB90 <- read.csv("IHME_GBD_2010_RESULTS_1990_HEPATITIS_Y2013M11D15.csv", header=T)

HepB10 <- read.csv("IHME_GBD_2010_RESULTS_2010_HEPATITIS_Y2013M11D15.csv", header=T)

HepB = rbind(HepB90,HepB05,HepB10)

head(HepB)
tail(HepB)
HepB$Envtrans <-'1'
HepB$Big3 <-'no'
HepB$causelevel1 <- NULL #remove unnecessary columns
HepB$causelevel2 <- NULL
HepB$causelevel3 <- NULL
HepB$causelevel5 <- NULL
HepB_subset <- HepB[which(HepB$age_name=='Under 5 years'|HepB$age_name=='5-14 years'|HepB$age_name=='15-49 years'|HepB$age_name=='50-69 years'| HepB$age_name=='70+ years' & HepB$sex=='Both sexes' & HepB$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepB_subset)
nrow(HepB_subset)


######### Hep C
HepC05 <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)

HepC90 <- read.csv("IHME_GBD_2010_RESULTS_1990_HEPATITIS_Y2013M11D15.csv", header=T)

HepC10 <- read.csv("IHME_GBD_2010_RESULTS_2010_HEPATITIS_Y2013M11D15.csv", header=T)

HepC = rbind(HepC90,HepC05,HepC10)

head(HepC)
tail(HepC)
HepC$Envtrans <-'0'
HepC$Big3 <-'no'
HepC$causelevel1 <- NULL #remove unnecessary columns
HepC$causelevel2 <- NULL
HepC$causelevel3 <- NULL
HepC$causelevel5 <- NULL
HepC_subset <- HepC[which(HepC$age_name=='Under 5 years'|HepC$age_name=='5-14 years'|HepC$age_name=='15-49 years'|HepC$age_name=='50-69 years'| HepC$age_name=='70+ years' & HepC$sex=='Both sexes' & HepC$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepC_subset)
nrow(HepC_subset)



######### HIV
HIV05 <- read.csv("IHME_GBD_2010_RESULTS_2005_HIV_AIDS_Y2013M11D15.csv", header=T)

HIV90 <- read.csv("IHME_GBD_2010_RESULTS_1990_HIV_AIDS_Y2013M11D15.csv", header=T)

HIV10 <- read.csv("IHME_GBD_2010_RESULTS_2010_HIV_AIDS_Y2013M11D15.csv", header=T)

HIV = rbind(HIV90,HIV05,HIV10)

head(HIV)
tail(HIV)
HIV$Envtrans <-'0'
HIV$Big3 <-'yes'
HIV$causelevel1 <- NULL #remove unnecessary columns
HIV$causelevel2 <- NULL
HIV$causelevel3 <- NULL
HIV$causelevel5 <- NULL
HIV_subset <- HIV[which(HIV$age_name=='Under 5 years'|HIV$age_name=='5-14 years'|HIV$age_name=='15-49 years'|HIV$age_name=='50-69 years'| HIV$age_name=='70+ years' & HIV$sex=='Both sexes' & HIV$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HIV_subset)
nrow(HIV_subset)

######### whooping cough
Whoop05 <- read.csv("IHME_GBD_2010_RESULTS_2005_WHOOPING_COUGH_Y2013M11D15.csv", header=T)

Whoop90 <- read.csv("IHME_GBD_2010_RESULTS_1990_WHOOPING_COUGH_Y2013M11D15.csv", header=T)

Whoop10 <- read.csv("IHME_GBD_2010_RESULTS_2010_WHOOPING_COUGH_Y2013M11D15.csv", header=T)

Whoop = rbind(Whoop90,Whoop05,Whoop10)

head(Whoop)
tail(Whoop)
Whoop$Envtrans <-'0'
Whoop$Big3 <-'no'
Whoop$causelevel1 <- NULL #remove unnecessary columns
Whoop$causelevel2 <- NULL
Whoop$causelevel3 <- NULL
Whoop$causelevel5 <- NULL
Whoop_subset <- Whoop[which(Whoop$age_name=='Under 5 years'|Whoop$age_name=='5-14 years'|Whoop$age_name=='15-49 years'|Whoop$age_name=='50-69 years'| Whoop$age_name=='70+ years' & Whoop$sex=='Both sexes' & Whoop$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Whoop_subset)
nrow(Whoop_subset)


######### varicella
Var05 <- read.csv("IHME_GBD_2010_RESULTS_2005_VARICELLA_Y2013M11D15.csv", header=T)

Var90 <- read.csv("IHME_GBD_2010_RESULTS_1990_VARICELLA_Y2013M11D15.csv", header=T)

Var10 <- read.csv("IHME_GBD_2010_RESULTS_2010_VARICELLA_Y2013M11D15.csv", header=T)

Var = rbind(Var90,Var05,Var10)

head(Var)
tail(Var)
Var$Envtrans <-'0'
Var$Big3 <-'no'
Var$causelevel1 <- NULL #remove unnecessary columns
Var$causelevel2 <- NULL
Var$causelevel3 <- NULL
Var$causelevel5 <- NULL
Var_subset <- Var[which(Var$age_name=='Under 5 years'|Var$age_name=='5-14 years'|Var$age_name=='15-49 years'|Var$age_name=='50-69 years'| Var$age_name=='70+ years' & Var$sex=='Both sexes' & Var$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Var_subset)
nrow(Var_subset)

######### typhoid
Typh05 <- read.csv("IHME_GBD_2010_RESULTS_2005_TYPHOID_AND_PARATYPHOID_FEVERS_Y2013M11D15.csv", header=T)

Typh90 <- read.csv("IHME_GBD_2010_RESULTS_1990_TYPHOID_AND_PARATYPHOID_FEVERS_Y2013M11D15.csv", header=T)

Typh10 <- read.csv("IHME_GBD_2010_RESULTS_2010_TYPHOID_AND_PARATYPHOID_FEVERS_Y2013M11D15.csv", header=T)

Typh = rbind(Typh90,Typh05,Typh10)

head(Typh)
tail(Typh)
Typh$Envtrans <-'1'
Typh$Big3 <-'no'
Typh$causelevel1 <- NULL #remove unnecessary columns
Typh$causelevel2 <- NULL
Typh$causelevel3 <- NULL
Typh$causelevel5 <- NULL
Typh_subset <- Typh[which(Typh$age_name=='Under 5 years'|Typh$age_name=='5-14 years'|Typh$age_name=='15-49 years'|Typh$age_name=='50-69 years'| Typh$age_name=='70+ years' & Typh$sex=='Both sexes' & Typh$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Typh_subset)
nrow(Typh_subset)


######### tuberculosis
Tub05 <- read.csv("IHME_GBD_2010_RESULTS_2005_TUBERCULOSIS_Y2013M11D15.csv", header=T)

Tub90 <- read.csv("IHME_GBD_2010_RESULTS_1990_TUBERCULOSIS_Y2013M11D15.csv", header=T)

Tub10 <- read.csv("IHME_GBD_2010_RESULTS_2010_TUBERCULOSIS_Y2013M11D15.csv", header=T)

Tub = rbind(Tub90,Tub05,Tub10)

head(Tub)
tail(Tub)
Tub$Envtrans <-'0'
Tub$Big3 <-'yes'
Tub$causelevel1 <- NULL #remove unnecessary columns
Tub$causelevel2 <- NULL
Tub$causelevel3 <- NULL
Tub$causelevel5 <- NULL
Tub_subset <- Tub[which(Tub$age_name=='Under 5 years'|Tub$age_name=='5-14 years'|Tub$age_name=='15-49 years'|Tub$age_name=='50-69 years'| Tub$age_name=='70+ years' & Tub$sex=='Both sexes' & Tub$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Tub_subset)
nrow(Tub_subset)

######### trachoma
Trach05 <- read.csv("IHME_GBD_2010_RESULTS_2005_TRACHOMA_Y2013M11D15.csv", header=T)

Trach90 <- read.csv("IHME_GBD_2010_RESULTS_1990_TRACHOMA_Y2013M11D15.csv", header=T)

Trach10 <- read.csv("IHME_GBD_2010_RESULTS_2010_TRACHOMA_Y2013M11D15.csv", header=T)

Trach = rbind(Trach90,Trach05,Trach10)

head(Trach)
tail(Trach)
Trach$Envtrans <-'1'
Trach$Big3 <-'no'
Trach$causelevel1 <- NULL #remove unnecessary columns
Trach$causelevel2 <- NULL
Trach$causelevel3 <- NULL
Trach$causelevel5 <- NULL
Trach_subset <- Trach[which(Trach$age_name=='Under 5 years'|Trach$age_name=='5-14 years'|Trach$age_name=='15-49 years'|Trach$age_name=='50-69 years'| Trach$age_name=='70+ years' & Trach$sex=='Both sexes' & Trach$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trach_subset)
nrow(Trach_subset)


######### measles
Measles05 <- read.csv("IHME_GBD_2010_RESULTS_2005_MEASLES_Y2013M11D15.csv", header=T)

Measles90 <- read.csv("IHME_GBD_2010_RESULTS_1990_MEASLES_Y2013M11D15.csv", header=T)

Measles10 <- read.csv("IHME_GBD_2010_RESULTS_2010_MEASLES_Y2013M11D15.csv", header=T)

Measles = rbind(Measles90,Measles05,Measles10)

head(Measles)
tail(Measles)
Measles$Envtrans <-'0'
Measles$Big3 <-'no'
Measles$causelevel1 <- NULL #remove unnecessary columns
Measles$causelevel2 <- NULL
Measles$causelevel3 <- NULL
Measles$causelevel5 <- NULL
Measles_subset <- Measles[which(Measles$age_name=='Under 5 years'|Measles$age_name=='5-14 years'|Measles$age_name=='15-49 years'|Measles$age_name=='50-69 years'| Measles$age_name=='70+ years' & Measles$sex=='Both sexes' & Measles$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Measles_subset)
nrow(Measles_subset)

######### leprosy
Lep05 <- read.csv("IHME_GBD_2010_RESULTS_2005_LEPROSY_Y2013M11D15.csv", header=T)

Lep90 <- read.csv("IHME_GBD_2010_RESULTS_1990_LEPROSY_Y2013M11D15.csv", header=T)

Lep10 <- read.csv("IHME_GBD_2010_RESULTS_2010_LEPROSY_Y2013M11D15.csv", header=T)

Lep = rbind(Lep90,Lep05,Lep10)

head(Lep)
tail(Lep)
Lep$Envtrans <-'0'
Lep$Big3 <-'no'
Lep$causelevel1 <- NULL #remove unnecessary columns
Lep$causelevel2 <- NULL
Lep$causelevel3 <- NULL
Lep$causelevel5 <- NULL
Lep_subset <- Lep[which(Lep$age_name=='Under 5 years'|Lep$age_name=='5-14 years'|Lep$age_name=='15-49 years'|Lep$age_name=='50-69 years'| Lep$age_name=='70+ years' & Lep$sex=='Both sexes' & Lep$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Lep_subset)
nrow(Lep_subset)

######### diptheria
Dip05 <- read.csv("IHME_GBD_2010_RESULTS_2005_DIPHTHERIA_Y2013M11D15.csv", header=T)

Dip90 <- read.csv("IHME_GBD_2010_RESULTS_1990_DIPHTHERIA_Y2013M11D15.csv", header=T)

Dip10 <- read.csv("IHME_GBD_2010_RESULTS_2010_DIPHTHERIA_Y2013M11D15.csv", header=T)

Dip = rbind(Dip90,Dip05,Dip10)

head(Dip)
tail(Dip)
Dip$Envtrans <-'0'
Dip$Big3 <-'no'
Dip$causelevel1 <- NULL #remove unnecessary columns
Dip$causelevel2 <- NULL
Dip$causelevel3 <- NULL
Dip$causelevel5 <- NULL
Dip_subset <- Dip[which(Dip$age_name=='Under 5 years'|Dip$age_name=='5-14 years'|Dip$age_name=='15-49 years'|Dip$age_name=='50-69 years'| Dip$age_name=='70+ years' & Dip$sex=='Both sexes' & Dip$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Dip_subset)
nrow(Dip_subset)

###################combine all into one dataset
combined = rbind(YF_subset,Schisto_subset,Rabies_subset, Onch_subset, Mal_subset, LF_subset, Leish_subset, HW_subset, Trich_subset, Asc_subset, FBT_subset, Ech_subset, Den_subset, Cyst_subset, Chagas_subset, AT_subset, HepA_subset, HepB_subset, HepC_subset, HIV_subset, Whoop_subset, Var_subset, Typh_subset, Tub_subset, Trach_subset, Measles_subset, Lep_subset, Dip_subset)
combined = combined[which(combined$sex=='Both sexes' & combined$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
write.csv(combined, file = 'allMurraydata.csv')


########################
##### 2005 data:
########################

######### Yellow Fever
YF <- read.csv("IHME_GBD_2010_RESULTS_2005_YELLOW_FEVER_Y2013M11D15.csv", header=T)
head(YF)
tail(YF)

YF$causelevel1 <- NULL #remove unnecessary columns
YF$causelevel2 <- NULL
YF$causelevel3 <- NULL
YF$causelevel5 <- NULL
YF$region <- NULL
YF$nm_upper <- NULL
YF$nm_lower <- NULL
YF$pc_mean <- NULL
YF$pc_upper <- NULL
YF$pc_lower <- NULL
YF$rt_mean <- NULL
YF$rt_upper <- NULL
YF$rt_lower <- NULL
head(YF)
nrow(YF)
YF_subset <- YF[which(YF$age_name=='Age-standardized' & YF$sex=='Both sexes' & YF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(YF_subset)
nrow(YF_subset)
YF_subset$causelevel4 <- NULL
YF_subset$age_name <- NULL
YF_subset$sex <- NULL
YF_subset$measure <- NULL
#YF_subset$year <- NULL
head(YF_subset)
colnames(YF_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(YF_subset)[2] <- "yf05_nm_mean"
head(YF_subset)

######### Schisto
Schisto <- read.csv("IHME_GBD_2010_RESULTS_2005_SCHISTOSOMIASIS_Y2013M11D15.csv", header=T)
head(Schisto)
tail(Schisto)

Schisto$causelevel1 <- NULL #remove unnecessary columns
Schisto$causelevel2 <- NULL
Schisto$causelevel3 <- NULL
Schisto$causelevel5 <- NULL
Schisto$region <- NULL
Schisto$nm_upper <- NULL
Schisto$nm_lower <- NULL
Schisto$pc_mean <- NULL
Schisto$pc_upper <- NULL
Schisto$pc_lower <- NULL
Schisto$rt_mean <- NULL
Schisto$rt_upper <- NULL
Schisto$rt_lower <- NULL
head(Schisto)
nrow(Schisto)
Schisto_subset <- Schisto[which(Schisto$age_name=='Age-standardized' & Schisto$sex=='Both sexes' & Schisto$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Schisto_subset)
nrow(Schisto_subset)
Schisto_subset$causelevel4 <- NULL
Schisto_subset$age_name <- NULL
Schisto_subset$sex <- NULL
Schisto_subset$measure <- NULL
#Schisto_subset$year <- NULL
head(Schisto_subset)
colnames(Schisto_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Schisto_subset)[2] <- "sch05_nm_mean"
head(Schisto_subset)

######### Rabies
Rabies <- read.csv("IHME_GBD_2010_RESULTS_2005_RABIES_Y2013M11D15.csv", header=T)
head(Rabies)
tail(Rabies)

Rabies$causelevel1 <- NULL #remove unnecessary columns
Rabies$causelevel2 <- NULL
Rabies$causelevel3 <- NULL
Rabies$causelevel5 <- NULL
Rabies$region <- NULL
Rabies$nm_upper <- NULL
Rabies$nm_lower <- NULL
Rabies$pc_mean <- NULL
Rabies$pc_upper <- NULL
Rabies$pc_lower <- NULL
Rabies$rt_mean <- NULL
Rabies$rt_upper <- NULL
Rabies$rt_lower <- NULL
head(Rabies)
nrow(Rabies)
Rabies_subset <- Rabies[which(Rabies$age_name=='Age-standardized' & Rabies$sex=='Both sexes' & Rabies$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Rabies_subset)
nrow(Rabies_subset)
Rabies_subset$causelevel4 <- NULL
Rabies_subset$age_name <- NULL
Rabies_subset$sex <- NULL
Rabies_subset$measure <- NULL
#Rabies_subset$year <- NULL
head(Rabies_subset)
colnames(Rabies_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Rabies_subset)[2] <- "rb05_nm_mean"
head(Rabies_subset)

######### Onchocerciasis
Onch <- read.csv("IHME_GBD_2010_RESULTS_2005_ONCHOCERCIASIS_Y2013M11D15.csv", header=T)
head(Onch)
tail(Onch)

Onch$causelevel1 <- NULL #remove unnecessary columns
Onch$causelevel2 <- NULL
Onch$causelevel3 <- NULL
Onch$causelevel5 <- NULL
Onch$region <- NULL
Onch$nm_upper <- NULL
Onch$nm_lower <- NULL
Onch$pc_mean <- NULL
Onch$pc_upper <- NULL
Onch$pc_lower <- NULL
Onch$rt_mean <- NULL
Onch$rt_upper <- NULL
Onch$rt_lower <- NULL
head(Onch)
nrow(Onch)
Onch_subset <- Onch[which(Onch$age_name=='Age-standardized' & Onch$sex=='Both sexes' & Onch$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Onch_subset)
nrow(Onch_subset)
Onch_subset$causelevel4 <- NULL
Onch_subset$age_name <- NULL
Onch_subset$sex <- NULL
Onch_subset$measure <- NULL
#Onch_subset$year <- NULL
head(Onch_subset)
#colnames(Onch_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Onch_subset)[2] <- "onch05_nm_mean"
#head(Onch_subset)

######### malaria
Mal <- read.csv("IHME_GBD_2010_RESULTS_2005_MALARIA_Y2013M11D15.csv", header=T)
head(Mal)
tail(Mal)

Mal$causelevel1 <- NULL #remove unnecessary columns
Mal$causelevel2 <- NULL
Mal$causelevel3 <- NULL
Mal$causelevel5 <- NULL
Mal$region <- NULL
Mal$nm_upper <- NULL
Mal$nm_lower <- NULL
Mal$pc_mean <- NULL
Mal$pc_upper <- NULL
Mal$pc_lower <- NULL
Mal$rt_mean <- NULL
Mal$rt_upper <- NULL
Mal$rt_lower <- NULL
head(Mal)
nrow(Mal)
Mal_subset <- Mal[which(Mal$age_name=='Age-standardized' & Mal$sex=='Both sexes' & Mal$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Mal_subset)
nrow(Mal_subset)
Mal_subset$causelevel4 <- NULL
Mal_subset$age_name <- NULL
Mal_subset$sex <- NULL
Mal_subset$measure <- NULL
#Mal_subset$year <- NULL
head(Mal_subset)
#colnames(Mal_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Mal_subset)[2] <- "mal05_nm_mean"
#head(Mal_subset)

######### lymphatic filariasis
LF <- read.csv("IHME_GBD_2010_RESULTS_2005_LYMPHATIC_FILARIASIS_Y2013M11D15.csv", header=T)
head(LF)
tail(LF)

LF$causelevel1 <- NULL #remove unnecessary columns
LF$causelevel2 <- NULL
LF$causelevel3 <- NULL
LF$causelevel5 <- NULL
LF$region <- NULL
LF$nm_upper <- NULL
LF$nm_lower <- NULL
LF$pc_mean <- NULL
LF$pc_upper <- NULL
LF$pc_lower <- NULL
LF$rt_mean <- NULL
LF$rt_upper <- NULL
LF$rt_lower <- NULL
head(LF)
nrow(LF)
LF_subset <- LF[which(LF$age_name=='Age-standardized' & LF$sex=='Both sexes' & LF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(LF_subset)
nrow(LF_subset)
LF_subset$causelevel4 <- NULL
LF_subset$age_name <- NULL
LF_subset$sex <- NULL
LF_subset$measure <- NULL
#LF_subset$year <- NULL
head(LF_subset)
#colnames(LF_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(LF_subset)[2] <- "lf05_nm_mean"
#head(LF_subset)

######### leishmaniasis
Leish <- read.csv("IHME_GBD_2010_RESULTS_2005_LEISHMANIASIS_Y2013M11D15.csv", header=T)
head(Leish)
tail(Leish)

Leish$causelevel1 <- NULL #remove unnecessary columns
Leish$causelevel2 <- NULL
Leish$causelevel3 <- NULL
Leish$causelevel5 <- NULL
Leish$region <- NULL
Leish$nm_upper <- NULL
Leish$nm_lower <- NULL
Leish$pc_mean <- NULL
Leish$pc_upper <- NULL
Leish$pc_lower <- NULL
Leish$rt_mean <- NULL
Leish$rt_upper <- NULL
Leish$rt_lower <- NULL
head(Leish)
nrow(Leish)
Leish_subset <- Leish[which(Leish$age_name=='Age-standardized' & Leish$sex=='Both sexes' & Leish$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Leish_subset)
nrow(Leish_subset)
Leish_subset$causelevel4 <- NULL
Leish_subset$age_name <- NULL
Leish_subset$sex <- NULL
Leish_subset$measure <- NULL
#Leish_subset$year <- NULL
head(Leish_subset)
#colnames(Leish_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Leish_subset)[2] <- "leish05_nm_mean"
#head(Leish_subset)

######### hookworm
HW <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(HW)
tail(HW)

HW$causelevel1 <- NULL #remove unnecessary columns
HW$causelevel2 <- NULL
HW$causelevel3 <- NULL
HW$region <- NULL
HW$nm_upper <- NULL
HW$nm_lower <- NULL
HW$pc_mean <- NULL
HW$pc_upper <- NULL
HW$pc_lower <- NULL
HW$rt_mean <- NULL
HW$rt_upper <- NULL
HW$rt_lower <- NULL
head(HW)
nrow(HW)
HW_subset <- HW[which(HW$age_name=='Age-standardized' & HW$sex=='Both sexes' & HW$measure=='daly' & HW$causelevel5=='Hookworm disease'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HW_subset)
nrow(HW_subset)
HW_subset$causelevel4 <- NULL
HW_subset$causelevel5 <- NULL
HW_subset$age_name <- NULL
HW_subset$sex <- NULL
HW_subset$measure <- NULL
#HW_subset$year <- NULL
head(HW_subset)
#colnames(HW_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(HW_subset)[2] <- "inhook05_nm_mean"
#head(HW_subset)

######### trichuriasis
Trich <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(Trich)
tail(Trich)

Trich$causelevel1 <- NULL #remove unnecessary columns
Trich$causelevel2 <- NULL
Trich$causelevel3 <- NULL
Trich$region <- NULL
Trich$nm_upper <- NULL
Trich$nm_lower <- NULL
Trich$pc_mean <- NULL
Trich$pc_upper <- NULL
Trich$pc_lower <- NULL
Trich$rt_mean <- NULL
Trich$rt_upper <- NULL
Trich$rt_lower <- NULL
head(Trich)
nrow(Trich)
Trich_subset <- Trich[which(Trich$age_name=='Age-standardized' & Trich$sex=='Both sexes' & Trich$measure=='daly' & Trich$causelevel5=='Trichuriasis'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trich_subset)
nrow(Trich_subset)
Trich_subset$causelevel4 <- NULL
Trich_subset$causelevel5 <- NULL
Trich_subset$age_name <- NULL
Trich_subset$sex <- NULL
Trich_subset$measure <- NULL
#Trich_subset$year <- NULL
head(Trich_subset)
#colnames(Trich_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Trich_subset)[2] <- "intrich05_nm_mean"
#head(Trich_subset)

######### Ascariasis
Asc <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(Asc)
tail(Asc)

Asc$causelevel1 <- NULL #remove unnecessary columns
Asc$causelevel2 <- NULL
Asc$causelevel3 <- NULL
Asc$region <- NULL
Asc$nm_upper <- NULL
Asc$nm_lower <- NULL
Asc$pc_mean <- NULL
Asc$pc_upper <- NULL
Asc$pc_lower <- NULL
Asc$rt_mean <- NULL
Asc$rt_upper <- NULL
Asc$rt_lower <- NULL
head(Asc)
nrow(Asc)
Asc_subset <- Asc[which(Asc$age_name=='Age-standardized' & Asc$sex=='Both sexes' & Asc$measure=='daly' & Asc$causelevel5=='Ascariasis'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Asc_subset)
nrow(Asc_subset)
Asc_subset$causelevel4 <- NULL
Asc_subset$causelevel5 <- NULL
Asc_subset$age_name <- NULL
Asc_subset$sex <- NULL
Asc_subset$measure <- NULL
#Asc_subset$year <- NULL
head(Asc_subset)
#colnames(Asc_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Asc_subset)[2] <- "inasc05_nm_mean"
#head(Asc_subset)

######### food-borne trematodiasis
FBT <- read.csv("IHME_GBD_2010_RESULTS_2005_FOOD-BORNE_TREMATODIASES_Y2013M11D15.csv", header=T)
head(FBT)
tail(FBT)

FBT$causelevel1 <- NULL #remove unnecessary columns
FBT$causelevel2 <- NULL
FBT$causelevel3 <- NULL
FBT$causelevel5 <- NULL
FBT$region <- NULL
FBT$nm_upper <- NULL
FBT$nm_lower <- NULL
FBT$pc_mean <- NULL
FBT$pc_upper <- NULL
FBT$pc_lower <- NULL
FBT$rt_mean <- NULL
FBT$rt_upper <- NULL
FBT$rt_lower <- NULL
head(FBT)
nrow(FBT)
FBT_subset <- FBT[which(FBT$age_name=='Age-standardized' & FBT$sex=='Both sexes' & FBT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(FBT_subset)
nrow(FBT_subset)
FBT_subset$causelevel4 <- NULL
FBT_subset$age_name <- NULL
FBT_subset$sex <- NULL
FBT_subset$measure <- NULL
#FBT_subset$year <- NULL
head(FBT_subset)
#colnames(FBT_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(FBT_subset)[2] <- "fbtrem05_nm_mean"
#head(FBT_subset)

######### echinococcus
Ech <- read.csv("IHME_GBD_2010_RESULTS_2005_ECHINOCOCCOSIS_Y2013M11D15.csv", header=T)
head(Ech)
tail(Ech)

Ech$causelevel1 <- NULL #remove unnecessary columns
Ech$causelevel2 <- NULL
Ech$causelevel3 <- NULL
Ech$causelevel5 <- NULL
Ech$region <- NULL
Ech$nm_upper <- NULL
Ech$nm_lower <- NULL
Ech$pc_mean <- NULL
Ech$pc_upper <- NULL
Ech$pc_lower <- NULL
Ech$rt_mean <- NULL
Ech$rt_upper <- NULL
Ech$rt_lower <- NULL
head(Ech)
nrow(Ech)
Ech_subset <- Ech[which(Ech$age_name=='Age-standardized' & Ech$sex=='Both sexes' & Ech$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Ech_subset)
nrow(Ech_subset)
Ech_subset$causelevel4 <- NULL
Ech_subset$age_name <- NULL
Ech_subset$sex <- NULL
Ech_subset$measure <- NULL
#Ech_subset$year <- NULL
head(Ech_subset)
#colnames(Ech_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Ech_subset)[2] <- "ech05_nm_mean"
#head(Ech_subset)

######### dengue
Den <- read.csv("IHME_GBD_2010_RESULTS_2005_DENGUE_Y2013M11D15.csv", header=T)
head(Den)
tail(Den)

Den$causelevel1 <- NULL #remove unnecessary columns
Den$causelevel2 <- NULL
Den$causelevel3 <- NULL
Den$causelevel5 <- NULL
Den$region <- NULL
Den$nm_upper <- NULL
Den$nm_lower <- NULL
Den$pc_mean <- NULL
Den$pc_upper <- NULL
Den$pc_lower <- NULL
Den$rt_mean <- NULL
Den$rt_upper <- NULL
Den$rt_lower <- NULL
head(Den)
nrow(Den)
Den_subset <- Den[which(Den$age_name=='Age-standardized' & Den$sex=='Both sexes' & Den$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Den_subset)
nrow(Den_subset)
Den_subset$causelevel4 <- NULL
Den_subset$age_name <- NULL
Den_subset$sex <- NULL
Den_subset$measure <- NULL
#Den_subset$year <- NULL
head(Den_subset)
#colnames(Den_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Den_subset)[2] <- "dengue05_nm_mean"
#head(Den_subset)

######### cysticercosis
Cyst <- read.csv("IHME_GBD_2010_RESULTS_2005_CYSTICERCOSIS_Y2013M11D15.csv", header=T)
head(Cyst)
tail(Cyst)

Cyst$causelevel1 <- NULL #remove unnecessary columns
Cyst$causelevel2 <- NULL
Cyst$causelevel3 <- NULL
Cyst$causelevel5 <- NULL
Cyst$region <- NULL
Cyst$nm_upper <- NULL
Cyst$nm_lower <- NULL
Cyst$pc_mean <- NULL
Cyst$pc_upper <- NULL
Cyst$pc_lower <- NULL
Cyst$rt_mean <- NULL
Cyst$rt_upper <- NULL
Cyst$rt_lower <- NULL
head(Cyst)
nrow(Cyst)
Cyst_subset <- Cyst[which(Cyst$age_name=='Age-standardized' & Cyst$sex=='Both sexes' & Cyst$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Cyst_subset)
nrow(Cyst_subset)
Cyst_subset$causelevel4 <- NULL
Cyst_subset$age_name <- NULL
Cyst_subset$sex <- NULL
Cyst_subset$measure <- NULL
#Cyst_subset$year <- NULL
head(Cyst_subset)
#colnames(Cyst_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Cyst_subset)[2] <- "cyst05_nm_mean"
#head(Cyst_subset)

######### chagas
Chagas <- read.csv("IHME_GBD_2010_RESULTS_2005_CHAGAS_DISEASE_Y2013M11D15.csv", header=T)
head(Chagas)
tail(Chagas)

Chagas$causelevel1 <- NULL #remove unnecessary columns
Chagas$causelevel2 <- NULL
Chagas$causelevel3 <- NULL
Chagas$causelevel5 <- NULL
Chagas$region <- NULL
Chagas$nm_upper <- NULL
Chagas$nm_lower <- NULL
Chagas$pc_mean <- NULL
Chagas$pc_upper <- NULL
Chagas$pc_lower <- NULL
Chagas$rt_mean <- NULL
Chagas$rt_upper <- NULL
Chagas$rt_lower <- NULL
head(Chagas)
nrow(Chagas)
Chagas_subset <- Chagas[which(Chagas$age_name=='Age-standardized' & Chagas$sex=='Both sexes' & Chagas$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Chagas_subset)
nrow(Chagas_subset)
Chagas_subset$causelevel4 <- NULL
Chagas_subset$age_name <- NULL
Chagas_subset$sex <- NULL
Chagas_subset$measure <- NULL
#Chagas_subset$year <- NULL
head(Chagas_subset)
#colnames(Chagas_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Chagas_subset)[2] <- "chag05_nm_mean"
#head(Chagas_subset)

######### african trypanosomiasis
AT <- read.csv("IHME_GBD_2010_RESULTS_2005_AFRICAN_TRYPANOSOMIASIS_Y2013M11D15.csv", header=T)
head(AT)
tail(AT)

AT$causelevel1 <- NULL #remove unnecessary columns
AT$causelevel2 <- NULL
AT$causelevel3 <- NULL
AT$causelevel5 <- NULL
AT$region <- NULL
AT$nm_upper <- NULL
AT$nm_lower <- NULL
AT$pc_mean <- NULL
AT$pc_upper <- NULL
AT$pc_lower <- NULL
AT$rt_mean <- NULL
AT$rt_upper <- NULL
AT$rt_lower <- NULL
head(AT)
nrow(AT)
AT_subset <- AT[which(AT$age_name=='Age-standardized' & AT$sex=='Both sexes' & AT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(AT_subset)
nrow(AT_subset)
AT_subset$causelevel4 <- NULL
AT_subset$age_name <- NULL
AT_subset$sex <- NULL
AT_subset$measure <- NULL
#AT_subset$year <- NULL
head(AT_subset)
#colnames(AT_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(AT_subset)[2] <- "aftryp05_nm_mean"
#head(AT_subset)

######### Hep A
HepA <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepA)
tail(HepA)

HepA$causelevel1 <- NULL #remove unnecessary columns
HepA$causelevel2 <- NULL
HepA$causelevel3 <- NULL
HepA$region <- NULL
HepA$nm_upper <- NULL
HepA$nm_lower <- NULL
HepA$pc_mean <- NULL
HepA$pc_upper <- NULL
HepA$pc_lower <- NULL
HepA$rt_mean <- NULL
HepA$rt_upper <- NULL
HepA$rt_lower <- NULL
head(HepA)
nrow(HepA)
HepA_subset <- HepA[which(HepA$age_name=='Age-standardized' & HepA$sex=='Both sexes' & HepA$measure=='daly' & HepA$causelevel5=='Acute hepatitis A'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepA_subset)
nrow(HepA_subset)
HepA_subset$causelevel4 <- NULL
HepA_subset$causelevel5 <- NULL
HepA_subset$age_name <- NULL
HepA_subset$sex <- NULL
HepA_subset$measure <- NULL
#HepA_subset$year <- NULL
head(HepA_subset)
#colnames(HepA_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(HepA_subset)[2] <- "hepa05_nm_mean"
#head(HepA_subset)

######### Hep B
HepB <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepB)
tail(HepB)

HepB$causelevel1 <- NULL #remove unnecessary columns
HepB$causelevel2 <- NULL
HepB$causelevel3 <- NULL
HepB$region <- NULL
HepB$nm_upper <- NULL
HepB$nm_lower <- NULL
HepB$pc_mean <- NULL
HepB$pc_upper <- NULL
HepB$pc_lower <- NULL
HepB$rt_mean <- NULL
HepB$rt_upper <- NULL
HepB$rt_lower <- NULL
head(HepB)
nrow(HepB)
HepB_subset <- HepB[which(HepB$age_name=='Age-standardized' & HepB$sex=='Both sexes' & HepB$measure=='daly' & HepB$causelevel5=='Acute hepatitis B'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepB_subset)
nrow(HepB_subset)
HepB_subset$causelevel4 <- NULL
HepB_subset$causelevel5 <- NULL
HepB_subset$age_name <- NULL
HepB_subset$sex <- NULL
HepB_subset$measure <- NULL
#HepB_subset$year <- NULL
head(HepB_subset)
#colnames(HepB_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(HepB_subset)[2] <- "hepb05_nm_mean"
#head(HepB_subset)

######### Hep C
HepC <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepC)
tail(HepC)

HepC$causelevel1 <- NULL #remove unnecessary columns
HepC$causelevel2 <- NULL
HepC$causelevel3 <- NULL
HepC$region <- NULL
HepC$nm_upper <- NULL
HepC$nm_lower <- NULL
HepC$pc_mean <- NULL
HepC$pc_upper <- NULL
HepC$pc_lower <- NULL
HepC$rt_mean <- NULL
HepC$rt_upper <- NULL
HepC$rt_lower <- NULL
head(HepC)
nrow(HepC)
HepC_subset <- HepC[which(HepC$age_name=='Age-standardized' & HepC$sex=='Both sexes' & HepC$measure=='daly' & HepC$causelevel5=='Acute hepatitis C'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepC_subset)
nrow(HepC_subset)
HepC_subset$causelevel4 <- NULL
HepC_subset$causelevel5 <- NULL
HepC_subset$age_name <- NULL
HepC_subset$sex <- NULL
HepC_subset$measure <- NULL
#HepC_subset$year <- NULL
head(HepC_subset)
#colnames(HepC_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(HepC_subset)[2] <- "hepc05_nm_mean"
#head(HepC_subset)

######### HIV
HIV <- read.csv("IHME_GBD_2010_RESULTS_2005_HIV_AIDS_Y2013M11D15.csv", header=T)
head(HIV)
tail(HIV)

HIV$causelevel1 <- NULL #remove unnecessary columns
HIV$causelevel2 <- NULL
HIV$causelevel3 <- NULL
#HIV$causelevel5 <- NULL
HIV$region <- NULL
HIV$nm_upper <- NULL
HIV$nm_lower <- NULL
HIV$pc_mean <- NULL
HIV$pc_upper <- NULL
HIV$pc_lower <- NULL
HIV$rt_mean <- NULL
HIV$rt_upper <- NULL
HIV$rt_lower <- NULL
head(HIV)
nrow(HIV)
HIV_subset <- HIV[which(HIV$age_name=='Age-standardized' & HIV$sex=='Both sexes' & HIV$measure=='daly' & HIV$causelevel5==''),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HIV_subset)
nrow(HIV_subset)
HIV_subset$causelevel4 <- NULL
HIV_subset$causelevel5 <- NULL
HIV_subset$age_name <- NULL
HIV_subset$sex <- NULL
HIV_subset$measure <- NULL
#HIV_subset$year <- NULL
head(HIV_subset)
#colnames(HIV_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(HIV_subset)[2] <- "hivaids05_nm_mean"
#head(HIV_subset)
#nrow(HIV_subset)
#collapse HIV data to get total HIV/AIDS related DALYs (including those cases resulting in mycobacterial infection, and those resulting in other diseases) -- could also only include HIV/AIDS DALY's from HIV itself
#HIV_subset <- summaryBy(hivaids05_nm_mean ~ NAME_LONG, data=HIV_subset, FUN=sum, na.rm=F)
#head(HIV_subset)
#nrow(HIV_subset)

######### whooping cough
Whoop <- read.csv("IHME_GBD_2010_RESULTS_2005_WHOOPING_COUGH_Y2013M11D15.csv", header=T)
head(Whoop)
tail(Whoop)

Whoop$causelevel1 <- NULL #remove unnecessary columns
Whoop$causelevel2 <- NULL
Whoop$causelevel3 <- NULL
Whoop$causelevel5 <- NULL
Whoop$region <- NULL
Whoop$nm_upper <- NULL
Whoop$nm_lower <- NULL
Whoop$pc_mean <- NULL
Whoop$pc_upper <- NULL
Whoop$pc_lower <- NULL
Whoop$rt_mean <- NULL
Whoop$rt_upper <- NULL
Whoop$rt_lower <- NULL
head(Whoop)
nrow(Whoop)
Whoop_subset <- Whoop[which(Whoop$age_name=='Age-standardized' & Whoop$sex=='Both sexes' & Whoop$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Whoop_subset)
nrow(Whoop_subset)
Whoop_subset$causelevel4 <- NULL
Whoop_subset$age_name <- NULL
Whoop_subset$sex <- NULL
Whoop_subset$measure <- NULL
#Whoop_subset$year <- NULL
head(Whoop_subset)
#colnames(Whoop_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Whoop_subset)[2] <- "whoop05_nm_mean"
#head(Whoop_subset)

######### varicella
Var <- read.csv("IHME_GBD_2010_RESULTS_2005_VARICELLA_Y2013M11D15.csv", header=T)
head(Var)
tail(Var)

Var$causelevel1 <- NULL #remove unnecessary columns
Var$causelevel2 <- NULL
Var$causelevel3 <- NULL
Var$causelevel5 <- NULL
Var$region <- NULL
Var$nm_upper <- NULL
Var$nm_lower <- NULL
Var$pc_mean <- NULL
Var$pc_upper <- NULL
Var$pc_lower <- NULL
Var$rt_mean <- NULL
Var$rt_upper <- NULL
Var$rt_lower <- NULL
head(Var)
nrow(Var)
Var_subset <- Var[which(Var$age_name=='Age-standardized' & Var$sex=='Both sexes' & Var$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Var_subset)
nrow(Var_subset)
Var_subset$causelevel4 <- NULL
Var_subset$age_name <- NULL
Var_subset$sex <- NULL
Var_subset$measure <- NULL
#Var_subset$year <- NULL
head(Var_subset)
#colnames(Var_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Var_subset)[2] <- "var05_nm_mean"
#head(Var_subset)

######### typhoid
Typh <- read.csv("IHME_GBD_2010_RESULTS_2005_TYPHOID_AND_PARATYPHOID_FEVERS_Y2013M11D15.csv", header=T)
head(Typh)
tail(Typh)

Typh$causelevel1 <- NULL #remove unnecessary columns
Typh$causelevel2 <- NULL
Typh$causelevel3 <- NULL
Typh$causelevel5 <- NULL
Typh$region <- NULL
Typh$nm_upper <- NULL
Typh$nm_lower <- NULL
Typh$pc_mean <- NULL
Typh$pc_upper <- NULL
Typh$pc_lower <- NULL
Typh$rt_mean <- NULL
Typh$rt_upper <- NULL
Typh$rt_lower <- NULL
head(Typh)
nrow(Typh)
Typh_subset <- Typh[which(Typh$age_name=='Age-standardized' & Typh$sex=='Both sexes' & Typh$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Typh_subset)
nrow(Typh_subset)
Typh_subset$causelevel4 <- NULL
Typh_subset$age_name <- NULL
Typh_subset$sex <- NULL
Typh_subset$measure <- NULL
#Typh_subset$year <- NULL
head(Typh_subset)
#colnames(Typh_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Typh_subset)[2] <- "typh05_nm_mean"
#head(Typh_subset)

######### tuberculosis
Tub <- read.csv("IHME_GBD_2010_RESULTS_2005_TUBERCULOSIS_Y2013M11D15.csv", header=T)
head(Tub)
tail(Tub)

Tub$causelevel1 <- NULL #remove unnecessary columns
Tub$causelevel2 <- NULL
Tub$causelevel3 <- NULL
Tub$causelevel5 <- NULL
Tub$region <- NULL
Tub$nm_upper <- NULL
Tub$nm_lower <- NULL
Tub$pc_mean <- NULL
Tub$pc_upper <- NULL
Tub$pc_lower <- NULL
Tub$rt_mean <- NULL
Tub$rt_upper <- NULL
Tub$rt_lower <- NULL
head(Tub)
nrow(Tub)
Tub_subset <- Tub[which(Tub$age_name=='Age-standardized' & Tub$sex=='Both sexes' & Tub$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Tub_subset)
nrow(Tub_subset)
Tub_subset$causelevel4 <- NULL
Tub_subset$age_name <- NULL
Tub_subset$sex <- NULL
Tub_subset$measure <- NULL
#Tub_subset$year <- NULL
head(Tub_subset)
#colnames(Tub_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Tub_subset)[2] <- "tuberc05_nm_mean"
#head(Tub_subset)

######### trachoma
Trach <- read.csv("IHME_GBD_2010_RESULTS_2005_TRACHOMA_Y2013M11D15.csv", header=T)
head(Trach)
tail(Trach)

Trach$causelevel1 <- NULL #remove unnecessary columns
Trach$causelevel2 <- NULL
Trach$causelevel3 <- NULL
Trach$causelevel5 <- NULL
Trach$region <- NULL
Trach$nm_upper <- NULL
Trach$nm_lower <- NULL
Trach$pc_mean <- NULL
Trach$pc_upper <- NULL
Trach$pc_lower <- NULL
Trach$rt_mean <- NULL
Trach$rt_upper <- NULL
Trach$rt_lower <- NULL
head(Trach)
nrow(Trach)
Trach_subset <- Trach[which(Trach$age_name=='Age-standardized' & Trach$sex=='Both sexes' & Trach$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trach_subset)
nrow(Trach_subset)
Trach_subset$causelevel4 <- NULL
Trach_subset$age_name <- NULL
Trach_subset$sex <- NULL
Trach_subset$measure <- NULL
#Trach_subset$year <- NULL
head(Trach_subset)
#colnames(Trach_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Trach_subset)[2] <- "trach05_nm_mean"
#head(Trach_subset)

######### measles
Measles <- read.csv("IHME_GBD_2010_RESULTS_2005_MEASLES_Y2013M11D15.csv", header=T)
head(Measles)
tail(Measles)

Measles$causelevel1 <- NULL #remove unnecessary columns
Measles$causelevel2 <- NULL
Measles$causelevel3 <- NULL
Measles$causelevel5 <- NULL
Measles$region <- NULL
Measles$nm_upper <- NULL
Measles$nm_lower <- NULL
Measles$pc_mean <- NULL
Measles$pc_upper <- NULL
Measles$pc_lower <- NULL
Measles$rt_mean <- NULL
Measles$rt_upper <- NULL
Measles$rt_lower <- NULL
head(Measles)
nrow(Measles)
Measles_subset <- Measles[which(Measles$age_name=='Age-standardized' & Measles$sex=='Both sexes' & Measles$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Measles_subset)
nrow(Measles_subset)
Measles_subset$causelevel4 <- NULL
Measles_subset$age_name <- NULL
Measles_subset$sex <- NULL
Measles_subset$measure <- NULL
#Measles_subset$year <- NULL
head(Measles_subset)
#colnames(Measles_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Measles_subset)[2] <- "meas05_nm_mean"
#head(Measles_subset)

######### leprosy
Lep <- read.csv("IHME_GBD_2010_RESULTS_2005_LEPROSY_Y2013M11D15.csv", header=T)
head(Lep)
tail(Lep)

Lep$causelevel1 <- NULL #remove unnecessary columns
Lep$causelevel2 <- NULL
Lep$causelevel3 <- NULL
Lep$causelevel5 <- NULL
Lep$region <- NULL
Lep$nm_upper <- NULL
Lep$nm_lower <- NULL
Lep$pc_mean <- NULL
Lep$pc_upper <- NULL
Lep$pc_lower <- NULL
Lep$rt_mean <- NULL
Lep$rt_upper <- NULL
Lep$rt_lower <- NULL
head(Lep)
nrow(Lep)
Lep_subset <- Lep[which(Lep$age_name=='Age-standardized' & Lep$sex=='Both sexes' & Lep$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Lep_subset)
nrow(Lep_subset)
Lep_subset$causelevel4 <- NULL
Lep_subset$age_name <- NULL
Lep_subset$sex <- NULL
Lep_subset$measure <- NULL
#Lep_subset$year <- NULL
head(Lep_subset)
#colnames(Lep_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Lep_subset)[2] <- "lep05_nm_mean"
#head(Lep_subset)

######### diptheria
Dip <- read.csv("IHME_GBD_2010_RESULTS_2005_DIPHTHERIA_Y2013M11D15.csv", header=T)
head(Dip)
tail(Dip)

Dip$causelevel1 <- NULL #remove unnecessary columns
Dip$causelevel2 <- NULL
Dip$causelevel3 <- NULL
Dip$causelevel5 <- NULL
Dip$region <- NULL
Dip$nm_upper <- NULL
Dip$nm_lower <- NULL
Dip$pc_mean <- NULL
Dip$pc_upper <- NULL
Dip$pc_lower <- NULL
Dip$rt_mean <- NULL
Dip$rt_upper <- NULL
Dip$rt_lower <- NULL
head(Dip)
nrow(Dip)
Dip_subset <- Dip[which(Dip$age_name=='Age-standardized' & Dip$sex=='Both sexes' & Dip$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Dip_subset)
nrow(Dip_subset)
Dip_subset$causelevel4 <- NULL
Dip_subset$age_name <- NULL
Dip_subset$sex <- NULL
Dip_subset$measure <- NULL
#Dip_subset$year <- NULL
head(Dip_subset)
#colnames(Dip_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Dip_subset)[2] <- "dipth05_nm_mean"
#head(Dip_subset)

#########################################################################
########################
##### 1990 data:
########################

######### Yellow Fever
YF <- read.csv("IHME_GBD_2010_RESULTS_1990_YELLOW_FEVER_Y2013M11D15.csv", header=T)
head(YF)
tail(YF)

YF$causelevel1 <- NULL #remove unnecessary columns
YF$causelevel2 <- NULL
YF$causelevel3 <- NULL
YF$causelevel5 <- NULL
YF$region <- NULL
YF$nm_upper <- NULL
YF$nm_lower <- NULL
YF$pc_mean <- NULL
YF$pc_upper <- NULL
YF$pc_lower <- NULL
YF$rt_mean <- NULL
YF$rt_upper <- NULL
YF$rt_lower <- NULL
head(YF)
nrow(YF)
YF_subset <- YF[which(YF$age_name=='Age-standardized' & YF$sex=='Both sexes' & YF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(YF_subset)
nrow(YF_subset)
YF_subset$causelevel4 <- NULL
YF_subset$age_name <- NULL
YF_subset$sex <- NULL
YF_subset$measure <- NULL
#YF_subset$year <- NULL
head(YF_subset)
#colnames(YF_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(YF_subset)[2] <- "yf90_nm_mean"
#head(YF_subset)

######### Schisto
Schisto <- read.csv("IHME_GBD_2010_RESULTS_1990_SCHISTOSOMIASIS_Y2013M11D15.csv", header=T)
head(Schisto)
tail(Schisto)

Schisto$causelevel1 <- NULL #remove unnecessary columns
Schisto$causelevel2 <- NULL
Schisto$causelevel3 <- NULL
Schisto$causelevel5 <- NULL
Schisto$region <- NULL
Schisto$nm_upper <- NULL
Schisto$nm_lower <- NULL
Schisto$pc_mean <- NULL
Schisto$pc_upper <- NULL
Schisto$pc_lower <- NULL
Schisto$rt_mean <- NULL
Schisto$rt_upper <- NULL
Schisto$rt_lower <- NULL
head(Schisto)
nrow(Schisto)
Schisto_subset <- Schisto[which(Schisto$age_name=='Age-standardized' & Schisto$sex=='Both sexes' & Schisto$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Schisto_subset)
nrow(Schisto_subset)
Schisto_subset$causelevel4 <- NULL
Schisto_subset$age_name <- NULL
Schisto_subset$sex <- NULL
Schisto_subset$measure <- NULL
#Schisto_subset$year <- NULL
head(Schisto_subset)
#colnames(Schisto_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Schisto_subset)[2] <- "sch90_nm_mean"
#head(Schisto_subset)

######### Rabies
Rabies <- read.csv("IHME_GBD_2010_RESULTS_1990_RABIES_Y2013M11D15.csv", header=T)
head(Rabies)
tail(Rabies)

Rabies$causelevel1 <- NULL #remove unnecessary columns
Rabies$causelevel2 <- NULL
Rabies$causelevel3 <- NULL
Rabies$causelevel5 <- NULL
Rabies$region <- NULL
Rabies$nm_upper <- NULL
Rabies$nm_lower <- NULL
Rabies$pc_mean <- NULL
Rabies$pc_upper <- NULL
Rabies$pc_lower <- NULL
Rabies$rt_mean <- NULL
Rabies$rt_upper <- NULL
Rabies$rt_lower <- NULL
head(Rabies)
nrow(Rabies)
Rabies_subset <- Rabies[which(Rabies$age_name=='Age-standardized' & Rabies$sex=='Both sexes' & Rabies$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Rabies_subset)
nrow(Rabies_subset)
Rabies_subset$causelevel4 <- NULL
Rabies_subset$age_name <- NULL
Rabies_subset$sex <- NULL
Rabies_subset$measure <- NULL
#Rabies_subset$year <- NULL
head(Rabies_subset)
#colnames(Rabies_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Rabies_subset)[2] <- "rb90_nm_mean"
#head(Rabies_subset)

######### Onchocerciasis
Onch <- read.csv("IHME_GBD_2010_RESULTS_1990_ONCHOCERCIASIS_Y2013M11D15.csv", header=T)
head(Onch)
tail(Onch)

Onch$causelevel1 <- NULL #remove unnecessary columns
Onch$causelevel2 <- NULL
Onch$causelevel3 <- NULL
Onch$causelevel5 <- NULL
Onch$region <- NULL
Onch$nm_upper <- NULL
Onch$nm_lower <- NULL
Onch$pc_mean <- NULL
Onch$pc_upper <- NULL
Onch$pc_lower <- NULL
Onch$rt_mean <- NULL
Onch$rt_upper <- NULL
Onch$rt_lower <- NULL
head(Onch)
nrow(Onch)
Onch_subset <- Onch[which(Onch$age_name=='Age-standardized' & Onch$sex=='Both sexes' & Onch$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Onch_subset)
nrow(Onch_subset)
Onch_subset$causelevel4 <- NULL
Onch_subset$age_name <- NULL
Onch_subset$sex <- NULL
Onch_subset$measure <- NULL
#Onch_subset$year <- NULL
head(Onch_subset)
#colnames(Onch_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Onch_subset)[2] <- "onch90_nm_mean"
#head(Onch_subset)

######### malaria
Mal <- read.csv("IHME_GBD_2010_RESULTS_1990_MALARIA_Y2013M11D15.csv", header=T)
head(Mal)
tail(Mal)

Mal$causelevel1 <- NULL #remove unnecessary columns
Mal$causelevel2 <- NULL
Mal$causelevel3 <- NULL
Mal$causelevel5 <- NULL
Mal$region <- NULL
Mal$nm_upper <- NULL
Mal$nm_lower <- NULL
Mal$pc_mean <- NULL
Mal$pc_upper <- NULL
Mal$pc_lower <- NULL
Mal$rt_mean <- NULL
Mal$rt_upper <- NULL
Mal$rt_lower <- NULL
head(Mal)
nrow(Mal)
Mal_subset <- Mal[which(Mal$age_name=='Age-standardized' & Mal$sex=='Both sexes' & Mal$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Mal_subset)
nrow(Mal_subset)
Mal_subset$causelevel4 <- NULL
Mal_subset$age_name <- NULL
Mal_subset$sex <- NULL
Mal_subset$measure <- NULL
#Mal_subset$year <- NULL
head(Mal_subset)
#colnames(Mal_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(Mal_subset)[2] <- "mal90_nm_mean"
#head(Mal_subset)

######### lymphatic filariasis
LF <- read.csv("IHME_GBD_2010_RESULTS_2005_LYMPHATIC_FILARIASIS_Y2013M11D15.csv", header=T)
head(LF)
tail(LF)

LF$causelevel1 <- NULL #remove unnecessary columns
LF$causelevel2 <- NULL
LF$causelevel3 <- NULL
LF$causelevel5 <- NULL
LF$region <- NULL
LF$nm_upper <- NULL
LF$nm_lower <- NULL
LF$pc_mean <- NULL
LF$pc_upper <- NULL
LF$pc_lower <- NULL
LF$rt_mean <- NULL
LF$rt_upper <- NULL
LF$rt_lower <- NULL
head(LF)
nrow(LF)
LF_subset <- LF[which(LF$age_name=='Age-standardized' & LF$sex=='Both sexes' & LF$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(LF_subset)
nrow(LF_subset)
LF_subset$causelevel4 <- NULL
LF_subset$age_name <- NULL
LF_subset$sex <- NULL
LF_subset$measure <- NULL
#LF_subset$year <- NULL
head(LF_subset)
#colnames(LF_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
#colnames(LF_subset)[2] <- "lf05_nm_mean"
#head(LF_subset)

######### leishmaniasis
Leish <- read.csv("IHME_GBD_2010_RESULTS_2005_LEISHMANIASIS_Y2013M11D15.csv", header=T)
head(Leish)
tail(Leish)

Leish$causelevel1 <- NULL #remove unnecessary columns
Leish$causelevel2 <- NULL
Leish$causelevel3 <- NULL
Leish$causelevel5 <- NULL
Leish$region <- NULL
Leish$nm_upper <- NULL
Leish$nm_lower <- NULL
Leish$pc_mean <- NULL
Leish$pc_upper <- NULL
Leish$pc_lower <- NULL
Leish$rt_mean <- NULL
Leish$rt_upper <- NULL
Leish$rt_lower <- NULL
head(Leish)
nrow(Leish)
Leish_subset <- Leish[which(Leish$age_name=='Age-standardized' & Leish$sex=='Both sexes' & Leish$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Leish_subset)
nrow(Leish_subset)
Leish_subset$causelevel4 <- NULL
Leish_subset$age_name <- NULL
Leish_subset$sex <- NULL
Leish_subset$measure <- NULL
Leish_subset$year <- NULL
head(Leish_subset)
colnames(Leish_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Leish_subset)[2] <- "leish05_nm_mean"
head(Leish_subset)

######### hookworm
HW <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(HW)
tail(HW)

HW$causelevel1 <- NULL #remove unnecessary columns
HW$causelevel2 <- NULL
HW$causelevel3 <- NULL
HW$region <- NULL
HW$nm_upper <- NULL
HW$nm_lower <- NULL
HW$pc_mean <- NULL
HW$pc_upper <- NULL
HW$pc_lower <- NULL
HW$rt_mean <- NULL
HW$rt_upper <- NULL
HW$rt_lower <- NULL
head(HW)
nrow(HW)
HW_subset <- HW[which(HW$age_name=='Age-standardized' & HW$sex=='Both sexes' & HW$measure=='daly' & HW$causelevel5=='Hookworm disease'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HW_subset)
nrow(HW_subset)
HW_subset$causelevel4 <- NULL
HW_subset$causelevel5 <- NULL
HW_subset$age_name <- NULL
HW_subset$sex <- NULL
HW_subset$measure <- NULL
HW_subset$year <- NULL
head(HW_subset)
colnames(HW_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(HW_subset)[2] <- "inhook05_nm_mean"
head(HW_subset)

######### trichuriasis
Trich <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(Trich)
tail(Trich)

Trich$causelevel1 <- NULL #remove unnecessary columns
Trich$causelevel2 <- NULL
Trich$causelevel3 <- NULL
Trich$region <- NULL
Trich$nm_upper <- NULL
Trich$nm_lower <- NULL
Trich$pc_mean <- NULL
Trich$pc_upper <- NULL
Trich$pc_lower <- NULL
Trich$rt_mean <- NULL
Trich$rt_upper <- NULL
Trich$rt_lower <- NULL
head(Trich)
nrow(Trich)
Trich_subset <- Trich[which(Trich$age_name=='Age-standardized' & Trich$sex=='Both sexes' & Trich$measure=='daly' & Trich$causelevel5=='Trichuriasis'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trich_subset)
nrow(Trich_subset)
Trich_subset$causelevel4 <- NULL
Trich_subset$causelevel5 <- NULL
Trich_subset$age_name <- NULL
Trich_subset$sex <- NULL
Trich_subset$measure <- NULL
Trich_subset$year <- NULL
head(Trich_subset)
colnames(Trich_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Trich_subset)[2] <- "intrich05_nm_mean"
head(Trich_subset)

######### Ascariasis
Asc <- read.csv("IHME_GBD_2010_RESULTS_2005_INTESTINAL_NEMATODE_INFECTIONS_Y2013M11D15.csv", header=T)
head(Asc)
tail(Asc)

Asc$causelevel1 <- NULL #remove unnecessary columns
Asc$causelevel2 <- NULL
Asc$causelevel3 <- NULL
Asc$region <- NULL
Asc$nm_upper <- NULL
Asc$nm_lower <- NULL
Asc$pc_mean <- NULL
Asc$pc_upper <- NULL
Asc$pc_lower <- NULL
Asc$rt_mean <- NULL
Asc$rt_upper <- NULL
Asc$rt_lower <- NULL
head(Asc)
nrow(Asc)
Asc_subset <- Asc[which(Asc$age_name=='Age-standardized' & Asc$sex=='Both sexes' & Asc$measure=='daly' & Asc$causelevel5=='Ascariasis'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Asc_subset)
nrow(Asc_subset)
Asc_subset$causelevel4 <- NULL
Asc_subset$causelevel5 <- NULL
Asc_subset$age_name <- NULL
Asc_subset$sex <- NULL
Asc_subset$measure <- NULL
Asc_subset$year <- NULL
head(Asc_subset)
colnames(Asc_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Asc_subset)[2] <- "inasc05_nm_mean"
head(Asc_subset)

######### food-borne trematodiasis
FBT <- read.csv("IHME_GBD_2010_RESULTS_2005_FOOD-BORNE_TREMATODIASES_Y2013M11D15.csv", header=T)
head(FBT)
tail(FBT)

FBT$causelevel1 <- NULL #remove unnecessary columns
FBT$causelevel2 <- NULL
FBT$causelevel3 <- NULL
FBT$causelevel5 <- NULL
FBT$region <- NULL
FBT$nm_upper <- NULL
FBT$nm_lower <- NULL
FBT$pc_mean <- NULL
FBT$pc_upper <- NULL
FBT$pc_lower <- NULL
FBT$rt_mean <- NULL
FBT$rt_upper <- NULL
FBT$rt_lower <- NULL
head(FBT)
nrow(FBT)
FBT_subset <- FBT[which(FBT$age_name=='Age-standardized' & FBT$sex=='Both sexes' & FBT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(FBT_subset)
nrow(FBT_subset)
FBT_subset$causelevel4 <- NULL
FBT_subset$age_name <- NULL
FBT_subset$sex <- NULL
FBT_subset$measure <- NULL
FBT_subset$year <- NULL
head(FBT_subset)
colnames(FBT_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(FBT_subset)[2] <- "fbtrem05_nm_mean"
head(FBT_subset)

######### echinococcus
Ech <- read.csv("IHME_GBD_2010_RESULTS_2005_ECHINOCOCCOSIS_Y2013M11D15.csv", header=T)
head(Ech)
tail(Ech)

Ech$causelevel1 <- NULL #remove unnecessary columns
Ech$causelevel2 <- NULL
Ech$causelevel3 <- NULL
Ech$causelevel5 <- NULL
Ech$region <- NULL
Ech$nm_upper <- NULL
Ech$nm_lower <- NULL
Ech$pc_mean <- NULL
Ech$pc_upper <- NULL
Ech$pc_lower <- NULL
Ech$rt_mean <- NULL
Ech$rt_upper <- NULL
Ech$rt_lower <- NULL
head(Ech)
nrow(Ech)
Ech_subset <- Ech[which(Ech$age_name=='Age-standardized' & Ech$sex=='Both sexes' & Ech$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Ech_subset)
nrow(Ech_subset)
Ech_subset$causelevel4 <- NULL
Ech_subset$age_name <- NULL
Ech_subset$sex <- NULL
Ech_subset$measure <- NULL
Ech_subset$year <- NULL
head(Ech_subset)
colnames(Ech_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Ech_subset)[2] <- "ech05_nm_mean"
head(Ech_subset)

######### dengue
Den <- read.csv("IHME_GBD_2010_RESULTS_2005_DENGUE_Y2013M11D15.csv", header=T)
head(Den)
tail(Den)

Den$causelevel1 <- NULL #remove unnecessary columns
Den$causelevel2 <- NULL
Den$causelevel3 <- NULL
Den$causelevel5 <- NULL
Den$region <- NULL
Den$nm_upper <- NULL
Den$nm_lower <- NULL
Den$pc_mean <- NULL
Den$pc_upper <- NULL
Den$pc_lower <- NULL
Den$rt_mean <- NULL
Den$rt_upper <- NULL
Den$rt_lower <- NULL
head(Den)
nrow(Den)
Den_subset <- Den[which(Den$age_name=='Age-standardized' & Den$sex=='Both sexes' & Den$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Den_subset)
nrow(Den_subset)
Den_subset$causelevel4 <- NULL
Den_subset$age_name <- NULL
Den_subset$sex <- NULL
Den_subset$measure <- NULL
Den_subset$year <- NULL
head(Den_subset)
colnames(Den_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Den_subset)[2] <- "dengue05_nm_mean"
head(Den_subset)

######### cysticercosis
Cyst <- read.csv("IHME_GBD_2010_RESULTS_2005_CYSTICERCOSIS_Y2013M11D15.csv", header=T)
head(Cyst)
tail(Cyst)

Cyst$causelevel1 <- NULL #remove unnecessary columns
Cyst$causelevel2 <- NULL
Cyst$causelevel3 <- NULL
Cyst$causelevel5 <- NULL
Cyst$region <- NULL
Cyst$nm_upper <- NULL
Cyst$nm_lower <- NULL
Cyst$pc_mean <- NULL
Cyst$pc_upper <- NULL
Cyst$pc_lower <- NULL
Cyst$rt_mean <- NULL
Cyst$rt_upper <- NULL
Cyst$rt_lower <- NULL
head(Cyst)
nrow(Cyst)
Cyst_subset <- Cyst[which(Cyst$age_name=='Age-standardized' & Cyst$sex=='Both sexes' & Cyst$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Cyst_subset)
nrow(Cyst_subset)
Cyst_subset$causelevel4 <- NULL
Cyst_subset$age_name <- NULL
Cyst_subset$sex <- NULL
Cyst_subset$measure <- NULL
Cyst_subset$year <- NULL
head(Cyst_subset)
colnames(Cyst_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Cyst_subset)[2] <- "cyst05_nm_mean"
head(Cyst_subset)

######### chagas
Chagas <- read.csv("IHME_GBD_2010_RESULTS_2005_CHAGAS_DISEASE_Y2013M11D15.csv", header=T)
head(Chagas)
tail(Chagas)

Chagas$causelevel1 <- NULL #remove unnecessary columns
Chagas$causelevel2 <- NULL
Chagas$causelevel3 <- NULL
Chagas$causelevel5 <- NULL
Chagas$region <- NULL
Chagas$nm_upper <- NULL
Chagas$nm_lower <- NULL
Chagas$pc_mean <- NULL
Chagas$pc_upper <- NULL
Chagas$pc_lower <- NULL
Chagas$rt_mean <- NULL
Chagas$rt_upper <- NULL
Chagas$rt_lower <- NULL
head(Chagas)
nrow(Chagas)
Chagas_subset <- Chagas[which(Chagas$age_name=='Age-standardized' & Chagas$sex=='Both sexes' & Chagas$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Chagas_subset)
nrow(Chagas_subset)
Chagas_subset$causelevel4 <- NULL
Chagas_subset$age_name <- NULL
Chagas_subset$sex <- NULL
Chagas_subset$measure <- NULL
Chagas_subset$year <- NULL
head(Chagas_subset)
colnames(Chagas_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Chagas_subset)[2] <- "chag05_nm_mean"
head(Chagas_subset)

######### african trypanosomiasis
AT <- read.csv("IHME_GBD_2010_RESULTS_2005_AFRICAN_TRYPANOSOMIASIS_Y2013M11D15.csv", header=T)
head(AT)
tail(AT)

AT$causelevel1 <- NULL #remove unnecessary columns
AT$causelevel2 <- NULL
AT$causelevel3 <- NULL
AT$causelevel5 <- NULL
AT$region <- NULL
AT$nm_upper <- NULL
AT$nm_lower <- NULL
AT$pc_mean <- NULL
AT$pc_upper <- NULL
AT$pc_lower <- NULL
AT$rt_mean <- NULL
AT$rt_upper <- NULL
AT$rt_lower <- NULL
head(AT)
nrow(AT)
AT_subset <- AT[which(AT$age_name=='Age-standardized' & AT$sex=='Both sexes' & AT$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(AT_subset)
nrow(AT_subset)
AT_subset$causelevel4 <- NULL
AT_subset$age_name <- NULL
AT_subset$sex <- NULL
AT_subset$measure <- NULL
AT_subset$year <- NULL
head(AT_subset)
colnames(AT_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(AT_subset)[2] <- "aftryp05_nm_mean"
head(AT_subset)

######### Hep A
HepA <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepA)
tail(HepA)

HepA$causelevel1 <- NULL #remove unnecessary columns
HepA$causelevel2 <- NULL
HepA$causelevel3 <- NULL
HepA$region <- NULL
HepA$nm_upper <- NULL
HepA$nm_lower <- NULL
HepA$pc_mean <- NULL
HepA$pc_upper <- NULL
HepA$pc_lower <- NULL
HepA$rt_mean <- NULL
HepA$rt_upper <- NULL
HepA$rt_lower <- NULL
head(HepA)
nrow(HepA)
HepA_subset <- HepA[which(HepA$age_name=='Age-standardized' & HepA$sex=='Both sexes' & HepA$measure=='daly' & HepA$causelevel5=='Acute hepatitis A'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepA_subset)
nrow(HepA_subset)
HepA_subset$causelevel4 <- NULL
HepA_subset$causelevel5 <- NULL
HepA_subset$age_name <- NULL
HepA_subset$sex <- NULL
HepA_subset$measure <- NULL
HepA_subset$year <- NULL
head(HepA_subset)
colnames(HepA_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(HepA_subset)[2] <- "hepa05_nm_mean"
head(HepA_subset)

######### Hep B
HepB <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepB)
tail(HepB)

HepB$causelevel1 <- NULL #remove unnecessary columns
HepB$causelevel2 <- NULL
HepB$causelevel3 <- NULL
HepB$region <- NULL
HepB$nm_upper <- NULL
HepB$nm_lower <- NULL
HepB$pc_mean <- NULL
HepB$pc_upper <- NULL
HepB$pc_lower <- NULL
HepB$rt_mean <- NULL
HepB$rt_upper <- NULL
HepB$rt_lower <- NULL
head(HepB)
nrow(HepB)
HepB_subset <- HepB[which(HepB$age_name=='Age-standardized' & HepB$sex=='Both sexes' & HepB$measure=='daly' & HepB$causelevel5=='Acute hepatitis B'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepB_subset)
nrow(HepB_subset)
HepB_subset$causelevel4 <- NULL
HepB_subset$causelevel5 <- NULL
HepB_subset$age_name <- NULL
HepB_subset$sex <- NULL
HepB_subset$measure <- NULL
HepB_subset$year <- NULL
head(HepB_subset)
colnames(HepB_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(HepB_subset)[2] <- "hepb05_nm_mean"
head(HepB_subset)

######### Hep C
HepC <- read.csv("IHME_GBD_2010_RESULTS_2005_HEPATITIS_Y2013M11D15.csv", header=T)
head(HepC)
tail(HepC)

HepC$causelevel1 <- NULL #remove unnecessary columns
HepC$causelevel2 <- NULL
HepC$causelevel3 <- NULL
HepC$region <- NULL
HepC$nm_upper <- NULL
HepC$nm_lower <- NULL
HepC$pc_mean <- NULL
HepC$pc_upper <- NULL
HepC$pc_lower <- NULL
HepC$rt_mean <- NULL
HepC$rt_upper <- NULL
HepC$rt_lower <- NULL
head(HepC)
nrow(HepC)
HepC_subset <- HepC[which(HepC$age_name=='Age-standardized' & HepC$sex=='Both sexes' & HepC$measure=='daly' & HepC$causelevel5=='Acute hepatitis C'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HepC_subset)
nrow(HepC_subset)
HepC_subset$causelevel4 <- NULL
HepC_subset$causelevel5 <- NULL
HepC_subset$age_name <- NULL
HepC_subset$sex <- NULL
HepC_subset$measure <- NULL
HepC_subset$year <- NULL
head(HepC_subset)
colnames(HepC_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(HepC_subset)[2] <- "hepc05_nm_mean"
head(HepC_subset)

######### HIV
HIV <- read.csv("IHME_GBD_2010_RESULTS_2005_HIV_AIDS_Y2013M11D15.csv", header=T)
head(HIV)
tail(HIV)

HIV$causelevel1 <- NULL #remove unnecessary columns
HIV$causelevel2 <- NULL
HIV$causelevel3 <- NULL
#HIV$causelevel5 <- NULL
HIV$region <- NULL
HIV$nm_upper <- NULL
HIV$nm_lower <- NULL
HIV$pc_mean <- NULL
HIV$pc_upper <- NULL
HIV$pc_lower <- NULL
HIV$rt_mean <- NULL
HIV$rt_upper <- NULL
HIV$rt_lower <- NULL
head(HIV)
nrow(HIV)
HIV_subset <- HIV[which(HIV$age_name=='Age-standardized' & HIV$sex=='Both sexes' & HIV$measure=='daly' & HIV$causelevel5==''),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(HIV_subset)
nrow(HIV_subset)
HIV_subset$causelevel4 <- NULL
HIV_subset$causelevel5 <- NULL
HIV_subset$age_name <- NULL
HIV_subset$sex <- NULL
HIV_subset$measure <- NULL
HIV_subset$year <- NULL
head(HIV_subset)
colnames(HIV_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(HIV_subset)[2] <- "hivaids05_nm_mean"
head(HIV_subset)
nrow(HIV_subset)
#collapse HIV data to get total HIV/AIDS related DALYs (including those cases resulting in mycobacterial infection, and those resulting in other diseases) -- could also only include HIV/AIDS DALY's from HIV itself
#HIV_subset <- summaryBy(hivaids05_nm_mean ~ NAME_LONG, data=HIV_subset, FUN=sum, na.rm=F)
#head(HIV_subset)
#nrow(HIV_subset)

######### whooping cough
Whoop <- read.csv("IHME_GBD_2010_RESULTS_2005_WHOOPING_COUGH_Y2013M11D15.csv", header=T)
head(Whoop)
tail(Whoop)

Whoop$causelevel1 <- NULL #remove unnecessary columns
Whoop$causelevel2 <- NULL
Whoop$causelevel3 <- NULL
Whoop$causelevel5 <- NULL
Whoop$region <- NULL
Whoop$nm_upper <- NULL
Whoop$nm_lower <- NULL
Whoop$pc_mean <- NULL
Whoop$pc_upper <- NULL
Whoop$pc_lower <- NULL
Whoop$rt_mean <- NULL
Whoop$rt_upper <- NULL
Whoop$rt_lower <- NULL
head(Whoop)
nrow(Whoop)
Whoop_subset <- Whoop[which(Whoop$age_name=='Age-standardized' & Whoop$sex=='Both sexes' & Whoop$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Whoop_subset)
nrow(Whoop_subset)
Whoop_subset$causelevel4 <- NULL
Whoop_subset$age_name <- NULL
Whoop_subset$sex <- NULL
Whoop_subset$measure <- NULL
Whoop_subset$year <- NULL
head(Whoop_subset)
colnames(Whoop_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Whoop_subset)[2] <- "whoop05_nm_mean"
head(Whoop_subset)

######### varicella
Var <- read.csv("IHME_GBD_2010_RESULTS_2005_VARICELLA_Y2013M11D15.csv", header=T)
head(Var)
tail(Var)

Var$causelevel1 <- NULL #remove unnecessary columns
Var$causelevel2 <- NULL
Var$causelevel3 <- NULL
Var$causelevel5 <- NULL
Var$region <- NULL
Var$nm_upper <- NULL
Var$nm_lower <- NULL
Var$pc_mean <- NULL
Var$pc_upper <- NULL
Var$pc_lower <- NULL
Var$rt_mean <- NULL
Var$rt_upper <- NULL
Var$rt_lower <- NULL
head(Var)
nrow(Var)
Var_subset <- Var[which(Var$age_name=='Age-standardized' & Var$sex=='Both sexes' & Var$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Var_subset)
nrow(Var_subset)
Var_subset$causelevel4 <- NULL
Var_subset$age_name <- NULL
Var_subset$sex <- NULL
Var_subset$measure <- NULL
Var_subset$year <- NULL
head(Var_subset)
colnames(Var_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Var_subset)[2] <- "var05_nm_mean"
head(Var_subset)

######### typhoid
Typh <- read.csv("IHME_GBD_2010_RESULTS_2005_TYPHOID_AND_PARATYPHOID_FEVERS_Y2013M11D15.csv", header=T)
head(Typh)
tail(Typh)

Typh$causelevel1 <- NULL #remove unnecessary columns
Typh$causelevel2 <- NULL
Typh$causelevel3 <- NULL
Typh$causelevel5 <- NULL
Typh$region <- NULL
Typh$nm_upper <- NULL
Typh$nm_lower <- NULL
Typh$pc_mean <- NULL
Typh$pc_upper <- NULL
Typh$pc_lower <- NULL
Typh$rt_mean <- NULL
Typh$rt_upper <- NULL
Typh$rt_lower <- NULL
head(Typh)
nrow(Typh)
Typh_subset <- Typh[which(Typh$age_name=='Age-standardized' & Typh$sex=='Both sexes' & Typh$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Typh_subset)
nrow(Typh_subset)
Typh_subset$causelevel4 <- NULL
Typh_subset$age_name <- NULL
Typh_subset$sex <- NULL
Typh_subset$measure <- NULL
Typh_subset$year <- NULL
head(Typh_subset)
colnames(Typh_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Typh_subset)[2] <- "typh05_nm_mean"
head(Typh_subset)

######### tuberculosis
Tub <- read.csv("IHME_GBD_2010_RESULTS_2005_TUBERCULOSIS_Y2013M11D15.csv", header=T)
head(Tub)
tail(Tub)

Tub$causelevel1 <- NULL #remove unnecessary columns
Tub$causelevel2 <- NULL
Tub$causelevel3 <- NULL
Tub$causelevel5 <- NULL
Tub$region <- NULL
Tub$nm_upper <- NULL
Tub$nm_lower <- NULL
Tub$pc_mean <- NULL
Tub$pc_upper <- NULL
Tub$pc_lower <- NULL
Tub$rt_mean <- NULL
Tub$rt_upper <- NULL
Tub$rt_lower <- NULL
head(Tub)
nrow(Tub)
Tub_subset <- Tub[which(Tub$age_name=='Age-standardized' & Tub$sex=='Both sexes' & Tub$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Tub_subset)
nrow(Tub_subset)
Tub_subset$causelevel4 <- NULL
Tub_subset$age_name <- NULL
Tub_subset$sex <- NULL
Tub_subset$measure <- NULL
Tub_subset$year <- NULL
head(Tub_subset)
colnames(Tub_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Tub_subset)[2] <- "tuberc05_nm_mean"
head(Tub_subset)

######### trachoma
Trach <- read.csv("IHME_GBD_2010_RESULTS_2005_TRACHOMA_Y2013M11D15.csv", header=T)
head(Trach)
tail(Trach)

Trach$causelevel1 <- NULL #remove unnecessary columns
Trach$causelevel2 <- NULL
Trach$causelevel3 <- NULL
Trach$causelevel5 <- NULL
Trach$region <- NULL
Trach$nm_upper <- NULL
Trach$nm_lower <- NULL
Trach$pc_mean <- NULL
Trach$pc_upper <- NULL
Trach$pc_lower <- NULL
Trach$rt_mean <- NULL
Trach$rt_upper <- NULL
Trach$rt_lower <- NULL
head(Trach)
nrow(Trach)
Trach_subset <- Trach[which(Trach$age_name=='Age-standardized' & Trach$sex=='Both sexes' & Trach$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Trach_subset)
nrow(Trach_subset)
Trach_subset$causelevel4 <- NULL
Trach_subset$age_name <- NULL
Trach_subset$sex <- NULL
Trach_subset$measure <- NULL
Trach_subset$year <- NULL
head(Trach_subset)
colnames(Trach_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Trach_subset)[2] <- "trach05_nm_mean"
head(Trach_subset)

######### measles
Measles <- read.csv("IHME_GBD_2010_RESULTS_2005_MEASLES_Y2013M11D15.csv", header=T)
head(Measles)
tail(Measles)

Measles$causelevel1 <- NULL #remove unnecessary columns
Measles$causelevel2 <- NULL
Measles$causelevel3 <- NULL
Measles$causelevel5 <- NULL
Measles$region <- NULL
Measles$nm_upper <- NULL
Measles$nm_lower <- NULL
Measles$pc_mean <- NULL
Measles$pc_upper <- NULL
Measles$pc_lower <- NULL
Measles$rt_mean <- NULL
Measles$rt_upper <- NULL
Measles$rt_lower <- NULL
head(Measles)
nrow(Measles)
Measles_subset <- Measles[which(Measles$age_name=='Age-standardized' & Measles$sex=='Both sexes' & Measles$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Measles_subset)
nrow(Measles_subset)
Measles_subset$causelevel4 <- NULL
Measles_subset$age_name <- NULL
Measles_subset$sex <- NULL
Measles_subset$measure <- NULL
Measles_subset$year <- NULL
head(Measles_subset)
colnames(Measles_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Measles_subset)[2] <- "meas05_nm_mean"
head(Measles_subset)

######### leprosy
Lep <- read.csv("IHME_GBD_2010_RESULTS_2005_LEPROSY_Y2013M11D15.csv", header=T)
head(Lep)
tail(Lep)

Lep$causelevel1 <- NULL #remove unnecessary columns
Lep$causelevel2 <- NULL
Lep$causelevel3 <- NULL
Lep$causelevel5 <- NULL
Lep$region <- NULL
Lep$nm_upper <- NULL
Lep$nm_lower <- NULL
Lep$pc_mean <- NULL
Lep$pc_upper <- NULL
Lep$pc_lower <- NULL
Lep$rt_mean <- NULL
Lep$rt_upper <- NULL
Lep$rt_lower <- NULL
head(Lep)
nrow(Lep)
Lep_subset <- Lep[which(Lep$age_name=='Age-standardized' & Lep$sex=='Both sexes' & Lep$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Lep_subset)
nrow(Lep_subset)
Lep_subset$causelevel4 <- NULL
Lep_subset$age_name <- NULL
Lep_subset$sex <- NULL
Lep_subset$measure <- NULL
Lep_subset$year <- NULL
head(Lep_subset)
colnames(Lep_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Lep_subset)[2] <- "lep05_nm_mean"
head(Lep_subset)

######### diptheria
Dip <- read.csv("IHME_GBD_2010_RESULTS_2005_DIPHTHERIA_Y2013M11D15.csv", header=T)
head(Dip)
tail(Dip)

Dip$causelevel1 <- NULL #remove unnecessary columns
Dip$causelevel2 <- NULL
Dip$causelevel3 <- NULL
Dip$causelevel5 <- NULL
Dip$region <- NULL
Dip$nm_upper <- NULL
Dip$nm_lower <- NULL
Dip$pc_mean <- NULL
Dip$pc_upper <- NULL
Dip$pc_lower <- NULL
Dip$rt_mean <- NULL
Dip$rt_upper <- NULL
Dip$rt_lower <- NULL
head(Dip)
nrow(Dip)
Dip_subset <- Dip[which(Dip$age_name=='Age-standardized' & Dip$sex=='Both sexes' & Dip$measure=='daly'),] # subset to only relevant rows (i.e. age standardized daly's for both sexes, by country)
head(Dip_subset)
nrow(Dip_subset)
Dip_subset$causelevel4 <- NULL
Dip_subset$age_name <- NULL
Dip_subset$sex <- NULL
Dip_subset$measure <- NULL
Dip_subset$year <- NULL
head(Dip_subset)
colnames(Dip_subset)[1] <- "NAME_LONG" #change column names to match Chelsea's ptrsb data
colnames(Dip_subset)[2] <- "dipth05_nm_mean"
head(Dip_subset)

#######################
## Merge 05 datasets:
#######################

DALYs_05 <- merge(YF_subset, Schisto_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Rabies_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Onch_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Mal_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, LF_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Leish_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, HW_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Trich_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Asc_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, FBT_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Ech_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Den_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Cyst_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Chagas_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, AT_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, HepA_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, HepB_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, HepC_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, HIV_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Whoop_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Var_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Typh_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Tub_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Trach_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Measles_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Lep_subset, by="NAME_LONG", all.x=T, all.y=T)
DALYs_05 <- merge(DALYs_05, Dip_subset, by="NAME_LONG", all.x=T, all.y=T)

#An alternative to the above:
#DALYs_05 <- Reduce(function(x,y) merge(x, y, by="NAME_LONG", all.x=T, all.y=T), list(YF_subset, Schisto_subset, Rabies_subset, Onch_subset, Mal_subset, LF_subset, Leish_subset, HW_subset, Trich_subset, Asc_subset, FBT_subset, Ech_subset, Den_subset, Cyst_subset, Chagas_subset, AT_subset, HepA_subset, HepB_subset, HepC_subset, HIV_subset, Whoop_subset, Var_subset, Typh_subset, Tub_subset, Trach_subset, Measles_subset, Lep_subset, Dip_subset))

head(DALYs_05)
nrow(DALYs_05)
unique(DALYs_05$NAME_LONG)

#######################################
## Merge 05 and full Murray datasets:
#######################################

Murray_with05 <- merge(Murray, DALYs_05, by="NAME_LONG", all.x=T, all.y=T)
head(Murray_with05)
nrow(Murray)
nrow(Murray_with05)

unique(Murray$NAME_LONG)
unique(Murray_with05$NAME_LONG)

write.csv(Murray_with05, file="ptrsb_murray_data_with05.csv", row.names=F)
Murray05_test <- read.csv("ptrsb_murray_data_with05.csv", header=T)
head(Murray05_test)


