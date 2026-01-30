# Load packages
library(tidyverse)
library(dplyr)
library(readxl)
library(vegan)
library(ggplot2)

rm(list = ls())
cruise <- "2024_02_Nova Canton"


data.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_02_Nova Canton/Data/Video Analysis/4_Data output products/EM Export_Lander") #Denise
metadata.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/#Video Analysis Documents") #Denise
export.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_02_Nova Canton/Data/Video Analysis/1_Morphotaxa Screening/Morphotaxa_QC")

setwd(data.dir)

dat<-read.csv("ALL Point measurements.csv",header=T,skip=4)
depcounts<-dat%>%
  dplyr::group_by(OpCode,Period,Family,Genus,X.8,Species,Number,Time..mins.,Comment,Comment.1)%>% 
  dplyr::mutate(Number=as.numeric(Number))%>%
  filter(Number != "NA")%>%#only include things with number 
  summarise(
    Count = sum(replace_na(Number, 1)),  # Run this summarise to also include things with number removed (e.g. forams)
    .groups = "drop"
  ) %>%
  #dplyr::summarise(Count=sum(Number))%>% #run this summarise line to only have things with numbers
  #filter(Period != "NA")%>% #only include things in period
  filter(Family !="")%>% #only include biological annotations
  unite("ID", Family:Species, sep= ".", #Created an 'ID' column to be able to join with higher taxonomic level obs 
        na.rm=TRUE,remove = FALSE)%>%
  mutate(
    ID = ID %>% 
      gsub("\\.{2,}", ".", .) %>%  # Replace two or more dots with a single dot
      gsub("\\.$", "", .)           # Remove any dot at the end of the ID
  )
head(depcounts)

sum(depcounts$Count)


# Bring in depth data
setwd(metadata.dir)
metadata <- read_excel("MASTER_Lander&Sub_Logsheet.xlsx",sheet=cruise)%>%
  rename(
    OpCode = opcode
  )

depcounts_env<- depcounts %>%
  left_join(metadata, by = c("OpCode"))


depth_summary <- depcounts_env %>%
  group_by(ID) %>%
  summarise(
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    frequency = n_distinct(OpCode),
    .groups = "drop"
  )


# Check unique IDs and in which OpCode they arew
depcounts_summary <- depcounts_env %>%
  select(-Period) %>%
  group_by(ID, OpCode) %>%
  summarise(Total_Count = sum(Count), .groups = "drop") %>%
  group_by(ID) %>%
  summarise(
    Total_Count = sum(Total_Count),
    OpCodes_List = paste(unique(OpCode), collapse = ", "),
    .groups = "drop"
  )

# Combine with depth ranges
op_summary <- depcounts_summary  %>%
  left_join(depth_summary, by = "ID")



####
setwd(export.dir)
write.csv(op_summary,"LanderTaxa_Depth_QC_20260129.csv",row.names = FALSE)


###
usethis::use_git()
usethis::use_git_config(
  user.name  = "DJBSwan",
  user.email = "denise.swanborn@uwa.edu.au"
)
getwd()
gert::git_status()
gert::git_add(".")
gert::git_commit("Initial commit")
gert::git_remote_list()
gert::git_remote_add(name = "origin", url = "https://github.com/MUDSRC/NC_Morphotaxa_QC.git")
gert::git_push(remote = "origin")


gert::git_remote_remove("origin")
gert::git_remote_add(name = "origin", url = "git@github.com:MUDSRC/NC_Morphotaxa_QC.git")
gert::git_remote_list()
gert::git_ssh_keygen()
usethis::use_ssh_key()
