# Load packages
library(tidyverse)
library(dplyr)
library(readxl)
library(vegan)
library(ggplot2)

rm(list = ls())
cruise <- "2024_02_Nova Canton_SUB"

data.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_02_Nova Canton/Data/Video Analysis/4_Data output products/EM Export_Sub") #Denise
metadata.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/#Video Analysis Documents") #Denise
export.dir=("C:/Users/00113122/UWA/EXT-MUDSRC - Documents/Cruises/2024_02_Nova Canton/Data/Video Analysis/1_Morphotaxa Screening/Morphotaxa_QC")

setwd(data.dir)

dat<-read.delim("ALL Point measurements.txt",header=T,sep = "\t", skip = 4)

depcounts_sub<-dat%>%
  dplyr::group_by(OpCode,Period,Family,Genus,X.8,Species,Number,Time..mins.,Comment,Comment.1)%>% 
  dplyr::mutate(Number=as.numeric(Number))%>%
  filter(Number != "NA")%>%#only include things with number 
  #summarise(
   # Count = sum(replace_na(Number, 1)),  # Run this summarise to also include things with number removed (e.g. forams)
    #.groups = "drop"
  #) %>%
  dplyr::summarise(Count=sum(Number))%>% #run this summarise line to only have things with numbers
  filter(Period != "NA")%>% #only include things in period
  filter(Family !="")%>% #only include biological annotations
  unite("ID", Family:Species, sep= ".", #Created an 'ID' column to be able to join with higher taxonomic level obs 
        na.rm=TRUE,remove = FALSE)%>%
  mutate(
    ID = ID %>% 
      gsub("\\.{2,}", ".", .) %>%  # Replace two or more dots with a single dot
      gsub("\\.$", "", .)           # Remove any dot at the end of the ID
  )
head(depcounts_sub)



# Bring in depth data
setwd(metadata.dir)
metadata_sub <- read_excel("MASTER_Lander&Sub_Logsheet.xlsx",sheet=cruise)%>%
  rename(
    OpCode = opcode
  )

# Link biology to subdata
depcounts_env_sub<- depcounts_sub %>%
  left_join(metadata_sub, by = c("OpCode"))


depth_summary_sub <- depcounts_env_sub %>%
  group_by(ID) %>%
  summarise(
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    frequency = n_distinct(OpCode),
    .groups = "drop"
  )


# Combine with depth ranges
op_summary_sub <- depcounts_env_sub  %>%
  left_join(depth_summary_sub, by = "ID")



####
setwd(export.dir)
write.csv(op_summary,"LanderTaxa_Depth_QC_20260129.csv",row.names = FALSE)


###
# ------------------------------
# Calculate total count per dive
# ------------------------------

divecounts_summary <- depcounts_sub %>%
  select(-Period) %>%
  group_by(OpCode, ID) %>%
  summarise(Total_Count = sum(Count), .groups = "drop")

sort(unique(divecounts_summary$ID)) #list all individual IDs


# ------------------------------
# Create wide format matrix: OpCode × ID
# ------------------------------
depcounts_wide<- depcounts_sub%>%
  group_by(OpCode,ID)%>%
  summarise(Number = n(), .groups = "drop") %>%
  #reframe(Number,ID)%>%
  #data.frame() %>%
  pivot_wider(
    names_from = ID,    # Column names will be taken from the 'ID' column
    values_from = Number, # Values will be taken from the 'count' column
    #values_fn = mean,
    values_fill = list(Number = 0) #NA converted to zeros
  )%>%
  ungroup() %>%
  as.data.frame()  # convert to data.frame here to handle rownames

rownames(depcounts_wide) <- depcounts_wide$OpCode
depcounts_wide$OpCode <- NULL

write.csv(depcounts_wide,"NC_Sub_taxa_wide_20260204.csv")
