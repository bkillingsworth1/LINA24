# Use Census data to characterize high and low usage customers on the CARE rate in CA for LINA
## identify how many people are eligible for the program, what are their characteristics 
#### packages ####
library(dplyr)
library(writexl)
library(scales)
library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(stringi)
library(tidycensus)
library(sf)

# Append CEC CZ and IOU service territory from 2022 LINA lookup tables

# Assign CZ groups: Table 3 of the research plan, these are groups of CEC climate zones

# Evergreen HDD/CDD Category: High-Low; CA Zones 1-5,16
# Evergreen HDD/CDD Category: Low-Low; CA Zones 6-8
# Evergreen HDD/CDD Category: Low-High; CA Zones 9,10,15
# Evergreen HDD/CDD Category: High-High; CA Zones 11-14


#Eligibility
## 200% FPL or less
## remove master-metered customers (who don't pay electricity directly)
## remove net-energy metering (i.e. onsite solar generation)

## filter to the top 90% and bottom 10% of annual electric and natural gas costs (combined) within each CZ group by IOU (among those that have reported costs)
#create indicator for top 90% (high) and bottom 10% (low) based on percentiles 
#Summarize prevalence of ___ among high and low users, separately

# 1+ children (e.g. 60% of low users compared to 80% of high users have at least one child) - AGEP
# 1+ senior - AGEP
# occupancy - NP
# disabled - DIS 
# veteran (has VA healthcare or ever served) - VPS
# own vs. rent - TEN
# home size - RMSP
# home type (SF, MF, mobile/manufactured) - BLD
# home age - YBL
# energy burden (annual bill $ / total annual income) for electric, gas, and all fuels combined - HINCP,ELEP,GASP,FULP
# heating fuel (natural gas, electricity, other, or no heating) - HFL
# multi-generational household (children + senior + working-age) - AGEP
# if time permits: 
    # Any English or Spanish speakers vs. no Eng or Span - ENG
    # categorical eligibility (i.e., receives other public assistance) - FS
    # receives Indian Health Services (IHS) - HINS4
    # estimated home value (if owned) - TEN, VALP
    # highest educational degree attained (by any person within the household) - SCHL



# pull variables of interest -----
## to calculate % you need to use the weighted number of the individual (PWGTP) or the household (WGTP), depending on the "level"
pums_vars <- pums_variables %>%
  filter(year == 2019 & survey == "acs5")

# determine vars needed for eligibility (puma, income, electricity payment, net-energy metering)
## 200% FPL or less
## remove master-metered customers (who don't pay electricity directly)
## remove net-energy metering (i.e. onsite solar generation)

elig_vars <- c("PUMA","HINCP","ELEFP","HFL") #probably don't need PUMA because pulling statewide 
               
# these vars are ones that want to characterize
compare_vars <- c("AGEP","NP","DIS","VPS","TEN","RMSP","BLD","YBL","HINCP","ELEP","GASP","FULP","HFL","ENG","FS","HINS4","TEN","VALP","SCHL")
#left off here, pull might just be too large and need to reduce or breakout 


#pull PUMS data
my_pums_vars <- c(elig_vars,compare_vars)
statevar <- "CA"
year <- 2020

my_pums <- get_pums(
  variables = my_pums_vars,
  state = statevar,
  survey = "acs5",
  year = year,
  key = "623020359418e43f907eddc1c27bbf7b9814d102",
  recode = F
)

#legend
legend <- pums_vars %>%
  filter(var_code %in% my_pums_vars)

#save out files 
save(legend, my_pums, file = "~/Documents/LINA24/PUMA.Rdata")

#load pulled data -----
load("/Volumes/Projects/477001 - MCE LIFT Process Eval/Analysis/PUMA.Rdata")

pums_2020 <- my_pums %>%
  mutate(year = substr(SERIALNO,1,4)) %>%
  filter(year == "2020") %>%
  filter(TYPEHUGQ == 1) %>% #filter to just housing units (no institutional or non-institutional)
  filter(BLD != "bb") #filter out unknown home types 

pums_household <- pums_2020 %>%
  distinct(SERIALNO,.keep_all=TRUE)

pums_population <- pums_2020

#calculate population level stats first 
population_summary <- pums_population %>%
  select(SERIALNO,SPORDER,AGEP,ENG,DIS,RACAIAN) %>% #select the individual-level stats only
  mutate(senior = ifelse(AGEP >64,T,F),
         child = ifelse(AGEP <18,T,F),
         limited_english = ifelse(ENG %in% c(3,4),T,F), #defined as ability to speak English is 3 - Not well or 4 - Not at all
         disability = ifelse(DIS == 1,T,F), #1=with a disability 
         native = ifelse(RACAIAN == 1,T,F)) %>% #American Indian and Alaska Native recode (alone or in combination with other races)
  group_by(SERIALNO) %>%
  summarise(across(c(senior,child,limited_english,disability,native),sum))

# join population summary onto the original dataset so each serial no (household) has the variables we created in population summary
pums_household_join <- pums_household %>%
  left_join(population_summary,by="SERIALNO") %>%
  mutate(inc_adj = ifelse(HINCP <= 0,0,HINCP),
         NP = as.numeric(NP))%>% # fixing income that is less than 0
  select(SERIALNO,WGTP,PUMA,NP,TEN,BLD,FS,ELEFP,TYPEHUGQ,inc_adj,senior:native) %>% #select just relevant variables
  mutate(large_family = ifelse(NP >4,T,F),#calculate household level stats 
         single_parent = ifelse(NP-child == 1 & child >0,T,F),
         pay_elec = ifelse(ELEFP == 3,T,F),
         snap = ifelse(FS == 1, T,F)) 

# Calculate %FPL - we used census FPL definition (which is slightly different than HHS definition)
## MCE LIFT uses XYZ definition - pending 

fpl_thresh<-read_xlsx("/Volumes/Projects/Common Data Files/fplthresh20.xlsx",skip=2) 

fpl_calc <- fpl_thresh%>%
  filter(!`Size of family unit` %in% c("one","two")) %>%
  select(-average) %>% 
  pivot_longer(`no children`:`eight children`) %>%
  filter(!is.na(value)) %>%
  rename(family_unit = `Size of family unit`, children = name,FPL = value)

pums_fpl <- pums_household_join %>%
  mutate(NP = as.numeric(NP),inc_adj = as.numeric(inc_adj))%>%
  mutate(family_unit = case_when(NP == 1 & senior == 1 ~ "one senior",
                                 NP == 1 & senior == 0 ~ "one non-senior",
                                 NP == 2 & senior > 0 ~ "two senior",
                                 NP == 2 & senior == 0 ~ "two non-senior",
                                 NP == 3 ~ "three",
                                 NP == 4 ~ "four",
                                 NP == 5 ~ "five",
                                 NP == 6 ~ "six",
                                 NP == 7 ~ "seven",
                                 NP == 8 ~ "eight",
                                 NP > 8 ~ "nine or more",
                                 T ~ "help"),
         children = case_when(child == 0 ~ "no children",
                              child == 1 ~ "one child",
                              child == 2 ~ "two children",
                              child == 3 ~ "three children",
                              child == 4 ~ "four children",
                              child == 5 ~ "five children",
                              child == 6 ~ "six children",
                              child == 7 ~ "seven children",
                              child >7  ~ "eight children",
                              T ~ "help")) %>%
  left_join(fpl_calc) %>%
  filter(!is.na(FPL)) %>% #dropping the children living by themselves (only 9 households)
  mutate(fpl_perc = inc_adj/FPL)

quantile(round(pums_fpl$fpl_perc,5),probs = seq(0,1,.1),na.rm = TRUE) # this is a quick check & tells us that 20% of the sample is below 188% FPL (which seems reasonable)
summary(pums_fpl$fpl_perc) # another quick check that tells us the median FPL in CA is 446% FPL (which seems reasonable)

#now, define eligibility for the program (renters, MF, 200% of FPL or less) and determine geography we care about by filtering PUMA
#list of all PUMAs in MCE territory 
# mce_pumas <- c("1301","1305","1308","1309","1310","1311","1312","1313","1314","4103","4104","5500","9501","9502","9503") this was original list of pumas, replaced 1302,1303,1304,1306,1307,4101,4102 with 2010 names, but still map to 2020 data 
mce_pumas <- c("1301","1305","1308","1309","1302","1303","1304","1306","1307","4101","4102","5500","9501","9502","9503")

# filter for marin pumas only
marin_pumas <- c("4101","4102")

# filter for non-Marin pumas 
nonmarin_pumas <- c("1301","1305","1308","1309","1302","1303","1304","1306","1307","5500","9501","9502","9503")

# run this for all, marin, non-marin separately 
our_data <- pums_fpl %>%
  filter(PUMA %in% mce_pumas) %>%  #filter to just the PUMAS we care about (MCE territory), 2772 households
  mutate(elig = ifelse(fpl_perc < 2.00 & TEN == 3 & BLD %in% c(05,06,07,08,09),1,0)) #fpl <200%, rented, 3-4 Apartments up to 50+ apartments

sum(our_data$elig,na.rm=T) # this says X homes in MCE territory PUMS are eligible (unweighted) - 217 households

sum(our_data$WGTP[our_data$elig==T]) #this says that X households are eligible once we weight it (weighted) -  7445 households 
sum(our_data$WGTP)  #141,870 households 
sum(our_data$WGTP[our_data$elig==T]) /sum(our_data$WGTP)  #this is the percentage of households that are eligible of all MCE households (weighted) - 5.2%

# summarize stats for variables of interest to find puma data eligible homes percentages
elig_summary <- our_data %>%
  filter(elig == T) %>%
  summarize(senior_count = sum(WGTP[senior>0]),
            children_count = sum(WGTP[child>0]),
            limited_english_count = sum(WGTP[limited_english>0]),
            disability_count = sum(WGTP[disability>0]),
            native_count = sum(WGTP[native>0]),
            large_family_count = sum(WGTP[large_family>0]),
            single_parent_count = sum(WGTP[single_parent>0]),
            pay_elec_count = sum(WGTP[pay_elec>0]),
            snap_count = sum(WGTP[snap>0]),
            eligible = sum(WGTP)) %>% 
  mutate(Percent_Seniors = senior_count/eligible * 100,
         Percent_Children = children_count/eligible * 100,
         Percent_Limited_English = limited_english_count/eligible * 100,
         Percent_Disability = disability_count/eligible * 100,
         Percent_Native = native_count/eligible * 100,
         Percent_Large_Fam = large_family_count/eligible * 100,
         Percent_Pay_Elec = pay_elec_count/eligible * 100,
         Percent_Single_Parent = single_parent_count/eligible * 100,
         Percent_Snap= snap_count/eligible * 100) %>%
  t() %>%
  as.data.frame()


# elig summary for all pumas 
elig_summary_with_row_names <- data.frame(Field = rownames(elig_summary), elig_summary)

# Filter rows where the 'Field' column contains "Percent", don't need counts
overall_elig_summary <- elig_summary_with_row_names %>%
  filter(grepl("Percent", Field, ignore.case = TRUE)) %>% 
  rename(Percent = V1)
overall_elig_summary[-1] <- round(overall_elig_summary[-1], 2) #round 

#save out 
write_csv(overall_elig_summary,"/Volumes/Projects/477001 - MCE LIFT Process Eval/Analysis/PUMS Eligibility/overall_elig_summary_newfpl.csv")




#marin 
# elig summary for marin pumas 
elig_summary_with_row_names <- data.frame(Field = rownames(elig_summary), elig_summary)

# Filter rows where the 'Field' column contains "Percent"
marin_elig_summary <- elig_summary_with_row_names %>%
  filter(grepl("Percent", Field, ignore.case = TRUE)) %>% 
  rename(Percent = V1)
marin_elig_summary[-1] <- round(marin_elig_summary[-1], 2)

#save out 
write_csv(marin_elig_summary,"/Volumes/Projects/477001 - MCE LIFT Process Eval/Analysis/PUMS Eligibility/marin_elig_summary_newfpl.csv")



# non marin
elig_summary_with_row_names <- data.frame(Field = rownames(elig_summary), elig_summary)

# Filter rows where the 'Field' column contains "Percent"
nonmarin_elig_summary <- elig_summary_with_row_names %>%
  filter(grepl("Percent", Field, ignore.case = TRUE)) %>% 
  rename(Percent = V1)
nonmarin_elig_summary[-1] <- round(nonmarin_elig_summary[-1], 2)

#save out 
write_csv(nonmarin_elig_summary,"/Volumes/Projects/477001 - MCE LIFT Process Eval/Analysis/PUMS Eligibility/nonmarin_elig_summary_newfpl.csv")



## Reproducable bar code
mce_pums_data <- data.frame(
  type = factor(c("Seniors","Children","Non-English Speakers","Disability","Native","Large Family","Pay for Electricity","Single Parent","SNAP")),
  percent = c(41, 38, 21, 32, 4,16,91,16,36)) %>% 
  mutate(data_source = "Income-Eligible MF Renter (Census PUMS)")

LIFT_treated <- data.frame(
  type = factor(c("Seniors","Children","Non-English Speakers","Disability","Native","Large Family","Pay for Electricity","Single Parent","SNAP")),
  percent = c(72, 19, 28, 35, 1,5,64,0,0)) %>%  #we don't know the numbers for single parent or snap (SM has)
  mutate(data_source = "LIFT Treated (Survey)")

# Reorder 'type' in mce_pums_data based on 'percent'
mce_pums_data$type <- reorder(mce_pums_data$type, -mce_pums_data$percent)
# Ensure that 'type' factor levels in LIFT_treated match those in mce_pums_data
LIFT_treated$type <- factor(LIFT_treated$type, levels = levels(mce_pums_data$type))


combined <- mce_pums_data %>% 
  bind_rows(LIFT_treated)



combined_chart <- ggplot(data = combined, aes(x = type, y = percent, fill = data_source)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + # Bar chart
  geom_text(aes(label = ifelse(percent == 0, "*", sprintf("%d%%", round(percent)))), position = position_dodge(width = 0.8), vjust = -0.25, size = 3) + # Replace 0 with "*" in labels
  # geom_text(aes(label = sprintf("%d%%", round(percent))), position = position_dodge(width = 0.8), vjust = -0.25, size = 3) + # Adding text labels with percent sign
  scale_fill_manual(values = c("Income-Eligible MF Renter (Census PUMS)" = "#095C9C", "LIFT Treated (Survey)" = "#73B633")) + # Custom fill colors
  theme_bw() + # White background theme
  theme(legend.position = "bottom", # Customizing legend, axis titles/texts, and removing X-axis label
        legend.text = element_text(size = 11),
        axis.title.x = element_blank(), # Remove X-axis label
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 7),
        legend.title = element_blank(),# Removing legend title
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_blank())

# Print the chart
print(combined_chart)


ggsave(file = "/Volumes/Projects/477001 - MCE LIFT Process Eval/Analysis/PUMS Eligibility/mce_pums_elig_newfpl.pdf", plot = combined_chart, width = 8, height = 5)

