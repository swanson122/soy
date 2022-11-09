#clean environment
rm(list = ls())

#load necessary packages
library(tidyverse)
library(magrittr)
install.packages("xlsx")
library(xlsx)
install.packages("stringi")
#set working directory
setwd("C:/Users/swans/OneDrive - The Ohio State University/Soybeanbase")

checks <- read.csv("2019_checks.csv", header = T)
checks_2019 <- unique(checks$Name)
write.csv(checks_2019, "checks_2019.csv")

#read in complete phenotypic data to draw accession info from
OSU_2019 <- as_tibble(read.csv("Full_OSU_2019.csv", header = TRUE))

#pulling unique names from 2019 trials and storing as tibble
accessions_2019 <- as_tibble(unique(OSU_2019$Name))
#changing column name to match soybeanbase format
colnames(accessions_2019)[1] <- "accession_name"
#adding species_name column to match soybeanbase format
accessions_2019 %<>% mutate(species_name = "Glycine max")

#write file for upload to soybeanbase, contains all accessions evaluated in 2019
write.csv(accessions_2019, "accessions_2019.csv")

#separating trials with different experimental designs, opt trials use and augmented design the rest of trials use a RCBD
#they can be separated either by trial name (contains "opt"), or if the value in the Block column in NA (non-opt trials) or not
OSU_2019_opt <- filter(OSU_2019, grepl("opt", Trial)) #filter(OSU_2019, !is.na(Block))
OSU_2019_non_opt <- filter(OSU_2019, !grepl("opt", Trial)) #filter(OSU_2019, is.na(Block))

#preparing non OPT trials for upload to soybeanbase

OSU_2019_non_opt %<>% mutate(block_number = Rep)

OSU_2019_non_opt %<>% mutate(plot_name = paste(Trial, Year, Location, block_number, Plot, sep = "_"),
                        accession_name = Name, 
                        plot_number = if_else(Plot < 10, paste(Rep, Plot, sep = "00"), paste(Rep, Plot, sep = "0")),
                        block_number = Rep, 
                        is_a_control = if_else(Name %in% checks_2019, "1", ""))
OSU_2019_non_opt %<>% mutate(accession_name = gsub("^M", "HM", accession_name), rep_number = "", range_number = "", 
                             row_number = "", col_number = "", seedlot_name = "", num_seed_per_plot = "",
                             weight_gram_seed_per_plot = "")
alta_WE <- filter(OSU_2019_non_opt, grepl("alta", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
        "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
        "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

#writing the .xls NOT .xlsx file that will be uploaded to soybean base 
write.xlsx(as.data.frame(alta_WE), "alta_we.xls", row.names = FALSE)

#### ALTB
altb_WO <- filter(OSU_2019_non_opt, grepl("altb", plot_name) & grepl("WO", plot_name)) [, c("plot_name", "accession_name",
                 "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(altb_WO), "altb_wo.xls", row.names = FALSE)

altb_WE <- filter(OSU_2019_non_opt, grepl("altb", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
                  "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(altb_WE), "altb_we.xls", row.names = FALSE)





### SLTA
slta_WO <- filter(OSU_2019_non_opt, grepl("slta", plot_name) & grepl("WO", plot_name)) [, c("plot_name", "accession_name",
                  "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(slta_WO), "Slta_wo.xls", row.names = FALSE)

slta_WE <- filter(OSU_2019_non_opt, grepl("slta", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
                  "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(slta_WE), "Slta_we.xls", row.names = FALSE)

#### SLTB
sltb_WO <- filter(OSU_2019_non_opt, grepl("sltb", plot_name) & grepl("WO", plot_name)) [, c("plot_name", "accession_name",
                 "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(sltb_WO), "Sltb_wo.xls", row.names = FALSE)

sltb_WE <- filter(OSU_2019_non_opt, grepl("sltb", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
                  "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]

write.xlsx(as.data.frame(sltb_WE), "Sltb_we.xls", row.names = FALSE)

#ifelse(substr(alta_WE$accession_name, 1, 1) == "M"), 

#### for OPT augmented design
block_key <- tibble(input = c ("A", "B", "C", "D", "E"), output = c(1, 2, 3, 4, 5))


#need to remove optblk trial for naming scheme, because it has not blocks

OSU_2019_optblk <- filter(OSU_2019_opt, grepl("optblk", Trial))
OSU_2019_opt %<>% filter(!grepl("optblk", Trial))

test <- mutate(block_number = sapply(Block, function(block) { loca <- block_key[['output']][sapply(block_key[['input']], grepl, block)]}), 
       .before = Name)

#nesting gsub seems to be best way to change multiple values in column
gsub("E", 5, gsub("D", 4, gsub("C", 3, gsub("B", 2, gsub("A", 1, OSU_2019_opt$Block)))) )

mutate(OSU_2019_opt, Block = gsub("E", 5, gsub("D", 4, gsub("C", 3, gsub("B", 2, gsub("A", 1, Block)))) ) )

OSU_2019_opt %<>% mutate(block_number = gsub("E", 5, gsub("D", 4, gsub("C", 3, gsub("B", 2, gsub("A", 1, Block)))) ) )
test <- OSU_2019_opt %>% 
  mutate(plot_name = paste(Trial, Year, Location, block_number, Plot, sep = "_"),
         accession_name = Name,
          plot_number = if_else(Plot > 99, paste(block_number, Plot, sep = ""), 
                                              if_else(Plot > 9, paste(block_number, Plot, sep = "0"), 
                                                      paste(block_number, Plot, sep = "00"))),
         rep_number = Rep, 
         is_a_control = if_else(Name %in% checks_2019, "1", ""))         
#edit for OPT
OSU_2019_non_opt %<>% mutate(accession_name = gsub("^M", "HM", accession_name), rep_number = "", range_number = "", 
                             row_number = "", col_number = "", seedlot_name = "", num_seed_per_plot = "",
                             weight_gram_seed_per_plot = "")
alta_WE <- filter(OSU_2019_non_opt, grepl("alta", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
                                                                                            "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                                                                                            "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]


#writing function for multi trial file upload
test <- OSU_2019_opt
test %<>%
  mutate(trial_name = paste(toupper(Trial), toupper(Location), Year, sep = "_")) %>%
  mutate(breeding_program = "OSU-Soybean") %>%
  mutate(location = case_when(Location == 'WO' ~ 'Wooster-OH',
                                         Location == 'WE' ~ 'Western-OH',
                                         Location == 'NW' ~ 'Northwest-OH')) %>%
  mutate(year = Year) %>%
  mutate(design_type = case_when(grepl('opt', Trial) ~ 'Augmented',
                                 !grepl('opt', Trial) ~ 'RCBD')) %>%
  mutate(description = paste(Year, toupper(Trial), 'trial', location, 'location', sep = ' ')) %>%
  mutate(trial_type = case_when(grepl('opt', Trial) ~ 'Preliminary Yield Trial',
                                !grepl('opt', Trial) ~ 'Advanced Yield Trial')) %>%
  mutate(plot_width = '') %>%
  mutate(plot_length = '') %>%
  mutate(field_size = '') %>%
  mutate(planting_date = '') %>%
  mutate(harvest_date = '') %>%
  mutate(OSU_2019_opt, test = sapply(Block, function(loc) { loca <- block_key[['output']][sapply(block_key[['input']], grepl, loc)]})) %>%
  mutate(plot_name = paste(Trial, Year, Location, block_number, Rep, Plot, sep = "_")) %>%
  mutate(accession_name = gsub("^M", "HM", Name)) %>%
  mutate(plot_number = case_when(Plot > 99 ~ paste(block_number,Rep, Plot, sep = ''),
                                 Plot > 9 ~ paste(block_number, Rep, '0', Plot, sep = ''),
                                 Plot <= 9 ~ paste(block_number, Rep, '00', Plot, sep = ''))) %>%
  mutate(is_a_control = if_else(!grepl("^H", Name), '1', '0'), .before = Name) %>%
  mutate(rep_number = Rep) %>%
  mutate(range_number = '') %>%
  mutate(row_number = '') %>%
  mutate(col_number = '') %>%
  mutate(seedlot_name = '') %>%
  mutate(num_seed_per_plot = '') %>%
  mutate(weight_gram_seed_per_plot = '') 
  
testt <- test[,c('trial_name',	'breeding_program',	'location',	'year',	'design_type',	'description',	'trial_type',	'plot_width',	
                 'plot_length',	'field_size',	'planting_date',	'harvest_date',	'plot_name',	'accession_name',	'plot_number',	
                 'block_number',	'is_a_control',	'rep_number',	'range_number',	'row_number',	'col_number',	'seedlot_name',	
                 'num_seed_per_plot',	'weight_gram_seed_per_plot')]  
  

write.xlsx(as.data.frame(testt), "opt_trail.xls", row.names = FALSE)       


test <- tibble(trial_name,	breeding_program,	location,	year,	design_type,	description,	trial_type,	plot_width,	
               plot_length,	field_size,	planting_date,	harvest_date,	plot_name,	accession_name,	plot_number,	
               block_number,	is_a_control,	rep_number,	range_number,	row_number,	col_number,	seedlot_name,	
               num_seed_per_plot,	weight_gram_seed_per_plot)

locations <- tibble(input = c( 'WO', 'WE', 'NW'), output = c('Wooster-OH', 'Western-OH', 'Northwest-OH')) 

OSU_2019 %>% mutate(test = sapply(Location, function(loc) { loca <- locations[['output']][sapply(locations[['input']], grepl, loc)]}), .before = Name)

locations[['input']]

##### pheno prep
test %<>%
mutate(observationunit_name = plot_name) %>%
  mutate('Height|SY_456:0000007'= Height) %>%
  mutate('Maturity|SY_456:0000036' = Maturity + 242) %>%
  mutate('Lodging| SY_456:0000012' = Lodging) %>%
  mutate('YieldBuA|SY_456:0000027' = Yieldbua) %>%
  mutate('Seed Quality|SY_456:0000004' = SeedQual) %>%
  mutate('Protein|SY_456:0000034' = ProteinDW * 0.87) %>%
  mutate('Oil|SY_456:0000030' = OilDW * 0.87) %>%
  mutate('Palmitic|SY_456:0000011' = Palmitic) %>%
  mutate('Stearic|SY_456:0000010' = Stearic) %>%
  mutate('Linoleic|SY_456:0000028' = Linoleic) %>%
  mutate('Linolenic|SY_456:0000001' = Linolenic) %>%
  mutate('Oleic|SY_456:0000021' = Oleic) %>%
  mutate('Seed Size|SY_456:0000006' = seedwt100) %>%
  mutate('notes' = '')

test_pheno <- test[,c('observationunit_name', 'Height|SY_456:0000007',	'Maturity|SY_456:0000036',	'Lodging| SY_456:0000012',	'YieldBuA|SY_456:0000027',	'Seed Quality|SY_456:0000004',
                          'Protein|SY_456:0000034',	'Oil|SY_456:0000030',	'Palmitic|SY_456:0000011',	'Stearic|SY_456:0000010',	'Linoleic|SY_456:0000028',
                          'Linolenic|SY_456:0000001',	'Oleic|SY_456:0000021',	'Seed Size|SY_456:0000006',	'notes')]

write.xlsx(test_pheno, 'opt_2019_pheno.xls', row.names = FALSE)
           
           Maturity = Maturity + 242, YieldBuA = Yieldbua, 
       `Hilum color` = HilumColor, `Seed Quality` = SeedQual, Protein = ProteinDW * 0.87, Oil = OilDW * 0.87
)


'Height|SY_456:0000007',	'Maturity|SY_456:0000036',	'Lodging| SY_456:0000012',	'YieldBuA|SY_456:0000027',	'Seed Quality|SY_456:0000004',
'Protein|SY_456:0000034',	'Oil|SY_456:0000030',	'Palmitic|SY_456:0000011',	'Stearic|SY_456:0000010',	'Linoleic|SY_456:0000028',
'Linolenic|SY_456:0000001',	'Oleic|SY_456:0000021',	'Seed Size|SY_456:0000006',	'notes'

#################################################################################################################

#OSU_2019_non_opt %<>% mutate(plot_name = paste(Trial, Year, Location, block_number, Plot, sep = "_"),
#                            accession_name = Name, 
                             plot_number = if_else(Plot > 99, paste(Block, Plot, sep = ""), 
                                                   if_else(Plot > 9, paste(Rep, Plot, sep = "0"), 
                                                           paste(Rep, Plot, sep = "00"))),
                             rep_number = Rep, 
                             is_a_control = if_else(Name %in% checks_2019, "1", ""))
OSU_2019_non_opt %<>% mutate(accession_name = gsub("^M", "HM", accession_name), rep_number = "", range_number = "", 
                             row_number = "", col_number = "", seedlot_name = "", num_seed_per_plot = "",
                             weight_gram_seed_per_plot = "")
alta_WE <- filter(OSU_2019_non_opt, grepl("alta", plot_name) & grepl("WE", plot_name)) [, c("plot_name", "accession_name",
                  "plot_number", "block_number", "is_a_control","rep_number", "range_number", "row_number", "col_number", 
                  "seedlot_name", "num_seed_per_plot", "weight_gram_seed_per_plot")]




#this adds an "H" to the front of an accession name where the first character is "M"
mutate(alta_WE, accession_name = gsub("^M", "HM", accession_name))

gsub("^M", "HM", substr(alta_WE$accession_name, 1, 1))

write.xlsx(as.data.frame(alta_WE), "alta_we.xls", row.names = FALSE)


write.csv(checks, "test_write.csv")
write.csv(checks, "test_write_2.csv", row.names = FALSE)

#will need to filter for each trial starting with alta
alta <- filter(OSU_2019, Trial == "alta")

alta_WO <- filter(alta, Location == "WO")

alta_WO <- alta_WO[,3:9]

alta_WO %<>% mutate(block_number = Rep)
alta_WO %<>% mutate(plot_name = paste(Trial, Year, Location, block_number, Plot, sep = "_"),
                       accession_name = Name, 
                       plot_number = if_else(Plot < 10, paste(Rep, Plot, sep = "00"), paste(Rep, Plot, sep = "0")),
                       block_number = Rep, 
                       is_a_control = if_else(Name %in% checks_2019, "1", ""))

alta_WO <- alta_WO[, c("plot_name", "accession_name", "plot_number", "block_number", "is_a_control")]
#this writes the spreadsheet for trial design upload to soybeanbase
write.csv(alta_WO, "alta_WO.csv")

alta %<>% mutate(plot_name = paste(Trial, Year, Location, Rep, Plot, sep = "_"), .before = 1)

alta_wo_pheno <- filter(alta, Location == "WO")[,c("plot_name", "Maturity", "Lodging", "Height", "Yieldbua", "HilumColor",
                                                   "SeedQual", "ProteinDW", "OilDW", "Palmitic", "Stearic", "Oleic", 
                                                   "Linoleic", "Linolenic")]
alta_wo_pheno <- mutate(alta_wo_pheno, observationunit_name = plot_name, Maturity = Maturity + 242, YieldBuA = Yieldbua, 
                          `Hilum color` = HilumColor, `Seed Quality` = SeedQual, Protein = ProteinDW * 0.87, Oil = OilDW * 0.87
                          )[,c("observationunit_name", "Maturity", "Lodging", "Height", "YieldBuA", "Hilum color",
                               "Seed Quality", "Protein", "Oil", "Palmitic", "Stearic", "Oleic", 
                               "Linoleic", "Linolenic")]

write.xlsx(alta_wo_pheno, "alta_wo_pheno.xls")

mutate(alta_wo_pheno, `Hilum color` = HilumColor, .before = 1)

dat <- read_excel("ALTA_WO_2019_pheno.xls")
datt <- alta_wo_pheno[,c("observationunit_name", "YieldBuA")]

dat <- left_join(dat, alta_wo_pheno)
write.xlsx(dat, "test.xls")


left_join(alta)

alta <- alta[, -1]

tstr <- mutate(alta_trial, is_a_control = if_else(Name %in% checks_2019, "1", ""))


write.xlsx(alta_WO, "test.xls")

#pulling unique accessions from alta
alta_accessions <- as_tibble(unique(alta$Name))

write.csv(alta_accessions, "alta_accessions.csv")

altb <- filter(OSU_2019, Trial == "altb")
length(unique(altb$Name))

slta <- filter(OSU_2019, Trial == "slta")
length(unique(slta$Name))

sltb <- filter(OSU_2019, Trial == "sltb")
length(unique(sltb$Name))

opta <- filter(OSU_2019, Trial == "opta")
length(unique(opta$Name))

optb <- filter(OSU_2019, Trial == "optb")
length(unique(optb$Name))

optblk <- filter(OSU_2019, Trial == "optblk")
length(unique(optblk$Name))

optc <- filter(OSU_2019, Trial == "optc")
length(unique(optc$Name))


Trials_sep <- function(x, y){
  as.symbol(paste(x)) <- filter(y, Trial == paste(x))
}



