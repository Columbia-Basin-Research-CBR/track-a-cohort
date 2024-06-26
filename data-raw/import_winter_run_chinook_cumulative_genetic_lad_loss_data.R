#' import  genetic loss data (shared by BOR), loss data from SacPas, and hydrological water year type
#'  @description cumulative loss for genetic and lad loss data with appended hydrological year type. Genetic data is not QA/QC'd prior to 2020 (data source: BOR)
#'  @returns writes a .rda to the data folder including wrangled annual jpe estimate data 



#import loss  data from SacPas
url<- "https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=default&year=all&species=1%3Af&dnaOnly=no&age=no"

#filter to include only winter-run chinook for LAD or winter-run for DNA
loss_wch<- read.csv(url) %>%
  filter(LAD.Race == 'Winter'| DNA.Race == 'Winter') 

#adjust imported loss to WY 
df_loss <- loss_wch %>% 
  select(Sample.Time, Facility, LAD.Race, DNA.Race,nfish, Loss) %>%
  mutate(Date = as.Date(Sample.Time)) %>%
  arrange(Date) %>%
  mutate(WY = year(Date) + (month(Date) >= 10),
         wDay = if_else(month(Date) >= 10, yday(Date) - 273, yday(Date) + 92),
         doy = yday(Date),
         CY = year(Date),
         wDate = if_else(month(Date) >= 10, Date + years(1), Date))

#pull in shared wytype.csv-- look to automate this process (in progress MC^2)
wytype <- read_csv(here::here('track-a-cohort/shared files/WYtype.csv')) %>% filter(Basin == "SacramentoValley")

df_loss %>% 
  filter()

cumulativeLAD <- df_loss %>% 
  filter(LAD.Race == 'Winter') %>% 
  group_by(WY) %>%
  mutate(cumloss = cumsum(Loss)) %>%
  #designate pre/post 2009 BiOp
  mutate(Status = case_when(WY < 2009 ~ 'Pre-2009 BiOp\n(1994 to 2008)',
                            WY > 2008 ~ '2009 & 2019 BiOp\n(2009 to present)')) %>%
  #set order of factor levels
  mutate(Status = factor(Status, levels = c('Pre-2009 BiOp\n(1994 to 2008)', '2009 & 2019 BiOp\n(2009 to present)'))) %>% 
  #join with wytype to designate wet/dry year (Hydrologic Classification Index?)
  left_join(select(wytype,WY, "hydro_type" = `Yr-type`) , by = 'WY') %>%
  #remove date na's
  filter(!is.na(Date)) %>%
  # #label historical versus current year
  # mutate(History = if_else(WY == '2024', 'Current', 'Past')) %>%
  #relabel wytype Yr.type to Wet/Dry -- add all?
  mutate( hydro_type = factor(hydro_type, levels = c("W", "AN", "BN", "D", "C"), labels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical")),
          hydro_type_grp = case_when(
            hydro_type %in% c("Wet", "Above Normal") ~ "Wet, Above Normal",
            hydro_type %in% c("Below Normal", "Dry", "Critical") ~ "Below Normal, Dry, & Critical" )
  )

