library(tidyverse)
library(busdater)
library(CDECRetrieve)
require(randomForest)
require(quantregForest)
require(zoo)
require(caret)

#Source model setup and training script, requires "ITMData.rda" to be in same folder
source(here("R/WR_Model_Setup.r"))

getwaterDay <- readRDS('Code/waterDay.rds')
wyWeeks <- data.frame(Date = seq(as.Date('2024-01-01'), Sys.Date(), 1)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(Week = format(min(Date), "%b-%d"))

#WR Loss table from SacPAS and clean up
loss <- read.csv(paste0('https://www.cbr.washington.edu/sacramento/data/php/rpt/juv_loss_detail.php?sc=1&outputFormat=csv&year='
                         ,get_fy(Sys.Date(), opt_fy_start = '10-01'),'&species=1%3Af&dnaOnly=no&age=no')) %>% 
  mutate(Date = as.Date(Sample.Time)) %>%
  filter(Adipose.Clip == 'Unclipped', LAD.Race == 'Winter') 

WRloss <- loss %>%
  group_by(Date) %>%
  summarize(Loss = sum(Loss, na.rm = TRUE)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  ungroup() %>%
  group_by(wyWeek) %>% 
  summarize(Loss = sum(Loss))

#pull environmental factors from CDEC and organize into inputs for Tillotson model
sac <- cdec_query('FPT', '20', 'D', min(loss$Date), max(loss$Date)) %>% #Sacramento flows at Freeport
  filter(!is.na(parameter_value)) %>% select(Date = 3, FPT = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(sac = mean(FPT, na.rm = TRUE))

sjr <- cdec_query('VNS', '41', 'D', min(loss$Date), max(loss$Date)) %>% #San Joaquin flows at Vernalis
  filter(!is.na(parameter_value)) %>% select(Date = 3, VNS = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(sjr = mean(VNS, na.rm = TRUE))

omrMissing <- read.csv('Data/missingOMR.csv') %>% #missing OMR values from Mar21-Apr1
  mutate(Date = as.Date(Date, "%m/%d/%Y"))
omr <- cdec_query('OMR', '41', 'D', min(loss$Date), max(loss$Date)) %>% #Old and Middle River flows
  filter(!is.na(parameter_value)) %>% select(Date = 3, OMR = 5) %>%
  mutate(Date = as.Date(Date)) %>%
  bind_rows(omrMissing) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(omr = mean(OMR, na.rm = TRUE))

cvp <- cdec_query('TRP', '70', 'D', min(loss$Date), max(loss$Date)) %>% #combined CVP and SWP exports
  filter(!is.na(parameter_value)) %>% select(Date = 3, TRP = 5)
exports <- cdec_query('HRO', '70', 'D', min(loss$Date), max(loss$Date)) %>% 
  filter(!is.na(parameter_value)) %>% select(Date = 3, HRO = 5) %>%
  left_join(cvp, by = 'Date') %>% 
  mutate(Flow = HRO + TRP) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date((waterDay(Date)-1), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% 
  summarize(exports = mean(Flow))

mal <- cdec_query('MAL', '25', 'H', min(loss$Date), max(loss$Date)) %>% #temp from mallard
  mutate(Date = as.Date(datetime)) %>%
  filter(!is.na(parameter_value)) %>% mutate(Date = as.Date(Date)) %>%
  mutate(wyWeek = week(as.Date(waterDay(Date), origin = as.Date("2024-01-01")))) %>%
  group_by(wyWeek) %>% summarize(mal = (mean(parameter_value)-32)*(5/9)) %>% na.omit()

#put covariates into dataframe for Tillotson
tillotson <- exports %>% left_join(WRloss, by = 'wyWeek') %>% 
  left_join(sac, by = 'wyWeek') %>% 
  left_join(sjr, by = 'wyWeek') %>%
  left_join(omr, by = 'wyWeek') %>%
  left_join(mal, by = 'wyWeek') %>%
  mutate(Loss = if_else(is.na(Loss), 0, Loss))

#run Tillotson model
tillotsonList <- list() #list for storing outputs

for(i in 1:nrow(tillotson)){
  NewData <- data.frame(
    wy_week = tillotson$wyWeek[i] + 1,
    mal_temp = tillotson$mal[i], 
    precip = 1,
    OMR = tillotson$omr[i],
    sac = tillotson$sac[i],
    sjr = tillotson$sjr[i],
    dcc = "closed",
    daily_exports = tillotson$exports[i],
    winter.pw = tillotson$Loss[i]
  )
  predictions <- data.frame(predict(WR_Simple_Combined[[2]], newdata = NewData, what = c(0.25, 0.5, 0.75))) %>%
    mutate(wyWeek = tillotson$wyWeek[i], OMR = tillotson$omr[i], Exports = tillotson$exports[i], 
           SAC = tillotson$sac[i], SJR = tillotson$sjr[i], Loss = tillotson$Loss[i])
  tillotsonList[[i]] <- predictions
}

PredLoss <- bind_rows(tillotsonList) %>% 
  left_join(wyWeeks, by = 'wyWeek') %>%
  rename('Qtl25' = 1, 'median' = 2, 'Qtl75' = 3, 'ObsLoss' = 9) %>%
  mutate(Week = fct_inorder(factor(Week), ordered = NA)) %>%
  mutate(OMR2 = ((OMR*-3) + 5000)/30)

tillGraph <- ggplot(PredLoss) +
  # geom_line(aes(y = median), color = 'steelblue2') +
  #geom_ribbon(aes(ymin = Qtl25, ymax = Qtl75), fill = 'steelblue2', alpha = .4) +
  geom_line(aes(x = Week, y = OMR2, group = 1), linetype = 'dashed', linewidth = 1) +
  geom_crossbar(aes(x = Week, y = median, ymin = Qtl25, ymax = Qtl75, fill = median), color = 'black', alpha = .5) +
  geom_point(aes(x = Week, y = ObsLoss), 
             shape = 8, color = 'black', 
             size = 2.5, alpha = .75, stroke = 1.5) +
  scale_y_continuous(
    sec.axis = sec_axis(~ (5000 - (. * 30)) / 3, name = "OMRI")
  ) +
  labs(y = 'Weekly Loss') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = ggplot2::margin(0.5,0.5,0.25,0.25, unit = 'cm'),
        axis.title.x = element_text(margin=ggplot2::margin(t=10)),
        axis.title.y = element_text(margin=ggplot2::margin(r=10)),
        axis.title.y.right = element_text(margin=ggplot2::margin(l=10)),
        legend.position = 'none') +
  scale_fill_distiller(palette = 'Reds', direction = 1)
tillGraph
view(theme_minimal)
ggsave(tillGraph, file = 'Viz_Output/tillotson.png', units = 'px', width = 2250, height = 1250)
