#######################################################
#### Data Visualization on American Time Use Survey####
# Time spent at work from 2003-2022
## Programmer: Haoshu Duan   ## Date: 10/12/23
##                           ## Updated: 11/30/23
#######################################################


## Set up ----

rm(list = ls())

library(haven)
library(tidyverse)
library(purrr)
library(summarytools)
library(ggplot2)


setwd("/Users/haoshu/Desktop/atus/")
getwd()

## Read in time-use data from 2003-2022 (activity file & summary file) ----
#dt_act<-read.csv("atusact_2022.dat") 
dt_sum_0322<-read.csv("atussum-0322/atussum_0322.dat")

# Note: 2003-2022 Act summary file contains the total number of minutes each respondent spent doing 
#       each 6-dig activity. Each row corresponds to a unique respondent, as indicated by a unique variable: TUCASEID



#dt_sum<-list(dt_sum_2022, dt_sum_2021, dt_sum_2020, dt_sum_2019)

#dt_set<-lapply(dt_sum, function(x) {
#        names(x)<-tolower(names(x))
#        return(x)
#} )

  names(dt_sum_0322)<-tolower(names(dt_sum_0322))

## Define work-related activities to be used -- 
  wrk_act<-c('t050101', 't050102', 't050103', 't050189',
             't050201', 't050202', 't050203', 't050204',
             't050289', 't050301', 't050302', 't050303',
             't050304', 't050389', 't050481', 't050403',
             't050404', 't050405', 't050499', 't059999')
  
## Define self-care sleep/grooming/eating
  sleep<-c('t010101', 't010102', 't010199')
  
  self_care<-c(#'t010101', 't010102', 't010199', # sleeping
               't010201', 't010299',  # washing, dressing, and grooming self
               't010301' , 't010399', 't010401', 't010499', 't010501', 't010599', 't019999', ## personal care
               't110101' ,'t110199',  't110281',  't110289', 't119999')  ## eating and drinking


## Define paid work & education 
  wrk_edu<- c('t050101', 't050102', 't050103', 't050189',  # working as a job
              't050201', 't050202', 't050203', 't050204', 't050289', # work-related activities
              't050301', 't050302', 't050303', 't050304', 't050389', # income-generating activities
              't050481', 't050403', 't050404', 't059999', # job-searching act
              't060101', 't060102', 't060103', 't060104', 't060199', # taking classes
              't060201', 't060202', 't060203', 't060289', # extracurricular act
              't060301', 't060302', 't060303', 't060399', #reseach/homework for a degree
              't060401', 't060402', 't060403', 't060499', 't069999') # admin act for education

## Define socializing time
  # socializing w/others, attending parties/ceremonies
  
  soc_leis<-c('t120101', 't120199', 't120201', 't120202', 't120299', 
              't120501', 't120502', 't120503', 't120504', 't120599', 
              't129999')
  
  # individual leisure & hobbies: things to do without requiring big parties

  ind_leis<-c('t120301', 't120302', 't120303', 't120304', 't120305', 
             't120306', 't120307', 't120308', 't120309', 't120310',
             't120311', 't120312', 't120313', 't120399',
             't120401', 't120402', 't120403', 't120404', 't120405', 't120499')
  
  
## Define house-work time (hourse work, shopping/services)
  house_wrk<-c('t020101', 't020102', 't020103', 't020104', 't020199',
               't020201', 't020202', 't020203', 't020299',
               't020301', 't020302', 't020303', 't020399', 
               't020401', 't020402', 't020499', 
               't020501', 't020502', 't020599', 
               't020681', 't020699',  ## per care
               't020701', 't020799' , 't020801', 't020899', ## car maintainance
               't020901', 't020902', 't020903', 't020904', 't020905',  
               't020999', 't029999',   #house finance, planning and management
               't070101', 't070102', 't070103', 't070104', 't070105', 't070199', 
               't070201', 't070299', 't080101', 't080102', 't080199', # using services
               't080201', 't080202', 't080203', 't080299', 't080301', 't080302', 't080399', #legal services
               't080401', 't080402', 't080403', 't080499', 't080501', 't080502', 't080599', # using home-health services
               't080601', 't080602', 't080699', # real-estate services
               't080701', 't080702', 't080799', # vet services
               't080801', 't080899', 't089999', # professional services
               't090101', 't090102', 't090103', 't090104', 't090199', # housework services
               't090201', 't090202', 't090299', 't090301', 't090302', 't090399', 
               't090401', 't090402', 't090499',
               't090501', 't090502', 't090599', 't099999', 
               't100101', 't100102', 't100103', 't100199', # social services
               't100201', 't100299', 't100381', 't100383', 't100399',
               't100401', 't100499', 't109999')
               
               
  
## Define care-taking time 
  # care to children
  child_care<-c('t030101', 't030102', 't030103', 't030104', 't030105', 't030186', #playing/talking w/children
                't030108', 't030109', 't030110', 't030111', 't030112', 't030199', 
                't030201', 't030202', 't030203', 't030204', 't030299', 
                't030301', 't030302', 't030303', 't030399', # health-related care to children
                't040101', 't040102', 't040103', 't040104', 't040105', 't040186',
                't040108', 't040109', 't040110', 't040111', 't040112', 't040199',
                't040201', 't040202', 't040203', 't040204', 't040299', 't040301',
                't040302', 't040303', 't040399')    #care to non-hh children
  
  
  
  # care to adults
  adult_care<-c('t030401', 't030402', 't030403', 't030404', 't030405', 't030499',
                't030501', 't030502', 't030503', 't030504', 't030599', 't039999',
                't040401', 't040402', 't040403', 't040404', 't040405', 't040499',#non-hh adults 
                't040501', 't040502', 't040503', 't040504', 't040505', 't040506', 
                't040507', 't040508', 't040599', 't049999' )
  
  
  ## Exercising/Working out 
  exec<-dt_sum_0322 %>% select(t130101: t130199) %>% names()
  
  ## sedentary hobbies (watching TV, watching sports)
  sed_act<-dt_sum_0322 %>% select(t130201:t130232) %>% names()
  
  ## religious activies 
  relig<-dt_sum_0322 %>% select(t140101: t149999) %>% names()
  
  ## travel
  travel<-dt_sum_0322 %>% select(t180101: t181699) %>% names()
  
  ## talking to friends/family over the phone
  phone_fam<-c('t160101', 't160102', 't150104')
  
  ## Social services -- voluneetering, fundrasing, helping communities 
  soc_serv<-c('t150106', 't150199', 't150201', 't150202', 't150203',
              't150204', 't150299', 't150301', 't150302', 
              't150399', 't150401', 't150402', 't150499', 
              't150501', 't150599', 't150601', 't150602', 't150699', 
              't159989')
              

  
  
  
  
  ####### Section 2: Calculating time-use mins by groups ----
  ##

# Generate basic demo variables
  dt1 <-dt_sum_0322 %>% 
    mutate(tucaseid = as.character(tucaseid)) %>%
    mutate(year = as.numeric(substr(tucaseid, 1,4)),
           ## Demographics
           gender = case_when(tesex ==1 ~ 'Male',
                              tesex ==2 ~ 'Female'),
           
           edu= factor(case_when(peeduca %in% c(31:38) ~ 'less than HS',
                          peeduca ==39 ~ 'HS',
                          peeduca %in% c(40:42) ~ 'some college/associate',
                          peeduca %in% c(43:46) ~'college+'), 
                       levels =c('less than HS', 'HS', 'some college/associate', 'college+'), 
                       ordered = T),
           
           race = case_when (pehspnon ==1 ~ 'Hispanic', 
                             pehspnon ==2 & ptdtrace %in% c(1, 7:9, 17:18) ~ 'Non-Hipanic White',
                             pehspnon ==2 & ptdtrace %in% c(2, 6, 10:12, 15, 19) ~ 'Non-Hispanic Black',
                             pehspnon ==2 & ptdtrace %in% c(4, 5, 13:14) ~ 'AAPI', 
                             pehspnon ==2 & ptdtrace %in% c(3) ~ 'Native Americans', 
                             TRUE ~ 'Others'), 
           race = factor(race, 
                  levels = c('Hispanic', 'Non-Hipanic White', 
                            'Non-Hispanic Black', 'AAPI', 
                            'Native Americans', 'Others'), ordered = T), 
           
           empstat = factor(case_when(telfs == 1 ~ 'Employed - at work',
                                      telfs == 2 ~ 'Employed - absent',
                                      telfs == 3 ~ 'Unemployed - laid-off',
                                      telfs == 4 ~ 'Unemployed - looking', 
                                      telfs == 5 ~ 'Not in labor force'), 
                            
                            levels = c('Employed - at work', 'Employed - absent',
                                       'Unemployed - laid-off', 'Unemployed - looking',
                                       'Not in labor force'), ordered = T))
                             
                      
# Generate grouped activities variables
  dt2<-dt1 %>%
    mutate(wrk_edu = rowSums(select(., all_of(wrk_edu)), na.rm = T),
           self_care = rowSums(select(., all_of(self_care)), na.rm= T),
           child_care = rowSums(select(., all_of(child_care)), na.rm = T),
           adult_care = rowSums(select(., all_of(adult_care)), na.rm = T), 
           house_wrk = rowSums(select(., all_of(house_wrk)), na.rm = T),
           exec =      rowSums(select(., all_of(exec)), na.rm = T), 
           phone_fam = rowSums(select(., all_of(phone_fam)), na.rm = T), 
           relig = rowSums(select(., all_of(relig)), na.rm = T), 
           sed_act = rowSums(select(., all_of(sed_act)), na.rm = T), 
           soc_leis = rowSums(select(., all_of(soc_leis)), na.rm = T), 
           soc_serv = rowSums(select(., all_of(soc_serv)), na.rm = T), 
           travel = rowSums(select(., all_of(travel)), na.rm = T),
           wrk_act = rowSums(select(., all_of(wrk_act)), na.rm = T),
           sleep = rowSums(select(., all_of(sleep)), na.rm = T),
           ind_leis = rowSums(select(., all_of(ind_leis)), na.rm = T)
    )
  
  
time_grp<-dt2 %>%
       filter(tudiaryday %in% c(2:6)) %>%  # exclude weekends 
       group_by(year, gender)  %>%
       summarise(across(c(wrk_edu, self_care, child_care, adult_care,
                          house_wrk, exec, phone_fam, relig, sed_act, 
                          soc_leis, soc_serv, travel, wrk_act, sleep, ind_leis), ~ mean(., na.rm = T))) %>%
  pivot_longer(-c(year,gender), names_to = 'category',
               values_to = 'minutes' )
  
# edu<-dt2 %>%
#     group_by(year, edu, gender)%>%
#     summarise(house_wrk = mean(house_wrk, na.rm = T))
  
  ## GGplot 
  
time_grp %>%
   filter(category %in% c('self_care', 'soc_leis', 'ind_leis', 'wrk_act')) %>%
    ggplot(aes(x= year, y = minutes, group = category, color = gender)) +
    geom_line() +
    facet_grid(category ~ gender) + ylim(0,500)
  



####### Section 3: Run a K-means clustering analysis ----
library(cluster)
library(factoextra)

set.seed(123)

#https://www.datacamp.com/tutorial/k-means-clustering-r

# Analytical dataset 
unique(time_grp$category)
var_int <-c('wrk_edu', 'child_care', 'adult_care', 'house_wrk', 
            'relig', 'sed_act', 'soc_leis', 'sleep', 'ind_leis')
df <- dt2 %>%
  filter(year == 2022) %>%
  select(all_of(var_int))

# Normalize the variables of interests
df<-df %>% mutate(across(everything(), ~ scale(.)))

# write a func to compute total within-cluster sum of square
wss<-function(k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# compute and plot wss for k =1 to k =15
k.values<-1:20

# extract wss for 2- 15 clusters
wss_values<-map_dbl(k.values, wss)

# plot WSS by cluster numbers 
wss_df<-tibble(cluster = 1:20, wss_values)

wss_df %>%
  ggplot(aes(x = cluster, y = wss_values,group =1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(0:20)) +
  xlab('Number of clusters')




     
  



        


