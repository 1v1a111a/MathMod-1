library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tbl = read_csv("eddypro.csv", skip =1, na =c("","NA","-9999","-9999.0"), comment=c("["))
tbl = tbl[-1,]
tbl
tbl = select(tbl, -(roll))
tbl
tbl=tbl[tbl$DOY>152 & tbl$DOY<245, c(1:ncol(tbl))]
tbl=select(tbl,date:un_H)
glimpse(tbl)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric))
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
cor_vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux)
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""))
formula
mod1=lm(h2o_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE +  
          rand_err_LE + co2_flux + h2o_flux + 
          rand_err_h2o_flux + H_strg + co2_molar_density + co2_mole_fraction + 
          co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + 
          air_density + air_molar_volume + es + RH + VPD + u_rot + 
          wind_speed + max_speed + u_star_ + TKE + T_star_ + x_offset + 
          un_Tau + un_H, data=tbl) 
summary(mod1)
anova(mod1)
mod2=lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE +  
                      rand_err_LE + co2_flux + h2o_flux + rand_err_h2o_flux +  
                      co2_molar_density + h2o_time_lag + air_molar_volume +
                      air_density + es + RH + VPD + u_star_ + T_star_ + 
                      un_Tau + un_H)^2, data=tbl)
summary(mod2)
anova(mod2)
mod3=lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE +  
                      rand_err_LE + co2_flux + h2o_flux + rand_err_h2o_flux +  
                      co2_molar_density + h2o_time_lag + air_molar_volume +
                      air_density + es + RH + VPD + u_star_ + T_star_ + 
                      un_Tau + un_H)^2-co2_flux:co2_molar_density-rand_err_h2o_flux
        -co2_molar_density:h2o_time_lag - co2_molar_density:RH - co2_molar_density:T_star_ -
          co2_molar_density:T_star_ - co2_molar_density:un_Tau - co2_molar_density:un_H - h2o_time_lag:RH  -  
          h2o_time_lag:VPD - h2o_time_lag:u_star_ - h2o_time_lag:un_Tau - RH:VPD - RH:u_star_ - RH:T_star_ - 
          RH:un_Tau - RH:un_H - VPD:u_star_ - VPD:T_star_ - u_star_:T_star_ - u_star_:un_Tau - 
          u_star_:un_H - T_star_:un_Tau - T_star_:un_H - un_Tau:un_H, data=tbl)
anova(mod3)
summary(mod3)
mod4=lm(h2o_flux ~ (Tau + rand_err_Tau + H + rand_err_H + LE +  
                      rand_err_LE + co2_flux + h2o_flux + rand_err_h2o_flux +  
                      co2_molar_density + h2o_time_lag + RH + VPD + u_star_ + 
                      T_star_ + un_Tau + un_H)^2 -co2_flux:co2_molar_density-rand_err_h2o_flux -
          co2_molar_density:h2o_time_lag - co2_molar_density:RH - co2_molar_density:T_star_ -
          co2_molar_density:T_star_ - co2_molar_density:un_Tau - co2_molar_density:un_H - h2o_time_lag:RH  -  
          h2o_time_lag:VPD - h2o_time_lag:u_star_ - h2o_time_lag:un_Tau - RH:VPD - RH:u_star_ - RH:T_star_ - 
          RH:un_Tau - RH:un_H - VPD:u_star_ - VPD:T_star_ - u_star_:T_star_ - u_star_:un_Tau - 
          u_star_:un_H - T_star_:un_Tau - T_star_:un_H - un_Tau:un_H - h2o_flux:u_star_ - h2o_flux:T_star_ - 
          rand_err_h2o_flux:u_star_ - rand_err_h2o_flux:T_star_  - rand_err_h2o_flux:un_H  - 
          h2o_time_lag:T_star_ - h2o_time_lag:un_H - VPD:un_Tau - VPD:un_H - 
          rand_err_h2o_flux:T_star_ -  rand_err_h2o_flux:un_Tau - 
          co2_molar_density:air_molar_volume - co2_molar_density:es - 
          co2_molar_density:VPD - h2o_time_lag:T_star_ - h2o_time_lag:un_H - 
          air_molar_volume:air_density - air_molar_volume:u_star_ , data=tbl)
summary(mod4)
anova(mod4)
