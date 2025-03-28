library(tidyverse)

en_input = read.csv("bld data/energy_input.csv")
en_price = read.csv("bld data/energy_price.csv")
subsidy = read.csv("bld data/IRA_building_subsidy.csv")
nonfuel_cost = read.csv("bld data/nonfuel_cost.csv")
service_output = read.csv("bld data/service_output.csv")
state_map = read.csv("Mapping_state_USREP.csv") %>% 
  rename(region = state, USREP_region = r)

Value_of_consumption_i = en_input %>% 
  group_by(scenario,subsector,Units,region) %>% 
  summarise(X2020 = sum(X2020),
            X2025 = sum(X2025),
            X2030 = sum(X2030),
            X2035 = sum(X2035)) %>% 
  ungroup() %>% 
  gather(year,value,5:8) %>% 
  mutate(year = gsub("X","",year)) %>% 
  rename(energy_input = value) %>% 
  left_join(en_price %>% 
              gather(year,value,5:12) %>% 
              mutate(year = gsub("X","",year),
                     fuel = gsub("elect_td_bld","electricity",fuel),
                     fuel = gsub("delivered gas","gas",fuel)) %>% 
              select(scenario,region,subsector = fuel,year,price = value)) %>% 
  mutate(value = energy_input*price,
         Units  = "Billions 1975$") %>% 
  left_join(state_map) %>% 
  group_by(scenario, subsector,Units,USREP_region,year) %>% 
  summarise(value= sum(value)) %>% 
  ungroup()
  
Value_of_consumption = Value_of_consumption_i %>%
  left_join(Value_of_consumption_i %>% 
              filter(scenario == "Current Policies") %>% 
              select(subsector,USREP_region,year,cpol_value = value)) %>% 
  mutate("PercentChangeFromCPOL" = (value/cpol_value) - 1) %>% 
  select(-cpol_value)

write.csv(Value_of_consumption,"Value_of_consumption.csv")

Change_In_Tech_Investment_i = service_output %>% 
  group_by(scenario,region,sector,technology,Units) %>% 
  summarise(X2020 = sum(X2020),
            X2025 = sum(X2025),
            X2030 = sum(X2030),
            X2035 = sum(X2035)) %>% 
  ungroup() %>% 
  gather(year,value,6:9) %>% 
  mutate(year = gsub("X","",year)) %>% 
  left_join(nonfuel_cost %>% 
              gather(year,cost,7:14) %>% 
              mutate(year = gsub("X","",year)) %>% 
              select(-Units, -scenario)) %>% 
  mutate(invest = cost*value,
         Units  = "Billions 1975$") %>% 
  left_join(state_map) %>% 
  group_by(scenario, sector, technology,subsector,Units,USREP_region,year) %>% 
  summarise(invest= sum(invest)) %>% 
  ungroup()

Change_In_Tech_Investment = Change_In_Tech_Investment_i %>% 
  left_join(Change_In_Tech_Investment_i %>% 
              filter(scenario == "Current Policies") %>% 
              select(USREP_region,sector,technology,Units,year,cpol_invest = invest)) %>% 
  mutate("PercentChangeFromCPOL" = (invest/cpol_invest) - 1) %>% 
  select(-cpol_invest)
  
write.csv(Change_In_Tech_Investment,"Change_In_Tech_Investment.csv")
  
Value_of_Subsidy = service_output %>% 
  group_by(scenario,region,sector,technology,Units) %>% 
  summarise(X2020 = sum(X2020),
            X2025 = sum(X2025),
            X2030 = sum(X2030),
            X2035 = sum(X2035)) %>% 
  ungroup() %>% 
  gather(year,value,6:9) %>% 
  mutate(year = gsub("X","",year)) %>% 
  filter(scenario == "ExecRollback") %>% 
  rename(ExecRollback = value) %>% 
  select(-scenario) %>% 
  left_join(service_output %>% 
              group_by(scenario,region,sector,technology,Units) %>% 
              summarise(X2020 = sum(X2020),
                        X2025 = sum(X2025),
                        X2030 = sum(X2030),
                        X2035 = sum(X2035)) %>% 
              ungroup() %>% 
              gather(year,value,6:9) %>% 
              mutate(year = gsub("X","",year)) %>% 
              filter(scenario == "FedRollback") %>% 
              rename(FedRollback = value) %>% 
              select(-scenario)) %>% 
  mutate(IRAEffect = ExecRollback-FedRollback) %>% 
  left_join(subsidy %>% 
              select(-subsector, -Units)) %>% 
  mutate(ValueOfSubsidy = subsidy*IRAEffect) %>% 
  summarise(ValueOfSubsidy = sum(ValueOfSubsidy,na.rm =TRUE))

write.csv(Value_of_Subsidy,"Value_of_Subsidy.csv")
