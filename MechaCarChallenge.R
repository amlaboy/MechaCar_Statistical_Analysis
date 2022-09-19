# Import Libraries
library(tidyverse)
# Create mecha car table
mecha_car_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Create LM
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = mecha_car_table)

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD , data = mecha_car_table))


# Create suspension coil table
coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Create total summary
total_summary <- summarize(coil_table, Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

##
#lot_grouping <- coil_table <- group_by(Manufacturing_Lot, PSI)
  
lot_summary <- coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean =mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI),  .groups = 'keep')
