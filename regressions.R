library(car)
source("prep_data_reg.R")

model_1 <- lm(behavior ~ age + sex + marriage + income, data = processed_data)
model_2 <- lm(behavior ~ hb_a_se + confi_gov + confi_hos + confi_wor + confi_media + age + sex + marriage + income, data = processed_data)
model_3 <- lm(behavior ~ hb_a_se*vulnerability + confi_gov*vulnerability + confi_hos*vulnerability + confi_wor*vulnerability + confi_media*vulnerability +
                age + sex + marriage + income, data = processed_data)

vif(model_1)
vif(model_2)
vif(model_3)
