
# # code variables as factors 
# factor_variables <- c("SEX", "AGE", "EMP", "EDU", "EXP", "CON", "POT", "INC", "MKT_NAT", "PROS_REG")
# 
# # write function to convert variables to factors
# convert_to_factor <- function(raw, variables) {
#   for (var in variables) {
#     raw[[var]] <- as.factor(raw[[var]])
#   }
#   return(raw)
# }
#  
# 
# raw_potential <- convert_to_factor(raw_potential, factor_variables)

# convert integer to factor with labels
clean_potential$SEX <- factor(clean_potential$SEX, 
                              levels = c(0, 1), 
                              labels = c("Weiblich", "Männlich"))

clean_potential$AGE <- factor(clean_potential$AGE, 
                              levels = c(0, 1), 
                              labels = c("18-24 und 45-64", "15-44"))

clean_potential$EMP <- factor(clean_potential$EMP, 
                              levels = c(0, 1), 
                              labels = c("arbeitslos", "geht einer beruflichen Beschäftigung nach"))

clean_potential$INC <- factor(clean_potential$INC, 
                              levels = c(0, 1), 
                              labels = c("unteres und mittleres Einkommensdrittel (des
Samples)", "oberes Einkommensdrittel (des Samples)"))

clean_potential$EDU <- factor(clean_potential$EDU, 
                              levels = c(0, 1), 
                              labels = c("weniger als FH-Abschluss", "mindestens FH-Abschluss"))

clean_potential$EXP <- factor(clean_potential$EXP, 
                              levels = c(0, 1), 
                              labels = c("nein", "ja"))

clean_potential$CON <- factor(clean_potential$CON, 
                              levels = c(0, 1), 
                              labels = c("nein", "ja"))

clean_potential$POT <- factor(clean_potential$POT, 
                              levels = c(0, 1), 
                              labels = c("nein", "ja"))

clean_potential$MKT_NAT <- factor(clean_potential$MKT_NAT, 
                                  levels = c(0, 1), 
                                  labels = c("niedriges bis mittleres Maß an wirtschaftlicher
Freiheit in der jeweiligen Volkswirtschaft", "hohes Maß an wirtschaftlicher Freiheit"))

clean_potential$PROS_REG <- factor(clean_potential$PROS_REG, 
                                   levels = c(0, 1), 
                                   labels = c("niedriges bis mittleres regionales Wohlstands-
und Wachstumsniveau in der jeweiligen Region", "hohes regionales Wohlstands- und
Wachstumsniveau"))


# impute data with the mice package ####
imputed_potential <- mice(raw_potential, 
                          m = 5, # number of imputations 
                          method = "logreg", # method for imputation
                          seed = 123)
# save imputed data
saveRDS(imputed_potential, "./01_data/processed/imputed_potential.rds")

# check the imputed data ####
summary(imputed_potential)
stripplot(imputed_potential, CNT ~ .imp)
stripplot(imputed_potential, INC ~ .imp)

list_of_imputed_dfs <- complete(imputed_potential, action = "long")

fit_model <- with(list_of_imputed_dfs, glm(Erfolg ~ Gruenderkontakt + Einkommen + WeitereBinareVariable, family = binomial))

# Ergebnisse poolen
pooled_results <- pool(fit_model)

# Zusammenfassung der gepoolten Ergebnisse
summary(pooled_results)


# create a complete data set from the imputed data
complete_potential <- complete(imputed_potential, 1) # use the first imputed dataset


# new data frame without INC and EMP ####
reduced_potential <- raw_potential %>%
  select(-INC, # about the half of all observations are missing
         -EMP) # about 25% of all observations are missing

# check for missing values
reduced_potential_missing <- colSums(is.na(reduced_potential))
print(reduced_potential_missing)

# delete all observations with NAs
reduced_potential <- na.omit(reduced_potential)
summary(reduced_potential)

# build reduced logit model
m1_reduced_potential <- glm(
  POT
  ~ SEX
  + AGE
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = reduced_potential,
  family = binomial
)

# check model results a ####
# print summary of the model
summary(m1_reduced_potential)

# convert coefficients to odds ratios
m1_reduced_potential_odds <- exp(coef(m1_reduced_potential))
print(m1_reduced_potential_odds)

# Prozentverteilung berechnen
raw_potential %>%
  filter(!is.na(INC)) %>%
  count(INC) %>%
  mutate(Prozent = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = reorder(INC, -Prozent), y = Prozent, fill = INC)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Prozent, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Verteilung nach Einkommensdritteln (prozentual)",
    x = NULL, y = "Prozent"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

raw_potential %>%
  filter(!is.na(CON)) %>%
  count(CON) %>%
  mutate(Prozent = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = reorder(CON, -Prozent), y = Prozent, fill = CON)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Prozent, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "Verteilung nach Gründerkontakt (prozentual)",
    x = NULL, y = "Prozent"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# check pattern of na
aggr_plot <- aggr(raw_potential, 
                  col = c("navyblue", "red"), 
                  numbers = TRUE, 
                  sortVars = TRUE, 
                  labels = names(raw_potential), 
                  cex.axis = 0.7, 
                  gap = 3, 
                  ylab = c("Missing data", "Pattern"))


# confusion matrix ####
predicted_classes <- factor(ifelse(predicted_probabilities > 0.5, "ja", "nein"),
                            levels = c("nein", "ja"))

actual_classes <- reduced_potential$POT

confusionMatrix(predicted_classes, actual_classes)