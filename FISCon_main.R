# load packages ####
req <- substitute(require(x, character.only = TRUE))
libs<-c("sjPlot", 
        "ggplot2", 
        "ggeffects",
        "gtsummary",
        "jtools", 
        "car", 
        "blorr",
        "broom.helpers",
        "DescTools", 
        "MASS", 
        "stargazer", 
        "dplyr", 
        "readxl", 
        "VIM", 
        "performance", # how well does the model fit the data?
        "DHARMa",
        "DataExplorer",
        "pROC",
        "see",
        "xfun",
        "emmeans",
        "flextable",
        "equatiomatic",
        "effectsize",
        "mice",
        "caret",
        "pscl",
        "fsmb", # Nagelkerke R2
        "randomForest",
        "vip") # For Anova Plot
sapply(libs, function(x) eval(req) || {install.packages(x); eval(req)})

# Function for renaming variables in the GEM dataset
rename_gem_vars <- function(df) {
  df %>%
    rename(
      CNT      = any_of("Land"),
      REG      = any_of("Region"),
      SEX      = any_of("Geschlecht"),
      AGE      = any_of("Alter"),
      EMP      = any_of("Arbeitsstatus"),
      INC      = any_of("Einkommen"),
      EDU      = any_of("Ausbildung"),
      EXP      = any_of("Gründungserfahrung"),
      CON      = any_of("Gründerkontakt"),
      MKT_NAT  = any_of("Marktfreiheit_national"),
      PROS_REG = any_of("Prosperität_regional")
    )
}

# load data and rename collums (and set -9 as NA ()aply function) ####
# then get frist view of data 

# Potential entrpreneurs
raw_potential <- read_excel("01_data/raw/1_POTENTIAL.xlsx") %>%
  rename_gem_vars() %>%
  rename(
    POT = POTENTIAL
  )

colnames(raw_potential)
summary(raw_potential)

# Nascent entrepreneurs
raw_nascent <- read_excel("01_data/raw/2_NASCENT.xlsx") %>%
  rename_gem_vars() %>%
  rename(
    NAS = NASCENT
  )

colnames(raw_nascent)
summary(raw_nascent)

# Young entrepreneurs
raw_young <- read_excel("01_data/raw/3_YOUNG.xlsx") %>%
  rename_gem_vars() %>%
  rename(
    YOUNG = YOUNG
  )

colnames(raw_young)
summary(raw_young)

# Nascent opp entrepreneurs
raw_nascent_opp <- read_excel("01_data/raw/4_NASCENT-OPP.xlsx") %>%
  rename_gem_vars() %>%
  rename(
    NAS_OPP = NASCENT_OPP
  )

colnames(raw_nascent_opp)
summary(raw_nascent_opp)

# Young opp entrepreneurs
raw_young_opp     <- read_excel("01_data/raw/5_YOUNG-OPP.xlsx") %>%
  rename_gem_vars() %>%
  rename(
    YOUNG_OPP = YOUNG_OPP
  )

colnames(raw_young_opp)
summary(raw_young_opp)

# function for setting -9 as NA and coding varaibels as factors ####
code_gem_factors <- function(df) {
  
  # set -9 (missing values) as NA
  df[df == -9] <- NA
  
  # convert CNT and REG from num to character
  df$CNT <- as.character(df$CNT)
  
  df$REG <- as.character(df$REG)
  
  if ("SEX" %in% names(df)) {
    df$SEX <- factor(df$SEX, levels = c(0, 1), labels = c("Weiblich", "Männlich"))
  }
  if ("AGE" %in% names(df)) {
    df$AGE <- factor(df$AGE, levels = c(0, 1), labels = c("18-24 und 45-64", "25-44"))
  }
  if ("EMP" %in% names(df)) {
    df$EMP <- factor(df$EMP, levels = c(0, 1), labels = c("arbeitslos", "geht einer beruflichen Beschäftigung nach"))
  }
  if ("INC" %in% names(df)) {
    df$INC <- factor(df$INC, levels = c(0, 1), labels = c("unteres und mittleres Einkommensdrittel (des Samples)", 
                                                          "oberes Einkommensdrittel (des Samples)"))
  }
  if ("EDU" %in% names(df)) {
    df$EDU <- factor(df$EDU, levels = c(0, 1), labels = c("weniger als FH-Abschluss", "mindestens FH-Abschluss"))
  }
  if ("EXP" %in% names(df)) {
    df$EXP <- factor(df$EXP, levels = c(0, 1), labels = c("nein", "ja"))
  }
  if ("CON" %in% names(df)) {
    df$CON <- factor(df$CON, levels = c(0, 1), labels = c("nein", "ja"))
  }
  if ("MKT_NAT" %in% names(df)) {
    df$MKT_NAT <- factor(df$MKT_NAT, levels = c(0, 1), labels = c("niedriges bis mittleres Maß", "hohes Maß"))
  }
  if ("PROS_REG" %in% names(df)) {
    df$PROS_REG <- factor(df$PROS_REG, levels = c(0, 1), labels = c("niedriges bis mittleres Niveau", "hohes Niveau"))
  }
  return(df)
}

# apply function to code variables as factors
raw_potential <- code_gem_factors(raw_potential) %>%
  mutate(POT = factor(POT, levels = c(0, 1), labels = c("nein", "ja")))

raw_nascent <- code_gem_factors(raw_nascent) %>%
  mutate(NAS = factor(NAS, levels = c(0, 1), labels = c("nein", "ja")))

raw_young <- code_gem_factors(raw_young) %>%
  mutate(YOUNG = factor(YOUNG, levels = c(0, 1), labels = c("nein", "ja")))

raw_nascent_opp <- code_gem_factors(raw_nascent_opp) %>%
  mutate(NAS_OPP = factor(NAS_OPP, levels = c(0, 1), labels = c("nein", "ja")))

raw_young_opp <- code_gem_factors(raw_young_opp) %>%
  mutate(YOUNG_OPP = factor(YOUNG_OPP, levels = c(0, 1), labels = c("nein", "ja")))

# check data types and structure ####
str(raw_potential)
summary(raw_potential)

# potential entrepreneurs data exploration ####
# check missing values
plot_missing(raw_potential)

# delete all observations with NAs
clean_potential <- na.omit(raw_potential)
clean_nascent <- na.omit(raw_nascent)
clean_nascent_opp <- na.omit(raw_nascent_opp)
clean_young <- na.omit(raw_young)
clean_young_opp <- na.omit(raw_young_opp)

summary(clean_potential)

plot_bar(clean_potential)

# check for correlation
plot_correlation(
  clean_potential %>% select(-CNT, -REG)
)

# build potential logit model ####
m1_potential <- glm(
  POT
  ~ INC
  + SEX
  + AGE
  + EMP
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = clean_potential,
  family = binomial
  )

m2_potential <- glm(
  POT
  ~ INC
  + SEX
  + AGE
  + EMP
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG 
  + INC * EDU,
  data = clean_potential,
  family = binomial
)

m3_potential <- glm(
  POT
  ~ INC
  + SEX
  + AGE
  + EMP
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG 
  + CON * EXP, # signifikanz auf einem Stern
  data = clean_potential,
  family = binomial
)

# build nascent logit model ####
m1_nascent <- glm(
  NAS
  ~ INC
  + SEX
  + AGE
  + EMP
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = clean_nascent,
  family = binomial
)

# build nascent opp logit model ####
m1_nascent_opp <- glm(
  NAS_OPP
  ~ INC
  + SEX
  + AGE
  + EMP
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = clean_nascent_opp,
  family = binomial
)

# build young logit model ####
m1_young <- glm(
  YOUNG
  ~ INC
  + SEX
  + AGE
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = clean_young,
  family = binomial
)

# build young opp logit model ####
m1_young_opp <- glm(
  YOUNG_OPP
  ~ INC
  + SEX
  + AGE
  + EDU
  + EXP
  + CON 
  + MKT_NAT 
  + PROS_REG,
  data = clean_young_opp,
  family = binomial
)

# check model results ####
summary(m1_potential)
exp_m1_potential <- exp(coef(m1_potential)) %>%
  round(digits = 3)

summary(m1_nascent)
exp_m1_nascent <- exp(coef(m1_nascent)) %>%
  round(digits = 3)

summary(m1_nascent_opp)
exp_m1_nascent_opp <- exp(coef(m1_nascent_opp)) %>%
  round(digits = 3)

summary(m1_young)
exp_m1_young <- exp(coef(m1_young)) %>%
  round(digits = 3)

summary(m1_young_opp)
exp_m1_young_opp <- exp(coef(m1_young_opp)) %>%
  round(digits = 3)

# 2. In DataFrame umwandeln (Variablen als Zeilen, Modell als Spalte)
df_models <- data.frame(
  Faktor = names(exp_m1_potential),
  OR_m1_nascent = as.numeric(exp_m1_potential),
  row.names = NULL
)

# Liste aller DataFrames
dfs <- list(
  data.frame(Faktor = names(exp_m1_potential), OR_m1_potential = as.numeric(exp_m1_potential)),
  data.frame(Faktor = names(exp_m1_nascent), OR_m1_nascent = as.numeric(exp_m1_nascent)),
  data.frame(Faktor = names(exp_m1_nascent_opp), OR_m1_nascent_opp = as.numeric(exp_m1_nascent_opp)),
  data.frame(Faktor = names(exp_m1_young), OR_m1_young = as.numeric(exp_m1_young)),
  data.frame(Faktor = names(exp_m1_young_opp), OR_m1_young_opp = as.numeric(exp_m1_young_opp))
)

# Reduziert mergen
df_models <- Reduce(function(x, y) merge(x, y, by = "Faktor", all = TRUE), dfs)

# 3. Optional: schöner darstellen
print(df_models)
rownames(df_models)

library(tidyr)
library(dplyr)

df_long <- df_models %>%
  filter(Faktor != "(Intercept)") %>%
  pivot_longer(
    cols = starts_with("OR_"),
    names_to = "Modell",
    values_to = "OR"
  )

# Mittelwert der ORs je Faktor berechnen
faktor_sortierung <- df_long %>%
  group_by(Faktor) %>%
  summarise(mean_OR = mean(OR, na.rm = TRUE)) %>%
  arrange(mean_OR)  # absteigend sortieren

df_long$Faktor <- factor(df_long$Faktor, levels = faktor_sortierung$Faktor)

library(ggplot2)

ggplot(df_long, aes(x = Faktor, y = OR, color = Modell, shape = Modell)) +
  geom_point(position = position_dodge(width = 0.5), size = 5, stroke = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +  # Referenzlinie
  coord_flip() +  # damit die Faktoren auf der y-Achse sind (bessere Lesbarkeit)
  theme_minimal() +
  scale_shape_manual(values = c(0, 22, 2, 1, 24)) +  # verschiedene Formen für die Punkte
  labs(
    title = "Odds Ratios nach Modell und Faktor",
    y = "Odds Ratio (exponentierter Koeffizient)",
    x = "Faktor"
  )

ggplot(df_long, aes(x = Faktor, y = OR, color = Modell, shape = Modell, fill = Modell)) +
  geom_point(position = position_dodge(width = 0.5), size = 5, stroke = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  coord_flip() +
  theme_minimal() +
  
  # Formen: ungefüllte = 0–20, gefüllte (_opp) = 21–25
  scale_shape_manual(values = c(
    "OR_m1_nascent"      = 0,   # offenes Quadrat
    "OR_m1_nascent_opp"  = 22,  # gefülltes Quadrat
    "OR_m1_potential"    = 1,   # offenes Dreieck
    "OR_m1_young"        = 2,   # offener Kreis
    "OR_m1_young_opp"    = 24   # gefülltes Dreieck
  )) +
  
  # Gleiche Randfarben für "Paar-Modelle"
  scale_color_manual(values = c(
    "OR_m1_nascent"      = "red4", 
    "OR_m1_nascent_opp"  = "red4", 
    "OR_m1_potential"    = "#A020F0", 
    "OR_m1_young"        = "royalblue4", 
    "OR_m1_young_opp"    = "royalblue4"
  )) +
  
  # Füllfarben nur für die gefüllten Shapes (_opp)
  scale_fill_manual(values = c(
    "OR_m1_nascent"      = NA,         # wird ignoriert bei ungefülltem Shape
    "OR_m1_nascent_opp"  = "red", 
    "OR_m1_potential"    = NA, 
    "OR_m1_young"        = NA, 
    "OR_m1_young_opp"    = "royalblue"
  )) + 
  
  labs(
    title = "Odds Ratios nach Modell und Faktor",
    y = "Odds Ratio (exponentierter Koeffizient)",
    x = "Faktor"
  )


# check model accuracy ####
pR2(m1_potential)
performance(m1_potential)
PseudoR2(m1_potential, which = "Nagelkerke")

# check model probability ####
m1_potential_prob <- predict(m1_potential, type = "response")
print(m1_potential_prob)

# ROC curve and AUC ####
roc_potential <- roc(response = clean_potential$POT, predictor = clean_potential$m1_potential_prob)
plot(roc_potential)
auc(roc_potential)

# compare model results ####
compare_performance(m1_potential, m1_nascent, m1_nascent_opp, m1_young, m1_young_opp)

# plot model results ####
# random forest variable importance plot
vip(m1_potential, num_features = 10) +
  labs(title = "Variable Importance Plot for Potential Entrepreneurs Model")


