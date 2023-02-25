library(tidyverse)
library(tidymodels)
library(splines)
library(corrplot)
library(fastDummies)

hitters = read_csv("https://raw.githubusercontent.com/ajaypatell8/MLB-Contracts/main/hitters_contracts_stats.csv")

# 0. FILTER TO OBS. WE WANT TO INCLUDE
# specifically, remove observations for pitchers and contracts w/ no AAV/years

hitters = hitters %>%
  filter(AAV > 1 & Position != "RP") %>%
  mutate(CleanName = paste0(Name," ",FAYear))

# 1. RESCALE VARIABLE WE'RE PREDICTING
# use log3 to rescale onto a more manageable scale

hitters = hitters %>%
  mutate(logAAV = log(AAV))

hist(hitters$AAV)
hist(hitters$logAAV)

# 2. DE-TREND BASED ON YEAR

# consider the hypothetical model AAV = FAYear + (some combo of other variables)
# we want to remove the FAYear term and then account for it at the end

ggplot(hitters, aes(x = FAYear, y = logAAV)) +
  geom_smooth(formula = y ~ bs(x,degree = 1)) +
  geom_point()

# de-trending model
detrend_year = glm(logAAV ~ bs(FAYear, degree = 1), data = hitters)

# calculate detrended log3 values
hitters = hitters %>%
  mutate(trend_term = predict(detrend_year,.)) %>%
  mutate(detrended_log = logAAV - trend_term)

# 3. CLEAN POSITION VARIABLE

# grouping position into useful categories
positionCleaner = function(position) {
  position %>%
    substr(.,1,2) %>%
    gsub("[[:punct:]]","",.) %>%
    toupper()
    
}

positionGrouper = function(position) {
  case_when(
    position %in% c("1B","3B") ~ "CI",
    position %in% c("2B","SS","UT","IN","MI") ~ "MI",
    position %in% c("C") ~ "C",
    position %in% c("LF","CF","RF","OF") ~ "OF",
    position %in% c("DH") ~ "DH"
  )
}

# create dummy variables for position group
hitters = hitters %>%
  mutate(pos_clean = positionCleaner(Position)) %>%
  mutate(pos_group = positionGrouper(pos_clean)) %>%
  dummy_cols(select_columns = "pos_group", remove_most_frequent_dummy = T)

ggplot(hitters, aes(x = pos_group, y = detrended_log)) +
  geom_boxplot()

## 4. CORRELATIONAL ANALYSIS
# note: there were plots here but they got removed for final output
# use corrplot(cor_matrix)

hitters_num = hitters %>%
  select(where(is.numeric))

cor_matrix = cor(hitters_num, hitters_num$detrended_log)

## 5. MODELING

hitters_modeling_data = hitters %>%
  select(detrended_log, Age, WAR_rolling, ISO_rolling, WRC_rolling, starts_with("pos_group_"),K_BB_diff_rolling)

# fit splines to WAR and Age, consider everything else as a linear term
hitters_model = glm(detrended_log ~  .*. + bs(Age, degree = 3) + bs(WAR_rolling, degree = 2), data = hitters_modeling_data)

## 6. PREDICTION
# calculate predictions and un-scale by reapplying the trend term and exponential function

# note: raw 'pred' is year-adjusted value, while 'aav_pred' is the final AAV prediction
# players may be more 'valuable' but have lower AAV if they were signed in an earlier season/the COVID year

hitters_pred = hitters %>%
  mutate(pred = predict(hitters_model,.)) %>%
  mutate(aav_pred = exp(pred + trend_term))

## 7. VISUALIZE PREDICTIONS

library(ggrepel)

hitters_pred = hitters_pred %>%
  mutate(lastname = gsub(".*\\s","",Name)) %>%
  mutate(plotlabel = paste0(lastname," '",substr(FAYear,3,4)))

write_csv(hitters_pred, "projections/HitterAAV.csv")

# ggplot(hitters_pred, aes(x = aav_pred, y = AAV)) +
#   geom_point() +
#   geom_abline() +
#   geom_label_repel(aes(label = plotlabel)) +
#   labs(x = "Projected AAV",
#        y = "Actual AAV",
#        title = "AAV Projections") +
#   theme_bw(base_size = 20) +
#   scale_x_continuous(labels = function(x) { paste0("$",x/1000000,"m")},
#                      limits = c(0, 50000000)) +
#   scale_y_continuous(labels = function(x) { paste0("$",x/1000000,"m")},
#                      limits = c(0, 50000000))

