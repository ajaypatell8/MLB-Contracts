library(tidyverse)
library(tidymodels)
library(splines)
library(corrplot)
library(fastDummies)

pitchers = read_csv("https://raw.githubusercontent.com/ajaypatell8/MLB-Contracts/main/pitchers_contracts_stats.csv")

pitchers = pitchers %>%
  filter(AAV > 1 & Position != "OF") %>%
  mutate(CleanName = paste0(Name," ",FAYear))

pitchers[which(pitchers$Name == "Kendall Graveman" & pitchers$FAYear == 2022),"AAV"] = 8000000

pitchers = pitchers %>%
  mutate(logAAV = log(AAV))

hist(pitchers$logAAV)

ggplot(pitchers, aes(x = FAYear, y = logAAV)) +
  geom_smooth(formula = y ~ bs(x,degree = 1)) +
  geom_point()

detrend_year = glm(logAAV ~ bs(FAYear, degree = 1), data = pitchers)

pitchers = pitchers %>%
  mutate(trend_term = predict(detrend_year,.)) %>%
  mutate(detrended_log = logAAV - trend_term)

positionCleaner = function(position) {
  position %>%
    substr(.,1,2) %>%
    gsub("[[:punct:]]","",.) %>%
    toupper()
  
}

positionGrouper = function(position) {
  case_when(
    position %in% c("CP","RP") ~ "RP",
    position %in% c("SP") ~ "SP"
  )
}

pitchers = pitchers %>%
  mutate(pos_clean = positionCleaner(Position)) %>%
  mutate(pos_group = positionGrouper(pos_clean)) %>%
  dummy_cols("pos_group",remove_first_dummy = T)

pitchers_num = pitchers %>%
  select(where(is.numeric))

cor_matrix = cor(pitchers_num, pitchers_num)

corrplot(cor_matrix)


pitchers_modeling_data = pitchers %>%
  select(detrended_log, Age, WAR, WAR_rolling, IP, IP_rolling, pos_group_SP)

pitchers_model = glm(detrended_log ~ . * . + bs(Age, degree = 3) + bs(WAR_rolling, degree = 2), data = pitchers_modeling_data)

pitchers_pred = pitchers %>%
  mutate(pred = predict(pitchers_model,.)) %>%
  mutate(aav_pred = exp(pred + trend_term))

library(ggrepel)
pitchers_pred = pitchers_pred %>%
  mutate(lastname = gsub(".*\\s","",Name)) %>%
  mutate(plotlabel = paste0(lastname," '",substr(FAYear,3,4)))

write_csv(pitchers_pred, "projections/PitcherAAV.csv")


# ggplot(pitchers_pred, aes(x = aav_pred, y = AAV)) +
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

