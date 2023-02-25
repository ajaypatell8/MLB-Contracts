library(tidyverse)
library(MLmetrics)
library(gt)
library(gtExtras)

hitters = read_csv("projections/HittersFinalProjections.csv")
pitchers = read_csv("projections/PitchersFinalProjections.csv")

calculate_eval_metrics_aav = function(df,type) {
  pred = pull(df[,"ProjAAV"])
  true = pull(df[,"AAV"])
  
  rmse = RMSE(pred,true)
  mae = MAE(pred,true)
  median_ae = MedianAE(pred,true)
  data.frame(type, rmse, mae, median_ae)
}

calculate_eval_metrics_years = function(df,type) {
  pred = pull(df[,"ProjYears"])
  true = pull(df[,"Years"])
  
  rmse = RMSE(pred,true)
  mae = MAE(pred,true)
  median_ae = MedianAE(pred,true)
  pct_correct = sum(pred == true)/length(pred)
  data.frame(type, rmse, mae, median_ae)
}

calculate_eval_metrics_aav(hitters,"hitters-aav") %>%
  gt() %>% gt_theme_espn() %>%
  fmt_currency(c(rmse,mae,median_ae), suffixing = T)
  
calculate_eval_metrics_aav(pitchers,"pitchers-aav") %>%
  gt() %>% gt_theme_espn() %>%
  fmt_currency(c(rmse,mae,median_ae), suffixing = T)

calculate_eval_metrics_years(hitters,"hitters-years") %>%
  gt() %>% gt_theme_espn() %>%
  fmt_number(c(rmse,mae,median_ae),decimals = 2)

calculate_eval_metrics_years(pitchers,"pitchers-years") %>%
  gt() %>% gt_theme_espn() %>%
  fmt_number(c(rmse,mae,median_ae),decimals = 2)

rbind(calculate_eval_metrics_aav(hitters,"Hitter AAV"),
      calculate_eval_metrics_aav(pitchers,"Pitcher AAV"),
      calculate_eval_metrics_years(hitters,"Hitter Years"),
      calculate_eval_metrics_years(pitchers,"Pitcher Years")
      ) %>%
  gt() %>% gt_theme_espn()%>%
  fmt_number(c(rmse,mae,median_ae),decimals = 2,rows = 3:4) %>%
  fmt_currency(c(rmse,mae,median_ae), suffixing = T,rows = 1:2) %>%
  cols_label(median_ae = "Median AE",
             type = "MODEL")




hitters = hitters %>%
  mutate(error = abs(ProjAAV - AAV))
pitchers = pitchers %>%
  mutate(error = abs(ProjAAV - AAV))

ggplot(hitters, aes(x = error)) +
  geom_histogram(color = "white",fill = "black") +
  scale_x_continuous(labels = function(x) paste0("$",x/10^6,"m")) +
  theme_bw() +
  geom_hline(yintercept=0) +
  labs(x = "Prediction Error",y = NULL,title = "Hitters Prediction Error")

ggplot(pitchers, aes(x = error)) +
  geom_histogram(color = "white",fill = "black") +
  scale_x_continuous(labels = function(x) paste0("$",x/10^6,"m")) +
  theme_bw() +
  geom_hline(yintercept=0) +
  labs(x = "Prediction Error",y = NULL,title = "Pitchers Prediction Error")

