library(tidyverse)
library(tidymodels)
library(splines)
library(corrplot)
library(fastDummies)
library(fitdistrplus)
library(pscl)

pitchers = read_csv("https://raw.githubusercontent.com/ajaypatell8/MLB-Contracts/main/pitchers_contracts_stats.csv")

# 0. FILTER TO OBS. WE WANT TO INCLUDE
# specifically, remove observations for pitchers and contracts w/ no AAV/years

pitchers = pitchers %>%
  filter(AAV > 1 & Position != "OF") %>%
  mutate(YearsNew = Years - 1,
         Starter = ifelse(Position == "SP",1,0)) %>%
  mutate(CleanName = paste0(Name," ",FAYear))


lambda = fitdistrplus::fitdist(pitchers$YearsNew, "pois", method = "mle")$estimate

pois = data.frame(x=rpois(10000, lambda))

ggplot(pois, aes(x=x,y=after_stat(density))) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.5) +
  geom_histogram(data = pitchers, aes(x=YearsNew,y=after_stat(density)), binwidth = 1, fill = "blue", alpha = 0.5)

model = zeroinfl(YearsNew ~ bs(Age, degree = 3) * WAR_rolling * Starter, data = pitchers)

summary(model)

pitchers_pred = pitchers %>%
  mutate(pred = predict(model,.) + 1,
         residual = Years - pred,
         PredYears = round(pred),
         TrueYears = Years)

write_csv(pitchers_pred, "projections/PitcherYrs.csv")

