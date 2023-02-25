library(tidyverse)
library(tidymodels)
library(splines)
library(corrplot)
library(fastDummies)
library(fitdistrplus)
library(pscl)

hitters = read_csv("https://raw.githubusercontent.com/ajaypatell8/MLB-Contracts/main/hitters_contracts_stats.csv")

# 0. FILTER TO OBS. WE WANT TO INCLUDE
# specifically, remove observations for pitchers and contracts w/ no AAV/years

hitters = hitters %>%
  filter(AAV > 1 & Position != "RP") %>%
  mutate(YearsNew = Years - 1) %>%
  mutate(CleanName = paste0(Name," ",FAYear))

  
lambda = fitdistrplus::fitdist(hitters$YearsNew, "pois", method = "mle")$estimate

pois = data.frame(x=rpois(10000, lambda))

ggplot(pois, aes(x=x,y=after_stat(density))) +
  geom_histogram(binwidth = 1, fill = "red", alpha = 0.5) +
  geom_histogram(data = hitters, aes(x=YearsNew,y=after_stat(density)), binwidth = 1, fill = "blue", alpha = 0.5)

model = zeroinfl(YearsNew ~ bs(Age, degree = 3) * WAR_rolling + PA, data = hitters)

summary(model)

hitters_pred = hitters %>%
  mutate(pred = predict(model,.) + 1,
         residual = Years - pred,
         PredYears = round(pred),
         TrueYears = Years)

write_csv(hitters_pred, "projections/HitterYrs.csv")

