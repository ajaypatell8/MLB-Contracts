source("scripts/HitterAAV.R",echo=F)
source("scripts/PitcherAAV.R",echo=F)
source("scripts/HitterYears.R",echo=F)
source("scripts/PitcherYears.R",echo=F)

HitterYears = read_csv("projections/HitterYrs.csv")
PitcherYears = read_csv("projections/PitcherYrs.csv")
HitterAAV = read_csv("projections/HitterAAV.csv")
PitcherAAV = read_csv("projections/PitcherAAV.csv")

hitters = inner_join(HitterYears,HitterAAV, by = c("FAYear","Name")) %>%
  dplyr::select(Name, FA_Season = FAYear, ProjYears = PredYears, ProjAAV = aav_pred,, Years = TrueYears, AAV = AAV.x) %>%
  mutate(ProjTotal = ProjYears*ProjAAV,Total = Years*AAV) %>%
  dplyr::select(Name, FA_Season, ProjYears, ProjAAV, ProjTotal, Years, AAV,Total) %>%
  arrange(-ProjTotal)

pitchers = inner_join(PitcherYears,PitcherAAV, by = c("FAYear","Name")) %>%
  dplyr::select(Name, FA_Season = FAYear, ProjYears = PredYears, ProjAAV = aav_pred,, Years = TrueYears, AAV = AAV.x) %>%
  mutate(ProjTotal = ProjYears*ProjAAV,Total=Years*AAV) %>%
  dplyr::select(Name, FA_Season, ProjYears, ProjAAV, ProjTotal, Years, AAV,Total) %>%
  arrange(-ProjTotal)

write_csv(hitters,"projections/HittersFinalProjections.csv")
write_csv(pitchers,"projections/PitchersFinalProjections.csv")
