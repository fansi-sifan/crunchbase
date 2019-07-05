library(tidyverse)

# matrix plot
load("../metro-data-warehouse/data/county_cbsa_st.rda")
load("../metro-datasets/metro_monitor_2019/metro_monitor_2019.rda")

cbsa_ECI <- read.csv("Metro_ECI_SI.csv") %>%
  mutate(cbsa_code = as.character(msa)) %>%
  select(-contains("msa"),-X)

cbsa_VC <- read.csv("../metro-datasets/source/VC.csv") %>%
  filter(round == "Total VC" & measure == "Capital Invested ($ M) per 1M Residents") %>%
  mutate(cbsa_code = as.character(cbsa13))%>%
  select(cbsa_code, VC = value)

cbsa_I5HGC <- read.csv("../metro-datasets/source/I5HGC_density.csv") %>%
  mutate(cbsa_code = as.character(CBSA))%>%
  select(cbsa_code, Inc5000_density = I5HGC_Density)

cbsa_patentCOMP <- read.csv("../metro-datasets/source/Complexity_msa.csv") %>%
  mutate(cbsa_code = as.character(cbsa))%>%
  select(cbsa_code, patent_complexity = complex)

cbsa_USPTO <- read.csv("../metro-datasets/source/USPTO_msa.csv") %>%
  mutate(cbsa_code = substr(as.character(ID.Code), 2, 6))%>%
  select(cbsa_code, patent_total = Total)

NSF_univRD <- read.csv("../metro-datasets/source/NSF_univ.csv")
cbsa_univRD <- NSF_univRD %>%
  group_by(cbsacode) %>%
  summarise(
    RDtotal = sum(Deflated.Total.R.D.Expenditures.in.All.Fields.Sum.),
    RDtotal_biz = sum(as.numeric(as.character(Deflated.Business.Financed.R.D.Expenditures.Sum.)))
  ) %>%
  mutate(cbsa_code = as.character(cbsacode))%>%
  select(cbsa_code, RD_value = RDtotal)

cbsa_cor <- cbsa_KCI %>%
  select(-cbsa_name)%>%
  full_join(county_cbsa_st[c("cbsa_code","cbsa_emp")], by = "cbsa_code") %>%
  full_join(cbsa_metromonitor[c("cbsa_code","output_per_job","employment_at_firms_0_5_years_old")], by = "cbsa_code") %>%
  full_join(cbsa_VC, by = "cbsa_code")%>%
  full_join(cbsa_I5HGC, by = "cbsa_code")%>%
  full_join(cbsa_patentCOMP, by = "cbsa_code")%>%
  full_join(cbsa_USPTO, by = "cbsa_code")%>%
  full_join(cbsa_univRD, by = "cbsa_code")%>%
  full_join(cbsa_ECI, by = "cbsa_code")%>%
  mutate(patent_pemp = patent_total/cbsa_emp,
         RD_pemp = RD_value/cbsa_emp,
         pct_young_firm = employment_at_firms_0_5_years_old/cbsa_emp)%>%
  rename(young_firms = employment_at_firms_0_5_years_old)%>%
  unique()

save(cbsa_cor, file = "data/cbsa_cor.rda")

colSums(!is.na(cbsa_cor))

# plot
M <- cor(cbsa_cor%>%select_if(is.numeric),use = "pairwise.complete.obs")

corrplot(M, method = "color", type ="upper",
         addCoef.col = "black", tl.col = "black",tl.srt=45)

cbsa_cor %>%
  filter(cbsa_code %in% c("19740","24340"))



plot_mean_ubi(complexity_cbsa_final)+
  geom_text(check_overlap = T, hjust = 0, nudge_x = 0.1)+theme_classic()+
  scale_x_continuous(limits = c(0,300))

ggplotly(plot_mean_ubi(complexity_cbsa_final))

corrplot(M, method = "color", type ="upper",
         addCoef.col = "black", tl.col = "black",tl.srt=45)



fit <- lm(startup_complexity ~ cbsa_emp+output_per_job+VC+patent_complexity+eci+coi+patent_pemp+RD_pemp+pct_young_firm, data = cbsa_cor)

model <- na.omit(cbsa_cor %>%
                   select(startup_complexity, cbsa_emp, output_per_job, patent_pemp, eci, patent_complexity)%>%
                   mutate(cbsa_emp = cbsa_emp/1000000,output_per_job = output_per_job/1000))

fit <- lm(startup_complexity ~ cbsa_emp+eci+patent_pemp, data = model)
fit <- lm(output_per_job ~ cbsa_emp+startup_complexity+patent_complexity, data = model)
fit <- lm(output_per_job ~ cbsa_emp+startup_complexity, data = model)

summary(fit)
format(summary(fit)$r.squared, digits = 2)



