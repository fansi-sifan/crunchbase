

# matrix plot
load("../metro.data/data/county_cbsa_st.rda")
load("../metro-datasets/metro_monitor_2019/metro_monitor_2019.rda")

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
  left_join(county_cbsa_st[c("cbsa_code","cbsa_emp")], by = "cbsa_code") %>%
  left_join(cbsa_metromonitor[c("cbsa_code","output_per_job","employment_at_firms_0_5_years_old")], by = "cbsa_code") %>%
  left_join(cbsa_VC, by = "cbsa_code")%>%
  left_join(cbsa_I5HGC, by = "cbsa_code")%>%
  left_join(cbsa_patentCOMP, by = "cbsa_code")%>%
  left_join(cbsa_USPTO, by = "cbsa_code")%>%
  left_join(cbsa_univRD, by = "cbsa_code")%>%
  mutate(VC = VC/cbsa_emp,
         patent_total = VC/cbsa_emp,
         RD_value = RD_value/cbsa_emp,
         young_firm = employment_at_firms_0_5_years_old/cbsa_emp)

save(cbsa_cor, file = "data/cbsa_cor.rda")

corrplot(M, method = "color", type ="upper",
         addCoef.col = "black", tl.col = "black",tl.srt=45)

