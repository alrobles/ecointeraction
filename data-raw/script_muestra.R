### script muestra

data(iris)
Sys.sleep(20)
S <- mean(iris$Sepal.Length)
write.csv(S, "S.csv")
dengue <- mammalvirus %>%
  group_by(mammal_species) %>%
  filter(virus == "WNV") %>%
  summarise(incidence = sum(incidence)) %>%
  arrange(desc(incidence)) %>%
  filter(incidence > 10)

m_pl = displ$new(dengue$incidence)
est = estimate_xmin(m_pl)
m_pl$setXmin(est)
plot(m_pl)
bs_p = bootstrap_p(m_pl, no_of_sims = 500, threads = 8)
bs_p$p
