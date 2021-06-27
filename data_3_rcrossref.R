#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Crossref data ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load sources and libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("data_1.R", encoding = 'UTF-8')
#source("datenvergleich.Rmd", encoding = 'UTF-8')

library(rcrossref)

data_doi <-
  data %>%
  drop_na(doi)

 data_doi_2018 <- data_doi %>%
   filter(year == 2018)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data from crossref ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Documentation: https://docs.ropensci.org/rcrossref/articles/rcrossref.html

#data_citation_2018 <- cr_citation_count(doi = data_doi_2018$doi)
#save(data_citation_2018, file = "data/data_citation_2018.Rda")
load("data/data_citation_2018.Rda")

#sample_data <- cr_citation_count(doi = sample_data$doi)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data with medbib data ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_citation_join <- data_doi_2018 %>%
  select(doi, oa_status) %>%
  left_join(data_citation_2018, by = "doi")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualization ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

median_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(median = median(count, na.rm = TRUE)) %>%
  mutate(median = replace_na(median, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = median, color = color)) %>%
  hc_title(text = "Median") %>%
  hc_tooltip(pointFormat = "Der Median liegt bei {point.median} Zitierungen")

mean_citation <-
  data_citation_join %>%
  group_by(oa_status) %>%
  summarise(mean = mean(count, na.rm = TRUE)) %>%
  mutate(mean = replace_na(mean, 0)) %>%
  hchart("column",
         hcaes(x = oa_status, y = mean, color = color)) %>%
  hc_title(text = "Mittelwert") %>%
  hc_tooltip(pointFormat = "Der Mittelwert liegt bei {point.mean:.1f} Zitierungen")


#data_citation_join <- data_citation_join %>%
#  left_join(colors_df, by = "oa_status") %>%
#  mutate(oa_status = factor(oa_status, levels = oa_status_colors))


dat <- data_to_boxplot(data_citation_join, count, oa_status, oa_status, add_outliers = FALSE, name = oa_status_colors)

boxplot_citation <-
  highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat) %>%
  hc_colors(color) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
    hc_yAxis(min = 0)


#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_violin()

#ggplot(data_citation_join, aes(oa_status, count)) +
#  geom_boxplot() +
#  ylim(0,50)

#ggplot(data_citation_join, aes(oa_status, fill=count)) +
#  geom_bar()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load license data from crossref ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Unpaywall data is better
# https://github.com/subugoe/metacheck/blob/main/R/metrics_cc.R


#req <- get_cr_md(sample_data$doi)

#cr_licenses(query = sample_data$doi)

#cr_citation_count(doi = sample_data$doi)

# out <- cr_compliance_overview(req)
