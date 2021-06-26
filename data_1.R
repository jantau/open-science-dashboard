#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Manipulation and Visualization of BIH dataset ----
# jan.taubitz@charite.de - 2021
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Clean the workspace ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())
gc()

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Load libraries ----
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(highcharter)
library(htmlwidgets)
library(janitor)
library(readxl)
library(tidyverse)
library(writexl)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

raw_data <- "raw_data/publications_cha_dashboard.csv"

cha_data <- read_csv(raw_data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define color and oa-status variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "NA")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Clean data, create some new variables ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data <- cha_data %>%
  clean_names() %>%
  mutate(doi = tolower(doi)) %>% # Convert dois to lower case
  distinct(doi, .keep_all = TRUE) %>%
  mutate(oa_color = replace_na(oa_color, "NA")) %>%
  mutate(oa_status = factor(oa_color, levels = oa_status_colors)) %>%
  mutate(is_oa = if_else(oa_status %in% c("gold", "hybrid", "green"), TRUE, FALSE), .after = "oa_status")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exploratory data analysis ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#sapply(data, function(x) length(unique(x)))
sort(table(data$oa_status), decreasing = TRUE)
#n_occur <- data.frame(table(data$titel))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of year and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data %>%
  ggplot(aes(x = year, fill = oa_status)) +
  geom_bar()

data_sum <- data %>%
  group_by(year, oa_status) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 1))

status_absolute <-
  hchart(data_sum,
         "column",
         hcaes(x = year, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color)

status_absolute_spline <-
hchart(data_sum,
       "spline",
       hcaes(x = factor(year), y = count, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_tooltip(shared = TRUE)

status_absolute_area <-
hchart(data_sum,
       "area",
       hcaes(x = factor(year), y = count, group = oa_status)) %>%
  hc_colors(color) %>%
  hc_tooltip(shared = TRUE) %>%
  hc_plotOptions(area = list(stacking = TRUE))

# saveWidget(status_absolute, file = "status_absolute.html") # , selfcontained = TRUE
# %>% hc_title(text = "Open access status in absolute numbers", align = "left", style = list(fontSize = "12px"))
# %>% hc_subtitle(text = text, align = "left", style = list(fontSize = "12px"))

status_percent <-
  hchart(data_sum,
         "column",
         hcaes(x = year, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_yAxis(labels = list(format = '{value} %'),
           max = 100) %>% # , reversedStacks = FALSE
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.value} Artikel ({point.percent} %)")

# reversed bar stacks https://www.highcharts.com/forum/viewtopic.php?t=10916

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of journals and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

journal_data <- data %>%
  filter(year == 2020) %>%
  group_by(journal_title, oa_status) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(count_zs = sum(count)) %>%
  ungroup() %>%
  filter(count_zs >= 5) %>%
  mutate(journal_title = forcats::fct_reorder(journal_title, -count_zs))

journal_data_2 <- journal_data %>%
  group_by(journal_title) %>%
  spread(oa_status, count, fill = 0) %>%
  gather(oa_status, count, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(count / sum(count) * 100, 1))

journal_absolute <- journal_data_2 %>%
  hchart("bar", hcaes(x = journal_title, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.count} Artikel ({point.percent} %)<br>{point.count_zs} Artikel insgesamt")

journal_percent <- journal_data_2 %>%
  hchart("bar", hcaes(x = journal_title, y = percent, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(labels = list(format = '{value} %'),
                         max = 100, reversedStacks = FALSE) %>%
  hc_size(height = 500) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.count} Artikel ({point.percent} %)<br>{point.value_zs} Artikel insgesamt")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of publishers and oa_status ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("data/data_unpaywall.Rda") # get publisher names from unpaywall dataset

data_publisher <- data_unpaywall %>%
  select(doi, publisher)

data_publisher_join <- data %>%
  filter(year == 2020) %>%
  select(doi, oa_status) %>%
  left_join(data_publisher, by = "doi")

data_publisher_join_sum <- data_publisher_join %>%
  group_by(publisher, oa_status) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(count_pub = sum(count)) %>%
  ungroup() %>%
 # filter(count_pub >= 5) %>%
  mutate(publisher = forcats::fct_reorder(publisher, -count_pub))

data_publisher_join_sum_2 <- data_publisher_join_sum %>%
  group_by(publisher) %>%
  spread(oa_status, count, fill = 0) %>%
  gather(oa_status, count, 3:8) %>%
  mutate(oa_status = factor(oa_status, levels = oa_status_colors)) %>%
  mutate(percent = round(count / sum(count) * 100, 1)) %>%
  filter(count_pub >= 3) %>%
  drop_na()

data_publisher_table <- data_publisher_join_sum_2 %>%
  select(-count_pub, -percent) %>%
  pivot_wider(names_from = oa_status, values_from = count)

publisher_absolute <- data_publisher_join_sum_2 %>%
  hchart("bar", hcaes(x = publisher, y = count, group = oa_status)) %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_colors(color) %>%
  hc_xAxis(min = 0,
           max = 15,
           scrollbar = list(enabled = TRUE)) %>%
  hc_size(height = 500) %>%
  hc_yAxis(reversedStacks = FALSE) %>%
  hc_tooltip(pointFormat = "<b>{point.oa_status}</b><br>{point.count} Artikel ({point.percent} %)<br>{point.count_pub} Artikel insgesamt")

publisher_donut <- data_publisher_join %>%
  group_by(publisher) %>%
  summarise(count = n()) %>%
  mutate(publisher_2 = if_else(count <= 500, "andere Verlage", publisher)) %>%
  group_by(publisher_2) %>%
  summarise(count = sum(count)) %>%
  mutate(perc = round(count / sum(count) * 100, 1)) %>%
  arrange(-count) %>%
  hchart("pie",
         hcaes(x = publisher_2, y = count),
         size = "65%",
         innerSize = "50%") %>%
  hc_tooltip(pointFormat = "{point.count} Artikel ({point.perc} %)")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
