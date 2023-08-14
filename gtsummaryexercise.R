library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							sleep_wkdy, sleep_wknd, income))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							sleep_wkdy, sleep_wknd, income),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		sleep_wkdy ~ "Sleep Weekday",
		sleep_wknd ~ "Sleep Weekend",
		income ~ "Income"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							starts_with("sleep"), income),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		sleep_wkdy ~ "Sleep Weekday",
		income ~ "Income",
		region_cat ~ "Region",
		sleep_wknd ~ "Sleep Weekend"
	),
	statistic = list(
		income ~ "10th {p10}, 90th  {p90}",
		starts_with("sleep") ~ "min = {min}; max={max}"
	),
	digits = list(
		income ~ c(3,3),
		starts_with("sleep") ~ c(1,1)) %>%
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**")
#for the income variable show 10 and 90 percentile and 3 digits

,

	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

