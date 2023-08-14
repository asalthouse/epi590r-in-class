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

#Univariate regression looking at association bt sex (x) between nsibs, sleepwkday/wkend, and income

tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, starts_with("sleep"),
							income),
	method = lm)

#fit a poisson for number of sibs with 3 predictors or your choice
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = poisson())

## Tables

tbl_regression(
	poisson_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


#question 5
eyes_binomial <-glm(glasses~ eyesight + sex_cat
									 family=binomial(link="log"), data= nlsy)
poisson() <-glm(glasses~ eyesight + sex_cat
									 family=poisson(link="log"), data= nlsy)

tbl_eyes_binomial <-tbl_regression(eyes_binomial,
																	 exponentiate=TRUE)
tbl_eyes_poisson <-tbl_regression(eyes_poisson,
																	 exponentiate=TRUE,
																	tidy_fun = partial(tidy_robust, vcov = "HC1"))










