library(survival)
library(haven)

# Load the lung dataset
lung <- survival::lung

# Recode status to Stata convention
# (R status: 1=censored, 2=dead -> Stata failure: 0=censored, 1=dead)
lung$stata_status <- lung$status - 1

# Stata names can't contain periods (e.g., ph.ecog -> ph_ecog)
names(lung) <- gsub(".", "_", names(lung), fixed = TRUE)

# Value labels (become Stata value labels; codes stay numeric)
lung$status <- labelled(lung$status, c(Censored = 1, Dead = 2))
lung$stata_status <- labelled(lung$stata_status, c(Censored = 0, Dead = 1))
lung$sex <- labelled(lung$sex, c(Male = 1, Female = 2))
lung$ph_ecog <- labelled(lung$ph_ecog, c(
  "Asymptomatic" = 0,
  "Symptomatic, ambulatory" = 1,
  "In bed <50% of day" = 2,
  "In bed >50% of day" = 3,
  "Bedbound" = 4
))

# Variable labels
var_labels <- c(
  inst         = "Institution code",
  time         = "Survival time (days)",
  status       = "Censoring status (1=censored, 2=dead)",
  age          = "Age (years)",
  sex          = "Sex",
  ph_ecog      = "ECOG performance score (physician)",
  ph_karno     = "Karnofsky score (physician)",
  pat_karno    = "Karnofsky score (patient)",
  meal_cal     = "Calories consumed at meals",
  wt_loss      = "Weight loss in last six months (lbs)",
  stata_status = "Death indicator (0=censored, 1=dead)"
)
for (v in names(var_labels)) attr(lung[[v]], "label") <- var_labels[[v]]

# Write to Stata .dta format
write_dta(lung, "lung.dta")
