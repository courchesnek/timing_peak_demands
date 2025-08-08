#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
male_mating <- read.csv("Output/final_weighted_predictions_m.csv")
female_lactation <- read.csv("Output/final_weighted_predictions_f.csv")

#test predictions
#1) On-midden cone use: During lactation, females will have a lower proportion of 
#     on-midden cone feeding than males do during mating.
male_on <- male_mating %>% 
  filter(food_group == "cone", midden_status == "on") %>% 
  summarise(
    lower = CI_lower,
    upper = CI_upper)

female_on <- female_lactation %>% 
  filter(food_group == "cone", midden_status == "on") %>% 
  summarise(
    lower = CI_lower,
    upper = CI_upper) 

# one‐sided test: female_on’s upper bound < male_on’s lower bound?
test1 <- female_on$upper < male_on$lower
test1
# ANSWER = TRUE - female on‐midden cone share is significantly LOWER than male’s

#2) Off-midden foraging: During lactation, females will have a higher combined proportion of 
#     off-midden feeding (both cones and non-cone items) than males do during mating.
male_off <- male_mating %>% 
  filter(midden_status == "off") %>% 
  summarise(
    lower = sum(CI_lower),
    upper = sum(CI_upper))

female_off <- female_lactation %>% 
  filter(midden_status == "off") %>% 
  summarise(
    lower = sum(CI_lower),
    upper = sum(CI_upper))

# one‐sided test: female_off’s lower bound > male_off’s upper bound?
test2 <- female_off$lower > male_off$upper
test2
# FALSE  = female off‐midden share is not significantly higher than male’s


# read in data ------------------------------------------------------------
male_feeding <- read.csv("Output/male_feeding_detailed.csv")
female_feeding <- read.csv("Output/female_feeding_detailed.csv",
                           stringsAsFactors = FALSE,
                           colClasses      = c(sex = "character"))

#run the bootstrap on your two event-level data frames to get CIs and p-values 
#   for the sex differences in on-midden cones and total off-midden

# 1) function to compute the two differences from raw events
calc_diffs <- function(m_df, f_df) {
  m_on      <- mean(m_df$food_group=="cone" & m_df$midden_status==1)
  f_on      <- mean(f_df$food_group=="cone" & f_df$midden_status==1)
  diff_on   <- f_on - m_on
  
  m_off_any <- mean(m_df$midden_status==0)
  f_off_any <- mean(f_df$midden_status==0)
  diff_off  <- f_off_any - m_off_any
  
  c(diff_on = diff_on, diff_off = diff_off)}

# 2) bootstrap resampling
set.seed(2025)
nboot <- 2000

boot_out <- replicate(nboot, {
  mboot <- sample_n(male_feeding,   nrow(male_feeding),   replace = TRUE)
  fboot <- sample_n(female_feeding, nrow(female_feeding), replace = TRUE)
  calc_diffs(mboot, fboot)})

boot_df <- as.data.frame(t(boot_out))
names(boot_df) <- c("diff_on", "diff_off")

# 3) Summarize bootstrap distributions
ci_on  <- quantile(boot_df$diff_on,  c(0.025, 0.975))
ci_off <- quantile(boot_df$diff_off, c(0.025, 0.975))

# 4) One‐sided p‐values
#    p_on  = P(diff_on  >= 0)  ; test female_on < male_on
#    p_off = P(diff_off <= 0)  ; test female_off > male_off
p_on  <- mean(boot_df$diff_on  >= 0)
p_off <- mean(boot_df$diff_off <= 0)

# 5) Display results
cat("On-midden cone share difference (F - M):\n",
    sprintf("  95%% bootstrap CI: [%.3f, %.3f]\n", ci_on[1], ci_on[2]),
    sprintf("  one-sided p = %.3f  (H1: female < male)\n\n", p_on))

cat("Total off-midden share difference (F - M):\n",
    sprintf("  95%% bootstrap CI: [%.3f, %.3f]\n", ci_off[1], ci_off[2]),
    sprintf("  one-sided p = %.3f  (H1: female > male)\n", p_off))


