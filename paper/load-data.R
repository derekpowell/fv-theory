# # load and pre-process data for reproducible manuscript

## notes: 
## 1. FFQ_F_grapes needed to be renamed ffq_fruit_grapes
## 2. need to load delayed test data!

# load packages and custom helper functions
library(tidyverse)
source("../code/helpers.R")

### Study 1
### ----------------------------------------


df1_raw <- read_qualtrics("../data/nutrition+scales+prereg_February+26,+2019_14.51.csv") %>%
  as_tibble() %>%
  select(workerId, StartDate:Finished, sex:ffq_fruit_plums, ffq_time_Page.Submit, e01:surv_pay, LocationLatitude, LocationLongitude) %>%
  rename(Duration = Duration..in.seconds., ffq_fruit_grapes = FFQ_F_grapes)

# and remove bad subjects
df1 <- df1_raw %>%
  filter(check_2==2, check_7==7, ffq_veg_check6==6, ffq_fruit_check30==30) %>%
  select(-contains("check"))

df1 <- df1 %>%
  mutate(
    income = recode_factor(
      income,
      `1` = "<20k",
      `2` = "20kto30k",
      `3` = "30kto50k",
      `4` = "50kto70k",
      `5` = "70kto100k",
      `6` = ">100k",
      `0` = "refuse",
                  )
    # educ = recode(
    #   educ,
    #   `1` = "none",
    #   `2` = "HS",
    #   `3` = "someUG",
    #   `4` = "bachlors",
    #   `5` = "someGrad",
    #   `6` = "graduate",
    #   `7` = "doctorate",
    #   `-1` = "refuse",
    # )
  )

### Study 2 (must be anonymized!)
### ----------------------------------------

pre_raw2 <- read_qualtrics("../data_private/nutrition+interv+may2019+pretest_May+16,+2019_09.33.csv")

post_raw2 <- read_qualtrics("../data_private/nutrition+interv+may2019+posttest_May+17,+2019_12.08.csv")


pre_df2 <- pre_raw2 %>%
  as_tibble() %>%
  filter(Progress == 100, check_2 == 2, check_7 == 7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, m01:e06, sex:enjoy_cook
  ) %>%
  # mutate_at(vars(contains("0")), as.numeric) %>%
  rename_at(vars(contains("0")), function(x) {
    paste0("pre_", x)
  })


post_df2 <- post_raw2 %>%
  as_tibble() %>%
  filter(Progress == 100, check_2 == 2, check_7 == 7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, condition, m01:e06
  ) %>%
  rename_at(vars(contains("0")), function(x) {
    paste0("post_", x)
  })

post_delay_raw2 <- read_qualtrics("../data_private/nutrition+interv+may2019+delay+posttest_May+23,+2019_10.08.csv")

post_delay_df2 <- post_delay_raw2 %>%
  as_tibble() %>%
  filter(Progress == 100, check_2 == 2, check_7 == 7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, m01:e06
  ) %>%
  rename_at(vars(contains("0")), function(x) {
    paste0("post2_", x)
  })

df2 <- left_join(post_df2, pre_df2, by = "workerId") %>%
  left_join(post_delay_df2, by = "workerId")

fr_df2 <- post_raw2 %>%
  as_tibble() %>%
  filter(Progress == 100, check_2 == 2, check_7 == 7) %>%
  select(workerId, condition, leafy_v_carrots, broccoli_cancer, phyto, free_radicals)

write_csv(fr_df2, "../data_private/interv-free-responses-may2019.csv")

rm(fr_df2)

### Study 3 (must anonymize!)
### ----------------------------------------

# load + preprocess data
pre_raw3 <- read_qualtrics("../data_private/nutrition+interv+june2019+pretest_June+5,+2019_09.32.csv")
post_raw3 <- read_qualtrics("../data_private/nutrition+interv+june2019+posttest_June+6,+2019_13.52.csv", skip=4)

dropped_out <- read_csv("../data_private/nutrition_interv_june2019_posttest-Responses in Progress.csv") %>%
  select(condition)

pre_df3 <- pre_raw3 %>%
  filter(Progress==100, check_2==2, check_7==7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, m01:e06, sex:enjoy_cook
  ) %>%
  rename_at(vars(contains("0")), function(x){paste0("pre_",x)})

post_df3 <- post_raw3 %>%
  filter(Progress==100, check_2==2, check_7==7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, condition, m01:e06
  ) %>%
  rename_at(vars(contains("0")), function(x){paste0("post_",x)})

post_delay_raw3 <- read_qualtrics("../data_private/nutrition+interv+june2019+delay+posttest_June+19%2C+2019_15.08.csv")

post_delay_df3 <- post_delay_raw3 %>%
  as_tibble() %>%
  filter(Progress==100, check_2==2, check_7==7) %>%
  select(-check_2, -check_7) %>%
  select(
    workerId, m01:e06
  ) %>%
  # mutate_at(vars(contains("0")), as.numeric) %>%
  rename_at(vars(contains("0")), function(x){paste0("post2_",x)})


df3 <- left_join( post_df3, pre_df3, by="workerId") %>%
  distinct(workerId, .keep_all = TRUE)  %>% # two duplicates with identical entries, not sure why
  left_join(post_delay_df3) %>%
  mutate(condition = factor(condition, levels = c("theoryBased","myplate","control")))

