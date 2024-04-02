#' data_poll
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
to_factor_poll <- function(df, age_bounds) {
  breaks <- c(-1, age_bounds[2:length(age_bounds)] - 1, 200)
  labels <- c(paste0(age_bounds[1:(length(age_bounds)-1)], '-', age_bounds[2:length(age_bounds)] - 1),
              paste0(age_bounds[length(age_bounds)], '+'))

  df <- df |> mutate(
    sex  = to_factor(sex, c("female"), other = "male"),
    race = to_factor(race, c("white", "black"), other = "other"),
    edu  = to_factor(edu, c("no hs", "some college", "4-year college", "post-grad"), other = "hs"),
    age  = cut(df$age, breaks, labels) |> as.character()
  )

  return(df)
}


aggregate_poll <- function(df, age_bounds, threshold = 0) {
  # identify columns
  df <- find_columns(df, expected_columns = c("sex", "race", "age", "edu", "state", "response"))

  # impute missing demographic data based on frequency
  df <- df |> mutate(across(c(sex, race, age, edu), impute))

  # # create factors from raw values
  df <- to_factor_poll(df, age_bounds)

  # aggregate test records based on combinations of factors
  # and omit cells with small number of tests
  df <- df |>
    group_by(state, edu, age, race, sex) |>
    filter(n() >= threshold) |>
    summarize(
      total = n(),
      positive = sum(response)
    ) |>
    ungroup()

  return(df)
}


check_poll_data <- function(df, expected_columns, na_threshold = 0.5) {
  errors <- list()

  df <- find_columns(df, expected_columns)
  missing <- df |> lapply(function(c) all(c == "")) |> unlist()
  missing_names <- names(missing)[missing]

  if(length(missing_names) > 1 | (length(missing_names) == 1 & !"state" %in% missing_names)) {
    errors$missing_column <- paste0("The following columns are missing: ",
                                    paste(missing_names, collapse = ", "),
                                    "(\"state\" column is optional)")
  } else {
    # check data types
    expected_types <- c("character", "character", "character", "character", "character|integer|numeric", "integer|numeric", "integer|numeric")
    types <- df |> lapply(class) |> unlist()
    valid <- mapply(grepl, types, expected_types)

    if(any(!valid)) {
      errors$data_type <- paste0("Columns corresponding to the following variables have inappropriate data types: ",
                                 paste(expected_columns[!valid], collapse = ", "))
    } else {
      na_percents <- df |>
        lapply(function(c) sum(as.numeric(is.na(c))) / length(c)) |>
        unlist()
      exceed <- na_percents > na_threshold

      if(any(exceed)) {
        errors$na <- paste0("Columns corresponding to the following variables have more than ",
                            na_threshold * 100, " percent rows with missing data: ",
                            paste(expected_columns[exceed], collapse = ", "))
      } else {
        # check if state column is missing
        if("state" %in% missing_names) {
          errors$state <- "States are not provided (\"state\" column is optional)"
        }

        # check if positive votes are less than or equal to total votes
        if(any(df$positive > df$total)) {
          errors$count <- "The number of supporting votes cannot be greater than the total number of votes"
        }
      }
    }
  }

  return(errors)
}

mod_fips <- function(df) {

  df <- df |>
    mutate(fips = substr(fips, 1, 2)) |>
    select(-county) |>
    distinct(fips, .keep_all = TRUE)

  return(df)
}

to_fips <- function(state_vec, fips_state) {
  if(is.null(state_vec)) {
    return(NULL)
  }

  counts <- fips_state |> apply(2, function(c) sum(c %in% state_vec))
  colname <- names(counts)[which.max(counts)]

  if(colname == "fips") {
    fips <- if(is.numeric(state_vec)) sprintf("%02d", state_vec) else state_vec
  } else {
    fips <- fips_state$fips[match(state_vec, fips_state[[colname]])]
  }

  return(fips)
}

get_state_predictors <- function(df) {
  df <- janitor::clean_names(df)
  all_cols <- names(df)
  state_col <- all_cols[grepl("state", all_cols, ignore.case = TRUE)]

  if(length(state_col) == 0) {
    return(data.frame())
  }

  bool <- df |>
    group_by(!!!syms(state_col)) |>
    summarize_all(n_distinct) |>
    lapply(function(c) all(c == 1)) |>
    unlist()

  state_pred_cols <- all_cols[bool]

  state_preds <- df |>
    select(all_of(c(state_col, state_pred_cols))) |>
    distinct(!!!syms(state_col), .keep_all = TRUE)

  names(state_preds)[1] <- "state"

  return(state_preds)
}

prepare_brms_poll <- function(
    data,
    pstrat,
    covar,
    demo_levels
) {

  states_in_data <- unique(data$state)
  if(is.null(states_in_data)) {
    new_data <- pstrat |>
      group_by(sex, race, age, edu) |>
      summarize(total = sum(total)) |>
      ungroup()
  } else {
    new_data <- pstrat |> filter(state %in% states_in_data)
  }

  new_data <- new_data |> mutate(
    sex  = factor(sex, levels = demo_levels$sex),
    race = factor(race, levels = demo_levels$race),
    age  = factor(age, levels = demo_levels$age),
    edu  = factor(edu, levels = demo_levels$edu)
  )

  # append state-level predictors
  if(ncol(covar) > 1) {
    data <- left_join(data, covar, by = "state")
    new_data <- left_join(new_data, covar, by = "state")
  }

  # create lists of all factor levels
  levels <- demo_levels
  if(!is.null(states_in_data)) {
    levels$state <- states_in_data
  }

  # list of variables for model specification
  vars <- list(
    `Individual-level Predictor` = c("sex", "race", "age", "edu")
  )

  if(!is.null(states_in_data)) {
    vars[["Geographic Indicator"]] <- "state"
    vars[["Geographic Predictor"]] <- setdiff(names(covar), "state")
  }


  return(list(data, new_data, levels, vars))
}


### FOR GENERATING STATIC DATA
# Codes below are adapted from https://bookdown.org/jl5522/MRP-case-studies/

state_abb <- datasets::state.abb
state_fips <- c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,
                25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,
                44,45,46,47,48,49,50,51,53,54,55,56)

recode_fips <- function(column) {
  return(factor(column, levels = state_fips, labels = state_abb))
}

# Recode CCES
clean_cces <- function(df, remove_nas = TRUE){

  ## Abortion -- dichotomous (0 - Oppose / 1 - Support)
  df$abortion <- abs(df$CC18_321d-2)

  ## State -- factor
  df$state <- recode_fips(df$inputstate)

  ## Gender -- dichotomous (coded as -0.5 Female, +0.5 Male)
  df$male <- abs(df$gender-2)-0.5

  ## ethnicity -- factor
  df$eth <- factor(df$race,
                   levels = 1:8,
                   labels = c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern"))
  df$eth <- fct_collapse(df$eth, "Other" = c("Hispanic", "Asian", "Other", "Middle Eastern", "Mixed", "Native American"))

  ## Age -- cut into factor
  df$age <- 2018 - df$birthyr
  df$age <- cut(as.integer(df$age), breaks = c(0, 29, 39, 49, 59, 69, 120),
                labels = c("18-29","30-39","40-49","50-59","60-69","70+"),
                ordered_result = TRUE)

  ## Education -- factor
  df$educ <- factor(as.integer(df$educ),
                    levels = 1:6,
                    labels = c("No HS", "HS", "Some college", "Associates", "4-Year College", "Post-grad"), ordered = TRUE)
  df$educ <- fct_collapse(df$educ, "Some college" = c("Some college", "Associates"))

  # Filter out unnecessary columns and remove NAs
  df <- df |> select(abortion, state, eth, male, age, educ)
  if (remove_nas){
    df <- df |> drop_na()
  }

  return(df)

}

create_pstrat_table <- function(df) {

  ## Remove non-citizens
  df <- df |> filter(CITIZEN<3)

  ## State
  df$state <- df$STATEFIP

  ## Gender
  df$male <- abs(df$SEX-2)-0.5

  ## Ethnicity
  df$RACE <- factor(df$RACE,
                    levels = 1:9,
                    labels = c("White", "Black", "Native American", "Chinese",
                               "Japanese", "Other Asian or Pacific Islander",
                               "Other race, nec", "Two major races",
                               "Three or more major races"))
  df$eth <- fct_collapse(df$RACE,
                         "Other" = c("Native American", "Chinese",
                                     "Japanese", "Other Asian or Pacific Islander",
                                     "Other race, nec", "Two major races",
                                     "Three or more major races"))

  ## Age
  df$age <- cut(as.integer(df$AGE), breaks = c(0, 17, 29, 39, 49, 59, 69, 120),
                labels = c("0-17", "18-29","30-39","40-49","50-59","60-69","70+"),
                ordered_result = TRUE)

  # filter out underages
  df <- filter(df, age!="0-17")
  df$age <- droplevels(df$age)

  ## Education
  # we need to use EDUCD (i.e. education detailed) instead of EDUC (i.e. general codes), as the
  # latter does not contain enough information about whether high school was completed or not.
  df$educ <- cut(as.integer(df$EDUCD), c(0, 61, 64, 100, 101, Inf),
                 ordered_result = TRUE,
                 labels = c("No HS", "HS", "Some college", "4-Year College", "Post-grad"))

  # Clean df by dropping NAs and cleaning states with recode_fips
  df <- df |> drop_na(state, eth, male, age, educ, PERWT) |>
    select(state, eth, male, age, educ, PERWT) |> filter(state %in% state_fips)

  # Generate cell frequencies using groupby
  poststrat_df <- df |>
    group_by(state, eth, male, age, educ, .drop = FALSE) |>
    summarise(n = sum(as.numeric(PERWT)))

  return(poststrat_df)
}
