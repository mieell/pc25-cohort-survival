library(ggplot2)

raw_path <- "data/raw/psa_jhs_cohort_survival.csv"
source_url <- "https://openstat.psa.gov.ph/PXWeb/pxweb/en/DB/DB__3I__G04/0033I3C41S1.px"
first_model_year <- 2004L
first_test_year <- 2021L
decline_cutoff <- -3
score_cutoff <- 0.30
excluded_regions <- c("Region VI", "Region VII", "Negros Island Region (NIR)",
                      "Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)")

prepare_data <- function(path) {
  if (!file.exists(path)) stop("Missing ", path, ". Restore the original source extract under data/raw.")
  raw <- read.csv(path, encoding = "UTF-8", check.names = FALSE, colClasses = "character",
                  na.strings = "", stringsAsFactors = FALSE)
  names(raw)[1] <- sub(paste0("^", intToUtf8(0xfeff)), "", names(raw)[1], perl = TRUE)
  value_field <- grep("^Value \\(", names(raw), value = TRUE)
  fields <- c("Indicator", "Geolocation", "Level of Education", "Sex", "Year",
              value_field, "Source status", "Release", "Source")
  if (length(value_field) != 1L || !all(fields %in% names(raw)) || !nrow(raw)) {
    stop("The source extract is empty or has an unexpected schema.")
  }
  if (anyNA(raw$Year) || !all(grepl("^[0-9]{4}$", raw$Year)) ||
      !all(raw[["Level of Education"]] %in% "Secondary (Junior High School)") ||
      !all(raw$Sex %in% c("Both Sexes", "Female", "Male")) || anyNA(raw$Geolocation)) {
    stop("Invalid year, sex, geography, or education level in the source.")
  }
  duplicates <- duplicated(raw[fields])
  extract <- raw[!duplicates, ]
  region <- sub(" [1234]/$", "", sub("^\\.\\.", "", extract$Geolocation))
  year <- as.integer(extract$Year)
  sex <- extract$Sex
  key <- paste(region, sex, year, sep = "|")
  if (anyDuplicated(key)) stop("Different source rows share a region, sex and year key.")
  rate <- suppressWarnings(as.numeric(extract[[value_field]]))
  if (any(!is.na(extract[[value_field]]) & (!is.finite(rate) | rate < 0 | rate > 100))) {
    stop("A reported cohort survival rate is invalid. Check the original extract.")
  }
  lag_rate <- rate[match(paste(region, sex, year - 1L, sep = "|"), key)]
  lag_two <- rate[match(paste(region, sex, year - 2L, sep = "|"), key)]
  change <- rate - lag_rate
  prior_change <- lag_rate - lag_two
  usable <- region != "PHILIPPINES" & !region %in% excluded_regions &
    sex %in% c("Female", "Male") & year >= first_model_year & complete.cases(rate, lag_rate, prior_change)
  clean <- data.frame(
    year = year, region = region, sex = sex, survival_pct = rate,
    previous_survival_pct = lag_rate, previous_change_pp = round(prior_change, 4),
    change_pp = round(change, 4),
    retention_drop = ifelse(is.na(rate) | is.na(lag_rate), NA_integer_,
                            as.integer(round(change, 4) <= decline_cutoff)),
    analysis_group = ifelse(usable, ifelse(year < first_test_year, "Train", "Test"), "Descriptive")
  )
  national <- clean[clean$region == "PHILIPPINES" & clean$sex == "Both Sexes" &
                      !is.na(clean$survival_pct), c("year", "survival_pct")]
  national <- national[order(national$year), ]
  if (nrow(national) < 12L || any(diff(national$year) != 1L)) stop("The national annual series is incomplete.")
  regional <- clean[clean$analysis_group != "Descriptive", ]
  limits <- quantile(regional$change_pp, c(.25, .75), na.rm = TRUE)
  spread <- diff(limits)
  audit <- data.frame(
    Check = c("Rows in source extract", "Exact duplicate rows removed", "Reported rates",
              "Unreported rates carried as missing", "Reported rates at 100% retained",
              "Modeled changes outside 1.5 IQR fences (retained)",
              "Training rows", "Later test rows", "National observations"),
    Rows = c(nrow(raw), sum(duplicates), sum(!is.na(rate)), sum(is.na(rate)),
             sum(rate == 100, na.rm = TRUE),
             sum(regional$change_pp < limits[1] - 1.5 * spread |
                   regional$change_pp > limits[2] + 1.5 * spread),
             sum(clean$analysis_group == "Train"), sum(clean$analysis_group == "Test"), nrow(national))
  )
  list(clean = clean, regional = regional, national = national, audit = audit,
       fields = data.frame(Field = fields, Imported_as = "character",
                           Missing = vapply(fields, function(field) sum(is.na(raw[[field]])),
                                            integer(1), USE.NAMES = FALSE)))
}

paired_comparison <- function(regional) {
  pairs <- reshape(regional[c("year", "region", "sex", "survival_pct")],
                   idvar = c("year", "region"), timevar = "sex", direction = "wide")
  pairs$gap_pp <- pairs$survival_pct.Female - pairs$survival_pct.Male
  annual <- aggregate(gap_pp ~ year, pairs, mean)
  annual <- annual[order(annual$year), ]
  list(annual = annual, test = t.test(annual$gap_pp),
       lag_one = as.numeric(acf(annual$gap_pp, lag.max = 1, plot = FALSE)$acf[2]))
}

classification <- function(actual, flag) {
  cm <- table(Observed = factor(actual, levels = 0:1), Predicted = factor(flag, levels = 0:1))
  tn <- cm[1, 1]
  fp <- cm[1, 2]
  fn <- cm[2, 1]
  tp <- cm[2, 2]
  divide <- function(a, b) if (b == 0) NA_real_ else as.numeric(a / b)
  list(matrix = cm, metrics = c(Accuracy = divide(tn + tp, sum(cm)), Precision = divide(tp, tp + fp),
                                Recall = divide(tp, tp + fn), F1 = divide(2 * tp, 2 * tp + fp + fn)))
}

fit_models <- function(regional) {
  train <- subset(regional, analysis_group == "Train")
  test <- subset(regional, analysis_group == "Test")
  train$region <- factor(train$region)
  train$sex <- factor(train$sex)
  test$region <- factor(test$region, levels = levels(train$region))
  test$sex <- factor(test$sex, levels = levels(train$sex))
  if (nrow(train) == 0L || nrow(test) == 0L || anyNA(test$region) ||
      length(unique(train$retention_drop)) != 2L || length(unique(test$retention_drop)) != 2L) {
    stop("The split lacks training/test coverage or has an unseen region.")
  }
  center <- mean(train$previous_survival_pct)
  train$prior10 <- (train$previous_survival_pct - center) / 10
  test$prior10 <- (test$previous_survival_pct - center) / 10
  simple <- lm(survival_pct ~ prior10, train)
  multiple <- lm(survival_pct ~ prior10 + previous_change_pp + region + sex, train)
  interaction <- update(multiple, . ~ . + prior10:sex)
  summary_row <- function(fit, label) {
    stats <- summary(fit)
    err <- test$survival_pct - predict(fit, test)
    data.frame(Model = label, R2 = stats$r.squared, Adjusted_R2 = stats$adj.r.squared,
               Test_MAE_pp = mean(abs(err)), Test_RMSE_pp = sqrt(mean(err^2)))
  }
  design <- model.matrix(multiple)[, -1, drop = FALSE]
  vif <- diag(solve(cor(design)))
  names(vif) <- colnames(design)
  logistic <- glm(retention_drop ~ prior10 + previous_change_pp + region + sex,
                  data = train, family = binomial())
  probability <- predict(logistic, test, type = "response")
  actual <- test$retention_drop
  most_common <- as.integer(mean(train$retention_drop) >= .50)
  coefs <- coef(logistic)
  margin <- qnorm(.975) * sqrt(diag(vcov(logistic)))
  list(train = train, test = test, center = center, simple = simple, multiple = multiple,
       compare = rbind(summary_row(simple, "Simple"), summary_row(multiple, "Multiple")),
       ancova = anova(multiple, interaction), interaction = interaction, vif = vif,
       logistic = logistic, actual = actual, probability = probability,
       logistic_result = classification(actual, probability >= score_cutoff),
       majority_result = classification(actual, rep(most_common, length(actual))),
       odds = data.frame(Term = names(coefs), OR = unname(exp(coefs)), Lower = unname(exp(coefs - margin)),
                         Upper = unname(exp(coefs + margin))))
}

forecast_national <- function(national) {
  y <- national$survival_pct
  holdout <- 4L
  fit <- head(y, -holdout)
  actual <- tail(y, holdout)
  naive <- rep(tail(fit, 1), holdout)
  recent <- rep(mean(tail(fit, 3)), holdout)
  errors <- function(p) c(MAE = mean(abs(actual - p)), RMSE = sqrt(mean((actual - p)^2)))
  horizon <- 1:3
  baseline <- tail(y, 1)
  bounds <- vapply(horizon, function(h) quantile(diff(y, lag = h), c(.1,.9), names = FALSE), numeric(2))
  future <- data.frame(Year = max(national$year) + horizon, Forecast = rep(baseline, length(horizon)),
                       Lower = pmax(0, baseline + bounds[1, ]), Upper = pmin(100, baseline + bounds[2, ]))
  train <- head(national, -holdout)
  train$trend <- as.numeric(stats::filter(fit, rep(1/3, 3), sides = 2))
  train$remainder <- train$survival_pct - train$trend
  list(errors = rbind("Last reported year" = errors(naive), "Mean of last 3 years" = errors(recent)),
       holdout = data.frame(Year = tail(national$year, holdout), Observed = actual, Naive = naive,
                            Three_year_mean = recent), future = future, trend = train)
}

plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom",
          plot.title.position = "plot", plot.title = element_text(face = "bold"))
}

plot_distribution <- function(regional) {
  ggplot(regional, aes(survival_pct)) +
    geom_histogram(binwidth = 5, boundary = 100, fill = "#1F536F", color = "white") +
    labs(title = "Distribution of reported cohort survival", x = "Cohort survival (%)", y = "Region-sex-year records") +
    plot_theme()
}

plot_groups <- function(regional) {
  y <- max(regional$year)
  ggplot(subset(regional, year == y), aes(survival_pct, reorder(region, survival_pct), color = sex)) +
    geom_point(size = 2.7, position = position_dodge(width = .48)) +
    scale_color_manual(values = c(Female = "#1F536F", Male = "#AB512F")) +
    labs(title = paste("Grade 7-10 cohort survival by region in", y), x = "Cohort survival (%)", y = NULL, color = NULL) +
    plot_theme()
}

plot_relationship <- function(regional) {
  ggplot(regional, aes(previous_survival_pct, survival_pct)) +
    geom_point(alpha = .38, color = "#1F536F", size = 1.7) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "#AB512F") +
    labs(title = "Current and prior-year cohort survival", x = "Previous year (%)", y = "Current year (%)") +
    plot_theme()
}

plot_series <- function(national) {
  ggplot(national, aes(year, survival_pct)) + geom_line(color = "#1F536F", linewidth = .8) +
    geom_point(color = "#1F536F", size = 1.8) +
    labs(title = "National Grade 7-10 cohort survival", x = "PSA reporting year", y = "Cohort survival (%)") +
    plot_theme()
}

plot_forecast <- function(national, forecast) {
  ggplot() +
    geom_line(data = tail(national, 12), aes(year, survival_pct, color = "Observed"), linewidth = .8) +
    geom_ribbon(data = forecast$future, aes(Year, ymin = Lower, ymax = Upper), fill = "#E4B69F", alpha = .65) +
    geom_line(data = forecast$future, aes(Year, Forecast, color = "Naive baseline"), linewidth = .9) +
    geom_point(data = forecast$future, aes(Year, Forecast, color = "Naive baseline"), size = 2) +
    scale_color_manual(values = c(Observed = "#1F536F", "Naive baseline" = "#AB512F")) +
    labs(title = "National cohort survival baseline forecast", x = "PSA reporting year", y = "Cohort survival (%)",
         color = NULL, subtitle = "Band based on past forecast errors at each horizon.") + plot_theme()
}

run_analysis <- function(path = raw_path) {
  data <- prepare_data(path)
  list(data = data, inference = paired_comparison(data$regional),
       models = fit_models(data$regional), forecast = forecast_national(data$national))
}

if (sys.nframe() == 0L) {
  p <- run_analysis()
  write.csv(p$data$clean, "05_Cleaned_Dataset.csv", row.names = FALSE, na = "")
  dir.create("outputs", showWarnings = FALSE)
  write.csv(p$data$audit, "outputs/data_quality_summary.csv", row.names = FALSE)
  write.csv(p$data$fields, "outputs/raw_field_audit.csv", row.names = FALSE)
  print(p$data$audit, row.names = FALSE)
  print(p$inference$test)
  print(p$models$compare)
  print(p$models$logistic_result)
  print(p$forecast$errors)
}
