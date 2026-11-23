library(grid)
source("04_Analysis_Pipeline.R", local = TRUE)

p <- run_analysis()
d <- p$data
i <- p$inference
m <- p$models
f <- p$forecast
cm <- m$logistic_result$matrix
score <- m$logistic_result$metrics
num <- function(x) sprintf("%.2f", x)
pct <- function(x) sprintf("%.1f%%", 100 * x)

label <- function(text, x, y, size = 21, color = "#253642", bold = FALSE) {
  grid.text(text, x, y, just = c("left", "top"),
            gp = gpar(fontsize = size, col = color, lineheight = 1.14,
                      fontface = if (bold) "bold" else "plain"))
}

slide <- function(title, page) {
  grid.newpage()
  label(title, .055, .91, 31, "#173E5B", TRUE)
  label("DepEd data via PSA OpenSTAT, SDG indicator 4.1.s1", .055, .06, 12, "#526573")
  label(sprintf("PC25 / %02d", page), .865, .06, 12, "#526573")
}

chart <- function(p) {
  p <- p + theme(text = element_text(size = 15), plot.title = element_text(size = 17),
                 plot.subtitle = element_text(size = 12), axis.text = element_text(size = 13))
  print(p, vp = viewport(x = .338, y = .47, width = .566, height = .66), newpage = FALSE)
}

render_slides <- function() {
  cairo_pdf("07_Presentation.pdf", width = 13.333, height = 7.5, family = "sans")
  on.exit(dev.off(), add = TRUE)
  grid.newpage()
  label("NATIONAL TEACHERS COLLEGE", .055, .9, 17, "#526573", TRUE)
  label("PC25 R Language w/Lab  |  Week 17 Final Terminal Assessment", .055, .842, 16)
  label("Grade 7-10 Cohort Survival", .055, .675, 42, "#173E5B", TRUE)
  label("Where should retention receive closer review?", .057, .55, 27)
  label("Education analytics and predictive dashboard", .057, .446, 16, "#526573")
  label("Vonn Christian Miel P. Meneses  |  BSIT 4.9", .057, .235, 20, "#173E5B", TRUE)
  label("Instructor  Edward James V. Grageda", .057, .16, 15)

  slide("Regional rates describe whole cohorts", 2)
  label(sprintf("%.1f%%", tail(d$national$survival_pct, 1)), .057, .705, 55, "#AB512F", TRUE)
  label(paste("Latest national estimate in", max(d$national$year)), .059, .555, 23)
  label("DepEd estimates the share of a school cohort reaching the final grade.", .059, .43, 21)
  label(sprintf("%s matched modeling regions", length(unique(d$regional$region))), .059, .285, 18, "#173E5B", TRUE)
  label(sprintf("%s unreported rates left blank", sum(is.na(d$clean$survival_pct))), .42, .285, 18, "#173E5B", TRUE)
  label("Published 100% rates kept", .72, .285, 17, "#173E5B", TRUE)

  slide("Female rates averaged higher in matched comparisons", 3)
  chart(plot_groups(d$regional))
  label(paste0(num(i$test$estimate), " points"), .663, .66, 39, "#AB512F", TRUE)
  label("Average annual female–male gap", .663, .55, 18)
  label(sprintf("95%% CI %.2f to %.2f points", i$test$conf.int[1], i$test$conf.int[2]), .663, .428, 18)
  label("Compare the groups within the\nsame region and reporting year.", .663, .303, 18)

  slide("Both retention models have similar test errors", 4)
  chart(plot_relationship(d$regional))
  label(paste0(num(m$compare$Test_MAE_pp[1]), " points"), .663, .66, 39, "#AB512F", TRUE)
  label("Simple-model test MAE", .663, .552, 18)
  label(paste0("Multiple-model MAE ", num(m$compare$Test_MAE_pp[2]), "."), .663, .435, 18)
  label("Historical rates help explain levels.\nLarge shifts are harder to predict.", .663, .306, 18)

  slide("The decline flag catches some drops and misses others", 5)
  label(pct(score["Precision"]), .057, .699, 43, "#173E5B", TRUE)
  label("Precision", .059, .585, 20)
  label(pct(score["Recall"]), .385, .699, 43, "#AB512F", TRUE)
  label("Recall", .387, .585, 20)
  label(pct(score["F1"]), .699, .699, 43, "#173E5B", TRUE)
  label("F1", .701, .585, 20)
  label(sprintf("%s declines flagged  |  %s declines missed", cm[2, 2], cm[2, 1]),
        .059, .407, 26, "#173E5B", TRUE)
  label("Decline means a fall of at least 3 percentage points.", .059, .31, 18)
  label(sprintf("Accuracy %s versus %s for the majority baseline.",
                pct(score["Accuracy"]), pct(m$majority_result$metrics["Accuracy"])), .059, .233, 18)

  slide("The national projection has a wide planning range", 6)
  chart(plot_forecast(d$national, f))
  label(paste0(num(f$errors[1, "MAE"]), " points"), .663, .66, 39, "#AB512F", TRUE)
  label("Last-rate baseline test MAE", .663, .553, 18)
  label(paste0("Three-year mean MAE ", num(f$errors[2, "MAE"]), "."), .663, .433, 18)
  label("One short holdout, with marked\nshifts in the national series.", .663, .301, 18)

  slide("Use the flags to start a regional review", 7)
  label("Check the latest levels and year-to-year changes.", .055, .705, 26, "#173E5B", TRUE)
  label("Review flags alongside unflagged but persistently low rates.", .055, .565, 26, "#173E5B", TRUE)
  label("Ask regional offices about enrollment, transfers, and completion.", .055, .425, 24, "#173E5B", TRUE)
  label("The dashboard shows public regional estimates and supports follow-up planning.", .057, .247, 17)

  slide("References", 8)
  label("PSA OpenSTAT. SDG 4.1.s1 Cohort survival rate.", .055, .711, 22, "#173E5B", TRUE)
  label("DepEd source data | OpenSTAT matrix 3I3C41S1", .055, .621, 18)
  label("Philippine Data Explorer. Pinned OpenSTAT mirror.", .055, .487, 22, "#173E5B", TRUE)
  label("Snapshot release 2026-09-22T10-54-38-a9e047f8723d", .055, .398, 17)
  label("National Teachers College. PC25 Week 17 Final Project Guide.", .055, .243, 22, "#173E5B", TRUE)
}

render_slides()
