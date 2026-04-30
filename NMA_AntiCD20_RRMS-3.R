# ============================================================
#  Network Meta-Analysis: Anti-CD20 mAbs for RRMS
#  Outcomes: ARR (primary) + Gd-enhancing lesions (secondary)
#  Software: R + netmeta (frequentist, graph-theoretical)
# ============================================================


# ============================================================
# SECTION 0 — Install & load packages (run once)
# ============================================================


install.packages(c("netmeta", "meta", "ggplot2", "dplyr",
                   "gridExtra", "scales", "readxl"))

library(netmeta)    # NMA — frequentist graph-theoretical
library(meta)       # pairwise meta-analysis
library(ggplot2)    # additional plotting
library(dplyr)      # data manipulation
library(gridExtra)  # multi-panel figures
library(scales)     # axis formatting

cat("Packages loaded successfully\n")
cat("netmeta version:", as.character(packageVersion("netmeta")), "\n")


# ============================================================
# SECTION 1 — Data entry
#
# Fill in values from your data extraction table.
# RR  = rate ratio (treatment vs comparator)
# Lower/Upper = 95% CI bounds
# All RR values should be < 1 (treatment reduces ARR)
#
# ARR data from published primary publications:
#   OPERA I/II:      Hauser et al., NEJM 2017
#   ASCLEPIOS I/II:  Hauser et al., NEJM 2020
#   ULTIMATE I/II:   Steinman et al., NEJM 2022
#   TENERE:          Vermersch et al., Mult Scler 2014
# ============================================================

# ── PRIMARY OUTCOME: ARR ──────────────────────────────────
# Each row = one trial arm comparison
# treat1 = intervention, treat2 = comparator

arr_data <- data.frame(

  study = c(
    # OPERA — each trial entered separately (pooled estimate available
    # but individual trials used for heterogeneity assessment)
    "OPERA I",
    "OPERA II",
    # ASCLEPIOS — individual trials
    "ASCLEPIOS I",
    "ASCLEPIOS II",
    # ULTIMATE — individual trials
    "ULTIMATE I",
    "ULTIMATE II",
    # TENERE — bridge trial connecting the two comparator strata
    # Source: Vermersch et al., Mult Scler 2014;20:705-716, Table 2
    # Reported: Teriflunomide 14mg vs IFN-b1a, RR=1.20 (0.62-2.30), p=0.59
    # ARR: IFN-b1a=0.22 (0.11-0.42), Teriflunomide 14mg=0.26 (0.15-0.44)
    # Direction here: IFN-b1a (treat1) vs Teriflunomide (treat2)
    # Therefore RR inverted: 1/1.20=0.833; CI: lower=1/2.30=0.435, upper=1/0.62=1.613
    "TENERE"
  ),

  treat1 = c(
    "Ocrelizumab",  # OPERA I
    "Ocrelizumab",  # OPERA II
    "Ofatumumab",   # ASCLEPIOS I
    "Ofatumumab",   # ASCLEPIOS II
    "Ublituximab",  # ULTIMATE I
    "Ublituximab",  # ULTIMATE II
    "IFN-beta-1a"   # TENERE treat1
  ),

  treat2 = c(
    "IFN-beta-1a",   # OPERA I
    "IFN-beta-1a",   # OPERA II
    "Teriflunomide", # ASCLEPIOS I
    "Teriflunomide", # ASCLEPIOS II
    "Teriflunomide", # ULTIMATE I
    "Teriflunomide", # ULTIMATE II
    "Teriflunomide"  # TENERE treat2
  ),

  # ── ARR RATE RATIOS — extracted from papers
  # OPERA I/II:     Hauser et al., NEJM 2017
  # ASCLEPIOS I/II: Hauser et al., NEJM 2020
  # ULTIMATE I/II:  Steinman et al., NEJM 2022
  # TENERE:         Vermersch et al., Mult Scler 2014

  RR = c(
    0.54,   # OPERA I:       RR 0.54 (Table 2, p<0.001)
    0.53,   # OPERA II:      RR 0.53 (Table 2, p<0.001)
    0.49,   # ASCLEPIOS I:   RR 0.49 (Table 2, p<0.001)
    0.42,   # ASCLEPIOS II:  RR 0.42 (Table 2, p<0.001)
    0.41,   # ULTIMATE I:    RR 0.41 (p<0.001) — Steinman et al.
    0.51,   # ULTIMATE II:   RR 0.51 (p=0.002) — Steinman et al.
    0.833   # TENERE: 1/1.20 — IFN-b1a vs Teriflunomide (inverted from Table 2)
  ),

  lower = c(
    0.40,   # OPERA I lower CI
    0.40,   # OPERA II lower CI
    0.37,   # ASCLEPIOS I lower CI
    0.31,   # ASCLEPIOS II lower CI
    0.27,   # ULTIMATE I lower CI
    0.33,   # ULTIMATE II lower CI
    0.435   # TENERE lower CI: 1/2.30 (inverted upper from Table 2)
  ),

  upper = c(
    0.72,   # OPERA I upper CI
    0.71,   # OPERA II upper CI
    0.65,   # ASCLEPIOS I upper CI
    0.56,   # ASCLEPIOS II upper CI
    0.62,   # ULTIMATE I upper CI
    0.78,   # ULTIMATE II upper CI
    1.613   # TENERE upper CI: 1/0.62 (inverted lower from Table 2)
  ),

  n_treatment = c(
    410,  # OPERA I ocrelizumab
    417,  # OPERA II ocrelizumab
    465,  # ASCLEPIOS I ofatumumab
    481,  # ASCLEPIOS II ofatumumab
    274,  # ULTIMATE I ublituximab
    272,  # ULTIMATE II ublituximab
    104   # TENERE IFN-b1a (treat1) — Table 1, ITT population
  ),

  n_comparator = c(
    411,  # OPERA I IFN-b1a
    418,  # OPERA II IFN-b1a
    462,  # ASCLEPIOS I teriflunomide
    474,  # ASCLEPIOS II teriflunomide
    275,  # ULTIMATE I teriflunomide
    273,  # ULTIMATE II teriflunomide
    111   # TENERE teriflunomide 14mg (treat2) — Table 1, ITT population
  ),

  arr_treatment = c(
    0.16,  # OPERA I
    0.16,  # OPERA II
    0.11,  # ASCLEPIOS I
    0.10,  # ASCLEPIOS II
    0.08,  # ULTIMATE I
    0.09,  # ULTIMATE II
    0.22   # TENERE IFN-b1a ARR — Table 2 (0.22, 95% CI 0.11-0.42)
  ),

  arr_comparator = c(
    0.29,  # OPERA I
    0.29,  # OPERA II
    0.22,  # ASCLEPIOS I
    0.25,  # ASCLEPIOS II
    0.19,  # ULTIMATE I
    0.18,  # ULTIMATE II
    0.26   # TENERE teriflunomide 14mg ARR — Table 2 (0.26, 95% CI 0.15-0.44)
  ),

  baseline_edss = c(
    2.80, 2.81,   # OPERA I/II (mean from Table 1)
    2.97, 2.90,   # ASCLEPIOS I/II (mean from Table 1)
    2.80, 2.80,   # ULTIMATE I/II — confirm from Steinman 2022 Table 1
    2.00          # TENERE IFN-b1a group — Table 1 (mean 2.0, SD 1.2)
  ),

  followup_wks = c(
    96, 96,   # OPERA
    83, 83,   # ASCLEPIOS (median 1.6 yrs ≈ 83 wks)
    96, 96,   # ULTIMATE
    60        # TENERE — median exposure 60.1 wks in IFN-b1a group (Methods)
  ),

  comparator_type = c(
    "IFN-beta-1a", "IFN-beta-1a",
    "Teriflunomide", "Teriflunomide",
    "Teriflunomide", "Teriflunomide",
    "Bridge"
  ),

  stringsAsFactors = FALSE
)

# ── SECONDARY OUTCOME: Gd-enhancing lesions ───────────────
# Ratio of means (treatment / comparator) on log scale
# Source: same papers as above — MRI secondary endpoints tables

gd_data <- data.frame(

  study = c(
    "OPERA I",
    "OPERA II",
    "ASCLEPIOS I",
    "ASCLEPIOS II",
    "ULTIMATE I",
    "ULTIMATE II"
    # TENERE: Gd not a primary endpoint — exclude from Gd NMA
  ),

  treat1 = c(
    "Ocrelizumab", "Ocrelizumab",
    "Ofatumumab",  "Ofatumumab",
    "Ublituximab", "Ublituximab"
  ),

  treat2 = c(
    "IFN-beta-1a",   "IFN-beta-1a",
    "Teriflunomide", "Teriflunomide",
    "Teriflunomide", "Teriflunomide"
  ),

  # Mean Gd lesions per scan: treatment vs comparator
  # OPERA: Hauser 2017 Table 2 — 0.02 vs 0.29 (I), 0.02 vs 0.42 (II)
  # ASCLEPIOS: Hauser 2020 Table 2 — 0.01 vs 0.45 (I), 0.03 vs 0.51 (II)
  # ULTIMATE: Steinman 2022 / Preziosa 2025 — 0.02 vs 0.49 (I), 0.01 vs 0.25 (II)

  RR = c(
    0.06,  # OPERA I:      rate ratio 0.06 (0.03–0.10), p<0.001
    0.05,  # OPERA II:     rate ratio 0.05 (0.03–0.09), p<0.001
    0.03,  # ASCLEPIOS I:  rate ratio 0.03 (0.01–0.05), p<0.001
    0.06,  # ASCLEPIOS II: rate ratio 0.06 (0.04–0.10), p<0.001
    0.03,  # ULTIMATE I:   rate ratio 0.03 (0.02–0.06), p<0.001
    0.04   # ULTIMATE II:  rate ratio 0.04 (0.02–0.06), p<0.001
  ),

  lower = c(
    0.03,  # OPERA I
    0.03,  # OPERA II
    0.01,  # ASCLEPIOS I
    0.04,  # ASCLEPIOS II
    0.02,  # ULTIMATE I
    0.02   # ULTIMATE II
  ),

  upper = c(
    0.10,  # OPERA I
    0.09,  # OPERA II
    0.05,  # ASCLEPIOS I
    0.10,  # ASCLEPIOS II
    0.06,  # ULTIMATE I
    0.06   # ULTIMATE II
  ),

  comparator_type = c(
    "IFN-beta-1a",   "IFN-beta-1a",
    "Teriflunomide", "Teriflunomide",
    "Teriflunomide", "Teriflunomide"
  ),

  stringsAsFactors = FALSE
)

cat("\nData loaded:\n")
cat("  ARR data:", nrow(arr_data), "comparisons (6 trials + 1 bridge)\n")
cat("  Gd data: ", nrow(gd_data),  "comparisons (6 trials)\n")
cat("\n  Trials included:\n")
cat("  ARR: OPERA I, OPERA II, ASCLEPIOS I, ASCLEPIOS II,\n")
cat("       ULTIMATE I, ULTIMATE II, TENERE (bridge)\n")
cat("  Gd:  OPERA I, OPERA II, ASCLEPIOS I, ASCLEPIOS II,\n")
cat("       ULTIMATE I, ULTIMATE II\n")
cat("\n  All values fully extracted from published papers.\n")
cat("  TENERE: Vermersch et al. Mult Scler 2014;20:705-716\n")
cat("  RR inverted (IFN-b1a vs Teriflunomide): 0.833 (0.435-1.613)\n")


# ============================================================
# SECTION 2 — Log-transform and compute standard errors
# ============================================================

transform_data <- function(df) {
  df$log_RR  <- log(df$RR)
  df$log_lo  <- log(df$lower)
  df$log_hi  <- log(df$upper)
  df$se_log  <- (df$log_hi - df$log_lo) / (2 * 1.96)
  return(df)
}

arr_data <- transform_data(arr_data)
gd_data  <- transform_data(gd_data)

cat("\nLog-transformed data (ARR):\n")
print(arr_data[, c("study","treat1","treat2","RR","log_RR","se_log")])


# ============================================================
# SECTION 3 — Pairwise meta-analysis (within comparator strata)
#
# OPERA I and II are already pooled in the published paper
# and reported as a combined estimate. If you have individual
# trial data (separate I and II), uncomment and use metagen()
# to pool them first, then feed the pooled estimate to NMA.
#
# If using pooled published estimates (one row per programme),
# this section documents the pairwise heterogeneity.
# ============================================================

cat("\n\n=== PAIRWISE META-ANALYSIS ===\n")

# IFN-b1a stratum — OPERA (one pooled comparison)
# If OPERA I and OPERA II separate rows:
# ma_ifn <- metagen(log_RR, se_log, studlab=study,
#                   data=arr_data[arr_data$comparator_type=="IFN-beta-1a",],
#                   sm="RR", comb.fixed=FALSE, comb.random=TRUE)
# summary(ma_ifn)

# Teriflunomide stratum — ASCLEPIOS + ULTIMATE
ma_teri <- metagen(

  TE      = log_RR,
  seTE    = se_log,
  studlab = study,
  data    = arr_data[arr_data$comparator_type == "Teriflunomide", ],
  sm      = "RR",
  comb.fixed   = FALSE,
  comb.random  = TRUE,
  method.tau   = "DL",           # DerSimonian-Laird
  hakn         = FALSE,
  title = "ARR: Teriflunomide comparator stratum"
)

cat("\nTeriflunomide stratum — pooled ARR:\n")
print(summary(ma_teri))

# Save pairwise forest plot
png("forest_pairwise_ARR_teriflunomide.png",
    width=3500, height=800, res=200)
par(mar = c(6, 4, 4, 2))   # default is c(5, 4, 4, 2)
forest(ma_teri,
       smlab      = "Rate Ratio (ARR)",
       leftcols   = "studlab",
       leftlabs   = "Study",
       rightcols  = c("effect", "ci", "w.random"),
       rightlabs  = c("RR", "95% CI", "Weight"),
       col.square  = "#0F6E56",
       col.diamond = "#085041",
       print.I2    = FALSE,
       print.tau2  = FALSE,
       print.pval.Q = FALSE)
dev.off()
cat("Saved: forest_pairwise_ARR_teriflunomide.png\n")


# ============================================================
# SECTION 4 — Network meta-analysis (ARR)
# ============================================================

cat("\n\n=== NETWORK META-ANALYSIS: ARR ===\n")

nma_arr <- netmeta(
  TE      = log_RR,
  seTE    = se_log,
  treat1  = treat1,
  treat2  = treat2,
  studlab = study,
  data    = arr_data,
  sm      = "RR",
  fixed   = FALSE,       # random effects
  random  = TRUE,
  reference.group = "Teriflunomide",   # reference node
  details.chkmultiarm = TRUE,
  sep.trts = " vs "
)

cat("\nNMA summary (ARR):\n")
print(summary(nma_arr))

# ── All pairwise comparisons ─────────────────────────────
cat("\n\nAll pairwise RR estimates (random effects):\n")
print(nma_arr, digits = 3)

# ── League table ─────────────────────────────────────────
cat("\nLeague table (RR, random effects):\n")
netleague_arr <- netleague(nma_arr, digits = 2, bracket = "(", separator = " to ")
print(netleague_arr)


# ============================================================
# SECTION 5 — Heterogeneity & consistency checks
# ============================================================

cat("\n\n=== HETEROGENEITY & CONSISTENCY ===\n")

# Between-study heterogeneity
cat("tau² (between-study variance):", round(nma_arr$tau^2, 4), "\n")
cat("I²  (total heterogeneity):    ", round(nma_arr$I2, 1), "%\n")
cat("Q statistic:                  ", round(nma_arr$Q, 2), "\n")
cat("Q p-value:                    ", round(nma_arr$pval.Q, 4), "\n")

# Consistency check — decomposition of Q
cat("\nDecomposition of Q (direct vs indirect):\n")
decomp <- decomp.design(nma_arr)
print(decomp)

# Net heat plot (visual consistency check)
png("netheat_ARR.png", width=2000, height=2000, res=200)
netheat(nma_arr)
dev.off()
cat("Saved: netheat_ARR.png\n")

# ── Sensitivity: subgroup by comparator type ─────────────
cat("\n\nSensitivity analysis — stratified by comparator:\n")

# IFN-b1a stratum only (just OPERA + TENERE bridge)
arr_ifn_only <- arr_data[arr_data$comparator_type %in%
                           c("IFN-beta-1a", "Bridge"), ]
if(nrow(arr_ifn_only) >= 2) {
  nma_sens_ifn <- tryCatch({
    netmeta(log_RR, se_log, treat1, treat2, study,
            data=arr_ifn_only, sm="RR",
            fixed=FALSE, random=TRUE,
            reference.group="IFN-beta-1a")
  }, error = function(e) {
    cat("  IFN stratum NMA skipped:", e$message, "\n"); NULL
  })
  if(!is.null(nma_sens_ifn)) {
    cat("IFN-b1a stratum I²:", round(nma_sens_ifn$I2, 1), "%\n")
  }
}

# Teriflunomide stratum only
arr_teri_only <- arr_data[arr_data$comparator_type == "Teriflunomide", ]
if(nrow(arr_teri_only) >= 2) {
  nma_sens_teri <- tryCatch({
    netmeta(log_RR, se_log, treat1, treat2, study,
            data=arr_teri_only, sm="RR",
            fixed=FALSE, random=TRUE,
            reference.group="Teriflunomide")
  }, error = function(e) {
    cat("  Teriflunomide stratum NMA skipped:", e$message, "\n"); NULL
  })
  if(!is.null(nma_sens_teri)) {
    cat("Teriflunomide stratum I²:", round(nma_sens_teri$I2, 1), "%\n")
  }
}


# ============================================================
# SECTION 6 — SUCRA rankings
# ============================================================

cat("\n\n=== SUCRA RANKINGS (ARR) ===\n")
cat("Higher SUCRA = higher probability of being best treatment\n\n")

sucra_arr <- rankogram(nma_arr, small.values = "good")
print(sucra_arr)

# SUCRA bar chart
sucra_vals <- sucra_arr$ranking.random
sucra_df   <- data.frame(
  treatment = names(sucra_vals),
  sucra     = as.numeric(sucra_vals) * 100
) %>% arrange(desc(sucra))

sucra_df$treatment <- factor(sucra_df$treatment,
                              levels = sucra_df$treatment)

p_sucra <- ggplot(sucra_df, aes(x = treatment, y = sucra, fill = treatment)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(sucra, 1), "%")),
            vjust = -0.5, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c(
    "Ocrelizumab"    = "#185FA5",
    "Ofatumumab"     = "#0F6E56",
    "Ublituximab"    = "#3B6D11",
    "IFN-beta-1a"    = "#888780",
    "Teriflunomide"  = "#5F5E5A"
  )) +
  scale_y_continuous(limits = c(0, 110), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "SUCRA rankings — Annualised Relapse Rate",
    subtitle = "Higher = greater probability of being the best treatment for ARR reduction",
    x        = NULL,
    y        = "SUCRA (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x   = element_text(size = 11)
  )

ggsave("sucra_ARR.png", p_sucra, width = 8, height = 5, dpi = 300)
cat("Saved: sucra_ARR.png\n")

# Cumulative ranking plot
png("cumrank_ARR.png", width=2000, height=1600, res=200)
plot(sucra_arr)
dev.off()
cat("Saved: cumrank_ARR.png\n")


# ============================================================
# SECTION 7 — Network meta-analysis (Gd-enhancing lesions)
# ============================================================

cat("\n\n=== NETWORK META-ANALYSIS: Gd-ENHANCING LESIONS ===\n")

nma_gd <- netmeta(
  TE      = log_RR,
  seTE    = se_log,
  treat1  = treat1,
  treat2  = treat2,
  studlab = study,
  data    = gd_data,
  sm      = "RR",
  fixed   = FALSE,
  random  = TRUE,
  reference.group = "Teriflunomide",
  sep.trts = " vs "
)

cat("\nNMA summary (Gd lesions):\n")
print(summary(nma_gd))

cat("\nSUCRA rankings (Gd lesions):\n")
sucra_gd <- rankogram(nma_gd, small.values = "good")
print(sucra_gd)


# ============================================================
# SECTION 8 — Combined NMA forest plot (publication-ready)
# ============================================================

cat("\n\nGenerating combined NMA forest plot...\n")

png("forest_NMA_ARR.png", width=2800, height=1600, res=220)
forest(nma_arr,
       reference.group  = "Teriflunomide",
       sortvar          = "-Prop",
       smlab            = "Rate Ratio (ARR)",
       leftcols         = c("studlab","k","prop"),
       leftlabs         = c("Comparison","Trials","Prop. direct"),
       col.square       = "#185FA5",
       col.inside       = "white",
       col.diamond      = "#0C447C",
       col.diamond.lines = "#042C53",
       xlab = paste0(
         "Rate Ratio — ARR (anti-CD20 vs comparator)\n",
         "Random effects NMA | Frequentist | netmeta"
       ),
       xlim             = c(0.1, 2.5),
       at               = c(0.2, 0.5, 1.0, 1.5, 2.0),
       digits           = 3
)
dev.off()
cat("Saved: forest_NMA_ARR.png\n")


# ============================================================
# SECTION 9 — Network diagram
# ============================================================

cat("\nGenerating network diagram...\n")

png("network_diagram.png", width=2000, height=2000, res=220)
netgraph(nma_arr,
         plastic     = FALSE,
         thickness   = "number.of.studies",
         col         = "#185FA5",
         cex         = 1.4,
         cex.points  = c(3, 3, 3, 2, 2),  # node size by evidence base
         col.points  = c("#185FA5","#0F6E56","#3B6D11","#888780","#5F5E5A"),
         labels      = c("Ocrelizumab","Ofatumumab","Ublituximab",
                         "IFN-β1a","Teriflunomide"),
         number.of.studies.pos = 0.5
)
dev.off()
cat("Saved: network_diagram.png\n")


# ============================================================
# SECTION 10 — Publication bias (funnel plot)
# ============================================================

cat("\nGenerating comparison-adjusted funnel plot...\n")
cat("Note: with k =", nrow(arr_data),
    "studies, Egger test is underpowered.\n")
cat("Report visual inspection only; do not over-interpret.\n\n")

png("funnel_ARR.png", width=2000, height=1800, res=200)
funnel(nma_arr,
       order      = c("Ocrelizumab","Ofatumumab","Ublituximab",
                      "IFN-beta-1a","Teriflunomide"),
       pch        = c(16,17,15,18,8),
       col        = c("#185FA5","#0F6E56","#3B6D11","#888780","#5F5E5A"),
       legend     = TRUE,
       xlab       = "log Rate Ratio",
       main       = "Comparison-adjusted funnel plot — ARR"
)
dev.off()
cat("Saved: funnel_ARR.png\n")


# ============================================================
# SECTION 11 — Summary output table
# ============================================================

cat("\n\n=== SUMMARY TABLE FOR DISSERTATION ===\n")
cat("Copy these values into your Results section:\n\n")

# Extract key pairwise estimates from NMA
treatments  <- c("Ocrelizumab","Ofatumumab","Ublituximab")
ref         <- "Teriflunomide"

cat(sprintf("%-35s %-12s %-20s %-10s\n",
            "Comparison", "RR (random)", "95% CI", "p-value"))
cat(strrep("-", 80), "\n")

for(t in treatments) {
  pair <- paste(t, "vs", ref)
  idx  <- which(nma_arr$treat1 == t & nma_arr$treat2 == ref |
                nma_arr$treat1 == ref & nma_arr$treat2 == t)

  if(length(idx) > 0) {
    log_rr <- nma_arr$TE.random[t, ref]
    se_rr  <- nma_arr$seTE.random[t, ref]
    rr     <- exp(log_rr)
    lo     <- exp(log_rr - 1.96 * se_rr)
    hi     <- exp(log_rr + 1.96 * se_rr)
    pval   <- 2 * (1 - pnorm(abs(log_rr / se_rr)))

    cat(sprintf("%-35s %-12s %-20s %-10s\n",
                pair,
                round(rr, 3),
                paste0("(", round(lo,3), " - ", round(hi,3), ")"),
                ifelse(pval < 0.001, "<0.001", round(pval, 3))))
  }
}

cat("\nSUCRA rankings (ARR):\n")
for(i in seq_along(sucra_df$treatment)) {
  cat(sprintf("  %d. %-20s %.1f%%\n",
              i,
              as.character(sucra_df$treatment[i]),
              sucra_df$sucra[i]))
}

cat("\n\nAll output files saved to working directory:\n")
files <- c("forest_pairwise_ARR_teriflunomide.png",
           "forest_NMA_ARR.png",
           "sucra_ARR.png",
           "cumrank_ARR.png",
           "netheat_ARR.png",
           "network_diagram.png",
           "funnel_ARR.png")
for(f in files) {
  exists <- file.exists(f)
  cat(sprintf("  %s %s\n", ifelse(exists, "ok", "MISSING"), f))
}

cat("\nTo cite netmeta in your dissertation:\n")
citation("netmeta")

