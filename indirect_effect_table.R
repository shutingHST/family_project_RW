library(lavaan)
library(dplyr)
library(tidyr)
library(stringr)

# Extract parameter estimates with bootstrap CI
pe <- parameterEstimates(
  fit5_1,
  standardized = TRUE,
  ci = TRUE,
  boot.ci.type = "perc"
)

# Keep only defined (indirect) effects
indirect <- pe %>%
  filter(op == ":=") %>%
  dplyr::select(label, est, ci.lower, ci.upper)


indirect <- indirect %>%
  mutate(
    # Remove "ind_" prefix
    label = str_remove(label, "^ind_")
  ) %>%
  separate(label, into = c( "Mediator","Predictor"), sep = "_")

indirect <- indirect %>%
  mutate(
    Estimate = sprintf("%.3f", est),
    CI = sprintf("[%.3f, %.3f]", ci.lower, ci.upper),
    Cell = paste0(Estimate, "\n", CI)
  )

table_wide <- indirect %>%
  dplyr::select(Predictor, Mediator, Cell) %>%
  pivot_wider(names_from = Mediator, values_from = Cell)


library(officer)
library(flextable)

ft <- flextable(table_wide)

ft <- ft %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  valign(valign = "center", part = "all") %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table X", style = "heading 1") %>%
  body_add_par("Indirect Effects with 95% Bootstrap Confidence Intervals", style = "Normal") %>%
  body_add_flextable(ft)

print(doc, target = "Indirect_Effects_Matrix2.docx")