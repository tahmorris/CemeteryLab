# Install once if needed:
library(pacman)
pacman::p_load("dplyr","ggplot2","gridExtra","grid","ggpubr")

##### IMPORTANT: Make sure you are in the directory you have the examplesheet.csv in! Usually, this will be in your documents. 
##### Click 'File' and then open a new window in the directory where your examplesheet.csv file is. Then run the below code. 

input_file <- "cemeteryData.csv"
output_dir <- "lifespan_tables"
birth_cutoff <- 1930 #Change this if you want but make a note in your methods what your new cutoff is!

#### IMPORTANT: Choose which variable you want to explore! Married, Military, or Born Before or after a certain date (default is set to births before and after 1930)
# Options: "sex", "sex_married", "sex_military", "sex_birth"
tables_to_combine <- c("sex", "sex_married") #Add the tables you want to combine from the options above! Note: Always include 'sex'.

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read and prepare data
dat <- read.csv(input_file, fileEncoding = "Windows-1252") |>
  mutate(
    Sex = tolower(trimws(as.character(Sex))),
    Married = tolower(trimws(as.character(Married))),
    Military = tolower(trimws(as.character(Military))),
    Birth = as.numeric(Birth),
    Death = as.numeric(Death),
    Lifespan = as.numeric(Lifespan),
    Birth_period = case_when(
      is.na(Birth) ~ NA_character_,
      Birth < birth_cutoff ~ paste0("Before ", birth_cutoff),
      Birth >= birth_cutoff ~ paste0(birth_cutoff, " or later")
    )
  )

# Summarize lifespan with sample size, mean, SD, median, and range.
summarize_lifespan <- function(data, grouping_vars) {
  data |>
    filter(!is.na(Lifespan)) |>
    group_by(across(all_of(grouping_vars))) |>
    summarise(
      n = n(),
      Mean = mean(Lifespan),
      SD = sd(Lifespan),
      Median = median(Lifespan),
      Minimum = min(Lifespan),
      Maximum = max(Lifespan),
      .groups = "drop"
    ) |>
    mutate(
      across(c(Mean, SD, Median, Minimum, Maximum), ~ round(.x, 1))
    )
}

# Create the four summary tables for making a table figure
summary_tables <- list(
  sex = summarize_lifespan(dat, "Sex"),
  sex_married = summarize_lifespan(dat, c("Sex", "Married")),
  sex_military = summarize_lifespan(dat, c("Sex", "Military")),
  sex_birth = summarize_lifespan(dat, c("Sex", "Birth_period"))
)

summary_titles <- c(
  sex = "Lifespan by sex",
  sex_married = "Lifespan by sex and marital status",
  sex_military = "Lifespan by sex and military service",
  sex_birth = "Lifespan by sex and birth period"
)

save_table_png <- function(table_data, title, filename) {
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 12,
      padding = unit(c(5, 7), "mm")
    )
  )

  png(
    filename = filename,
    width = 1400,
    height = max(500, 180 + nrow(table_data) * 75),
    res = 150
  )

  grid.newpage()
  grid.arrange(
    textGrob(title, gp = gpar(fontsize = 18, fontface = "bold")),
    table_grob,
    heights = c(0.15, 0.85)
  )
  dev.off()
}

# Save each table separately
for (table_name in names(summary_tables)) {
  save_table_png(
    summary_tables[[table_name]],
    summary_titles[[table_name]],
    file.path(output_dir, paste0(table_name, ".png"))
  )
}
##########################################################
####Optionally combine selected tables into one figure####
##########################################################

if (length(tables_to_combine) > 0) {
  selected_tables <- summary_tables[tables_to_combine]
  selected_titles <- summary_titles[tables_to_combine]

  table_grobs <- Map(
    function(tbl, title) {
      arrangeGrob(
        textGrob(title, gp = gpar(fontsize = 16, fontface = "bold")),
        tableGrob(
          tbl,
          rows = NULL,
          theme = ttheme_minimal(
            base_size = 11,
            padding = unit(c(4, 6), "mm")
          )
        ),
        heights = c(0.18, 0.82)
      )
    },
    selected_tables,
    selected_titles
  )

  combined_grob <- arrangeGrob(
    grobs = table_grobs,
    ncol = 1
  )

  ggsave(
    filename = file.path(output_dir, "combined_tables.png"),
    plot = combined_grob,
    width = 10,
    height = 3.5 * length(table_grobs),
    dpi = 300,
    bg = "white"
  )
}