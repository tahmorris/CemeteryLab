# Install once if needed:
library(pacman)
pacman::p_load("dplyr","ggplot2","survival","survminer","ggpubr")

input_file <- "cemeteryData.csv"
output_dir <- "survivorship_curves"
birth_cutoff <- 1930 #Change this if you want but make a note in your methods what your new cutoff is!

#Change the values around to be nice for the graphic
sex_male_values <- c("m", "male")
sex_female_values <- c("f", "female")
yes_values <- c("y", "yes", "true", "1")
no_values <- c("n", "no", "false", "0")

# Set colors for the groups. Change them to whatever you want! Make it personalized. 
color_female <- "#D8C7F0"  
color_male <- "#F2B6B6"    
color_yes <- "#8E6BBE"     
color_no <- "#C96F6F"      

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

raw <- read.csv(input_file, fileEncoding = "Windows-1252")

required_columns <- c("Birth", "Sex", "Married", "Military", "Lifespan")
missing_columns <- setdiff(required_columns, names(raw))
if (length(missing_columns) > 0) {
  stop("Missing required column(s): ", paste(missing_columns, collapse = ", "))
}

normalize_yes_no <- function(x) {
  value <- tolower(trimws(as.character(x)))
  case_when(
    value %in% yes_values ~ "Yes",
    value %in% no_values ~ "No",
    TRUE ~ NA_character_
  )
}

dat <- raw |>
  mutate(
    Sex_raw = tolower(trimws(as.character(Sex))),
    Sex_group = case_when(
      Sex_raw %in% sex_male_values ~ "Male",
      Sex_raw %in% sex_female_values ~ "Female",
      TRUE ~ NA_character_
    ),
    Married_group = normalize_yes_no(Married),
    Military_group = normalize_yes_no(Military),
    Birth = suppressWarnings(as.numeric(Birth)),
    Lifespan = suppressWarnings(as.numeric(Lifespan)),
    Birth_period = case_when(
      is.na(Birth) ~ NA_character_,
      Birth < birth_cutoff ~ paste0("Before ", birth_cutoff),
      Birth >= birth_cutoff ~ paste0(birth_cutoff, " or later")
    )
  ) |>
  filter(!is.na(Lifespan), Lifespan >= 0)

if (nrow(dat) == 0) stop("No valid Lifespan values found.")

##########################################################
####Make the Survivorship Curves!#########################
##########################################################
# Compute empirical survivorship. At each lifespan x, this is the fraction
# of individuals with Lifespan >= x. The median is where this proportion
# first reaches 50%. The mean is shown separately in the legend.

make_survivorship_plot <- function(data, group_var, title, palette,
                                   filename, subtitle = NULL) {
  group_sym <- rlang::sym(group_var)
  plot_data <- data |>
    filter(!is.na(!!group_sym)) |>
    mutate(Group = as.character(!!group_sym))

  if (nrow(plot_data) == 0) {
    warning("Skipping ", filename, ": no usable observations.")
    return(invisible(NULL))
  }

  groups <- unique(plot_data$Group)

  curve_data <- bind_rows(lapply(groups, function(g) {
    values <- plot_data$Lifespan[plot_data$Group == g]
    times <- sort(unique(values))
    tibble(
      Lifespan = c(0, times),
      Surviving = c(100, vapply(times, function(t) mean(values >= t) * 100,
                                numeric(1))),
      Group = g
    )
  }))

  medians <- plot_data |>
    group_by(Group) |>
    summarise(
      Median = median(Lifespan),
      Mean = mean(Lifespan),
      n = n(),
      .groups = "drop"
    )

  legend_labels <- setNames(
    paste0(medians$Group, " (mean = ", round(medians$Mean, 1), ")"),
    medians$Group
  )

  p <- ggplot(curve_data,
              aes(x = Lifespan, y = Surviving, color = Group, group = Group)) +
    geom_step(linewidth = 1.1, direction = "hv") +
    geom_hline(yintercept = 50, linetype = "dashed", color = "grey55") +
    geom_vline(
      data = medians,
      aes(xintercept = Median, color = Group),
      linetype = "dotted",
      linewidth = 0.8,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = palette,
      breaks = names(legend_labels),
      labels = legend_labels,
      drop = FALSE
    ) +
    scale_y_continuous(
      name = "% surviving",
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Lifespan",
      color = NULL
    ) +
    theme_light(base_size = 13) +
    theme(legend.position = "bottom")

  if (!is.null(filename)) {
    ggsave(
      filename = file.path(output_dir, filename),
      plot = p,
      width = 8,
      height = 5.5,
      dpi = 300,
      bg = "white"
  )
}
  print(medians)
  invisible(p)
  }

# 1. Male and female survivorship curves
sex_dat <- dat |>
  filter(Sex_group %in% c("Female", "Male")) |>
  mutate(Sex_group = factor(Sex_group, levels = c("Female", "Male")))

make_survivorship_plot(
  data = sex_dat,
  group_var = "Sex_group",
  title = "Survivorship by sex",
  palette = c("Female" = color_female, "Male" = color_male),
  filename = "survivorship_by_sex.png",
  subtitle = "Male and Female"
)

# 2. Separate married versus unmarried plots for males and females
for (sex_value in c("Male", "Female")) {
  make_survivorship_plot(
    data = dat |> filter(Sex_group == sex_value),
    group_var = "Married_group",
    title = paste("Survivorship by marital status:", sex_value),
    palette = c("Yes" = color_yes, "No" = color_no),
    filename = paste0("survivorship_married_", tolower(sex_value), ".png")
  )
}

# 2.1 If you want to have a single figure with both graphics, do this too:
married_male <- make_survivorship_plot(
  data = dat |> filter(Sex_group == "Male"),
  group_var = "Married_group",
  title = "Male survivorship by marital status",
  palette = c("Yes" = color_yes, "No" = color_no),
  filename = NULL
)

married_female <- make_survivorship_plot(
  data = dat |> filter(Sex_group == "Female"),
  group_var = "Married_group",
  title = "Female survivorship by marital status",
  palette = c("Yes" = color_yes, "No" = color_no),
  filename = NULL
)

married_combined <- ggarrange(
  married_male,
  married_female,
  ncol = 2,
  nrow = 1
)

ggsave(
  file.path(output_dir, "survivorship_married_by_sex_combined.png"),
  married_combined,
  width = 12,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# 3. Military service among males
make_survivorship_plot(
  data = dat |> filter(Sex_group == "Male"),
  group_var = "Military_group",
  title = "Male survivorship by military service",
  palette = c("Yes" = color_yes, "No" = color_no),
  filename = "survivorship_military_males.png"
)

# 4. Birth period, sexes combined
make_survivorship_plot(
  data = dat |> filter(Sex_group %in% c("Female", "Male")),
  group_var = "Birth_period",
  title = paste("Survivorship by birth period (cutoff:", birth_cutoff, ")"),
  palette = c("Before 1930" = "#6B5B95", "1930 or later" = "#8BB7A5"),
  filename = "survivorship_by_birth_period.png"
)

message("File are put into the directory: ", normalizePath(output_dir))
