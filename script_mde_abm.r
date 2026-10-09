# List of packages needed
packages <- c("readxl", "readr", "dplyr", "stringr", "tidyr", "ggplot2", "forcats", "purrr", "tm", "wordcloud", "igraph", "ggraph", "corrplot", "RColorBrewer", "widyr", "tibble", "tidyverse", "tidygraph", "knitr", "kableExtra")

# Install missing packages
install.packages(packages[!packages %in% installed.packages()[, "Package"]])

install.packages("gapminder")

# Packages
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(purrr)
library(tm)
library(wordcloud)
library(igraph)
library(ggraph)
library(corrplot)
library(RColorBrewer)
library(widyr)
library(tibble)
library(tidyverse)
library(tidygraph)
library(knitr)
library(kableExtra)
library(stringi)
library(purrr)
library(stringr)

library(gapminder)

data("gapminder")
attach(gapminder)

# 1) load CSV
df <- read_csv2("C:/Users/tingo/Documents/Self_Vscode/Tout_Bazar/IDM-SocioEcosysteme/RevueSystematique/JeuDeDonnées/Analyse/mde_abm_data.csv", locale = locale(encoding = "UTF-8"))

glimpse(df) # View(df) to open
View(df)

# 2) Normalize column names
df <- df %>%
  rename(
    key = `Key`,
    item_type = `Item Type`,
    year = `Publication Year`,
    authors = Author,
    title = Title,
    venue = `Publication Title`,
    doi = DOI,
    abstract = `Abstract Note`,
    date = Date,
    approach_category = approach_category,
    mde_level = mde_level,
    target_platform = target_platform,
    main_tool = main_tool,
    code_generation = code_generation, 
    graphical_editor = graphical_editor, 
    open_source = open_source,
    application_domain = application_domain
  )

# 3) Clean Base Fields
df <- df %>%
  mutate(
    year = as.integer(year),
    item_type = str_to_lower(item_type),
    venue = str_squish(venue),
    title = str_squish(title),
    abstract = if_else(is.na(abstract), "", abstract)
  ) %>%
  filter(!is.na(year))

# 4) Normalization function
normalize_text <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_squish()
}

df <- df %>%
  mutate(domain_primary = factor("Methodology / DSL / MDE"))


# ============================================================
# Time distribution
# ============================================================

# 1. annual count
annual_counts <- df %>%
  filter(!is.na(year)) %>%
  count(year, name = "n") %>%
  arrange(year)

print(annual_counts)

# 2. descriptive statistics
summary_stats <- df %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    median_year = median(year, na.rm = TRUE),
    total_articles = n(),
    peak_year = year[which.max(table(year))],
    peak_n = max(table(year)),
    mean_articles_per_year = round(mean(table(year)), 2),
    annual_croissance_rate = round((total_articles / (max_year - min_year)), 3),
    sd_articles_per_year = round(sd(table(year)), 2),
    cv_articles_per_year = round((sd_articles_per_year / mean_articles_per_year) * 100, 2)
  )

print(summary_stats, width = Inf, n = Inf)

# 3. five-year periods
df <- df %>%
  mutate(
    period5 = case_when(
      year >= 2003 & year <= 2007 ~ "2003–2007",
      year >= 2008 & year <= 2012 ~ "2008–2012",
      year >= 2013 & year <= 2017 ~ "2013–2017",
      year >= 2018 & year <= 2022 ~ "2018–2022",
      year >= 2023 ~ "2023–2025",
      TRUE ~ NA_character_
    ),
    period5 = factor(period5, levels = c(
      "2003–2007", "2008–2012", "2013–2017",
      "2018–2022", "2023–2025"
    ))
  )

# Counting by five-year period
period5_counts <- df %>%
  count(period5, name = "n") %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    cum_pct = round(100 * cumsum(n) / sum(n), 1)
  ) %>%
  drop_na(period5)

print(period5_counts)


# 4. Main chart: annual histogram + LOESS trend + periods in the background
# Data frame for period bands
period_bands <- tribble(
  ~start, ~end, ~label, ~x_label,
  2003, 2007, "2003–2007", 2005,
  2008, 2012, "2008–2012", 2010,
  2013, 2017, "2013–2017", 2015,
  2018, 2022, "2018–2022", 2020,
  2023, 2025, "2023–2025", 2024
)

ggplot(annual_counts, aes(x = year, y = n)) +
  # Strips in the background
  geom_rect(
    data = period_bands,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    alpha = 0.1, fill = "grey70", inherit.aes = FALSE
  ) +
  # Labels for periods
  geom_text(
    data = period_bands,
    aes(x = x_label, y = Inf, label = label),
    vjust = 1.5, size = 3.5, color = "grey30", inherit.aes = FALSE
  ) +
  # Bars
  geom_col(fill = "#2C7FB8", width = 0.8) +
  # Trend LOESS
  geom_smooth(
    method = "loess", span = 0.5, se = FALSE,
    color = "red", linewidth = 1.2
  ) +
  # Scales
  scale_x_continuous(breaks = seq(2003, 2025, by = 2)) +
  labs(
    x = "Year of publication",
    y = "Number of articles",
    title = "Temporal distribution of publications (2003–2025)",
    caption = "Shaded bands: five-year periods | Red curve: LOESS trend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 9, color = "grey50"),
    panel.grid.minor = element_blank()
  )

# save for paper
ggsave("figures/temporal_distribution.png", width = 12, height = 7, dpi = 300, bg = "white")



# ============================================================
# Sources
# ============================================================

# 1. Source Categorization
df <- df %>%
  mutate(
    source_type = case_when(
      # 1. Exact matches on item_type
      item_type == "journalarticle" ~ "Journal",
      item_type %in% c("conferencepaper", "proceedingsPaper") ~ "Conf/Workshop",
      item_type == "booksection" ~ "bookChapter",

      # 2. Specific journals (Checked against lowercase venue name)
      str_detect(str_to_lower(venue), "autonomous agents and multi-agent systems|journal of systems and software|information and software technology|computer languages, systems & structures|computer standards & interfaces|acm trans.*model.*comput.*simul|science of computer programming|applied ontology|engineering applications of artificial intelligence") ~ "Journal",

      # 3. Conferences, proceedings, and workshops
      str_detect(str_to_lower(venue), "procedia computer science|conference|proceedings|workshop|symposium|compsac|sose|web intelligence|winter simulation|fedcsis|paams|simultech|essa|ecmda|model driven architecture|dexa|system of systems engineering|etfa") ~ "Conf/Workshop",

      # 4. Agent-specific workshops
      str_detect(str_to_lower(venue), "agent-oriented software engineering|engineering societies in the agents world|multi-agent systems and applications") ~ "Conf/Workshop",

      # 5. Handle missing values explicitly, otherwise throw to Autre
      is.na(venue) & is.na(item_type) ~ NA_character_,
      TRUE ~ "Autre"
    ),
    source_type = factor(source_type, levels = c("Journal", "Conf/Workshop", "bookChapter", "Autre"))
  )

# 2. venue count
venue_counts <- df %>%
  filter(!is.na(venue) & venue != "") %>%
  count(venue, name = "n") %>%
  arrange(desc(n), venue)

print(head(venue_counts, 8)) # Top 8

# 3. Concentration measurement
venue_summary <- venue_counts %>%
  summarise(
    total_venues = n_distinct(venue),
    total_articles = sum(n),
    singletons = sum(n == 1),
    singleton_share = round(100 * singletons / total_venues, 1),
    multi_article_venues = total_venues - singletons,
    coverage_top = sum(head(arrange(., desc(n)), multi_article_venues)$n) / total_articles * 100
  )

print(venue_summary)


# 4. Global distribution by source type
source_type_counts <- df %>%
  count(source_type, name = "n") %>%
  mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1)) %>%
  arrange(desc(n))

print(source_type_counts)





# ============================================================
# Areas of application for ABM models
# ============================================================

ci <- function(x) paste0("(?i)\\b(", x, ")\\b") # case-insensitive, word-bounded

tags_map <- tribble(
  ~axis, ~tag, ~pattern,

  # === Application ===
  "Application", "Transport / Traffic / ABTM / Smart Roads",
  ci("transport\\w*|traffic|mobility|travel[- ]demand|ABTM|smart[- ]roads?|crowd[- ]simulation"),
  "Application", "Energy / Smart Grids / CIM / Power Systems",
  ci("energy|smart[- ]?grids?|micro-?grids?|power[- ](engineering|systems?)|CIM|renewables?|electricity"),
  "Application", "Industry / Manufacturing / Scheduling / Industry 4.0",
  ci("manufactur\\w*|production[- ]systems?|scheduling|industry[- ]?4\\.0|PLCs?|saarstahl|supply[- ]chains?|logistics"),
  "Application", "SoS / Security / Cyber-Physical Systems",
  ci("security|RBAC|SoSSecML|systems?[- ]of[- ]systems?|SoSs?|cyber[- ]physical|CPS|vulnerabilit\\w*|attacks?|secure"),
  "Application", "Health / AAL / Ambient Assisted Living",
  ci("parkinson\\w*|ambient[- ]intelligence|AAL|assisted[- ]living|health\\w*|medical|patients?|epidemics?"),
  "Application", "Disaster / Emergency Management",
  ci("disasters?|emergenc(y|ies)|fire[- ]?fighting|floods?|DISPLAN"),

  # === Technical ===
  "Technical", "Metamodelling / DSML Design",
  ci("meta-?model\\w*|DSMLs?|domain[- ]specific[- ](modell?ing[- ])?languages?|DSLs?"),
  "Technical", "Model Transformations / MDA / Code Generation",
  ci("MDA|model[- ]driven[- ](architecture|engineering|development)|CIM|model[- ]transformations?|code[- ]generat\\w+|PIM|PSM|platform[- ]independent"),
  "Technical", "Verification / Validation / Debugging / Evaluation",
  ci("verification|validation|debugging|model[- ]checking|real-time[- ]constraints|requirements?[- ]constraints"),
  # 'evaluation', 'usability', 'throughput' removed: too generic
  "Technical", "Organizational Models / Interoperability",
  ci("AGR|MOISE\\+?|TAEMS|ISLANDER|OperA|organi[sz]ation(al)?[- ]models?|interoperab\\w*"),
  "Technical", "Platforms / Frameworks ABM/MAS",
  ci("JADE|JACK|JaCaMo|Jason|CArtAgO|Moise|Repast|INGENIAS|IDK|Malaca|MASDK|JIAC|PIM4Agents|SEA_ML|DSML4MAS"),
  "Technical", "Co-simulation / DEVS / Continuous-Time",
  ci("co-?simulation|FMI|DEVS|discrete[- ]event|continuous[- ]time|hybrid[- ]simulation|ML3"),
  "Technical", "Autonomic / Self-x / Organic Computing",
  ci("autonomic|self-(configur|optimi[sz]|protect|heal|adapt|organi[sz])\\w*|organic[- ]computing"),
  "Technical", "SOA / Service-Oriented / SoaML",
  ci("SOA|service[- ]oriented|SoaML")
)

detect_tags <- function(title, venue, abstract, map = tags_map) {
  text <- paste(coalesce(title, ""), coalesce(venue, ""), coalesce(abstract, ""))

  hits <- map[map_lgl(map$pattern, ~ str_detect(text, .x)), ]

  # Per-axis fallback
  app <- hits$tag[hits$axis == "Application"]
  tech <- hits$tag[hits$axis == "Technical"]
  if (length(app) == 0) app <- "Application: Other / General"
  if (length(tech) == 0) tech <- "Technical: General methodology / Theory"

  unique(c(app, tech)) # plain character vector
}

df <- df %>%
  mutate(tags = pmap(list(title, venue, abstract), detect_tags))


# 1. Classify the papers
classified <- df %>%
  mutate(
    domain_tags = pmap(list(title, venue, abstract), detect_tags)
  )

# 2. Long format expansion
fallback_tags <- c(
  "Application: Other / General",
  "Technical: General methodology / Theory"
)

# 1. One row per (paper, tag)
tags_long <- df %>%
  mutate(paper_id = row_number()) %>% # skip if you already have an ID
  unnest_longer(tags, values_to = "tag") %>%
  filter(!is.na(tag)) %>%
  mutate(tag = as.character(tag)) %>%
  distinct(paper_id, tag) # guard against duplicates

# 2. Counts, excluding fallbacks
tags_counts <- tags_long %>%
  filter(!tag %in% fallback_tags) %>%
  count(tag, name = "n", sort = TRUE)

# 3. Counts per axis, keeping tags with zero occurrences
summary_metrics <- tags_map %>%
  select(axis, tag) %>% # drop the regex column
  left_join(tags_counts, by = "tag") %>%
  mutate(occurrences = replace_na(n, 0L)) %>%
  select(axis, tag, occurrences) %>%
  arrange(axis, desc(occurrences))

print(tags_counts)
print(summary_metrics)

# 4. Export
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
write_csv(tags_counts, "tables/tags_counts_20261002.csv")
write_csv(summary_metrics, "tables/tags_summary_by_axis_20261002.csv")

# 5. Generate precise frequencies, numerators, and denominators
total_occurrences <- sum(summary_metrics$occurrences)
technical_total <- summary_metrics %>%
  filter(axis == "Technical") %>%
  pull(occurrences) %>%
  sum()
application_total <- summary_metrics %>%
  filter(axis == "Application") %>%
  pull(occurrences) %>%
  sum()
technical_ratio <- (technical_total / total_occurrences) * 100

# Print
cat(sprintf("Total Label Occurrences (Denominator): %d\n", total_occurrences))
cat(sprintf("Technical Axis Occurrences (Numerator): %d\n", technical_total))
cat(sprintf("Application Axis Occurrences: %d\n", application_total))
cat(sprintf("Calculated Percentage: %.2f%%\n", technical_ratio))

# 6. Plot
plot_data <- tags_counts %>%
  left_join(distinct(tags_map, axis, tag), by = "tag") %>%
  mutate(
    axis      = factor(axis, levels = c("Application", "Technical")),
    tag_label = str_wrap(tag, width = 35)
  )

p <- ggplot(
  plot_data,
  aes(x = n, y = fct_reorder(tag_label, n), fill = axis)
) +
  geom_col(width = 0.7) +
  #geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(
    values = c(Application = "#2C7FB8", Technical = "#F28E2B"),
    name   = NULL
  ) +
  labs(
    x = "Number of articles",
    y = NULL,
    title = "Themes identified (applications and technical areas)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y        = element_text(size = 10),
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom"
  )

print(p)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/main_themes.png", p,
  width = 10, height = max(5, 0.4 * nrow(plot_data) + 1.5),
  dpi = 300, bg = "white"
)


# 7. Compute the symmetric co-occurrence matrix using matrix multiplication

fallback_tags <- c(
  "Application: Other / General",
  "Technical: General methodology / Theory"
)

# tags_long must contain: key, tag
binary_df <- tags_long %>%
  filter(!tag %in% fallback_tags) %>%
  distinct(paper_id, tag) %>% # avoids list-cols in pivot_wider
  mutate(present = 1L) %>%
  pivot_wider(
    id_cols     = paper_id,
    names_from  = tag,
    values_from = present,
    values_fill = 0L
  )

# Order columns by axis, then by frequency
tag_order <- tags_map %>%
  distinct(axis, tag) %>%
  filter(tag %in% names(binary_df)) %>%
  left_join(tags_counts, by = "tag") %>%
  arrange(axis, desc(n)) %>%
  pull(tag)

binary_tag_matrix <- binary_df %>%
  select(all_of(tag_order)) %>% # drops key and sets the order
  as.matrix()
storage.mode(binary_tag_matrix) <- "integer"

# Co-occurrence: counts of papers sharing two tags
co_occurrence_matrix <- crossprod(binary_tag_matrix)

# Optional variants
co_offdiag <- co_occurrence_matrix
diag(co_offdiag) <- 0

n_tag <- diag(co_occurrence_matrix)
jaccard <- co_occurrence_matrix / (outer(n_tag, n_tag, "+") - co_occurrence_matrix)

cat("=== CO-OCCURRENCE MATRIX ===\n")
print(co_occurrence_matrix)

# Export
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
write_csv(
  as_tibble(co_occurrence_matrix, rownames = "tag"),
  "tables/co_occurrence_matrix_20261002.csv"
)
write_csv(
  as_tibble(round(jaccard, 3), rownames = "tag"),
  "tables/co_occurrence_jaccard_matrix_20261002.csv"
)


# 8. export in graphic

# Number of Application tags (they come first in tag_order)
n_app <- sum(tag_order %in% tags_map$tag[tags_map$axis == "Application"])

heat_data <- co_offdiag %>%
  as_tibble(rownames = "tag1") %>%
  pivot_longer(-tag1, names_to = "tag2", values_to = "n") %>%
  mutate(
    tag1 = factor(tag1, levels = rev(tag_order)),
    tag2 = factor(tag2, levels = tag_order)
  ) %>%
  # keep the lower triangle only, diagonal excluded
  filter(as.integer(factor(tag1, levels = tag_order)) >
    as.integer(tag2))

max_n <- max(heat_data$n, na.rm = TRUE)

p_heat <- ggplot(heat_data, aes(x = tag2, y = tag1, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(
      label = ifelse(n > 0, n, ""),
      colour = n > 0.6 * max_n
    ),
    size = 3.2, show.legend = FALSE
  ) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey15")) +
  scale_fill_gradient(
    low = "#F2F7FB", high = "#08519C",
    limits = c(0, max_n), name = "Number of\narticles"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 22), position = "bottom", drop = FALSE) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 32), drop = FALSE) +
  geom_vline(xintercept = n_app + 0.5, colour = "grey40", linetype = "dashed") +
  geom_hline(
    yintercept = length(tag_order) - n_app + 0.5,
    colour = "grey40", linetype = "dashed"
  ) +
  coord_fixed() +
  labs(
    x = NULL, y = NULL,
    title = "Co-occurrence of themes",
    subtitle = "Number of articles sharing two themes"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5, colour = "grey30"),
    axis.text.x     = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y     = element_text(size = 9),
    panel.grid      = element_blank(),
    legend.position = "right"
  )

print(p_heat)

# Export
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
k <- length(tag_order)
w <- max(8, 0.65 * k + 4)
h <- max(6, 0.55 * k + 2)

ggsave("figures/cooccurrence_heatmap_20261002.png", p_heat,
  width = w, height = h, dpi = 300, bg = "white"
)
ggsave("figures/cooccurrence_heatmap_20261002.pdf", p_heat,
  width = w, height = h, bg = "white"
)


# =============================================
#  Bubble plot tags × période
# =============================================

fallback_tags <- c(
  "Application: Other / General",
  "Technical: General methodology / Theory"
)

# 1. Periods (single source of truth for the labels)
breaks <- c(2003, 2008, 2013, 2018, 2023, 2026) # upper bound exclusive
labels <- c("2003–2007", "2008–2012", "2013–2017", "2018–2022", "2023–2025")

df <- df %>%
  mutate(
    paper_id = row_number(),
    key = as.character(key),
    year = as.integer(year),
    period5 = cut(year,
      breaks = breaks, labels = labels,
      right = FALSE, include.lowest = TRUE
    )
  )

# Checks: unique key, and papers falling outside the periods
stopifnot(!anyDuplicated(df$key))
df %>%
  filter(is.na(period5)) %>%
  count(year) # should be empty or expected


tags_period <- tags_long %>%
  select(paper_id, tag) %>%
  distinct() %>%
  left_join(df %>% select(paper_id, period5),
    by = "paper_id",
    relationship = "many-to-one"
  ) %>%
  filter(!tag %in% fallback_tags, !is.na(period5)) %>%
  count(tag, period5, name = "n") %>%
  group_by(tag) %>%
  filter(sum(n) >= 3) %>%
  ungroup() %>%
  complete(tag, period5, fill = list(n = 0L))

# 4. Papers per period, for normalisation
papers_per_period <- df %>%
  filter(!is.na(period5)) %>%
  count(period5, name = "N_period")

print(papers_per_period)

tags_period <- tags_period %>%
  left_join(papers_per_period, by = "period5") %>%
  mutate(share = n / N_period)

# 5. Wrap only for plotting, after joining anything that needs the raw tag
plot_period <- tags_period %>%
  mutate(tag_label = str_wrap(tag, width = 40))


# version1
plot_bubble <- plot_bubble %>%
  mutate(n_plot = na_if(n, 0L)) # no bubble for zero cells

# order themes by total count, keep every theme as a level
ord <- plot_bubble %>%
  group_by(tag_label) %>%
  summarise(tot = sum(n), .groups = "drop") %>%
  arrange(tot) %>%
  pull(tag_label) %>%
  as.character()

plot_bubble <- plot_bubble %>%
  mutate(tag_label = factor(as.character(tag_label), levels = ord))

p_bubble <- ggplot(
  plot_bubble,
  aes(x = period5, y = tag_label, size = n_plot)
) +
  geom_point(colour = "#2C7FB8", alpha = 0.8, na.rm = TRUE) +
  # geom_text(aes(label = n_plot), size = 2.8, colour = "grey20",
  #          vjust = -1.8, na.rm = TRUE) +            # optional: count above each bubble or geom_text(aes(label = n_plot), size = 3, colour = "black", na.rm = TRUE, show.legend = FALSE) +   # centred by default
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_size_area(max_size = 12, name = "Number of\narticles") +
  labs(
    x = "Five-year period", y = NULL,
    title = "Evolution of themes by five-year period"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_bubble)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/themes_by_period_bubble_counts_20261002.png", p_bubble,
  width = 10, height = max(5, 0.45 * nlevels(plot_bubble$tag_label) + 2),
  dpi = 300, bg = "white"
)
-----------------------------------------------------
  # version 2
  plot_bubble <- tags_period %>%
  left_join(distinct(tags_map, axis, tag), by = "tag") %>%
  mutate(
    axis = factor(axis, levels = c("Application", "Technical")),
    n_plot = na_if(n, 0L), # no bubble for zero cells
    share_plot = na_if(share, 0),
    tag_label = str_wrap(tag, width = 35)
  )

stopifnot(sum(is.na(plot_bubble$axis)) == 0) # every tag must exist in tags_map

p_bubble <- ggplot(
  plot_bubble,
  aes(
    x = period5,
    y = tidytext::reorder_within(tag_label, n, axis, fun = sum),
    size = share_plot, fill = axis
  )
) +
  geom_point(shape = 21, colour = "white", stroke = 0.5, alpha = 0.9, na.rm = TRUE) +
  geom_text(aes(label = n_plot),
    size = 2.8, colour = "grey20",
    vjust = -1.6, na.rm = TRUE
  ) + # count above each bubble
  tidytext::scale_y_reordered() +
  facet_grid(axis ~ ., scales = "free_y", space = "free_y") +
  scale_size_area(
    max_size = 11, labels = scales::percent_format(accuracy = 1),
    name = "Share of articles\nin the period"
  ) +
  scale_fill_manual(
    values = c(Application = "#2C7FB8", Technical = "#F28E2B"),
    guide = "none"
  ) +
  labs(
    x = "Five-year period",
    y = NULL,
    title = "Evolution of themes by five-year period"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(hjust = 0.5, face = "bold"),
    axis.text.y        = element_text(size = 10),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.y       = element_text(angle = 0, face = "bold"),
    legend.position    = "right"
  )

print(p_bubble)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/themes_by_period_bubble.png", p_bubble,
  width = 10, height = max(5, 0.45 * n_distinct(plot_bubble$tag) + 2),
  dpi = 300, bg = "white"
)
ggsave("figures/themes_by_period_bubble.pdf", p_bubble,
  width = 10, height = max(5, 0.45 * n_distinct(plot_bubble$tag) + 2)
)


# ==============================================================================
# SENSITIVITY AND ROBUSTNESS ANALYSIS FOR THE MDE-ABM SLR
# ==============================================================================

stopifnot(
  exists("df"), exists("tags_map"), exists("tags_long"),
  exists("fallback_tags"), exists("tags_counts")
)

# 0. Metric definition
# "Technical vs Application" label per study.
tech_occ_share <- function(tl, label) {
  tl <- tl %>%
    filter(!tag %in% fallback_tags) %>%
    left_join(distinct(tags_map, axis, tag), by = "tag")
  k <- sum(tl$axis == "Technical", na.rm = TRUE)
  n <- sum(tl$axis %in% c("Technical", "Application"))
  tibble(
    Analysis = label, Occurrences = n, Technical = k,
    Pct = 100 * k / n
  )
}

baseline <- tech_occ_share(tags_long, "Baseline (all occurrences)")
print(baseline)

# 1. Known coding weaknesses to carry into the tests
ambiguous_security_studies <- df %>%
  filter(str_detect(paste(title, venue, abstract), regex("\\b(security|secure)\\b", ignore_case = TRUE))) %>%
  pull(paper_id)
cat(
  "Studies where the ambiguous 'security'/'secure' term fires:",
  length(ambiguous_security_studies), "\n"
)


# 2. Weakly evaluated studies (Test 1)
notes_clean <- df$notes %>%
  coalesce("") %>%
  str_remove_all("<[^>]+>") %>%
  str_squish()

weak_pattern <- regex(
  paste0(
    # French: "un seul cas", "cas unique", etc.
    "(un seul|unique) (cas|exemple)",
    "|(limit|restreint)\\w* .{0,4} un (seul )?(cas|exemple)",
    "|(encore|purement) th.{1,3}oriq",
    "|pas de validation|absence de validation",
    # English equivalents
    "|only (a |one )?(single )?(case|example)",
    "|(limited|restricted) to (a |one )?(single )?(case|example)",
    "|(still|purely|merely) theoretical",
    "|(no|lack(?:s|ing)? of|without) validation"
  ),
  ignore_case = TRUE
)

df <- df %>% mutate(weak_eval = str_detect(notes_clean, weak_pattern))
cat(
  "Studies flagged as weakly evaluated (proxy from Notes):",
  sum(df$weak_eval), "/", nrow(df), "\n"
)

t1 <- tags_long %>%
  semi_join(filter(df, !weak_eval), by = "paper_id") %>%
  tech_occ_share("T1: excluding weakly evaluated studies")

print(t1)

# 3. Consolidate studies describing the same named tool/framework (Test 2)
tool_families <- c(
  "PIM4Agents"    = "PIM4Agents",
  "SEA_ML"        = "SEA_ML",
  "DSML4MAS"      = "DSML4MAS",
  "INGENIAS"      = "INGENIAS",
  "Malaca"        = "Malaca",
  "MetaMORPhOSY"  = "MetaMORP\\(?h?\\)?OSY"
)

df <- df %>%
  mutate(
    text_all = paste(title, venue, abstract),
    tool_family = tool_families[
      map_chr(text_all, function(t) {
        hit <- names(tool_families)[map_lgl(tool_families, ~ str_detect(t, regex(.x, ignore_case = TRUE)))]
        if (length(hit) == 0) NA_character_ else hit[1]
      })
    ]
  )

cat("Studies attached to a named tool family:", sum(!is.na(df$tool_family)), "\n")
print(count(filter(df, !is.na(tool_family)), tool_family, sort = TRUE))

units <- df %>%
  mutate(unit = coalesce(tool_family, key)) %>%
  group_by(unit) %>%
  summarise(rep_paper_id = first(paper_id), .groups = "drop") # one representative paper per unit

t2 <- tags_long %>%
  semi_join(units, by = c("paper_id" = "rep_paper_id")) %>%
  tech_occ_share("T2: one occurrence set per tool family")

print(t2)

# 4. Stress test on the ambiguous "security" / "secure" occurrences (Test 3)
t3a <- tags_long %>%
  filter(!(paper_id %in% ambiguous_security_studies &
    tag == "SoS / Security / Cyber-Physical Systems")) %>%
  tech_occ_share("T3a: drop ambiguous 'security' occurrences")

t3b <- tags_long %>%
  filter(!tag %in% fallback_tags) %>%
  left_join(distinct(tags_map, axis, tag), by = "tag") %>%
  mutate(axis = if_else(paper_id %in% ambiguous_security_studies &
    tag == "SoS / Security / Cyber-Physical Systems",
  "Technical", axis
  )) %>%
  {
    tibble(
      Analysis = "T3b: ambiguous 'security' reassigned to Technical",
      Occurrences = sum(.$axis %in% c("Technical", "Application")),
      Technical = sum(.$axis == "Technical"),
      Pct = 100 * sum(.$axis == "Technical") /
        sum(.$axis %in% c("Technical", "Application"))
    )
  }

# 5. Leave-one-study-out
all_papers <- unique(tags_long$paper_id)
loo <- map_dbl(all_papers, function(pid) {
  tl <- filter(tags_long, paper_id != pid)
  tech_occ_share(tl, "loo")$Pct
})
cat(sprintf("Leave-one-out range: %.1f%% to %.1f%%\n", min(loo), max(loo)))


# 6. Summary
sensitivity_matrix <- bind_rows(baseline, t1, t2, t3a, t3b) %>%
  mutate(
    Delta_pp = round(Pct - baseline$Pct, 1),
    Pct = round(Pct, 1)
  )

cat("\n=== SENSITIVITY EXECUTION MATRIX ===\n")
print(sensitivity_matrix, width = Inf)

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
write_csv(sensitivity_matrix, "tables/sensitivity_analysis_20261002.csv")



# =============================================
# Co-occurrence Network
# =============================================

COOC_MIN_STUDIES <- 2

# 1. Build the edge list (aligned with the rest of the pipeline: use
#    fallback_tags, not the outdated single-string fallback label)
cooc_df <- tags_long %>%
  filter(!tag %in% fallback_tags) %>%
  distinct(paper_id, tag) %>%
  group_by(paper_id) %>%
  filter(n() > 1) %>% # studies with >= 2 substantive tags only
  ungroup() %>%
  inner_join(., ., by = "paper_id", relationship = "many-to-many") %>%
  filter(tag.x < tag.y) %>%
  count(tag.x, tag.y, name = "weight")

# Consistency check against the co-occurrence matrix built earlier
stopifnot(exists("co_occurrence_matrix"))
check <- cooc_df %>%
  rowwise() %>%
  mutate(ref = co_occurrence_matrix[tag.x, tag.y]) %>%
  ungroup()
stopifnot(all(check$weight == check$ref))

# Report the full (unthresholded) weight table
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
write_csv(cooc_df, "tables/cooccurrence_weights_full_20261002.csv")

edges <- cooc_df %>% filter(weight >= COOC_MIN_STUDIES)
cat(sprintf(
  "Edges shown: %d of %d pairs with at least one shared study (threshold: >= %d shared studies)\n",
  nrow(edges), nrow(cooc_df), COOC_MIN_STUDIES
))

# 2. Graph and figure
graph <- edges %>%
  rename(from = tag.x, to = tag.y) %>%
  as_tbl_graph(directed = FALSE)

p_net <- ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight), colour = "grey60", show.legend = FALSE) +
  geom_node_point(aes(size = centrality_degree() + 1), colour = "#2C7FB8") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4, max.overlaps = 20) +
  scale_edge_width(range = c(1, 5), name = "Shared studies") +
  scale_edge_alpha(range = c(0.5, 1)) +
  labs(
    title = "Thematic co-occurrence network",
    subtitle = sprintf(
      "Descriptive network; edges shown for pairs of themes shared by at least %d studies",
      COOC_MIN_STUDIES
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, colour = "grey30"),
    legend.position = "bottom"
  )

print(p_net)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
ggsave("figures/network_cooc_themes20261002.png", p_net, width = 12, height = 10, dpi = 300, bg = "white")



# =============================================
# MDE
# =============================================

view(df)

# 2. checking
cat("Total loaded articles :", nrow(df), "\n")
cat("columns present :", paste(names(df), collapse = ", "), "\n")

# Overview of the first 10 lines
print(head(df, 10))

# Quick Category Statistics
cat("\n--- Distribution by approach category ---\n")
table(df$approach_category, useNA = "ifany")


cat("\n--- Distribution by period  ---\n")
df <- df %>%
  mutate(
    period5 = case_when(
      year >= 2003 & year <= 2007 ~ "2003–2007",
      year >= 2008 & year <= 2012 ~ "2008–2012",
      year >= 2013 & year <= 2017 ~ "2013–2017",
      year >= 2018 & year <= 2022 ~ "2018–2022",
      year >= 2023 ~ "2023–2025",
      TRUE ~ NA_character_
    ),
    # Transform into a factor with chronological order
    period5 = factor(period5,
      levels = c(
        "2003–2007", "2008–2012", "2013–2017",
        "2018–2022", "2023–2025"
      )
    )
  )

cat("Distribution by five-year period :\n")
if ("period5" %in% names(df)) {
  table(df$period5, useNA = "ifany")
} else {
  cat("Column period5 missing - we will recreate it if needed later\n")
}


# 3. Clean internal backup (optional, for quick recovery)
saveRDS(df, "intermediate_data/approaches_classif_final.rds")

cat("\nData loaded and ready! You can now run the queries for tables/figures.\n")


# 4. Frequency table by category
freq_table <- df %>%
  count(approach_category, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  tidyr::replace_na(list(approach_category = "Non specified")) %>%
  arrange(desc(n))
print(freq_table)

# Export LaTeX
freq_table %>%
  kable(format = "latex", booktabs = TRUE, caption = "Frequency of MDE approach categories for MAS") %>%
  cat(file = "tables/freq_approaches_20261002.tex")

# frequency for mde_level
freq_mde <- df %>%
  count(mde_level, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
print(freq_mde)



# 5. Frequency band by category
ggplot(freq_table, aes(x = reorder(approach_category, n), y = n, fill = approach_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), hjust = 0, nudge_y = 0.5, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Number of articles",
    title = "Distribution of MDE approaches in primary studies",
    subtitle = "Comparison of methodological categories"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    axis.text.y = element_text(size = 11),
    legend.position = "none"
  )

ggsave("figures/freq_approaches_20261002.png", width = 12, height = 6, dpi = 300, bg = "white")


# 7. Evolution by period (stacked bar) - optional but impacting
freq_period <- df %>%
  count(period5, approach_category) %>%
  group_by(period5) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()
print(freq_period)

ggplot(freq_period, aes(x = period5, y = n, fill = approach_category)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    x = "Five-year period",
    y = "Number of articles",
    fill = "Approach category",
    title = "Evolution of MDE approach categories over time",
    subtitle = "Distribution by five-year period"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("figures/evolution_approaches_20261002.png", width = 12, height = 8, dpi = 300, bg = "white")


# 9. Heatmap co‑occurrence (approach_category × mde_level)

# Counting co-occurrences
freq_cooc <- df %>%
  count(approach_category, mde_level, name = "n") %>%
  group_by(approach_category) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Heatmap
ggplot(freq_cooc, aes(x = mde_level, y = approach_category, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(n, "\n", pct, "%")), size = 3) +
  scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
  labs(
    x = "MDE level",
    y = "Approach category",
    fill = "Number of articles",
    title = "Co-occurrence of approach categories and MDE levels",
    subtitle = "Each cell indicates the count and relative percentage per category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

ggsave("figures/heatmap_approach_mdelevel_20261002.png", width = 10, height = 6, dpi = 300, bg = "white")


# =============================================
# Tool and platform
# =============================================

view(df)

cat("Columns found:", paste(names(df), collapse = ", "), "\n")
cat("Rows:", nrow(df), "Columns:", ncol(df), "\n")

missing <- setdiff(c("key", "main_tool"), names(df))
if (length(missing) > 0) {
  stop(
    "Missing column(s): ", paste(missing, collapse = ", "),
    ". Check (1) that write/read use the same delimiter, and ",
    "(2) that you are reading the manually annotated file, not the raw export."
  )
}

stopifnot(all(c("key", "main_tool") %in% names(df)))

# Flag studies whose main_tool is a composite (comparison papers) so they are
# handled deliberately rather than silently forming a spurious extra group.
composite <- df %>% filter(str_detect(main_tool, ","))
if (nrow(composite) > 0) {
  cat("WARNING:", nrow(composite), "studies have a composite main_tool value:\n")
  print(composite %>% select(key, df))
}

tools_clean <- df %>%
  filter(!is.na(main_tool), main_tool != "", main_tool != "NA")


# 1. Aggregation per tool -- consolidation rule stated explicitly here and in
outil_comparison <- tools_clean %>%
  group_by(main_tool) %>%
  summarise(
    references = paste(sort(unique(key)), collapse = ","),

    # count the number of references (studies) per tool, for the main inventory table
    study_count = n_distinct(key),
    target_platform = {
      raw <- target_platform[!is.na(target_platform) & !target_platform %in% c("NA", "No", "")]
      split <- unlist(strsplit(raw, ",\\s*"))
      uniq <- sort(unique(str_trim(split)))
      if (length(uniq) > 0) paste(uniq, collapse = ", ") else "Not specified"
    },
    code_generation = case_when(
      any(code_generation == "Yes") ~ "Yes",
      any(code_generation == "Partial") ~ "Partial",
      TRUE ~ "No"
    ),
    graphical_editor = case_when(
      any(graphical_editor == "Yes") ~ "Yes",
      any(graphical_editor == "Partial") ~ "Partial",
      TRUE ~ "No"
    ),
    open_source = if_else(any(open_source == "Yes"), "Yes", "No"),

    # tool_version   = paste(sort(unique(na.omit(tool_version))),   collapse = "; "),
    # date_assessed  = paste(sort(unique(na.omit(date_assessed))),  collapse = "; "),

    # to determine date assessed let's determine the minimum year and maximum one
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_studies = n(), # RENAMED from n_mentions: number of studies whose
    # primary/central tool is main_tool (consolidated
    # across all papers reporting on it)
    .groups = "drop"
  ) %>%
  mutate(
    target_platform = if_else(target_platform == "", "Not specified", target_platform),
    latex_refs = paste0("\\cite{", references, "}")
  )

n_tools_total <- n_distinct(outil_comparison$main_tool, na.rm = TRUE)
cat("Total distinct tools identified:", n_tools_total, "\n")
cat(
  "Total studies consolidated:", sum(outil_comparison$n_studies),
  "(should equal", nrow(tools_clean), "if main_tool is clean)\n"
)
stopifnot(sum(outil_comparison$n_studies) == nrow(tools_clean))


dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("intermediate_data", showWarnings = FALSE, recursive = TRUE)

# 2a. FULL inventory (all n_tools_total tools)
write_csv(outil_comparison, "tables/tool_inventory_full_20261002.csv")


# 2b. Study-to-tool mapping
study_tool_map <- tools_clean %>%
  select(key, year, title, main_tool) %>%
  arrange(main_tool, year)
write_csv(study_tool_map, "tables/study_to_tool_mapping_20261002.csv")


# 3. Main-text extract 
# ------------------------------------------------------------------------------
# Inclusion rule (state this verbatim in Methods):
#   A tool/framework is included in the main-text comparative table if it
#   satisfies BOTH:
#     (C1) Evidence sufficiency: reported/evaluated in >= MIN_STUDIES
#          independent primary studies (avoids comparing on the basis of a
#          single team's implementation choices).
#     (C2) Data completeness: none of the five compared capability
#          dimensions (code_generation, graphical_editor, validation_support,
#          open_source, maturity_level) is missing/unassessable for that tool.
#   Tools failing either criterion remain in the full inventory
#   (tables/tool_inventory_full.tex) but are excluded from the main-text
#   comparison, since an incomplete or single-study row is not a fair
#   comparison point.
# ==============================================================================

MIN_STUDIES <- 2   # justified a priori: excludes single-paper ("hapax") tools

# --- Criterion 1: evidence sufficiency -----------------------------------
meets_evidence <- outil_comparison %>%
  filter(n_studies >= MIN_STUDIES)

cat(sprintf("C1 (n_studies >= %d): %d / %d tools qualify\n",
            MIN_STUDIES, nrow(meets_evidence), nrow(outil_comparison)))

# --- Criterion 2: data completeness on the compared dimensions -----------
#capability_cols <- c("code_generation", "graphical_editor",
#                     "validation_support", "open_source", "maturity_level")

capability_cols <- c("code_generation", "graphical_editor", "open_source")

is_complete <- meets_evidence %>%
  mutate(n_unassessed = rowSums(across(all_of(capability_cols),
                                       ~ is.na(.x) | .x %in% c("NA", "")))) %>%
  filter(n_unassessed == 0) %>%
  select(-n_unassessed)

cat(sprintf("C1 & C2 (also fully assessed): %d / %d tools qualify\n",
            nrow(is_complete), nrow(meets_evidence)))

excluded_c1 <- setdiff(outil_comparison$main_tool, meets_evidence$main_tool)
excluded_c2 <- setdiff(meets_evidence$main_tool, is_complete$main_tool)
cat("Excluded by C1 (< ", MIN_STUDIES, " studies): ", paste(excluded_c1, collapse = ", "), "\n", sep = "")
cat("Excluded by C2 (incomplete data): ", paste(excluded_c2, collapse = ", "), "\n", sep = "")

main_text_tools <- is_complete %>%
  arrange(desc(n_studies), main_tool)   

n_selected <- nrow(main_text_tools)
cat(sprintf("\nMain-text comparison table: %d tools meeting both criteria (out of %d total identified).\n",
            n_selected, nrow(outil_comparison)))

MAIN_TEXT_SPACE_LIMIT <- 10   

if (n_selected > MAIN_TEXT_SPACE_LIMIT) {
  main_text_tools <- main_text_tools %>% slice(1:MAIN_TEXT_SPACE_LIMIT)
  cat(sprintf(
    "NOTE: %d tools met the inclusion criteria; main text shows the %d most-studied for space, remainder in Supplementary Table S[Y].\n",
    n_selected, MAIN_TEXT_SPACE_LIMIT))
}


stopifnot(exists("main_text_tools"), nrow(main_text_tools) > 0)

main_text_tools %>%
  select(main_tool, n_studies, latex_refs, target_platform, code_generation,
         graphical_editor, open_source) %>%
  kable(format = "latex", booktabs = TRUE, escape = FALSE,
        col.names = c("Main tool", "N studies", "References", "Target platform(s)",
                      "Code generation.", "Graphical editor",
                      "Open source"),
        caption = sprintf(
          "Tools meeting the inclusion criteria for comparative analysis: reported in at least %d independent primary studies, with complete capability data across all compared dimensions (%d of %d tools identified qualify; ranked here by number of studies, ties broken alphabetically; see Table Y for the complete inventory, including tools excluded by these criteria).",
          MIN_STUDIES, n_selected, nrow(outil_comparison))) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header"), font_size = 8) %>%
  column_spec(1, width = "3.2cm", bold = TRUE) %>%
  column_spec(2, width = "1.2cm") %>%
  column_spec(3, width = "2.8cm") %>%
  column_spec(4, width = "2.2cm") %>%
  column_spec(5:7, width = "2.2cm") %>%
  row_spec(0, bold = TRUE) %>%
  cat(file = "tables/extrait_outils_20261002.tex")

cat(sprintf("Exported main-text comparison table: %d tools (tables/extrait_outils_7.2.2.tex)\n",
            nrow(main_text_tools)))



# =============================================
# 3. Feature Figure 
# =============================================

n_tools <- nrow(outil_comparison)

# 1. Feature calculation (with na.rm = TRUE to avoid NA)
features_summary <- outil_comparison %>%
  summarise(
    n_total = n_tools,
    `Code generation` = sum(code_generation == "Yes", na.rm = TRUE),
    `Graphical editor` = sum(graphical_editor == "Yes", na.rm = TRUE),
  ) %>%
  pivot_longer(-n_total, names_to = "Feature", values_to = "Number") %>%
  mutate(
    percent = round(100 * Number / n_total, 1),
    Label = paste0(Number, " (", percent, " %)")
  ) %>%
  arrange(desc(Number))

print(features_summary)

features_summary %>%
  # 1. Clean UTF-8 strings (critical for French accents like é and à)
  mutate(across(where(is.character), ~ iconv(., to = "UTF-8", sub = "byte"))) %>%
  # 2. Drop the raw counts/totals to only display the key columns
  select(Feature, Label) %>%
  # 3. Render into a LaTeX table
  kable(
    format = "latex", 
    booktabs = TRUE, 
    escape = FALSE,
    col.names = c("Feature", "Number"),
    caption = "Summary of key features identified among the tools identified."
  ) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  column_spec(1, width = "5.0cm", bold = TRUE) %>%
  column_spec(2, width = "3.5cm", ) %>%
  row_spec(0, bold = TRUE) %>%
  # 4. Export as a .tex file
  cat(file = "tables/features_summary_20261002.tex")

getwd()
