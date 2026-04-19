library(tidyverse)
library(tidytext)
library(Fonology)

# TODO: We will now load Moby Dick and Braz Cubas
load("data/moby_raw.RData")

md <- moby_raw |>
  unnest_tokens(word, text) |>
  filter(
    !word %in% stopwords_en
  ) |>
  mutate(
    ipa = ipa(word, lg = "en"),
    cv = cv(ipa),
    weight = getWeight(ipa, lg = "en"),
    stress = getStress(ipa),
    finSyl = getSyl(ipa, 1),
    onsetFin = syllable(finSyl, const = "onset", glides_as_onsets = TRUE)
  )

# NOTE: Check output (random sample):
md |> sample_n(size = 10)

# NOTE: Split syllables
md_CV <- md |>
  separate_rows(cv, sep = "\\.")

phonotactics_md <- md_CV |>
  distinct() |>
  filter(cv %in% c("CCV", "CV", "V", "VC", "CVC", "VCC", "CVCC", "CCVCC")) |>
  summarize(
    obs = n(), .by = cv
  ) |>
  mutate(
    prop = obs / sum(obs)
  )
# # A tibble: 8 × 3
#   cv      obs   prop
#   <chr> <int>  <dbl>
# 1 CV     7345 0.301
# 2 CVC    7738 0.317
# 3 V      1633 0.0670
# 4 CCV    1843 0.0756
# 5 VCC     460 0.0189
# 6 VC     2217 0.0910
# 7 CVCC   2556 0.105
# 8 CCVCC   580 0.0238

# NOTE: MaxEnt
tableau_en <- tribble(
  ~input, ~output, ~onset, ~no_coda, ~no_complex_onset, ~obs,
  "input", "CV", 0, 0, 0, 7596,
  "input", "CVC", 0, 1, 0, 8018,
  "input", "VC", 1, 1, 0, 2309,
  "input", "V", 1, 0, 0, 1718,
  "input", "CCV", 0, 0, 1, 1874,
  "input", "VCC", 1, 2, 0, 477,
  "input", "CVCC", 0, 2, 0, 2645,
  "input", "CCVCC", 0, 2, 1, 585,
)

weights_en <- tableau_en |>
  maxent() |>
  _$weights
#            onset          no_coda no_complex_onset
#         1.399693         0.439589         1.629694


# NOTE: Portuguese
braz_cubas <- read_lines("data/braz_cubas.txt") |>
  as_tibble() |>
  rename(text = value)

# NOTE: Tokenization
bc <- braz_cubas |>
  unnest_tokens(word, text) |>
  filter(
    !word %in% stopwords_pt,
  ) |>
  mutate(
    word = cleanText(word),
    ipa = ipa(word, lg = "pt"),
    cv = cv(ipa),
    weight = getWeight(ipa, lg = "pt"),
    stress = getStress(ipa),
    finSyl = getSyl(ipa, 1),
    onsetFin = syllable(finSyl, const = "onset", glides_as_onsets = TRUE)
  )

# NOTE: Let's now split syllables
bc_CV <- bc |>
  separate_rows(cv, sep = "\\.")

phonotactics_bc <- bc_CV |>
  distinct() |>
  filter(cv %in% c("CCV", "CV", "V", "VC", "CVC", "VCC", "CVCC", "CCVCC")) |>
  summarize(obs = n(), .by = cv) |>
  arrange(desc(obs)) |>
  mutate(prop = obs / sum(obs))
# # A tibble: 8 × 3
#   cv      obs    prop
#   <chr> <int>   <dbl>
# 1 CV     8209 0.444
# 2 CVC    5034 0.272
# 3 V      2122 0.115
# 4 VC     1617 0.0874
# 5 CCV    1369 0.0740
# 6 CVCC     82 0.00443
# 7 CCVCC    37 0.00200
# 8 VCC      28 0.00151

# NOTE: Now, the tableau

tableau_bc <- tribble(
  ~input, ~output, ~onset, ~no_coda, ~no_complex_onset, ~obs,
  "input", "CV", 0, 0, 0, 8209,
  "input", "CVC", 0, 1, 0, 5034,
  "input", "V", 1, 0, 0, 2122,
  "input", "VC", 1, 1, 0, 1617,
  "input", "CCV", 0, 0, 1, 1369,
  "input", "CVCC", 0, 2, 0, 82,
  "input", "VCC", 1, 2, 0, 28,
  "input", "CCVCC", 0, 2, 1, 37,
)

weights_pt <- tableau_bc |>
  maxent() |>
  _$weights
#            onset          no_coda no_complex_onset
#         1.263354         1.086148         1.984223


# NOTE: Let's create some figures now to visualize the patterns we just found.
# First, let's prepare our data by combining the phonotactics of both languages

phonotactics_md <- phonotactics_md |>
  mutate(
    lang = "English"
  )
phonotactics_bc <- phonotactics_bc |>
  mutate(
    lang = "Portuguese"
  )
phonotactics <- phonotactics_md |>
  bind_rows(phonotactics_bc)

# NOTE: Now we're ready for the figure
ggplot(data = phonotactics, aes(x = reorder(cv, prop), y = prop, color = lang)) +
  geom_line(aes(group = lang, color = lang), linetype = "dashed") +
  geom_point(aes(size = obs), show.legend = F) +
  theme_classic(base_family = "Futura") +
  theme(legend.position = "top") +
  labs(
    x = "Syllable shape", y = "Proportion", color = "Language:",
    title = "Phonotactics from a novel",
    subtitle = "Moby Dick and Braz Cubas"
  ) +
  scale_colour_manual(
    values = c("steelblue2", "darkorange2")
  )

# NOTE: Now let's see the grammars for both languages

weights <- tibble(
  lang = rep(c("En", "Pt"), each = length(weights_pt)),
  constraint = rep(names(weights_pt), times = 2),
  weight = c(weights_en, weights_pt)
)

# NOTE: Now the figure
ggplot(data = weights, aes(x = reorder(constraint, weight), y = weight, color = lang)) +
  geom_line(aes(group = lang, color = lang), linetype = "dashed") +
  geom_point(size = 4) +
  theme_classic(base_family = "Futura") +
  theme(legend.position = "top") +
  labs(
    x = "Constraint", y = "Weight", color = "Language:",
    title = "Phonotactics from a novel",
    subtitle = "Weights learned via MaxEnt"
  ) +
  scale_colour_manual(
    values = c("steelblue2", "darkorange2")
  )
