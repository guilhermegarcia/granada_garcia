# NOTE: Now load the packages and we're good to go
library(tidyverse)
library(tidytext)
library(Fonology)

# NOTE: Let's create some variables
numbers <- c(5, 10, 15)

# NOTE: Two ways to run a function
mean(numbers)
numbers |> mean()

# NOTE: We can also have a variable with strings
cities <- c("Madrid", "Granada", "Rome")
cities[2]
length(cities)

# NOTE: With tidyverse (stringr), we have many functions for strings
str_length(cities)
str_to_upper(cities)
str_to_lower(cities)
cities |>
  str_extract(pattern = "[A-Z]")
cities |>
  str_sub(start = 1, end = 3) |>
  str_to_upper()

# NOTE: Now let's get into Fonology
# NOTE: Let's transcribe the words in cities
cities |>
  ipa(lg = "en")

# NOTE: Save transcriptions in a variable
phon <- cities |>
  ipa(lg = "en")

# NOTE: Now we have a new object with our transcriptions

# NOTE: Let's extract the stress
phon |>
  getStress()

# NOTE: How about getting the final syllables?
phon |>
  getSyl(pos = 1)

# NOTE: And the onsets of final syllables?
phon |>
  getSyl(pos = 1) |>
  syllable(const = "onset")

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

md

md_CV <- md |>
  separate_rows(cv, sep = "\\.")

md_CV |>
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
  ~input, ~output, ~onset, ~no_coda, ~no_complex_onset, ~no_complex_coda, ~obs,
  "input", "CV", 0, 0, 0, 0, 7596,
  "input", "CVC", 0, 1, 0, 0, 8018,
  "input", "VC", 1, 1, 0, 0, 2309,
  "input", "V", 1, 0, 0, 0, 1718,
  "input", "CCV", 0, 0, 1, 0, 1874,
  "input", "VCC", 0, 0, 1, 0, 477,
  "input", "CVCC", 0, 0, 1, 0, 2645,
  "input", "CCVCC", 0, 0, 1, 0, 585,
)

tableau_en |>
  maxent() |>
  _$weights


# NOTE: Portuguese
bc <- read_lines("data/braz_cubas.txt")

# NOTE: Tokenization
bc <- tibble(palavra = bc |> cleanText())

bc <- bc |>
  mutate(
    ipa = ipa(palavra),
    cv = ipa |> cv()
  ) |>
  separate_rows(cv, sep = "\\.")

bc |>
  distinct() |>
  filter(cv %in% c("CCV", "CV", "V", "VC", "CVC", "VCC", "CVCC", "CCVCC")) |>
  summarize(obs = n(), .by = cv) |>
  arrange(desc(obs)) |>
  mutate(prop = obs / sum(obs))
# # A tibble: 8 × 3
#   cv      obs     prop
#   <chr> <int>    <dbl>
# 1 CV    32073 0.460
# 2 CVC   16352 0.235
# 3 V     11249 0.161
# 4 VC     6033 0.0866
# 5 CCV    3602 0.0517
# 6 CVCC    239 0.00343
# 7 VCC     103 0.00148
# 8 CCVCC    45 0.000646

# NOTE: Now, the tableau

tableau_bc <- tribble(
  ~input, ~output, ~onset, ~no_coda, ~no_complex_onset, ~no_complex_coda, ~obs,
  "input", "CV", 0, 0, 0, 0, 32073,
  "input", "CVC", 0, 1, 0, 0, 16352,
  "input", "V", 1, 0, 0, 0, 11249,
  "input", "VC", 1, 1, 0, 0, 6033,
  "input", "CCV", 0, 0, 1, 0, 3602,
  "input", "CVCC", 0, 0, 0, 1, 239,
  "input", "VCC", 1, 0, 0, 1, 103,
  "input", "CCVCC", 0, 0, 1, 1, 45,
)

tableau_bc |>
  maxent() |>
  _$weights
#            onset          no_coda no_complex_onset  no_complex_coda
#        1.0293322        0.6599184        2.1771449        4.7977184
