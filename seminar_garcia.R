# NOTE: Now load the packages and we're good to go
library(tidyverse)
library(tidytext)
library(Fonology)

# NOTE: Let's create some variables
# The arrow symbol assigns values to an object (variable)
numbers <- c(5, 10, 15)

# NOTE: Two ways to run a function
mean(numbers) # Typical
numbers |> mean() # With a pipe: |>

# NOTE: We can also have a variable with strings
cities <- c("Madrid", "Granada", "Rome", "Auckland")

# WARNING: Both numbers and cities are VECTORS
# Inside a vector, we can't mix classes: strings and numbers don't go together. Think of a vector as the column of a table: we don't mix different types of information in the same column.

cities[2]
length(cities)

# NOTE: With tidyverse, we have many functions for strings
str_length(cities)
str_to_upper(cities)
str_rank(cities)
str_sort(cities)

str_to_lower(cities)
cities |>
  str_extract(pattern = "[A-Z]")
cities |>
  str_sub(start = 1, end = 3) |>
  str_to_upper()

# NOTE: Now let's get into Fonology
# Starting with IPA transcription
cities |>
  ipa(lg = "it")

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

# NOTE: What if we want to the syllable SHAPE?
phon |>
  getSyl(pos = 1) |>
  cv()

# NOTE: And the onsets of final syllables?
phon |>
  getSyl(pos = 1) |>
  syllable(const = "onset")

# NOTE: We can now count how many onsets each word has in its final syllable
phon |>
  getSyl(pos = 1) |>
  syllable(const = "onset") |>
  str_length()

# NOTE: What if we have a sentence...?

sentence <- "This is a simple sentence in English!!"

# We can use the cleanText() function to clean and tokenize text
sentence |>
  cleanText() |>
  ipa(lg = "en")

# Now, we can do anything with our data: for example,
# let's extract syllable shapes from our sentence
tokens <- sentence |>
  cleanText() |>
  ipa(lg = "en") |>
  cv()

tokens |>
  str_split(pattern = "\\.") |>
  unlist() |>
  as_tibble() |>
  count(value, sort = T)

# NOTE: We can also create a simple figure:
tokens |>
  str_split(pattern = "\\.") |>
  unlist() |>
  as_tibble() |>
  count(value, sort = T) |>
  ggplot(aes(x = value, y = n)) +
  geom_col()
