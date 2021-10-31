library(tidyverse)

tasters <- c(
  "kath",
  "kate",
  "gaby",
  "jon",
  "emily",
  "will",
  "rage",
  "mickey",
  "chris",
  "christian",
  "alex",
  "lauren"
)

ratings_raw <- "
122222313221
111323253544
253313431123
154224334133
121113212143
242222142334
142445231112
131111112122
121314112111
112114111133
141?23134324
111212133545
141111111111
353445311121
232444111121
154333433455
"

fruit_snack_types_in_consumption_order <- c(
  "Annie's Sunny Citrus",
  "Betty Crocker Spiderman",
  "Welch's Island Fruits",
  "Welch's Berries & Cherries",
  "Welch's Superfruit",
  "Betty Crocker Scooby-Doo",
  "Annies Bunny Tropical Treats",
  "Betty Crocker Starwars",
  "Motts Tropical",
  "Fruit Shoppe Sharks",
  "Funables Mixed Berry",
  "Motts Animals",
  "Betty Crocker Trolls",
  "Annie's Bunny Summer Strawberry",
  "Annie's Bunny Berry Patch",
  "Welch's Mixed Fruit"
)

ratings_raw %>% 
  str_trim() %>% 
  str_split("\n") %>% 
  pluck(1) %>% 
  str_split("") %>% 
  map(set_names, tasters) %>% 
  bind_rows(.id = "consumption_order") %>% 
  mutate_all(na_if, "?") %>% 
  mutate_all(as.integer) %>% 
  mutate(
    fruit_snack_type = fruit_snack_types_in_consumption_order
  ) %>% 
  write_csv(
    here::here("data/ratings.csv")
  )

