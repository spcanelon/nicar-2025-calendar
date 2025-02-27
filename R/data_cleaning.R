library(dplyr)
library(stringr)
library(tidyr)
library(jsonlite)
library(rvest)

# import JSON
schedule_json <- stream_in(file("data/nicar-2025-schedule.json"))

# wrangle
room_flat <- flatten(schedule_json$room)
schedule <- schedule_json |> select(-c(room, recorded)) |> cbind(room_flat)

# extracting nested speaker info
speakers_info <-
  schedule  |>
  # select(session_id, speakers) |>
  unnest(cols = c(speakers)) |>
  # fix outlier
  mutate(first = str_replace(first, pattern = "Elizabeth Santos, UC Berkeley",
                             replacement = "Elizabeth")) |>
  group_by(session_id) |>
  mutate(full_name = str_c(first, last, sep = " "),
         speaker = glue::glue("{full_name} - {affiliation}")) |>
  summarize(speaker_list = list(speaker)) |>
  mutate(speaker_list = as.character(speaker_list),
         speaker_list_tidy = str_remove_all(speaker_list, pattern = "\""),
         speaker_list_tidy = str_remove_all(speaker_list_tidy, pattern = "^c\\(|\\)$")) |>
  select(-speaker_list) |>
  rename(speakers = speaker_list_tidy) |>
  unique()

# create variable for session location
schedule <-
  left_join(schedule |> select(-speakers), speakers_info) |>
  mutate(location = glue::glue("{room_name}, {level} floor")) |>
  mutate(recorded = if_else(recorded == TRUE, "Yes", "No")) |>
  select(-c(room_name, level))

# scrape session URLs from NICAR site
schedule_site <- read_html("https://schedules.ire.org/nicar-2025/")

links <- schedule_site |> html_elements(".share-input") |> html_attr("value") |> as_tibble()
links <- links |>
  rename(url = value) |>
  separate_wider_delim(url, delim = "#", names = c("url", "session_id"), cols_remove = FALSE)

# join URLs back to schedule
schedule <- schedule |> mutate(session_id = as.character(session_id)) |> left_join(links)

# change NA to "Not available/NA/-999/"
schedule <- schedule |>
  mutate(across(.cols = description,
                .fns = ~replace(., .=="", "Not available/NA/-999/404")))

# export schedule
schedule <- schedule |>
  rename(session_description = description,
         start_datetime = start_time,
         end_datetime = end_time) |>
  # add dummy variable for speaker bio
  mutate(bio_html = "") |>
  select(-c(canceled, evergreen, sponsor, audio_recording_link, tipsheets, os))
readr::write_csv(schedule, file = "data/schedule.csv")
