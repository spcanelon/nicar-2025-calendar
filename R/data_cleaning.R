library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(jsonlite)
library(rvest)

# import JSON
schedule_json <- stream_in(file("data/nicar-2025-schedule.json"))

# wrangle
room_flat <- flatten(schedule_json$room)
schedule <- schedule_json |> select(-c(room, recorded)) |> cbind(room_flat)

# function to clean lists
clean_list <- function(x) {
  
  x <- as.character(x)
  x <- str_remove_all(x, pattern = "\"")
  x <<- str_remove_all(x, pattern = "^c\\(|\\)$")
}

# extracting nested speaker info
speakers_info <-
  schedule  |>
  # select(session_id, speakers) |>
  unnest(cols = c(speakers)) |>
  # fix outlier
  mutate(first = str_replace(first, pattern = "Elizabeth Santos, UC Berkeley",
                             replacement = "Elizabeth")) |>
  group_by(session_id) |>
  mutate(speaker_name = str_c(first, last, sep = " "),
         speaker = glue::glue("{speaker_name} - {affiliation}")) |>
  reframe(speaker_name,
          affiliation,
          speaker,
          speaker_name_list = list(speaker_name),
          speaker_list = list(speaker)) |>
  mutate(speaker_name_list_tidy = clean_list(speaker_name_list),
         speaker_list_tidy = clean_list(speaker_list)) |> 
  rename(speaker_names = speaker_name_list_tidy,
         speakers = speaker_list_tidy) |>
  select(c(session_id, speaker_name, affiliation, speaker, speaker_names, speakers)) |>
  unique()

# create variable for session location
schedule <-
  left_join(schedule |> select(-speakers), 
            speakers_info |> select(-c(speaker_name:speaker)) |> unique()) |>
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

# dealing with NAs and empty cells
schedule <- schedule |>
  mutate(description = replace(description, description == "", "Not available/NA/-999/404"),
         skill_level = replace(skill_level, skill_level == "", NA))

# wrangling factors
type_list <- c("Networking", "Panel", "Demo", "Hands-on", "Commons", "Special", "Pre-registration - Hands-on")
skill_list <- c("Beginner", "Intermediate", "Advanced")

schedule <- schedule |> 
  mutate(session_type = factor(
    session_type,
    levels = type_list, 
    labels = type_list)) |> 
  mutate(skill_level = factor(
    skill_level,
    levels = skill_list,
    labels = skill_list
  ))

# export schedule
schedule <- schedule |>
  rename(session_description = description) |>
  # add dummy variable for speaker bio
  mutate(start_datetime = as_datetime(start_time),
         end_datetime = as_datetime(end_time),
         bio_html = "") |>
  select(-c(canceled, evergreen, sponsor, audio_recording_link, tipsheets, os, start_time, end_time)) |> 
  unique()

readr::write_rds(schedule, file = "data/schedule.rds")
readr::write_rds(speakers_info, file = "data/speakers_info.rds")
