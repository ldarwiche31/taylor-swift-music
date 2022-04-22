# Getting the dataset -----------------------------------------------------

# load packages
library(tidyverse)
library(spotifyr)
library(geniusr)

# set seed
set.seed(274)

# set credentials - Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = '1bff3ae8f2bf478cb5e5be684a4d4084')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'ed310531d5cf446cab74c55cc3947ef1')
spotify_access_token <- get_spotify_access_token()

# set credentials - Genius API
Sys.setenv(GENIUS_API_TOKEN = 'H7PLJNHT_r-rMdutn7VYdzJ2TrcscA_j6pCnBexZAx627bNwFtNW24DwOR4yedto')
genius_access_token <- genius_token()

# dataset - Spotify -------------------------------------------------------

# will get all of our information via a playlist I created with these

output <- spotifyr::get_playlist_tracks("2NSGY6cPW4xdvnBAGyoPf2")

all_albums <- bind_rows(output)
names(all_albums) <- str_replace_all(names(all_albums), "\\.", "_")

# hard to work with lists in a dataframe so I will extract, manipulate, then reinsert
markets_count <- unlist(map(all_albums$track_available_markets, length))

all_albums <- all_albums %>%
  mutate(available_markets = markets_count) %>%
  mutate(available_markets = case_when(available_markets == 0 ~ NA_integer_,
                                       TRUE ~ available_markets)) %>%
  filter(track_is_local == FALSE) %>% # local songs do not have the same stored information
  as_tibble() %>%
  unnest(track_artists, keep_empty = TRUE, names_sep = "_") %>%
  select(track_id, track_name, track_artists_name, track_artists_id,
         track_duration_ms, track_explicit, track_popularity, available_markets,
         track_album_album_type, track_album_id, track_album_name, track_album_release_date,
         track_album_total_tracks, track_track_number) %>%
  rename(album_type = track_album_album_type)

# track is repetitive at the beginning... lets get rid of it
names(all_albums) <- str_replace_all(names(all_albums), "^track_", "")

# set up artist info
all_albums <- all_albums %>%
  group_by(id) %>%
  mutate(artist_rank = row_number()) %>%
  mutate(artist_rank = case_when(artist_rank == 1 ~ "main",
                                 TRUE ~ str_c("feature_", artist_rank - 1))
  ) %>%
  pivot_wider(names_from = artist_rank, values_from = c(artists_name, artists_id)) %>%
  ungroup()

# add additional data sources
audio_features <- map(all_albums$id, get_track_audio_features)
artist_info <- map(unique(all_albums$artists_id_main), spotifyr::get_artist)
bind_rows(audio_features)
artists_id <- sapply(artist_info, "[[", "id")
popularity <- sapply(artist_info, "[[", "popularity")
followers <- lapply(artist_info, "[[", "followers")
followers <- sapply(followers, "[[", "total")
genres <- lapply(artist_info, "[[", "genres")
artist_info_table <- as_tibble(list(artists_id = artists_id, artists_popularity = popularity, artists_followers = followers, genres = genres))
audio_features_table <- bind_rows(audio_features)
all_albums <- all_albums %>%
  left_join(audio_features_table %>% select(danceability:id, time_signature), by = "id") %>%
  left_join(artist_info_table, by = c("artists_id_main" = "artists_id"))

# creating the subsets
fearless <- subset(all_albums, album_id == "43OpbkiiIxJO8ktIB777Nn")
red <- subset(all_albums, album_id == "1KlU96Hw9nlvqpBPlSqcTV")
red_re <- subset(all_albums, album_id == "6kZ42qRrzov54LcAk4onW9")
fearless_re <- subset(all_albums, album_id == "4hDok0OAJd57SGIT8xuWJH")

# dataset - Genius --------------------------------------------------------

# getting fearless lyrics



