# Carregando arquivo TED Talks
ted <- read_csv("/cloud/project/aula-05/data/ted_main.csv.gz") %>%
  mutate( duration  = duration(duration, units = "seconds")
          , film_date = as_datetime(film_date) %>% as_date()
          , published_date = as_datetime(published_date)) %>%
  mutate( event = factor(event)
          , speaker_occupation = factor(speaker_occupation)) %>%
  select(title, views, comments, duration:main_speaker, num_speaker:published_date, speaker_occupation)

# Gerando o Histograma
ted %>%
  mutate( year = year( published_date )) %>%
  filter(views < mean(views) + (2 * sd(views))) %>%
  ggplot( aes( x = views )) +
  geom_bar( alpha = 0.6) +
  facet_wrap(~ year) +
  scale_x_continuous(labels = scales::format_format(scientific = FALSE)) +
  geom_histogram(bins = 15)