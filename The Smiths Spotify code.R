# Install spotifyr from GitHub
devtools::install_github('charlie86/spotifyr')

# Load Libraries
library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(viridis)
library(corrplot)
library(ggjoy)

# Client ID
Sys.setenv(SPOTIFY_CLIENT_ID = '3d597be369484670abbd4d6376d42c9b')
# Client Secret
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f3f8235f278140459ca6c2295bacd41d') 

# Spotify access token
access_token <- get_spotify_access_token()

# Read the data
the_smiths <- get_artist_audio_features('the smiths')

the_smiths %>%
  count(key_mode, sort = TRUE) %>%
  head(10) %>%
  kable()

the_smiths %>% 
  arrange(valence) %>%
  select(track_name,valence) %>%
  head(10) %>%
  kable()

# Duration - Top 10 songs 
the_smiths %>%
  mutate(duration_min=round(duration_ms/60000, digits=2)) %>%
  arrange(duration_ms) %>%
  select(track_name, duration_min) %>%
  tail(10) %>%
  kable()

# Most common keys
the_smiths %>% 
  count(key_mode, sort=TRUE) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(key_mode, -n), y=n, fill=key_mode)) +
  geom_bar(stat = "identity", show.legend=FALSE) +
  geom_text(aes(label=n), position=position_stack(vjust=0.8)) +
  labs(x="Keys", y="Count",
       title="The Smiths discography",
       subtitle="Most common keys") +
  theme_bw()

# Density plots 
the_smiths %>%
  gather(features, values, c(9,10, 15,16, 17, 18)) %>%
  ggplot(aes(x=values, fill=features)) +
  geom_density(alpha=0.5, show.legend=FALSE) +
  facet_wrap(~features) +
  labs(y="Density",
       title="The Smiths discography",
       subtitle="Audio features") +
  theme(axis.title.x=element_blank()) +
  theme_bw()

# Boxplots  
the_smiths %>%
  gather(features, values, c(9,10, 15,16, 17, 18)) %>%
  ggplot(aes(x=reorder(features, values, FUN=median), y=values, fill=features)) +
  geom_boxplot(show.legend=FALSE) +
  labs(x="Audio feature", y="Density",
       title="The Smiths discography",
       subtitle="Audio features") +
  theme_bw() 

# Ridgeline plots
the_smiths %>%
  mutate(album_name_year=paste0(album_name, " (", substring(album_release_date, 1, 4), ")")) %>%
  gather(features, values, c(9, 10, 15)) %>%
  ggplot(aes(x=values, y=album_name_year, fill=..x..)) + 
  geom_density_ridges_gradient(scale=2, rel_min_height=0.01, gradient_lwd=1., show.legend=FALSE) +
  scale_fill_viridis(option="B") +
  facet_wrap(~features) +
  labs(title="The Smiths discography",
       subtitle="Audio features") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

# Ridgeline plots
the_smiths %>%
  mutate(album_name_year=paste0(album_name, " (", substring(album_release_date, 1, 4), ")")) %>%
  gather(features, values, c(16, 17, 18)) %>%
  ggplot(aes(x=values, y=album_name_year, fill=..x..)) + 
  geom_density_ridges_gradient(scale=2, rel_min_height=0.01, gradient_lwd=1., show.legend=FALSE) +
  scale_fill_viridis(option="D") +
  facet_wrap(~features) +
  labs(title="The Smiths discography",
       subtitle="Audio features") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Average values 
radarchart_data <- the_smiths %>%
  group_by(album_name) %>%
  summarise(Danceability=mean(danceability),
            Energy=mean(energy),
            Speechiness=mean(speechiness),
            Acousticness=mean(acousticness),
            Instrumentalness=mean(instrumentalness),
            Liveness=mean(liveness),
            Valence=mean(valence)) %>%
  gather(features, values, 2:8)

# Album names abbreviations 
radarchart_data <- radarchart_data %>%
  mutate(album_name=recode(album_name, `The Queen Is Dead (Deluxe Edition)`="The Queen Is Dead")) 

# Radarchart (first eight albums)
ggplot(radarchart_data %>% filter(album_name==c("The Smiths","Meat Is Murder","The Queen Is Dead","Strangeways, Here We Come")),
       aes(x=features, y=values, col=album_name, group=album_name)) +
  geom_polygon(fill=NA, size=1.5, show.legend=FALSE) +
  geom_point(size=2, col="black") +
  coord_polar() +
  facet_wrap(~album_name, nrow=2) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        text=element_text(size=8)) 

# Correlation matrix 
corrplot(cor(the_smiths[,c(9,10,15,16,17,18)]), 
         type="upper", method="ellipse", tl.cex=0.9)



ggplot(the_smiths, aes(x= valence, y= album_name)) +
  geom_joy() +
  theme_joy()

the_smiths %>%
  group_by(album_name) %>%
  filter(!album_name %in% c("Live in England","The Queen Is Dead (Deluxe Edition)","Rank")) %>%
  ggplot(aes(x=valence,y=album_name,fill=..x..)) +
  geom_density_ridges_gradient()

the_smiths %>%
  filter(!album_name %in% c("Live in England","The Queen Is Dead (Deluxe Edition)","Rank")) %>%
  group_by(album_name) %>%
  summarise(mean(valence)) %>%
  arrange(desc(`mean(valence)`)) %>%
  kable()

#lyrics
library(genius)
tqid <- genius_album(artist = "The Smiths", album = "The Queen Is Dead")
library(tidytext)
library(textdata)
view(tqid)

tqid %>%
  unnest_tokens(word,lyric) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) -> tqid_words

tqid_words %>%
  count(word,sentiment,sort=TRUE) %>%
  filter(n>1) %>%
  ggplot(aes(reorder(word,n),n,fill=sentiment)) +
  geom_col() + coord_flip() +
  facet_wrap(~sentiment,scales="free_y")

tqid_words %>%
  count(word,sentiment,sort=TRUE) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(reorder(word,n),n,fill=sentiment)) +
  geom_col() + coord_flip()

tqid_words %>%
  count(word,sentiment,sort=TRUE) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x=n,y=as.factor(sentiment))) +
  geom_density_ridges()

the_smiths %>% group_by(album_name) %>%
  filter(album_name %in% "The Queen Is Dead") %>%
  ggplot(aes(x=valence,y=album_name,fill=..x..)) +
  geom_density_ridges_gradient()

the_smiths %>%
  filter(!album_name %in% c("Live in England","The Queen Is Dead (Deluxe Edition)","Rank")) %>%
  count(key_mode,sort=TRUE) %>%
  ggplot(aes(reorder(key_mode,n),n)) +
  geom_col() + coord_flip()

the_smiths %>%
  filter(!album_name %in% c("Live in England","The Queen Is Dead (Deluxe Edition)","Rank")) %>%
  count(mode_name,sort=TRUE) %>%
  ggplot(aes(reorder(mode_name,n),n)) +
  geom_col() + coord_flip()

correlated_density <- ggplot(the_smiths) +
  geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
  geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
  geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
  scale_x_continuous(name = "Energy, Valence and Danceability") +
  scale_y_continuous(name = "Density") +
  ggtitle("Density plot of Energy, Valence and Danceability") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12)) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Accent")

correlated_density




