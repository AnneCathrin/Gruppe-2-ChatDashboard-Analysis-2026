################################################
##
##
##    Script to visualize WhatsApp-Chatlogs
##
##
#################################################

# install packages if necessary!
#install.packages("ggplot2")
#...

library(ggplot2)
library(ragg)
library(lubridate)
library(slider)
library(ragg)
library(tidyverse)
library(WhatsR)

source(file.path("Scripts", "00_Helpers.R")) ## lets again load our helpers, we might need them!

save_dir <- "Data_cleaned"
plot_dir <- "Plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

simulated_chat <- WhatsR::parse_chat(file.path("Data", "Simulated_WhatsR_chatlog.txt")) ## lets load a simulated chat example.


## load all cleaned chats
files <- list.files(save_dir, pattern="\\.rds$")

chatlogs <- list()

for (file in files) {
  chat <- readRDS(file.path(save_dir, file))
  name <- tools::file_path_sans_ext(file)
  
  chatlogs[[name]] <- chat ## now we have a list of all our chatlogs!
}

#View(chatlogs)


## All chatlogs are contained in the list "chatlogs"
# extract them using [[filename_without_extension]]
# mylog <- chatlogs[["example_chat"]]



#### Visualizations

# First, let's start with the in-built plots from WhatsR

example_chat <- chatlogs[["Teilnehmer_1_2026-05-07_20-20-28_1883dad6"]]

## to plot emojis, I implemented my own fix since the package has some issues with emoji rendering. 
## Do NOT use WhatsR::plot_emoji, use the function in the repo instead. By default, it is already masked.
## Just run "plot_emoji", but not WhatsR::plot_emoji
## The function supports several plot types, we will iterate through them because lazy code is good code

plot_types <- c("heatmap", "cumsum", "bar", "splitbar") # always check the documentation! Press F1 while the cursor is within a function name to open it! or write ?function in the CLI.

for(plot_type in plot_types) {
  
  myplot <- plot_emoji(example_chat,
                       min_occur = 10,
                       plot = plot_type
  )
  
  ## whatsR uses ggplot2's system, therefore we can save the output the usual way
  print(myplot)
  ggsave(file.path(plot_dir, paste0("emoji_plot_",plot_type,".png")),
         device = ragg::agg_png,
         bg = "white"
         ) ## we use agg rendering because emoji can be tricky.
  
}


## there are many plots available in whatsR, explore them using autocomplete with WhatsR::
## However, keep in mind that much of the data is not available in our exports (location, media, messages/tokens...)
## you can directly read your own whatsapp chat exports in R if you want to experience the full functionality, or use the simulated chat

WhatsR::plot_links(example_chat)

WhatsR::plot_tokens(simulated_chat, exclude_sm = TRUE) ## we can only plot tokens for the simulated chat since our exports do not contain the whole text.


##### now we will look into creating our own visualizations!

## Firstly, all data needs to be tabular to be understood by ggplot. 
## Our chat-exports may seem tabular, but many entries are in fact *NESTED*, 
## that is, the data frame contains lists inside its elements, e.g. for emojis.
## We will need to deal with this on a case-by-case-basis

## one handy feature from tidyverse is "unnest_longer()"

## emojis


emoji_unnested <- example_chat %>%
  select(Emoji) %>%
  unnest_longer(Emoji) %>%
  filter(!is.na(Emoji))


emoji_counts <- example_chat %>%
  select(Emoji) %>%
  unnest_longer(Emoji) %>%
  filter(!is.na(Emoji)) %>%
  count(Emoji, sort = TRUE)

emoji_counts



### with tabular (and in this case aggregated) data, we can now move on to visualize!



#### GGPlot2 is the definitive package for R visualization. For further info see https://rstudio.github.io/cheatsheets/html/data-visualization.html 

# Every GGPlot is created in the following way:

# ggplot(data = <Data>) +
#   <Geom_Function>(mapping = aes(<Mappings>),
#                   stat = <Stat>,
#                   position = <Position>) +
#   <Coordinate_Function> +
#   <Facet_Function> +
#   <Scale_Function> +
#   <Theme_Function>

# The core functionality is using "+" to add features/visual details/labels etc. to a graph


emoji_counts %>%
  slice_max(n, n = 20) %>%   # Top 20 Emojis
  ggplot(aes(x = reorder(Emoji, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Häufigste Emojis",
    x = NULL,
    y = "Anzahl"
  ) +
  theme_minimal(base_size = 14, base_family = "Segoe UI Emoji")


## uh oh, the emoji display is broken! But there's a workaround (don't ask)



agg_png(
  file.path(plot_dir, "emoji_plot_bar_fixed.png"),
  width = 1200,
  height = 800,
  res = 144
)

emoji_counts %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(Emoji, n), y = n)) +
  geom_col() +
  labs(
    title = "Häufigste Emojis",
    x = "Emoji",
    y = "N"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "emoji")
  )

dev.off()



### lets do the same for links now, as an exercise!

links_unnested <- example_chat %>%
  select(DateTime, URL) %>%
  unnest_longer(URL) %>%
  filter(!is.na(URL))

## now we have one row for each link in the dataset. We can now move on to cumulate them or visualize them otherwise

## basic barplot 

# Count how often each domain appears
domain_counts <- links_unnested %>%
  count(URL, sort = TRUE)

# Plot the 10 most common domains
domain_counts %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(URL, n), y = n)) +
  
  # Create bars
  geom_col() +
  
  # Flip axes so labels are easier to read
  coord_flip() +
  
  # Add labels
  labs(
    title = "Most Shared Domains",
    x = "Domain",
    y = "Number of Shared Links"
  ) +
  
  # Use a clean theme
  theme_minimal()


#### links over time

# Group timestamps into calendar weeks
links_per_week <- links_unnested %>%
  mutate(week = floor_date(DateTime, unit = "week")) %>%
  count(week)

# Plot activity over time
ggplot(links_per_week, aes(x = week, y = n)) +
  
  # Draw a line
  geom_line() +
  
  # Add labels
  labs(
    title = "Shared Links Over Time",
    x = "Week",
    y = "Number of Shared Links"
  ) +
  
  # Clean visual style
  theme_minimal()



#### 

#### A lot of things are possible with ggplot. Do not underestimate the power of a good data viz!
## See this advanced example for instance
## EMOJI X TIME rolling average


plot_emoji_x_time <- function(chatlog, num_emoji = 5, plotname = "placeholder", start_date = as.POSIXct("2010-01-01")) {
  chatlog <- chatlog %>%
    filter(DateTime >= start_date)
  
  
  
  # Unnest emoji column
  emoji_time <- chatlog %>%
    select(DateTime, Emoji) %>%
    unnest_longer(Emoji) %>%
    filter(!is.na(Emoji))
  
  # Find top 10 emojis overall
  top_emojis <- emoji_time %>%
    count(Emoji, sort = TRUE) %>%
    slice_head(n = num_emoji) %>%
    pull(Emoji)
  
  # Count emoji usage per month
  emoji_monthly <- emoji_time %>%
    
    # Keep only top emojis
    filter(Emoji %in% top_emojis) %>%
    
    # Convert timestamps to months
    mutate(month = floor_date(DateTime, "month")) %>%
    
    # Count uses per emoji per month
    count(month, Emoji)
  
  # Compute rolling 3-month average
  emoji_rolling <- emoji_monthly %>%
    
    arrange(Emoji, month) %>%
    
    group_by(Emoji) %>%
    
    mutate(
      rolling_avg = slide_dbl(
        n,
        mean,
        .before = 2,
        .complete = FALSE
      )
    )
  
  # Plot rolling averages
  agg_png(
    file.path(plot_dir, paste0(plotname, ".png")),
    width = 2000,
    height = 1400,
    res = 144
  )
  
  
  ggplot(
    emoji_rolling,
    aes(
      x = month,
      y = rolling_avg,
      color = Emoji
    )
  ) +
    
    # Draw lines
    geom_line(linewidth = 1.2) +
    
    # Labels
    labs(
      title = "Emoji Usage Over Time",
      subtitle = "3-Month Rolling Average",
      x = "Time",
      y = "Average Uses per Month",
      color = "Emoji"
    ) +
    
    # Minimal theme
    theme_minimal(base_size = 14) +
    
    # Larger emoji legend
    theme(
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 14)
    )
  ggsave(
    file.path(plot_dir, paste0(plotname, ".png")),
    device = ragg::agg_png,
    width = 2000,
    height = 1400,
    dpi = 144,
    units = "px",
    bg = "white"
  )
  
}

plot_emoji_x_time(example_chat, 5, "emoji_x_time_plot", as.POSIXct("2020-01-01"))



##################################





### lets write it as a function so that we can easily re-use it!

plot_relative_emoji <- function(chatlog, num_emoji = 5, plotname = "placeholder", start_date = as.POSIXct("2010-01-01")) {
  

  chatlog <- chatlog %>%
    filter(DateTime >= start_date)
  
  # ----------------------------
  # Prepare emoji time data
  # ----------------------------
  
  # Convert list-column into one row per emoji
  emoji_time <- chatlog %>%
    select(DateTime, Emoji) %>%
    unnest_longer(Emoji) %>%
    filter(!is.na(Emoji))
  
  # Find most frequently used emojis overall
  top_emojis <- emoji_time %>%
    count(Emoji, sort = TRUE) %>%
    slice_head(n = num_emoji) %>%
    pull(Emoji)
  
  # ----------------------------
  # Count ALL emoji usage per month
  # ----------------------------
  
  all_emoji_per_month <- emoji_time %>%
    mutate(month = floor_date(DateTime, "month")) %>%
    count(month, name = "total_emoji")
  
  # ----------------------------
  # Count top emoji usage per month
  # and normalize by ALL emoji activity
  # ----------------------------
  
  emoji_monthly <- emoji_time %>%
    
    # Keep only top emojis
    filter(Emoji %in% top_emojis) %>%
    
    # Convert timestamps to months
    mutate(month = floor_date(DateTime, "month")) %>%
    
    # Count emoji usage
    count(month, Emoji, name = "emoji_count") %>%
    
    # Join total emoji activity
    left_join(all_emoji_per_month, by = "month") %>%
    
    # Relative share of all emojis
    mutate(
      emoji_share = emoji_count / total_emoji
    )
  
  # ----------------------------
  # Compute rolling average
  # ----------------------------
  
  emoji_rolling <- emoji_monthly %>%
    
    arrange(Emoji, month) %>%
    
    group_by(Emoji) %>%
    
    mutate(
      rolling_avg = slide_dbl(
        emoji_share,
        mean,
        .before = 2,
        .complete = FALSE
      )
    )
  

  # ----------------------------
  # Plot
  # ----------------------------
  
  ggplot(
    emoji_rolling,
    aes(
      x = month,
      y = rolling_avg,
      color = Emoji
    )
  ) +
    
    # Draw lines
    geom_line(linewidth = 1.4) +
    
    # Labels
    labs(
      title = "Relative Emoji Popularity Over Time",
      subtitle = "3-Month Rolling Average",
      x = "Time",
      y = "Share of All Emoji Usage",
      color = "Emoji"
    ) +
    
    # Clean theme
    theme_minimal(base_size = 16) +
    
    # White background + larger legend text
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16)
    )
  
  ggsave(
    file.path(plot_dir, paste0(plotname, ".png")),
    device = ragg::agg_png,
    width = 2000,
    height = 1400,
    dpi = 144,
    units = "px",
    bg = "white"
  )
  
}

plot_relative_emoji(example_chat, 5, "rel_emoji", as.POSIXct("2020-01-01"))
#

###############################################################################

# 1. Das Suchmuster definieren (falls noch nicht geschehen)
emoji_regex <- "[\\x{1F300}-\\x{1F6FF}\\x{1F900}-\\x{1F9FF}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]"

# 2. Daten verarbeiten
emoji_counts_mit_person <- chat %>%  # <- Hier den Namen deiner Tabelle eintragen
  # JETZT KORRIGIERT: Erst die Nachrichtenspalte, dann die Regex-Formel
  mutate(Emoji = str_extract_all(Emoji, emoji_regex)) %>% 
  unnest(Emoji) %>%
  # Hier zählen wir nach Sender und Emoji
  count(Sender, Emoji, name = "n")

# Wir nutzen nun den neuen Datensatz mit der Personen-Spalte
ggplot(data = emoji_counts_mit_person, aes(x = Sender, y = n, fill = Sender)) +
  
  # Erstellt die Balken und summiert die Emojis pro Person automatisch auf
  geom_col(show.legend = FALSE, width = 0.5) +
  
  # Schöne Farben für Person_1 und Person_2
  scale_fill_brewer(palette = "Set1") + 
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Gesamtanzahl der gesendeten Emojis pro Person",
    subtitle = "Vergleich zwischen Person 1 und Person 2",
    x = "Person",
    y = "Gesamtanzahl Emojis"
  ) +
  
  # Schreibt die exakte Gesamtzahl über die Balken
  stat_summary(
    fun = sum, 
    aes(label = after_stat(y)), 
    geom = "text", 
    vjust = -0.5, 
    fontface = "bold"
  )

# 1. Suchmuster für Emojis definieren
emoji_regex <- "[\\x{1F300}-\\x{1F6FF}\\x{1F900}-\\x{1F9FF}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]"

# --- ZWEITER DATENSATZ ---
# 2. RDS-Datei einlesen
raw_data_2 <- readRDS("Teilnehmer_1_2026-05-07_20-22-52_40ccf5e8.rds") 

# 3. Emojis extrahieren und zählen
emoji_counts_mit_person_2 <- raw_data_2 %>% 
  mutate(Emoji = str_extract_all(Emoji, emoji_regex)) %>% 
  unnest(Emoji) %>%
  count(Sender, Emoji, name = "n")


# --- DRITTER DATENSATZ ---
# 4. RDS-Datei einlesen
raw_data_3 <- readRDS("Teilnehmer_1_2026-05-07_20-25-04_d0f4f621.rds") 

# 5. Emojis extrahieren und zählen
emoji_counts_mit_person_3 <- raw_data_3 %>% 
  mutate(Emoji = str_extract_all(Emoji, emoji_regex)) %>% 
  unnest(Emoji) %>%
  count(Sender, Emoji, name = "n")

# Zusammenführen und benennen, woher die Daten stammen
kombinierte_daten <- bind_rows(
  "Chat 1" = emoji_counts_mit_person,
  "Chat 2" = emoji_counts_mit_person_2,
  "Chat 3" = emoji_counts_mit_person_3,
  .id = "Quelle"
)

ggplot(data = kombinierte_daten, aes(x = Sender, y = n, fill = Quelle)) +
  
  # Nebeneinanderstehende Balken
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Farbschema ("Set2" hat schöne, dezente Farben für 3 Gruppen)
  scale_fill_brewer(palette = "Set2") + 
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Gesamtanzahl der gesendeten Emojis pro Person",
    subtitle = "Vergleich über drei verschiedene Datensätze",
    x = "Person",
    y = "Gesamtanzahl Emojis",
    fill = "Datenquelle"
  ) +
  
  # Exakte Zahlen über die Balken schreiben
  stat_summary(
    fun = sum,
    aes(label = after_stat(y)),
    geom = "text",
    position = position_dodge(width = 0.8), 
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# 1. Daten präzise für das Diagramm vorbereiten (Vorab aufsummieren)
plot_daten <- kombinierte_daten %>%
  group_by(Quelle, Sender) %>%
  summarise(Gesamtanzahl = sum(n), .groups = "drop")

# 2. Das korrigierte Diagramm zeichnen
ggplot(data = plot_daten, aes(x = Sender, y = Gesamtanzahl, fill = Quelle)) +
  
  # Balken nebeneinander zeichnen
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Das Label direkt aus der berechneten Spalte holen (geom_text statt stat_summary)
  geom_text(
    aes(label = Gesamtanzahl),
    position = position_dodge(width = 0.8),
    vjust = -0.5,           # Leicht über dem Balken positionieren
    fontface = "bold",
    size = 4
  ) +
  
  # Styling & Beschriftung (wie gehabt)
  scale_fill_brewer(palette = "Set2") + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Gesamtanzahl der gesendeten Emojis pro Person",
    subtitle = "Vergleich über drei verschiedene Datensätze",
    x = "Person",
    y = "Gesamtanzahl Emojis",
    fill = "Datenquelle"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# 1. Daten wie vorhin vorbereiten
plot_daten <- kombinierte_daten %>%
  group_by(Quelle, Sender) %>%
  summarise(Gesamtanzahl = sum(n), .groups = "drop")

# 2. Das neu strukturierte Diagramm zeichnen
# HIER GEÄNDERT: x ist jetzt 'Quelle', fill ist 'Sender'
ggplot(data = plot_daten, aes(x = Quelle, y = Gesamtanzahl, fill = Sender)) +
  
  # Zeichnet die Balken für Person_1 und Person_2 direkt nebeneinander
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Platziert die Zahlen exakt über den jeweiligen Balken des Paares
  geom_text(
    aes(label = Gesamtanzahl),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  
  # Zwei gut unterscheidbare Farben für die beiden Personen
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Gesamtanzahl der gesendeten Emojis im Chatvergleich",
    subtitle = "Direkter Vergleich zwischen Person 1 und Person 2 pro Datensatz",
    x = "Datenquelle (Chat)",
    y = "Gesamtanzahl Emojis",
    fill = "Person"
  ) +
  
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank() # Entfernt vertikale Linien für mehr Übersicht
  )

#############################################################################

library(tidyverse)

# --- CHAT 1 ---
chat_daten_1 <- readRDS("Teilnehmer_1_2026-05-07_20-20-28_1883dad6.rds")

msg_counts_1 <- chat_daten_1 %>% 
  # HIER WIRD GEFILTERT: Entfernt leere Zeilen und System-Absender
  filter(!is.na(Sender), Sender != "", Sender != "WhatsApp System Message", Sender != "System") %>% 
  count(Sender, name = "Anzahl_Nachrichten")


# --- CHAT 2 ---
chat_daten_2 <- readRDS("Teilnehmer_1_2026-05-07_20-22-52_40ccf5e8.rds")

msg_counts_2 <- chat_daten_2 %>% 
  filter(!is.na(Sender), Sender != "", Sender != "WhatsApp System Message", Sender != "System") %>% 
  count(Sender, name = "Anzahl_Nachrichten")


# --- CHAT 3 ---
chat_daten_3 <- readRDS("Teilnehmer_1_2026-05-07_20-25-04_d0f4f621.rds") 

msg_counts_3 <- chat_daten_3 %>% 
  filter(!is.na(Sender), Sender != "", Sender != "WhatsApp System Message", Sender != "System") %>% 
  count(Sender, name = "Anzahl_Nachrichten")

# Zusammenführen der sauberen Daten
kombinierte_nachrichten_sauber <- bind_rows(
  "Chat 1" = msg_counts_1,
  "Chat 2" = msg_counts_2,
  "Chat 3" = msg_counts_3,
  .id = "Quelle"
)

# Diagramm zeichnen
library(ggplot2)

ggplot(data = kombinierte_nachrichten_sauber, aes(x = Quelle, y = Anzahl_Nachrichten, fill = Sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  geom_text(
    aes(label = Anzahl_Nachrichten),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    fontface = "bold",
    size = 4
  ) +
  
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(
    title = "Gesamtanzahl der gesendeten Nachrichten im Chatvergleich",
    subtitle = "Bereinigte Daten (ohne WhatsApp-Systemnachrichten)",
    x = "Datenquelle (Chat)",
    y = "Anzahl geschriebener Nachrichten",
    fill = "Person"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

###############################################################################

library(tidyverse)
library(lubridate)

berechne_reaktionszeit <- function(rds_pfad) {
  readRDS(rds_pfad) %>%
    # 1. System-Nachrichten direkt ausschließen
    filter(!is.na(Sender), Sender != "", Sender != "WhatsApp System Message", Sender != "System") %>%
    # 2. Chronologisch sortieren (Wichtig: Nutze deine echte Zeit-Spalte statt 'Timestamp')
    arrange(DateTime) %>%
    # 3. Den vorherigen Sender und die vorherige Zeit ermitteln
    mutate(
      Vorheriger_Sender = lag(Sender),
      Vorherige_Zeit = lag(DateTime)
    ) %>%
    # 4. Filter: Nur Zeilen behalten, bei denen der Sender GEWECHSELT hat
    filter(Sender != Vorheriger_Sender) %>%
    # 5. Zeitdifferenz in Minuten berechnen
    mutate(
      Reaktionszeit_Min = as.numeric(difftime(DateTime, Vorherige_Zeit, units = "mins")),
      # Datum extrahieren für die X-Achse (z.B. auf Tage gerundet)
      Datum = as.Date(DateTime)
    ) %>%
    # 6. Ausreißer begrenzen (z.B. Antworten, die länger als 3 Stunden/180 Min gedauert haben, ignorieren)
    filter(Reaktionszeit_Min <= 180) %>%
    # 7. Daten aggregieren: Durchschnittliche Antwortzeit pro Tag und Person berechnen
    group_by(Datum, Sender) %>%
    summarise(Avg_Reaktionszeit = mean(Reaktionszeit_Min, na.rm = TRUE), .groups = "drop")
}

# Berechnen für alle drei Chats
chat_1_bereinigt <- berechne_reaktionszeit("Teilnehmer_1_2026-05-07_20-20-28_1883dad6.rds")
chat_2_bereinigt <- berechne_reaktionszeit("Teilnehmer_1_2026-05-07_20-22-52_40ccf5e8.rds")
chat_3_bereinigt <- berechne_reaktionszeit("Teilnehmer_1_2026-05-07_20-25-04_d0f4f621.rds")

# Zusammenfügen
kombinierte_reaktionszeit <- bind_rows(
  "Chat 1" = chat_1_bereinigt,
  "Chat 2" = chat_2_bereinigt,
  "Chat 3" = chat_3_bereinigt,
  .id = "Quelle"
)

library(ggplot2)

ggplot(data = kombinierte_reaktionszeit, aes(x = Datum, y = Avg_Reaktionszeit, color = Sender)) +
  
  # Zeichnet die Linien (glättet sie leicht mit geom_smooth für bessere Lesbarkeit, falls es zappelig ist)
  geom_line(alpha = 0.4, linewidth = 0.8) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.3, linewidth = 1.2) +
  
  # Trennt das Diagramm sauber in 3 Fenster (eines pro Chat) untereinander auf
  facet_wrap(~Quelle, ncol = 1, scales = "free_x") +
  
  # Farben für die Personen (konsistent zum vorherigen Plot)
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Durchschnittliche Reaktionszeit im Zeitverlauf",
    subtitle = "Nur bei Senderwechsel gemessen (Trends geglättet, max. 180 Min Wartezeit gewertet)",
    x = "Datum",
    y = "Ø Antwortzeit (in Minuten)",
    color = "Person"
  ) +
  
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "lines") # Mehr Abstand zwischen den Chat-Fenstern
  )