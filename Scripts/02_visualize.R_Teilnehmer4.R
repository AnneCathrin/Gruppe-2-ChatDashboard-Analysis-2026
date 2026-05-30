################################################
##
##
##    Script to visualize WhatsApp-Chatlogs
##    Untersuchung 4 - Teilnehmer 4
##    Dynamischer Zeitraum basierend auf kürzestem Chat
##
##
#################################################

library(ggplot2)
library(ragg)
library(lubridate)
library(slider)
library(tidyverse)
library(WhatsR)

# Ordner-Strukturen definieren
save_dir <- "Data_cleaned"
plot_dir <- "Plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================
# 1. PERSONEN ANPASSEN & UNTER NEUEN NAMEN SPEICHERN (U4)
# ==============================================================

# Chat 1: Einlesen und Person_1 mit Person_2 tauschen
chat1_neu <- readRDS("Teilnehmer_4_2026-05-06_18-15-31_da3b1714.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Chat 2: Einlesen, Personenzuteilung passt (unverändert)
chat2_neu <- readRDS("Teilnehmer_4_2026-05-06_18-14-26_91818129.rds")

# Chat 3: Einlesen und Person_1 mit Person_2 tauschen
chat3_neu <- readRDS("Teilnehmer_4_2026-05-06_18-16-13_a0a9dd05.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Speichern im Datenordner mit eindeutigen U4-Namen
saveRDS(chat1_neu, file.path(save_dir, "Chat1_U4_KORRIGIERT.rds"))
saveRDS(chat2_neu, file.path(save_dir, "Chat2_U4_KORRIGIERT.rds"))
saveRDS(chat3_neu, file.path(save_dir, "Chat3_U4_KORRIGIERT.rds"))


# ==============================================================
# 2. DATEN LADEN & ZEITRAUM DYNAMISCH ERMITTELN
# ==============================================================

raw_data_1 <- readRDS(file.path(save_dir, "Chat1_U4_KORRIGIERT.rds")) 
raw_data_2 <- readRDS(file.path(save_dir, "Chat2_U4_KORRIGIERT.rds")) 
raw_data_3 <- readRDS(file.path(save_dir, "Chat3_U4_KORRIGIERT.rds")) 

# Hilfsfunktion, um valide Datums-Grenzen pro Chat zu bestimmen
get_chat_borders <- function(df) {
  df_clean <- df %>% 
    filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime))
  return(tibble(Start = min(df_clean$Datum, na.rm = TRUE), Ende = max(df_clean$Datum, na.rm = TRUE)))
}

# Grenzen für alle drei Chats holen
borders <- bind_rows(get_chat_borders(raw_data_1), get_chat_borders(raw_data_2), get_chat_borders(raw_data_3))

# Kürzester gemeinsamer Zeitraum: Das späteste Startdatum bis zum frühesten Enddatum
start_dynamisch <- max(borders$Start)
ende_dynamisch  <- min(borders$Ende)

# Text-Formatierung für Untertitel generieren
zeitraum_text <- paste0("Kürzester gemeinsamer Zeitraum: ", 
                        format(start_dynamisch, "%d.%m.%Y"), " bis ", 
                        format(ende_dynamisch, "%d.%m.%Y"))


# ==============================================================
# 3. BALKENDIAGRAMME (FILTER AUF DYNAMISCHEN ZEITRAUM)
# ==============================================================

# --- 3.1 BALKENDIAGRAMM: ANZAHL EMOJIS ---
get_emoji_counts <- function(df, start, ende) {
  df %>%  
    filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% 
    filter(Datum >= start & Datum <= ende) %>% 
    filter(!is.na(EmojiDescriptions), EmojiDescriptions != "") %>% 
    mutate(Message_Clean = sapply(EmojiDescriptions, function(x) paste(unlist(x), collapse = " "))) %>% 
    mutate(n = str_count(Message_Clean, "\\w+")) %>% 
    group_by(Sender) %>% summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
}

kombinierte_emojis <- bind_rows(
  "Chat 1" = get_emoji_counts(raw_data_1, start_dynamisch, ende_dynamisch), 
  "Chat 2" = get_emoji_counts(raw_data_2, start_dynamisch, ende_dynamisch), 
  "Chat 3" = get_emoji_counts(raw_data_3, start_dynamisch, ende_dynamisch), 
  .id = "Quelle"
) %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_emojis, aes(x = Quelle, y = n, fill = Sender)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75), vjust = -0.6, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Emojis im Chatvergleich (U4)", 
       subtitle = zeitraum_text, 
       x = "Untersuchte Chats", y = "Gesamtanzahl Emojis", fill = "Person") +
  theme(
    legend.position = "bottom", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.x = element_text(margin = margin(t = 15))
  )


# --- 3.2 BALKENDIAGRAMM: ANZAHL NACHRICHTEN ---
get_msg_counts <- function(df, start, ende) {
  df %>% 
    filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% 
    filter(Datum >= start & Datum <= ende) %>% 
    count(Sender, name = "Anzahl_Nachrichten")
}

kombinierte_nachrichten <- bind_rows(
  "Chat 1" = get_msg_counts(raw_data_1, start_dynamisch, ende_dynamisch), 
  "Chat 2" = get_msg_counts(raw_data_2, start_dynamisch, ende_dynamisch), 
  "Chat 3" = get_msg_counts(raw_data_3, start_dynamisch, ende_dynamisch), 
  .id = "Quelle"
) %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_nachrichten, aes(x = Quelle, y = Anzahl_Nachrichten, fill = Sender)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = Anzahl_Nachrichten), position = position_dodge(width = 0.75), vjust = -0.6, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Nachrichten im Chatvergleich (U4)", 
       subtitle = zeitraum_text, 
       x = "Untersuchte Chats", y = "Anzahl Nachrichten", fill = "Person") +
  theme(
    legend.position = "bottom", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.x = element_text(margin = margin(t = 15))
  )


# ==============================================================
# 4. LINIENDIAGRAMME (DYNAMISCHE ACHSENBEGRENZUNG)
# ==============================================================

# ==============================================================
# GEMEINSAMEN MAXIMALEN ZEITRAUM FÜR LINIENDIAGRAMME ERMITTELN
# ==============================================================
# Wir holen das absolute Minimum und Maximum über alle Daten hinweg
start_linie <- min(borders$Start)
ende_linie  <- max(borders$Ende)

# --- 4.1 LINIENDIAGRAMM: EMOJIS IM ZEITVERLAUF ---
ggplot(data = kombinierte_emoji_timeline, aes(x = Datum, y = Emoji_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  
  # X-Achse bleibt für alle Chats identisch, Y-Achse skaliert frei
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # OPTIMIERUNG: Grenzen fixieren, aber R die schönsten Abstände (z.B. Monate) 
  # vollautomatisch und passgenau für diesen Zeitraum wählen lassen.
  scale_x_date(limits = c(start_linie, ende_linie)) +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der gesendeten Emojis im Zeitverlauf (U4)", 
       subtitle = "Anzahl gesendeter Emojis pro Tag (Trends geglättet)", x = "Datum", y = "Emojis pro Tag", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Horizontal zentriert
  )


# --- 4.2 LINIENDIAGRAMM: NACHRICHTEN IM ZEITVERLAUF ---
ggplot(data = kombinierte_timeline, aes(x = Datum, y = Nachrichten_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  
  # X-Achse bleibt für alle Chats identisch, Y-Achse skaliert frei
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # OPTIMIERUNG: Grenzen fixieren, automatische und harmonische Schrittweite
  scale_x_date(limits = c(start_linie, ende_linie)) +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Chat-Aktivität im Zeitverlauf (U4)", 
       subtitle = "Anzahl gesendeter Nachrichten pro Tag (Trends geglättet)", x = "Datum", y = "Nachrichten pro Tag", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Horizontal zentriert
  )


# --- 4.3 LINIENDIAGRAMM: REAKTIONSZEIT IM ZEITVERLAUF ---
ggplot(data = kombinierte_reaktionszeit, aes(x = Datum, y = Median_Reaktionszeit, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  
  # X-Achse bleibt für alle Chats identisch, Y-Achse skaliert frei
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # OPTIMIERUNG: Grenzen fixieren, automatische und harmonische Schrittweite
  scale_x_date(limits = c(start_linie, ende_linie)) +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Reaktionszeit im Zeitverlauf (U4)", 
       subtitle = "Median der Reaktionszeit pro Tag (Trends geglättet)", x = "Datum", y = "Reaktionszeit (Minuten)", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Horizontal zentriert
  )