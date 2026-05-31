################################################
##
##    Script to visualize WhatsApp-Chatlogs
##    Untersuchung 3 - Teilnehmer 3 (KOMPLETT)
##    Echter, unbeschnittener Zeitraum für Liniendiagramme
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
# 1. PERSONEN ANPASSEN & UNTER NAMEN FÜR U3 SPEICHERN
# ==============================================================

# Chat 1: Einlesen, Personenzuteilung passt (unverändert für U3)
chat1_neu <- readRDS("Teilnehmer_3_2026-05-09_07-26-50_84e42b6b.rds")

# Chat 2: Einlesen und Person_1 mit Person_2 tauschen (für U3)
chat2_neu <- readRDS("Teilnehmer_3_2026-05-09_07-24-23_794d8845.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Chat 3: Einlesen, Personenzuteilung passt (unverändert für U3)
chat3_neu <- readRDS("Teilnehmer_3_2026-05-09_07-23-45_d03c8997.rds")

# Speichern im Datenordner mit eindeutigen U3-Namen
saveRDS(chat1_neu, file.path(save_dir, "Chat1_U3_KORRIGIERT.rds"))
saveRDS(chat2_neu, file.path(save_dir, "Chat2_U3_KORRIGIERT.rds"))
saveRDS(chat3_neu, file.path(save_dir, "Chat3_U3_KORRIGIERT.rds"))


# ==============================================================
# 2. DATEN FÜR UNTERSUCHUNG 3 EXKLUSIV LADEN
# ==============================================================

raw_data_1 <- readRDS(file.path(save_dir, "Chat1_U3_KORRIGIERT.rds")) 
raw_data_2 <- readRDS(file.path(save_dir, "Chat2_U3_KORRIGIERT.rds")) 
raw_data_3 <- readRDS(file.path(save_dir, "Chat3_U3_KORRIGIERT.rds")) 

# Hilfsfunktion für Balkendiagramm-Grenzen (KORRIGIERT: %in% statt %in=)
get_chat_borders <- function(df) {
  df_clean <- df %>% 
    filter(!is.na(Sender), !Sender %in% c('', 'WhatsApp', 'System', 'WhatsApp System Message')) %>% 
    mutate(Datum = as.Date(DateTime))
  return(tibble(Start = min(df_clean$Datum, na.rm = TRUE), Ende = max(df_clean$Datum, na.rm = TRUE)))
}

borders <- bind_rows(get_chat_borders(raw_data_1), get_chat_borders(raw_data_2), get_chat_borders(raw_data_3))
start_dynamisch <- max(borders$Start)
ende_dynamisch  <- min(borders$Ende)

zeitraum_text <- paste0("Kürzester gemeinsamer Zeitraum: ", 
                        format(start_dynamisch, "%d.%m.%Y"), " bis ", 
                        format(ende_dynamisch, "%d.%m.%Y"))

# ==============================================================
# 3. BALKENDIAGRAMME (REIHENFOLGE GETAUSCHT & MIT SÄULENBESCHRIFTUNG)
# ==============================================================

# --- 3.1 BALKENDIAGRAMM: GESAMTANZAHL EMOJIS ---
balken_daten_emojis <- kombinierte_emoji_timeline %>%
  filter(Datum >= start_dynamisch & Datum <= ende_dynamisch) %>%
  group_by(Quelle, Sender) %>%
  summarise(Gesamt_Emojis = sum(Emoji_Anzahl, na.rm = TRUE), .groups = "drop")

ggplot(data = balken_daten_emojis, aes(x = Sender, y = Gesamt_Emojis, fill = Sender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, width = 0.7) +
  
  # Werte stehen fett über den Säulen, formatiert mit Tausenderpunkt
  geom_text(aes(label = scales::comma(Gesamt_Emojis, big.mark = ".")), 
            vjust = -0.5, color = "#333333", fontface = "bold", size = 4) +
  
  facet_wrap(~Quelle, ncol = 3) +  # Nebeneinander für den direkten Vergleich
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Gesamtanzahl der gesendeten Emojis (U3)",
    subtitle = zeitraum_text,
    x = "Person",
    y = "Anzahl Emojis",
    fill = "Person"
  ) +
  theme(
    legend.position = "none",  # Legende ausblenden, da X-Achse beschriftet ist
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )


# --- 3.2 BALKENDIAGRAMM: GESAMTANZAHL NACHRICHTEN ---
balken_daten_nachrichten <- kombinierte_timeline %>%
  filter(Datum >= start_dynamisch & Datum <= ende_dynamisch) %>%
  group_by(Quelle, Sender) %>%
  summarise(Gesamt_Nachrichten = sum(Nachrichten_Anzahl, na.rm = TRUE), .groups = "drop")

ggplot(data = balken_daten_nachrichten, aes(x = Sender, y = Gesamt_Nachrichten, fill = Sender)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, width = 0.7) +
  
  # Werte stehen fett über den Säulen, formatiert mit Tausenderpunkt
  geom_text(aes(label = scales::comma(Gesamt_Nachrichten, big.mark = ".")), 
            vjust = -0.5, color = "#333333", fontface = "bold", size = 4) +
  
  facet_wrap(~Quelle, ncol = 3) +  
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Gesamtanzahl der gesendeten Nachrichten (U3)",
    subtitle = zeitraum_text,
    x = "Person",
    y = "Anzahl Nachrichten",
    fill = "Person"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

# ==============================================================
# 3. GENERIERUNG DER TIMELINES FÜR LINIENDIAGRAMME (FRISCH AUS U3)
# ==============================================================

# --- Timelines generieren ---
get_timeline_emojis <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% filter(!is.na(EmojiDescriptions), EmojiDescriptions != "") %>% 
    mutate(Message_Clean = sapply(EmojiDescriptions, function(x) paste(unlist(x), collapse = " "))) %>% 
    mutate(Anzahl_In_Nachricht = str_count(Message_Clean, "\\w+")) %>% 
    group_by(Datum, Sender) %>% summarise(Emoji_Anzahl = sum(Anzahl_In_Nachricht, na.rm = TRUE), .groups = "drop")
}

get_msg_timeline <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% count(Datum, Sender, name = "Nachrichten_Anzahl")
}

get_reaktion_timeline <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% 
    mutate(Reaktionszeit_Min = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>% 
    filter(Sender != lag(Sender), Reaktionszeit_Min > 0, Reaktionszeit_Min < 180) %>% 
    group_by(Datum, Sender) %>% summarise(Median_Reaktionszeit = median(Reaktionszeit_Min, na.rm = TRUE), .groups = "drop")
}

kombinierte_emoji_timeline <- bind_rows("Chat 1" = get_timeline_emojis(raw_data_1), "Chat 2" = get_timeline_emojis(raw_data_2), "Chat 3" = get_timeline_emojis(raw_data_3), .id = "Quelle") %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))
kombinierte_timeline       <- bind_rows("Chat 1" = get_msg_timeline(raw_data_1), "Chat 2" = get_msg_timeline(raw_data_2), "Chat 3" = get_msg_timeline(raw_data_3), .id = "Quelle") %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))
kombinierte_reaktionszeit  <- bind_rows("Chat 1" = get_reaktion_timeline(raw_data_1), "Chat 2" = get_reaktion_timeline(raw_data_2), "Chat 3" = get_reaktion_timeline(raw_data_3), .id = "Quelle") %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))


# ==============================================================
# 4. LINIENDIAGRAMME (ECHTER U3 ZEITRAUM - ABSOLUT SYNCHRONISIERT)
# ==============================================================

# --- 4.1 LINIENDIAGRAMM: EMOJIS IM ZEITVERLAUF ---
limits_emoji <- range(kombinierte_emoji_timeline$Datum, na.rm = TRUE)

ggplot(data = kombinierte_emoji_timeline, aes(x = Datum, y = Emoji_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = limits_emoji) +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der gesendeten Emojis im Zeitverlauf (U3)", 
       subtitle = "Anzahl gesendeter Emojis pro Tag (Trends geglättet)", x = "Datum", y = "Emojis pro Tag", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))


# --- 4.2 LINIENDIAGRAMM: NACHRICHTEN IM ZEITVERLAUF ---
limits_nachrichten <- range(kombinierte_timeline$Datum, na.rm = TRUE)

ggplot(data = kombinierte_timeline, aes(x = Datum, y = Nachrichten_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = limits_nachrichten) +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Chat-Aktivität im Zeitverlauf (U3)", 
       subtitle = "Anzahl gesendeter Nachrichten pro Tag (Trends geglättet)", x = "Datum", y = "Nachrichten pro Tag", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))


# --- 4.3 LINIENDIAGRAMM: REAKTIONSZEIT IM ZEITVERLAUF ---
limits_reaktion <- range(kombinierte_reaktionszeit$Datum, na.rm = TRUE)

ggplot(data = kombinierte_reaktionszeit, aes(x = Datum, y = Median_Reaktionszeit, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = limits_reaktion) +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Reaktionszeit im Zeitverlauf (U3)", 
       subtitle = "Median der Reaktionszeit pro Tag (Trends geglättet)", x = "Datum", y = "Reaktionszeit (Minuten)", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11))