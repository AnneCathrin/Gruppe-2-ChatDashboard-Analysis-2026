################################################
##
##
##    Script to visualize WhatsApp-Chatlogs
##    Untersuchung 3 - Teilnehmer 3
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
# 1. PERSONEN ANPASSEN & UNTER NEUEN NAMEN SPEICHERN (U3)
# ==============================================================

# Chat 1: Einlesen, Personenzuteilung passt (unverändert)
chat1_neu <- readRDS("Teilnehmer_3_2026-05-09_07-26-50_84e42b6b.rds")

# Chat 2: Einlesen und Person_1 mit Person_2 tauschen
chat2_neu <- readRDS("Teilnehmer_3_2026-05-09_07-24-23_794d8845.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Chat 3: Einlesen, Personenzuteilung passt (unverändert)
chat3_neu <- readRDS("Teilnehmer_3_2026-05-09_07-23-45_d03c8997.rds")

# Speichern im Datenordner mit eindeutigen U3-Namen
saveRDS(chat1_neu, file.path(save_dir, "Chat1_U3_KORRIGIERT.rds"))
saveRDS(chat2_neu, file.path(save_dir, "Chat2_U3_KORRIGIERT.rds"))
saveRDS(chat3_neu, file.path(save_dir, "Chat3_U3_KORRIGIERT.rds"))


# ==============================================================
# 2. DATEN LADEN & ZEITRAUM DYNAMISCH ERMITTELN
# ==============================================================

raw_data_1 <- readRDS(file.path(save_dir, "Chat1_U3_KORRIGIERT.rds")) 
raw_data_2 <- readRDS(file.path(save_dir, "Chat2_U3_KORRIGIERT.rds")) 
raw_data_3 <- readRDS(file.path(save_dir, "Chat3_U3_KORRIGIERT.rds")) 

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
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Emojis im Chatvergleich (U3)", 
       subtitle = zeitraum_text, 
       x = "Datenquelle", y = "Gesamtanzahl Emojis", fill = "Person") +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())


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
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Anzahl_Nachrichten), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Nachrichten im Chatvergleich (U3)", 
       subtitle = zeitraum_text, 
       x = "Datenquelle", y = "Anzahl Nachrichten", fill = "Person") +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())


# ==============================================================
# 4. LINIENDIAGRAMME (DYNAMISCHE ACHSENBEGRENZUNG)
# ==============================================================

# --- 4.1 LINIENDIAGRAMM: EMOJIS IM ZEITVERLAUF (NUR JAHRESZAHLEN) ---
ggplot(data = kombinierte_emoji_timeline, aes(x = Datum, y = Emoji_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # ORIENTIERUNG AM VORBILD: Schritte alle 2 Jahre, nur die Jahreszahl (%Y) anzeigen
  scale_x_date(limits = c(start_dynamisch, ende_dynamisch), 
               date_labels = "%Y", 
               date_breaks = "2 years") +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der gesendeten Emojis im Zeitverlauf (U3)", subtitle = zeitraum_text, x = "Datum", y = "Emojis pro Tag", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Perfekt horizontal zentriert
  )

# --- 4.2 LINIENDIAGRAMM: NACHRICHTEN IM ZEITVERLAUF (NUR JAHRESZAHLEN) ---
ggplot(data = kombinierte_timeline, aes(x = Datum, y = Nachrichten_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # ORIENTIERUNG AM VORBILD: Schritte alle 2 Jahre, nur die Jahreszahl (%Y) anzeigen
  scale_x_date(limits = c(start_dynamisch, ende_dynamisch), 
               date_labels = "%Y", 
               date_breaks = "2 years") +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Chat-Aktivität im Zeitverlauf (U3)", subtitle = zeitraum_text, x = "Datum", y = "Nachrichten pro Tag", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Perfekt horizontal zentriert
  )

# --- 4.3 LINIENDIAGRAMM: REAKTIONSZEIT IM ZEITVERLAUF (NUR JAHRESZAHLEN) ---
ggplot(data = kombinierte_reaktionszeit, aes(x = Datum, y = Median_Reaktionszeit, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  
  # ORIENTIERUNG AM VORBILD: Schritte alle 2 Jahre, nur die Jahreszahl (%Y) anzeigen
  scale_x_date(limits = c(start_dynamisch, ende_dynamisch), 
               date_labels = "%Y", 
               date_breaks = "2 years") +
  
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Reaktionszeit im Zeitverlauf (U3)", subtitle = zeitraum_text, x = "Datum", y = "Reaktionszeit (Minuten)", color = "Person") +
  theme(
    legend.position = "bottom", 
    strip.text = element_text(face = "bold"), 
    panel.grid.minor.x = element_blank(), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11) # Perfekt horizontal zentriert
  )