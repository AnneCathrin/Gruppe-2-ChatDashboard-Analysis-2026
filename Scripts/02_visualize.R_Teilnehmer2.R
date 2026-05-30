################################################
##
##
##    Script to visualize WhatsApp-Chatlogs
##    Untersuchung 2 - Begrenzt auf 01.01.2026 bis 09.05.2026
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
# 1. PERSONEN TAUSCHEN, JAHR KORRIGIEREN & SPEICHERN
# ==============================================================

# Chat 1: Einlesen, Personen tauschen & Zeilen 1-9 auf das Jahr 2025 korrigieren
chat1_neu <- readRDS("Teilnehmer_2_2026-05-08_19-57-07_a6404cf7.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))
year(chat1_neu$DateTime[1:9]) <- 2025

# Chat 2: Einlesen und Personen tauschen
chat2_neu <- readRDS("Teilnehmer_2_2026-05-08_20-33-42_d1b73358.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Chat 3: Einlesen und Personen tauschen
chat3_neu <- readRDS("Teilnehmer_2_2026-05-08_20-35-15_ba08e176.rds") %>% 
  mutate(Sender = recode(Sender, "Person_1" = "Person_2", "Person_2" = "Person_1"))

# Speichern im Datenordner
saveRDS(chat1_neu, file.path(save_dir, "Chat1_U2_KORRIGIERT.rds"))
saveRDS(chat2_neu, file.path(save_dir, "Chat2_U2_KORRIGIERT.rds"))
saveRDS(chat3_neu, file.path(save_dir, "Chat3_U2_KORRIGIERT.rds"))


# ==============================================================
# 2. DATEN FÜR DIE VISUALISIERUNG LADEN
# ==============================================================

raw_data_1 <- readRDS(file.path(save_dir, "Chat1_U2_KORRIGIERT.rds")) 
raw_data_2 <- readRDS(file.path(save_dir, "Chat2_U2_KORRIGIERT.rds")) 
raw_data_3 <- readRDS(file.path(save_dir, "Chat3_U2_KORRIGIERT.rds")) 

# Exakte Zeitraumgrenzen setzen
start_2026 <- as.Date("2026-01-01")
ende_2026  <- as.Date("2026-05-09") # Taggenaues Enddatum


# ==============================================================
# 3. BALKENDIAGRAMME (FILTER AUF WUNSCHZEITRAUM)
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
  "Chat 1" = get_emoji_counts(raw_data_1, start_2026, ende_2026), 
  "Chat 2" = get_emoji_counts(raw_data_2, start_2026, ende_2026), 
  "Chat 3" = get_emoji_counts(raw_data_3, start_2026, ende_2026), 
  .id = "Quelle"
) %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_emojis, aes(x = Quelle, y = n, fill = Sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Emojis im Chatvergleich (U2)", 
       subtitle = "Zeitraum: 01.01.2026 bis 09.05.2026", 
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
  "Chat 1" = get_msg_counts(raw_data_1, start_2026, ende_2026), 
  "Chat 2" = get_msg_counts(raw_data_2, start_2026, ende_2026), 
  "Chat 3" = get_msg_counts(raw_data_3, start_2026, ende_2026), 
  .id = "Quelle"
) %>% mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_nachrichten, aes(x = Quelle, y = Anzahl_Nachrichten, fill = Sender)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Anzahl_Nachrichten), position = position_dodge(width = 0.8), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) + 
  theme_minimal(base_size = 14) +
  labs(title = "Gesamtanzahl der gesendeten Nachrichten im Chatvergleich (U2)", 
       subtitle = "Zeitraum: 01.01.2026 bis 09.05.2026", 
       x = "Datenquelle", y = "Anzahl Nachrichten", fill = "Person") +
  theme(legend.position = "bottom", panel.grid.major.x = element_blank())


# ==============================================================
# 4. LINIENDIAGRAMME (ACHSEN BEGRENZT BIS 09.05.2026)
# ==============================================================

# --- 4.1 LINIENDIAGRAMM: EMOJIS IM ZEITVERLAUF ---
get_timeline_emojis <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% filter(!is.na(EmojiDescriptions), EmojiDescriptions != "") %>% 
    mutate(Message_Clean = sapply(EmojiDescriptions, function(x) paste(unlist(x), collapse = " "))) %>% 
    mutate(Anzahl_In_Nachricht = str_count(Message_Clean, "\\w+")) %>% 
    group_by(Datum, Sender) %>% summarise(Emoji_Anzahl = sum(Anzahl_In_Nachricht, na.rm = TRUE), .groups = "drop")
}

kombinierte_emoji_timeline <- bind_rows("Chat 1" = get_timeline_emojis(raw_data_1), "Chat 2" = get_timeline_emojis(raw_data_2), "Chat 3" = get_timeline_emojis(raw_data_3), .id = "Quelle") %>% 
  mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_emoji_timeline, aes(x = Datum, y = Emoji_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = c(start_2026, ende_2026), date_labels = "%d.%m.%Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der gesendeten Emojis im Zeitverlauf (U2)", subtitle = "Zeitraum: 01.01.2026 bis 09.05.2026", x = "Datum", y = "Emojis pro Tag", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))


# --- 4.2 LINIENDIAGRAMM: NACHRICHTEN IM ZEITVERLAUF ---
get_msg_timeline <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% count(Datum, Sender, name = "Nachrichten_Anzahl")
}

kombinierte_timeline <- bind_rows("Chat 1" = get_msg_timeline(raw_data_1), "Chat 2" = get_msg_timeline(raw_data_2), "Chat 3" = get_msg_timeline(raw_data_3), .id = "Quelle") %>% 
  mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_timeline, aes(x = Datum, y = Nachrichten_Anzahl, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = c(start_2026, ende_2026), date_labels = "%d.%m.%Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Chat-Aktivität im Zeitverlauf (U2)", subtitle = "Zeitraum: 01.01.2026 bis 09.05.2026", x = "Datum", y = "Nachrichten pro Tag", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


# --- 4.3 LINIENDIAGRAMM: REAKTIONSZEIT IM ZEITVERLAUF ---
get_reaktion_timeline <- function(df) {
  df %>% filter(!is.na(Sender), !Sender %in% c("", "WhatsApp", "System", "WhatsApp System Message")) %>% 
    mutate(Datum = as.Date(DateTime)) %>% 
    mutate(Reaktionszeit_Min = as.numeric(difftime(DateTime, lag(DateTime), units = "mins"))) %>% 
    filter(Sender != lag(Sender), Reaktionszeit_Min > 0, Reaktionszeit_Min < 180) %>% 
    group_by(Datum, Sender) %>% summarise(Median_Reaktionszeit = median(Reaktionszeit_Min, na.rm = TRUE), .groups = "drop")
}

kombinierte_reaktionszeit <- bind_rows("Chat 1" = get_reaktion_timeline(raw_data_1), "Chat 2" = get_reaktion_timeline(raw_data_2), "Chat 3" = get_reaktion_timeline(raw_data_3), .id = "Quelle") %>% 
  mutate(Sender = factor(Sender, levels = c("Person_1", "Person_2")))

ggplot(data = kombinierte_reaktionszeit, aes(x = Datum, y = Median_Reaktionszeit, color = Sender)) +
  geom_line(alpha = 0.3, linewidth = 0.5) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 15), linewidth = 1.2) +
  facet_wrap(~Quelle, ncol = 1, scales = "free_y") + 
  scale_x_date(limits = c(start_2026, ende_2026), date_labels = "%d.%m.%Y", date_breaks = "1 month") +
  scale_color_manual(values = c("Person_1" = "#4e79a7", "Person_2" = "#e15759")) +
  theme_minimal(base_size = 14) + 
  labs(title = "Entwicklung der Reaktionszeit im Zeitverlauf (U2)", subtitle = "Zeitraum: 01.01.2026 bis 09.05.2026", x = "Datum", y = "Reaktionszeit (Minuten)", color = "Person") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))