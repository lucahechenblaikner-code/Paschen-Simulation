################################################################################
# Paschen-Simulation
################################################################################
rm(list = objects())

################################################################################
# 1. Vorbereitungen
################################################################################

# set.seed für Replizierbarkeit
set.seed(12345)

# Berechnen der Anzahl der maximalen Kombinationen
Kombinationen_moglich <- 6 * 6 * 6


# Erstellen eines Datensatzes mit den möglichen Würfen, deren Wahrscheinlichkeit
# anhand der Würfelpaaren Kombinationen
paschen_df <- data.frame(
  Wurf = c(
    "7", "8", "9", "10", "12", "13", "14",
    "64", "65", "66", "67", "68", "70",
    "104", "106", "107", "108", "109", "110",
    "122", "123", "124", "125",
    "162", "163", "164",
    "202", "203", "204", "205",
    "260",
    "1-2-3", "2-3-4", "3-4-5", "4-5-6",
    "1-1-1", "2-2-2", "3-3-3", "4-4-4", "5-5-5", "6-6-6",
    "Summe 11", "1-6-5"
  ),
  Rang = 43:1,
  Kombinationen = c(
    3, 6, 3, 12, 3, 6, 3,
    3, 6, 9, 12, 9, 3,
    3, 9, 12, 9, 6, 3,
    3, 3, 6, 3,
    6, 6, 6,
    3, 3, 3, 3,
    3,
    6, 6, 6, 6,
    1, 1, 1, 1, 1, 1,
    15, 6
  )
)

# Wahrscheinlichkeit & kumulierte Wahrscheinlichkeit
paschen_df$Wahrscheinlichkeit <- paschen_df$Kombinationen / Kombinationen_moglich
paschen_df$Akkumuliert <- cumsum(paschen_df$Wahrscheinlichkeit)


################################################################################
# 2. Funktion für Strategie pro Wurf
################################################################################

# der Input max_Wurf gibt die maximal mögliche Anzahl an Würfen an, die durch
# eine for Schleife umgesetzt ist
# Strategie gibt die Grenze an ob ein Wurf stehen gelassen wird oder nicht

Wurf <- function(max_Wurf = 3, Ränge = c(27, 20, 5)) {
  
  momentaner_Wurf <- 0
  
  for (i in 1:max_Wurf) {
    
    momentaner_Wurf <- momentaner_Wurf + 1
    Wurf <- sample(paschen_df$Wurf, 1, prob = paschen_df$Wahrscheinlichkeit)
    names(Wurf) <- momentaner_Wurf
    q <- Ränge[i]
    
    if (paschen_df$Rang[paschen_df$Wurf == Wurf] <= q) {
      return(Wurf)
      
    }
  }
  return(Wurf)
}

################################################################################
# 3. Spielrunden Funktion für zwei Spieler
################################################################################


# Defenieren der Strategien der Spieler, wobei für den dritten Wurf die Strategie
# egal ist
strat_S1 <- c(27, 27, 27)
strat_S2 <- c(27, 15, 5)

# Die folgende Fuktion stellt den Ablauf des Spiels dar, aber nur die 1. Phase,
# dass sogenannte Deckel aufnehmen

# Wie man in der Funktion sehen kann ist die Anzahl der Würfe des zweiten Spielers
# durch die Anzahl der Würfe von Spieler 1 restriktiert. 
# Es gibt drei Möglichkeiten:
# - Draw: Unentschieden und der letzte Spieler fängt wieder an
# - S1 gewinnt: bei besseren Wurf = niedriger Rang des Wurfes
# - S2 gewinnt: bei besseren Wurf = niedriger Rang des Wurfes
# Alle Informationen werdne je nach Fall dann in einer Liste gespeichert um
# spätere Funktionen zu erleichtern.
# Es beginnt immer wieder der Verlierer der vorherigen Runde

Spielrunden <- function(starter = "S1",
                         Strategie_S1 = strat_S1,
                         Strategie_S2 = strat_S2) {
  
  if (starter == "S1") {
    Wurf_S1 <- Wurf(max_Wurf = 3, Ränge = Strategie_S1)
    Wurf_S2 <- Wurf(max_Wurf = as.integer(names(Wurf_S1)), Ränge = Strategie_S2)
    if (paschen_df$Rang[paschen_df$Wurf == Wurf_S1] < paschen_df$Rang[paschen_df$Wurf == Wurf_S2]) {
      return(list(next_starter = "S2", winner = "S1", last_player = "S2"))
    } else if (paschen_df$Rang[paschen_df$Wurf == Wurf_S1] > paschen_df$Rang[paschen_df$Wurf == Wurf_S2]) {
      return(list(next_starter = "S1", winner = "S2", last_player = "S2"))
    } else {
      return(list(next_starter = "Draw", winner = "Draw", last_player = "S2"))
    }
  } else {
    Wurf_S2 <- Wurf(max_Wurf = 3, Ränge = Strategie_S2)
    Wurf_S1 <- Wurf(max_Wurf = as.integer(names(Wurf_S2)), Ränge = Strategie_S1)
    if (paschen_df$Rang[paschen_df$Wurf == Wurf_S2] < paschen_df$Rang[paschen_df$Wurf == Wurf_S1]) {
      return(list(next_starter = "S1", winner = "S2", last_player = "S1"))
    } else if (paschen_df$Rang[paschen_df$Wurf == Wurf_S2] > paschen_df$Rang[paschen_df$Wurf == Wurf_S1]) {
      return(list(next_starter = "S2", winner = "S1", last_player = "S1"))
    } else {
      return(list(next_starter = "Draw", winner = "Draw", last_player = "S1"))
    }
  }
}

################################################################################
# 4. Turnier Simulation, jetzt lassen wir die Spieler gegeinander antreten
################################################################################

Turnier <- function(starter = "S1", Runden = 1000,
                     Strategie_S1 = strat_S1,
                     Strategie_S2 = strat_S2) {
  
  Siege_S1 <- 0
  Siege_S2 <- 0
  Draws <- 0
  
  for (i in 1:Runden) {
    
    ergebnis <- Spielrunden(starter, Strategie_S1, Strategie_S2)
    
    if (ergebnis$winner == "S1") {
      
      Siege_S1 <- Siege_S1 + 1
      starter <- ergebnis$next_starter
      
    } else if (ergebnis$winner == "S2") {
      
      Siege_S2 <- Siege_S2 + 1
      starter <- ergebnis$next_starter
      
    } else { # Draw
      
      Draws <- Draws + 1
      starter <- ergebnis$last_player
      
    }
  }
  
  return(list(Siege_S1 = Siege_S1,
              Siege_S2 = Siege_S2,
              Draws = Draws,
              Winrate_S1 = Siege_S1 / (Runden - Draws)))
}


################################################################################
# 5. Beste Strategie, gegeben einer Strategie des Gegners
################################################################################

optimierer <- function(Runden_pro_Strategie = 2000) {
  ergebnisse <- data.frame()
  
  for (r1 in 1:43) {
    for (r2 in 1:43) {
      strat_S1 <- c(r1, r2, 100)  # Dritter Wert fest auf 100
      strat_S2 <- c(26, 22, 100)   # Gegnerstrategie fest
      
      res <- Turnier(Runden = Runden_pro_Strategie,
                      Strategie_S1 = strat_S1,
                      Strategie_S2 = strat_S2)
      
      ergebnisse <- rbind(ergebnisse, data.frame(
        R1 = r1, R2 = r2, R3 = 100,
        Siege_S1 = res$Siege_S1,
        Siege_S2 = res$Siege_S2,
        Draws = res$Draws,
        Winrate_S1 = res$Winrate_S1
      ))
    }
  }
  
  best <- ergebnisse[which.max(ergebnisse$Winrate_S1), ]
  return(list(beste_Strategie = best, alle_Ergebnisse = ergebnisse))
}

test <- optimierer(Runden_pro_Strategie = 100)
print(test$beste_Strategie)
