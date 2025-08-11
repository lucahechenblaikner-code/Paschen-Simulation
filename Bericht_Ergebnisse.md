# Paschen Simulation - Bericht
Der folgende Abschnitt enthält Visualsierungen und Ergebnisse bezüglich der Simulationen. Als erstes wird die
theorethische Verteilung präsentiert. Anschließend Ergebnisse von verschiedenen Simulationen.

### 1. Theorethische Verteilung

```{r}
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
    "202", "203", "204", "205", "220",
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
    3, 3, 3, 3, 3,
    3,
    6, 6, 6, 6,
    1, 1, 1, 1, 1, 1,
    15, 6
  )
)

# Wahrscheinlichkeit & kumulierte Wahrscheinlichkeit
paschen_df$Wahrscheinlichkeit <- paschen_df$Kombinationen / Kombinationen_moglich
paschen_df$Akkumuliert <- cumsum(paschen_df$Wahrscheinlichkeit)

barplot(paschen_df$Wahrscheinlichkeit ~ as.factor(paschen_df$Wurf),
        las = 2,
        cex.names = 0.8,
        main = "Theoretische Verteilung beim Spiel Paschen",
        ylab = "Wahrscheinlichkeit",
        xlab = "Wurf",
        col  = hcl.colors(n = nrow(paschen_df), palette = "hawaii"))
```

<img width="600" height="550" alt="6d71d92f-03af-49ec-bad1-c57e369f0e0e" src="https://github.com/user-attachments/assets/0a83175a-0f08-482a-a2ec-3def830b87fb" />

Die Grafik zeigt die Wahrscheinlichkeit der Würfe, je nachdem wie viele Kombinationen ein Wurf hat, angenommen das jede Kombination gleich wahrscheinlich ist. 
Gemäß dieser Berechnung ist der am häufigsten vorkommende Wurf die Würfelsumme 11.
Weiters häufige Würfe sind 10, 107 und 67.
Am seltensten werden Päsche geworfen, da es dafür nur 1 mögliche Kombination gibt. 

## Simulation zwei Spieler Paschen - 1.Phase
Die folgenden Ergebnisse kommen aus einer Simulation, wie in Paschen_Simulation - mit der Funktion optimiere und zeigt die 
beste Strategie von S1 gegeben der Strategie S2 von c(26,22,x)

## Code für die Simulation:

```{r}
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
strat_S1 <- c(21, 23, 100)
strat_S2 <- c(26, 23, 100)

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
  
  for (r1 in 1:42) {
    for (r2 in 1:42) {
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

test <- optimierer(Runden_pro_Strategie = 5000)
print(test$beste_Strategie)

test <- as.data.frame(test)

ord_simulation <- test[order(test$alle_Ergebnisse.Winrate_S1, decreasing = TRUE), ]

df <- ord_simulation[ , c("alle_Ergebnisse.R1", "alle_Ergebnisse.R2", "alle_Ergebnisse.R3",
                     "alle_Ergebnisse.Winrate_S1")]

head(df, n = 100)


# Ausgabe für Github
library(knitr)

kable(head(df, n = 100), format = "markdown")
kable(tail(df, n = 100), format = "markdown")

```


## 1 . Durchlauf
#### Ergebnisse (100 beste Strategien nach Winrate - 5000 Simulationen per Strategie)

|     | alle_Ergebnisse.R1| alle_Ergebnisse.R2| alle_Ergebnisse.R3| alle_Ergebnisse.Winrate_S1|
|:----|------------------:|------------------:|------------------:|--------------------------:|
|863  |                 21|                 23|                100|                  0.5127408|
|1073 |                 26|                 23|                100|                  0.5091999|
|1116 |                 27|                 24|                100|                  0.5082542|
|989  |                 24|                 23|                100|                  0.5078415|
|825  |                 20|                 27|                100|                  0.5074689|
|1279 |                 31|                 19|                100|                  0.5074166|
|905  |                 22|                 23|                100|                  0.5073271|
|1032 |                 25|                 24|                100|                  0.5072254|
|826  |                 20|                 28|                100|                  0.5068579|
|987  |                 24|                 21|                100|                  0.5065042|
|913  |                 22|                 31|                100|                  0.5064828|
|1041 |                 25|                 33|                100|                  0.5064023|
|1034 |                 25|                 26|                100|                  0.5063839|
|907  |                 22|                 25|                100|                  0.5060662|
|823  |                 20|                 25|                100|                  0.5049505|
|955  |                 23|                 31|                100|                  0.5047698|
|995  |                 24|                 29|                100|                  0.5044284|
|985  |                 24|                 19|                100|                  0.5043460|
|951  |                 23|                 27|                100|                  0.5043424|
|1071 |                 26|                 21|                100|                  0.5042593|
|1079 |                 26|                 29|                100|                  0.5042382|
|1112 |                 27|                 20|                100|                  0.5040398|
|1074 |                 26|                 24|                100|                  0.5039062|
|1196 |                 29|                 20|                100|                  0.5035052|
|1030 |                 25|                 22|                100|                  0.5033058|
|822  |                 20|                 24|                100|                  0.5030966|
|786  |                 19|                 30|                100|                  0.5028962|
|1022 |                 25|                 14|                100|                  0.5028795|
|1204 |                 29|                 28|                100|                  0.5025832|
|778  |                 19|                 22|                100|                  0.5024876|
|862  |                 21|                 22|                100|                  0.5024865|
|988  |                 24|                 22|                100|                  0.5024763|
|1069 |                 26|                 19|                100|                  0.5022718|
|1068 |                 26|                 18|                100|                  0.5022662|
|820  |                 20|                 22|                100|                  0.5021780|
|784  |                 19|                 28|                100|                  0.5021735|
|1154 |                 28|                 20|                100|                  0.5020773|
|1151 |                 28|                 17|                100|                  0.5020721|
|984  |                 24|                 18|                100|                  0.5020636|
|816  |                 20|                 18|                100|                  0.5019714|
|1147 |                 28|                 13|                100|                  0.5019551|
|941  |                 23|                 17|                100|                  0.5018580|
|782  |                 19|                 26|                100|                  0.5018564|
|1070 |                 26|                 20|                100|                  0.5016611|
|949  |                 23|                 25|                100|                  0.5015531|
|1081 |                 26|                 31|                100|                  0.5012417|
|1289 |                 31|                 29|                100|                  0.5012407|
|1201 |                 29|                 25|                100|                  0.5011432|
|904  |                 22|                 22|                100|                  0.5010339|
|991  |                 24|                 25|                100|                  0.5009350|
|1082 |                 26|                 32|                100|                  0.5008261|
|900  |                 22|                 18|                100|                  0.5007245|
|737  |                 18|                 23|                100|                  0.5007233|
|1283 |                 31|                 23|                100|                  0.5005164|
|1025 |                 25|                 17|                100|                  0.5002064|
|1072 |                 26|                 22|                100|                  0.5001035|
|912  |                 22|                 30|                100|                  0.5000000|
|1127 |                 27|                 35|                100|                  0.4998965|
|901  |                 22|                 19|                100|                  0.4998962|
|865  |                 21|                 25|                100|                  0.4995863|
|1077 |                 26|                 27|                100|                  0.4995863|
|1033 |                 25|                 25|                100|                  0.4995861|
|908  |                 22|                 26|                100|                  0.4994829|
|703  |                 17|                 31|                100|                  0.4994797|
|1166 |                 28|                 32|                100|                  0.4993797|
|694  |                 17|                 22|                100|                  0.4993771|
|858  |                 21|                 18|                100|                  0.4992788|
|1239 |                 30|                 21|                100|                  0.4992746|
|1155 |                 28|                 21|                100|                  0.4990666|
|650  |                 16|                 20|                100|                  0.4989661|
|1111 |                 27|                 19|                100|                  0.4989614|
|868  |                 21|                 28|                100|                  0.4987608|
|775  |                 19|                 19|                100|                  0.4987603|
|610  |                 15|                 22|                100|                  0.4987598|
|1156 |                 28|                 22|                100|                  0.4986501|
|781  |                 19|                 25|                100|                  0.4985507|
|827  |                 20|                 29|                100|                  0.4985489|
|1293 |                 31|                 33|                100|                  0.4985459|
|1162 |                 28|                 28|                100|                  0.4984552|
|944  |                 23|                 20|                100|                  0.4984546|
|740  |                 18|                 26|                100|                  0.4984539|
|994  |                 24|                 28|                100|                  0.4984482|
|1119 |                 27|                 27|                100|                  0.4983478|
|1238 |                 30|                 20|                100|                  0.4982478|
|1117 |                 27|                 25|                100|                  0.4982427|
|1113 |                 27|                 21|                100|                  0.4981413|
|1197 |                 29|                 21|                100|                  0.4980360|
|734  |                 18|                 20|                100|                  0.4980327|
|938  |                 23|                 14|                100|                  0.4980319|
|1028 |                 25|                 20|                100|                  0.4978102|
|998  |                 24|                 32|                100|                  0.4976342|
|613  |                 15|                 25|                100|                  0.4974211|
|1330 |                 32|                 28|                100|                  0.4972171|
|1036 |                 25|                 28|                100|                  0.4971062|
|990  |                 24|                 24|                100|                  0.4970023|
|617  |                 15|                 29|                100|                  0.4969973|
|776  |                 19|                 20|                100|                  0.4969936|
|1026 |                 25|                 18|                100|                  0.4967010|
|983  |                 24|                 17|                100|                  0.4966983|
|996  |                 24|                 30|                100|                  0.4966942|


#### Hier die 100 schlechtesten Strategien (5000 Simulationen per Strategie): 

|     | alle_Ergebnisse.R1| alle_Ergebnisse.R2| alle_Ergebnisse.R3| alle_Ergebnisse.Winrate_S1|
|:----|------------------:|------------------:|------------------:|--------------------------:|
|333  |                  8|                 39|                100|                  0.4101927|
|124  |                  3|                 40|                100|                  0.4099276|
|341  |                  9|                  5|                100|                  0.4098190|
|1727 |                 42|                  5|                100|                  0.4097208|
|260  |                  7|                  8|                100|                  0.4096286|
|293  |                  7|                 41|                100|                  0.4095634|
|302  |                  8|                  8|                100|                  0.4095634|
|174  |                  5|                  6|                100|                  0.4082226|
|16   |                  1|                 16|                100|                  0.4073075|
|176  |                  5|                  8|                100|                  0.4071695|
|35   |                  1|                 35|                100|                  0.4070632|
|51   |                  2|                  9|                100|                  0.4070154|
|87   |                  3|                  3|                100|                  0.4068638|
|1757 |                 42|                 35|                100|                  0.4068394|
|257  |                  7|                  5|                100|                  0.4067095|
|301  |                  8|                  7|                100|                  0.4064249|
|172  |                  5|                  4|                100|                  0.4064060|
|216  |                  6|                  6|                100|                  0.4063149|
|17   |                  1|                 17|                100|                  0.4061723|
|215  |                  6|                  5|                100|                  0.4058273|
|88   |                  3|                  4|                100|                  0.4056917|
|13   |                  1|                 13|                100|                  0.4055567|
|49   |                  2|                  7|                100|                  0.4054897|
|175  |                  5|                  7|                100|                  0.4051527|
|52   |                  2|                 10|                100|                  0.4049741|
|177  |                  5|                  9|                100|                  0.4048507|
|86   |                  3|                  2|                100|                  0.4045991|
|218  |                  6|                  8|                100|                  0.4041365|
|248  |                  6|                 38|                100|                  0.4040571|
|213  |                  6|                  3|                100|                  0.4035486|
|33   |                  1|                 33|                100|                  0.4034245|
|50   |                  2|                  8|                100|                  0.4032358|
|250  |                  6|                 40|                100|                  0.4029944|
|92   |                  3|                  8|                100|                  0.4028583|
|342  |                  9|                  6|                100|                  0.4028152|
|212  |                  6|                  2|                100|                  0.4018208|
|133  |                  4|                  7|                100|                  0.4016257|
|12   |                  1|                 12|                100|                  0.4013226|
|294  |                  7|                 42|                100|                  0.4010717|
|217  |                  6|                  7|                100|                  0.4009973|
|259  |                  7|                  7|                100|                  0.4008282|
|255  |                  7|                  3|                100|                  0.4007854|
|128  |                  4|                  2|                100|                  0.4003312|
|208  |                  5|                 40|                100|                  0.4002476|
|296  |                  8|                  2|                100|                  0.4000000|
|125  |                  3|                 41|                100|                  0.3997507|
|298  |                  8|                  4|                100|                  0.3997078|
|91   |                  3|                  7|                100|                  0.3995855|
|89   |                  3|                  5|                100|                  0.3993399|
|210  |                  5|                 42|                100|                  0.3990464|
|134  |                  4|                  8|                100|                  0.3979275|
|173  |                  5|                  5|                100|                  0.3976730|
|126  |                  3|                 42|                100|                  0.3969008|
|130  |                  4|                  4|                100|                  0.3968847|
|170  |                  5|                  2|                100|                  0.3964840|
|37   |                  1|                 37|                100|                  0.3960191|
|83   |                  2|                 41|                100|                  0.3958678|
|82   |                  2|                 40|                100|                  0.3957899|
|336  |                  8|                 42|                100|                  0.3957773|
|45   |                  2|                  3|                100|                  0.3957297|
|131  |                  4|                  5|                100|                  0.3954451|
|46   |                  2|                  4|                100|                  0.3952332|
|379  |                 10|                  1|                100|                  0.3946336|
|132  |                  4|                  6|                100|                  0.3945069|
|171  |                  5|                  3|                100|                  0.3942107|
|11   |                  1|                 11|                100|                  0.3941772|
|90   |                  3|                  6|                100|                  0.3939959|
|44   |                  2|                  2|                100|                  0.3926723|
|256  |                  7|                  4|                100|                  0.3926419|
|47   |                  2|                  5|                100|                  0.3923841|
|48   |                  2|                  6|                100|                  0.3899606|
|129  |                  4|                  3|                100|                  0.3886816|
|168  |                  4|                 42|                100|                  0.3883615|
|36   |                  1|                 36|                100|                  0.3874172|
|38   |                  1|                 38|                100|                  0.3868839|
|9    |                  1|                  9|                100|                  0.3868704|
|39   |                  1|                 39|                100|                  0.3862325|
|10   |                  1|                 10|                100|                  0.3859938|
|337  |                  9|                  1|                100|                  0.3859359|
|254  |                  7|                  2|                100|                  0.3855049|
|84   |                  2|                 42|                100|                  0.3845836|
|252  |                  6|                 42|                100|                  0.3831155|
|295  |                  8|                  1|                100|                  0.3809721|
|8    |                  1|                  8|                100|                  0.3797311|
|7    |                  1|                  7|                100|                  0.3781218|
|253  |                  7|                  1|                100|                  0.3775616|
|40   |                  1|                 40|                100|                  0.3761620|
|41   |                  1|                 41|                100|                  0.3728324|
|211  |                  6|                  1|                100|                  0.3723711|
|42   |                  1|                 42|                100|                  0.3719059|
|5    |                  1|                  5|                100|                  0.3714226|
|2    |                  1|                  2|                100|                  0.3677994|
|43   |                  2|                  1|                100|                  0.3663980|
|169  |                  5|                  1|                100|                  0.3649544|
|6    |                  1|                  6|                100|                  0.3648732|
|85   |                  3|                  1|                100|                  0.3632417|
|127  |                  4|                  1|                100|                  0.3631666|
|4    |                  1|                  4|                100|                  0.3596110|
|3    |                  1|                  3|                100|                  0.3521127|
|1    |                  1|                  1|                100|                  0.3364968|


## 2. Durchlauf
Der zweite Durchlauf wiederholt die Simulation um die Ergebnisse zu validieren.

#### Ergebnisse (100 beste Strategien nach Winrate - 5000 Simulationen per Strategie)

|     | alle_Ergebnisse.R1| alle_Ergebnisse.R2| alle_Ergebnisse.R3| alle_Ergebnisse.Winrate_S1|
|:----|------------------:|------------------:|------------------:|--------------------------:|
|862  |                 21|                 22|                100|                  0.5124974|
|1029 |                 25|                 21|                100|                  0.5113543|
|993  |                 24|                 27|                100|                  0.5102125|
|1076 |                 26|                 26|                100|                  0.5096055|
|1037 |                 25|                 29|                100|                  0.5094028|
|904  |                 22|                 22|                100|                  0.5092113|
|994  |                 24|                 28|                100|                  0.5086225|
|951  |                 23|                 27|                100|                  0.5084711|
|864  |                 21|                 24|                100|                  0.5081662|
|867  |                 21|                 27|                100|                  0.5069143|
|1117 |                 27|                 25|                100|                  0.5063082|
|858  |                 21|                 18|                100|                  0.5062189|
|1111 |                 27|                 19|                100|                  0.5061191|
|1071 |                 26|                 21|                100|                  0.5060041|
|1026 |                 25|                 18|                100|                  0.5055924|
|995  |                 24|                 29|                100|                  0.5053520|
|1079 |                 26|                 29|                100|                  0.5049917|
|1157 |                 28|                 23|                100|                  0.5049566|
|1078 |                 26|                 28|                100|                  0.5046574|
|863  |                 21|                 23|                100|                  0.5046555|
|947  |                 23|                 23|                100|                  0.5044339|
|820  |                 20|                 22|                100|                  0.5044266|
|989  |                 24|                 23|                100|                  0.5043317|
|1109 |                 27|                 17|                100|                  0.5042399|
|1075 |                 26|                 25|                100|                  0.5041237|
|1207 |                 29|                 31|                100|                  0.5040298|
|819  |                 20|                 21|                100|                  0.5040148|
|913  |                 22|                 31|                100|                  0.5039240|
|949  |                 23|                 25|                100|                  0.5037375|
|987  |                 24|                 21|                100|                  0.5036345|
|784  |                 19|                 28|                100|                  0.5036224|
|905  |                 22|                 23|                100|                  0.5036105|
|900  |                 22|                 18|                100|                  0.5035138|
|821  |                 20|                 23|                100|                  0.5031926|
|1036 |                 25|                 28|                100|                  0.5031017|
|1040 |                 25|                 32|                100|                  0.5030139|
|778  |                 19|                 22|                100|                  0.5028854|
|859  |                 21|                 19|                100|                  0.5027979|
|871  |                 21|                 31|                100|                  0.5026915|
|1067 |                 26|                 17|                100|                  0.5024845|
|988  |                 24|                 22|                100|                  0.5022737|
|1232 |                 30|                 14|                100|                  0.5021753|
|813  |                 20|                 15|                100|                  0.5018672|
|990  |                 24|                 24|                100|                  0.5015486|
|948  |                 23|                 24|                100|                  0.5015429|
|811  |                 20|                 13|                100|                  0.5012381|
|1110 |                 27|                 18|                100|                  0.5009303|
|1331 |                 32|                 29|                100|                  0.5009303|
|1019 |                 25|                 11|                100|                  0.5007254|
|1118 |                 27|                 26|                100|                  0.5007221|
|815  |                 20|                 17|                100|                  0.5006214|
|614  |                 15|                 26|                100|                  0.5006211|
|1065 |                 26|                 15|                100|                  0.5006203|
|1038 |                 25|                 30|                100|                  0.5002070|
|903  |                 22|                 21|                100|                  0.5001033|
|945  |                 23|                 21|                100|                  0.5000000|
|1066 |                 26|                 16|                100|                  0.5000000|
|1199 |                 29|                 23|                100|                  0.5000000|
|1286 |                 31|                 26|                100|                  0.5000000|
|1031 |                 25|                 23|                100|                  0.4997930|
|1114 |                 27|                 22|                100|                  0.4997928|
|1240 |                 30|                 22|                100|                  0.4996895|
|783  |                 19|                 27|                100|                  0.4995875|
|739  |                 18|                 25|                100|                  0.4995868|
|1032 |                 25|                 24|                100|                  0.4994844|
|1034 |                 25|                 26|                100|                  0.4994836|
|1290 |                 31|                 30|                100|                  0.4993814|
|1245 |                 30|                 27|                100|                  0.4993784|
|1125 |                 27|                 33|                100|                  0.4992797|
|908  |                 22|                 26|                100|                  0.4992737|
|912  |                 22|                 30|                100|                  0.4992731|
|940  |                 23|                 16|                100|                  0.4991739|
|1161 |                 28|                 27|                100|                  0.4991715|
|1000 |                 24|                 34|                100|                  0.4989686|
|695  |                 17|                 23|                100|                  0.4989618|
|984  |                 24|                 18|                100|                  0.4988643|
|1077 |                 26|                 27|                100|                  0.4988639|
|997  |                 24|                 31|                100|                  0.4987634|
|1195 |                 29|                 19|                100|                  0.4987572|
|1190 |                 29|                 14|                100|                  0.4986562|
|1073 |                 26|                 23|                100|                  0.4986545|
|991  |                 24|                 25|                100|                  0.4985579|
|822  |                 20|                 24|                100|                  0.4985537|
|825  |                 20|                 27|                100|                  0.4985537|
|1158 |                 28|                 24|                100|                  0.4985507|
|911  |                 22|                 29|                100|                  0.4984450|
|772  |                 19|                 16|                100|                  0.4983478|
|826  |                 20|                 28|                100|                  0.4983430|
|1070 |                 26|                 20|                100|                  0.4982521|
|950  |                 23|                 26|                100|                  0.4981397|
|1167 |                 28|                 33|                100|                  0.4980457|
|827  |                 20|                 29|                100|                  0.4980352|
|1237 |                 30|                 19|                100|                  0.4980352|
|780  |                 19|                 24|                100|                  0.4980335|
|1238 |                 30|                 20|                100|                  0.4979330|
|1122 |                 27|                 30|                100|                  0.4978328|
|571  |                 14|                 25|                100|                  0.4978274|
|1041 |                 25|                 33|                100|                  0.4976352|
|1280 |                 31|                 20|                100|                  0.4976235|
|953  |                 23|                 29|                100|                  0.4975176|

#### Hier die 100 schlechtesten Strategien (5000 Simulationen per Strategie): 

|     | alle_Ergebnisse.R1| alle_Ergebnisse.R2| alle_Ergebnisse.R3| alle_Ergebnisse.Winrate_S1|
|:----|------------------:|------------------:|------------------:|--------------------------:|
|172  |                  5|                  4|                100|                  0.4098361|
|1759 |                 42|                 37|                100|                  0.4095023|
|293  |                  7|                 41|                100|                  0.4093071|
|217  |                  6|                  7|                100|                  0.4090438|
|301  |                  8|                  7|                100|                  0.4089967|
|1761 |                 42|                 39|                100|                  0.4089027|
|49   |                  2|                  7|                100|                  0.4088375|
|15   |                  1|                 15|                100|                  0.4087531|
|164  |                  4|                 38|                100|                  0.4086848|
|123  |                  3|                 39|                100|                  0.4084507|
|174  |                  5|                  6|                100|                  0.4081294|
|50   |                  2|                  8|                100|                  0.4068394|
|378  |                  9|                 42|                100|                  0.4060166|
|298  |                  8|                  4|                100|                  0.4057971|
|208  |                  5|                 40|                100|                  0.4057760|
|51   |                  2|                  9|                100|                  0.4057309|
|1729 |                 42|                  7|                100|                  0.4053328|
|254  |                  7|                  2|                100|                  0.4048414|
|90   |                  3|                  6|                100|                  0.4041761|
|252  |                  6|                 42|                100|                  0.4038581|
|88   |                  3|                  4|                100|                  0.4036906|
|92   |                  3|                  8|                100|                  0.4031957|
|300  |                  8|                  6|                100|                  0.4031858|
|421  |                 11|                  1|                100|                  0.4029387|
|46   |                  2|                  4|                100|                  0.4028702|
|251  |                  6|                 41|                100|                  0.4027807|
|302  |                  8|                  8|                100|                  0.4024036|
|379  |                 10|                  1|                100|                  0.4023987|
|33   |                  1|                 33|                100|                  0.4023681|
|296  |                  8|                  2|                100|                  0.4017359|
|34   |                  1|                 34|                100|                  0.4017341|
|91   |                  3|                  7|                100|                  0.4014978|
|133  |                  4|                  7|                100|                  0.4013295|
|213  |                  6|                  3|                100|                  0.4011194|
|47   |                  2|                  5|                100|                  0.4009150|
|166  |                  4|                 40|                100|                  0.4009140|
|173  |                  5|                  5|                100|                  0.4007077|
|297  |                  8|                  3|                100|                  0.4005818|
|81   |                  2|                 39|                100|                  0.4004141|
|218  |                  6|                  8|                100|                  0.4002073|
|176  |                  5|                  8|                100|                  0.4001654|
|215  |                  6|                  5|                100|                  0.3997522|
|89   |                  3|                  5|                100|                  0.3992111|
|209  |                  5|                 41|                100|                  0.3992111|
|129  |                  4|                  3|                100|                  0.3991718|
|13   |                  1|                 13|                100|                  0.3989648|
|84   |                  2|                 42|                100|                  0.3987118|
|45   |                  2|                  3|                100|                  0.3986738|
|171  |                  5|                  3|                100|                  0.3985869|
|259  |                  7|                  7|                100|                  0.3982666|
|257  |                  7|                  5|                100|                  0.3978895|
|338  |                  9|                  2|                100|                  0.3978785|
|125  |                  3|                 41|                100|                  0.3976003|
|294  |                  7|                 42|                100|                  0.3973988|
|87   |                  3|                  3|                100|                  0.3964978|
|128  |                  4|                  2|                100|                  0.3964077|
|126  |                  3|                 42|                100|                  0.3963554|
|86   |                  3|                  2|                100|                  0.3962146|
|212  |                  6|                  2|                100|                  0.3957773|
|11   |                  1|                 11|                100|                  0.3953102|
|36   |                  1|                 36|                100|                  0.3947532|
|124  |                  3|                 40|                100|                  0.3943720|
|256  |                  7|                  4|                100|                  0.3941651|
|82   |                  2|                 40|                100|                  0.3940713|
|167  |                  4|                 41|                100|                  0.3939206|
|299  |                  8|                  5|                100|                  0.3938513|
|130  |                  4|                  4|                100|                  0.3935444|
|9    |                  1|                  9|                100|                  0.3925680|
|210  |                  5|                 42|                100|                  0.3925098|
|48   |                  2|                  6|                100|                  0.3923525|
|337  |                  9|                  1|                100|                  0.3920231|
|260  |                  7|                  8|                100|                  0.3898551|
|170  |                  5|                  2|                100|                  0.3897989|
|132  |                  4|                  6|                100|                  0.3893732|
|253  |                  7|                  1|                100|                  0.3893383|
|35   |                  1|                 35|                100|                  0.3893162|
|336  |                  8|                 42|                100|                  0.3875648|
|44   |                  2|                  2|                100|                  0.3868961|
|255  |                  7|                  3|                100|                  0.3861839|
|38   |                  1|                 38|                100|                  0.3832472|
|10   |                  1|                 10|                100|                  0.3814646|
|8    |                  1|                  8|                100|                  0.3800746|
|127  |                  4|                  1|                100|                  0.3774247|
|37   |                  1|                 37|                100|                  0.3762131|
|295  |                  8|                  1|                100|                  0.3760595|
|39   |                  1|                 39|                100|                  0.3727310|
|85   |                  3|                  1|                100|                  0.3721362|
|6    |                  1|                  6|                100|                  0.3719298|
|7    |                  1|                  7|                100|                  0.3716174|
|169  |                  5|                  1|                100|                  0.3714876|
|40   |                  1|                 40|                100|                  0.3705311|
|42   |                  1|                 42|                100|                  0.3700041|
|3    |                  1|                  3|                100|                  0.3687708|
|41   |                  1|                 41|                100|                  0.3669612|
|211  |                  6|                  1|                100|                  0.3667081|
|5    |                  1|                  5|                100|                  0.3653647|
|43   |                  2|                  1|                100|                  0.3635423|
|4    |                  1|                  4|                100|                  0.3613184|
|2    |                  1|                  2|                100|                  0.3593718|
|1    |                  1|                  1|                100|                  0.3331956|

### Intepreation der Simulation
In den Tabellen kann man erkennen, dass die Strategien die besonders schlecht performt haben gegen unseren Gegner der c(26,22,100) gespielt hat, extreme Werte waren. 
Das heißt Sie haben entweder ziemlich alle Würfe stehen lassen, auch wenn er eigentlich schlecht war, oder gar keinen. Die schlechteste Strategie in beiden
Fällen ist nur den besten Wurf in beiden Fällen stehen zu lassen - mit einer Winrate von rund 34% im 1. Durchlauf und 33% im 2. Durchlauf.
Besser performt haben jene Strategien die mittlere Werte genommen, meist zwischen 20 und 30.

#### Zur Erläuterung:
In einem Spiel mit zwei Spielern, wobei ein bestimmter Spieler startet ist die nach Wahrscheinlichkeit beste Strategie,
denn Wurf 108 bzw. Rang 27 stehen zu lassen, wenn nur einmal gewürfelt werden kann. Warum?

<img width="650" height="450" alt="b223f78e-a0fb-451a-a03c-624a34cd2566" src="https://github.com/user-attachments/assets/03705217-81ec-478a-89ac-d745fef4546b" />

Die Verteilungsfunktion gibt die akkumulierte Wahrscheinlichkeit an, einen besseren oder schlechteren Wurf zu werfen, wenn es nur einen Wurf gibt.
Für 108 gilt, dass wenn man beim 1. Versuch 108 wirft ist die Wahrscheinlichkeit zu gewinnen ca. 51%. Spielt man dieses Spiel häufig genug und es wird hypothetisch immer
vom gleichen Spieler begonnen der immer 108 wirft, dann sollte er in 51 % der Fälle gewinnen.

Warum kann nun im 1 und 2 Durchlauf jemand mit einer Strategie von c(21, 23, 100) und c(21, 22, 100) gewinnen. Dieser Spieler nimmt einfach mehr Risiko beim ersten Wurf,
als unser Gegenspieler. Bedenkt man, dass der Spieler ganz viele verschiedene Strategien die ungefähr gleich sind gespielt hat und damit auch unter eine Winrate von 50 % gekommen ist,
kann man das als Zufallsgewinn erklären. Um gegen einen Spieler der sicher spielt - ohne viel Risiko gewinnen zu können, macht es Sinn etwas riskanter zu spielen um eine Chance
auf den Gewinn zu haben. 





