# Visualisierungen
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

###
