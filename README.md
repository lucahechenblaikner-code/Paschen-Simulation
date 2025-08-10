# Paschen-Simulation
In dieser Simulation wird versucht das Spiel Paschen zu simulieren. Dieses Projekt ist momentan "Work-in-progress" und wird regelmäßig weiterentwickelt werden.

 Das Spiel "Paschen" benötigt folgende Materialien:
   - 3 Würfel
   - 1 Würfelbecher
   - 1 Würfelbrett
   - ca. 7 Bierdeckel 

## Die Spielregelen:
 - man kann das Spiel theoretisch mit 2 bis unendlich vielen Spielern spielen
 - in der ersten Phase des Spiels nimmt der jenige Spieler mit dem niedrigsten
   Wurf einen Deckel auf
 - Nachdem alle Deckel verteilt wurden, startet die zweite Phase des Spiels
 - in der zweiten Phase darf derjenige Spieler mit höchsten Wurf einen Deckel
   ablegen
 - die Runde verloren hat derjenige Spieler welche als letzter noch Deckel hat
 - das Spiel verloren hat derjenige Spieler der zweimal eine Runde verliert

## Würfelbewertung
 - die 1: steht für 100
 - die 6: steht für 60
 - 2,3,4,5 <- bedeuten auch <- 2,3,4,5
 - Bei normalen Würfen wie 2,5,3 werden die Zahlen zu addiert.
 - Außerdem gibt es "Pasche" <- dreimal die gleiche Zahl
 - Es gibt "Reihen" wie 1,2,3
 - der 3-beste Wurf im Spiel ist die Würfelsumme 11
 - der 2-best Wurf ist die Zahlenkombination 165
 - der beste Wurf ist der Hauswurf, welcher jedes Neujahr neu festgelegt wird

Von der Bewertung gilt normale Würfe < Reihe < Pasch < Würfelsumme 11 < 1,6,5 < Hauspasch

Hier eine Auflistung aller möglichen Würfelsummen nach Bewertung
### Normale Zahlen
 7, 8, 9, 10, 12, 13, 14, 64, 65, 66, 67, 68, 70, 104, 106, 107, 108,
 109, 110, 122, 123, 124, 125, 162, 163, 164, 202, 203, 204, 205, 260

### Reihen
 1.2.3, 2.3.4, 3.4.5, 4.5.6

### Pasche
 1.1.1, 2.2.2, 3.3.3, 4.4.4, 5.5.5, 6.6.6

### Sonderzahlen
 Würfelsumme 11, der Wurf 1.6.5, der Hauspasch

### Anzahl der möglichen Kombinationen
Anzahl = 6³ = 216

### Wahrscheinlichkeiten zu würfeln

| Zahl   | Kombinationen |
|--------|---------------|
| 7      | 3             |
| 8      | 6             |
| 9      | 3             |
| 10     | 12            |
| 12     | 3             |
| 13     | 6             |
| 14     | 3             |
| 64     | 3             |
| 65     | 6             |
| 66     | 9             |
| 67     | 12            |
| 68     | 9             |
| 70     | 3             |
| 104    | 3             |
| 106    | 9             |
| 107    | 12            |
| 108    | 9             |
| 109    | 6             |
| 110    | 3             |
| 122    | 3             |
| 123    | 3             |
| 124    | 6             |
| 125    | 3             |
| 162    | 6             |
| 163    | 6             |
| 164    | 6             |
| 202    | 3             |
| 203    | 3             |
| 204    | 3             |
| 205    | 3             |
| 260    | 3             |
| 1.2.3  | 6             |
| 2.3.4  | 6             |
| 3.4.5  | 6             |
| 4.5.6  | 6             |
| 1.1.1  | 1             |
| 2.2.2  | 1             |
| 3.3.3  | 1             |
| 4.4.4  | 1             |
| 5.5.5  | 1             |
| 6.6.6  | 1             |
| W - 11 | 15            |
| 1.6.5  | 6             |
| Hauspa.| *ersetzt eine bestimmte Zahl* |
