extensions [matrix]

breed [antibodies antibody]
breed [vira virus]

vira-own [l_func ex_dist danger danger_anti own_a_eq own_a_norm spm_virus rwm_virus pnl_virus a_matrix_virus n_virus n_antigene virus_antigene func_virus countdown_respawn]
antibodies-own [energy]
globals [func_survie survie_antigene n_survie_antigene n_survie spm_survie rwm_survie pnl_survie a_matrix_survie
func_mode vira_number n_mode_antigene n_mode spm_mode pnl_mode rwm_mode a_matrix_mode a_eq_mode a_norm_mode lastc countdown_i]

to kill
  ask antibodies [die]
end

to create-right
  let x 0
  let y 0
  ask vira [set x xcor set y ycor]
  create-antibodies 1 [set xcor (x + 1) set ycor y]
end


to create-left
  let x 0
  let y 0
  ask vira [set x xcor set y ycor]
  create-antibodies 1 [set xcor (x - 1) set ycor y]
end


to create-up
  let x 0
  let y 0
  ask vira [set x xcor set y ycor]
  create-antibodies 1 [set xcor x set ycor (y + 1)]
end


to create-down
  let x 0
  let y 0
  ask vira [set x xcor set y ycor]
  create-antibodies 1 [set xcor x set ycor (y - 1)]
end

to setup
  clear-patches
  clear-turtles
  ;clear-plot

  ;;Variables pour le super-agent
  set func_mode ["survive" "develop"]
  set n_mode_antigene 1
  set n_mode 2
  set spm_mode matrix:make-constant n_mode n_mode_antigene 0
  set rwm_mode matrix:make-constant n_mode n_mode_antigene 0
  set pnl_mode matrix:make-constant n_mode n_mode_antigene 0
  set a_matrix_mode matrix:make-constant n_mode n_mode 0
  set a_eq_mode matrix:make-constant 1 n_mode 0
  set a_norm_mode matrix:make-constant 1 n_mode 0
  set lastc -1

  ;;Variables pour la réponse immunitaire des virus
  ;; ;; Mode survive
  set func_survie ["move-right" "move-left" "move-up" "move-down"]
  set survie_antigene ["left" "right" "down" "up"]
  set n_survie_antigene 4
  set n_survie 4
  set spm_survie matrix:make-constant n_survie n_survie_antigene 0
  set rwm_survie matrix:make-constant n_survie n_survie_antigene 0
  set pnl_survie matrix:make-constant n_survie n_survie_antigene 0
  set a_matrix_survie matrix:make-constant n_survie n_survie 0

  set countdown_i countdown
 set vira_number (count vira)
  ;; ;; Mode develop

  ;pre-learn virus_antigene func_virus spm_virus rwm_virus pnl_virus
  ;;
  create-vira 5 [set color blue
    set xcor random-xcor
    set ycor random-ycor
    set countdown_respawn countdown_vira
                 set l_func -1
                 set func_virus func_survie
                 set virus_antigene survie_antigene
                 set n_antigene n_survie_antigene
                 set n_virus n_survie
                 set spm_virus spm_survie
                 set rwm_virus rwm_survie
                 set pnl_virus pnl_survie
                 set a_matrix_virus a_matrix_survie
                 set own_a_eq matrix:make-constant 1 n_virus 0
                 set own_a_norm matrix:make-constant 1 n_virus 0
                 set l_func -1
                 ]
  set vira_number (count vira)

  create-antibodies 50 [set color pink
                        set xcor random-xcor
                        set ycor random-ycor
                        set energy energy_antibody]

  ask patches [set pcolor white]

  reset-ticks
end

to go
  ask antibodies [move eat death]
  if any? vira [ask one-of vira [
  if lastc != -1 and countdown_i = 0 [learn spm_mode rwm_mode pnl_mode lastc func_mode 0
                          memorize n_mode spm_mode rwm_mode pnl_mode lastc 0 a_matrix_mode
                          set countdown_i countdown
                          set vira_number (count vira)
                         ]
  set lastc choose n_mode -1 func_mode a_eq_mode a_norm_mode a_matrix_mode alpha_mode beta_mode natural_death_mode
  act lastc func_mode]
  ]
  set countdown_i (countdown_i - 1)
  tick
end
;; Idée
;; if tick != 0 [
;; learn (arguments des virus + tampon_virus)
;; memorize (arguments des virus + tampon_virus) ]
;;
;; set tampon_virus (choose [argument des virus])
;;
;; On peut séparer le calcul des fonctions à sélectionner, de la définition du problème
;; Mettre à jour les fonctions bind et bind_learn lors de la définition d'une nouvelle fonctions, toutes les fonctions confondues

to pre-learn [antigenes function specific_matrix reward_matrix penalty_matrix]
  foreach function [let func ?
    foreach antigenes[
      let i 0
      while [i < 1000] [
        let antigene (position ? antigenes)
        let chosen (position func function)
        if-else chosen = antigene
        [matrix:set reward_matrix chosen antigene ((matrix:get reward_matrix chosen antigene) + 1)]
        [matrix:set penalty_matrix chosen antigene ((matrix:get penalty_matrix chosen antigene) + 1)]
        matrix:set specific_matrix chosen antigene ((matrix:get specific_matrix chosen antigene) + 1)
        set i (i + 1)
        ]
    ]
  ]
end

to learn [specific_matrix reward_matrix penalty_matrix chosen func antigene]
  ;;Regarde avec bind_learn la réussite ou l'echec d'une fonction et modifie les matrices correspondantes
  ;; antigene: int
  if-else (bind_learn (item chosen func))
  [matrix:set reward_matrix chosen antigene ((matrix:get reward_matrix chosen antigene) + 1)]
  [matrix:set penalty_matrix chosen antigene ((matrix:get penalty_matrix chosen antigene) + 1)]
  matrix:set specific_matrix chosen antigene ((matrix:get specific_matrix chosen antigene) + 1)
end

to memorize [n specific_matrix reward_matrix penalty_matrix chosen antigene a_matrix]
  ;; change dans a_matrix les coefficients après les changements apportés par learn
  let i 0
  while [i < n] [let j 0
                 while [j < n] [if (matrix:get specific_matrix i antigene + (matrix:get specific_matrix j antigene) ) != 0 [matrix:set a_matrix i j ( ((matrix:get penalty_matrix i antigene) + (matrix:get reward_matrix j antigene)) / ((matrix:get specific_matrix i antigene) + (matrix:get specific_matrix j antigene)))]
                                set j (j + 1)
                                ]
                 set i (i + 1)
                 ]
end

to-report choose [n antigene func A_eq a_normalised a_matrix alpha0 beta0 natural_death0]
  ;; Applique la formule et sélectionne la fonction étant la plus concentré
  ;; n: int; chosen: int; func: list; A_eq: matrix; a_normalised: matrix; a_matrix: matrix
  let chosen -1
  if n > 0[
    let i 0
    while [i < n] [let j 0
                   let direct_stimul 0
                   let stimul_sum 0
                   let inhib_sum 0
                   if i = antigene [set direct_stimul 1]
                   while [j < n] [if i != j [
                                    set stimul_sum ( stimul_sum + ((matrix:get a_matrix j i) * (matrix:get a_normalised 0 j)) )
                                    set inhib_sum ( inhib_sum + ((matrix:get a_matrix i j) * (matrix:get a_normalised 0 j)) )
                                    ]
                                  set j (j + 1)
                   ]
                   matrix:set A_eq 0 i ( ( (alpha0 * (1 / (n - 1)) * stimul_sum) - (alpha0 * (1 / (n - 1)) * inhib_sum) + (beta0 * direct_stimul) - natural_death0 ) * (matrix:get A_normalised 0 i) + (matrix:get A_eq 0 i) )
                   if (matrix:get A_eq 0 i) < -8
                   [matrix:set A_eq 0 i -8]
                   matrix:set a_normalised 0 i (1 / (1 + exp (0.5 - (matrix:get A_eq 0 i))) )
                   set i (i + 1)
                   ]
    set chosen (get-max n a_normalised)
    ]
  report chosen
end

to-report get-max [n a_normalised]
  let maxi 0
  let i 0
  while [i < n] [if matrix:get a_normalised 0 i >= matrix:get a_normalised 0 maxi [set maxi i]
                 set i (i + 1)
                 ]
  report maxi
end

to act [chosen func]
  ;;applique la fonction à l'indice chosen dans func
  ;; chosen : int, func: list of string
  bind (item chosen func)
end

to bind [s]
  ;;Associe s à la bonne fonction
  ;; s: string
  if s = "move-right" [move-right]
  if s = "move-left" [move-left]
  if s = "move-up" [move-up]
  if s = "move-down" [move-down]
  if s = "survive" [survive]
  if s = "develop" [develop]
end

to-report bind_learn [s]
  ;;Associe s à la fonction d'apprentissage
  ;; s: string
  if s = "move-right" [report move-right-learn]
  if s = "move-left" [report move-left-learn]
  if s = "move-down" [report move-down-learn]
  if s = "move-up" [report move-up-learn]
  if s = "survive" [report survive-learn]
  if s = "develop" [report develop-learn]
  ;report true
end




;;; FONCTIONS DES AGENTS

;; ;; Antibodies
to move
  rt random 50
  lt random 50
  fd 1
  set energy (energy - 1)
end

to eat
  if any? vira-here [
    ask one-of vira-here [die]
    ;hatch 1
 ]
end

to death
  ;if energy = 0 [die]
end
;; ;; Vira
to-report find-relative-position
  ;; trouve la position relative de l'anticorps au virus
  if-else not (any? antibodies-on neighbors) [report -1]
  [set danger (one-of antibodies-on neighbors)
   set heading (towards danger)
   if 45 <= heading and heading <= 135 [report "right"]
   if 135 < heading and heading <= 225 [report "down"]
   if 225 < heading and heading <= 315 [report "left"]
   if (315 < heading and heading <= 360) or (0 <= heading and heading < 45) [report "up"]
   report -1
  ]
end

to reproduce
  hatch 1
end

;;;

;;FONCTIONS

to move-right
  set ex_dist distance danger
  set heading 90
  fd 1
end

to-report move-right-learn
  report danger_anti = 0
end

;;

to move-left
  set ex_dist (distance danger)
  set heading 270
  fd 1
end

to-report move-left-learn
  report danger_anti = 1
end

;;

to move-down
  set ex_dist (distance danger)
  set heading 180
  fd 1
end

to-report move-down-learn
  report danger_anti = 3
end

;;

to move-up
  set ex_dist (distance danger)
  set heading 0
  fd 1
end

to-report move-up-learn
  report danger_anti = 2
end

to survive
    ask vira [
              let mem_rwm (matrix:copy rwm_virus)
              let mem_spm (matrix:copy spm_virus)
              let mem_pnl (matrix:copy pnl_virus)
              let antigene (position find-relative-position virus_antigene)
              if antigene != false and danger_anti != false [if l_func != -1 [learn spm_virus rwm_virus pnl_virus l_func func_virus danger_anti
                                                         memorize n_virus spm_virus rwm_virus pnl_virus l_func danger_anti a_matrix_virus
                                                     ]]
              set l_func choose n_virus antigene func_virus own_a_eq own_a_norm a_matrix_virus alpha beta natural_death
              if antigene != false [act l_func func_virus]
              set danger_anti antigene
              set spm_survie (matrix:plus (matrix:minus spm_virus mem_spm) spm_survie)
              set pnl_survie (matrix:plus (matrix:minus pnl_virus mem_pnl) pnl_survie)
              set rwm_survie (matrix:plus (matrix:minus rwm_virus mem_rwm) rwm_survie)
    ]
end

to-report survive-learn
  report (count vira - vira_number >= 0)
end

to develop
  ask vira [if count vira < 1000 and countdown_respawn = 0 [reproduce set countdown_respawn countdown_vira] set countdown_respawn (countdown_respawn - 1)]
end

to-report develop-learn
  report (count vira - vira_number >= 0)
end
@#$#@#$#@
GRAPHICS-WINDOW
182
16
621
476
16
16
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
5
15
172
48
alpha
alpha
0
10
10
0.1
1
NIL
HORIZONTAL

SLIDER
5
81
177
114
beta
beta
0
400
400
0.1
1
NIL
HORIZONTAL

SLIDER
4
48
176
81
natural_death
natural_death
0
100
100
0.1
1
NIL
HORIZONTAL

BUTTON
90
121
164
154
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
93
173
156
206
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
10
504
681
549
Reward matrix of virus
rwm_survie
17
1
11

MONITOR
10
551
685
596
Penalty matrix of virus
pnl_survie
17
1
11

MONITOR
8
601
684
646
Specific matrix of virus
spm_survie
17
1
11

BUTTON
115
310
170
343
Right
create-right
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
63
278
118
311
Up
create-up
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
64
343
119
376
Down
create-down
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
12
310
67
343
Left
create-left
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
58
398
121
431
NIL
kill
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
17
657
74
702
NIL
lastc
17
1
11

MONITOR
755
594
1001
639
NIL
pnl_mode
17
1
11

MONITOR
757
550
1000
595
NIL
rwm_mode
17
1
11

MONITOR
754
639
1003
684
NIL
spm_mode
17
1
11

MONITOR
755
505
1098
550
NIL
a_norm_mode
17
1
11

MONITOR
760
451
855
496
NIL
vira_number
17
1
11

SLIDER
189
695
361
728
countdown
countdown
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
219
737
394
770
energy_antibody
energy_antibody
0
100
58
1
1
NIL
HORIZONTAL

SLIDER
615
771
787
804
countdown_vira
countdown_vira
0
100
15
1
1
NIL
HORIZONTAL

SLIDER
825
210
997
243
alpha_mode
alpha_mode
0
100
100
0.1
1
NIL
HORIZONTAL

SLIDER
894
295
1066
328
beta_mode
beta_mode
0
1
1
0.01
1
NIL
HORIZONTAL

SLIDER
748
262
958
295
natural_death_mode
natural_death_mode
0
1
0
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
