globals [green_killed yellow_killed pink_killed count_antigene countdown_immune antibody_respawn tmp ind chosen a_matrix reward_matrix penalty_matrix specific_matrix A_eq a_normalised k epitope_antigene]
;; a_matrix = affinity_matrix, reward_matrix contains rewards (positive answer to antigene), penalty_matrix contains failures (negative answer to antigene),
;; specific_matrix contains number of sollicitation from an antigene, A_eq contained A of the equation at t instant, a_normalised contained concentration

breed [antibodies antibody]
breed [antigenes antigene]

extensions [matrix]

antigenes-own [countdown_respawn]
turtles-own [epitope paratope energy]
patches-own [cell countdown]

to init
  ;; Matrix initialised
  set penalty_matrix matrix:from-row-list [[0 0 0] [0 0 0] [0 0 0]]
  set reward_matrix matrix:from-row-list [[0 0 0] [0 0 0] [0 0 0]]
  set specific_matrix matrix:from-row-list [[0 0 0] [0 0 0] [0 0 0]]
end

to setup
  clear-patches
  clear-turtles
  clear-plot

  set epitope_antigene 1
  ;; Immune System variables
  set a_matrix matrix:from-row-list [[0 0 0] [0 0 0] [0 0 0]]
  set A_eq matrix:from-row-list [[0 0 0]]
  if-else learning [set a_normalised matrix:from-row-list [[0.33 0.33 0.33]]] [set a_normalised matrix:from-row-list [[0 0 0]]]
  set k 1
  set countdown_immune immunity_rapidity
  set chosen -1
  choose

  ;; Environment variables
  set count_antigene 0
  ask patches [set pcolor orange
               set countdown countdown_cell]
  reset-ticks
end

to go
  set count_antigene 0

  ask antibodies [move
                  kill_cell
                  if energy = 0 [die]]
  ask antigenes [set count_antigene (count_antigene + 1)
                 move
                 eat
                 if energy = 0 [die]
                 ifelse countdown_respawn = 0 [split set countdown_respawn (random antigene_respawn)][set countdown_respawn (countdown_respawn - 1)]]
  ask patches [if countdown = 0 [set pcolor orange]
               set countdown (countdown - 1)]
  set antibody_respawn (antibody_respawn - 1)
  ;;if antibody_respawn = 0 [bone_create]
  set countdown_immune (countdown_immune - 1)
  if countdown_immune = 0 [learn choose]
  tick
end

to kill_cell
  let para paratope
  let epi 0
  if any? antigenes-here [
  ask one-of antigenes-here [set epi epitope
                             if epitope = para [die]]]
  if epi = para [split if color = green [set green_killed (green_killed + 1)] if color = pink [set pink_killed (pink_killed + 1)] if color = yellow [set yellow_killed (yellow_killed + 1)]]
end

to move
  set energy (energy - 1)
  rt random 50
  lt random 50
  fd 1
end

to create_antigene
    create-antigenes 5 [set epitope epitope_antigene00
                        set paratope (one-of (remove epitope [1 2 3]))
                        set countdown_respawn antigene_respawn
                        set energy energy_antigene
                        set color blue]
end

to split
  if-else is-antibody? self [hatch 1 [set energy (20 + random (energy_antibody - 20)) rt 180 fd 1]] [hatch 1 [set energy (energy_antigene) set countdown_respawn (random antigene_respawn) rt 180 fd 1]]
end

to eat
  let eat? false
  ask patch-here [if pcolor = orange [set eat? true set pcolor black set countdown countdown_cell]]
  if eat? [set energy (energy + 1)]
end

to make-antibodies [i j] ;; i paratope & epitode id, j number of antibodies
  if i = 1 [create-antibodies j [set paratope 1 set epitope 3 set color green set energy (20 + random (energy_antibody - 20))] matrix:set specific_matrix 0 (epitope_antigene - 1) ( (matrix:get specific_matrix 0 (epitope_antigene - 1)) + 1 )]
  if i = 2 [create-antibodies j [set paratope 2 set epitope 1 set color yellow set energy (20 + random (energy_antibody - 20))] matrix:set specific_matrix 1 (epitope_antigene - 1) ( (matrix:get specific_matrix 1 (epitope_antigene - 1)) + 1 )]
  if i = 3 [create-antibodies j [set paratope 3 set epitope 2 set color pink set energy (20 + random (energy_antibody - 20))] matrix:set specific_matrix 2 (epitope_antigene - 1) ( (matrix:get specific_matrix 2 (epitope_antigene - 1)) + 1 )]
end

to choose
  if not learning and count_antigene > -1[
  foreach [0 1 2] [let i ?
                   let direct_stimul 0
                   let stimul_sum 0
                   let inhib_sum 0
                   if ? = -1 and count_antigene > 0 [set direct_stimul 1]
                   foreach [0 1 2] [if i != ? [
                                    set stimul_sum ( stimul_sum + ((matrix:get a_matrix ? i) * (matrix:get a_normalised 0 ?)) )
                                    set inhib_sum ( inhib_sum + ((matrix:get a_matrix i ?) * (matrix:get a_normalised 0 ?)) )]]
                   matrix:set A_eq 0 ? ( ( (alpha * (1 / 2) * stimul_sum) - ineq * (alpha * (1 / 2) * inhib_sum) + (beta * direct_stimul) - natural_death ) * (matrix:get A_normalised 0 ?) + (matrix:get A_eq 0 ?) )
                   if matrix:get a_eq 0 ? < -2 [matrix:set a_eq 0 ? -2]
                   matrix:set a_normalised 0 ? (1 / (1 + exp (0.5 - (matrix:get A_eq 0 ?))) )]

             get-max]
  set countdown_immune immunity_rapidity
  act
end

;+ (beta * direct_stimul) - k

to act
  if learning or roulette [let somme 0
               foreach [0 1 2] [set somme (somme + (matrix:get a_normalised 0 ?))]
               let p random-float somme
               if p < matrix:get a_normalised 0 0 [set chosen 0]
               if p > matrix:get a_normalised 0 0 and p < (matrix:get a_normalised 0 0 + matrix:get a_normalised 0 1) [set chosen 1]
               if p > (matrix:get a_normalised 0 1 + matrix:get a_normalised 0 0)[set chosen 2]]
  set tmp chosen
  if chosen = 0 and count_antigene > 0 [make-antibodies 1 number_sent set ind 0]
  if chosen = 1 and count_antigene > 0 [make-antibodies 2 number_sent set ind 1]
  if chosen = 2 and count_antigene > 0 [make-antibodies 3 number_sent set ind 2]
end

to learn
  if count_antigene > 0 [
  if ((associate tmp) = 0) [matrix:set penalty_matrix ind (epitope_antigene - 1) ((matrix:get penalty_matrix ind (epitope_antigene - 1)) + 1)] ;; penalty point to antibody i
  if ((associate tmp) > 0) [matrix:set reward_matrix ind (epitope_antigene - 1) ((matrix:get reward_matrix ind (epitope_antigene - 1)) + 1)] ;; good point added to antibody i
  set green_killed 0
  set yellow_killed 0
  set pink_killed 0
  memorize ];; now we modify affinity_matrix
end

to memorize
  if not learning [
  foreach [0 1 2] [let i ?
                   foreach [0 1 2] [if ((matrix:get specific_matrix i (epitope_antigene - 1) + (matrix:get specific_matrix ? (epitope_antigene - 1)))) != 0 [matrix:set a_matrix i ? ( ((matrix:get penalty_matrix i (epitope_antigene - 1)) + (matrix:get reward_matrix ? (epitope_antigene - 1))) / ((matrix:get specific_matrix i (epitope_antigene - 1) + (matrix:get specific_matrix ? (epitope_antigene - 1)))) )]]]]
end

to get-max
  let maxi 0
  foreach [0 1 2] [if matrix:get a_normalised 0 ? >= matrix:get a_normalised 0 maxi [set maxi ?]]
  set chosen maxi
end

to kill_antigenes
  ask antigenes [die]
end

to-report associate [i]
  if i = 0 [report green_killed]
  if i = 1 [report yellow_killed]
  if i = 2 [report pink_killed]
  report -1
end
@#$#@#$#@
GRAPHICS-WINDOW
520
10
959
470
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

BUTTON
164
10
228
43
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
228
10
291
43
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

BUTTON
389
10
506
43
Create an antigene
create_antigene
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
8
166
506
470
Number of antibodies
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Antibodies 2" 1.0 0 -1184463 true "" "let y 0 ask antibodies [if color = yellow [set y (y + 1)]] plot y"
"Antibodies 3" 1.0 0 -2064490 true "" "let p 0 ask antibodies [if color = pink [set p (p + 1)]] plot p"
"Antibodies 1" 1.0 0 -10899396 true "" "let g 0 ask antibodies [if color = green [set g (g + 1)]] plot g"
"Antigene" 1.0 0 -13345367 true "" "plot count_antigene"

SLIDER
309
124
507
157
antigene_respawn
antigene_respawn
0
100
50
1
1
tick
HORIZONTAL

SLIDER
111
48
291
81
immunity_rapidity
immunity_rapidity
0
100
33
1
1
tick
HORIZONTAL

CHOOSER
309
47
507
92
epitope_antigene00
epitope_antigene00
1 2 3
0

SLIDER
111
81
291
114
energy_antibody
energy_antibody
20
100
91
1
1
NIL
HORIZONTAL

BUTTON
308
10
389
43
Kill antigenes
kill_antigenes
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
109
10
164
43
init
init
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
13
10
103
43
learning
learning
1
1
-1000

SLIDER
309
91
507
124
energy_antigene
energy_antigene
0
100
33
1
1
NIL
HORIZONTAL

SLIDER
111
114
291
147
countdown_cell
countdown_cell
0
100
48
1
1
tick
HORIZONTAL

SWITCH
13
43
103
76
roulette
roulette
1
1
-1000

MONITOR
966
68
1479
113
Reward Matrix (example of element : e(2,1) = antibody 3 use with antigene 2)
reward_matrix
17
1
11

MONITOR
966
112
1479
157
Penalty_matrix
penalty_matrix
17
1
11

MONITOR
966
23
1479
68
Concentration of functions (element : 0 for function which make antibody 1 etc.)
a_normalised
17
1
11

MONITOR
966
157
1479
202
Total Matrix
specific_matrix
17
1
11

BUTTON
431
247
500
280
Clear plot
clear-plot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
971
209
1102
227
Matrix are indexed from 0
11
0.0
0

SLIDER
967
241
1139
274
natural_death
natural_death
0
1
1
0.01
1
NIL
HORIZONTAL

SLIDER
967
277
1139
310
beta
beta
0
10
0.83
0.01
1
NIL
HORIZONTAL

SLIDER
966
313
1138
346
alpha
alpha
0
10
1
0.1
1
NIL
HORIZONTAL

SLIDER
2
489
174
522
number_sent
number_sent
0
100
10
1
1
NIL
HORIZONTAL

SLIDER
965
346
1137
379
ineq
ineq
0
1
1
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
# Immune system

## WHAT IS IT?
This is a representation of an immune system. Here we have three types of antibodies which struggle against three types of antigene (virus). The program has to find the correct antibody to use by machine learning.

## HOW IT WORKS

### Environment

   World is composed of patches which represent cells of human body in orange (they could represent health of someone, the less they are, the less they have good fitness). You can control the reappearance of these whith **countdown_cell** parameter.
A description of the two main agent :

- Antigenes eat cells to survive and increase their energy (controlled by **energy_antigene**). They can split each **antigene_respawn** ticks. Antigenes die when their energy reach 0. To spread them in the world press _Create an antigene_ button after choosing an *epitope*. **Don't spread several antigenes with different epitope in the same time.**

- Antibody can kill virus only if they have a paratope adapted to the *epitope* of the antigenes spread, in this case the antibody is stimulated and _split_. This is what the algorithm have to find. They born with an energy equals to **energy_antibody** and they can't split without stimulation.

### Program

   We have to separate these two worlds. In fact, in the world of program antigenes and antibodies are not those in the environment. Antigenes are parameters of the environment (more precisely here epitope chosen in the menu bar **epitope_antigene** and the number of antigenes in the body). Antibodies are the function used to make the antigenes parameters in the program world disappearing (here three functions wich create 10 antibodies with paratope 1, or 2, or 3)

  We calculate the "concentration" of antibodies (function) in program world. Actually, like in the immune system of Jerne, we calculate the stimulation between function thanks to a rewards and panalties memory. We memorize the number of time the response of a function was good or bad to a specific antigene. To do that, the main important globals variables are :

- *specific_matrix* : contains the number of time we call a function to respond to an antigene stimulation
- *reward_matrix* : contains the number of time a function respond positively to an antigene stimulation
- *penalty_matrix* : contains the number of time a function respond negatively to an antigene stimulation
- *a_normalised* : contains the concentration of antibodies.

We analyse the results of function each **immunity_rapidity** ticks. And we count the number of antigenes which had been killed by the antibodies of the called function (>0 positive answer, =0 negative answer).

   After we use the equation in section 3.5 in the work of [Ishiguro et al.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.9687&rep=rep1&type=pdf) modified a bit (to assure the convergence of equation, assuming that the third and forth term are equal). The calcul of mij is given in this work section 4.1 and can be easily understood.Finally, we choose the most concentrate function in normal mode but you can choose it by roulette-wheel-selection in roulette mode.


The following was a problem, because learning phase seems to be useless.
(The main problem is the initialisation of the matrix (*specific_matrix*, *reward_matrix* and *penalty_matrix*). That's why there is an *init* button which set the matrix to 0, only when we press it, the setup does not set the matrix to 0. We made that to be able to repeat the experience without do a learning each time. To do that, we have put *learning* switch, in this mode the three function are chosen uniformly and randomly. Then we create antigenes and wait that they all die. We repeat this operation and change the **epitope_antigene** to fill the matrix. 10 times per each is good. After leave the learning mode and observe the reaction of immune system.)

## HOW TO USE IT

If you have not made the learning mode, switch to learning mode and press init, setup, go and put antigenes in the environment, repeat and change their epitope-antigene only when they all die. We remarked that this phase is useless.

Left the learning mode, and just setup and go, choose an epitope for your antigene by using **epitope_antigene** selection, and put it.

You can set some parameters, check the section environment before.

####Recommended
- antigene_respawn : 25
- immunity_rapidity : 25
- energy_antibody : 50
- countdown_cell : 20
- energy_antigene : 40

## THINGS TO NOTICE

We change a bit the original equation to make the concentration not converging to 1.

## THINGS TO TRY

Put two antigene with different epitope at the same time.

## NETLOGO FEATURES

We use matrix package in NETlogo.

## RELATED MODELS

Wolf and sheep predation, virus are almost like sheep.

## CREDITS AND REFERENCES

###For the model itself:

- Ishiguro et al., [*A Robot with decentralized consensus-making mechanism based on the Immune Sysyem*](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.9687&rep=rep1&type=pdf)
- Doyne FARMER and Norman H. Packard, *The Immune System, adaptation, and machine learning
- G. Di Marzo Serugendo, M.-P. Gleizes and A. Karageorgos, *Self-organising Software* Chap. 10

###Netlogo software:
Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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
