extensions [matrix]

breed [antibodies antibody]
breed [vira virus]

patches-own [growth_time]
antibodies-own [energy]
vira-own [energy last_energy split_time unedible l_func own_a_eq own_a_norm spm_virus rwm_virus pnl_virus a_matrix_virus n_virus n_antigene virus_antigene func_virus antigene_learn]
;; growth_time : time needed for cells to regenerate
;; energy : the energy of virus or antibodies.
;; split_time : time between reproduction
;; unedible : makes the virus unedible for an antibody for one time
;; l_func : last function used.
;; own_a_eq : A from the equation proper to each virus.
;; own_a_norm : A from the equation normalised.
;; spm_virus : specific matrix
;; rwm_virus : reward matrix
;; pnl_virus : penalty matrix
;; a_matrix_virus : matrix of weight (mij in the equation).
;; n_virus : number of usable functions
;; n_antigene : number of antigene able to attack the algorithmic immune system.
;; virus_antigene : name of the antigene
;; func_virus : name of the functions
;; antigene_learn : keep the antigene used in the algorithm

globals [
;; Strategy variables
func_mode mode_antigene vira_number n_mode_antigene n_mode spm_mode pnl_mode rwm_mode a_matrix_mode a_eq_mode a_norm_mode lastc countdown_i
cells_number virus_number antibodies_number antigene_mode

rmd_antibodies rmd_virus rmd_cells

;; Duplicate variables
func_survie survie_antigene n_survie_antigene n_survie spm_survie rwm_survie pnl_survie a_matrix_survie

;; Expansion variables
func_exp exp_antigene n_exp_antigene n_exp spm_exp rwm_exp pnl_exp a_matrix_exp

;; Reduction variables
func_red red_antigene n_red_antigene n_red spm_red rwm_red pnl_red a_matrix_red

]

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;SETUP;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ;;;CLEANING;;;
  clear-patches
  clear-turtles
  clear-plot

  ;; Fist level variables
  set func_mode ["duplicate" "expansion" "reduction"]
  set mode_antigene ["virus_number" "cells_number" "antibodies_number"]
  set n_mode_antigene 3
  set n_mode 3
  set spm_mode matrix:make-constant n_mode n_mode_antigene 0
  set rwm_mode matrix:make-constant n_mode n_mode_antigene 0
  set pnl_mode matrix:make-constant n_mode n_mode_antigene 0
  set a_matrix_mode matrix:make-constant n_mode n_mode 0
  set a_eq_mode matrix:make-constant 1 n_mode 0
  set a_norm_mode matrix:make-constant 1 n_mode 0
  set lastc -1

  set countdown_i 0
  ;; Second level variables
  ;; ;; Duplicate mode
  set func_survie ["hide" "split"]
  set survie_antigene ["threat" "no-threat"]
  set n_survie_antigene 2
  set n_survie 2
  set spm_survie matrix:make-constant n_survie n_survie_antigene 0
  set rwm_survie matrix:make-constant n_survie n_survie_antigene 0
  set pnl_survie matrix:make-constant n_survie n_survie_antigene 0
  set a_matrix_survie matrix:make-constant n_survie n_survie 0

  ;; ;; Expansion mode
  set func_exp ["move" "eat"]
  set exp_antigene ["threat" "no-threat"]
  set n_exp_antigene 2
  set n_exp 2
  set spm_exp matrix:make-constant n_exp n_exp_antigene 0
  set rwm_exp matrix:make-constant n_exp n_exp_antigene 0
  set pnl_exp matrix:make-constant n_exp n_exp_antigene 0
  set a_matrix_exp matrix:make-constant n_exp n_exp 0

  ;; ;; Reduction mode
  set func_red ["freeze" "autokill" "starve"]
  set red_antigene ["threat" "friend" "nothing"]
  set n_red_antigene 3
  set n_red 3
  set spm_red matrix:make-constant n_red n_red_antigene 0
  set rwm_red matrix:make-constant n_red n_red_antigene 0
  set pnl_red matrix:make-constant n_red n_red_antigene 0
  set a_matrix_red matrix:make-constant n_red n_red 0

  ;;;CREATION AND INITIALIZATION OF AGENTS;;;
  create-vira 5
  [ set color blue
    set xcor random-xcor
    set ycor random-ycor
    set energy (random energy_vira)
    set split_time 0
    set unedible false ]

  create-antibodies 5
  [ set color white
    set xcor random-xcor
    set ycor random-ycor
    set energy (random energy_antibodies) ]

  ask patches
  [ set pcolor black
    set growth_time 0 ]

  set cells_number (count patches with [pcolor = black])
  set virus_number (count vira)
  set antibodies_number (count antibodies)


  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;GO;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  choose-strategy

  ask antibodies
  [ eat-vira
    move
    death ]

  ask patches
  [ growth ]

  tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;FUNCTIONS OF AGENTS;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;;;VIRA FUNCTIONS;;;
;;;;;;;;;;;;;;;;;;;;

to eat-cells
  ;; Vira kill cells
  let eaten? false
  ask patch-here
  [ if pcolor = black
     [ set pcolor grey
       set growth_time cells_respawn
       set eaten? true
       if (random-float 1 ) > 0.95
       [ sprout-antibodies 1
         [ set color white
           set xcor random-xcor
           set ycor random-ycor
           set energy (random energy_antibodies)
         ]
       ]
     ]
  ]

  if eaten?
  [ set energy (energy + (random cells_bonus)) ]
end

to split-vira
  let parent self
  if split_time = 0 and (count vira < 5000)
  [ hatch 1
    [ set split_time (random vira_reproduction_countdown)
      create-link-with parent
    ]
    set split_time (random vira_reproduction_countdown)
    set energy (energy / 2)
  ]
end

to hide
  set unedible true
end

to freeze
end

to autokill
  eat-vira
end

to starve
  move
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;ANTIBODIES FUNCTIONS;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to eat-vira
  ;; Antibodies eat vira
  let eaten? false
  if any? vira-here
  [ ask one-of vira-here [die]
    set eaten? true ]

  if eaten?
  [ set energy (energy + (random vira_bonus))
    split ]
end

;;;;;;;;;;;;;;;;;;;;;;
;;;COMMON FUNCTIONS;;;
;;;;;;;;;;;;;;;;;;;;;;

to split
  ;;Split an agent

  hatch 1
  [ set energy (random energy_antibodies) ]
end

to move
  rt random 50
  lt random 50
  fd 1
  set energy (energy - 1)
end

to death
  if energy <= 0 [die]
end

;;;;;;;;;;;;;;;;;;;;;;;
;;;PATCHES FUNCTIONS;;;
;;;;;;;;;;;;;;;;;;;;;;;

to growth
  set growth_time (growth_time - 1)
  if growth_time = 0 and pcolor != black
  [ set pcolor black
    set growth_time (random cells_respawn) ]
end




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IMMUNE SYSTEM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;First level
  if s = "duplicate" [duplicate]
  if s = "expansion" [expansion]
  if s = "reduction" [reduction]

  ;; Second level
  ;; ;; Duplicate
  if s = "split" [split-vira]
  if s = "hide" [hide]

  ;; ;; Expansion
  if s = "move" [move]
  if s = "eat" [eat-cells]

  ;; ;; Reduction
  if s = "freeze" [freeze]
  if s = "autokill" [autokill]
  if s = "starve" [starve]
end

to-report bind_learn [s]
  ;;Associe s à la fonction d'apprentissage
  ;; s: string

  ;;First level
  if s = "duplicate" [report duplicate-learn]
  if s = "expansion" [report expansion-learn]
  if s = "reduction" [report reduction-learn]

  ;; Second level
  ;; ;; Duplicate
  if s = "split" [report split-learn]
  if s = "hide" [report hide-learn]

  ;; ;; Expansion
  if s = "move" [report move-learn]
  if s = "eat" [report eat-learn]

  ;; ;; Reduction
  if s = "freeze" [report freeze-learn]
  if s = "autokill" [report autokill-learn]
  if s = "starve" [report starve-learn]
  ;report true
end

to choose-strategy
  if any? vira
  [ ask one-of vira
    [ if countdown_i = 0
      [ if lastc != -1
        [ learn spm_mode rwm_mode pnl_mode lastc func_mode antigene_mode
          memorize n_mode spm_mode rwm_mode pnl_mode lastc antigene_mode a_matrix_mode
          set vira_number (count vira) ]
      set rmd_antibodies (count antibodies)
      set rmd_virus (count vira)
      set rmd_cells (count patches with [pcolor = black])
      set antigene_mode compute-antigene-strategy
      set lastc choose n_mode antigene_mode func_mode a_eq_mode a_norm_mode a_matrix_mode alpha_mode beta_mode natural_death_mode
      set countdown_i countdown
      ]
      if lastc != -1 [act lastc func_mode]
      set countdown_i (countdown_i - 1)
    ]
  ]

end

to-report compute-antigene-strategy
  set cells_number (rmd_cells)
  set virus_number rmd_virus
  set antibodies_number rmd_antibodies
  let total (cells_number + virus_number + antibodies_number)
  let cn (cells_number / total)
  let vn (virus_number / total)
  let an (antibodies_number / total)

  print "Antibodies"
  show an
  show antibodies_number
  print "Virus"
  show vn
  show virus_number
  print "Cells"
  show cn
  show cells_number

  let maxi cn
  if vn > maxi [set maxi vn]
  if an > maxi [set maxi an]

  if vn >= an and vn <= cn [report 0]
  if cn <= vn and vn >= an [report 1]
  if an >= vn [report 2]
end

to duplicate
  if countdown_i = countdown
  [ ask vira
    [ set l_func -1
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
      set antigene_learn true
    ]
  ]
  ask vira
    [ let mem_rwm (matrix:copy rwm_virus)
      let mem_spm (matrix:copy spm_virus)
      let mem_pnl (matrix:copy pnl_virus)

      if antigene_learn != false
      [ if l_func != -1
        [ learn spm_virus rwm_virus pnl_virus l_func func_virus antigene_learn
          memorize n_virus spm_virus rwm_virus pnl_virus l_func antigene_learn a_matrix_virus
        ]
      ]
      set antigene_learn 1
      if any? antibodies-on neighbors or any? antibodies-here
      [ set antigene_learn 0 ]
      set l_func choose n_virus antigene_learn func_virus own_a_eq own_a_norm a_matrix_virus alpha_survie beta_survie natural_death_survie
      if split_time > 0 [set split_time (split_time - 1)]
      if antigene_learn != false [act l_func func_virus]
      set spm_survie (matrix:plus (matrix:minus spm_virus mem_spm) spm_survie)
      set pnl_survie (matrix:plus (matrix:minus pnl_virus mem_pnl) pnl_survie)
      set rwm_survie (matrix:plus (matrix:minus rwm_virus mem_rwm) rwm_survie)
      death
    ]
end

to expansion
    if countdown_i = countdown
  [ ask vira
    [ set l_func -1
      set func_virus func_exp
      set virus_antigene exp_antigene
      set n_antigene n_exp_antigene
      set n_virus n_exp
      set spm_virus spm_exp
      set rwm_virus rwm_exp
      set pnl_virus pnl_exp
      set a_matrix_virus a_matrix_exp
      set own_a_eq matrix:make-constant 1 n_virus 0
      set own_a_norm matrix:make-constant 1 n_virus 0
      set l_func -1
      set antigene_learn true
      set last_energy energy
    ]
  ]
  ask vira
    [ let mem_rwm (matrix:copy rwm_virus)
      let mem_spm (matrix:copy spm_virus)
      let mem_pnl (matrix:copy pnl_virus)

      if antigene_learn != false
      [ if l_func != -1
        [ learn spm_virus rwm_virus pnl_virus l_func func_virus antigene_learn
          memorize n_virus spm_virus rwm_virus pnl_virus l_func antigene_learn a_matrix_virus
        ]
      ]
      set antigene_learn 1
      if any? antibodies-on neighbors or any? antibodies-here
      [ set antigene_learn 0 ]
      if ([pcolor] of patch-here) = grey
      [ set antigene_learn 0 ]
      set l_func choose n_virus antigene_learn func_virus own_a_eq own_a_norm a_matrix_virus alpha_exp beta_exp natural_death_exp
      set last_energy energy
      if antigene_learn != false [act l_func func_virus]
      set spm_exp (matrix:plus (matrix:minus spm_virus mem_spm) spm_exp)
      set pnl_exp (matrix:plus (matrix:minus pnl_virus mem_pnl) pnl_exp)
      set rwm_exp (matrix:plus (matrix:minus rwm_virus mem_rwm) rwm_exp)
      death
    ]
end

to reduction
      if countdown_i = countdown
  [ ask vira
    [ set l_func -1
      set func_virus func_red
      set virus_antigene red_antigene
      set n_antigene n_red_antigene
      set n_virus n_red
      set spm_virus spm_red
      set rwm_virus rwm_red
      set pnl_virus pnl_red
      set a_matrix_virus a_matrix_red
      set own_a_eq matrix:make-constant 1 n_virus 0
      set own_a_norm matrix:make-constant 1 n_virus 0
      set l_func -1
      set antigene_learn true
      set last_energy energy
    ]
  ]
  ask vira
    [ let mem_rwm (matrix:copy rwm_virus)
      let mem_spm (matrix:copy spm_virus)
      let mem_pnl (matrix:copy pnl_virus)

      if antigene_learn != false
      [ if l_func != -1
        [ learn spm_virus rwm_virus pnl_virus l_func func_virus antigene_learn
          memorize n_virus spm_virus rwm_virus pnl_virus l_func antigene_learn a_matrix_virus
        ]
      ]
      set antigene_learn 2
      if any? antibodies-on neighbors or any? antibodies-here
      [ set antigene_learn 0 ]
      if any? vira-on neighbors or any? vira-here
      [ if (count vira-on neighbors) + (count vira-here) >= (count antibodies-on neighbors) + (count antibodies-here)
        [ set antigene_learn 1 ]
      ]
      set l_func choose n_virus antigene_learn func_virus own_a_eq own_a_norm a_matrix_virus alpha_red beta_red natural_death_red
      set last_energy energy
      if antigene_learn != false [act l_func func_virus]
      set spm_red (matrix:plus (matrix:minus spm_virus mem_spm) spm_red)
      set pnl_red (matrix:plus (matrix:minus pnl_virus mem_pnl) pnl_red)
      set rwm_red (matrix:plus (matrix:minus rwm_virus mem_rwm) rwm_red)
      death
    ]
end

to-report duplicate-learn
  report rmd_virus < (count vira) and (count vira) < 1500
end

to-report expansion-learn
  report rmd_cells >= (count patches with [pcolor = black]) or antigene_mode = 1
end

to-report reduction-learn
  report rmd_antibodies < (count antibodies)
end

to-report hide-learn
  if-else unedible
  [ set unedible false
    report false ]
  [ report true ]
end

to-report split-learn
  let answer false
  if one-of link-neighbors != nobody
  [ ask one-of link-neighbors [set answer true] ]
  if split_time != 0 [set answer true ]
  report answer
end

to-report move-learn
  if antigene_learn = 2
  [report true]
  if antigene_learn = 1
  [report (true and last_energy != 0)]
  if antigene_learn = 0
  [report false]
end

to-report eat-learn
  report last_energy <= energy
end

to-report freeze-learn
  report energy = last_energy
end

to-report autokill-learn
  report energy > last_energy
end

to-report starve-learn
  report last_energy > energy
end
@#$#@#$#@
GRAPHICS-WINDOW
263
10
702
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

SLIDER
419
762
688
795
vira_reproduction_countdown
vira_reproduction_countdown
0
100
40
1
1
NIL
HORIZONTAL

SLIDER
197
763
369
796
energy_vira
energy_vira
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
197
795
386
828
energy_antibodies
energy_antibodies
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
419
795
591
828
cells_respawn
cells_respawn
0
100
50
1
1
NIL
HORIZONTAL

BUTTON
6
22
79
55
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
796
177
829
cells_bonus
cells_bonus
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
5
763
177
796
vira_bonus
vira_bonus
0
100
50
1
1
NIL
HORIZONTAL

BUTTON
6
55
69
88
NIL
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

TEXTBOX
6
744
156
762
Eating bonus
12
0.0
1

TEXTBOX
197
743
347
761
Energy of agents
12
0.0
1

TEXTBOX
422
742
572
760
Respawn parameters
12
0.0
1

SLIDER
732
39
924
72
countdown
countdown
0
100
7
1
1
NIL
HORIZONTAL

TEXTBOX
729
17
879
35
Strategy parameters
12
0.0
1

SLIDER
732
72
924
105
alpha_mode
alpha_mode
0
10
5.5
0.1
1
NIL
HORIZONTAL

SLIDER
731
105
924
138
beta_mode
beta_mode
0
1
0.7
0.01
1
NIL
HORIZONTAL

SLIDER
731
137
924
170
natural_death_mode
natural_death_mode
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
13
178
185
211
alpha_survie
alpha_survie
0
10
5.2
0.1
1
NIL
HORIZONTAL

SLIDER
13
210
185
243
beta_survie
beta_survie
0
1
1
0.01
1
NIL
HORIZONTAL

SLIDER
14
242
245
275
natural_death_survie
natural_death_survie
0
1
0.53
0.01
1
NIL
HORIZONTAL

MONITOR
732
264
1216
309
NIL
a_norm_mode
17
1
11

MONITOR
729
220
1215
265
NIL
a_eq_mode
17
1
11

MONITOR
730
410
1056
455
NIL
spm_mode
17
1
11

MONITOR
731
361
1053
406
NIL
rwm_mode
17
1
11

MONITOR
732
312
1053
357
NIL
pnl_mode
17
1
11

SLIDER
1037
623
1209
656
alpha_exp
alpha_exp
0
10
6
0.1
1
NIL
HORIZONTAL

SLIDER
1037
656
1209
689
beta_exp
beta_exp
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
1037
689
1227
722
natural_death_exp
natural_death_exp
0
1
0.37
0.01
1
NIL
HORIZONTAL

SLIDER
1574
597
1746
630
alpha_red
alpha_red
0
10
4.9
0.1
1
NIL
HORIZONTAL

SLIDER
1575
629
1747
662
beta_red
beta_red
0
1
0.48
0.01
1
NIL
HORIZONTAL

SLIDER
1575
662
1769
695
natural_death_red
natural_death_red
0
1
0.26
0.01
1
NIL
HORIZONTAL

MONITOR
729
175
883
220
Mode
item lastc func_mode
17
1
11

TEXTBOX
12
149
162
167
Duplicate parameters\n
12
0.0
1

MONITOR
350
650
428
695
NIL
count vira
17
1
11

MONITOR
881
176
1132
221
antigene
item antigene_mode mode_antigene
17
1
11

PLOT
1232
134
1738
471
plot 1
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot matrix:get a_norm_mode 0 0"
"pen-1" 1.0 0 -7500403 true "" "plot matrix:get a_norm_mode 0 1"
"pen-2" 1.0 0 -2674135 true "" "plot matrix:get a_norm_mode 0 2"

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
