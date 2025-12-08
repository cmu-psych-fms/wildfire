;;;; Copyright 2024–2025 Carnegie Mellon University

(in-package :wildfire)

(defgame model-game (:model test-model
                     :height 300
                     :width 300
                     :ignitions ((:x 155 :y 150 :t 4)
                                 (:x 190 :y 190 :t 7)
                                 (:x 191 :y 198)
                                 (:x 125 :y 120)
                                 (:x 164 :y 147 :t 14))
                     :costs (:tree 5 :house 20)
                     :duration 600)
         (:forest ("sherwood-forest") 100 100  130 100  145 145  110 140  100 125)
         (:lake ("loch-ness") 155 155  180 165  170 175  160 168  145 160)
         (:river ("nile") 170 190  160 120  164 160)
         (:road ("lincoln-highway") 140 100  104 199)
         (:outcrop ("bear-rocks") 145 144  149 146  147 151  146 149)
         (:houses ("levittown") 161 145 169 145  169 149  161 149))
