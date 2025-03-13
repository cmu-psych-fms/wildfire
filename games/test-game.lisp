;;;; Copyright 2024 Carnegie Mellon University

(in-package :wildfire)

(defgame test-game (:height 300
                    :width 300
                    :ignitions ((:x 155 :y 150 :t 14)
                                (:x 190 :y 190 :t 47)
                                (:x 191 :y 198)
                                (:x 125 :y 120)
                                (:x 164 :y 147 :t 100))
                    :costs (:tree 5 :house 20)
                    :duration 600)
         (:forest ("Sherwood Forest") 100 100  130 100  145 145  110 140  100 125)
         (:lake ("Loch Ness") 155 155  180 165  170 175  160 168  145 160)
         (:river ("Nile") 170 190  160 120  164 160)
         (:road ("Lincoln Highway") 140 100  104 199)
         (:outcrop ("Bear Rocks") 145 144  149 146  147 151  146 149)
         (:houses ("Levittown") 161 145 169 145  169 149  161 149)
         (:houses ("Houses 2") 110 110  112 110  112 113  110 113))
