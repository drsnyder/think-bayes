(ns think-bayes.suite-test
  (:use [midje.sweet])
  (:require (think-bayes [suite :as suite])))

(facts :monty
  (let [monty (suite/new-suite ["A" "B" "C"]
                               :likelihood (fn [_ selection hypo]
                                             (condp = hypo
                                               selection 0
                                               "A"  0.5
                                               1)))
        updated-monty (suite/update-hypothesis monty "B")]
        (suite/probability updated-monty "B") => (roughly 0.0)
        (suite/probability updated-monty "A") => (roughly 0.33333)
        (suite/probability updated-monty "C") => (roughly 0.66666)))

(facts :m_and_m
  (let [mix94 {:brown 30
               :yellow 20
               :red 20
               :green 10
               :orange 10
               :tan 10}
        mix96 {:blue 24
               :green 20
               :orange 16
               :yellow 14
               :red 13
               :brown 13
               }
        hypoA {:bag1 mix94
               :bag2 mix96}
        hypoB {:bag1 mix96
               :bag2 mix94}
        hypotheses {"A" hypoA "B" hypoB}
        likelihood (fn [bowls selection hypo]
                     (let [[bag color] selection
                           like (get-in bowls [hypo bag color])]
                       like))
        update-state (fn [state selection]
                       ; this is missing one level-- we need to decrement the leaf
                       ; in each hypothesis
                       (reduce (fn [state hypo]
                                 (update-in state (conj (seq selection) hypo) dec))
                               state
                               (keys state)))
        mandm (suite/new-suite ["A" "B"] :likelihood likelihood :state hypotheses :update-state update-state)
        updated-mandm (suite/update-hypothesis mandm [:bag1 :yellow])
        updated-mandm (suite/update-hypothesis updated-mandm [:bag2 :green])]
    (suite/probability updated-mandm "A") => (roughly 0.74074)
    (suite/probability updated-mandm "B") => (roughly 0.25925)
    (get-in (suite/state updated-mandm) ["A" :bag1 :yellow]) => 19
    (get-in (suite/state updated-mandm) ["A" :bag2 :green]) => 19
    (get-in (suite/state updated-mandm) ["B" :bag1 :yellow]) => 13
    (get-in (suite/state updated-mandm) ["B" :bag2 :green]) => 9))

; 3.1
(facts :dice
  (let [likelihood (fn [_ selection hypo]
                     (if (> selection hypo)
                       0
                       (/ 1.0 hypo)))
        dice (suite/new-suite [4 6 8 12 20] :likelihood likelihood)
        updated-dice (reduce suite/update-hypothesis dice [6 6 8 7 7 5 4])]
    ; test
    (clojure.pprint/pprint (suite/probabilities updated-dice))))
