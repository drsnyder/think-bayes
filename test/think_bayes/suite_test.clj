(ns think-bayes.suite-test
  (:use [midje.sweet])
  (:require (think-bayes [suite :as suite])))

(facts :monty
  (let [monty (suite/new-suite ["A" "B" "C"]
                               :likelihood (fn [data hypo]
                                             (condp = hypo
                                               data 0
                                               "A"  0.5
                                               1)))
        updated-monty (suite/update-hypothesis monty "B")]
        (suite/probability updated-monty "B") => (roughly 0.0)
        (suite/probability updated-monty "A") => (roughly 0.33333)
        (suite/probability updated-monty "C") => (roughly 0.66666)))
