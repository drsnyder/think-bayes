(ns think-bayes.suite
  (:require (think-stats [hist :as hist])))


(defprotocol SuiteProtocol
  (update-hypothesis [this hypothesis])
  (state [this])
  (probabilities [this])
  (probability [this hypothesis]))

(defrecord SuiteRecord [hypotheses likelihood state update-state]
  SuiteProtocol
  (update-hypothesis [this hypothesis]
    (let [likelihood (:likelihood this)
          updated (into {}
                        (for [[hypo prob] (:hypotheses this)]
                          [hypo (* prob (likelihood (:state this) hypothesis hypo))]))]
      (cond-> this
        true (assoc :hypotheses (hist/normalize-pmf updated))
        (and (:state this)
             (:update-state this)) (assoc :state (update-state (:state this) hypothesis)))))
  (state [this]
    (:state this))
  (probability [this hypothesis]
    (get (:hypotheses this) hypothesis))
  (probabilities [this]
    (:hypotheses this)))

(defn new-suite [hypotheses &{:keys [likelihood state update-state]}]
  (->SuiteRecord (hist/pmf hypotheses) likelihood state update-state))

