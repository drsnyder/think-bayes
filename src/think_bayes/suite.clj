(ns think-bayes.suite
  (:require (think-stats [hist :as hist])))


(defprotocol SuiteProtocol
  (update-hypothesis [this hypothesis])
  (probability [this hypothesis])
  (probabilities [this]))

(defrecord SuiteRecord [hypotheses likelihood]
  SuiteProtocol
  (update-hypothesis [this hypothesis]
    (let [likelihood (:likelihood this)
          updated (into {}
                        (for [[hypo prob] (:hypotheses this)]
                          [hypo (* prob (likelihood hypothesis hypo))]))]
      (assoc this :hypotheses (hist/normalize-pmf updated))))
  (probability [this hypothesis]
    (get (:hypotheses this) hypothesis))
  (probabilities [this]
    (:hypotheses this)))

(defn new-suite [hypotheses &{:keys [likelihood]}]
  (->SuiteRecord (hist/pmf hypotheses) likelihood))

