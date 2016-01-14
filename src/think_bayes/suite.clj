(ns think-bayes.suite
  (:import [clojure.lang Associative Sequential LongRange])
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

(defmulti hypotheses->pmf type)

; if the input is a map we'll accept that as a pre-computed PMF
(defmethod hypotheses->pmf Associative [m] m)

; if the input is a sequence then convert it to a PMF
(defmethod hypotheses->pmf Sequential [s] (hist/pmf s))

; this is to handle a bug in think-stats-- the type dispatching in the hist
; multimethod is less nuanced
(defmethod hypotheses->pmf LongRange [s] (hist/pmf (vec s)))
(prefer-method hypotheses->pmf Sequential Associative)

(defn new-suite [hypotheses &{:keys [likelihood state update-state]}]
  ; FIXME: we need a way to initialize the suite with a custom built PMF
  ; is that a multi-method based on the type of input? if it's a seq then
  ; do what we are doing now if it's a map then use that directly?
  (->SuiteRecord (hypotheses->pmf hypotheses) likelihood state update-state))

