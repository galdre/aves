(ns aves.processor)

(def ^:dynamic *event-processors* #{})

(defprotocol EventProcessor
  (processes? [processor non-final-data] "Takes the non-finalized event map;
  returns true/false. Should not evaluate any event-thunks, or
  dereference any delays, contained within the map.")
  (process! [processor data] "Expects the finalized event data. Does
  whatever side effecty things it so desires. Should not be blocking."))

(extend-protocol EventProcessor
  clojure.lang.Atom
  (processes? [_ _] true)
  (process! [this data]
    (swap! this conj data))

  clojure.lang.Fn
  (processes? [_ _] true)
  (process! [this data] (this data)))

(defn assert-processor!
  [object]
  (assert (satisfies? EventProcessor object)))
