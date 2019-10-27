(ns aves.sink)

(def ^:dynamic *event-sinks* #{})

(defprotocol EventSink
  (captures? [sink non-final-data] "Takes the non-finalized event map;
  returns true/false. Should not evaluate any functions, or
  dereference any values, contained within the map.")
  (capture! [sink data] "Expects the finalized event data. Does
  whatever side effecty things it feels like. Should not be blocking."))

(extend-protocol EventSink
  clojure.lang.Atom
  (captures? [_ _] true)
  (capture! [this data]
    (swap! this conj data))

  clojure.lang.Fn
  (captures? [_ _] true)
  #_(captures? [this data] (this :captures? data))
  (capture! [this data] (this data))
  #_(capture! [this data] (this :capture! data)))

(defn assert-sink!
  [event-sink]
  (assert (satisfies? EventSink event-sink)))
