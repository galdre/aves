(ns aves.sink)

(def ^:dynamic *event-sinks* #{})

(defprotocol EventSink
  (captures? [sink data-keys] "Takes the non-finalized event map;
  returns true/false. Should not evaluate any functions, or
  dereference any values, contained within the map.")
  (capture! [sink data-delay] "Expects the finalized event data. Does
  whatever side effecty things it feels like."))

(extend-protocol EventSink
  clojure.lang.Atom
  (captures? [_ _] true)
  (capture! [this data]
    (swap! this conj data))

  clojure.lang.Fn
  (captures? [_ _] true)
  (capture! [this data]
    (this data)))

(defn assert-sink!
  [event-sink]
  (assert (satisfies? EventSink event-sink)))
