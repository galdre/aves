(ns aves.core
  (:require [aves.event :as event]
            [morphe.core :as morphe]))

(def ^:dynamic *default-tags* {})

(defn set-default-tags!
  [tags]
  (assert (map? tags) "Default event tags must be a map.")
  (alter-var-root #'*default-tags* (constantly tags)))

(defn register-sink!
  [event-sink]
  (event/assert-sink! event-sink)
  (alter-var-root #'event/*event-sinks* conj event-sink))

(defmacro with-sink
  [event-sink & body]
  `(let [sink# ~event-sink]
     (event/assert-sink! sink#)
     (binding [event/*event-sinks* (conj event/*event-sinks* sink#)]
       ~@body)))

(def tag:metrics "keys to a map of metrics" ::metrics)
(def tag:metrics-timer "keys to running time in ms" ::timer)
(def tag:metrics-meter "keys to meter value at start of event" ::meter)

(defn- with-event-emission
  [fn-def]
  (morphe/alter-bodies fn-def
    `(event/emitting-event ~@&body)))

(defmacro with-event
  [& body]
  `(event/emitting-event ~@body))

(defmacro with-event-data
  [data & body]
  `(event/emitting-event
    (event/merge-tags! ~data)
    ~@body))

(defn event
  [& event-mods]
  (fn [fn-def]
    (with-event-emission ;; must be done last
      (reduce #(%2 %)
              fn-def
              event-mods))))

(defn timed
  [fn-def]
  (morphe/alter-bodies fn-def
    `(let [start# (. System (nanoTime))
           return# (do ~@&body)
           elapsed# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
       (event/merge-tags! {tag:metrics {tag:metrics-timer elapsed#}})
       return#)))

(defn metered
  [fn-def]
  (-> fn-def
      (morphe/alter-form
          `(let [meter# (atom 0)]
             ~&form))
      (morphe/alter-bodies
          `(let [metered-count# (swap! meter# inc)]
             (event/merge-tags! {tag:metrics {tag:metrics-meter metered-count#}})
             (try
               ~@&body
               (finally (swap! meter# dec)))))))

(defn tagged-with
  [tag-map]
  (fn [fn-def]
    (morphe/prefix-bodies fn-def
      `(event/merge-tags! ~tag-map))))

(defmacro tagging-with
  [tags & body]
  `(do
     (event/merge-tags! ~tags)
     ~@body))

(defn tag!
  [data]
  (event/merge-tags! data))
