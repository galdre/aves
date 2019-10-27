(ns aves.core
  (:require [aves.event :as event]
            [aves.sink :as sink]
            [morphe.core :as morphe]))

(def ^:dynamic *default-data* {})

(defn set-default-data!
  [tags]
  (assert (map? tags) "Default event tags must be a map.")
  (alter-var-root #'*default-data* (constantly tags)))

(defmacro defsink
  [name-sym & {:keys [captures? capture!]}]
  `(def ~name-sym
     (let [captures-pred# (or ~captures?
                              (constantly true))
           capture-impl# ~capture!]
       (assert (some? capture-impl#) ":capture! key not optional in defsink")
       (reify sink/EventSink
         (~'captures? [this# data#]
           (captures-pred# data#))
         (~'capture! [this# data#]
           (capture-impl# data#))))))

(defn register-sink!
  [event-sink]
  (sink/assert-sink! event-sink)
  (alter-var-root #'sink/*event-sinks* conj event-sink))

(defmacro with-sink
  [event-sink & body]
  `(let [sink# ~event-sink]
     (sink/assert-sink! sink#)
     (binding [sink/*event-sinks* (conj sink/*event-sinks* sink#)]
       ~@body)))

;;;;;;;;;;;
;; UTILS ;;

(defmacro with-event
  [& body]
  `(event/emitting-event ~@body))

(defmacro with-event-data
  [data & body]
  `(event/emitting-event
    (merge-data! ~data)
    ~@body))

(defn- rec-merge
  [m1 m2]
  (if-not (and (map? m1) (map? m2))
    m2
    (merge-with rec-merge m1 m2)))

(defn merge-data!
  "Expects a map. When in event, recursively merges into existing event
  data, overwriting."
  [data]
  (when event/*current-event*
    (swap! event/*current-event* rec-merge data)))

(defmacro merging-data ;; with-data
  {:style/indent 1}
  [data & body]
  `(do
     (merge-data! ~data)
     ~@body))

(defn- rec-merge-with
  [f m1 m2]
  (if-not (and (map? m1) (map? m2))
    (f m1 m2)
    (merge-with (partial rec-merge-with f) m1 m2)))

(defn merge-data-with!
  "Expects a map. Recursively merges into existing event data. On
  conflict, uses (f prev-data new-data) to determine new value."
  [f data]
  (when event/*current-event*
    (swap! event/*current-event* (partial rec-merge-with f) data)))

(defmacro merging-data-with
  {:style/indent 2}
  [f data & body]
  `(do
     (merge-data-with! ~f ~data)
     ~@body))

(defn event-id [event-map] (event/id-key event-map))
(defn parent-event-id [event-map] (event/parent-id-key event-map))
