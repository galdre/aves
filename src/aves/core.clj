(ns aves.core
  (:require [aves.event :as event]
            [aves.processor :as processor]))

(def ^:dynamic *default-data* {})

(defn set-default-data!
  [tags]
  (assert (map? tags) "Default event tags must be a map.")
  (alter-var-root #'*default-data* (constantly tags)))

(defmacro defprocessor
  [name-sym & {:keys [processes? process!]}]
  `(def ~name-sym
     (let [processes?# (or ~processes? (constantly true))
           process!# ~process!]
       (assert (some? process!#) ":process! key not optional in defprocessor")
       (reify processor/EventProcessor
         (~'processes? [this# data#]
           (processes?# data#))
         (~'process! [this# data#]
           (process!# data#))))))

(defn register-processor!
  [event-processor]
  (processor/assert-processor! event-processor)
  (alter-var-root #'processor/*event-processors* conj event-processor))

(defmacro with-processor
  [event-processor & body]
  `(let [processor# ~event-processor]
     (processor/assert-processor! processor#)
     (binding [processor/*event-processors* (conj processor/*event-processors* processor#)]
       ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Manipulation ;;

(defmacro with-event
  [& body]
  `(event/emitting-event ~@body))

(defmacro with-event-data
  {:style/indent 1}
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

(defmacro merging-data
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

;;;;;;;;;;;;;;;;;;;
;; UTILS & OTHER ;;

(defn event-id [event-map] (event/id-key event-map))
(defn parent-event-id [event-map] (event/parent-id-key event-map))
