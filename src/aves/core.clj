(ns aves.core
  (:require [aves.event :as event]
            [aves.sink :as sink]
            [morphe.core :as morphe]))

(def ^:dynamic *default-data* {})

(defn set-default-data!
  [tags]
  (assert (map? tags) "Default event tags must be a map.")
  (alter-var-root #'*default-data* (constantly tags)))

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
    (event/merge-event-data! ~data)
    ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Morphe aspect utils ;;

(def ^:dynamic *in-event-aspect* false)

(defn assert-event-aspect!
  [name]
  (assert (true? *in-event-aspect*)
          (format "The %s aspect must be wrapped by the aves.core/event aspect."
                  name)))

(defn event-aspect
  "Apply this aspect to functions that are themselves aspects which
  should only be applied within the `event` aspect."
  [fn-def]
  (morphe/prefix-bodies fn-def
    `(assert-event-aspect! ~&name)))

(defn event
  "Tags a function as an event."
  [& event-mods]
  (fn [fn-def]
    (binding [*in-event-aspect* true]
      (with-event-emission ;; must be done last
        (reduce #(%2 %)
                fn-def
                event-mods)))))

(defn instrumented-with*
  [tag-map]
  (fn [fn-def]
    (assert-event-aspect! 'tagged-with)
    (morphe/prefix-bodies fn-def
      `(event/merge-event-data! ~tag-map))))

(defmacro instrumented
  [data]
  `(instrumented-with* '~data))

(defmacro instrumenting
  {:style/indent 1}
  [data & body]
  `(do
     (event/merge-event-data! ~data)
     ~@body))

(defn record!
  [data]
  (event/merge-event-data! data))

(defn record-with-merge!
  [f data]
  (event/merge-event-data-with! f data))

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
