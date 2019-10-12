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

(def ^:dynamic *in-event-aspect* false)

(defn assert-event-aspect!
  [name]
  (assert (true? *in-event-aspect*)
          (format "The %s aspect must be wrapped by the aves.core/event aspect."
                  name)))

(defn event-aspect
  [fn-def]
  (morphe/prefix-bodies fn-def
    `(assert-event-aspect! ~&name)))

(defn event
  [& event-mods]
  (fn [fn-def]
    (binding [*in-event-aspect* true]
      (with-event-emission ;; must be done last
        (reduce #(%2 %)
                fn-def
                event-mods)))))

(morphe/defn ^{::morphe/aspects [event-aspect]} timed
  [fn-def]
  (morphe/alter-bodies fn-def
    `(let [start# (. System (nanoTime))
           return# (do ~@&body)
           elapsed# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
       (event/merge-tags! {tag:metrics {tag:metrics-timer elapsed#}})
       return#)))

(morphe/defn ^{::morphe/aspects [event-aspect]} metered
  [fn-def]
  (let [meter-sym (gensym 'meter)]
    (-> fn-def
        (morphe/alter-form
            `(let [~meter-sym (atom 0)]
               ~&form))
        (morphe/alter-bodies
            `(let [metered-count# (swap! ~meter-sym inc)]
               (event/merge-tags! {tag:metrics {tag:metrics-meter metered-count#}})
               (try
                 ~@&body
                 (finally (swap! ~meter-sym dec))))))))

(defn tagged-with*
  [tag-map]
  (fn [fn-def]
    (assert-event-aspect! 'tagged-with)
    (morphe/prefix-bodies fn-def
      `(event/merge-tags! ~tag-map))))

(defmacro tagged-with
  [tag-map]
  `(tagged-with* '~tag-map))

(defmacro tagging-with
  {:style/indent 1}
  [tags & body]
  `(do
     (event/merge-tags! ~tags)
     ~@body))

(defn tag!
  [data]
  (event/merge-tags! data))
