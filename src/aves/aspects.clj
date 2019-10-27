(ns aves.aspects
  (:require [aves.core :as core]
            [morphe.core :as m]))

(def ^:dynamic *in-event-aspect* false)

(defn assert-event-aspect!
  [name]
  (assert (true? *in-event-aspect*)
          (format "The %s aspect must be wrapped by the aves.aspects/event aspect."
                  name)))

(defn event-aspect
  "Apply this aspect to functions that are themselves aspects which
  should only be applied within the `event` aspect."
  [fn-def]
  (m/prefix-bodies fn-def
    `(assert-event-aspect! ~&name)))

(defn- with-event-emission
  [fn-def]
  (m/alter-bodies fn-def
    `(core/with-event ~@&body)))

(defn event
  "Tags a function as an event."
  [& event-mods]
  (fn [fn-def]
    (binding [*in-event-aspect* true]
      (with-event-emission ;; must be done last
        (reduce #(%2 %)
                fn-def
                event-mods)))))

(defn tagged*
  [data]
  (fn [fn-def]
    (assert-event-aspect! 'tag)
    (m/prefix-bodies fn-def
      `(core/merge-data! ~data))))

(defmacro tagged
  [data]
  `(tagged* '~data))
