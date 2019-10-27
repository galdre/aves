(ns aves.event
  (:require [aves.processor :as processor]
            [humilia.core :as hum])
  (:import [java.util UUID]))

;; An event is a plain map of data, tracked in an atom in a dynamic var.
;; You can emit the event to a processor.
(def ^:dynamic *current-event* nil)

;; It may be useful to register certain data that all new events will contain.
(def ^:dynamic *default-data* {})

(defn finalize-data
  "finalize-data prepares an event for final emission. Values that are
  derefable resolve to the dereference. All other data remains the same."
  [data]
  (cond (map? data) (into {} (hum/map-vals finalize-data) data)
        (coll? data) (into (empty data) (map finalize-data) data)
        (instance? clojure.lang.IDeref data) (deref data)
        :else data))

(defn emit!
  ([event-data] (emit! event-data processor/*event-processors*))
  ([event-data processors]
   (let [finalized-data (delay (finalize-data event-data))]
     (doseq [processor processors :when (processor/processes? processor event-data)]
       (processor/process! processor @finalized-data)))))

;; Only generate event ids when needed by event finalization:
(defn promised-uuid [] (delay (str (UUID/randomUUID))))

(def id-key ::id)
(def parent-id-key ::parent-id)

(defn new-event
  "Returns an atom containing a map of data."
  ([] (new-event nil))
  ([data]
   (let [parent-id (when *current-event*
                     (id-key @*current-event*))]
     (-> {id-key (promised-uuid)}
         (cond-> parent-id (assoc parent-id-key parent-id)
                 data (merge data))
         (merge *default-data*)
         (atom)))))

(defmacro emitting-event
  [& body]
  `(binding [*current-event* (new-event)]
     (try
       ~@body
       (finally
         (emit! @*current-event*)))))
