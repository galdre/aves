(ns aves.event
  (:require [aves.sink :as sink]
            [humilia.core :as hum])
  (:import [java.util UUID]))

;; An event is a plain map of data, tracked in an atom in a dynamic var.
;; You can emit the event to a sink.
(def ^:dynamic *current-event* nil)

(defn finalize-data
  "finalize-data prepares an event for final emission. Values that are
  functions are resolved to the output of the function; those that are
  derefable resolve to the dereference. All other data remains the same."
  [data]
  (cond (map? data) (into {} (hum/map-vals finalize-data) data)
        (coll? data) (into (empty data) (map finalize-data) data)
        (fn? data) (data)
        (instance? clojure.lang.IDeref data) (deref data)
        :else data))

(defn emit!
  ([event-data] (emit! event-data sink/*event-sinks*))
  ([event-data sinks]
   (let [finalized-data (delay (finalize-data event-data))]
     (doseq [sink sinks :when (sink/captures? sink event-data)]
       (sink/capture! sink @finalized-data)))))

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
         (atom)))))

(defmacro emitting-event
  [& body]
  `(binding [*current-event* (new-event)]
     (try
       ~@body
       (finally
         (emit! @*current-event*)))))
