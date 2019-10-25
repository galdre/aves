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

(defn new-event
  "Returns an atom containing a map of data."
  ([] (new-event nil))
  ([data]
   (let [parent-id (when *current-event*
                     (::id @*current-event*))]
     (-> {::id (promised-uuid)}
         (cond-> parent-id (assoc ::parent-id parent-id)
                 data (merge data))
         (atom)))))

(defmacro emitting-event
  [& body]
  `(binding [*current-event* (new-event)]
     (try
       ~@body
       (finally
         (emit! @*current-event*)))))

(defn- rec-merge
  [m1 m2]
  (if-not (and (map? m1) (map? m2))
    m2
    (merge-with rec-merge m1 m2)))

(defn- rec-merge-with
  [f m1 m2]
  (if-not (and (map? m1) (map? m2))
    (f m1 m2)
    (merge-with (partial rec-merge-with f) m1 m2)))

(defn merge-event-data!
  "Expects a map. Recursively merges into existing event data, overwriting."
  [data]
  (when *current-event*
    (swap! *current-event* rec-merge data)))

(defn merge-event-data-with!
  [f data]
  "Expects a map. Recursively merges into existing event data. On
  conflict, uses (f prev-data new-data) to determine new value."
  (when *current-event*
    (swap! *current-event* (partial rec-merge-with f) data)))
