(ns aves.event
  (:require [humilia.core :as hum]))

;; An event is just a map of data. You can send it to a sink.

(def ^:dynamic *event*)

(defprotocol EventSink
  (send-data [sink data]))

(extend-protocol EventSink
  clojure.lang.Atom
  (send-data [this data]
    (swap! this conj data))

  clojure.lang.Fn
  (send-data [this data]
    (this data)))

;; The vals of event data may be functions or promises
;; intended to be called/dereferenced at time of submission.
(defn finalize-data
  [data]
  (cond (map? data) (into {} (hum/map-vals finalize-data) data)
        (coll? data) (into (empty data) (map finalize-data) data)
        (fn? data) (data)
        (instance? clojure.lang.IDeref data) (deref data)
        :else data))

(defn assert-sink!
  [event-sink]
  (assert (satisfies? EventSink event-sink)
           (format "Cannot send to invalid EventSink: %s" event-sink)
           #_{:event-sink event-sink})) ;; missing `insist`

(def ^:dynamic *event-sink*)

(defn set-default-sink!
  [event-sink]
  (assert-sink! event-sink)
  (alter-var-root #'*event-sink* (constantly event-sink)))

;;;;;;;;;;;;;;;;;
;; SENDING DATA

(defn send!
  ([event-data] (send! *event-sink* event-data))
  ([event-sink event-data]
   (when event-sink ;; TODO: optional warnings here
     (->> event-data
          finalize-data
          (send-data event-sink)))))

(def ^:dynamic *current-event*)

;; Only generate event ids when needed by event finalization:
(defn promised-uuid [] (delay (str (UUID/randomUUID))))

(defn new-event
  "Returns an atom containing a map of data."
  ([data]
   (let [parent-id (when *current-event*
                     (::id @*current-event*))]
     (-> {::id (promised-uuid)}
         (cond-> parent-id (assoc ::parent-id parent-id)
                 data (merge data))
         (atom)))))

(defmacro emitting-event
  [& body]
  `(binding [*current-event* (atom (new-event))]
     (try
       ~@body
       (finally
         (send! @*current-event*)))))

(defn- rec-merge
  [m1 m2]
  (if-not (and (map? m1) (map? m2))
    m2
    (merge-with rec-merge m1 m2)))

(defn merge-tags!
  [tags]
  (when *current-event*
    (swap! *current-event* rec-merge tags)))
