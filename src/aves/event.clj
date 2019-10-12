(ns aves.event
  (:require [humilia.core :as hum])
  (:import [java.util UUID]))

;; An event is just a map of data. You can send it to a sink.

(def ^:dynamic *event*)

(defprotocol EventSink
  (captures? [sink data])
  (capture! [sink data]))

(extend-protocol EventSink
  clojure.lang.Atom
  (captures? [_ _] true)
  (capture! [this data]
    (swap! this conj data))

  clojure.lang.Fn
  (captures? [_ _] true)
  (capture! [this data]
    (this data)))

(defmacro defsink
  [name-sym & {:keys [captures? capture!]}]
  `(def ~name-sym
     (let [captures-pred# (or ~captures?
                              (constantly true))
           capture-impl# ~capture!]
       (assert (some? capture-impl#) ":capture! key not optional in defsink")
       (reify EventSink
         (~'captures? [this# data#]
           (captures-pred# data#))
         (~'capture! [this# data#]
           (capture-impl# data#))))))

;; The vals of event data may be functions or promises
;; intended to be called/dereferenced at time of submission.
(defn finalize-data
  [data]
  (cond (map? data) (into {} (hum/map-vals finalize-data) data)
        (coll? data) (into (empty data) (map finalize-data) data)
        (fn? data) (data)
        (instance? clojure.lang.IDeref data) (deref data)
        :else data))

 ;; missing `insist`

(def ^:dynamic *event-sinks* #{})

;;;;;;;;;;;;;;;;;
;; SENDING DATA

(defn emit!
  ([event-data]
   (doseq [sink *event-sinks*]
     (emit! sink event-data)))
  ([event-sink event-data]
   (when (and (satisfies? EventSink event-sink) ;; TODO: optional warnings here
              (captures? event-sink event-data))
     (->> event-data finalize-data (capture! event-sink)))))

(def ^:dynamic *current-event* nil)

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

(defn merge-tags!
  [tags]
  (when *current-event*
    (swap! *current-event* rec-merge tags)))

(defn assert-sink!
  [event-sink]
  (assert (satisfies? EventSink event-sink)
          (format "Cannot send to invalid EventSink: %s" event-sink)
          #_{:event-sink event-sink}))
