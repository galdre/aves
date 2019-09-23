(ns aves.honeycomb.client
  (:require [aves.event :as aves-event]
            [aves.honeycomb.event :as hc-event])
  (:import [io.honeycomb.libhoney Event HoneyClient LibHoney]))

(extend-type HoneyClient
  aves-event/EventSink
  (send-data [honey-client data]
    (let [^Event event (hc-event/event honey-client data)]
      (.send event))))

(defn client
  [& args] ;; TODO
  (let [option-builder (LibHoney/options)]
    (-> option-builder
        ;; set API Host
        ;; set dataset
        ;; set write key
        ;; set global fields
        ;; set global dynamic fields
        ;; set sample rate
        ;; etc.
        (.build)
        (LibHoney/create))))
