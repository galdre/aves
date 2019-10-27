(ns aves.core-test
  (:require [aves.core :as core]
            [aves.event :as event]
            [clojure.test :as t]
            [morphe.core :as m]))

(t/deftest unit:with-event
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 3
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (+ 1 2)))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id} (set (keys (first @event-log)))))))
  (t/testing "A single event with function sink"
    (let [atom-log (atom [])
          event-log (fn [data] (swap! atom-log conj data))]
      (t/is
       (= 3
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @atom-log)))
              (+ 1 2)))))
      (t/is (= 1 (count @atom-log)))
      (t/is (= #{::event/id} (set (keys (first @atom-log)))))))
  (t/testing "A nested event"
    (let [event-log (atom [])]
      (t/is
       (= :fifties
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (+ 1 2)
              (core/with-event
                (t/is (zero? (count @event-log)))
                (+ 2 4))
              (t/is (= 1 (count @event-log))))
            (t/is (= 2 (count @event-log)))
            :fifties)))
      (let [[child-event parent-event] @event-log]
        (t/is (= (::event/parent-id child-event)
                 (::event/id parent-event)))))))

(t/deftest unit:with-sink
  (t/testing "Single event, two sinks"
    (let [event-log-1 (atom [])
          event-log-2 (atom [])]
      (t/is
       (= 3
          (core/with-sink event-log-1
            (core/with-sink event-log-2
              (core/with-event
                (t/is (zero? (count @event-log-1)))
                (t/is (zero? (count @event-log-2)))
                (+ 1 2))))))
      (t/is (= 1 (count @event-log-1)))
      (t/is (= 1 (count @event-log-2)))
      (t/is (= #{::event/id}
               (set (keys (first @event-log-1)))
               (set (keys (first @event-log-2)))))))
  (t/testing "Multiple events, multiple sinks"
    (let [event-log-1 (atom [])
          event-log-2 (atom [])]
      (t/is
       (= 3
          (core/with-sink event-log-1
            (core/with-event
              (t/is (zero? (count @event-log-1))))
            (core/with-sink event-log-2
              (core/with-event
                (t/is (= 1 (count @event-log-1)))
                (t/is (zero? (count @event-log-2)))
                (+ 1 2))))))
      (t/is (= 2 (count @event-log-1)))
      (t/is (= 1 (count @event-log-2)))
      (t/is (= #{::event/id} (set (keys (first @event-log-1)))))))
  (t/testing "Nested events with multiple sinks"
    (let [outer-event-log (atom [])
          inner-event-log (atom [])]
      (t/is
       (= :fifties ;; last line of with-sink
          (core/with-sink outer-event-log
            (core/with-event
              (t/is (zero? (count @outer-event-log)))
              (t/is (zero? (count @inner-event-log)))
              (+ 1 2)
              (core/with-sink inner-event-log
                (core/with-event
                  (t/is (zero? (count @outer-event-log)))
                  (t/is (zero? (count @inner-event-log)))
                  (+ 2 4)))
              (t/is (= 1 (count @outer-event-log)))
              (t/is (= 1 (count @inner-event-log))))
            (t/is (= 2 (count @outer-event-log)))
            (t/is (= 1 (count @inner-event-log)))
            :fifties)))
      (let [[child-event parent-event] @outer-event-log]
        (t/is (= (::event/parent-id child-event)
                 (::event/id parent-event)))))))

(t/deftest unit:with-event-data
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 3
          (core/with-sink event-log
            (core/with-event-data
              {:hi :there}
              (t/is (zero? (count @event-log)))
              (+ 1 2)))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :hi} (set (keys (first @event-log)))))
      (t/is (= :there (:hi (first @event-log))))))
  (t/testing "A nested event"
    (let [event-log (atom [])
          alternate-log (fn [data] (swap! event-log conj data))]
      (t/is
       (= :darth-vader
          (core/with-sink alternate-log
            (core/with-event-data
              {:excellent :wine}
              (t/is (zero? (count @event-log)))
              (+ 1 2)
              (core/with-event-data
                {:aged :rum}
                (t/is (zero? (count @event-log)))
                (+ 2 1))
              (t/is (= 1 (count @event-log))))
            (t/is (= 2 (count @event-log)))
            :darth-vader)))
      (let [[child-event parent-event] @event-log]
        (t/is (= (::event/parent-id child-event)
                 (::event/id parent-event)))
        (t/is (= :wine (:excellent parent-event)))
        (t/is (= :rum (:aged child-event)))))))

(t/deftest unit:with-data
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 7
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/with-data {:golgotha :calvary}
                (+ 1 2))
              (core/with-data {:dungeons :dragons}
                (- 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :dungeons :golgotha} (set (keys (first @event-log)))))
      (t/is (= :dragons (:dungeons (first @event-log))))
      (t/is (= :calvary (:golgotha (first @event-log)))))))

(t/deftest unit:with-data-with-derefables
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 7
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/with-data {:golgotha (delay :calvary)}
                (+ 1 2))
              (core/with-data {:dungeons (future :dragons)}
                (- 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :dungeons :golgotha} (set (keys (first @event-log)))))
      (t/is (= :dragons (:dungeons (first @event-log))))
      (t/is (= :calvary (:golgotha (first @event-log)))))))

(t/deftest unit:with-data-with-fns
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 30
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/with-data {:clojure (constantly :awesome)}
                (/ 1 2))
              (core/with-data {:java (fn [] "tewonmasoe")}
                (* 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :clojure :java} (set (keys (first @event-log)))))
      (t/is (= :awesome (:clojure (first @event-log))))
      (t/is (= "tewonmasoe" (:java (first @event-log)))))))

(t/deftest unit:merge-data!
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 18
          (core/with-sink event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/merge-data! {:honey :pot})
              (+ 1 2)
              (core/merge-data! {:cat :hat})
              (+ 10 3 5)))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :honey :cat} (set (keys (first @event-log)))))
      (t/is (= :pot (:honey (first @event-log))))
      (t/is (= :hat (:cat (first @event-log)))))))

(t/deftest unit:merge-data-with!
  (t/testing "A single event"
    (let [event-log (atom [])]
      (core/with-sink event-log
        (core/with-event
          (t/is (zero? (count @event-log)))
          (core/merge-data! {:integers []})
          (core/merge-data-with! conj {:integers 10})
          (core/merge-data-with! conj {:integers 159233})))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :integers} (set (keys (first @event-log)))))
      (t/is (= [10 159233] (:integers (first @event-log)))))))

(m/defn ^{::m/aspects [(core/event)]} event-fn
  [x]
  (inc x))

(t/deftest unit:event-aspect
  (let [event-log (atom [])]
    (core/with-sink event-log
      (t/is (= 5 (event-fn 4))))
    (t/is (= 1 (count @event-log)))
    (t/is (= #{::event/id} (set (keys (first @event-log)))))))

(m/defn ^{::m/aspects [(core/event (core/instrumented {:arg arg}))]}
  instrumented-fn
  ([arg]
   (inc arg))
  ([buzzwords arg]
   (+ buzzwords arg)))

(t/deftest unit:instrumented
  (let [event-log (atom [])]
    (core/with-sink event-log
      (t/is (= 11 (instrumented-fn 10)))
      (t/is (= 12 (instrumented-fn 7 5))))
    (let [[event-1 event-2] @event-log]
      (t/is (= 10 (:arg event-1)))
      (t/is (= 5 (:arg event-2))))))

(m/defn ^{::m/aspects [(core/event)]}
  event-fn-with-instrumentation
  [x]
  (if (even? x)
    (core/with-data {:even true}
      (inc x))
    (core/with-data {:odd :ball}
      (dec x))))

(t/deftest unit:with-data-within-event-fn
  (let [event-log (atom [])]
    (core/with-sink event-log
      (t/is (= 13 (event-fn-with-instrumentation 12)))
      (t/is (= 10 (event-fn-with-instrumentation 11))))
    (let [[event-1 event-2] @event-log]
      (t/is (= #{::event/id :even} (set (keys event-1))))
      (t/is (= #{::event/id :odd} (set (keys event-2)))))))

(def agent-kitty-sink (agent []))
(def ref-puppy-sink (ref []))

(core/defsink kitty-sink
  :captures?
  (fn [data] (contains? data :tiger))
  :capture!
  (fn [data]
    (send agent-kitty-sink conj data)
    (await agent-kitty-sink)))

(core/defsink puppy-sink
  :captures?
  (fn [data] (contains? data :mastiff))
  :capture!
  (fn [data]
    (dosync (alter ref-puppy-sink conj data))))

(t/deftest contract:conditional-capture
  (let [omni-log (atom [])]
    (send agent-kitty-sink empty)
    (dosync (alter ref-puppy-sink empty))
    (core/with-sink omni-log
      (t/is
       (core/with-sink puppy-sink
         (= 18
            (core/with-sink kitty-sink
              (core/with-event
                (await agent-kitty-sink)
                (t/is (zero? (count @agent-kitty-sink)))
                (core/merge-data! {:mastiff :chihuahua})
                (* 10 10))
              (core/with-event
                (await agent-kitty-sink)
                (t/is (zero? (count @agent-kitty-sink)))
                (core/merge-data! {:tiger :singapura})
                (- 24 6))))))
      (t/testing "Only the omni-log captured both events"
        (t/is (= 2 (count @omni-log)))
        (t/is (= 1 (count @agent-kitty-sink) (count @ref-puppy-sink))))
      (t/testing "The other sinks captured different events"
        (t/is (= #{::event/id :tiger} (set (keys (first @agent-kitty-sink)))))
        (t/is (= #{::event/id :mastiff} (set (keys (first @ref-puppy-sink))))))
      (t/testing "Data integrity compmlete."
        (t/is (= :singapura (:tiger (first @agent-kitty-sink))))
        (t/is (= :chihuahua (:mastiff (first @ref-puppy-sink))))))))
