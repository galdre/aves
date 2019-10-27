(ns aves.core-test
  (:require [aves.aspects :as asp]
            [aves.core :as core]
            [aves.event :as event]
            [clojure.test :as t]
            [morphe.core :as m]))

(t/deftest unit:with-event
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 3
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (+ 1 2)))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id} (set (keys (first @event-log)))))))
  (t/testing "A single event with function processor"
    (let [atom-log (atom [])
          event-log (fn [data] (swap! atom-log conj data))]
      (t/is
       (= 3
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @atom-log)))
              (+ 1 2)))))
      (t/is (= 1 (count @atom-log)))
      (t/is (= #{::event/id} (set (keys (first @atom-log)))))))
  (t/testing "A nested event"
    (let [event-log (atom [])]
      (t/is
       (= :fifties
          (core/with-processor event-log
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

(t/deftest unit:with-processor
  (t/testing "Single event, two processors"
    (let [event-log-1 (atom [])
          event-log-2 (atom [])]
      (t/is
       (= 3
          (core/with-processor event-log-1
            (core/with-processor event-log-2
              (core/with-event
                (t/is (zero? (count @event-log-1)))
                (t/is (zero? (count @event-log-2)))
                (+ 1 2))))))
      (t/is (= 1 (count @event-log-1)))
      (t/is (= 1 (count @event-log-2)))
      (t/is (= #{::event/id}
               (set (keys (first @event-log-1)))
               (set (keys (first @event-log-2)))))))
  (t/testing "Multiple events, multiple processors"
    (let [event-log-1 (atom [])
          event-log-2 (atom [])]
      (t/is
       (= 3
          (core/with-processor event-log-1
            (core/with-event
              (t/is (zero? (count @event-log-1))))
            (core/with-processor event-log-2
              (core/with-event
                (t/is (= 1 (count @event-log-1)))
                (t/is (zero? (count @event-log-2)))
                (+ 1 2))))))
      (t/is (= 2 (count @event-log-1)))
      (t/is (= 1 (count @event-log-2)))
      (t/is (= #{::event/id} (set (keys (first @event-log-1)))))))
  (t/testing "Nested events with multiple processors"
    (let [outer-event-log (atom [])
          inner-event-log (atom [])]
      (t/is
       (= :fifties ;; last line of with-processor
          (core/with-processor outer-event-log
            (core/with-event
              (t/is (zero? (count @outer-event-log)))
              (t/is (zero? (count @inner-event-log)))
              (+ 1 2)
              (core/with-processor inner-event-log
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
          (core/with-processor event-log
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
          (core/with-processor alternate-log
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

(t/deftest unit:merging-data
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 7
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/merging-data {:golgotha :calvary}
                (+ 1 2))
              (core/merging-data {:dungeons :dragons}
                (- 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :dungeons :golgotha} (set (keys (first @event-log)))))
      (t/is (= :dragons (:dungeons (first @event-log))))
      (t/is (= :calvary (:golgotha (first @event-log)))))))

(t/deftest unit:with-data-using-repeaters
  (t/testing "Simple test"
    (let [event-log (atom [])]
      (t/is
       (= 30
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/merging-data {:clojure (core/repeater* (constantly :awesome))
                                  :fruit (constantly :banana)}
                (/ 1 2))
              (core/merging-data {:java (core/repeater "tewonmasoe")
                                  :meat (fn [] "antelope")}
                (* 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :clojure :java :fruit :meat} (set (keys (first @event-log)))))
      (t/is (= :awesome (:clojure (first @event-log))))
      (t/is (= "tewonmasoe" (:java (first @event-log))))
      (t/is (= :banana ((:fruit (first @event-log)))))
      (t/is (= "antelope" ((:meat (first @event-log))))))))

(t/deftest unit:repeater+set-default-data!
  (try
    (let [counter (atom 0)
          repeater (core/repeater (swap! counter inc) :some-value)
          event-log (atom [])]
      (core/set-default-data! {:a-universal-key repeater})
      (t/is
       (= 1
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              -1
              (core/with-event
                (t/is (zero? (count @event-log)))
                1)))))
      (t/is (= 2 (count @event-log)))
      (t/is (= 2 @counter))
      (t/is (every? (comp #{:some-value} :a-universal-key) @event-log)))
    (finally
      (core/set-default-data! {}))))

(t/deftest unit:with-data-with-derefables
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 7
          (core/with-processor event-log
            (core/with-event
              (t/is (zero? (count @event-log)))
              (core/merging-data {:golgotha (delay :calvary)}
                (+ 1 2))
              (core/merging-data {:dungeons (future :dragons)}
                (- 10 3))))))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :dungeons :golgotha} (set (keys (first @event-log)))))
      (t/is (= :dragons (:dungeons (first @event-log))))
      (t/is (= :calvary (:golgotha (first @event-log)))))))

(t/deftest unit:merge-data!
  (t/testing "A single event"
    (let [event-log (atom [])]
      (t/is
       (= 18
          (core/with-processor event-log
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
      (core/with-processor event-log
        (core/with-event
          (t/is (zero? (count @event-log)))
          (core/merge-data! {:integers []})
          (core/merge-data-with! conj {:integers 10})
          (core/merge-data-with! conj {:integers 159233})))
      (t/is (= 1 (count @event-log)))
      (t/is (= #{::event/id :integers} (set (keys (first @event-log)))))
      (t/is (= [10 159233] (:integers (first @event-log)))))))

(m/defn ^{::m/aspects [(asp/event)]} event-fn
  [x]
  (inc x))

(t/deftest unit:event-aspect
  (let [event-log (atom [])]
    (core/with-processor event-log
      (t/is (= 5 (event-fn 4))))
    (t/is (= 1 (count @event-log)))
    (t/is (= #{::event/id} (set (keys (first @event-log)))))))

(m/defn ^{::m/aspects [(asp/event (asp/tagged {:arg arg}))]}
  instrumented-fn
  ([arg] (inc arg))
  ([buzzwords arg] (+ buzzwords arg)))

(t/deftest unit:tagged
  (let [event-log (atom [])]
    (core/with-processor event-log
      (t/is (= 11 (instrumented-fn 10)))
      (t/is (= 12 (instrumented-fn 7 5))))
    (let [[event-1 event-2] @event-log]
      (t/is (= 10 (:arg event-1)))
      (t/is (= 5 (:arg event-2))))))

(m/defn ^{::m/aspects [(asp/event)]}
  event-fn-with-instrumentation
  [x]
  (if (even? x)
    (core/merging-data {:even true}
      (inc x))
    (core/merging-data {:odd :ball}
      (dec x))))

(t/deftest unit:with-data-within-event-fn
  (let [event-log (atom [])]
    (core/with-processor event-log
      (t/is (= 13 (event-fn-with-instrumentation 12)))
      (t/is (= 10 (event-fn-with-instrumentation 11))))
    (let [[event-1 event-2] @event-log]
      (t/is (= #{::event/id :even} (set (keys event-1))))
      (t/is (= #{::event/id :odd} (set (keys event-2)))))))

(def agent-kitty-processor (agent []))
(def ref-puppy-processor (ref []))

(core/defprocessor kitty-processor
  :processes?
  (fn [data] (contains? data :tiger))
  :process!
  (fn [data]
    (send agent-kitty-processor conj data)
    (await agent-kitty-processor)))

(core/defprocessor puppy-processor
  :processes?
  (fn [data] (contains? data :mastiff))
  :process!
  (fn [data]
    (dosync (alter ref-puppy-processor conj data))))

(t/deftest contract:conditional-capture
  (let [omni-log (atom [])]
    (send agent-kitty-processor empty)
    (dosync (alter ref-puppy-processor empty))
    (core/with-processor omni-log
      (t/is
       (core/with-processor puppy-processor
         (= 18
            (core/with-processor kitty-processor
              (core/with-event
                (await agent-kitty-processor)
                (t/is (zero? (count @agent-kitty-processor)))
                (core/merge-data! {:mastiff :chihuahua})
                (* 10 10))
              (core/with-event
                (await agent-kitty-processor)
                (t/is (zero? (count @agent-kitty-processor)))
                (core/merge-data! {:tiger :singapura})
                (- 24 6))))))
      (t/testing "Only the omni-log captured both events"
        (t/is (= 2 (count @omni-log)))
        (t/is (= 1 (count @agent-kitty-processor) (count @ref-puppy-processor))))
      (t/testing "The other processors captured different events"
        (t/is (= #{::event/id :tiger} (set (keys (first @agent-kitty-processor)))))
        (t/is (= #{::event/id :mastiff} (set (keys (first @ref-puppy-processor))))))
      (t/testing "Data integrity compmlete."
        (t/is (= :singapura (:tiger (first @agent-kitty-processor))))
        (t/is (= :chihuahua (:mastiff (first @ref-puppy-processor))))))))
