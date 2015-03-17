(ns everything-will-flow.core
  (:refer-clojure
    :exclude [take])
  (:require
    [incanter.core :refer [view]]
    [incanter.charts :as charts]))

;;;

(defn normal-rand [mean sigma]
  (let [u1 (rand)
        u2 (rand)
        theta (* 2 Math/PI u2)
        r (Math/sqrt (* 2 -1 (Math/log u1)))]
    (+ mean (* sigma r (Math/sin theta)))))

(defn lerp [a b t]
  (+ a (* (- b a) t)))

;;;

(def ^:dynamic *event-accumulator* (atom []))

(defmacro with-accumulator [acc & body]
  `(binding [*event-accumulator* ~acc]
     ~@body))

(defn execute-at! [timestamp event]
  (swap! *event-accumulator* conj [timestamp event]))

;;;

(defn event-heap []
  (sorted-map))

(defn push-event [heap timestamp event]
  (update heap timestamp #(if % (conj % event) [event])))

(defn first-event [heap]
  (let [[timestamp events] (first heap)]
    (if (empty? events)
      (recur (dissoc heap timestamp))
      [timestamp (last events)])))

(defn pop-event [heap]
  (let [[timestamp events] (first heap)]
    (if (empty? events)
      (recur (dissoc heap timestamp))
      (update heap timestamp pop))))

;;;

(defn empty-queue []
  clojure.lang.PersistentQueue/EMPTY)

(defprotocol IMessageQueue
  (put-msg [_ timestamp msg])
  (take-msg [_ timestamp f]))

(defrecord MessageQueue
  [max-queue-length
   takes
   messages
   arrivals
   rejections
   queue-latencies]
  IMessageQueue
  (put-msg [this timestamp msg]
    (let [this (update this :arrivals conj timestamp)]
      (cond
        (peek takes)
        (let [[t f] (peek takes)]
          (execute-at! timestamp (partial f msg))
          (-> this
            (update :takes pop)
            (update :queue-latencies conj [timestamp 0])))

        (<= max-queue-length (count messages))
        (-> this
          (update :rejections conj timestamp))

        :else
        (-> this
          (update :messages conj [timestamp msg])))))

  (take-msg [this timestamp f]
    (if-let [[t msg] (peek messages)]
      (do
        (execute-at! timestamp (partial f msg))
        (-> this
          (update :messages pop)
          (update :queue-latencies conj [timestamp (- timestamp t)])))
      (-> this
        (update :takes conj [timestamp f])))))

(defn queue [max-queue-length]
  (MessageQueue. max-queue-length (empty-queue) (empty-queue) [] [] []))

;;;

(defrecord Simulation
  [events
   state])

(defn advance [^Simulation s]
  (if-let [[t f] (first-event (.events s))]
    (let [acc (atom [])]
      (with-accumulator acc
        (let [state' (f t (.state s))]
          (Simulation.
            (reduce
              (fn [h [t e]] (push-event h t e))
              (pop-event (.events s))
              @acc)
            state'))))
    s))

;;;

(defn producer [queue [mean sigma] msg-generator]
  (let [period-mean (/ 1 mean)
        period-sigma (if (zero? sigma) 0 (/ 1 sigma))]
    (fn this [timestamp state]
      (execute-at!
        (+ timestamp (max 0 (normal-rand period-mean period-sigma)))
        this)
      (update state queue put-msg timestamp (msg-generator timestamp)))))

(defn consumer [queue latencies]
  (fn this [timestamp state]
    (update state queue take-msg timestamp
      (fn [task-length timestamp' state]
        (let [t' (+ timestamp' task-length)]
          (execute-at! t' this)
          (update state latencies conj [t' (- t' timestamp)]))))))

(defn simulation [state fs]
  (Simulation.
    (reduce
      #(push-event %1 0 %2)
      (event-heap)
      fs)
    state))
