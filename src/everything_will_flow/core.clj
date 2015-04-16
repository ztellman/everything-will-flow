(ns everything-will-flow.core
  (:refer-clojure
    :exclude [take])
  (:require
    [everything-will-flow.viz :as viz]
    [incanter.core :refer [view]]
    [incanter.charts :as charts]))

;;;

(defn pareto
  "Returns a Pareto-distributed value in [min,max], with the tail size specified by `alpha`."
  [alpha min max]
  (let [r (rand)]
    (if (or (zero? r) (== 1 r))
      (recur alpha min max)
      (let [ha (Math/pow max alpha)
            la (Math/pow min alpha)]
        (Math/pow
          (-
            (/
              (- (* r ha) (* r la) ha)
              (* ha la)))
          (- (/ 1 alpha)))))))

(defn next-arrival
  "An exponentially distributed arrival time"
  [rate]
  (double (/ (- (Math/log (rand))) rate)))

(defn lerp [a b t]
  (+ a (* (- b a) (max 0 (min 1 t)))))

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
   queue-lengths
   queue-latencies]
  IMessageQueue
  (put-msg [this timestamp msg]
    (let [this (update this :arrivals conj timestamp)]
      (cond
        (peek takes)
        (let [[t f] (peek takes)]
          (execute-at! timestamp (partial f msg timestamp))
          (-> this
            (update :takes pop)
            (update :queue-latencies conj [timestamp 0])))

        (<= max-queue-length (count messages))
        (-> this
          (update :rejections conj timestamp))

        :else
        (-> this
          (update :queue-lengths conj [timestamp (inc (count messages))])
          (update :messages conj [timestamp msg])))))

  (take-msg [this timestamp f]
    (if-let [[t msg] (peek messages)]
      (do
        (execute-at! timestamp (partial f msg t))
        (-> this
          (update :messages pop)
          (update :queue-lengths conj [timestamp (dec (count messages))])
          (update :queue-latencies conj [timestamp (- timestamp t)])))
      (-> this
        (update :takes conj [timestamp f])))))

(defn queue [max-queue-length]
  (MessageQueue. max-queue-length (empty-queue) (empty-queue) [] [] [] []))

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

(defn advance-until [s timestamp]
  (->> (iterate advance s)
    (drop-while #(< (-> % :events ffirst) timestamp))
    first))

;;;

(defn producer [queue-fn rate-fn task-lengths task-length-fn]
  (fn this [timestamp state]
    (execute-at!
      (+ timestamp (next-arrival (rate-fn timestamp)))
      this)
    (let [len (task-length-fn timestamp)]
      (-> state
        (update-in task-lengths conj [timestamp len])
        (update-in (queue-fn) put-msg timestamp len)))))

(defn consumer [queue latencies]
  (fn this [timestamp state]
    (update-in state queue take-msg timestamp
      (fn [task-length initial-time timestamp' state]
        (let [t' (+ timestamp' task-length)]
          (execute-at! t' this)
          (update-in state latencies conj [initial-time (- t' initial-time)]))))))

(defn simulation [state fs]
  (Simulation.
    (reduce
      #(push-event %1 0 %2)
      (event-heap)
      fs)
    state))

(def sim
  (simulation
    {:queue (queue 1e9), :latencies [], :task-lengths []}
    [(producer (constantly [:queue]) (constantly 4) [:task-lengths]
       (fn [t] (pareto (lerp 5 2 (/ t 1e6)) 0.5 100)))
     (consumer [:queue] [:latencies])
     (consumer [:queue] [:latencies])
     (consumer [:queue] [:latencies])
     (consumer [:queue] [:latencies])
     (consumer [:queue] [:latencies])]))

(defn multi-consumer-grid [counts directory]
  (doseq [[rate consumers] counts]
    (let [s (advance-until
              (simulation
                {:queue (queue 1e9), :latencies [], :task-lengths []}
                (list*
                  (producer (constantly [:queue]) (constantly rate) [:task-lengths]
                    (fn [t] (pareto (lerp 4 1.5 (/ t 1e6)) 0.5 100)))
                  (repeat consumers (consumer [:queue] [:latencies]))))
              1e6)]
      (->> s
        :state
        :latencies
        viz/log10
        (viz/spectrogram 5e3 0.05 3 55)
        (viz/save-image (str directory "/multi-consumer-rate-" rate "-consumers-" consumers ".png"))))))
