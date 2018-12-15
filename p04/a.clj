(use 'clojure.java.io)

(defn parse-int [x] (Integer/parseInt x))

(defn tap [val] 
  (do (println val) val))

(defn assert-equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn parse-input 
  "string -> event"
  {:test #(do
    (assert-equal (parse-input "[1518-11-01 00:00] Guard #10 begins shift") '((:guard 10) 1518 11 1 0 0))
    (assert-equal (parse-input "[1518-11-01 00:05] falls asleep") '(:falls 1518 11 1 0 5))
    (assert-equal (parse-input "[1518-11-05 00:25] wakes up") '(:wakes 1518 11 5 0 25))
  )}
  [line]
    (cons 
        (cond
            (re-find #"Guard" line) (cons :guard (map parse-int (rest (re-find #"Guard #([0-9]+)" line))))
            (re-find #"falls" line) :falls
            (re-find #"wakes" line) :wakes )
        (map parse-int (rest (re-find #"([0-9]+)\-([0-9]+)\-([0-9]+) ([0-9]+):([0-9]+)" line))))
)

(test #'parse-input)

(def example '("[1518-11-01 00:00] Guard #10 begins shift", "[1518-11-01 00:05] falls asleep", "[1518-11-01 00:25] wakes up", "[1518-11-01 00:30] falls asleep", "[1518-11-01 00:55] wakes up", "[1518-11-01 23:58] Guard #99 begins shift", "[1518-11-02 00:40] falls asleep", "[1518-11-02 00:50] wakes up", "[1518-11-03 00:05] Guard #10 begins shift", "[1518-11-03 00:24] falls asleep", "[1518-11-03 00:29] wakes up", "[1518-11-04 00:02] Guard #99 begins shift", "[1518-11-04 00:36] falls asleep", "[1518-11-04 00:46] wakes up", "[1518-11-05 00:03] Guard #99 begins shift", "[1518-11-05 00:45] falls asleep", "[1518-11-05 00:55] wakes up"))

(defn sleeping-minutes
  {:test #(do
    (assert-equal (sleeping-minutes '("[1518-11-01 00:00] Guard #2 begins shift", "[1518-11-01 00:05] falls asleep", "[1518-11-01 00:10] wakes up")) [[2 5] [2 6] [2 7] [2 8] [2 9]])
  )}
  [seq]  
    (let [events (sort (fn [a b] (compare (into [] (rest a)) (into [](rest b)))) (map parse-input seq))] 
        (loop [events events guard nil minute 0 result []]
          (let [event (first events) next-events (rest events)]
            (if (= events []) result
              (case (first event)
                :wakes (let [minutes (map (fn [m] [guard m]) (range minute (last event) ))]
                          (recur next-events guard (last event) (concat minutes result)))
                :falls (recur next-events guard (last event) result)
                (recur next-events (second (first event)) (last event) result)))))))

(test #'sleeping-minutes)

(defn right-answer
  {:test #(do
    (assert-equal (right-answer (shuffle example)) 240)
  )}
  [seq]
    (let [sleep-schedule (sleeping-minutes seq)
          sleep-the-most (first (apply max-key second (frequencies (map first sleep-schedule))))
          minute-more-slept (first (apply max-key second (frequencies (map second (filter (fn [[guard m]] (= guard sleep-the-most)) sleep-schedule)))))]
      (* sleep-the-most minute-more-slept))
)

(test #'right-answer)

(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (right-answer input))
