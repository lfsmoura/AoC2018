(use 'clojure.java.io)

(defn tap [val] 
  (do (println val) val))

(defn assert-equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn parse-input 
  "string -> plan"
  {:test #(do
    (assert-equal (parse-input "#1 @ 1,3: 4x4") '((1 3) (4 4)))
    (assert-equal (parse-input "#339 @ 99,438: 14x25") '((99 438) (14 25)))
  )}
  [line]
    ; first element is the matched string
    (partition 2 (map read-string (rest (re-find #"([0-9]+)\,([0-9]+)\: ([0-9]+)x([0-9]+)" line))))
)

(test #'parse-input)

(defn fabric-map
  "plan -> fabric-map"
  {:test #(do
    (assert-equal (fabric-map '((1 1) (2 2))) '((1 1) (1 2) (2 1) (2 2)))
  )}
  [plan]
    (let [[x0 y0] (first plan) [width height] (second plan)]
      (for [x (range x0 (+ x0 width)) y (range y0 (+ y0 height))]
        [x y]
      ))
)

(test #'fabric-map)

(defn overlapping
  {:test #(do
    (assert-equal (overlapping ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"]) 4)
  )}
  [seq]
    (let [plans (map parse-input seq)]
      (let [fabric-maps (partition 2 (flatten (map fabric-map plans)))]
        (count (filter (fn [freq] (> (second freq) 1)) (frequencies fabric-maps)))
    ))
)

(test #'overlapping)

(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (overlapping input))
