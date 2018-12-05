(use 'clojure.java.io)
(use 'clojure.set)

(defn tap [val] 
  (do (println val) val))

(defn assert-equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn parse-input 
  "string -> plan"
  {:test #(do
    (assert-equal (parse-input "#1 @ 1,3: 4x4") '(1 1 3 4 4))
    (assert-equal (parse-input "#339 @ 99,438: 14x25") '(339 99 438 14 25))
  )}
  [line]
    ; first element is the matched string
    (map read-string (rest (re-find #"([0-9]+) @ ([0-9]+)\,([0-9]+)\: ([0-9]+)x([0-9]+)" line))))

(test #'parse-input)

(defn fabric-map
  "plan -> fabric-map"
  {:test #(do
    (assert-equal (fabric-map '(4 1 1 2 2)) '((1 1) (1 2) (2 1) (2 2)))
  )}
  [plan]
    (let [[ignore x0 y0 width height] plan]
      (for [x (range x0 (+ x0 width)) y (range y0 (+ y0 height))]
        [x y]
      ))
)

(test #'fabric-map)

(defn non-overlapping
  [seq]
    ; modification of the function overlapping of the previous part, now we get tiles that have a frequency of one
    (let [plans (map parse-input seq)]
      (let [fabric-maps (partition 2 (flatten (map fabric-map plans)))]
        (filter (fn [freq] (= (second freq) 1)) (frequencies fabric-maps))
    ))
)

(defn right-answer
  {:test #(do
    (assert-equal (right-answer '("#20 @ 1,1: 2x2" "#1 @ 2,2: 1x1" "#300 @ 3,3: 2x2")) 300)
  )}
  [seq]
    (let [plans (map parse-input seq) no-map (into #{} (non-overlapping seq))]
      ; the first element of the plan is the id, according to the problem description there's only one plan that do not overlap others
      (first (first
        ; filter plans for the one for which the resulting tiles are included in the non-overlapping tiles 
        (filter
          (fn [line] 
            (subset? (into #{} (frequencies (fabric-map line))) no-map)) 
          plans)))
    )
 )

(test #'right-answer)

(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (right-answer input))
