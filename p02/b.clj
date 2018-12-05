(use 'clojure.java.io)

(defn tap [val] 
  (do (println val) val))

(defn assert_equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn diff_by_one
  "both words have same length"
  {:test #(do
    (assert (diff_by_one "abcd" "abcc"))
    (assert_equal (diff_by_one "xkcd" "xkcd") false)
    (assert_equal (diff_by_one "xkcd" "xkxk") false))}
  [w1 w2] 
    (= 1 (reduce + (map (fn [pair] (if (not= (first pair) (second pair)) 1 0 )) (partition 2 (interleave w1 w2)))))
)

(test #'diff_by_one)
    
(defn correct_id
  "day 2 - 2"
  {:test #(do
    (assert_equal (correct_id '("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")) "fgid"))}
  [words]
    (let [result (first (filter (fn [pair] (> (count (second pair)) 0)) (map (fn [w1] [w1, (filter (fn [w2] (diff_by_one w1 w2)) words)]) words)))] result)
)

;(test #'correct_id)


(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (correct_id input))
