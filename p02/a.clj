(use 'clojure.java.io)

(defn tap [val] 
  (do (println val) val))

(defn assert_equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn pad 
  "Fill coll with val until list is size n"
  {:test #(do 
    (assert_equal (pad 3 '(0) 0) [0 0 0]))}
  [n coll val] (take n (concat coll (repeat val))))

(test #'pad)

(defn sum_repeat 
  "[0,1], [0,0,2] => [0,1,2]"
  {:test #(do
    (assert_equal (sum_repeat [0 1 0] [0 0 2]) '(0 1 2)))}
  [a, b]
  (map (fn [a] (+ (first a) (second a)))
      (partition 2 
          (interleave (pad (max (count a) (count b)) a 0) (pad (max (count a) (count b)) b 0)))))

(test #'sum_repeat)


(defn repeat_count 
  ""
  {:test #(do
    (assert_equal (repeat_count '([a 2] [c 3])) [0,1,1]))} 
  [freq] 
    (let [dissaciated_counts (map (fn [c] (let [index (- (second c) 1)] (assoc (vec (pad index '() 0)) index 1))) 
        freq)]
      (reduce sum_repeat dissaciated_counts))
)

(test #'repeat_count)

(defn lcount
  "aabccc => {a 2, b 1, c 3} => ([a 2] [c 3]) => [ [0,1], [0,0,1] ] => [0,1,1]"
  {:test #(do
    (assert_equal (lcount "aaabb") [0,1,1])
    (assert_equal (lcount "aabcdd") [2,2])
    (assert_equal (lcount "abbffffxx") [1,2,0,1]))}
  [word]
    (repeat_count (frequencies word))
)

(test #'lcount)

(assert (= (lcount "bababc") [1,1,1]))
(assert (= (lcount "abcdef") [6]))
(assert (= (lcount "aaabbb") [0,0,2]))

(defn normalize
  {:test #(do
    (assert_equal (normalize '(0 2 0 3)) [0 1 0 1]))}
  [seq]
    (vec (map (fn [c] (if (> c 0) 1 0)) seq))
) 

(test #'normalize)

(defn checksum
  "day 2"
  {:test #(do 
    (assert_equal (checksum '["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]) 12))}
  [words] 
    (let [repeats (reduce sum_repeat (map normalize (map lcount words)))]
      (reduce * (rest repeats)))
)

(test #'checksum)

; does the actual thing
(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (checksum input))
