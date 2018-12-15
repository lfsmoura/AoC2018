(use 'clojure.java.io)
(use 'clojure.string)

(defn tap [val] 
  (do (println val) val))

(defn assert-equal [val expected] 
  (if (not= val expected) (do (println val "!=" expected) (assert (= val expected))) '()))

(defn react?
  [a b]
    (and (= (upper-case a) (upper-case b)) (not= a b))
)

(defn react-once
  {:test #(do
    (assert-equal (react-once '(\d \a \B \b)) '(\d \a))
    (assert-equal (react-once '(\d \D \a)) '(\a))
    (assert-equal (react-once '(\d \c \C \D)) '(\d \D)) 
    (assert-equal (react-once '(\d \a \B \A)) '(\d \a \B \A))      
  )}
  [chain]
    (if (= chain '()) '()
      (let [a (first chain) b (or (second chain) "")]
        (if (react? a b) 
          (rest (rest chain))
          (cons a (react-once (rest chain)))
        )
      )
    )
)

(test #'react-once)

(defn react
  {:test #(do
    (assert-equal (react "dabAcCaCBAcCcaDA") 10)
  )}
  [chain]
    (count (loop [chain chain]
      (let [result-chain (react-once chain)]
        (if (= result-chain chain)
          result-chain
          (recur result-chain)))))
)

(test #'react)

(defn best-react
  {:test #(do
    (assert-equal (best-react "dabAcCaCBAcCcaDA") 4)
  )}
  [chain]
    (let [chars (set (map clojure.string/lower-case chain))
          possibilites (map 
                          (fn [c] (filter
                                 (fn [el] (not= (lower-case el) c))
                                  chain))
                          chars)]
      (apply min (map react possibilites))
    )
)

(test #'best-react)

(def input (with-open [rdr (reader "input")]
	(doall (line-seq rdr) )))

(println (best-react (first input)))
