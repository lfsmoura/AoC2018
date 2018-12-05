(use 'clojure.java.io)

(defn f_twice 
	([seq] (f_twice (cycle seq) 0 #{}))
	([seq current_freq dict]
		;(do (println current_freq dict (take 3 seq))
			(if (contains? dict current_freq) (do (println "result" current_freq) current_freq)
				(recur 0 1 (f_twice (rest seq) 
				 	(+ current_freq (first seq)) 
				 	(conj dict current_freq)
				)))
		;)
	)
)

; some test becausa I'm a software professional
(def ex1 '(1,-1))

(assert (= (f_twice ex1) 0))
(assert (= (f_twice '(3,3,4,-2,-4)) 10))
(assert (= (f_twice '(7,7,-2,-7,-4)) 14))

; does the actual thing
(def input (with-open [rdr (reader "input")]
	(map read-string (doall (line-seq rdr) ))))

(println (f_twice input))
