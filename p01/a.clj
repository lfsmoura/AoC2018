(use 'clojure.java.io)

(println (with-open [rdr (reader "input")]
	(apply + (map read-string (doall (line-seq rdr) )))
))
