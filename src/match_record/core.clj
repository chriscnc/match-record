(ns match-record.core)


(defn parse-clause 
  [clause] 
  (let [pattern (first clause)
        body (nth clause 2)]
    (if (and (symbol? pattern)
             (= '_ pattern))
      {:pattern pattern
       :rec-type nil
       :rec-fields nil
       :body body}
      (let [rec-type (first pattern)
            rec-fields (rest pattern)]
        {:pattern pattern
         :rec-type rec-type
         :rec-fields rec-fields
         :body body}))))


(defn emit [parsed-clause record-var]
  (let [{:keys [pattern, rec-type, rec-fields, body]} parsed-clause]
    (if (= '_ pattern)
      `(~body)
      `(~rec-type (let [{:keys [~@rec-fields]} ~record-var] ~body))
    )))


(defmacro match-record 
  [record-var & body]
  (let [clauses (partition 3 body)]
    `(condp = (type ~record-var)
       ~@(mapcat #(emit % record-var) (map parse-clause clauses)))
  ))

