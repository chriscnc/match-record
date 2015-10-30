(ns match-record.core-test
  (:require [clojure.test :refer :all]
            [match-record.core :refer :all]))


(deftest test-emit
  (testing "Single arg pattern"
    (is (= (emit {:pattern '(VarExp id)
                  :rec-type 'VarExp
                  :rec-fields '(id)
                  :body 'id}
                 'rec-var)
           '(VarExp (clojure.core/let [{:keys[id]} rec-var] id)))))
  (testing "Multi arg pattern"
    (is (= (emit {:pattern '(LambdaExp id body)
                  :rec-type 'LambdaExp
                  :rec-fields '(id body)
                  :body '(list 'lambda
                               (list unparse-expr id)
                               (unparse-expr body))}
                 'rec-var)
           '(LambdaExp (clojure.core/let [{:keys[id body]} rec-var] 
                         (list 'lambda 
                               (list unparse-expr id)
                               (unparse-expr body)))))))
  (testing "Default clause"
    (is (= (emit {:pattern '_
                  :rec-type nil
                  :rec-fields nil
                  :body '(throw (Exception. "No matching type"))}
                 'rec-var)
           '((throw (Exception. "No matching type")))))))


(deftest test-parse-clause
  (testing "Single arg pattern"
    (let [c '((VarExp id) -> id)]
      (is (= (parse-clause c)
             {:pattern '(VarExp id)
              :rec-type 'VarExp
              :rec-fields '(id)
              :body 'id}))))
  (testing "Multi arg pattern"
    (let [c '((LambdaExp id body) -> (list 'lambda
                                           (list unparse-expr id)
                                           (unparse-expr body)))]
      (is (= (parse-clause c)
             {:pattern '(LambdaExp id body)
              :rec-type 'LambdaExp
              :rec-fields '(id body)
              :body '(list 'lambda
                           (list unparse-expr id)
                           (unparse-expr body))}))))
  (testing "Default clause"
    (let [c '(_ -> (throw (Exception. "No matching type")))]
      (is (= (parse-clause c)
             {:pattern '_
              :rec-type nil
              :rec-fields nil
              :body '(throw (Exception. "No matching type"))})))))


(defrecord VarExp [id])
(defrecord LambdaExp [id body])
(defrecord AppExp [rator rand])

(defn unparse-ast
  [node]
  (match-record node 
    (VarExp id) -> id
    (LambdaExp id body) -> (list 'lambda 
                                 (list (unparse-ast id))
                                 (unparse-ast body))
    (AppExp rator rand) -> (list (unparse-ast rator)
                                 (unparse-ast rand))
    _ -> (throw (Exception. "No match for type"))))

(deftest test-macro
  (let [e (AppExp. (LambdaExp. (VarExp. 'a)
                               (AppExp. (VarExp. 'a)
                                        (VarExp. 'b)))
                   (VarExp. 'c))]
    (is (= (unparse-ast e)
           '((lambda (a) (a b)) c)))))

