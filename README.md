# match-record

A Clojure library with a match macro `match-record` for the purpose of matching on Clojure record types. 

## Motivation
To emulate the `deftype` macro from the Racket EOPL dialect, and to have some fun constructing a non-trival macro in Clojure.

## Usage

```clojure
; Given an AST represented as nested records
(defrecord VarExp [id])
(defrecord LambdaExp [id body])
(defrecord AppExp [rator rand])

; And such an AST...
(def e (AppExp. (LambdaExp. (VarExp. 'a)
                            (AppExp. (VarExp. 'a)
                                     (VarExp. 'b)))
                (VarExp. 'c)))

; which represents...
 ((lambda (a) (a b)) c)

; instead of writing a functions like this...
(defn unparse-ast
  [node]
  (condp = (type node)
    VarExp (let [{:keys [id]} node] 
             id)
    LambdaExp (let [{:keys [id body]} node]
                (list 'lambda 
                      (list (unparse-ast id))
                      (unparse-ast body)))
    AppExp (let [{:keys [rator rand]} node]
             (list (unparse-ast rator)
                   (unparse-ast rand)))
    (throw (Exception. "Unmatched type"))))

; we would like to write this...
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

(unparse-ast e) ;-> ((lambda (a) (a b)) c)
```

## License

Copyright © 2015 Chris Cornelison

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
