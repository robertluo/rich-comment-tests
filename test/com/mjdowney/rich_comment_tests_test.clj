(ns com.mjdowney.rich-comment-tests-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [com.mjdowney.rich-comment-tests :as rct]
            [com.mjdowney.rich-comment-tests.test-runner :as test-runner]
            [matcho.core :as m]
            [rewrite-clj.zip :as z]))

(deftest rct-tests
  (test-runner/run-tests-in-file-tree! :dirs #{"src"}))

(defn ctx-strings [comment-body]
  (let [form (str "^:rct/test\n(comment\n" comment-body "\n)")
        >str (comp string/trim string/join)]
    (->> (z/of-string form {:track-position? true})
         rct/rct-zlocs
         (mapcat rct/rct-data-seq)
         (map (juxt :test-sexpr (comp >str :context-strings)))
         (into {}))))

(deftest context-strings-test
  (let [strs (ctx-strings
               "(* 0 0) ;=> 0

                ;; Test for
                ;; addition
                (+ 1 1) ;=> 2
                (+ 2 2) ;=> 4

                (* 1 1) ;=> 1
                (* 2 3) ;=> 6

                ;; Squares
                ;; and such
                (* 2 2) ;=> 4
                ; 3 squared
                (* 3 3) ;=> 9")]
    (m/assert
      '{(* 0 0) ""
        (+ 1 1) ";; Test for\n;; addition"
        (+ 2 2) ";; Test for\n;; addition"
        (* 1 1) ""
        (* 2 3) ""
        (* 2 2) ";; Squares\n;; and such"
        (* 3 3) ";; Squares\n;; and such\n; 3 squared"}
      strs)))

(defn exp-strings [comment-body]
  (let [form (str "^:rct/test\n(comment\n" comment-body "\n)")
        >str (comp string/trim string/join)
        ?read-string #(if (empty? %) nil (read-string %))]
    (->> (z/of-string form {:track-position? true})
         rct/rct-zlocs
         (mapcat rct/rct-data-seq)
         (map (juxt :test-sexpr (comp ?read-string >str :expectation-string)))
         (into {}))))

(deftest expectation-strings-test
  (let [strs (exp-strings
               "(* 4 4) ;=> 16

               (def x {:a 1 :b 2})
               (update x :a inc)
               ;=>
               {:a 2
                :b 2}

               (println x)
               ;=>

               ; Removing :a from `x`
               (dissoc x :a)
               ;=> {:b
               ;
               ;     2}")]
    (m/assert
      '{(* 4 4)             16
        (def x {:a 1 :b 2}) nil
        (update x :a inc)   {:a 2 :b 2}
        (println x)         nil
        (dissoc x :a)       {:b 2}}
      strs)))

(defn test-sexprs
  "Extract just the test sexprs from a comment body string."
  [comment-body]
  (let [form (str "^:rct/test\n(comment\n" comment-body "\n)")]
    (->> (z/of-string form {:track-position? true})
         rct/rct-zlocs
         (mapcat rct/rct-data-seq)
         (mapv :test-sexpr))))

(deftest multiline-expectation-not-treated-as-test-sexpr
  ;; When ;=> appears on its own line, the following form is the expected value,
  ;; NOT a test expression. Verifies the follows-empty-result-comment? filter.
  (let [sexprs (test-sexprs
                "(update {} :a inc)
                  ;=>
                  {:a 1}

                  (+ 1 1) ;=> 2")]
    (is (= ['(update {} :a inc) '(+ 1 1)] sexprs)
        "Multi-line expectation value should not appear as a test sexpr")))

(defn rctstr
  "Run an RCT string through the test pipeline and capture output."
  [s]
  (rct/capture-clojure-test-out
   (rct/run-tests*
    (z/of-string
     (str "^:rct/test\n (comment\n" s "\n)")
     {:track-position? true}))))

(deftest reader-conditional-in-expectation
  ;; read-string with {:read-cond :allow} should handle reader conditionals
  ;; in expectation strings (the ;=> side)
  (is (= "" (rctstr "(+ 1 0) ;=> #?(:clj 1 :cljs 2)"))
      "Reader conditional in expectation should pass")
  (is (string/includes? (rctstr "(+ 1 0) ;=> #?(:clj 2 :cljs 1)")
                        "(not (= 2 1))")
      "the :clj branch is read and compared, not skipped"))

(deftest expectation-evaluates-after-the-test-sexpr
  (is (= "" (rctstr "(def m1 {:a 1}) ;=> #'m1"))
      "a var the test-sexpr defines resolves in the expectation")
  (is (string/includes? (rctstr "(def m1 {:a 1}) ;=> :not-the-var")
                        "expected: (= :not-the-var (def m1 {:a 1}))")
      "the expectation is compared, not skipped"))

(deftest a-throwing-expectation-errors-with-its-own-form
  ;; the second shape throws an Error, not an Exception
  (doseq [[assertion expected] {"(range 3) ;=> (0 1 2)" "expected: (= (0 1 2) (range 3))"
                                "1 ;=> (assert false)" "expected: (= (assert false) 1)"}]
    (let [result (rctstr (str assertion "\n(+ 1 1) ;=> 3"))]
      (is (string/includes? result expected)
          (str assertion " names the expectation"))
      (is (string/includes? result "(not (= 3 2))")
          (str assertion " does not end the block")))))

(deftest an-uncompilable-expectation-errors-at-its-own-line
  ;; rctstr prepends two lines, so the first assertion sits on line 3
  (doseq [expectation ["(no-such-fn 1 2)" "nope-not-a-var" "(inc 1 2 3)"]]
    (let [result (rctstr (str "1 ;=> " expectation "\n(+ 1 1) ;=> 3"))]
      (is (string/includes? result ":3)")
          (str expectation " reports the line it sits on"))
      (is (string/includes? result (str ";=> " expectation))
          (str expectation " names the expectation, not the test-sexpr"))
      (is (string/includes? result "(not (= 3 2))")
          (str expectation " does not end the block")))))

(deftest a-throwing-test-sexpr-errors-without-ending-the-block
  (let [result (rctstr "(/ 1 0) ;=> 1\n(+ 1 1) ;=> 3")]
    (is (string/includes? result "(/ 1 0)\n;=> 1")
        "the error names the form that threw")
    (is (string/includes? result "(not (= 3 2))")
        "the assertions following it still run")))
