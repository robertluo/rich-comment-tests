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
  (let [result (rctstr "(+ 1 0) ;=> #?(:clj 1 :cljs 2)")]
    (is (not (string/includes? result "FAIL"))
        "Reader conditional in expectation should pass")
    (is (not (string/includes? result "ERROR"))
        "Reader conditional in expectation should not error")))
