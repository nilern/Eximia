(ns eximia.core-test
  (:require [eximia.core :as e]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]])
  (:import [javax.xml XMLConstants]
           [javax.xml.stream XMLInputFactory]))

(defn coalesce-strs [coll]
  (loop [acc [], coll coll]
    (if (seq coll)
      (let [[strs coll] (split-with string? coll)]
        (if (seq strs)
          (recur (conj acc (str/join strs)) coll)
          (recur (conj acc (first coll)) (rest coll))))
      acc)))

(defn canonicalize-strs [tree]
  (postwalk (fn [tree]
              (cond
                (and (map? tree) (contains? tree :tag))
                (update tree :content (comp #(filterv (fn [child] (or (not (string? child)) (seq child)))
                                                      %)
                                            coalesce-strs))

                (and (map? tree) (contains? tree :target))
                (-> tree
                    (update :target #(if (= % "") nil %))
                    (update :data #(if (= % "") nil %)))

                :else tree))
            tree))

;;;; # Generators

(def name-gen
  (gen/let [c gen/char-alpha
            cs gen/string-alphanumeric]
           (str c cs)))

(def content-char-gen
  (gen/such-that (fn [^Character c] (< (.indexOf "<&" (int c)) 0))
                 gen/char-ascii))

(def characters-gen (gen/fmap str/join (gen/vector content-char-gen)))

(def cdata-gen (gen/fmap e/cdata characters-gen))

(def comment-gen (gen/fmap e/comment gen/string))

(def pi-gen
  (gen/let [target name-gen
            data (gen/one-of [gen/string-alphanumeric (gen/return nil)])]
    (e/processing-instruction target data)))

(def qname-gen
  (gen/let [local-name name-gen
            namespace-uri (gen/one-of [name-gen (gen/return XMLConstants/NULL_NS_URI)])
            prefix (gen/one-of [name-gen (gen/return XMLConstants/DEFAULT_NS_PREFIX)])]
    (e/qname namespace-uri local-name prefix)))

(def attrs-gen (gen/map qname-gen gen/string))

(def non-element-content-gen (gen/one-of [characters-gen cdata-gen pi-gen comment-gen]))

(defn element-of-gen [element-gen]
  (gen/let [tag qname-gen
            attrs attrs-gen
            content (gen/vector element-gen)]
    (e/element tag attrs content)))

(def element-gen
  (element-of-gen (gen/recursive-gen element-of-gen non-element-content-gen)))

;;;; # Tests

(def input-factory
  (doto (e/input-factory)
    (.setProperty XMLInputFactory/IS_COALESCING false)))

(defspec write-read
  10000
  (for-all [el element-gen]
           (println "---")
           (prn (canonicalize-strs el))
           (let [xml (e/write-str el {:xml-version "1.1"})
                 _ (prn xml)
                 el* (e/read-str xml
                                 {:preserve #{:processing-instruction :cdata :comment}}
                                 input-factory)]
             (prn (canonicalize-strs el*))
             (= (canonicalize-strs el) (canonicalize-strs el*)))))
