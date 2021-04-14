(ns eximia.core-test
  (:require [eximia.core :as e]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]])
  (:import [javax.xml XMLConstants]
           [javax.xml.stream XMLInputFactory]
           [eximia.core CData]))

(defn coalesce-strs [string? ->string coll]
  (loop [acc [], coll coll]
    (if (seq coll)
      (let [[strs coll] (split-with string? coll)]
        (if (seq strs)
          (recur (conj acc (str/join (map ->string strs))) coll)
          (recur (conj acc (first coll)) (rest coll))))
      acc)))

(defn canonicalize-strs [string? ->string  tree]
  (postwalk (fn [tree]
              (cond
                (and (map? tree) (contains? tree :tag))
                (update tree :content (comp #(filterv (fn [child] (or (not (string? child)) (seq child)))
                                                      %)
                                            #(coalesce-strs string? ->string %)))

                (and (map? tree) (contains? tree :target))
                (-> tree
                    (update :target #(if (= % "") nil %))
                    (update :data #(if (= % "") nil %)))

                :else tree))
            tree))

(defn characters? [v] (or (string? v) (instance? CData v)))

(defn characters->string [v] (if (string? v) v (.chars v)))

;;;; # Generators

(def name-gen
  (gen/let [c gen/char-alpha
            cs gen/string-alphanumeric]
           (str c cs)))

(def content-char-gen
  (gen/such-that (fn [^Character c]
                   (let [c (int c)]
                     (and (>= c 10) (< (.indexOf "<&" c) 0))))
                 gen/char-ascii))

(def characters-gen (gen/fmap str/join (gen/vector content-char-gen)))

(def cdata-gen (gen/fmap e/cdata (gen/such-that #(not (.contains % "]]>")) characters-gen)))

(def comment-gen (gen/fmap e/comment gen/string-alphanumeric))

(def pi-gen
  (gen/let [target name-gen
            data (gen/one-of [(gen/return nil) gen/string-alphanumeric])]
    (e/processing-instruction target data)))

(def qname-gen
  (gen/let [local-name name-gen
            namespace-uri (gen/one-of [(gen/return XMLConstants/NULL_NS_URI) name-gen])
            prefix (gen/one-of [(gen/return XMLConstants/DEFAULT_NS_PREFIX) name-gen])]
    (e/qname namespace-uri local-name prefix)))

(def attrs-gen (gen/map qname-gen characters-gen))

(defn element-of-gen [element-gen]
  (gen/let [tag qname-gen
            attrs attrs-gen
            content (gen/vector element-gen)]
    (e/element tag attrs content)))

(def element-gen-all
  (element-of-gen (gen/recursive-gen element-of-gen
                                     (gen/one-of [characters-gen cdata-gen pi-gen comment-gen]))))

(def element-gen-cdata
  (element-of-gen (gen/recursive-gen element-of-gen
                                     (gen/one-of [characters-gen cdata-gen]))))

;;;; # Tests

(defspec write-read
  50
  (for-all [el element-gen-cdata]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml)]
      (= (canonicalize-strs characters? characters->string el)
         (canonicalize-strs characters? characters->string el*)))))

(def noncoalescing-input-factory
  (doto (e/input-factory)
    (.setProperty XMLInputFactory/IS_COALESCING false)))

(defspec write-read-all
  50
  (for-all [el element-gen-all]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml
                          {:preserve #{:processing-instruction :cdata :comment}}
                          noncoalescing-input-factory)]
      (= (canonicalize-strs string? identity el)
         (canonicalize-strs string? identity el*)))))

(defspec write-read-cdata
  50
  (for-all [el element-gen-cdata]
    (let [xml (e/write-str el {:xml-version "1.1"})         ; Enable some extra encoding
          el* (e/read-str xml
                          {:preserve #{:cdata}}
                          noncoalescing-input-factory)]
      (= (canonicalize-strs string? identity el)
         (canonicalize-strs string? identity el*)))))
