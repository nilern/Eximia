(ns eximia.core-test
  (:require [eximia.core :as e]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str])
  (:import [javax.xml XMLConstants]))

;;;; # Generators

(def content-char-gen (gen/such-that (fn [^Character c] (< (.indexOf "<&" (int c)) 0)) gen/char))

(def characters-gen (gen/fmap str/join (gen/vector content-char-gen)))

(def cdata-gen (gen/fmap e/cdata characters-gen))

(def comment-gen (gen/fmap e/comment gen/string))

(def pi-gen
  (gen/let [target gen/string
            data (gen/one-of [gen/string (gen/return nil)])]
    e/processing-instruction ))

;; HACK:
(def name-gen (gen/fmap name gen/symbol))

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

(defspec write-read
  (for-all [element element-gen]
    (= element (e/read-str (e/write-str element)))))
