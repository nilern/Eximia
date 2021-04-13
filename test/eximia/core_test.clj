(ns eximia.core-test
  (:require [eximia.core :as e]
            [clojure.test :refer :all]
            [clojure.test.check.generators :as gen])
  (:import [javax.xml XMLConstants]))

;;;; # Generators

(def characters-gen gen/string)

(def cdata-gen (gen/fmap e/cdata characters-gen))

(def comment-gen (gen/fmap e/comment gen/string))

(def pi-gen
  (gen/let [target gen/string
            data (gen/one-of [gen/string (gen/return nil)])]
    e/processing-instruction ))

(def qname-gen
  (gen/let [local-name gen/string
            namespace-uri (gen/one-of [gen/string (gen/return XMLConstants/NULL_NS_URI)])
            prefix (gen/one-of [gen/string (gen/return XMLConstants/DEFAULT_NS_PREFIX)])]
    (e/qname namespace-uri local-name prefix)))

(def attrs-gen (gen/map qname-gen gen/string))

(def non-element-content-gen (gen/one-of [characters-gen cdata-gen comment-gen pi-gen]))

(defn element-of-gen [element-gen]
  (gen/let [tag qname-gen
            attrs attrs-gen
            content (gen/vector element-gen)]
    (e/element tag attrs content)))

(def element-gen
  (element-of-gen (gen/recursive-gen element-of-gen non-element-content-gen)))
