(ns eximia.core
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamConstants]
           [java.io Reader InputStream StringReader]
           [javax.xml.transform Source]))

;;;; # Input Conversions

(defprotocol ^:private ToStreamReader
  (-stream-reader ^XMLStreamReader [self factory]))

(extend-protocol ToStreamReader
  Reader
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  InputStream
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  Source
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  String
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory (StringReader. self))))

;;;; # Parsing

(defmacro ^:private eval-case [e & clauses]
  `(case ~e
     ~@(->> clauses
            (partition-all 2)
            (mapcat (fn [clause]
                      (case (count clause)
                        1 clause
                        2 (let [[pat expr] clause]
                            [(if (seq? pat) (map eval pat) (eval pat))
                             expr])))))))

(defrecord Element [tag attrs content])

(defn- parse-tokens [^XMLStreamReader input]
  (letfn [(skip-prolog [^XMLStreamReader input]
            (.next input)                                   ; START_DOCUMENT
            (loop []
              (eval-case (.getEventType input)
                (XMLStreamConstants/DTD
                 XMLStreamConstants/COMMENT XMLStreamConstants/SPACE XMLStreamConstants/PROCESSING_INSTRUCTION)
                (do (.next input)
                    (recur))

                nil)))

          (skip-epilog [^XMLStreamReader input]
            (loop []
              (eval-case (.getEventType input)
                (XMLStreamConstants/COMMENT XMLStreamConstants/SPACE XMLStreamConstants/PROCESSING_INSTRUCTION)
                (do (.next input)
                    (recur))

                XMLStreamConstants/END_DOCUMENT nil)))

          (parse-element [^XMLStreamReader input]
            (let [tag (keyword (.getLocalName input))
                  attrs (parse-attrs input)
                  content (parse-contents input)]
              (Element. tag attrs content)))

          (parse-attrs [^XMLStreamReader input]
            (let [attr-count (.getAttributeCount input)]
              (loop [i 0, attrs (transient {})]
                (if (< i attr-count)
                  (recur (inc i)
                         (assoc! attrs
                                 (keyword (.getAttributeNamespace input i) (.getAttributeLocalName input i))
                                 (.getAttributeValue input i)))
                  (do (.next input)
                      (persistent! attrs))))))

          (parse-contents [^XMLStreamReader input]
            (loop [elems (transient [])]
              (eval-case (.getEventType input)
                XMLStreamConstants/START_ELEMENT (recur (conj! elems (parse-element input)))

                (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA XMLStreamConstants/ENTITY_REFERENCE)
                (recur (conj! elems (parse-chars input)))

                (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT XMLStreamConstants/PROCESSING_INSTRUCTION)
                (do (.next input)
                    (recur elems))

                XMLStreamConstants/END_ELEMENT (do (.next input)
                                                   (persistent! elems)))))

          ;; FIXME: Skip "insignificant" whitespace, like clojure.xml:
          (parse-chars [^XMLStreamReader input]
            (let [sb (StringBuilder.)]
              (loop []
                (eval-case (.getEventType input)
                  (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA)
                  (do (.append sb (.getTextCharacters input) (.getTextStart input) (.getTextLength input))
                      (.next input)
                      (recur))

                  XMLStreamConstants/ENTITY_REFERENCE (do (.append sb (.getText input))
                                                          (recur))

                  (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT XMLStreamConstants/PROCESSING_INSTRUCTION)
                  (do (.next input)
                      (recur))

                  (.toString sb)))))]

    (skip-prolog input)
    (let [v (parse-element input)]
      (skip-epilog input)
      v)))

;;;; # API

(def ^:private ^XMLInputFactory default-factory
  (doto (XMLInputFactory/newFactory)
    (.setProperty "javax.xml.stream.isSupportingExternalEntities" false)))

(defn parse
  ([input] (parse input default-factory))
  ([input xml-input-factory]
   (with-open [input (-stream-reader input xml-input-factory)]
     (let [v (parse-tokens input)]
       (assert (not (.hasNext input)))
       v))))
