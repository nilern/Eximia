(ns eximia.core
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamConstants]
           [java.io Reader Writer InputStream StringReader]
           [javax.xml.namespace QName]))

;;;; # QName Support

(defmethod print-method QName [^QName qname ^Writer out]
  (.write out "#qname[")
  (.write out (.toString qname))
  (.write out (int \])))

;;;; # Input Conversions

(defprotocol ^:private ToStreamReader
  (-stream-reader ^XMLStreamReader [self factory]))

(extend-protocol ToStreamReader
  Reader
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  InputStream
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
            (let [tag (.getName input)
                  attrs (parse-attrs input)
                  content (parse-contents input)]
              (Element. tag attrs content)))

          (parse-attrs [^XMLStreamReader input]
            (let [attr-count (.getAttributeCount input)]
              (loop [i 0, attrs (transient {})]
                (if (< i attr-count)
                  (recur (inc i) (assoc! attrs (.getAttributeName input i) (.getAttributeValue input i)))
                  (do (.next input)
                      (persistent! attrs))))))

          (parse-contents [^XMLStreamReader input]
            (loop [elems (transient [])]
              (eval-case (.getEventType input)
                XMLStreamConstants/START_ELEMENT (recur (conj! elems (parse-element input)))

                (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA XMLStreamConstants/ENTITY_REFERENCE)
                (let [s (.getText input)]
                  (.next input)
                  (recur (conj! elems s)))

                (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT XMLStreamConstants/PROCESSING_INSTRUCTION)
                (do (.next input)
                    (recur elems))

                XMLStreamConstants/END_ELEMENT (do (.next input)
                                                   (persistent! elems)))))]

    (skip-prolog input)
    (let [v (parse-element input)]
      (skip-epilog input)
      (assert (not (.hasNext input)))
      v)))

;;;; # API

(def ^:private ^XMLInputFactory default-factory
  (doto (XMLInputFactory/newFactory)
    (.setProperty XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES false)
    (.setProperty XMLInputFactory/IS_COALESCING true)))

(defn parse
  ([input] (parse input default-factory))
  ([input xml-input-factory]
   (with-open [input (-stream-reader input xml-input-factory)]
     (parse-tokens input))))
