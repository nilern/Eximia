(ns eximia.core
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamConstants XMLStreamException]
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

(def ^:private parse-tokens
  (letfn [(parse-element [^XMLStreamReader input]
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

                (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA) (if-not (.isWhiteSpace input)
                                                                           (recur (conj! elems (parse-chars input)))
                                                                           (do (.next input)
                                                                               (recur elems)))

                (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT) (do (.next input)
                                                                          (recur elems))

                XMLStreamConstants/END_ELEMENT (do (.next input)
                                                   (persistent! elems)))))

          (parse-chars [^XMLStreamReader input]
            (let [sb (StringBuilder.)]
              (.append sb (.getTextCharacters input) (.getTextStart input) (.getTextLength input))
              (.next input)

              (loop []
                (eval-case (.getEventType input)
                  (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA)
                  (if-not (.isWhiteSpace input)
                    (.append sb (.getTextCharacters input) (.getTextStart input) (.getTextLength input))
                    (do (.next input)
                        (recur)))

                  (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT) (do (.next input)
                                                                            (recur))

                  (.toString sb)))))

          (parse-any [^XMLStreamReader input]
            (eval-case (.getEventType input)
              XMLStreamConstants/START_ELEMENT (parse-element input)

              (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA) (if-not (.isWhiteSpace input)
                                                                         (parse-chars input)
                                                                         (do (.next input)
                                                                             (recur input)))

              (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT) (do (.next input)
                                                                        (recur input))))]
    parse-any))

;;;; # API

(def ^:private ^XMLInputFactory default-factory
  (doto (XMLInputFactory/newFactory)
    (.setProperty "javax.xml.stream.isSupportingExternalEntities" false)))

(defn parse
  ([input] (parse input default-factory))
  ([input xml-input-factory]
   (let [input (-stream-reader input xml-input-factory)]
     (try
       (.next input)                                        ; discard START_DOCUMENT
       (let [v (parse-tokens input)]
         (if (identical? (.getEventType input) XMLStreamConstants/END_DOCUMENT)
           v
           (throw (XMLStreamException. (str "Expected END_DOCUMENT, got " (.getEventType input))
                                       (.getLocation input)))))
       (finally (.close input))))))
