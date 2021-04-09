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

(defn- parse-tokens [^XMLStreamReader input]

  ;;; * NAMESPACE and ATTRIBUTE are subservient to START_ELEMENT and don't appear in XMLStreamReaders
  ;;; * START_DOCUMENT, SPACE, COMMENT, DTD, NOTATION_DECLARATION, ENTITY_DECLARATION and PROCESSING_INSTRUCTION
  ;;;   are skipped
  ;;; * Sequences of CDATA, CHARACTERS and ENTITY_REFERENCE strings are concatenated
  ;;; * END_DOCUMENT is only checked for in `parse` (TODO: and redundantly even there?)

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

                (XMLStreamConstants/CHARACTERS XMLStreamConstants/CDATA XMLStreamConstants/ENTITY_REFERENCE)
                (recur (conj! elems (parse-chars input)))

                (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT
                 XMLStreamConstants/DTD XMLStreamConstants/NOTATION_DECLARATION XMLStreamConstants/ENTITY_DECLARATION
                 XMLStreamConstants/PROCESSING_INSTRUCTION)
                (do (.next input)
                    (recur elems))

                XMLStreamConstants/END_ELEMENT (do (.next input)
                                                   (persistent! elems)))))

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

                  (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT
                   XMLStreamConstants/DTD XMLStreamConstants/NOTATION_DECLARATION XMLStreamConstants/ENTITY_DECLARATION
                   XMLStreamConstants/PROCESSING_INSTRUCTION)
                  (do (.next input)
                      (recur))

                  (.toString sb)))))]

    (eval-case (.next input)                                ; also skips START_DOCUMENT
      XMLStreamConstants/START_ELEMENT (parse-element input)

      (XMLStreamConstants/SPACE XMLStreamConstants/COMMENT
       XMLStreamConstants/DTD XMLStreamConstants/NOTATION_DECLARATION XMLStreamConstants/ENTITY_DECLARATION
       XMLStreamConstants/PROCESSING_INSTRUCTION)
      (do (.next input)
          (recur input)))))

;;;; # API

(def ^:private ^XMLInputFactory default-factory
  (doto (XMLInputFactory/newFactory)
    (.setProperty "javax.xml.stream.isSupportingExternalEntities" false)))

(defn parse
  ([input] (parse input default-factory))
  ([input xml-input-factory]
   (let [input (-stream-reader input xml-input-factory)]
     (try
       (let [v (parse-tokens input)]
         (if (identical? (.getEventType input) XMLStreamConstants/END_DOCUMENT)
           v
           (throw (XMLStreamException. (str "Expected END_DOCUMENT, got " (.getEventType input))
                                       (.getLocation input)))))
       (finally (.close input))))))
