(ns eximia.core
  (:refer-clojure :exclude [read])
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamWriter XMLStreamConstants XMLOutputFactory]
           [java.io Reader Writer InputStream OutputStream StringReader StringWriter]
           [javax.xml.namespace QName]))

;;;; # QName Support

(defmethod print-method QName [^QName qname ^Writer out]
  (.write out "#qname[")
  (.write out (.toString qname))
  (.write out (int \])))

;;;; # Output Conversions

(defprotocol ^:private ToStreamWriter
  (-stream-writer ^XMLStreamWriter [self factory]))

(extend-protocol ToStreamWriter
  Writer
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self))

  OutputStream
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self)))

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

;;;; # Writing

(defprotocol WriteXML
  (-write [self ^XMLStreamWriter out]))

(defrecord Element [tag attrs content]
  WriteXML
  (-write [_ out]
    (let [^XMLStreamWriter out out
          ^QName tag tag]
      (.writeStartElement out (.getPrefix tag) (.getLocalPart tag) (.getNamespaceURI tag))
      (reduce-kv (fn [^XMLStreamWriter out ^QName k v]
                   (.writeAttribute out (.getPrefix k) (.getLocalPart k) (.getNamespaceURI k) v))
                 out attrs)
      (reduce (fn [out child] (-write child out) out) out content)
      (.writeEndElement out))))

(extend-type String
  WriteXML
  (-write [s out] (.writeCharacters ^XMLStreamWriter out s)))

(defn- write-document [tree ^XMLStreamWriter out]
  (.writeStartDocument out)
  (-write tree out)
  (.writeEndDocument out))

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

(def ^:private ^XMLInputFactory default-input-factory
  (doto (XMLInputFactory/newFactory)
    (.setProperty XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES false)
    (.setProperty XMLInputFactory/IS_COALESCING true)))

(defn read
  ([input] (read input default-input-factory))
  ([input xml-input-factory]
   (with-open [input (-stream-reader input xml-input-factory)]
     (parse-tokens input))))

(def ^:private ^XMLOutputFactory default-output-factory
  (doto (XMLOutputFactory/newFactory)
    (.setProperty XMLOutputFactory/IS_REPAIRING_NAMESPACES true)))

(defn write
  ([tree out] (write tree out default-output-factory))
  ([tree out xml-output-factory]
   (with-open [out (-stream-writer out xml-output-factory)]
     (write-document tree out))))

(defn write-str
  ([tree] (write-str tree default-output-factory))
  ([tree xml-output-factory]
   (with-open [sout (StringWriter.)
               out (-stream-writer sout xml-output-factory)]
     (write-document tree out)
     (.toString sout))))
