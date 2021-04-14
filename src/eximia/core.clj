(ns eximia.core
  (:refer-clojure :exclude [read])
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamWriter XMLStreamConstants XMLOutputFactory]
           [java.io Reader Writer InputStream OutputStream StringReader StringWriter]
           [javax.xml.namespace QName]
           [clojure.lang IPersistentMap]))

;;;; # QName Support

(defmethod print-method QName [^QName qname ^Writer out]
  (.write out "#qname[")
  (.write out (.toString qname))
  (.write out (int \])))

(defn qname
  "Create a [[javax.xml.namespace.QName]] from a local name and optional namespace URI and prefix."
  ([local-name] (QName. local-name))
  ([ns-uri local-name] (QName. ns-uri local-name))
  ([ns-uri local-name prefix] (QName. ns-uri local-name prefix)))

(defn local-name
  "Get the local name string of a [[javax.xml.namespace.QName]], similar to [[clojure.core/name]]."
  [^QName qname]
  (.getLocalPart qname))

(defn ns-uri
  "Get the namespace URI string of a [[javax.xml.namespace.QName]]."
  [^QName qname]
  (.getNamespaceURI qname))

(defn prefix
  "Get the namespace prefix string of a [[javax.xml.namespace.QName]], similar to [[clojure.core/namespace]]."
  [^QName qname]
  (.getPrefix qname))

;;;; # Output Conversions

(defprotocol ToStreamWriter
  "Conversions to [[javax.xml.stream.XMLStreamWriter]]"
  (-stream-writer ^XMLStreamWriter [self factory]
    "Wrap `self` into a [[javax.xml.stream.XMLStreamWriter]], using the [[javax.xml.stream XMLOutputFactory]] `factory`."))

(extend-protocol ToStreamWriter
  Writer
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self))

  OutputStream
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self)))

;;;; # Input Conversions

(defprotocol ToStreamReader
  "Conversions to [[javax.xml.stream.XMLStreamReader]]"
  (-stream-reader ^XMLStreamReader [self factory]
    "Wrap `self` into a [[javax.xml.stream.XMLStreamReader]], using the [[javax.xml.stream XMLInputFactory]] `factory`."))

(extend-protocol ToStreamReader
  Reader
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  InputStream
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self)))

;;;; # Writing

(defprotocol WriteXML
  "Emitting an XML fragment"
  (-write [self out] "Write the XML fragment for `self` into the [javax.xml.stream.XMLStreamWriter]] `out`."))

(defn- write-attrs [out attrs]
  (reduce-kv (fn [^XMLStreamWriter out, ^QName k, v]
               (doto out (.writeAttribute (.getPrefix k) (.getNamespaceURI k) (.getLocalPart k) v)))
             out attrs))

(defn- write-content [out content] (reduce (fn [out child] (-write child out) out) out content))

(defn- write-element [out tag attrs content]
  (let [^XMLStreamWriter out out
        ^QName tag tag]
    (if (seq content)
      (do (.writeStartElement out (.getPrefix tag) (.getLocalPart tag) (.getNamespaceURI tag))
          (write-attrs out attrs)
          (write-content out content)
          (.writeEndElement out))
      (do (.writeEmptyElement out (.getPrefix tag) (.getLocalPart tag) (.getNamespaceURI tag))
          (write-attrs out attrs)
          (write-content out content)))))

(defrecord Element [tag attrs content]
  WriteXML
  (-write [_ out] (write-element out tag attrs content)))

(defrecord CData [chars]
  WriteXML
  (-write [_ out] (.writeCData ^XMLStreamWriter out chars)))

(defrecord Comment [chars]
  WriteXML
  (-write [_ out] (.writeComment ^XMLStreamWriter out chars)))

(defrecord ProcessingInstruction [target data]
  WriteXML
  (-write [_ out]
    (if data
      (.writeProcessingInstruction ^XMLStreamWriter out target data)
      (.writeProcessingInstruction ^XMLStreamWriter out target))))

(extend-protocol WriteXML
  IPersistentMap
  (-write [{:keys [tag attrs content]} out] (write-element out tag attrs content))

  String
  (-write [s out] (.writeCharacters ^XMLStreamWriter out s)))

(defn- write-document [tree ^XMLStreamWriter out {:keys [xml-version]}]
  (if xml-version
    (.writeStartDocument out xml-version)
    (.writeStartDocument out))
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

(defn- parse-tokens [^XMLStreamReader input {:keys [preserve]}]
  (let [preserve-pis (contains? preserve :processing-instruction)
        preserve-cdata (contains? preserve :cdata)
        preserve-comments (contains? preserve :comment)]
    (letfn [(skip-prolog [^XMLStreamReader input]
              (.next input)                                 ; START_DOCUMENT
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

                  (XMLStreamConstants/CHARACTERS XMLStreamConstants/ENTITY_REFERENCE)
                  (let [s (.getText input)]
                    (.next input)
                    (recur (conj! elems s)))

                  XMLStreamConstants/CDATA
                  (let [s (.getText input)]
                    (.next input)
                    (recur (conj! elems (if preserve-cdata (->CData s) s)))) ; OPTIMIZE: branches every time

                  XMLStreamConstants/PROCESSING_INSTRUCTION
                  (if preserve-pis                          ; OPTIMIZE: branches every time
                    (let [target (.getPITarget input)
                          data (.getPIData input)]
                      (.next input)
                      (recur (conj! elems (->ProcessingInstruction target data))))
                    (do (.next input)
                        (recur elems)))

                  XMLStreamConstants/COMMENT
                  (if preserve-comments                     ; OPTIMIZE: branches every time
                    (let [s (.getText input)]
                      (.next input)
                      (recur (conj! elems (->Comment s))))
                    (do (.next input)
                        (recur elems)))

                  XMLStreamConstants/SPACE
                  (do (.next input)
                      (recur elems))

                  XMLStreamConstants/END_ELEMENT (do (.next input)
                                                     (persistent! elems)))))]

      (skip-prolog input)
      (let [v (parse-element input)]
        (skip-epilog input)
        (assert (not (.hasNext input)))
        v))))

;;;; # API

(defn input-factory ^XMLInputFactory [opts]
  (reduce-kv (fn [^XMLInputFactory factory k v]
               (let [k (if (keyword? k)
                         (case k
                           :validating XMLInputFactory/IS_VALIDATING
                           :namespace-aware XMLInputFactory/IS_NAMESPACE_AWARE
                           :coalescing XMLInputFactory/IS_COALESCING
                           :replacing-entity-references XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
                           :supporting-external-entities XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
                           :support-dtd XMLInputFactory/SUPPORT_DTD
                           :reporter XMLInputFactory/REPORTER
                           :resolver XMLInputFactory/RESOLVER
                           :allocator XMLInputFactory/ALLOCATOR
                           (throw (ex-info "Unknown XMLInputFactory property" {:property k})))
                         k)]
                 (doto factory (.setProperty k v))))
             (doto (XMLInputFactory/newFactory)
               ;; Prevent XXE vulnerability by default:
               (.setProperty XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES false))
             opts))

(def ^:private ^XMLInputFactory default-input-factory (input-factory {:coalescing true}))

(defn read
  ([input] (read input {}))
  ([input opts] (read input opts default-input-factory))
  ([input opts xml-input-factory]
   (with-open [input (-stream-reader input xml-input-factory)]
     (parse-tokens input opts))))

(defn read-str
  ([input] (read-str input {}))
  ([input opts] (read-str input opts default-input-factory))
  ([input opts xml-input-factory]
   (with-open [input (StringReader. input)]
     (read input opts xml-input-factory))))

(defn output-factory ^XMLOutputFactory [opts]
  (reduce-kv (fn [^XMLOutputFactory factory k v]
               (let [k (if (keyword? k)
                         (case k
                           :repairing-namespaces XMLOutputFactory/IS_REPAIRING_NAMESPACES
                           (throw (ex-info "Unknown XMLOutputFactory property" {:property k})))
                         k)]
                 (doto factory (.setProperty k v))))
             (XMLOutputFactory/newFactory) opts))

(def ^:private ^XMLOutputFactory default-output-factory (output-factory {:repairing-namespaces true}))

(defn write
  ([tree out] (write tree out {}))
  ([tree out opts] (write tree out opts default-output-factory))
  ([tree out opts xml-output-factory]
   (with-open [out (-stream-writer out xml-output-factory)]
     (write-document tree out opts))))

(defn write-str
  ([tree] (write-str tree {}))
  ([tree opts] (write-str tree opts default-output-factory))
  ([tree opts xml-output-factory]
   (with-open [out (StringWriter.)]
     (write tree out opts xml-output-factory)
     (.toString out))))
