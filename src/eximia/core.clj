(ns eximia.core
  "Fast and small XML processor for Clojure.

  Reads and writes a format similar to clojure.xml and data.xml:

  * Elements as maps with `:tag`, `:attrs` and `:content` keys
  * Character data as Strings
      - By default, adjacent character sections (including CDATA) are joined together to one string when reading.

  But tags and attribute names are represented as javax.xml.namespace `QName`s to support XML namespaces.

  CDATA blocks can also be read and written as [[CData]] records, processing instructions as [[ProcessingInstruction]]s
  and comments as [[Comment]]s."
  (:refer-clojure :exclude [read])
  (:import [javax.xml.stream XMLInputFactory XMLStreamReader XMLStreamWriter XMLStreamConstants XMLOutputFactory]
           [javax.xml.namespace QName]
           [javax.xml XMLConstants]
           [java.io Reader Writer InputStream OutputStream StringReader StringWriter]
           [clojure.lang IPersistentMap]))

;;;; # QName Support

(defmethod print-method QName [^QName qname ^Writer out]
  (.write out "#qname[")
  (.write out (.toString qname))
  (.write out (int \])))

(defn qname
  "Create a javax.xml.namespace.QName from a local name and optional namespace URI and prefix."
  ([local-name] (QName. local-name))
  ([ns-uri local-name] (QName. ns-uri local-name))
  ([ns-uri local-name prefix] (QName. ns-uri local-name prefix)))

(defn local-name
  "Get the local name string of a javax.xml.namespace.QName, similar to [[clojure.core/name]]."
  [^QName qname]
  (.getLocalPart qname))

(defn ns-uri
  "Get the namespace URI string of a javax.xml.namespace.QName."
  [^QName qname]
  (.getNamespaceURI qname))

(defn prefix
  "Get the namespace prefix string of a javax.xml.namespace.QName, similar to [[clojure.core/namespace]]."
  [^QName qname]
  (.getPrefix qname))

(defn qname->keyword
  "Convert the javax.xml.namespace.QName `qname` to a keyword, with the prefix as the keyword namespace."
  [^QName qname]
  (let [prefix (.getPrefix qname)
        local-name (.getLocalPart qname)]
    (if (= prefix XMLConstants/DEFAULT_NS_PREFIX)
      (keyword local-name)
      (keyword prefix local-name))))

(defn qname->unq-keyword
  "Convert the javax.xml.namespace.QName `qname` to an unqualified keyword."
  [^QName qname]
  (keyword (.getLocalPart qname)))

;;;; # Writing

;;;; ## Impl

(defprotocol WriteXML
  "Emitting an XML fragment"
  (-write [self out] "Write the XML fragment for `self` into the XMLStreamWriter `out`."))

(def ^:private write-element
  (letfn [(write-attrs [out attrs]
            (reduce-kv (fn [^XMLStreamWriter out, ^QName k, v]
                         (doto out (.writeAttribute (.getPrefix k) (.getNamespaceURI k) (.getLocalPart k) v)))
                       out attrs))
          (write-content [out content] (reduce (fn [out child] (-write child out) out) out content))]
    (fn [out tag attrs content]
      (let [^XMLStreamWriter out out
            ^QName tag tag]
        (if (seq content)
          (do (.writeStartElement out (.getPrefix tag) (.getLocalPart tag) (.getNamespaceURI tag))
              (write-attrs out attrs)
              (write-content out content)
              (.writeEndElement out))
          (do (.writeEmptyElement out (.getPrefix tag) (.getLocalPart tag) (.getNamespaceURI tag))
              (write-attrs out attrs)
              (write-content out content)))))))

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

;;;; ## Output Conversions

(defprotocol ToStreamWriter
  "Conversions to XMLStreamWriter"
  (-stream-writer ^XMLStreamWriter [self factory]
    "Wrap `self` into a XMLStreamWriter, using the XMLOutputFactory `factory`."))

(extend-protocol ToStreamWriter
  Writer
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self))

  OutputStream
  (-stream-writer [self factory] (.createXMLStreamWriter ^XMLOutputFactory factory self)))

;;;; ## Main Writing API

(defn output-factory
  "Create an XMLOutputFactory. The `opts` map keys and values are the
  [XMLOutputFactory properties](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLOutputFactory.html)
  but `:repairing-namespaces` can also be used instead of `XMLOutputFactory/IS_REPAIRING_NAMESPACES` or
  `\"javax.xml.stream.isRepairingNamespaces\"`.

  Note that XMLOutputFactory instances can be very expensive to create but are reusable. So call this as rarely as
  possible, probably only in the application startup phase."
  ^XMLOutputFactory [opts]
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
  "Write `tree` as XML to `out`, which should be an OutputStream or a Writer (or any [[ToStreamWriter]]). Does not
  close `out`.

  The optional `opts` map can have the following keys:

  | Key                   | Description             | Value                  | Default   |
  |-----------------------|-------------------------|------------------------|-----------|
  | `:xml-version`        | The XML standard to use | `\"1.0\"` or `\"1.1\"` | `\"1.0\"` |
  | `:xml-output-factory` | The XMLOutputFactory to use. See also [[output-factory]].
      | An XMLOutputFactory | A `(output-factory {:repairing-namespaces true})` cached internally in Eximia |"
  ([tree out] (write tree out {}))
  ([tree out opts]
   (with-open [out (-stream-writer out (get opts :xml-output-factory default-output-factory))]
     (write-document tree out opts))))

(defn write-str
  "Like [[write]], but returns a String instead of writing to a provided destination."
  ([tree] (write-str tree {}))
  ([tree opts]
   (with-open [out (StringWriter.)]
     (write tree out opts)
     (.toString out))))

;;;; # Reading

;;;; ## Impl

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

(defn- parse-tokens [^XMLStreamReader input {:keys [tag-fn key-fn wrap-cdata preserve]}]
  (let [tag-fn (or tag-fn identity)
        key-fn (or key-fn identity)
        wrap-cdata (boolean wrap-cdata)
        preserve-pis (contains? preserve :processing-instruction)
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
              (let [tag (tag-fn (.getName input))
                    attrs (parse-attrs input)
                    content (parse-contents input)]
                (Element. tag attrs content)))

            (parse-attrs [^XMLStreamReader input]
              (let [attr-count (.getAttributeCount input)]
                (loop [i 0, attrs (transient {})]
                  (if (< i attr-count)
                    (recur (inc i) (assoc! attrs (key-fn (.getAttributeName input i)) (.getAttributeValue input i)))
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
                    (recur (conj! elems (if wrap-cdata (->CData s) s)))) ; OPTIMIZE: branches every time

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

;;;; ## Input Conversions

(defprotocol ToStreamReader
  "Conversions to XMLStreamReader"
  (-stream-reader ^XMLStreamReader [self factory]
    "Wrap `self` into a XMLStreamReader, using the XMLInputFactory `factory`."))

(extend-protocol ToStreamReader
  Reader
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self))

  InputStream
  (-stream-reader [self factory] (.createXMLStreamReader ^XMLInputFactory factory self)))

;;;; ## Main Reading API

(defn input-factory
  "Create an XMLInputFactory. The `opts` map keys and values are the
  [XMLInputFactory properties](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/XMLInputFactory.html) but
  `:validating`, `:namespace-aware`, `:coalescing`, `:replacing-entity-references`, `:supporting-external-entities`,
  `:support-dtd`, `:reporter` and `:allocator` can be used instead of the XMLInputFactory constants or the raw
  \"javax.xml.stream.*\" strings.

  `:support-dtd` and `:supporting-external-entities` default to `false` to prevent
  [XXE attacks](https://owasp.org/www-community/vulnerabilities/XML_External_Entity_(XXE)_Processing).

  Note that XMLInputFactory instances can be very expensive to create but are reusable. So call this as rarely as
  possible, probably only in the application startup phase."
  ^XMLInputFactory [opts]
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
               (.setProperty XMLInputFactory/SUPPORT_DTD false)
               (.setProperty XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES false))
             opts))

(def ^:private ^XMLInputFactory default-input-factory (input-factory {:coalescing true}))

(defn read
  "Read XML from `input`, which should be an InputStream or a Reader (or any [[ToStreamReader]]).
  Does not close `input`.

  The optional `opts` map can have the following keys:

  | Key                  | Description                                 | Value  | Default    |
  |----------------------|---------------------------------------------|--------|------------|
  | `:tag-fn`            | Function to apply to tag `QName`s           | An IFn | `identity` |
  | `:key-fn`            | Function to apply to attribute key `QName`s | An IFn | `identity` |
  | `:wrap-cdata`        | Return CDATA contents wrapped in [[CData]] instead of just the String. | boolean | `false` |
  | `:preserve`          | Return [[ProcessingInstruction]]s and [[Comment]]s instead of skipping them.
      | A subset of `#{:processing-instruction, :comment}` | `#{}` |
  | `:xml-input-factory` | The XMLInputFactory to use. See also [[input-factory]].
      | An XMLInputFactory | A `(input-factory {:coalescing true})` cached internally in Eximia |"
  ([input] (read input {}))
  ([input opts]
   (with-open [input (-stream-reader input (get opts :xml-input-factory default-input-factory))]
     (parse-tokens input opts))))

(defn read-str
  "Like [[read]], but with a String as the `input`."
  ([input] (read-str input {}))
  ([input opts]
   (with-open [input (StringReader. input)]
     (read input opts))))
