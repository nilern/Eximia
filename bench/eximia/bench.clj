(ns eximia.bench
  (:require [eximia.core :as e]
            clojure.xml
            clojure.data.xml
            [criterium.core :as cc]
            [clojure.java.io :as io])
  (:import [java.io StringReader ByteArrayInputStream StringWriter ByteArrayOutputStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(comment
  ;; Force the default JRE StAX implementations:
  (System/setProperty "javax.xml.stream.XMLOutputFactory" "com.sun.xml.internal.stream.XMLOutputFactoryImpl")
  (System/setProperty "javax.xml.stream.XMLInputFactory" "com.sun.xml.internal.stream.XMLInputFactoryImpl")
  (System/setProperty "javax.xml.stream.XMLEventFactory" "com.sun.xml.internal.stream.events.XMLEventFactoryImpl"))

;;;; # Reading

(def pom-file (io/file "pom.xml"))
(def ^String pom-str (slurp pom-file))
(def pom-bytes (.getBytes pom-str))

;;;; ## InputStream

(defn benchmark-pom-io-inputstream []
  (println "# clojure.xml:\n")
  (cc/bench
    (with-open [input (io/input-stream pom-file)]
      (clojure.xml/parse input)))

  (println "\n# data.xml:\n")
  (cc/bench
    (with-open [input (io/input-stream pom-file)]
      (clojure.data.xml/parse input)))

  (println "\n# eximia:\n")
  (cc/bench
    (with-open [input (io/input-stream pom-file)]
      (e/read input))))

(defn benchmark-pom-inputstream []
  (println "# clojure.xml:\n")
  (cc/bench (clojure.xml/parse (ByteArrayInputStream. pom-bytes)))

  (println "\n# data.xml:\n")
  (cc/bench (clojure.data.xml/parse (ByteArrayInputStream. pom-bytes)))

  (println "\n# eximia:\n")
  (cc/bench (e/read (ByteArrayInputStream. pom-bytes))))

;;;; ## Reader

(defn benchmark-pom-io-reader []
  (println "# data.xml:\n")
  (cc/bench
    (with-open [input (io/reader pom-file)]
      (clojure.data.xml/parse input)))

  (println "\n# eximia:\n")
  (cc/bench
    (with-open [input (io/reader pom-file)]
      (e/read input))))

(defn benchmark-pom-reader []
  (println "# data.xml:\n")
  (cc/bench (e/read (StringReader. pom-str)))

  (println "\n# eximia:\n")
  (cc/bench (e/read (StringReader. pom-str))))

;;;; ## String

(defn benchmark-pom-str []
  (println "# data.xml:\n")
  (cc/bench (clojure.data.xml/parse-str pom-str))

  (println "\n# eximia:\n")
  (cc/bench (e/read-str pom-str)))

;;;; # Writing

;; HACK: remove namespaces to make data.xml emit work:
(def data-xml-tree (assoc (clojure.data.xml/parse-str pom-str) :attrs {}))
(def eximia-tree (assoc (e/read-str pom-str) :attrs {}))

(def temp-file (.toFile (Files/createTempFile nil ".xml" (make-array FileAttribute 0))))

;;;; ## OutputStream

(defn benchmark-pom-io-outputstream []
  (println "# eximia:\n")
  (cc/bench
    (with-open [output (io/output-stream temp-file)]
      (e/write eximia-tree output))))

(defn benchmark-pom-outputstream []
  (println "# eximia:\n")
  (cc/bench (e/write eximia-tree (ByteArrayOutputStream.))))

;;;; ## Writer

(defn benchmark-pom-io-writer []
  (println "# data.xml:\n")
  (cc/bench
    (with-open [output (io/writer temp-file)]
      (clojure.data.xml/emit data-xml-tree output)))

  (println "\n# eximia:\n")
  (cc/bench
    (with-open [output (io/writer temp-file)]
      (e/write eximia-tree output))))

(defn benchmark-pom-writer []
  (println "# data.xml:\n")
  (cc/bench (clojure.data.xml/emit data-xml-tree (StringWriter.)))

  (println "\n# eximia:\n")
  (cc/bench (e/write eximia-tree (StringWriter.))))

;;;; ## String

(defn benchmark-pom-to-str []
  (println "# data.xml:\n")
  (cc/bench (clojure.data.xml/emit-str data-xml-tree))

  (println "\n# eximia:\n")
  (cc/bench (e/write-str eximia-tree)))
