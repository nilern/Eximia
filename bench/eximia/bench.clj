(ns eximia.bench
  (:require [eximia.core :as e]
            clojure.xml
            clojure.data.xml
            [criterium.core :as cc]
            [clojure.java.io :as io])
  (:import [java.io StringReader ByteArrayInputStream StringWriter ByteArrayOutputStream]))

;;;; # Reading

;;;; ## InputStream

(defn benchmark-inputstream [bytes]
  (println "### clojure.xml:\n")
  (cc/quick-bench (clojure.xml/parse (ByteArrayInputStream. bytes)))

  (println "\n### data.xml:\n")
  (cc/quick-bench (clojure.data.xml/parse (ByteArrayInputStream. bytes)))

  (println "\n### eximia:\n")
  (cc/quick-bench (e/read (ByteArrayInputStream. bytes))))

;;;; ## Reader

(defn benchmark-reader [str]
  (println "### data.xml:\n")
  (cc/quick-bench (clojure.data.xml/parse (StringReader. str)))

  (println "\n### eximia:\n")
  (cc/quick-bench (e/read (StringReader. str))))

;;;; ## String

(defn benchmark-from-str [str]
  (println "### data.xml:\n")
  (cc/quick-bench (clojure.data.xml/parse-str str))

  (println "\n### eximia:\n")
  (cc/quick-bench (e/read-str str)))

;;;; ## Run 'Em

(defn benchmark-read []
  (doseq [filename ["hello.xml", "1.xml", "10.xml", "100.xml"]
          :let [file (io/file (str "dev-resources/" filename))
                input-str (slurp file)
                input-bytes (.getBytes input-str)]]
    (println "\n#" filename "\n")

    (println "## InputStream\n")
    (benchmark-inputstream input-bytes)

    (println "\n## Reader\n")
    (benchmark-reader input-str)

    (println "\n## String\n")
    (benchmark-from-str input-str)))

;;;; # Writing

;;;; ## OutputStream

(defn benchmark-outputstream [eximia-tree]
  (println "### eximia:\n")
  (cc/quick-bench (e/write eximia-tree (ByteArrayOutputStream.))))

;;;; ## Writer

(defn benchmark-writer [clojure-xml-tree data-xml-tree eximia-tree]
  (println "### clojure.xml:\n")
  (cc/quick-bench
    (binding [*out* (StringWriter.)]
      (clojure.xml/emit clojure-xml-tree)))

  (println "\n### data.xml:\n")
  (cc/quick-bench (clojure.data.xml/emit data-xml-tree (StringWriter.)))

  (println "\n### eximia:\n")
  (cc/quick-bench (e/write eximia-tree (StringWriter.))))

;;;; ## String

(defn benchmark-to-str [data-xml-tree eximia-tree]
  (println "### data.xml:\n")
  (cc/quick-bench (clojure.data.xml/emit-str data-xml-tree))

  (println "\n### eximia:\n")
  (cc/quick-bench (e/write-str eximia-tree)))

;;;; ## Run 'Em

(defn benchmark-write []
  (doseq [filename ["hello.xml", "1.xml", "10.xml", "100.xml"]
          :let [file (io/file (str "dev-resources/" filename))
                ^String input (slurp file)
                ;; HACK: remove namespaces to make data.xml emit work:
                clojure-xml-tree (assoc (clojure.xml/parse (ByteArrayInputStream. (.getBytes input))) :attrs {})
                data-xml-tree (assoc (clojure.data.xml/parse-str input) :attrs {})
                eximia-tree (assoc (e/read-str input) :attrs {})]]

    (println "\n#" filename "\n")

    (println "## OutputStream\n")
    (benchmark-outputstream eximia-tree)

    (println "\n## Writer\n")
    (benchmark-writer clojure-xml-tree data-xml-tree eximia-tree)

    (println "\n## String\n")
    (benchmark-to-str data-xml-tree eximia-tree)))
