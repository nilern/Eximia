# Eximia

[![Clojars Project](https://img.shields.io/clojars/v/com.deepbeginnings/eximia.svg)](https://clojars.org/com.deepbeginnings/eximia)
[![cljdoc badge](https://cljdoc.org/badge/com.deepbeginnings/eximia)](https://cljdoc.org/d/com.deepbeginnings/eximia/CURRENT)
[![Build Status](https://img.shields.io/github/workflow/status/nilern/eximia/Run%20tests.svg)](https://github.com/nilern/eximia/actions)

> *Eximia Cum Laude Approbatur* or just **E** is the second highest grade in the Finnish Matriculation Exam.
> It was split off from the highest *Laudatur* grade [in 1996](https://en.wikipedia.org/wiki/XML#History).

A fast and small XML processor for Clojure. With XML namespace support and secure defaults.

## Fast

* About 4x faster than `data.xml` (0.0.8) or `clojure.xml` (Clojure 1.10.3) on read
* About 3-4x faster than `data.xml` and about 7x faster than `clojure.xml` on write

## Small

One 300 SLOC namespace.

## Basic Usage

The requires required for these examples:

```clojure
> (require '[eximia.core :as exml] '[clojure.java.io :as io])
```

### Reading

```clojure
> (with-open [input (io/input-stream "dev-resources/hello.xml")]
    (exml/read input))
;=> #eximia.core.Element{:tag #qname[greeting], :attrs {#qname[style] "programmatic"}, :content ["Hello, world!"]}

;; Reader works too (but can be slightly slower):
> (with-open [input (io/reader "dev-resources/hello.xml")]
    (exml/read input))
;=> #eximia.core.Element{:tag #qname[greeting], :attrs {#qname[style] "programmatic"}, :content ["Hello, world!"]}

;; Even slower, just to demonstrate `read-str`:
> (exml/read-str (slurp "dev-resources/hello.xml"))
;=> #eximia.core.Element{:tag #qname[greeting], :attrs {#qname[style] "programmatic"}, :content ["Hello, world!"]}

;; javax.xml.namespace.QName:s are used by default, to support XML namespaces.
;; But keywords can be obtained instead:
> (with-open [input (io/input-stream "dev-resources/hello.xml")]
    (exml/read input {:tag-fn exml/qname->keyword, :key-fn exml/qname->keyword}))
;=> #eximia.core.Element{:tag :greeting, :attrs {:style "programmatic"}, :content ["Hello, world!"]}
```

### Writing

```clojure
> (def tree (exml/->Element (exml/qname "greeting") {(exml/qname "style") "programmatic"} ["Hello, world!"]))

> (exml/write tree System/out)
; prints <?xml version="1.0" ?><greeting xmlns="" style="programmatic">Hello, world!</greeting>
;=> nil

;; Writer works too (but can be slightly slower)
> (exml/write tree *out*)
; prints <?xml version="1.0" ?><greeting xmlns="" style="programmatic">Hello, world!</greeting>
;=> nil

;; `write-str` is also available:
> (exml/write-str tree)
;=> "<?xml version=\"1.0\" ?><greeting xmlns=\"\" style=\"programmatic\">Hello, world!</greeting>"

;; QName:s are used by default, to support XML namespaces. But keywords can be converted on write.
;; Also while a compact Element record type is provided, any map with the right keys works:
> (def tree* {:tag :greeting, :attrs {:style "programmatic"}, :content ["Hello, world!"]})
> (exml/write tree* System/out {:tag-fn exml/keyword->qname, :key-fn exml/keyword->qname})
; prints <?xml version="1.0" ?><greeting xmlns="" style="programmatic">Hello, world!</greeting>
;=> nil
```

## Tip: Non-Standard StAX Implementations

Eximia is built on the standard Java StAX cursor API and the JRE ships with an implementation of that. But you might
want to look at third-party StAX implementations such as [Woodstox](https://github.com/FasterXML/woodstox) which has
both more features (e.g. `IS_VALIDATING`) and better performance. [Aalto](https://github.com/FasterXML/aalto-xml) should
be even faster. Although the performance differences might be swamped by all the persistent tree allocations Eximia has
to do...
