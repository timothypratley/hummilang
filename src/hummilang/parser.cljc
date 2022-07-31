(ns hummilang.parser
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def hlp
  (insta/parser (io/resource "hummilang-grammar.ebnf")))

(prn (hlp "(1 -2.0 alpha {0 {} (1 2) x})"))
