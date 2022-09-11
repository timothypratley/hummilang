(ns hummilang.parser
  (:refer-clojure :exclude [parse read])
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [instaparse.transform :as t]
            [clojure.java.io :as io]))

(def parse
  (insta/parser (io/resource "hummilang-grammar.ebnf")))

(def tag-transform
  {:number edn/read-string
   :symbol symbol
   :quote  #(list 'quote %)
   :list   list
   :set    hash-set
   :forms  list})

(defn read [s]
  (->> (parse s)
    (t/transform tag-transform)))
