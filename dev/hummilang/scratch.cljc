(ns hummilang.scratch
  (:require [hummilang.hummicore :as hl]
            [hummilang.parser :as hp]))

(eval (hp/read (slurp "dev/examples/basic.hum")))

(hl/read-evaluate (slurp "dev/examples/basic.hum"))
