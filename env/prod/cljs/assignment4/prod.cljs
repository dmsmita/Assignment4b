(ns assignment4.prod
  (:require [assignment4.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
