(ns mulesoft-clj.utils
  (:gen-class)
  (:require [csv-map.core :as csv]
           [clojure.java.io :as io]))

(defn qpmap
  "Code copied from pmap but this version allows to select the number of threads to use to limit the parallelism"
  ([f threads coll ]
    (let [n threads
          rets (map #(future (f %)) coll)
          step (fn step [[x & xs :as vs] fs]
                 (lazy-seq
                   (if-let [s (seq fs)]
                     (cons (deref x) (step xs (rest s)))
                     (map deref vs))))]
      (step rets (drop n rets))))
  ([f threads coll & colls]
    (let [step (fn step [cs]
                 (lazy-seq
                   (let [ss (map seq cs)]
                     (when (every? identity ss)
                       (cons (map first ss) (step (map rest ss)))))))]
      (pmap #(apply f %) threads (step (cons coll colls))))))

(defn get-csv
  "Ingest the CSV file and return a list of maps. Each map is a line in the CSV file where we can
  access each column by name (provided the first line is the header"
  [fname]
  (with-open [in-file (io/reader fname)]
    (doall
      (csv/parse-csv in-file :key :keyword))))


