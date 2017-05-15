(ns mulesoft-clj.utils
  (:gen-class)
  (:require [csv-map.core :as csv]
           [clojure.java.io :as io]))

(defn parse-int [number]
  (if (integer? number)
      number
      (try (Integer/parseInt number)
           (catch Exception e nil))))



(defmacro rest-call [URL opts]
  `(let [method# (string/lower-case (name (:method ~opts)))
         orig-headers# (:headers ~opts)
         new-headers# (conj {:accept "application/json" :Authorization (str "Bearer " @access-token)} orig-headers#)
         added-opts# {:insecure? true :headers new-headers#}
         new-map# (merge ~opts added-opts#)
         full-url# (str @baseURL ~URL)
         ]
    (do
      (log/info "Invoking: " method# " : " full-url#)
      (log/debug "Opts: " new-map#)
      (try+
        (let [handlers# (:handlers ~opts)
              response# (~(symbol (str "client/" (string/lower-case (name (:method opts))))) full-url# new-map#)
              status# (:status response#)
              test# (log/trace "Response: " response# )]
          ;If there's a handler for a successful state, execute it (passing the response to it)
          ;otherwise simply return the response
          (log/info "Satus " status#)
          (if-let [handler# (get-in ~opts [:handlers (keyword (str status#))])]
            (handler# response#)
            response#))
        (catch Object err-response#
               (let [err-status# (:status err-response#)
                     test# (log/info "The error:" err-response#)
                     err-response# (assoc err-response# :error true)]
                 (if-let [error-handler# (get-in ~opts [:handlers (keyword (str err-status#))])]
                   (do
                     (log/error "Triggering error handler for " err-status#)
                     (error-handler# err-response#))
                   (do
                     (log/error "No error handler found for " err-status# ". Simply returning the error response...")
                     err-response#))))))))

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


