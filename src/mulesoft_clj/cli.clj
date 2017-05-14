(ns mulesoft-clj.cli
  (:gen-class)
  (:import (java.net InetAddress))
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def cli-options

  [
   ["-f" "--filename FILE" "csv file containing the original proxy->cluster mapping"]
   ["-H" "--hostname HOST" "the hostname of the target 1.5 instance"
    :parse-fn #(InetAddress/getByName %)
    :id :hostname]
   ["-P" "--port PORT" "The port used to connect to the Anypoint platform is not 443"
    :default 443
    :parse-fn #(Integer/parseInt %)]
   ["-u" "--username USERNAME" "The username to access the Anypoint Platform. This must be an Admin user"]
   ["-p" "--password PASSWORD" "The Anypoint Platform user password"]
   ["-t" "--threads NUM THREADS" "The number parallel deployment execution."
    :default 5
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]

   ]
  )

(defn usage [options-summary]
  (->> ["Deploy API proxies on the new 1.5 Anypoint Platform"
        ""
        "Usage: core [options] action"
        ""
        "Options:"
        options-summary
        ]
       (string/join \newline)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-options
  "Validate the command line options passed when invoking the script."
  [options summary]

  (let [ {:keys [username password hostname filename]} options ]

    (when (nil? username)
      (exit 1 (error-msg ["-u is a mandatory parameter\n" summary])))
    (when (nil? password)
      (exit 1 (error-msg ["-p is a mandatory parameter\n" summary])))
    (when (nil? filename)
      (exit 1 (error-msg ["-f is a mandatory parameter\n" summary])))
    (when (nil? hostname)
      (exit 1 (error-msg ["-H is a mandatory parameter\n" summary])))
    (when (not (.exists (io/file filename)))
      (exit 1 (error-msg [(str "File " filename " could not be found\n") summary])))))

