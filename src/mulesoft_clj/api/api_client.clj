(ns mulesoft-clj.api.api-client
  (:gen-class)
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.java.io :refer [as-url]])
  (:use [slingshot.slingshot :only [throw+ try+]]))

(def baseURL (atom ""))
(def access-token (atom ""))
(def user-details (atom nil))
(def hierarchy (atom nil))

(defn set-base-url [url]
  (reset! baseURL url))



(defn- replace-org-id [url org-id]
  (string/replace url #"\{orgId\}" org-id))








