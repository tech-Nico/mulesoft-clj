(ns mulesoft-clj.core
  (:gen-class)
  (:import (java.net InetAddress))
  (:require [clojure.string :as string]
            [clojure.tools.cli :as cli :refer [parse-opts]]
            [post-migration-deploy-proxies.api-client :as api]
            [clojure.tools.logging :as log]
            [mulesoft-clj.utils :refer :all]
            [mulesoft-clj.cli :refer :all]
            [com.climate.claypoole :as cp]))


(defn- find-server
  "Given a server name returns the related server in the live 1.5 environment in order to use
  the right server id for deployment"
  [lookup org-id env-id server-name]
  (first (filter (fn[server]
          (log/debug "Checking that " (:org-id server) "=" org-id " and " (:env-id server) "=" env-id " and " (clojure.string/upper-case(:name server)) "="  (clojure.string/upper-case server-name))
          (and (= (:org-id server) org-id)
               (= (:env-id server) env-id)
               (= (clojure.string/upper-case(:name server)) (clojure.string/upper-case server-name))))
        lookup)))



(defn deploy-proxy
  "Deploy a single API proxy"
  [api-csv servers-lookup]

  (let [{:keys [org_id
                environment_id
                api_id
                api_name
                org_id
                version_id
                server_name
                version_name
                version_id]} api-csv
         deploy-to-server    (find-server servers-lookup org_id environment_id server_name )
        ]
    (if (not (empty? deploy-to-server))
      (do
        (log/info "**********")
        (log/info "Deploying API " api_name "(id: " api_id ") version '" version_name "' (id: " version_id ") to server " deploy-to-server)
        (log/info "**********")
        ;If call to deploy-api-proxy is empty it means API hasn't been deployed therefore we just leave this function
        (if (api/deploy-api-proxy org_id environment_id api_id version_id deploy-to-server)
          (loop [status "BEGIN" curr 1]
            (if (and
                    ;(< curr 40) ;Only retry for 2 minute
                    (not (= status "started"))
                    (not (= status "failed"))
                    (not (= status "not deployed")))
              (do
                (log/info "Deploying API " api_name "(id: " api_id ") version '" version_name "' (id: " version_id "): Wait 5 seconds before checking the status again since status was " status)
                (Thread/sleep 5000) ;wait 2 seconds
                (recur (api/get-proxy-status org_id api_id version_id) (inc curr)))
              (log/info "Polling for api " api_name " status terminated after " curr " attempt. Status is " status)))
            (log/error "**** API NOT DEPLOYED ****"))
        (log/info "Deployment for api " api_name " terminated"))
      (log/error "No server could be found matching name " server_name " for api " api_name))))


(defn test-deploy-proxy [api servers]
  (let [{:keys [api_name api_id server_name]} api]
    (log/info "Deploying " api_name " to server " server_name)))

(defn deploy-proxy-by-server [apis servers-lookup]
  (log/debug "Deploying APIs on cluster " (key apis))
  (let [ apis (val apis)]
    (doall
      ;(map #(test-deploy-proxy % servers-lookup) apis)
      (map #(deploy-proxy % servers-lookup) apis)
      )))

(defn deploy-proxies
  "Ingest the CSV map and for each line deploy the related proxy"
  [source-csv params]
  (let [host (.getHostName (:hostname params))
        port (:port params)
        baseUrl (str "https://" host ":" port)
        baseUrl (api/set-base-url baseUrl)
        group-by-server (->> source-csv
                             (group-by #(str (:server_name %)) )
                             (filter (fn [elem]
                                       (not (empty? (key elem)))))) ;Group API by server and remove APIs whose server name is empty

        num-threads (min (count (keys group-by-server)) (:threads params)) ;One thread for each server with a max of num-threads parallel threads
        my-pool (cp/threadpool
                               num-threads
                              :daemon false
                              :thread-priority 3
                              :name "deploy-proxies-pool")]
    (log/info "The hostname: " host)
    (log/info "The baseurl " baseUrl)
      (api/login (:username params) (:password params))
    (let [hierarchy (api/get-hierarchy)
          servers (api/get-all-servers hierarchy)]
      (log/debug "The servers: " servers)
      (log/info "Deploying servers at a maximum of " num-threads " at a time")
      (log/info "Group-by-servers count " (count group-by-server))
      (cp/with-shutdown! [pool my-pool]
        (doall ;we use doall to force side effects to happen so that REST calls are performed.
            (cp/upmap pool #(deploy-proxy-by-server % servers) group-by-server)
            ;(cp/upmap pool #(deploy-proxy % servers) source-csv)
            )))))

(defn -main
  "Read a CSV file containing API details and for each API deploy the proxy in the 1.5 instance"
  [& args]
  (let [ opts (cli/parse-opts args cli-options)
        {:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors))
      (empty? options) (exit 0 (error-msg ["No options specified" opts]))
      (not (empty? options)) (validate-options options summary)
    )
    ;if we reach this point all input parameters can be considered valid so I can start processing the file
    (let [{:keys [filename uri username password]} options
          csv (get-csv filename)]
      (spit "./output.txt" (with-out-str (pr csv)))
      (println (deploy-proxies csv options)))))
