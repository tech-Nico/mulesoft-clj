(ns mulesoft-clj.api-client
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


(def ARM "/armui/api/v1")
(def SERVERS (str ARM "/servers"))

;Account Management APIs
(def AM "/accounts/api") ;base path of Access Management APIs
(def ME (str AM "/me"))
(def AM-ORG (str AM "/organizations/{orgId}"))
(def HIERARCHY (str AM-ORG "/hierarchy"))
(def ENVIRONMENTS (str AM-ORG "/environments"))

;API platform APIs
(def API "/apiplatform/repository/v2")
(def API-ORG (str API "/organizations/{orgId}"))
(def API-VERSION (str API-ORG "/apis/{apiId}/versions/{versionId}"))
(def DEPLOY-PROXY (str API-VERSION "/proxy/deployment"))
(def PROXY-STATUS (str DEPLOY-PROXY "/status"))
(def API-ENDPOINT (str API-VERSION "/endpoint"))


(defn parse-int [number]
  (if (integer? number)
      number
      (try (Integer/parseInt number)
           (catch Exception e nil))))


(defn- replace-org-id [url org-id]
  (string/replace url #"\{orgId\}" org-id))

(defn- get-ARM-headers [org-id env-id]
  {
   :X-ANYPNT-ENV-ID env-id
   :X-ANYPNT-ORG-ID org-id
   })

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

(defn login
  "Login the user into the Anypoint Platform and returns an authentication token if login was successfull"
  [username password]
  (log/info "Logging in to " (str @baseURL LOGIN))
      (let [auth-token (client/post (str @baseURL LOGIN)
                          {:content-type :json
                           :body (generate-string {:username username
                           :password password})
                           :insecure? true
                          })
            auth-token (get-in auth-token [:body])
            auth-token (get (parse-string auth-token) "access_token")
            ]

        (println "Access Token: " auth-token)
        (reset! access-token auth-token)
        auth-token
  ))


(defn- me-api-call []
  (log/info "Invoked -me-api-call. This should happen only once...")
  (let [URL  ME
        me (rest-call URL {:method :GET})
        me (:body me)]
    (parse-string me)))

(defn me []
  ;Get the user details ONLY if not initialized yet.. otherwise returns the curren value
  (compare-and-set! user-details nil (me-api-call))
  @user-details
  )

(defn- hierarchy-api-call []
  (let [user (me)
        root-id (get-in user ["user" "organization" "id"])
        URL (replace-org-id HIERARCHY root-id)
        res (rest-call URL {:method :GET})]
    (parse-string (:body res))))

(defn get-hierarchy []
  ;Get the user details ONLY if not initialized yet.. otherwise returns the curren value
  (compare-and-set! hierarchy nil (hierarchy-api-call))
  @hierarchy
  )

(defn get-environments [org-id]
  (let [url (replace-org-id ENVIRONMENTS org-id)
        envs (rest-call url {:method :GET })]
    (log/debug "In get environments. Call to " url)
    (parse-string (:body envs))))

(defn- strip-proxy-uri-port [endpoint]
  (-> (or (get endpoint "proxyUri") (get endpoint "proxyRegistrationUri"))
      (as-url)
      (as-> url (str (.getProtocol url) "://"
                     (.getHost url)
                     (.getPath url)
                     (when-not (empty? (.getQuery url))
                               (str "?" (.getQuery url)))))))

(defn set-domain-flag
  "Set the flag refereneUserDomain to true. This is because otherwise proxies will fail to deploy given HSBC
  runtimes will use a domain project for declaring the https listener."
  [org-id api-id version-id]
  (let [path      (-> API-ENDPOINT
                      (string/replace #"\{orgId\}" org-id)
                      (string/replace #"\{apiId\}" api-id)
                      (string/replace #"\{versionId\}" version-id))
        resp      (rest-call path {:method :get})
        endpoint  (parse-string (:body resp))
        curr-val  (get endpoint "referencesUserDomain" :not-found)]
    (log/debug "ReferencesUserDomain is " curr-val " and it's of type " (type curr-val))
    (cond
      (not (nil? (:error resp)))
         (do
          (log/info "No endpoint found or error thrown while querying api " api-id " version " version-id ". referencesUserDomain won't be set")
          nil)
      (or (= curr-val :not-found)
          (nil? curr-val)
          (= curr-val false)
          (= curr-val "false")
          (not curr-val))
         (do
           (let [test       (log/debug "The old proxyUri " (get endpoint "proxyUri"))
                 proxy-uri  (strip-proxy-uri-port endpoint)
                 test (log/debug "Setting new proxyUri to " proxy-uri)
                 new-endpoint {"apiId"                (parse-int api-id)
                               "apiVersionId"         (parse-int version-id)
                               "id"                   (parse-int (get endpoint "id"))
                               "isCloudHub"           (get endpoint "isCloudHub")
                               "masterOrganizationId" (get endpoint "masterOrganizationId")
                               "proxyRegistrationUri" (get endpoint "proxyRegistrationUri")
                               "organizationId"       (get endpoint "organizationId")
                               "proxyUri"             proxy-uri
                               "referencesUserDomain" true
                               "type"                 (get endpoint "type")
                               "uri"                  (get endpoint "uri")
                               "lastActiveDate"       (get endpoint "lastActiveDate")

                } ]
           (log/debug "Setting new endpoint to " new-endpoint)
           (parse-string (:body (rest-call path {
                                                 :content-type :json
                                                 :method :patch
                                                 :body (generate-string new-endpoint)})))))
      (= curr-val true) (do
                          (log/debug "referencesUserDomain will not be set. It's already true")
                          endpoint)
      :else (do
              (log/error "I shouldn't be here")
              (throw (Exception. "Unhandled case while setting endpoint's referencesUserDomain field"))))))

(defn get-servers
  "Get all servers for a single organization and environment"
  [org-id env-id]
  (let [url SERVERS
        auth-headers (get-ARM-headers org-id env-id)
        servers (-> (rest-call url {:method :get :headers auth-headers})
                    (:body)
                    (parse-string)
                    (get "data"))
        build-server (fn [server]
                       {
                        :id (get server "id")
                        :name (get server "name")
                        :org-id org-id
                        :env-id env-id
                        :type (get server "type")
                        :gateway-version (case (clojure.string/upper-case (get server "type"))
                                           "CLUSTER" (get-in (first (get-in server ["details" "servers"])) ["details" "runtimeVersion"])
                                           "SERVER"  (get-in server ["details" "runtimeVersion"])
                                           "")})]
      (map build-server servers)
    ))



(defn get-all-servers
  "Recursively retrieve all servers for each organization in the organizations tree.
  For each org and environments for an org, returns all servers. The final result will be
  list of maps:
  {:org-id org-id :server-id id :name name :env-id env-id}"
  [root-org]
    ;For the current org, get envs and for each env get servers
  (let [org-id (get root-org "id")
        envs (get-environments org-id)
        sub-orgs (get root-org "subOrganizations")
        curr-orgs-servers (map #(get-servers org-id (get % "id")) (get envs "data"))
        sub-orgs-servers (map #(get-all-servers %) (get root-org "subOrganizations"))]
    (flatten (into curr-orgs-servers sub-orgs-servers))))

(defn get-api-version [org-id api-id version-id]
  (let [path (-> API-VERSION
                 (string/replace #"\{orgId\}" org-id)
                 (string/replace #"\{versionId\}" version-id)
                 (string/replace #"\{apiId\}" api-id))
        resp (rest-call path {:method :get})]
    (parse-string (:body resp))))

(defn get-proxy-status [org-id api-id version-id]
  (let [path (-> PROXY-STATUS
                 (string/replace #"\{orgId\}" org-id)
                 (string/replace #"\{apiId\}" api-id)
                 (string/replace #"\{versionId\}" version-id))
        resp (-> path
               (rest-call {:method :get
                           :handlers {
                                      :404 (fn [resp]
                                             (assoc resp :body (generate-string {"status" "not deployed"})))}})
               (:body)
               (parse-string)
               )
        ]
    (log/info "The status response " resp)
    (get resp "status")))

(defn deploy-api-proxy
  "Deploy the autogenerated proxy associated to the api_id and version_id specified.
  The server parameter is a map containing the following server details necessary for the deployment:
  :id
  :name
  :type
  :gateway-version
  This function must also be able to distinguish whether the API already has a proxy deployed.
  If so, perform a PATCH otherwise a POST
  "
  [org-id env-id api-id version-id server]
  (let [url (-> DEPLOY-PROXY
                (string/replace #"\{orgId\}" org-id)
                (string/replace #"\{apiId\}" api-id)
                (string/replace #"\{versionId\}" version-id))
        api-version (get-api-version org-id api-id version-id)
        has-proxy (nil? (:deployment api-version))
        endpoint (set-domain-flag org-id api-id version-id)
        will-deploy? (not (or (nil? endpoint)
                              (nil? (get endpoint "referencesUserDomain"))))]
    (log/debug "Will deploy? " will-deploy? )
    (when will-deploy? ;only deploy if the referencesUserDomain has been succesfully set to true
      (if has-proxy
        (do
          (log/info "Proxy for api " api-id " (version " version-id ") already exists. Redeploying")
          (rest-call url {
                                    :method  :PATCH
                                    :body (generate-string {:environmentId env-id
                                                            :targetId (parse-int (:id server))
                                                            :gatewayVersion (:gateway-version server)
                                                            :targetType (string/lower-case (:type server))
                                                            :type "HY"
                                                            :targetName (:name server)
                                                            :apiVersionId (parse-int version-id)
                                                            :applicationId (parse-int (get-in api-version ["deployment" "applicationId"]))
                                                            :applicationName (get-in api-version ["deployment" "applicationName"])
                                                            :masterOrganizationId (get api-version "masterOrganizationId")
                                                            :organizationId (get api-version "organizationId")
                                                            })
                                    :content-type :json
                                    }))
        (do
          (log/info "Proxy for API " api-id " (version " version-id " )not found. Deploying a new proxy")
          (rest-call url {
                                    :method  :POST
                                    :body (generate-string {:environmentId env-id
                                                            :targetId (parse-int (:id server))
                                                            :gatewayVersion (:gateway-version server)
                                                            :targetType (:type server)
                                                            :type "HY"
                                                            :targetName (:name server)
                                                            :apiVersionId (parse-int version-id)
                                                            :masterOrganizationId (get api-version "masterOrganizationId")
                                                            :organizationId (get api-version "organizationId")
                                                            })
                                    :content-type :json
                                    }))))))

