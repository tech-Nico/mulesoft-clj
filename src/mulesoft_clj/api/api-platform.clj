(ns mulesoft-clj.api.api-platform)

;API platform APIs
(def API "/apiplatform/repository/v2")
(def API-ORG (str API "/organizations/{orgId}"))
(def API-VERSION (str API-ORG "/apis/{apiId}/versions/{versionId}"))
(def DEPLOY-PROXY (str API-VERSION "/proxy/deployment"))
(def PROXY-STATUS (str DEPLOY-PROXY "/status"))
(def API-ENDPOINT (str API-VERSION "/endpoint"))

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

