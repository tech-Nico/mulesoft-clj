(ns mulesoft-clj.api.access-management)

(def LOGIN "/accounts/login")

;Account Management APIs
(def AM "/accounts/api") ;base path of Access Management APIs
(def ME (str AM "/me"))
(def AM-ORG (str AM "/organizations/{orgId}"))
(def HIERARCHY (str AM-ORG "/hierarchy"))
(def ENVIRONMENTS (str AM-ORG "/environments"))

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
