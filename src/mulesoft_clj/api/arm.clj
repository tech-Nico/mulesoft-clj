(ns mulesoft-clj.api.arm)

(def ARM "/armui/api/v1")
(def SERVERS (str ARM "/servers"))

(defn- get-ARM-headers [org-id env-id]
  {
    :X-ANYPNT-ENV-ID env-id
    :X-ANYPNT-ORG-ID org-id
    })

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
