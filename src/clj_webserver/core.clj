(ns clj-webserver.core
  (:import
   [java.net ServerSocket InetSocketAddress SocketAddress]
   [java.nio.charset Charset]
   [java.io InputStreamReader BufferedReader]
   [java.io File]
   [java.nio.file Files]
   [io.netty.handler.codec.http QueryStringDecoder])
  (:require
   [clojure.string :refer [trim join]]
   [clj-webserver.helper-macros :refer [cond-let]]
   [clj-time.format :as f]
   [clj-time.core :as t]
   [pantomime.mime :refer [mime-type-of]]))

(def PORT 80)
(def time-format (f/formatter "EEE, dd MMM yyyy HH:mm:ss"))
(defn time->str [time] (str (f/unparse time-format time) " GMT"))
(defn throw-error [s] (println s))

(defn deep-merge [a b] (merge-with (fn [x y] (cond (map? y) (deep-merge x y) (vector? y) (concat x y) :else y)) a b))

(defn handle-head [request-map])
(defn handle-options [request-map])
(defn handle-put [request-map])
(defn handle-delete [request-map])
(defn handle-trace [request-map])
(defn handle-connect [request-map])

(defn handle-post [request-map]
  (println request-map))

(defn handle-get [request-map]
  (try
   (let [raw-path (-> request-map :request-uri .path)
         abs-path (if (= "/" raw-path) (str "server-files/" "index.html") (str "server-files/" raw-path))
         resource (.. Files (readAllBytes (.toPath (File. abs-path))))]
     {:response-body resource
      :headers {"Content-Type" (mime-type-of abs-path) "Content-Length" (alength resource)}})
   (catch Exception e {:status-code 404 :reason-phrase "Not Found"})))

(def http-methods
  {"GET" #(handle-get %)
   "POST" #(handle-post %)
   "HEAD" #(handle-head %)
   "OPTIONS" #(handle-options %)
   "PUT" #(handle-put %)
   "DELETE" #(handle-delete %)
   "TRACE" #(handle-trace %)
   "CONNECT" #(handle-connect %)})

(defn read-method [s]
  (if-let [method (re-find #"^\S+" s)]
    (if (contains? (set (keys http-methods)) method)
      [method (subs s (count method))]
      (throw-error "Invalid Method"))
    (throw-error "Invalid HTTP Request")))

(defn read-uri [s]
  (if-let [uri (get (re-find #"^ (\S+)" s) 1)]
    [(QueryStringDecoder. uri) (subs s (inc (count uri)))]
    (throw-error "Invalid route field")))

(defn read-version [s]
  (if-let [uri (get (re-find #"^ (\S+)\r\n" s) 1)]
    [uri (subs s (+ 3 (count uri)))]
    (throw-error "Invalid version field")))

(defn read-headers [s]
  (loop [rmn s, key nil, val nil, key? true, res {}]
    (cond-let
      (re-find #"^\r\n" rmn) [(assoc res key val) (subs rmn 2)]
      [colon (re-find #"^\: (?! +)" rmn)] (recur (subs rmn (count colon)) key nil false res)
      (true? key?) (if-let [match (re-find #"^(?:(?![\(\)\,\/\:\;<=>\?@\[\]\\\{\}\"])[\x21-\x7E])+" rmn)]
                     (recur (subs rmn (count match)) (.toLowerCase match) nil false res)
                     (throw-error "Invalid HTML Header Key"))
      (false? key?) (if-let [match (re-find #"^(?:[\x20-\x7E])+\r\n" rmn)]
                      (recur (subs rmn (count match)) nil nil true (assoc res key (subs match 0 (- (count match) 2))))
                      (do (println rmn) (throw-error "Invalid HTML Header Value")))
      :else (throw-error "Invalid HTML Header"))))

(defn read-body [s content-length]
  (if (= (count s) content-length) s (throw-error "Invalid Msg body")))

(defn encode-http-request [response-map out]
  (let [st-ln (str (response-map :protocol-version) " "
                   (response-map :status-code) " "
                   (response-map :reason-phrase) "\r\n")
        st-ln+hdr (str (reduce-kv (fn [s k v] (str s k ": " v "\r\n")) st-ln (response-map :headers)) "\r\n")
        stln+hdr->bytes (.getBytes st-ln+hdr (Charset/forName "UTF-8"))
        stln+hdr+body-bytes (byte-array (mapcat seq [stln+hdr->bytes (response-map :response-body)]))]
    (.write out stln+hdr+body-bytes)))

(defn process-http-request [request-map]
  (let [method (request-map :request-method), handler (http-methods method)]
    (deep-merge {:protocol-version "HTTP/1.1"
                 :status-code 200
                 :reason-phrase "OK"
                 :headers {"Server" "Clj-HTTP 0.1", "Date" (time->str (t/now)), "Access-Control-Allow-Origin" "*"}}
                (handler request-map))))

(defn decode-http-request [http-str]
  (let [[method rmn] (read-method http-str),
        [uri rmn] (read-uri rmn),
        [version rmn] (read-version rmn),
        [headers rmn] (read-headers rmn),
        body (if-let [length (headers "content-length")] (read-body rmn (Integer/parseInt length)) nil)]
    {:request-method method :request-uri uri :protocol-version version :headers headers :request-body body}))

(defn input-bytes->request-str [in]
  (loop [l (.readLine in) result ""]
    (if-not (.isEmpty l)
      (recur (.readLine in) (str result l "\r\n"))
      (str result "\r\n"))))

(defn handle-connection [in out]
  (-> in
      (input-bytes->request-str)
      (decode-http-request)
      (process-http-request)
      (encode-http-request out)))

(defn start-server [port]
  (let [server (new ServerSocket port)]
    (println (format "Listening on port %d..." port))
    (while true
      (let [client-socket (.accept server)
            in (->> client-socket (.getInputStream) (new InputStreamReader) (new BufferedReader))
            out (.getOutputStream client-socket)]
         (-> (Thread. #(handle-connection in out)) .start)))))

(defn -main [] (start-server PORT))




;accept() method of ServerSocket class blocks the console until the client is connected,
;after the successful connection of client, it returns the instance of Socket

;the server accepts the connection. Upon acceptance, the server gets a new socket bound
; to the same local port and also has its remote endpoint set to the address and port of the client.
; It needs a new socket so that it can continue to listen to the original socket for connection requests
; while tending to the needs of the connected client