(defproject clj-webserver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-time "0.15.2"]
                 [com.novemberain/pantomime "2.11.0"]
                 [io.netty/netty-all "4.1.43.Final"]]
  :repl-options {:init-ns clj-webserver.core}
  :main clj-webserver.core)
