(defproject org.clojars.pbaille/utils "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [backtick "0.3.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.gfredericks/debug-repl "0.0.7"]]
  :profiles {:user {:dependencies [[com.gfredericks/debug-repl "0.0.6"]]
                    :plugins      [[com.gfredericks/nrepl-53-monkeypatch "0.1.0"]]
                    :repl-options {:nrepl-middleware
                                   [com.gfredericks.debug-repl/wrap-debug-repl]}}})
