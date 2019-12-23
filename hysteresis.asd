(defsystem :hysteresis
  :description "Historized bindings library"
  :version "0.1.0"
  :author "TANAKA Shinichi <shinichi.tanaka45@gmail.com>"
  :license "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "history")
                             (:file "hysteresis")))))
