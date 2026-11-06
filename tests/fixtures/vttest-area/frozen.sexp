;; Synthetic schema fixture. Oracle and readiness paths deliberately do not exist.
(:kind :vttest-area-manifest :version 1 :area "cursor-movements" :backend "x11"
 :scope-status :frozen :conditions ("x11-scale1-font16") :required-count 1
 :scenarios ((:id "cursor-fixture" :recorder-label "cursor" :menu-path (1) :pauses 6
              :captures ((:id "pause-1" :pause 1)) :bytes t :timing t
              :oracle (:path "absent-fixture-oracle.sexp"
                       :sha256 "0000000000000000000000000000000000000000000000000000000000000000")
              :requires ((:capability "capture" :path "absent-fixture-readiness.sexp")))))
