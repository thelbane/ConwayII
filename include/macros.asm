              mac LOG_REGION
              if {3} && >. != >{2}
              echo "(",{2},"-",.,")",{1},"*** BOUNDARY CROSSED ***"
              err
              else
              echo "(",{2},"-",.,")",{1}
              endif
              endm

              mac WARN_BOUNDARY
.start        set {2}
.end          set *
              if >.start != >.end
              echo "Page boundary crossed (",{1},.start,"-",.end,")"
              endif
              endm

              mac REQUIRED_BOUNDARY
.start        set {2}
.end          set *
              if >.start == >.end
              echo "Expected page boundary not crossed (",{1},.start,"-",.end,")"
              endif
              endm

              mac ABORT
              echo ""
              echo "*** ABORTING ASSEMBLY"
              echo "***", {0}
              err
              endm