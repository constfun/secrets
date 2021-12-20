(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit
          "5e9aca6fb898f5abcd22fe0af4e9ac3fe9c8481f")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'myguix)
        (url "https://github.com/constfun/myguix.git")
        (commit
          "1a35e7dcdaa8590faf69e1d7ef49fccbaaee499a")
        (introduction
          (make-channel-introduction
            "2251d016071175543722963018fc8473c4cfe03e"
            (openpgp-fingerprint
              "C789 7641 FA2B 963A ED0C  2ACE 51DE 6885 B4B5 4236"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
          "a5bbd38fd131282e928144e869dcdf1e09259085")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
