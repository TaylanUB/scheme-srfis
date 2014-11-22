((scheme-mode
  .
  ((indent-tabs-mode . nil)
   (fill-column . 80)
   (eval . (put 'test-assert 'scheme-indent-function 1))
   (eval . (put 'test-eqv 'scheme-indent-function 2))
   (eval . (put 'test-eq 'scheme-indent-function 2))
   (eval . (put 'test-equal 'scheme-indent-function 2))
   (eval . (put 'guard 'scheme-indent-function 1))
   (eval . (put 'lambda* 'scheme-indent-function 1)))))
