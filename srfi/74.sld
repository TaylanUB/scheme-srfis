(define-library (srfi 74)
  (export

   endianness-big endianness-little endianness-native

   bytevector-s8-ref bytevector-s8-set!

   bytevector-uint-ref bytevector-uint-set!
   bytevector-sint-ref bytevector-sint-set!

   bytevector-u16-ref bytevector-u16-set!
   bytevector-s16-ref bytevector-s16-set!
   bytevector-u16-native-ref bytevector-u16-native-set!
   bytevector-s16-native-ref bytevector-s16-native-set!

   bytevector-u32-ref bytevector-u32-set!
   bytevector-s32-ref bytevector-s32-set!
   bytevector-u32-native-ref bytevector-u32-native-set!
   bytevector-s32-native-ref bytevector-s32-native-set!

   bytevector-u64-ref bytevector-u64-set!
   bytevector-s64-ref bytevector-s64-set!
   bytevector-u64-native-ref bytevector-u64-native-set!
   bytevector-s64-native-ref bytevector-s64-native-set!

   bytevector->u8-list u8-list->bytevector

   bytevector->uint-list uint-list->bytevector
   bytevector->sint-list sint-list->bytevector

   )
  (import
   (scheme base)
   (srfi 26)
   (srfi 60)
   (srfi aux))
  (include "74.body.scm"))
