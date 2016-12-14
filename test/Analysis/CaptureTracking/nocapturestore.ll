; RUN: opt < %s -passes='print<captures>' -disable-output 2>&1 | FileCheck %s

; CHECK-NOT: ptr may be captured
; CHECK-NOT: ptrtoptr may be captured

define void @sample() {
entry:
  %ptr = alloca i32
  store i32 1, i32* %ptr
  %ptrtoptr = alloca i32*
  store i32* %ptr, i32** %ptrtoptr
  ret void
}
