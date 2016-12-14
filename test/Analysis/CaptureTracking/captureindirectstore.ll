; RUN: opt < %s -passes='print<captures>' -disable-output 2>&1 | FileCheck %s

; CHECK: ptr may be captured
; CHECK-NOT: ptrtoptr may be captured
; CHECK: deref may be captured

@global = external global i32*

define void @sample() {
entry:
  %ptr = alloca i32
  store i32 1, i32* %ptr
  %ptrtoptr = alloca i32*
  store i32* %ptr, i32** %ptrtoptr
  %deref = load i32*, i32** %ptrtoptr
  store i32* %deref , i32** @global
  ret void
}
