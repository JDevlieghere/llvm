; RUN: opt < %s -passes='print<captures>' -disable-output 2>&1 | FileCheck %s

; CHECK: l may be captured
; CHECK: n may be captured
; CHECK: j may be captured

define void @sample(i32* %i) #0 {
entry:
  %i.addr = alloca i32*, align 8
  %k = alloca i32*, align 8
  store i32* %i, i32** %i.addr, align 8
  %call = call i8* @malloc(i64 4)
  %l = bitcast i8* %call to i32*
  store i32* %l, i32** %k, align 8
  %m = load i32*, i32** %k, align 8
  store i32 0, i32* %m, align 4
  %n = load i32*, i32** %k, align 8
  store i32* %n, i32** %i.addr, align 8
  ret void
}

declare i8* @malloc(i64)

define i32 @main() {
entry:
  %retval = alloca i32, align 4
  %j = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  store i32 1, i32* %j, align 4
  call void @sample(i32* %j)
  %0 = load i32, i32* %j, align 4
  ret i32 %0
}
