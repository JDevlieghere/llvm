; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-apple-macosx -mattr=+sse2 -verify-machineinstrs | FileCheck %s

; After tail duplication, two copies in an early exit BB can be cancelled out.
; rdar://10640363
define i32 @t1(i32 %a, i32 %b) nounwind  {
; CHECK-LABEL: t1:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    movl %esi, %edx
; CHECK-NEXT:    movl %edi, %eax
; CHECK-NEXT:    testl %edx, %edx
; CHECK-NEXT:    je LBB0_1
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB0_2: ## %while.body
; CHECK-NEXT:    ## =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    movl %edx, %ecx
; CHECK-NEXT:    cltd
; CHECK-NEXT:    idivl %ecx
; CHECK-NEXT:    testl %edx, %edx
; CHECK-NEXT:    movl %ecx, %eax
; CHECK-NEXT:    jne LBB0_2
; CHECK-NEXT:  ## %bb.3: ## %while.end
; CHECK-NEXT:    movl %ecx, %eax
; CHECK-NEXT:    retq
; CHECK-NEXT:  LBB0_1:
; CHECK-NEXT:    retq
entry:
  %cmp1 = icmp eq i32 %b, 0
  br i1 %cmp1, label %while.end, label %while.body

while.body:                                       ; preds = %entry, %while.body
  %a.addr.03 = phi i32 [ %b.addr.02, %while.body ], [ %a, %entry ]
  %b.addr.02 = phi i32 [ %rem, %while.body ], [ %b, %entry ]
  %rem = srem i32 %a.addr.03, %b.addr.02
  %cmp = icmp eq i32 %rem, 0
  br i1 %cmp, label %while.end, label %while.body

while.end:                                        ; preds = %while.body, %entry
  %a.addr.0.lcssa = phi i32 [ %a, %entry ], [ %b.addr.02, %while.body ]
  ret i32 %a.addr.0.lcssa
}

; Two movdqa (from phi-elimination) in the entry BB cancels out.
; rdar://10428165
define <8 x i16> @t2(<8 x i16> %T0, <8 x i16> %T1) nounwind readnone {
; CHECK-LABEL: t2:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    pshufd {{.*#+}} xmm0 = xmm0[3,1,2,3]
; CHECK-NEXT:    pshuflw {{.*#+}} xmm0 = xmm0[0,1,1,2,4,5,6,7]
; CHECK-NEXT:    punpcklqdq {{.*#+}} xmm0 = xmm0[0],xmm1[0]
; CHECK-NEXT:    retq
entry:
  %tmp8 = shufflevector <8 x i16> %T0, <8 x i16> %T1, <8 x i32> < i32 undef, i32 undef, i32 7, i32 2, i32 8, i32 undef, i32 undef , i32 undef >
  ret <8 x i16> %tmp8
}

define i32 @t3(i64 %a, i64 %b) nounwind  {
; CHECK-LABEL: t3:
; CHECK:       ## %bb.0: ## %entry
; CHECK-NEXT:    movq %rsi, %rdx
; CHECK-NEXT:    movq %rdi, %rax
; CHECK-NEXT:    testq %rdx, %rdx
; CHECK-NEXT:    je LBB2_1
; CHECK-NEXT:    .p2align 4, 0x90
; CHECK-NEXT:  LBB2_2: ## %while.body
; CHECK-NEXT:    ## =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    movq %rdx, %rcx
; CHECK-NEXT:    cqto
; CHECK-NEXT:    idivq %rcx
; CHECK-NEXT:    testq %rdx, %rdx
; CHECK-NEXT:    movq %rcx, %rax
; CHECK-NEXT:    jne LBB2_2
; CHECK-NEXT:  ## %bb.3: ## %while.end
; CHECK-NEXT:    movl %ecx, %eax
; CHECK-NEXT:    retq
; CHECK-NEXT:  LBB2_1:
; CHECK-NEXT:    retq
entry:
  %cmp1 = icmp eq i64 %b, 0
  br i1 %cmp1, label %while.end, label %while.body

while.body:                                       ; preds = %entry, %while.body
  %a.addr.03 = phi i64 [ %b.addr.02, %while.body ], [ %a, %entry ]
  %b.addr.02 = phi i64 [ %rem, %while.body ], [ %b, %entry ]
  %rem = srem i64 %a.addr.03, %b.addr.02
  %cmp = icmp eq i64 %rem, 0
  br i1 %cmp, label %while.end, label %while.body

while.end:                                        ; preds = %while.body, %entry
  %a.addr.0.lcssa = phi i64 [ %a, %entry ], [ %b.addr.02, %while.body ]
  %t = trunc i64 %a.addr.0.lcssa to i32
  ret i32 %t
}

; Check that copy propagation does not kill thing like:
; dst = copy src <-- do not kill that.
; ... = op1 dst<undef>
; ... = op2 dst <-- this is used here.
define <16 x float> @foo(<16 x float> %x) {
; CHECK-LABEL: foo:
; CHECK:       ## %bb.0: ## %bb
; CHECK-NEXT:    movaps %xmm3, %xmm8
; CHECK-NEXT:    xorps %xmm3, %xmm3
; CHECK-NEXT:    pxor %xmm6, %xmm6
; CHECK-NEXT:    pcmpgtd %xmm0, %xmm6
; CHECK-NEXT:    movdqa {{.*#+}} xmm5 = [255,255,255,255]
; CHECK-NEXT:    pand %xmm6, %xmm5
; CHECK-NEXT:    packuswb %xmm5, %xmm5
; CHECK-NEXT:    packuswb %xmm5, %xmm5
; CHECK-NEXT:    cvttps2dq %xmm0, %xmm13
; CHECK-NEXT:    movdqa %xmm0, %xmm10
; CHECK-NEXT:    cmpltps %xmm3, %xmm10
; CHECK-NEXT:    movdqa %xmm6, %xmm9
; CHECK-NEXT:    pxor %xmm10, %xmm9
; CHECK-NEXT:    cvttps2dq %xmm1, %xmm14
; CHECK-NEXT:    movaps %xmm1, %xmm11
; CHECK-NEXT:    cmpltps %xmm3, %xmm11
; CHECK-NEXT:    movdqa %xmm6, %xmm7
; CHECK-NEXT:    pxor %xmm11, %xmm7
; CHECK-NEXT:    cvttps2dq %xmm2, %xmm1
; CHECK-NEXT:    cmpltps %xmm3, %xmm2
; CHECK-NEXT:    movdqa %xmm6, %xmm4
; CHECK-NEXT:    pxor %xmm2, %xmm4
; CHECK-NEXT:    cvttps2dq %xmm8, %xmm12
; CHECK-NEXT:    cmpltps %xmm3, %xmm8
; CHECK-NEXT:    pxor %xmm8, %xmm6
; CHECK-NEXT:    movdqa {{.*#+}} xmm0 = [1,1,1,1]
; CHECK-NEXT:    pand %xmm0, %xmm6
; CHECK-NEXT:    pand %xmm0, %xmm4
; CHECK-NEXT:    pand %xmm0, %xmm7
; CHECK-NEXT:    pand %xmm0, %xmm9
; CHECK-NEXT:    cvtdq2ps %xmm13, %xmm15
; CHECK-NEXT:    cvtdq2ps %xmm14, %xmm14
; CHECK-NEXT:    cvtdq2ps %xmm1, %xmm13
; CHECK-NEXT:    cvtdq2ps %xmm12, %xmm12
; CHECK-NEXT:    pxor %xmm0, %xmm0
; CHECK-NEXT:    cmpltps %xmm12, %xmm0
; CHECK-NEXT:    xorps %xmm1, %xmm1
; CHECK-NEXT:    cmpltps %xmm13, %xmm1
; CHECK-NEXT:    packssdw %xmm0, %xmm1
; CHECK-NEXT:    xorps %xmm0, %xmm0
; CHECK-NEXT:    cmpltps %xmm14, %xmm0
; CHECK-NEXT:    cmpltps %xmm15, %xmm3
; CHECK-NEXT:    packssdw %xmm0, %xmm3
; CHECK-NEXT:    packsswb %xmm1, %xmm3
; CHECK-NEXT:    pand %xmm5, %xmm3
; CHECK-NEXT:    movdqa %xmm3, %xmm1
; CHECK-NEXT:    punpcklbw {{.*#+}} xmm1 = xmm1[0],xmm0[0],xmm1[1],xmm0[1],xmm1[2],xmm0[2],xmm1[3],xmm0[3],xmm1[4],xmm0[4],xmm1[5],xmm0[5],xmm1[6],xmm0[6],xmm1[7],xmm0[7]
; CHECK-NEXT:    movdqa %xmm1, %xmm0
; CHECK-NEXT:    punpcklwd {{.*#+}} xmm0 = xmm0[0,0,1,1,2,2,3,3]
; CHECK-NEXT:    pslld $31, %xmm0
; CHECK-NEXT:    psrad $31, %xmm0
; CHECK-NEXT:    punpckhwd {{.*#+}} xmm1 = xmm1[4],xmm0[4],xmm1[5],xmm0[5],xmm1[6],xmm0[6],xmm1[7],xmm0[7]
; CHECK-NEXT:    pslld $31, %xmm1
; CHECK-NEXT:    psrad $31, %xmm1
; CHECK-NEXT:    punpckhbw {{.*#+}} xmm3 = xmm3[8],xmm0[8],xmm3[9],xmm0[9],xmm3[10],xmm0[10],xmm3[11],xmm0[11],xmm3[12],xmm0[12],xmm3[13],xmm0[13],xmm3[14],xmm0[14],xmm3[15],xmm0[15]
; CHECK-NEXT:    movdqa %xmm3, %xmm5
; CHECK-NEXT:    punpcklwd {{.*#+}} xmm5 = xmm5[0],xmm0[0],xmm5[1],xmm0[1],xmm5[2],xmm0[2],xmm5[3],xmm0[3]
; CHECK-NEXT:    pslld $31, %xmm5
; CHECK-NEXT:    psrad $31, %xmm5
; CHECK-NEXT:    punpckhwd {{.*#+}} xmm3 = xmm3[4],xmm0[4],xmm3[5],xmm0[5],xmm3[6],xmm0[6],xmm3[7],xmm0[7]
; CHECK-NEXT:    pslld $31, %xmm3
; CHECK-NEXT:    psrad $31, %xmm3
; CHECK-NEXT:    pxor %xmm9, %xmm0
; CHECK-NEXT:    pxor %xmm15, %xmm0
; CHECK-NEXT:    pxor %xmm7, %xmm1
; CHECK-NEXT:    pxor %xmm14, %xmm1
; CHECK-NEXT:    pxor %xmm4, %xmm5
; CHECK-NEXT:    pxor %xmm13, %xmm5
; CHECK-NEXT:    pxor %xmm6, %xmm3
; CHECK-NEXT:    pxor %xmm12, %xmm3
; CHECK-NEXT:    pand %xmm8, %xmm3
; CHECK-NEXT:    pand %xmm2, %xmm5
; CHECK-NEXT:    pand %xmm11, %xmm1
; CHECK-NEXT:    pand %xmm10, %xmm0
; CHECK-NEXT:    pxor %xmm9, %xmm0
; CHECK-NEXT:    pxor %xmm7, %xmm1
; CHECK-NEXT:    pxor %xmm4, %xmm5
; CHECK-NEXT:    pxor %xmm6, %xmm3
; CHECK-NEXT:    movdqa %xmm5, %xmm2
; CHECK-NEXT:    retq
bb:
  %v3 = icmp slt <16 x i32> undef, zeroinitializer
  %v14 = zext <16 x i1> %v3 to <16 x i32>
  %v16 = fcmp olt <16 x float> %x, zeroinitializer
  %v17 = sext <16 x i1> %v16 to <16 x i32>
  %v18 = zext <16 x i1> %v16 to <16 x i32>
  %v19 = xor <16 x i32> %v14, %v18
  %v20 = or <16 x i32> %v17, undef
  %v21 = fptosi <16 x float> %x to <16 x i32>
  %v22 = sitofp <16 x i32> %v21 to <16 x float>
  %v69 = fcmp ogt <16 x float> %v22, zeroinitializer
  %v75 = and <16 x i1> %v69, %v3
  %v77 = bitcast <16 x float> %v22 to <16 x i32>
  %v79 = sext <16 x i1> %v75 to <16 x i32>
  %v80 = and <16 x i32> undef, %v79
  %v81 = xor <16 x i32> %v77, %v80
  %v82 = and <16 x i32> undef, %v81
  %v83 = xor <16 x i32> %v19, %v82
  %v84 = and <16 x i32> %v83, %v20
  %v85 = xor <16 x i32> %v19, %v84
  %v86 = bitcast <16 x i32> %v85 to <16 x float>
  ret <16 x float> %v86
}
