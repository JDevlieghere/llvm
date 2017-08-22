/* Compile with:
   for FILE in `seq 3`; do
     clang -g -c  X86/odr-fwd-declaration2.cpp -DFILE$FILE -o Inputs/odr-fwd-declaration2/$FILE.o
   done
 */

// RUN: llvm-dsymutil -f -oso-prepend-path=%p/../Inputs/odr-fwd-declaration2 -y %p/dummy-debug-map.map -o - | llvm-dwarfdump -debug-dump=info - | FileCheck %s

/*
The output of dsymutil should contain debug info for
the definition of the struct A::B.
*/

#ifdef FILE1
# 1 "Header.h" 1
struct A {
  struct B;
  B *bptr;
  B &bref;
};
# 3 "Source1.cpp" 2
void foo() {
  A *ptr1 = 0;
}

// CHECK: DW_TAG_member
// CHECK: AT_name{{.*}} "bptr"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[PTR1:[a-f0-9]*]]
// CHECK: [[STRUCT1:[a-f0-9]*]]:{{.*}}TAG_structure_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: DW_AT_declaration
// CHECK: DW_TAG_member
// CHECK: AT_name{{.*}} "bref"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[REF1:[a-f0-9]*]]
// CHECK: [[PTR1]]:{{.*}}TAG_pointer_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[STRUCT1]]
// CHECK: [[REF1]]:{{.*}}TAG_reference_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[STRUCT1]]

#elif defined(FILE2)
# 1 "Header.h" 1
struct A {
  struct B;
  B *bptr;
  B &bref;
};
# 3 "Source2.cpp" 2
struct A::B {
  int x;
};
void bar() {
  A *ptr2 = 0;
}

// CHECK: DW_TAG_member
// CHECK: AT_name{{.*}} "bptr"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[PTR2:[a-f0-9]*]]
// CHECK: [[STRUCT2:[a-f0-9]*]]:{{.*}}TAG_structure_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: DW_AT_byte_size
// CHECK: DW_TAG_member
// CHECK: AT_name{{.*}} "bref"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[REF1:[a-f0-9]*]]
// CHECK: [[PTR2]]:{{.*}}TAG_pointer_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[STRUCT2]]
// CHECK: [[REF1]]:{{.*}}TAG_reference_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[STRUCT2]]

#elif defined(FILE3)
#else
#error "You must define which file you generate"
#endif
