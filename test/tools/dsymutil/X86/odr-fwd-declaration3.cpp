/* Compile with:
   for FILE in `seq 3`; do
     clang -g -c  X86/odr-fwd-declaration3.cpp -DFILE$FILE -o Inputs/odr-fwd-declaration3/$FILE.o
   done
 */

// RUN: llvm-dsymutil -f -oso-prepend-path=%p/../Inputs/odr-fwd-declaration3 -y %p/dummy-debug-map.map -o - | llvm-dwarfdump -debug-dump=info - | FileCheck %s

/*
The output of dsymutil should contain debug info for
the definition of the struct A::B.
*/

#ifdef FILE1
# 1 "Header.h" 1
struct B;
struct A {
  int B::*PtrToField;
};
# 3 "Source1.cpp" 2
void foo() {
  A *ptr1 = 0;
}

// CHECK: TAG_member
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_name{{.*}} "PtrToField"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[PTRTOMEMBER1:[a-f0-9]*]]
// CHECK: [[PTRTOMEMBER1]]:{{.*}}TAG_ptr_to_member_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_containing_type{{.*}}0x{{0*}}[[STRUCT1:[a-f0-9]*]]
// CHECK: [[STRUCT1]]:{{.*}}TAG_structure_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_name{{.*}} "B"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK-NOT: AT_byte_size
// CHECK: AT_declaration

#elif defined(FILE2)
# 1 "Header.h" 1
struct B;
struct A {
  int B::*PtrToField;
};
# 3 "Source2.cpp" 2
struct B {
  int x;
};
void bar() {
  A *ptr2 = 0;
}

// CHECK: TAG_member
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_name{{.*}} "PtrToField"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_type{{.*}}0x{{0*}}[[PTRTOMEMBER2:[a-f0-9]*]]
// CHECK: [[PTRTOMEMBER2]]:{{.*}}TAG_ptr_to_member_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_containing_type{{.*}}0x{{0*}}[[STRUCT2:[a-f0-9]*]]
// CHECK: [[STRUCT2]]:{{.*}}TAG_structure_type
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_name{{.*}} "B"
// CHECK-NOT: {{DW_TAG|NULL}}
// CHECK: AT_byte_size

#elif defined(FILE3)
#else
#error "You must define which file you generate"
#endif
