// clang -fmodules -g -c objc.m -o objc.macho.x86_64.o
// ld -demangle -dynamic -arch x86_64 -macosx_version_min 10.10.0 objc.macho.x86_64.o -o objc.macho.x86_64 -lSystem
@import Foundation;

@interface A : NSObject
- (int)method1:(int)param;
@end

@implementation A
- (int)method1:(int)param {
  return param;
}
@end

@interface A (Category) @end

@implementation A (Category)
- (int)method2:(int)param {
  return param;
}
@end

int main (int argc, char const *argv[])
{
   NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
   A *a = [A new];

   NSString *s = [NSString stringWithFormat:@"argc = %i", argc];
   [pool release];
   return 0;
}
