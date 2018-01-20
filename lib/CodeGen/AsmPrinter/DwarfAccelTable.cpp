//===- llvm/CodeGen/DwarfAccelTable.cpp - Dwarf Accelerator Tables --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains support for writing dwarf accelerator tables.
//
//===----------------------------------------------------------------------===//

#include "DwarfAccelTable.h"

using namespace llvm;

void AppleAccelTableHeader::Emit(AsmPrinter *Asm) {
  // Emit Header.
  Asm->OutStreamer->AddComment("Header Magic");
  Asm->EmitInt32(Header.Magic);
  Asm->OutStreamer->AddComment("Header Version");
  Asm->EmitInt16(Header.Version);
  Asm->OutStreamer->AddComment("Header Hash Function");
  Asm->EmitInt16(Header.HashFunction);
  Asm->OutStreamer->AddComment("Header Bucket Count");
  Asm->EmitInt32(Header.BucketCount);
  Asm->OutStreamer->AddComment("Header Hash Count");
  Asm->EmitInt32(Header.HashCount);
  Asm->OutStreamer->AddComment("Header Data Length");
  Asm->EmitInt32(Header.HeaderDataLength);

  //  Emit Header Data
  Asm->OutStreamer->AddComment("HeaderData Die Offset Base");
  Asm->EmitInt32(HeaderData.DieOffsetBase);
  Asm->OutStreamer->AddComment("HeaderData Atom Count");
  Asm->EmitInt32(HeaderData.Atoms.size());

  for (size_t i = 0; i < HeaderData.Atoms.size(); i++) {
    Atom A = HeaderData.Atoms[i];
    Asm->OutStreamer->AddComment(dwarf::AtomTypeString(A.Type));
    Asm->EmitInt16(A.Type);
    Asm->OutStreamer->AddComment(dwarf::FormEncodingString(A.Form));
    Asm->EmitInt16(A.Form);
  }
}

constexpr AppleAccelTableHeader::Atom AppleAccelTableTypeData::Atoms[];
constexpr AppleAccelTableHeader::Atom AppleAccelTableOffsetData::Atoms[];
