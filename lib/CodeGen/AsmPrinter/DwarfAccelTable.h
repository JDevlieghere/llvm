//==- llvm/CodeGen/DwarfAccelTable.h - Dwarf Accelerator Tables --*- C++ -*-==//
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

#ifndef LLVM_LIB_CODEGEN_ASMPRINTER_DWARFACCELTABLE_H
#define LLVM_LIB_CODEGEN_ASMPRINTER_DWARFACCELTABLE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/DIE.h"
#include "llvm/CodeGen/DwarfStringPoolEntry.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cstddef>
#include <cstdint>
#include <vector>

// The dwarf accelerator tables are an indirect hash table optimized
// for null lookup rather than access to known data. They are output into
// an on-disk format that looks like this:
//
// .-------------.
// |  HEADER     |
// |-------------|
// |  BUCKETS    |
// |-------------|
// |  HASHES     |
// |-------------|
// |  OFFSETS    |
// |-------------|
// |  DATA       |
// `-------------'
//
// where the header contains a magic number, version, type of hash function,
// the number of buckets, total number of hashes, and room for a special
// struct of data and the length of that struct.
//
// The buckets contain an index (e.g. 6) into the hashes array. The hashes
// section contains all of the 32-bit hash values in contiguous memory, and
// the offsets contain the offset into the data area for the particular
// hash.
//
// For a lookup example, we could hash a function name and take it modulo the
// number of buckets giving us our bucket. From there we take the bucket value
// as an index into the hashes table and look at each successive hash as long
// as the hash value is still the same modulo result (bucket value) as earlier.
// If we have a match we look at that same entry in the offsets table and
// grab the offset in the data for our final match.

namespace llvm {

/// Representation of the header of an accelerator table. This consists of the
/// fixed header and the header data. The latter contains the atoms which
/// define the columns of the table.
class AppleAccelTableHeader {
  struct Header {
    uint32_t Magic = MagicHash;
    uint16_t Version = 1;
    uint16_t HashFunction = dwarf::DW_hash_function_djb;
    uint32_t BucketCount = 0;
    uint32_t HashCount = 0;
    uint32_t HeaderDataLength;

    /// 'HASH' magic value to detect endianness.
    static const uint32_t MagicHash = 0x48415348;

    Header(uint32_t DataLength) : HeaderDataLength(DataLength) {}

#ifndef NDEBUG
    void print(raw_ostream &OS) const {
      OS << "Magic: " << format("0x%x", Magic) << "\n"
         << "Version: " << Version << "\n"
         << "Hash Function: " << HashFunction << "\n"
         << "Bucket Count: " << BucketCount << "\n"
         << "Header Data Length: " << HeaderDataLength << "\n";
    }

    void dump() const { print(dbgs()); }
#endif
  };

public:
  /// An Atom defines the structure of the accelerator table. You can think of
  /// it as a column with a specification of its form.
  struct Atom {
    /// Atom Type.
    const uint16_t Type;
    /// DWARF Form.
    const uint16_t Form;

    constexpr Atom(uint16_t Type, uint16_t Form) : Type(Type), Form(Form) {}

#ifndef NDEBUG
    void print(raw_ostream &OS) const {
      OS << "Type: " << dwarf::AtomTypeString(Type) << "\n"
         << "Form: " << dwarf::FormEncodingString(Form) << "\n";
    }

    void dump() const { print(dbgs()); }
#endif
  };

private:
  struct HeaderData {
    uint32_t DieOffsetBase;
    SmallVector<Atom, 3> Atoms;

    HeaderData(ArrayRef<Atom> AtomList, uint32_t Offset = 0)
        : DieOffsetBase(Offset), Atoms(AtomList.begin(), AtomList.end()) {}

#ifndef NDEBUG
    void print(raw_ostream &OS) const {
      OS << "DIE Offset Base: " << DieOffsetBase << "\n";
      for (size_t i = 0; i < Atoms.size(); i++)
        Atoms[i].print(OS);
    }

    void dump() const { print(dbgs()); }
#endif
  };

  Header Header;
  HeaderData HeaderData;

public:
  /// The length of the header data is always going to be 4 + 4 + 4*NumAtoms.
  AppleAccelTableHeader(ArrayRef<Atom> Atoms)
      : Header(8 + (Atoms.size() * 4)), HeaderData(Atoms) {}

  /// Update header with hash and bucket count.
  void SetBucketAndHashCount(uint32_t HashCount) {
    if (HashCount > 1024)
      Header.BucketCount = HashCount / 4;
    else if (HashCount > 16)
      Header.BucketCount = HashCount / 2;
    else
      Header.BucketCount = HashCount > 0 ? HashCount : 1;

    Header.HashCount = HashCount;
  }

  uint32_t GetHashCount() const { return Header.HashCount; }
  uint32_t GetBucketCount() const { return Header.BucketCount; }

  /// Emits the header via the AsmPrinter.
  void Emit(AsmPrinter *);

#ifndef NDEBUG
  void print(raw_ostream &OS) const {
    Header.print(OS);
    HeaderData.print(OS);
  }

  void dump() const { print(dbgs()); }
#endif
};

template <typename DataT> class AppleAccelTable {
  struct DataArray {
    DwarfStringPoolEntryRef Name;
    std::vector<DataT *> Values;
  };

  friend struct HashData;

  struct HashData {
    StringRef Str;
    uint32_t HashValue;
    MCSymbol *Sym;
    AppleAccelTable::DataArray &Data;

    HashData(StringRef S, AppleAccelTable::DataArray &Data)
        : Str(S), Data(Data) {
      HashValue = dwarf::djbHash(S);
    }
  };

  /// Emits the header for the table via the AsmPrinter.
  void EmitHeader(AsmPrinter *Asm);

  /// Helper function to compute the number of buckets needed based on the
  /// number of unique hashes.
  void ComputeBucketCount();

  /// Walk through and emit the buckets for the table. Each index is an offset
  /// into the list of hashes.
  void EmitBuckets(AsmPrinter *);

  /// Walk through the buckets and emit the individual hashes for each bucket.
  void EmitHashes(AsmPrinter *);

  /// Walk through the buckets and emit the individual offsets for each element
  /// in each bucket. This is done via a symbol subtraction from the beginning
  /// of the section. The non-section symbol will be output later when we emit
  /// the actual data.
  void EmitOffsets(AsmPrinter *, const MCSymbol *);

  /// Walk through the buckets and emit the full data for each element in the
  /// bucket. For the string case emit the dies and the various offsets.
  /// Terminate each HashData bucket with 0.
  void EmitData(AsmPrinter *);

  /// Allocator for HashData and Values.
  BumpPtrAllocator Allocator;

  /// Header containing both the header and header data.
  AppleAccelTableHeader Header;
  std::vector<HashData *> Data;

  using StringEntries = StringMap<DataArray, BumpPtrAllocator &>;
  StringEntries Entries;

  using HashList = std::vector<HashData *>;
  HashList Hashes;

  using BucketList = std::vector<HashList>;
  BucketList Buckets;

public:
  AppleAccelTable() : Header(DataT::Atoms), Entries(Allocator) {}

  AppleAccelTable(const AppleAccelTable &) = delete;
  AppleAccelTable &operator=(const AppleAccelTable &) = delete;

  void AddName(DwarfStringPoolEntryRef Name, const DIE *Die);

  void FinalizeTable(AsmPrinter *, StringRef);

  void Emit(AsmPrinter *Asm, const MCSymbol *SecBegin) {
    EmitHeader(Asm);
    EmitBuckets(Asm);
    EmitHashes(Asm);
    EmitOffsets(Asm, SecBegin);
    EmitData(Asm);
  }

#ifndef NDEBUG
  void print(raw_ostream &OS) const {
    // Print Header.
    Header.print(OS);

    // Print Content.
    OS << "Entries: \n";
    for (const auto &Entry : Entries) {
      OS << "Name: " << Entry.first() << "\n";
      for (auto *V : Entry.second.Values)
        V->print(OS);
    }

    OS << "Buckets and Hashes: \n";
    for (auto &Bucket : Buckets)
      for (auto &Hash : Bucket)
        Hash->Print(OS);

    OS << "Data: \n";
    for (auto &D : Data)
      D->print(OS);
  }
  void dump() const { print(dbgs()); }
#endif
};

template <typename DataT>
void AppleAccelTable<DataT>::EmitHeader(AsmPrinter *Asm) {
  Header.Emit(Asm);
}

template <typename DataT>
void AppleAccelTable<DataT>::EmitBuckets(AsmPrinter *Asm) {
  unsigned index = 0;
  for (size_t i = 0, e = Buckets.size(); i < e; ++i) {
    Asm->OutStreamer->AddComment("Bucket " + Twine(i));
    if (!Buckets[i].empty())
      Asm->EmitInt32(index);
    else
      Asm->EmitInt32(std::numeric_limits<uint32_t>::max());
    // Buckets point in the list of hashes, not to the data. Do not increment
    // the index multiple times in case of hash collisions.
    uint64_t PrevHash = std::numeric_limits<uint64_t>::max();
    for (auto *HD : Buckets[i]) {
      uint32_t HashValue = HD->HashValue;
      if (PrevHash != HashValue)
        ++index;
      PrevHash = HashValue;
    }
  }
}

template <typename DataT>
void AppleAccelTable<DataT>::EmitHashes(AsmPrinter *Asm) {
  uint64_t PrevHash = std::numeric_limits<uint64_t>::max();
  for (size_t i = 0, e = Buckets.size(); i < e; ++i) {
    for (auto &Hash : Buckets[i]) {
      uint32_t HashValue = Hash->HashValue;
      if (PrevHash == HashValue)
        continue;
      Asm->OutStreamer->AddComment("Hash in Bucket " + Twine(i));
      Asm->EmitInt32(HashValue);
      PrevHash = HashValue;
    }
  }
}

template <typename DataT>
void AppleAccelTable<DataT>::EmitOffsets(AsmPrinter *Asm,
                                         const MCSymbol *SecBegin) {
  uint64_t PrevHash = std::numeric_limits<uint64_t>::max();
  for (size_t i = 0, e = Buckets.size(); i < e; ++i) {
    for (auto HI = Buckets[i].begin(), HE = Buckets[i].end(); HI != HE; ++HI) {
      uint32_t HashValue = (*HI)->HashValue;
      if (PrevHash == HashValue)
        continue;
      PrevHash = HashValue;
      Asm->OutStreamer->AddComment("Offset in Bucket " + Twine(i));
      MCContext &Context = Asm->OutStreamer->getContext();
      const MCExpr *Sub = MCBinaryExpr::createSub(
          MCSymbolRefExpr::create((*HI)->Sym, Context),
          MCSymbolRefExpr::create(SecBegin, Context), Context);
      Asm->OutStreamer->EmitValue(Sub, sizeof(uint32_t));
    }
  }
}

template <typename DataT>
void AppleAccelTable<DataT>::EmitData(AsmPrinter *Asm) {
  for (size_t i = 0, e = Buckets.size(); i < e; ++i) {
    uint64_t PrevHash = std::numeric_limits<uint64_t>::max();
    for (auto &Hash : Buckets[i]) {
      // Terminate the previous entry if there is no hash collision with the
      // current one.
      if (PrevHash != std::numeric_limits<uint64_t>::max() &&
          PrevHash != Hash->HashValue)
        Asm->EmitInt32(0);
      // Remember to emit the label for our offset.
      Asm->OutStreamer->EmitLabel(Hash->Sym);
      Asm->OutStreamer->AddComment(Hash->Str);
      Asm->emitDwarfStringOffset(Hash->Data.Name);
      Asm->OutStreamer->AddComment("Num DIEs");
      Asm->EmitInt32(Hash->Data.Values.size());
      for (const auto *V : Hash->Data.Values) {
        V->Emit(Asm);
      }
      PrevHash = Hash->HashValue;
    }
    // Emit the final end marker for the bucket.
    if (!Buckets[i].empty())
      Asm->EmitInt32(0);
  }
}

template <typename DataT>
void AppleAccelTable<DataT>::AddName(DwarfStringPoolEntryRef Name,
                                     const DIE *D) {
  assert(Data.empty() && "Already finalized!");
  // If the string is in the list already then add this die to the list
  // otherwise add a new one.
  DataArray &DIEs = Entries[Name.getString()];
  assert(!DIEs.Name || DIEs.Name == Name);
  DIEs.Name = Name;
  DIEs.Values.push_back(new (Allocator) DataT(D));
}

template <typename DataT> void AppleAccelTable<DataT>::ComputeBucketCount() {
  // First get the number of unique hashes.
  std::vector<uint32_t> uniques(Data.size());
  for (size_t i = 0, e = Data.size(); i < e; ++i)
    uniques[i] = Data[i]->HashValue;
  array_pod_sort(uniques.begin(), uniques.end());
  std::vector<uint32_t>::iterator p =
      std::unique(uniques.begin(), uniques.end());

  // Compute the hashes count and use it to set that together with the bucket
  // count in the header.
  Header.SetBucketAndHashCount(std::distance(uniques.begin(), p));
}

template <typename DataT>
void AppleAccelTable<DataT>::FinalizeTable(AsmPrinter *Asm, StringRef Prefix) {
  // Create the individual hash data outputs.
  Data.reserve(Entries.size());
  for (auto &E : Entries) {
    // Unique the entries.
    std::stable_sort(E.second.Values.begin(), E.second.Values.end(),
                     [](const DataT *A, const DataT *B) { return *A < *B; });
    E.second.Values.erase(
        std::unique(E.second.Values.begin(), E.second.Values.end()),
        E.second.Values.end());

    HashData *Entry = new (Allocator) HashData(E.first(), E.second);
    Data.push_back(Entry);
  }

  // Figure out how many buckets we need, then compute the bucket contents and
  // the final ordering. We'll emit the hashes and offsets by doing a walk
  // during the emission phase. We add temporary symbols to the data so that we
  // can reference them during the offset later, we'll emit them when we emit
  // the data.
  ComputeBucketCount();

  // Compute bucket contents and final ordering.
  Buckets.resize(Header.GetBucketCount());
  for (auto &D : Data) {
    uint32_t bucket = D->HashValue % Header.GetBucketCount();
    Buckets[bucket].push_back(D);
    D->Sym = Asm->createTempSymbol(Prefix);
  }

  // Sort the contents of the buckets by hash value so that hash collisions end
  // up together. Stable sort makes testing easier and doesn't cost much more.
  for (auto &Bucket : Buckets)
    std::stable_sort(Bucket.begin(), Bucket.end(),
                     [](HashData *LHS, HashData *RHS) {
                       return LHS->HashValue < RHS->HashValue;
                     });
}

/// Accelerator table value implementation for type accelerator tables.
struct AppleAccelTableTypeData {
  const DIE *Die;

  AppleAccelTableTypeData(const DIE *D) : Die(D) {}

  void Emit(AsmPrinter *Asm) const {
    Asm->EmitInt32(Die->getDebugSectionOffset());
    Asm->EmitInt16(Die->getTag());
    Asm->EmitInt8(0);
  }

  bool operator<(const AppleAccelTableTypeData &Other) const {
    return Die->getOffset() < Other.Die->getOffset();
  }

  static constexpr AppleAccelTableHeader::Atom Atoms[] = {
      AppleAccelTableHeader::Atom(dwarf::DW_ATOM_die_offset,
                                  dwarf::DW_FORM_data4),
      AppleAccelTableHeader::Atom(dwarf::DW_ATOM_die_tag, dwarf::DW_FORM_data2),
      AppleAccelTableHeader::Atom(dwarf::DW_ATOM_type_flags,
                                  dwarf::DW_FORM_data1)};

#ifndef NDEBUG
  void print(raw_ostream &OS) const {
    OS << "  Offset: " << Die->getOffset() << "\n";
    OS << "  Tag: " << dwarf::TagString(Die->getTag()) << "\n";
  }
#endif
};

/// Accelerator table value implementation for simple accelerator tables with
/// just a DIE reference.
struct AppleAccelTableOffsetData {
  const DIE *Die;

  AppleAccelTableOffsetData(const DIE *D) : Die(D) {}

  void Emit(AsmPrinter *Asm) const {
    Asm->EmitInt32(Die->getDebugSectionOffset());
  }

  bool operator<(const AppleAccelTableOffsetData &Other) const {
    return Die->getOffset() < Other.Die->getOffset();
  }

  static constexpr AppleAccelTableHeader::Atom Atoms[] = {
      AppleAccelTableHeader::Atom(dwarf::DW_ATOM_die_offset,
                                  dwarf::DW_FORM_data4)};

#ifndef NDEBUG
  void print(raw_ostream &OS) const {
    OS << "  Offset: " << Die->getOffset() << "\n";
  }
#endif
};

} // end namespace llvm

#endif // LLVM_LIB_CODEGEN_ASMPRINTER_DWARFACCELTABLE_H
