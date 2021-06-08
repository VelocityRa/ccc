#pragma once

#include <map>
#include <vector>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <filesystem>

// *****************************************************************************
// util.cpp
// *****************************************************************************

namespace fs = std::filesystem;

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;

using s8 = int8_t;
using s16 = int16_t;
using s32 = int32_t;
using s64 = int64_t;

using buffer = std::vector<u8>;

// Like assert, but for user errors.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-security"
template <typename... Args>
void verify_impl(const char* file, int line, bool condition, const char* error_message, Args... args) {
	if(!condition) {
		fprintf(stderr, "[%s:%d] ", file, line);
		fprintf(stderr, error_message, args...);
		__debugbreak();
		exit(1);
	}
}
#define verify(condition, ...) \
	verify_impl(__FILE__, __LINE__, condition, __VA_ARGS__)
template <typename... Args>
[[noreturn]] void verify_not_reached_impl(const char* file, int line, const char* error_message, Args... args) {
	fprintf(stderr, "[%s:%d] ", file, line);
	fprintf(stderr, error_message, args...);
	__debugbreak();
	exit(1);
}
#define verify_not_reached(...) \
	verify_not_reached_impl(__FILE__, __LINE__, __VA_ARGS__)
#pragma GCC diagnostic pop

#ifdef _MSC_VER
	#define packed_struct(name, ...) \
		__pragma(pack(push, 1)) struct name { __VA_ARGS__ } __pragma(pack(pop));
#else
	#define packed_struct(name, ...) \
		struct __attribute__((__packed__)) name { __VA_ARGS__ };
#endif

template <typename T>
const T& get_packed(const std::vector<u8>& bytes, u64 offset, const char* subject) {
	verify(bytes.size() >= offset + sizeof(T), "error: Failed to read %s.\n", subject);
	return *(const T*) &bytes[offset];
}

buffer read_file_bin(fs::path const& filepath);
std::string read_string(const buffer& bytes, u64 offset);

struct Range {
	s32 low;
	s32 high;
};

// *****************************************************************************
// Core data structures
// *****************************************************************************

struct ProgramImage {
	std::vector<u8> bytes;
};

// This is like a simplified ElfSectionType.
enum class ProgramSectionType {
	MIPS_DEBUG,
	OTHER
};

struct ProgramSection {
	u64 image;
	u64 file_offset;
	u64 size;
	ProgramSectionType type;
};

enum class SymbolType : u32 {
	NIL = 0,
	GLOBAL = 1,
	STATIC = 2,
	PARAM = 3,
	LOCAL = 4,
	LABEL = 5,
	PROC = 6,
	BLOCK = 7,
	END = 8,
	MEMBER = 9,
	TYPEDEF = 10,
	FILE_SYMBOL = 11,
	STATICPROC = 14,
	CONSTANT = 15
};

enum class SymbolClass : u32 {
	COMPILER_VERSION_INFO = 11
};

struct Symbol {
	std::string string;
	u32 value;
	SymbolType storage_type;
	SymbolClass storage_class;
	u32 index;
};

struct SymFileDescriptor {
	std::string name;
	Range procedures;
	std::vector<Symbol> symbols;
};

struct SymProcedureDescriptor {
	std::string name;
};

struct SymbolTable {
	std::vector<SymProcedureDescriptor> procedures;
	std::vector<SymFileDescriptor> files;
	u64 procedure_descriptor_table_offset;
	u64 local_symbol_table_offset;
	u64 file_descriptor_table_offset;
};

struct Program {
	std::vector<ProgramImage> images;
	std::vector<ProgramSection> sections;
};

// *****************************************************************************
// elf.cpp
// *****************************************************************************

ProgramImage read_program_image(fs::path path);
void parse_elf_file(Program& program, u64 image_index);

// *****************************************************************************
// mdebug.cpp
// *****************************************************************************

SymbolTable parse_symbol_table(const ProgramImage& image, const ProgramSection& section);
const char* symbol_type(SymbolType type);
const char* symbol_class(SymbolClass symbol_class);

// *****************************************************************************
// stabs.cpp
// *****************************************************************************

enum class StabsSymbolDescriptor : s8 {
	LOCAL_VARIABLE = '\0',
	A = 'a',
	LOCAL_FUNCTION = 'f',
	GLOBAL_FUNCTION = 'F',
	GLOBAL_VARIABLE = 'G',
	REGISTER_PARAMETER = 'P',
	VALUE_PARAMETER = 'p',
	REGISTER_VARIABLE = 'r',
	STATIC_GLOBAL_VARIABLE = 's',
	TYPE_NAME = 't',
	ENUM_STRUCT_OR_TYPE_TAG = 'T',
	STATIC_LOCAL_VARIABLE = 'V'
};

enum class StabsTypeDescriptor : s8 {
	TYPE_REFERENCE = '\0',
	ARRAY = 'a',
	ENUM = 'e',
	FUNCTION = 'f',
	RANGE = 'r',
	STRUCT = 's',
	UNION = 'u',
	// I'm not sure what some of these are, they're not all listed in the
	// current version of the documentation.
	AMPERSAND = '&',
	POINTER = '*',
	SLASH = '/',
	MEMBER = '@',
	METHOD = '#',
};

struct StabsField;
struct StabsMember;

struct StabsType {
	StabsTypeDescriptor descriptor;
	StabsType* aux_type = nullptr;
	// Tagged "union" based on the value of the type descriptor.
	struct {
		s64 type_number;
	} type_reference;
	struct {
		StabsType* index_type = nullptr;
		StabsType* element_type = nullptr;
	} array_type;
	struct {
		std::vector<std::pair<std::string, s64>> fields;
	} enum_type;
	struct {
		
	} function_type;
	struct {
		StabsType* type;
		s64 low;
		s64 high;
	} range_type;
	struct {
		s64 type_number;
		std::vector<StabsField> fields;
		std::vector<StabsMember> members;
	} struct_type;
	struct {
		s64 type_number;
		std::vector<StabsField> fields;
	} union_type;
	struct {
		StabsType* value_type = nullptr;
	} pointer_type;
};

struct StabsField {
	std::string name;
	StabsType type;
	s64 offset;
	s64 size;
	std::string type_name;
};

struct StabsMember {
	std::string name;
	std::string physname;
	StabsType type;
};

struct StabsSymbol {
	std::string name;
	StabsSymbolDescriptor descriptor;
	s64 type_number;
	StabsType type;
};

StabsSymbol parse_stabs_symbol(const char* input, bool verbose);
void print_stabs_type(const StabsType& type);
