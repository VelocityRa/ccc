#include "ccc.h"

#include <filesystem>
#include <stdexcept>
#include <fstream>
#include <iterator>
#include <vector>
#include <cstdint>
#include <string>
#include <cstring>

namespace ccc {

std::vector<u8> read_file_bin(fs::path const& filepath) {
	std::ifstream ifs(filepath, std::ios::binary | std::ios::ate);

	if (!ifs)
		throw std::runtime_error(filepath.string() + ": " + std::strerror(errno));

	const auto end = ifs.tellg();
	ifs.seekg(0, std::ios::beg);

	const auto size = std::size_t(end - ifs.tellg());

	if (size == 0)  // avoid undefined behavior
		return {};

	const std::vector<u8> buf(size);

	if (!ifs.read((char*)buf.data(), buf.size()))
		throw std::runtime_error(filepath.string() + ": " + std::strerror(errno));

	return buf;
}

std::string get_string(const std::vector<u8>& bytes, u64 offset) {
	verify(offset < bytes.size(), "Offset of string is too large.");
	std::string result;
	for(u64 i = offset; i < bytes.size() && bytes[i] != '\0'; i++) {
		result += bytes[i];
	}
	return result;
}

std::string string_format(const char* format, va_list args) {
	static char buffer[16 * 1024];
	vsnprintf(buffer, 16 * 1024, format, args);
	return std::string(buffer);
}

std::string stringf(const char* format, ...) {
	va_list args;
	va_start(args, format);
	std::string string = string_format(format, args);
	va_end(args);
	return string;
}

}
