import os
import re
import sys
from elftools.elf.elffile import ELFFile

def parse_symbol_table(elf_file_path):
    """Parse the symbol table from the ELF file."""
    symbol_map = {}

    with open(elf_file_path, 'rb') as f:
        elf = ELFFile(f)
        symtab = elf.get_section_by_name('.symtab')

        if not symtab:
            print("No .symtab section found in the ELF file.")
            sys.exit(1)

        for symbol in symtab.iter_symbols():
            # Convert the value to uppercase hexadecimal without leading zeros
            value = f"S_0x{symbol['st_value']:X}"
            name = symbol.name
            if name:
                symbol_map[value] = name

    return symbol_map

def replace_symbols_in_file(input_file, output_file, symbol_map):
    """Replace symbols in the assembly file with their corresponding names."""
    symbol_pattern = re.compile(r'S_0x[0-9A-Fa-f]+')

    with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
        for line in infile:
            # Replace symbols using the map
            new_line = symbol_pattern.sub(lambda match: symbol_map.get(match.group(0), match.group(0)), line)
            outfile.write(new_line)

def main():
    # Ensure the correct number of arguments
    if len(sys.argv) != 2:
        print("Usage: python symbolize.py <elf_file>")
        sys.exit(1)

    # Hardcoded assembly file name
    asm_file = "final.s"
    elf_file = sys.argv[1]

    # Check if assembly file exists
    if not os.path.exists(asm_file):
        print(f"Assembly file {asm_file} not found.")
        sys.exit(1)

    # Check if ELF file exists
    if not os.path.exists(elf_file):
        print(f"ELF file {elf_file} not found.")
        sys.exit(1)

    # Parse the symbol table
    print("Parsing symbol table from ELF file...")
    symbol_map = parse_symbol_table(elf_file)

    # Replace symbols in the assembly file
    output_file = "symbolized_final.s"
    print("Replacing symbols in the assembly file...")
    replace_symbols_in_file(asm_file, output_file, symbol_map)

    print(f"Symbolized assembly file written to {output_file}.")

if __name__ == "__main__":
    main()
