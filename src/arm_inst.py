class Inst(object):
    reg_list = [
        "r0",
        "r1",
        "r2",
        "r3",
        "r4",
        "r5",
        "r6",
        "r7",
        "r8",
        "r11",
        "r9",
        "sb",
        "r10",
        "sl",
        "r12",
        "ip",
        "r13",
        "sp",
        "r14",
        "lr",
        "r15",
        "pc",
    ]

    """
    A class to represent an instruction.
    """

    def __init__(
        self, addr: int = 0, opcode: str = "", regs: list[str] = [], hex: str = ""
    ):
        self.addr = addr
        self.opcode = opcode
        self.width_specifier = None
        self.operand = None
        self.regs = regs
        self.hex = hex
        self.src_exp = ""
        # A register is expected,
        # but an expression can be given for str instructions
        self.dst_exp = ""

    def init_parse(self, objdump_line: str):
        """
        Parse the instruction from a objdump result and
        extract the address, opcode, registers, and data.
        """

        # Split the instruction into parts
        objdump_line = objdump_line.split(";")[0]
        parts = objdump_line.strip().rstrip("\n").split("\t")
        self.addr = int(parts[0].rstrip(":"), 16)
        self.hex = parts[1].strip()
        if "." in parts[2]:
            opcode_parts = parts[2].split(".")
            self.opcode = opcode_parts[0]
            self.width_specifier = opcode_parts[1]
        else:
            self.opcode = parts[2]
        self.operands = parts[3]

        # Extract the registers from the instruction
        operands_arr = self.operands.split(",")
        if "str" in self.opcode:
            self.src_exp = operands_arr[0].strip()
            self.dst_exp = ",".join(operands_arr[1:]).strip()
        else:
            self.dst_exp = operands_arr[0].strip()
            self.src_exp = ",".join(operands_arr[1:]).strip()

        # All registers in the instruction
        self.regs = []
        for reg in self.reg_list:
            if reg in self.operands:
                self.regs.append(reg)

    def print(self):
        """
        Print the instruction in a human-readable format.
        """
        print(f"addr: {hex(self.addr)}")
        print(f"opcode: {self.opcode}")
        print(f"width_specifier: {self.width_specifier}")
        print(f"regs: {self.regs}")
        print(f"hex: {self.hex}")
        print(f"src_exp: {self.src_exp}")
        print(f"dst_exp: {self.dst_exp}")
